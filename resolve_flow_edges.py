#!/usr/bin/env python3
"""Second-pass enrichment for COBOL flow edges.

Reads original raw edges JSONL (flow_edges.jsonl) and paragraphs JSONL to produce
an enriched JSONL suitable for the cobol-flow-edges-v2 index.

Fields produced per line:
  edge_id, file_id, caller_para, raw_target, resolved_target_para,
  resolved, confidence, edge_subkind, kind, line, caller_program_id,
  target_program_id, file_program_id, family_key, resolution_strategy,
  version, created_ts

Strategy precedence: exact > normalized-equals > prefix-match > fuzzy > none
Confidence scores:   1.0     0.95               0.85          0.70    0.0

Ambiguity: if multiple candidates share highest score, mark resolved=false and strategy=ambiguous.

Usage:
  python resolve_flow_edges.py --edges JSONL/flow_edges.jsonl --paragraphs JSONL/paragraphs.jsonl \
      --out JSONL/flow_edges_enriched.jsonl --version v2 --limit 0

Limit 0 means no limit (process all).

Assumptions:
 - Paragraph JSONL lines have at least: para_id, file_id, name, kind (optional), start_line, end_line
 - Program ID derivation left to heuristics: attempt to extract from name prefix or leave blank.

Future extensions:
 - Incorporate program_id via a dedicated index or metadata file.
 - Add optional dumping of candidate lists for ambiguous cases.
"""
from __future__ import annotations
import os, sys, json, argparse, re, time
from typing import Dict, List, Tuple, Optional
from collections import defaultdict, Counter

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]
FAMILY_SPLIT_RE = re.compile(r"[^A-Z0-9]+")
VERSION_DEFAULT = "v2"

# Simple Levenshtein distance (optimized enough for small candidate sets)
def levenshtein(a: str, b: str) -> int:
    if a == b:
        return 0
    if not a: return len(b)
    if not b: return len(a)
    prev = list(range(len(b)+1))
    for i,ca in enumerate(a,1):
        cur = [i]
        for j,cb in enumerate(b,1):
            if ca == cb:
                cur.append(prev[j-1])
            else:
                cur.append(1 + min(prev[j-1], prev[j], cur[-1]))
        prev = cur
    return prev[-1]

def normalize_token(tok: str) -> str:
    t = tok.upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    # collapse consecutive non-alphanum to single hyphen
    t = re.sub(r"[^A-Z0-9]+","-", t)
    t = t.strip('-')
    return t

def family_key_from_normalized(norm: str) -> str:
    # family key = first component before second hyphen OR full token if short
    parts = norm.split('-') if (norm := norm) else []
    if len(parts) >= 2:
        # Heuristic: if first part length <= 3 (common short prefixes), use first two
        if len(parts[0]) <= 3:
            return '-'.join(parts[:2])
        return parts[0]
    return norm

def load_paragraphs(path: str):
    table: Dict[str, List[Dict]] = defaultdict(list)
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            try:
                obj = json.loads(line)
            except Exception:
                continue
            name = obj.get('name') or obj.get('para_name')
            if not name:
                continue
            norm = normalize_token(name)
            obj['_norm'] = norm
            table[norm].append(obj)
    return table

def candidate_lookup(raw_target: str, paragraph_table: Dict[str, List[Dict]]) -> Tuple[Optional[List[Dict]], str, float, str]:
    """Return (candidates, strategy, confidence, family_key)."""
    if not raw_target:
        return None, 'none', 0.0, ''
    norm_target = normalize_token(raw_target)
    family_key = family_key_from_normalized(norm_target)

    # Exact normalized match
    if norm_target in paragraph_table:
        return paragraph_table[norm_target], 'normalized-equals', 0.95, family_key

    # Prefix match (choose any paragraphs whose normalized starts with norm_target or vice versa)
    prefix_hits = [v for k,v in paragraph_table.items() if k.startswith(norm_target) or norm_target.startswith(k)]
    if prefix_hits:
        merged = [p for bucket in prefix_hits for p in bucket]
        return merged, 'prefix-match', 0.85, family_key

    # Fuzzy (distance <=1 against any existing key)
    fuzzy = []
    for k,bucket in paragraph_table.items():
        if levenshtein(k, norm_target) <= 1:
            fuzzy.extend(bucket)
    if fuzzy:
        return fuzzy, 'fuzzy', 0.70, family_key

    return None, 'none', 0.0, family_key

def choose_canonical(candidates: List[Dict]) -> Optional[Dict]:
    if not candidates:
        return None
    if len(candidates) == 1:
        return candidates[0]
    # Heuristic tie-breakers: shortest name length, then earliest start_line
    candidates = sorted(candidates, key=lambda c: (len(c.get('name','')), c.get('start_line', 1e9)))
    return candidates[0]

def enrich_edges(edges_path: str, paragraphs_path: str, out_path: str, version: str, limit: int = 0, aliases_path: Optional[str] = None):
    """Enhanced enrichment with additional heuristics:

    Added strategies:
      - file-local-equals (0.99): normalized target matches a paragraph defined in same file
      - caller-derived (0.90): target token appears as a caller elsewhere (synthetic paragraph assumption)
    Existing strategies retained: normalized-equals, prefix-match, fuzzy, none, ambiguous.

    Optionally loads aliases JSONL (schema: alias, canonical_target, candidate_targets, family_key) to allow
    potential future linkage (currently used only to attach family_key if paragraph set empty).
    """
    para_table = load_paragraphs(paragraphs_path)
    # Build file-local map: file_id -> norm -> list[para]
    file_local: Dict[str, Dict[str, List[Dict]]] = defaultdict(lambda: defaultdict(list))
    for norm, plist in para_table.items():
        for p in plist:
            fid = p.get('file_id') or p.get('program_id') or ''
            if fid:
                file_local[fid][norm].append(p)

    # Optional: load aliases for family key enrichment
    alias_map: Dict[str, Dict] = {}
    if aliases_path and os.path.exists(aliases_path):
        try:
            with open(aliases_path,'r',encoding='utf-8') as fa:
                for line in fa:
                    if not line.strip():
                        continue
                    try:
                        a = json.loads(line)
                    except Exception:
                        continue
                    al = a.get('alias')
                    if al:
                        alias_map[normalize_token(al)] = a
        except Exception:
            pass

    # First pass: collect set of normalized caller names (potential synthetic definitions)
    caller_norms: Dict[str, int] = Counter()
    with open(edges_path,'r',encoding='utf-8') as ftmp:
        for line in ftmp:
            if not line.strip():
                continue
            try:
                e = json.loads(line)
            except Exception:
                continue
            c = e.get('caller_para')
            if c:
                caller_norms[normalize_token(c)] += 1

    stats = Counter()
    total = 0
    created_ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())

    with open(edges_path,'r',encoding='utf-8') as fin, open(out_path,'w',encoding='utf-8') as fout:
        for line in fin:
            if not line.strip():
                continue
            try:
                e = json.loads(line)
            except Exception:
                continue
            total += 1
            raw_target = e.get('target_para') or e.get('raw_target') or ''
            caller_para = e.get('caller_para')
            kind = e.get('kind')
            if raw_target is None:
                raw_target = ''

            # Base lookup
            candidates, strategy, conf, family_key = candidate_lookup(raw_target, para_table)

            norm_target = normalize_token(raw_target) if raw_target else ''
            fid = e.get('file_id')

            # Heuristic override: file-local exact match BEFORE global normalized-equals
            if fid and norm_target and fid in file_local and norm_target in file_local[fid]:
                candidates = file_local[fid][norm_target]
                strategy = 'file-local-equals'
                conf = 0.99
                if not family_key and norm_target in alias_map:
                    family_key = alias_map[norm_target].get('family_key')
            # If no paragraph candidates and token appears as a caller elsewhere -> synthetic
            synthetic_used = False
            if (not candidates) and norm_target and caller_norms.get(norm_target,0) > 0:
                # synth record
                synthetic = {
                    'name': raw_target if raw_target else norm_target,
                    'file_id': fid,
                    'program_id': e.get('program_id') or fid,
                    'start_line': None,
                    'end_line': None,
                    '_norm': norm_target,
                    '_synthetic': True,
                }
                candidates = [synthetic]
                strategy = 'caller-derived'
                conf = 0.90
                if not family_key and norm_target in alias_map:
                    family_key = alias_map[norm_target].get('family_key')
                synthetic_used = True

            resolved = False
            resolved_target = None
            target_program_id = None
            edge_subkind = None

            if candidates:
                if len(candidates) > 1:
                    canonical = choose_canonical(candidates)
                    alt = [c for c in candidates if c is not canonical]
                    ambiguous = any(abs(len(c.get('name','')) - len(canonical.get('name',''))) <= 2 for c in alt)
                    if ambiguous and strategy not in ('file-local-equals','caller-derived','normalized-equals'):
                        strategy = 'ambiguous'
                        conf = 0.0
                        resolved = False
                    else:
                        resolved_target = canonical.get('name')
                        target_program_id = canonical.get('program_id') or canonical.get('file_id')
                        resolved = True
                else:
                    canonical = candidates[0]
                    resolved_target = canonical.get('name')
                    target_program_id = canonical.get('program_id') or canonical.get('file_id')
                    resolved = True

            # edge_subkind heuristic
            if kind == 'perform':
                edge_subkind = 'perform-paragraph' if resolved else 'perform-unresolved'
            elif kind == 'goto':
                edge_subkind = 'goto-label' if resolved else 'goto-unresolved'
            else:
                edge_subkind = kind

            rec = {
                'edge_id': e.get('edge_id'),
                'file_id': fid,
                'caller_para': caller_para,
                'raw_target': raw_target,
                'resolved_target_para': resolved_target,
                'resolved': resolved,
                'confidence': conf if resolved else 0.0,
                'edge_subkind': edge_subkind,
                'kind': kind,
                'line': e.get('line'),
                'caller_program_id': e.get('program_id') or fid,
                'target_program_id': target_program_id,
                'file_program_id': e.get('program_id') or fid,
                'family_key': family_key,
                'resolution_strategy': strategy,
                'version': version,
                'created_ts': created_ts,
                'synthetic_target': bool(resolved and strategy == 'caller-derived' and synthetic_used),
            }
            fout.write(json.dumps(rec, ensure_ascii=False) + '\n')
            stats[strategy] += 1

            if limit and total >= limit:
                break

    print(f"Processed edges: {total}")
    print("Resolution strategy counts:")
    for k,v in stats.most_common():
        print(f"  {k}: {v}")
    resolved_total = sum(v for k,v in stats.items() if k not in ('none','ambiguous'))
    print(f"Resolved: {resolved_total} ({resolved_total/total*100:.2f}%)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', default='JSONL/flow_edges.jsonl')
    ap.add_argument('--paragraphs', default='JSONL/paragraphs.jsonl')
    ap.add_argument('--out', default='JSONL/flow_edges_enriched.jsonl')
    ap.add_argument('--version', default=VERSION_DEFAULT)
    ap.add_argument('--aliases', help='Optional aliases JSONL for family key enrichment', default=None)
    ap.add_argument('--limit', type=int, default=0)
    args = ap.parse_args()

    enrich_edges(args.edges, args.paragraphs, args.out, args.version, args.limit, args.aliases)

if __name__ == '__main__':
    main()
