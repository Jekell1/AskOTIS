#!/usr/bin/env python3
"""Generate and upload routine alias documents to cobol-routine-aliases.

Data sources:
  - Enriched flow edges JSONL (default: JSONL/flow_edges_enriched.jsonl)
  - Paragraphs JSONL (optional, to augment candidate target sets) (default: JSONL/paragraphs.jsonl)

Goal:
  Provide fast normalization mapping from variant paragraph / routine tokens (raw_target, caller names)
  to a canonical target plus a family key. This supports user queries like:
    "Where is TIM360 performed?" or "What does TIM-360A do?" despite name variance.

Output index schema (create_indexes.py:index_routine_aliases):
  alias (key), family_key, canonical_target, candidate_targets[], confidence, alias_kind,
  version, created_ts

Alias kinds:
  - raw-target: derived from unresolved or resolved raw_target tokens
  - caller-para: caller paragraph names acting as potential routine identifiers
  - direct-paragraph: paragraph name directly included (from paragraph list)

Heuristics:
  1. Normalize tokens (upper, strip common prefixes, collapse separators) -> norm token.
  2. family_key derived from norm (same as resolver).
  3. For each family_key gather candidate canonical_target names observed as resolved targets.
  4. Canonical choice: most frequent resolved_target_para; tie-breaker shortest name.
  5. Confidence: (#resolved occurrences) / (total occurrences of alias in any role), bounded [0.2,1.0].

Usage:
  python generate_aliases.py --edges JSONL/flow_edges_enriched.jsonl --paragraphs JSONL/paragraphs.jsonl \
       --out JSONL/aliases.jsonl --upload

If --upload is supplied, documents are pushed to index cobol-routine-aliases via Search REST API.

Environment:
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY or local.settings.json Values.
"""
import os, sys, json, argparse, time, re, math, collections, requests
from typing import Dict, List, Set, Tuple

API_VERSION = "2024-07-01"
TARGET_INDEX = "cobol-routine-aliases"
VERSION_DEFAULT = "v1"
NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]

FAMILY_SPLIT_RE = re.compile(r"[^A-Z0-9]+")

TRANSIENT = {429, 503, 500}


def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    t = re.sub(r"[^A-Z0-9]+","-", t)
    return t.strip('-')

def family_key_from_normalized(norm: str) -> str:
    parts = norm.split('-') if norm else []
    if len(parts) >= 2:
        if len(parts[0]) <= 3:
            return '-'.join(parts[:2])
        return parts[0]
    return norm

def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    ep = (first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT') or '').rstrip('/')
    key = first('AZURE_SEARCH_KEY','SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY (or SEARCH_*)')
    return ep, key


def read_jsonl(path: str):
    if not os.path.exists(path):
        return
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            try:
                yield json.loads(line)
            except Exception:
                continue


def aggregate_aliases(edges_path: str, paragraphs_path: str):
    # Data structures
    family_candidates: Dict[str, collections.Counter] = collections.defaultdict(collections.Counter)  # family_key -> counts of resolved_target_para
    alias_occurrences: Dict[str, int] = collections.Counter()  # alias token normalized occurrences (any role)
    alias_roles: Dict[str, Set[str]] = collections.defaultdict(set)  # alias -> set of alias_kind roles
    alias_family: Dict[str, str] = {}  # alias -> family_key
    alias_to_targets: Dict[str, collections.Counter] = collections.defaultdict(collections.Counter)  # alias -> resolved target counts

    # Pass 1: edges
    for e in read_jsonl(edges_path):
        raw = e.get('raw_target') or ''
        if raw:
            n = normalize_token(raw)
            if n:
                fk = family_key_from_normalized(n)
                alias_occurrences[n] += 1
                alias_roles[n].add('raw-target')
                alias_family[n] = fk
                if e.get('resolved') and (rt := e.get('resolved_target_para')):
                    family_candidates[fk][rt] += 1
                    alias_to_targets[n][rt] += 1
        # caller paragraph names as potential aliases
        cp = e.get('caller_para')
        if cp:
            n2 = normalize_token(cp)
            if n2:
                fk2 = family_key_from_normalized(n2)
                alias_occurrences[n2] += 1
                alias_roles[n2].add('caller-para')
                alias_family[n2] = fk2
                # not adding to targets unless edge shows resolved target mapping
                if e.get('resolved') and (rt := e.get('resolved_target_para')):
                    alias_to_targets[n2][rt] += 1
                    family_candidates[fk2][rt] += 1

    # Pass 2: paragraphs (add direct paragraph names)
    for p in read_jsonl(paragraphs_path):
        name = p.get('name') or p.get('para_name')
        if not name:
            continue
        n = normalize_token(name)
        if not n:
            continue
        fk = family_key_from_normalized(n)
        alias_occurrences[n] += 1
        alias_roles[n].add('direct-paragraph')
        alias_family[n] = fk
        # Use paragraph name as a candidate target implicitly
        alias_to_targets[n][name] += 1
        family_candidates[fk][name] += 1

    # Determine canonical targets per family
    family_canonical: Dict[str,str] = {}
    for fk, ctr in family_candidates.items():
        if not ctr:
            continue
        # Choose most frequent, tie-breaker shortest
        most_common = ctr.most_common()
        top_freq = most_common[0][1]
        tied = [name for name,count in most_common if count == top_freq]
        canonical = sorted(tied, key=lambda s: (len(s), s))[0]
        family_canonical[fk] = canonical

    # Build alias documents
    created_ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    docs = []
    for alias_norm, occ in alias_occurrences.items():
        fk = alias_family.get(alias_norm,'')
        target_counts = alias_to_targets.get(alias_norm, collections.Counter())
        candidate_targets = list({*target_counts.keys(), family_canonical.get(fk,'')})
        candidate_targets = [c for c in candidate_targets if c]
        canonical = family_canonical.get(fk)
        resolved_hits = sum(target_counts.values())
        # confidence heuristic
        if occ == 0:
            conf = 0.0
        else:
            base = resolved_hits / occ
            conf = max(0.2, min(1.0, base))
        alias_kind = ','.join(sorted(alias_roles.get(alias_norm, [])))
        doc = {
            'alias': alias_norm,
            'family_key': fk,
            'canonical_target': canonical,
            'candidate_targets': candidate_targets,
            'confidence': round(conf,3),
            'alias_kind': alias_kind,
            'version': VERSION_DEFAULT,
            'created_ts': created_ts,
        }
        docs.append(doc)

    return docs


def upload_aliases(docs, endpoint: str, key: str, batch_size=500):
    url = f"{endpoint}/indexes/{TARGET_INDEX}/docs/index?api-version={API_VERSION}"
    total = 0
    failed_total = 0
    # Sanitize alias keys: Azure key rules letters, digits, underscore, dash, equals
    import re, hashlib
    def sanitize(k: str) -> str:
        if not k:
            return 'alias'
        sk = re.sub(r'[^A-Za-z0-9_\-=]', '-', k.upper())
        sk = re.sub(r'-{2,}', '-', sk).strip('-') or 'alias'
        if len(sk) > 250:
            h = hashlib.sha1(k.encode()).hexdigest()[:8]
            sk = sk[:240] + '-' + h
        return sk
    seen = {}
    for i in range(0, len(docs), batch_size):
        chunk = docs[i:i+batch_size]
        for d in chunk:
            orig = d.get('alias')
            sk = sanitize(orig)
            if sk in seen and seen[sk] != orig:
                # collision -> append short hash
                import hashlib
                suff = hashlib.sha1(orig.encode()).hexdigest()[:6]
                sk2 = sanitize(f"{sk}-{suff}")
                sk = sk2
            seen[sk] = orig
            d['alias'] = sk
            d['@search.action'] = 'mergeOrUpload'
        for attempt in range(1,7):
            r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'value':chunk}, timeout=60)
            if r.status_code == 200:
                body = r.json()
                failed = []
                for v in body.get('value',[]):
                    status = v.get('status')
                    if status is None:
                        status = v.get('succeeded')
                    if status is False:
                        failed.append(v)
                if failed:
                    failed_total += len(failed)
                    print(f"Batch {i//batch_size+1}: {len(failed)} failures")
                break
            if r.status_code in TRANSIENT and attempt < 6:
                time.sleep(min(8.0, 0.5*(2**(attempt-1))))
                continue
            print(f"HTTP {r.status_code} error: {r.text[:200]}")
            failed_total += len(chunk)
            break
        total += len(chunk)
        if i//batch_size % 10 == 0:
            print(f"Uploaded {total}/{len(docs)} (failed_total={failed_total})")
    print(f"Alias upload done total={total} failed={failed_total}")


def write_jsonl(docs, path: str):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path,'w',encoding='utf-8') as f:
        for d in docs:
            f.write(json.dumps(d, ensure_ascii=False)+"\n")


def sample_report(docs):
    print(f"Total alias docs: {len(docs)}")
    # top families by candidate breadth
    fam_span = collections.Counter([d['family_key'] for d in docs])
    print("Top families (count of aliases):")
    for fk,count in fam_span.most_common(10):
        print(f"  {fk}: {count}")
    # low confidence examples
    low = [d for d in docs if d['confidence'] < 0.5][:10]
    if low:
        print("Low confidence examples:")
        for d in low:
            print(f"  {d['alias']} -> {d['canonical_target']} conf={d['confidence']} kind={d['alias_kind']} candidates={len(d['candidate_targets'])}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', default='JSONL/flow_edges_enriched.jsonl')
    ap.add_argument('--paragraphs', default='JSONL/paragraphs.jsonl')
    ap.add_argument('--out', default='JSONL/aliases.jsonl')
    ap.add_argument('--upload', action='store_true')
    ap.add_argument('--no-write', action='store_true')
    ap.add_argument('--limit', type=int, default=0, help='Optional: limit number of docs (post-build) for testing')
    args = ap.parse_args()

    docs = aggregate_aliases(args.edges, args.paragraphs)
    if args.limit:
        docs = docs[:args.limit]

    sample_report(docs)

    if not args.no_write:
        write_jsonl(docs, args.out)
        print(f"Wrote {len(docs)} docs to {args.out}")

    if args.upload:
        ep, key = load_settings()
        upload_aliases(docs, ep, key)

if __name__ == '__main__':
    main()
