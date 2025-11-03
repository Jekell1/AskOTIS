#!/usr/bin/env python3
"""Extract control-flow edges (PERFORM, CALL, simple GO TO) from COBOL source files.

Outputs JSONL to JSONL/flow_edges.jsonl with schema:
  edge_id, file_id, caller_para, target_para, line, kind, program_id (optional best-effort)

Heuristics:
- Paragraph definition: line starting with optional digits/whitespace then token ending with a period.
  Accept tokens with letters/digits/hyphens. Exclude SECTION names ending in ' SECTION.'.
- Program id: the first IDENTIFICATION DIVISION PROGRAM-ID. captured (simplistic regex) reused for all edges in file.
- Caller paragraph = most recently seen paragraph label while scanning forward.
- PERFORM <target> or PERFORM <target> THRU <target2>: create edge kind=perform (target only for now)
- CALL "PROG" or CALL PROG => kind=call
- GO TO <target> => kind=goto
- Targets are normalized stripped trailing period.
- Unresolved targets still emitted; if target not in discovered paragraph set mark kind suffix ':unresolved'.

Limitations: no nested program parsing, no EVALUATE branches, no PERFORM UNTIL dynamic analysis.

Usage:
  python extract_flow_edges.py --root src --out JSONL/flow_edges.jsonl

"""
import os, re, json, argparse, sys, hashlib
from pathlib import Path

PARA_DEF_RE = re.compile(r'^\s*([A-Z0-9][A-Z0-9-]*)(\.)\s*(?:\*.*)?$')
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\.?\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
PERFORM_RE = re.compile(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
PERFORM_THRU_RE = re.compile(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\s+THRU\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
CALL_RE = re.compile(r'\bCALL\s+"?([A-Z0-9][A-Z0-9-]*)"?', re.IGNORECASE)
GOTO_RE = re.compile(r'\bGO\s+TO\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
SECTION_RE = re.compile(r'\bSECTION\b', re.IGNORECASE)

COBOL_EXTS_BASE = {'.cbl', '.cob', '.cobol'}

def hash_id(*parts: str) -> str:
    raw = '|'.join(parts)
    return hashlib.md5(raw.encode('utf-8')).hexdigest()[:16]

def discover_files(root: Path, include_copybooks: bool=False):
    allowed=set(COBOL_EXTS_BASE)
    if include_copybooks:
        allowed.update({'.cpy','.copy'})
    for p in root.rglob('*'):
        if p.suffix.lower() in allowed and p.is_file():
            yield p

def extract_from_file(path: Path):
    text = path.read_text(errors='ignore')
    lines = text.splitlines()
    program_id = None
    paragraphs = []  # list order matters
    paragraph_set = set()
    edges = []
    current_para = None
    for idx, line in enumerate(lines, start=1):
        # Program id detection
        if program_id is None:
            mprog = PROGRAM_ID_RE.search(line)
            if mprog:
                program_id = mprog.group(1).upper()
        # Paragraph definition
        mpara = PARA_DEF_RE.match(line.rstrip())
        if mpara:
            token = mpara.group(1).upper()
            # Skip section lines
            if SECTION_RE.search(line):
                pass
            else:
                current_para = token
                paragraphs.append((token, idx))
                paragraph_set.add(token)
            continue
        upper_line = line.upper()
        # PERFORM THRU (capture first only for now)
        mthru = PERFORM_THRU_RE.search(upper_line)
        if mthru:
            tgt = mthru.group(1).upper()
            kind = 'perform'
            unresolved = tgt not in paragraph_set
            if unresolved:
                kind += ':unresolved'
            edges.append((current_para, tgt, idx, kind))
            continue
        # PERFORM simple
        mperf = PERFORM_RE.search(upper_line)
        if mperf:
            tgt = mperf.group(1).upper()
            kind = 'perform'
            unresolved = tgt not in paragraph_set
            if unresolved:
                kind += ':unresolved'
            edges.append((current_para, tgt, idx, kind))
            continue
        # CALL
        mcall = CALL_RE.search(upper_line)
        if mcall:
            tgt = mcall.group(1).upper()
            kind = 'call'
            edges.append((current_para, tgt, idx, kind))
            continue
        # GO TO
        mgoto = GOTO_RE.search(upper_line)
        if mgoto:
            tgt = mgoto.group(1).upper()
            kind = 'goto'
            unresolved = tgt not in paragraph_set
            if unresolved:
                kind += ':unresolved'
            edges.append((current_para, tgt, idx, kind))
            continue
    file_token = path.stem[:8]
    # Build JSON records
    out_records = []
    for caller, target, line_no, kind in edges:
        caller_norm = caller or '<<NONE>>'
        target_norm = target
        edge_id = f"{file_token}-{line_no}-{caller_norm}-{target_norm}"[:120]
        rec = {
            'edge_id': edge_id,
            'file_id': file_token,
            'caller_para': caller_norm,
            'target_para': target_norm,
            'line': line_no,
            'kind': kind
        }
        if program_id:
            rec['program_id'] = program_id
        out_records.append(rec)
    return out_records

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', default='.', help='Root directory to scan for COBOL source')
    ap.add_argument('--out', default='JSONL/flow_edges.jsonl', help='Output JSONL path')
    ap.add_argument('--min-edges', type=int, default=0, help='Warn if total edges below this')
    ap.add_argument('--show-stats', action='store_true', help='Print summary stats')
    ap.add_argument('--include-copybooks',action='store_true',help='Also scan copybooks (.cpy/.copy)')
    ap.add_argument('--tag-source-kind',action='store_true',help='Add origin_kind field (program|copybook) to edges')
    args = ap.parse_args()

    root = Path(args.root)
    files = list(discover_files(root, include_copybooks=args.include_copybooks))
    total_edges = 0
    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open('w', encoding='utf-8') as f:
        for fp in files:
            try:
                recs = extract_from_file(fp)
            except Exception as ex:
                print(f"WARN: failed parsing {fp}: {ex}", file=sys.stderr)
                continue
            if args.tag_source_kind:
                origin_kind='copybook' if fp.suffix.lower() in {'.cpy','.copy'} else 'program'
                for r in recs:
                    r['origin_kind']=origin_kind
            for r in recs:
                f.write(json.dumps(r, ensure_ascii=False) + '\n')
            total_edges += len(recs)
    if args.show_stats:
        print(f"Scanned {len(files)} source files -> {total_edges} edges written to {out_path}")
    if total_edges < args.min_edges:
        print(f"NOTE: total edges {total_edges} < min-edges {args.min_edges}")

if __name__ == '__main__':
    main()
