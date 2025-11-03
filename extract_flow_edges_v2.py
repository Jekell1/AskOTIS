"""Enhanced extraction producing edges suitable for new_cobol_flow_edges_v2.

Differences vs extract_flow_edges.py:
  - Always sets program_id (falls back to file stem if PROGRAM-ID not found)
  - Emits resolved_target_para identical to target_para initially (placeholder for later resolution)
  - Distinguishes perform_thru edges (adds separate edge for thru end label when present)
  - Filters out edges with missing caller paragraph (skips until first paragraph encountered)
  - Normalizes labels (uppercase, strip trailing period)
  - Adds edge_subkind for perform_thru_start / perform_thru_end

Output fields: edge_id, file_id, program_id, caller_para, target_para, resolved_target_para, line, edge_kind, edge_subkind, edge_text
"""
from __future__ import annotations
import re, os, json, argparse, sys, hashlib
from pathlib import Path

PARA_DEF_RE = re.compile(r'^\s*([A-Z0-9][A-Z0-9-]*)(\.)\s*(?:\*.*)?$')
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\.?\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
PERFORM_THRU_RE = re.compile(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\s+THRU\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
PERFORM_RE = re.compile(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\b', re.IGNORECASE)
CALL_RE = re.compile(r'\bCALL\s+"?([A-Z0-9][A-Z0-9-]*)"?', re.IGNORECASE)
GOTO_RE = re.compile(r'\bGO\s+TO\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
SECTION_RE = re.compile(r'\bSECTION\b', re.IGNORECASE)

COBOL_EXTS_BASE={'.cbl','.cob','.cobol'}

def hash16(*parts):
    return hashlib.md5('|'.join(parts).encode()).hexdigest()[:16]

def discover(root: Path, include_copybooks: bool=True):
    allowed = set(COBOL_EXTS_BASE)
    if include_copybooks:
        allowed.update({'.cpy','.copy'})
    for p in root.rglob('*'):
        if p.suffix.lower() in allowed and p.is_file():
            yield p

def extract(path: Path):
    try:
        text=path.read_text(errors='ignore')
    except Exception as ex:
        print(f"[WARN] read fail {path}: {ex}", file=sys.stderr); return []
    lines=text.splitlines()
    program_id=None
    paragraphs=[]; para_set=set(); current=None
    edges=[]
    for idx,line in enumerate(lines, start=1):
        if program_id is None:
            mprog=PROGRAM_ID_RE.search(line)
            if mprog:
                program_id=mprog.group(1).upper()
        mpara=PARA_DEF_RE.match(line.rstrip())
        if mpara:
            token=mpara.group(1).upper()
            if not SECTION_RE.search(line):
                current=token; paragraphs.append(token); para_set.add(token)
            continue
        if not current:
            continue  # skip edges until first paragraph
        u=line.upper()
        # PERFORM THRU first
        for m in PERFORM_THRU_RE.finditer(u):
            a=m.group(1).upper(); b=m.group(2).upper()
            edges.append((current,a,idx,'perform','perform_thru_start',line.strip()))
            if b!=a:
                edges.append((current,b,idx,'perform','perform_thru_end',line.strip()))
        # Simple PERFORM (avoid double counting first token of THRU by excluding lines that matched above)
        if not PERFORM_THRU_RE.search(u):
            for m in PERFORM_RE.finditer(u):
                tgt=m.group(1).upper()
                edges.append((current,tgt,idx,'perform','',line.strip()))
        for m in CALL_RE.finditer(u):
            tgt=m.group(1).upper(); edges.append((current,tgt,idx,'call','',line.strip()))
        for m in GOTO_RE.finditer(u):
            tgt=m.group(1).upper(); edges.append((current,tgt,idx,'goto','',line.strip()))
    if not program_id:
        program_id=path.stem.upper()[:8]
    file_id=path.stem.upper()[:8]
    recs=[]
    for caller,target,line_no,kind,subkind,src_line in edges:
        edge_id=f"{file_id}-{line_no}-{caller}-{target}"[:120]
        recs.append({
            'edge_id': edge_id,
            'file_id': file_id,
            'program_id': program_id,
            'caller_para': caller,
            'target_para': target,
            'resolved_target_para': target,  # placeholder identical for now
            'line': line_no,
            'edge_kind': kind,
            'edge_subkind': subkind or None,
            'edge_text': src_line
        })
    return recs

def main():
    ap=argparse.ArgumentParser(description='Enhanced COBOL flow edge extractor (v2)')
    ap.add_argument('--root',default='cobol_src')
    ap.add_argument('--out',default='JSONL/flow_edges_v2.jsonl')
    ap.add_argument('--include-copybooks',action='store_true',help='Also scan .cpy/.copy files and treat each copybook as its own pseudo-program')
    ap.add_argument('--tag-source-kind',action='store_true',help='Add origin_kind field (program|copybook) to each edge record')
    ap.add_argument('--show-stats',action='store_true')
    ap.add_argument('--max-files',type=int)
    args=ap.parse_args()
    root=Path(args.root)
    files=list(discover(root, include_copybooks=args.include_copybooks))
    if args.max_files:
        files=files[:args.max_files]
    total=0
    out_path=Path(args.out); out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open('w',encoding='utf-8') as f:
        for p in files:
            recs=extract(p)
            if args.tag_source_kind:
                origin_kind = 'copybook' if p.suffix.lower() in {'.cpy','.copy'} else 'program'
                for r in recs:
                    r['origin_kind']=origin_kind
            for r in recs:
                f.write(json.dumps(r,ensure_ascii=False)+'\n')
            total+=len(recs)
    if args.show_stats:
        print(f"Files={len(files)} edges={total} out={out_path}")

if __name__=='__main__':
    main()
