"""Extract real symbol reference occurrences from cobol-xrefs into new_cobol_symbol_refs.

Relies on the existing xref index (cobol-xrefs) fields:
  simple_name, program_id, direction, line, snippet

Mapping direction -> kind:
  read -> READ
  write -> WRITE
  param_in -> PARAM_IN (also counts as READ)
  param_out -> PARAM_OUT (also counts as WRITE)

Features:
  * Avoid duplicates via constructed ref_id (program|line|symbol|kind)
  * Optional limit and dry-run
  * Handles $skip pagination until limit or exhaustion (warns if skip would exceed 100k)

Usage:
  python extract_symbol_refs_from_xrefs.py --batch 500
  python extract_symbol_refs_from_xrefs.py --limit 200000 --dry-run
"""
from __future__ import annotations
import os, sys, json, argparse, time, requests
from id_normalization import (make_global_symbol_id, make_scoped_symbol_id, make_ref_id, normalize_program_id)
import pathlib, re

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
XREF_INDEX='cobol-xrefs'
TARGET_INDEX='new_cobol_symbol_refs'
PAGE=1000
PARA_LABEL_RE=re.compile(r'^(?P<label>[A-Z0-9][A-Z0-9-]{1,63})\.(\s*$)',re.IGNORECASE)

_PROGRAM_FILE_CACHE={}  # program_id -> path str or None
_PARAGRAPH_CACHE={}     # program_id -> list of (start_line,label)

def discover_program_files(root: str, include_copybooks: bool=False) -> dict:
    mapping={}
    p=pathlib.Path(root)
    if not p.exists():
        return mapping
    prog_exts={'.cbl','.cob'}
    copy_exts={'.cpy','.copy'} if include_copybooks else set()
    allowed=prog_exts|copy_exts
    for fp in p.rglob('*'):
        if fp.suffix.lower() in allowed:
            stem=fp.stem.upper()
            mapping.setdefault(stem,fp)
    return mapping

def load_paragraphs(program_id: str, file_path: pathlib.Path):
    try:
        text=file_path.read_text(encoding='utf-8',errors='ignore')
    except Exception:
        return []
    lines=text.splitlines()
    out=[]
    for i,line in enumerate(lines, start=1):
        m=PARA_LABEL_RE.match(line.strip())
        if m:
            out.append((i,m.group('label').upper()))
    return out

def resolve_program_context(program_id: str, line_number: int, source_map: dict, source_root: str):
    pid=program_id.upper()
    if pid not in _PROGRAM_FILE_CACHE:
        fp=source_map.get(pid)
        _PROGRAM_FILE_CACHE[pid]=str(fp) if fp else None
        if fp and pid not in _PARAGRAPH_CACHE:
            _PARAGRAPH_CACHE[pid]=load_paragraphs(pid,fp)
    file_path=_PROGRAM_FILE_CACHE.get(pid)
    para_name=''  # determine paragraph by last start <= line
    plist=_PARAGRAPH_CACHE.get(pid) or []
    prev=None
    for start,label in plist:
        if start>line_number:
            break
        prev=label
    if prev:
        para_name=prev
    return file_path or '', para_name

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_xrefs(ep,key,skip,top):
    body={'search':'*','top':top,'skip':skip,'select':'simple_name,program_id,direction,line,snippet'}
    r=requests.post(f"{ep}/indexes/{XREF_INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,batch):
    if not batch: return
    r=requests.post(f"{ep}/indexes/{TARGET_INDEX}/docs/index?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':batch})
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:300]); sys.exit(1)

def main():
    ap=argparse.ArgumentParser(description='Extract real symbol refs from xref index')
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--limit',type=int,default=0,help='Max xref records to process (0=all)')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--recreate-first',action='store_true',help='Recreate symbol refs index before ingest (delegates to create_symbol_refs_index.py)')
    ap.add_argument('--source-root',default=os.getenv('COBOL_SOURCE_ROOT','cobol_src'),help='Root directory of COBOL sources for paragraph resolution')
    ap.add_argument('--include-copybooks',action='store_true',help='Also scan copybooks (.cpy/.copy) for paragraph resolution')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    if args.recreate_first:
        os.system(f"python create_symbol_refs_index.py --force")
    processed=0; uploaded=0; skip=0; start=time.time(); seen=set(); out=[]
    source_map=discover_program_files(args.source_root, include_copybooks=args.include_copybooks)
    if not source_map:
        print(f"[WARN] No COBOL sources discovered under {args.source_root}; paragraph_name and file_path will be blank.")
    while True:
        if args.limit and processed>=args.limit: break
        if skip>=100000:
            print('[WARN] Reached $skip >= 100000; stopping to avoid service limit. Consider shard strategy.')
            break
        top=PAGE
        rows=fetch_xrefs(ep,key,skip,top)
        if not rows: break
        for r in rows:
            if args.limit and processed>=args.limit: break
            processed+=1
            name=(r.get('simple_name') or '').strip()
            if not name: continue
            pid=normalize_program_id(r.get('program_id') or '')
            if not pid: continue
            direction=(r.get('direction') or '').lower()
            line=r.get('line') or 0
            kind_map={'read':'READ','write':'WRITE','param_in':'PARAM_IN','param_out':'PARAM_OUT'}
            raw_kind=kind_map.get(direction,'READ')
            # normalized kind collapses param in/out back to READ/WRITE for higher level aggregations
            norm_map={'PARAM_IN':'READ','PARAM_OUT':'WRITE'}
            kind=raw_kind
            normalized_kind=norm_map.get(raw_kind,raw_kind)
            gid=make_global_symbol_id(name)
            sid=make_scoped_symbol_id(pid,name)
            ref_id=make_ref_id(pid,line,name,kind)
            if ref_id in seen:
                continue
            seen.add(ref_id)
            file_path, para_name = resolve_program_context(pid,line,source_map,args.source_root)
            snippet=(r.get('snippet') or '')
            # if snippet missing or very short, attempt to fetch line text
            if (not snippet or len(snippet)<8) and file_path:
                try:
                    file_lines=open(file_path,'r',encoding='utf-8',errors='ignore').read().splitlines()
                    if 1<=line<=len(file_lines):
                        raw_line=file_lines[line-1].strip()
                        snippet=raw_line[:160]
                except Exception:
                    pass
            out.append({
                '@search.action':'mergeOrUpload',
                'ref_id':ref_id,
                'program_id':pid,
                'symbol_name':name,
                'symbol_id':sid,
                'symbol_id_global':gid,
                'kind':kind,
                'op':kind,
                'normalized_kind': normalized_kind,
                'line_number':line,
                'file_path': file_path or '',
                'paragraph_name': para_name,
                'excerpt': snippet[:400],
                'context_before':'',  # filled by context harvesting step (optional) below
                'context_after':'',
                'has_vector': False,
                'first_in_program': False, # can be post-processed if needed
                'cluster_key': f"{pid}|{name}",
                'ingested_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
            })
            if len(out)>=args.batch:
                if not args.dry_run:
                    upload(ep,key,out); uploaded+=len(out)
                out.clear()
        skip+=top
        if processed and processed % (args.batch*20)==0:
            print(f"Progress processed={processed} uploaded={uploaded}")
    if out and not args.dry_run:
        upload(ep,key,out); uploaded+=len(out)
    dur=time.time()-start
    print(f"Done processed={processed} uploaded={uploaded} dry_run={args.dry_run} in {dur:.1f}s")

if __name__=='__main__':
    main()
