"""Scan COBOL sources and build alias variants into new_cobol_name_aliases.

LEGACY NOTE:
    Paragraph + copybook paragraph ingestion with richer metadata is now provided by
    `extract_paragraphs_and_update_aliases.py`. This script remains for quick bootstrap
    or recovery but may under-count if run without --include-copybooks.

Sources for canonical names:
    * Program files (.cbl/.cob) -> filename stem
    * (Optional) Copybook files (.cpy/.copy when --include-copybooks) -> copybook stem
    * COPY statements -> copybook name
    * Paragraph labels (LABEL.) -> paragraph names from all scanned files

Enhancement:
    --include-copybooks: also scan .cpy/.copy files for copybook + paragraph aliases.

For each canonical, generate variants via alias_utils.generate_variants.

Usage:
    python build_name_aliases.py --batch 800
    python build_name_aliases.py --recreate-first --include-copybooks
"""
from __future__ import annotations
import os, json, argparse, pathlib, re, time, requests
from alias_utils import generate_variants, make_alias_record

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_name_aliases'
COPY_RE=re.compile(r'COPY\s+([A-Z0-9][A-Z0-9_-]{0,63})',re.IGNORECASE)
PARA_LABEL_RE=re.compile(r'^(?P<label>[A-Z0-9][A-Z0-9-]{1,63})\.(\s*$)',re.IGNORECASE)

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")

def scan(root: str, limit: int|None=None, include_copybooks: bool=False):
    p=pathlib.Path(root)
    programs=set(); copybooks=set(); paragraphs=set()
    count=0
    prog_exts={'.cbl','.cob'}
    copy_exts={'.cpy','.copy'} if include_copybooks else set()
    allowed=prog_exts|copy_exts
    for fp in p.rglob('*'):
        ext=fp.suffix.lower()
        if ext not in allowed:
            continue
        try:
            text=fp.read_text(encoding='utf-8',errors='ignore')
        except Exception:
            continue
        stem=fp.stem.upper()
        if ext in prog_exts:
            programs.add(stem)
        elif include_copybooks and ext in copy_exts:
            copybooks.add(stem)  # treat copybook stem itself as canonical
        for m in COPY_RE.finditer(text):
            copybooks.add(m.group(1).upper())
        for line in text.splitlines():
            m=PARA_LABEL_RE.match(line.strip())
            if m:
                paragraphs.add(m.group('label').upper())
        count+=1
        if limit and count>=limit:
            break
    return programs, copybooks, paragraphs

def main():
    ap=argparse.ArgumentParser(description='Build name aliases index')
    ap.add_argument('--source-root',default=os.getenv('COBOL_SOURCE_ROOT','cobol_src'))
    ap.add_argument('--batch',type=int,default=800)
    ap.add_argument('--limit-files',type=int)
    ap.add_argument('--include-copybooks',action='store_true',help='Also scan .cpy/.copy files for copybook + paragraph aliases')
    ap.add_argument('--recreate-first',action='store_true')
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    if args.recreate_first:
        os.system('python create_name_aliases_index.py --force')
    progs,copies,paras=scan(args.source_root,args.limit_files, include_copybooks=args.include_copybooks)
    print(f"Discovered programs={len(progs)} copybooks={len(copies)} paragraphs={len(paras)}")
    now=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime())
    out=[]; uploaded=0
    def emit(canon, kind, source):
        vmap=generate_variants(canon)
        for variant, vtype in vmap.items():
            rec=make_alias_record(canon, variant, vtype, kind, source)
            rec['ingested_at']=now
            out.append(rec)
    for n in progs: emit(n,'PROGRAM','filename')
    for n in copies: emit(n,'COPYBOOK','COPY stmt')
    for n in paras: emit(n,'PARAGRAPH','paragraph')
    print(f"Prepared {len(out)} alias docs")
    if args.dry_run:
        print('Dry-run sample:')
        for d in out[:10]:
            print(d)
        return
    for i in range(0,len(out),args.batch):
        upload(ep,key,out[i:i+args.batch]); uploaded+=len(out[i:i+args.batch])
    print(f"Uploaded {uploaded} alias docs")

if __name__=='__main__':
    main()
