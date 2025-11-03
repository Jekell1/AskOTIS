"""Reconstruct and repopulate the cobol-files index after recreation.

Heuristic reconstruction using the code-chunks index (and optionally paragraphs) to derive
unique file entries. This is a fallback when the original file export / datasource ingestion
is not immediately available.

Assumptions:
 - code-chunks index contains fields: file_id, path, program_id
 - File name is the last path component after '/' or '\\'
 - copybook flag inferred if path contains 'copy' (case-insensitive) OR file name ends with 'cpy'/'cpyb'
 - lines / format / procedure_using not recoverable here -> set to defaults (0 / None / [])

After running this, execute embed_cobol_files.py to create vectors.

Usage:
  python reconstruct_cobol_files.py [--limit N] [--dry-run]
"""
import os, json, sys, argparse, requests
from pathlib import Path

API='2024-07-01'
SRC_INDEX='code-chunks'
DEST_INDEX='cobol-files'

def load_settings():
    p=Path('local.settings.json')
    if p.exists():
        try:
            vals=json.loads(p.read_text()).get('Values',{})
            for k,v in vals.items(): os.environ.setdefault(k,v)
        except Exception:
            pass

def search_page(ep, key, index, select, top, skip):
    url=f"{ep.rstrip('/')}/indexes/{index}/docs/search?api-version={API}"
    body={'search':'*','select':select,'top':top,'skip':skip}
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body)
    if r.status_code!=200:
        raise RuntimeError(f"Search {index} failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def chunk(seq, size):
    for i in range(0,len(seq),size):
        yield seq[i:i+size]

def infer_name(path_val):
    if not path_val:
        return ''
    parts=path_val.replace('\\','/').split('/')
    return parts[-1]

def infer_copybook(path_val, name):
    low=(path_val or '').lower()
    nlow=name.lower()
    if 'copy' in low or nlow.endswith('.cpy') or nlow.endswith('.cpyb'):
        return True
    return False

def push(ep, key, index, docs):
    if not docs: return
    url=f"{ep.rstrip('/')}/indexes/{index}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload failed {r.status_code}: {r.text[:300]}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--limit', type=int, default=None, help='Limit number of reconstructed file docs')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--page', type=int, default=1000)
    args=ap.parse_args()

    load_settings()
    ep=os.environ.get('SEARCH_ENDPOINT'); key=os.environ.get('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr)
        sys.exit(1)

    files={}  # file_id -> doc
    skip=0
    # Azure Search skip hard limit is 100,000; enforce safety
    MAX_SKIP=95000
    while True:
        if skip > MAX_SKIP:
            print(f"Reached skip safety cap ({MAX_SKIP}); stopping early with {len(files)} files")
            break
        batch=search_page(ep,key,SRC_INDEX,'file_id,path,program_id', args.page, skip)
        if not batch: break
        for b in batch:
            fid=b.get('file_id')
            if not fid: continue
            if fid in files: continue
            path_val=b.get('path')
            name=infer_name(path_val)
            copybook=infer_copybook(path_val, name)
            files[fid]={
                'file_id': fid,
                'path': path_val,
                'name': name,
                'program_id': b.get('program_id'),
                'lines': 0,
                'format': None,
                'procedure_using': [],
                'copybook': copybook,
            }
            if args.limit and len(files)>=args.limit:
                break
        skip+=len(batch)
        if args.limit and len(files)>=args.limit:
            break
        if len(batch) < args.page:
            break
    docs=list(files.values())
    print(f"Reconstructed {len(docs)} file docs")
    if args.dry_run:
        for d in docs[:5]:
            print('SAMPLE', d)
        print('Dry-run complete (no upload).')
        return
    uploaded=0
    for c in chunk(docs, 500):
        push(ep,key,DEST_INDEX,c)
        uploaded+=len(c)
        print(f"Uploaded {uploaded}/{len(docs)}")
    print('Repopulation complete.')

if __name__=='__main__':
    main()
