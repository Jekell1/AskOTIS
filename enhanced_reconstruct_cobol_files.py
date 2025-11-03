import os, json, sys, requests, argparse
from pathlib import Path

API='2024-07-01'
DEST_INDEX='cobol-files'
SOURCE_INDEXES=['code-chunks','cobol-symbols','cobol-xrefs','cobol-calls','cobol-paragraphs']
PAGE=1000
MAX_SKIP=95000

# Fields we attempt to capture from sources (best-effort)
POSSIBLE_FIELDS=['file_id','path','program_id']


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


def infer_name(path_val):
    if not path_val: return ''
    parts=path_val.replace('\\','/').split('/')
    return parts[-1]

def infer_copybook(path_val, name):
    low=(path_val or '').lower(); nlow=name.lower()
    if 'copy' in low or nlow.endswith('.cpy') or nlow.endswith('.cpyb'):
        return True
    return False


def fetch_existing_ids(ep,key):
    # We only need file_id and maybe path to avoid re-upload. Use paging until exhaustion or limit.
    existing=set(); skip=0
    while True:
        batch=search_page(ep,key,DEST_INDEX,'file_id', 1000, skip)
        if not batch: break
        for b in batch:
            fid=b.get('file_id');
            if fid: existing.add(fid)
        skip+=len(batch)
        if len(batch)<1000: break
        if skip>MAX_SKIP: break
    return existing


FIELD_MAP={
    'code-chunks':['file_id','path','program_id'],
    'cobol-symbols':['file_id','path','program_id'],
    'cobol-xrefs':['file_id','path','program_id'],
    'cobol-calls':['file_id'],  # no path, no program_id field
    'cobol-paragraphs':['file_id']
}

def gather(ep,key, limit=None):
    files={}  # file_id -> doc
    for src in SOURCE_INDEXES:
        skip=0
        while True:
            if skip>MAX_SKIP:
                print(f"[{src}] reached skip cap {MAX_SKIP}")
                break
            desired_fields = FIELD_MAP[src]
            # Try full set first; if API rejects (unknown field), progressively reduce
            select_clause = ','.join(desired_fields)
            try:
                batch=search_page(ep,key,src,select_clause, PAGE, skip)
            except RuntimeError as e:
                # Attempt pruning unknown fields heuristically
                reduced=[]
                for f in desired_fields:
                    try:
                        trial=search_page(ep,key,src,f,1,0)
                        reduced.append(f)
                    except RuntimeError:
                        continue
                if not reduced:
                    print(f"[{src}] all requested fields invalid; skipping index")
                    break
                select_clause=','.join(reduced)
                batch=search_page(ep,key,src,select_clause, PAGE, skip)
            if not batch: break
            for b in batch:
                fid=b.get('file_id')
                if not fid: continue
                if fid not in files:
                    path_val=b.get('path')
                    name=infer_name(path_val)
                    files[fid]={
                        'file_id': fid,
                        'path': path_val,
                        'name': name,
                        'program_id': b.get('program_id'),
                        'lines': 0,
                        'format': None,
                        'procedure_using': [],
                        'copybook': infer_copybook(path_val, name),
                    }
                else:
                    # Fill any missing path/program_id if absent
                    if not files[fid].get('path') and b.get('path'):
                        files[fid]['path']=b.get('path')
                        files[fid]['name']=infer_name(b.get('path'))
                        files[fid]['copybook']=infer_copybook(files[fid]['path'], files[fid]['name'])
                    if not files[fid].get('program_id') and b.get('program_id'):
                        files[fid]['program_id']=b.get('program_id')
                if limit and len(files)>=limit:
                    break
            skip+=len(batch)
            if limit and len(files)>=limit:
                break
            if len(batch)<PAGE: break
        if limit and len(files)>=limit:
            break
    return files


def upload(ep,key, docs):
    if not docs: return
    url=f"{ep.rstrip('/')}/indexes/{DEST_INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--limit', type=int, default=None)
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--batch', type=int, default=500)
    args=ap.parse_args()
    load_settings()
    ep=os.environ.get('SEARCH_ENDPOINT'); key=os.environ.get('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr)
        sys.exit(1)

    existing=fetch_existing_ids(ep,key)
    print(f"Existing file docs: {len(existing)}")
    files=gather(ep,key,limit=args.limit)
    print(f"Gathered total unique file_ids: {len(files)}")
    new_docs=[doc for fid,doc in files.items() if fid not in existing]
    print(f"New docs to upload: {len(new_docs)}")
    if args.dry_run:
        for d in new_docs[:5]:
            print('SAMPLE', d)
        print('Dry-run complete (no upload).')
        return
    uploaded=0
    for i in range(0,len(new_docs),args.batch):
        upload(ep,key,new_docs[i:i+args.batch])
        uploaded+=len(new_docs[i:i+args.batch])
        print(f"Uploaded {uploaded}/{len(new_docs)}")
    print('Enhanced reconstruction complete.')

if __name__=='__main__':
    main()
