"""Patch script: fill missing program_id in new_cobol_flow_edges using file_id fallback.

Logic:
  - Query edges where program_id eq '' (empty string) in pages of 1000.
  - For each doc with file_id present, set program_id = upper(file_id).
  - Upload partial merge docs (edge_id + program_id).
  - Stop when page empty or --limit reached.

Usage:
  python fix_missing_program_id_in_edges.py [--limit N] [--dry-run]

Notes:
  Safe to re-run: only updates docs missing program_id; skips those already populated.
"""
from __future__ import annotations
import os, json, argparse, requests, sys, time, datetime

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges'
PAGE=1000


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch_batch(ep,key,skip):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','filter':"program_id eq ''",'top':PAGE,'skip':skip,'select':'edge_id,file_id,program_id'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body, timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def upload_patch(ep,key,patch_docs):
    if not patch_docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    payload={'value':patch_docs}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:200]}")


def main():
    ap=argparse.ArgumentParser(description='Fill missing program_id in flow edges index.')
    ap.add_argument('--limit', type=int, help='Max documents to patch (overall)')
    ap.add_argument('--dry-run', action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    total_scanned=0; total_patched=0; skip=0
    start=time.time()
    while True:
        rows=fetch_batch(ep,key,skip)
        if not rows: break
        skip+=len(rows)
        patch=[]
        for r in rows:
            if args.limit and total_patched>=args.limit: break
            edge_id=r.get('edge_id'); file_id=r.get('file_id'); prog=r.get('program_id') or ''
            if prog or not file_id: continue
            patch.append({'@search.action':'mergeOrUpload','edge_id':edge_id,'program_id':file_id.upper()})
        total_scanned+=len(rows)
        if patch:
            if args.dry_run:
                print('[DRY-RUN] would patch', len(patch))
            else:
                upload_patch(ep,key,patch)
            total_patched+=len(patch)
        print(f"Scanned={total_scanned} patched={total_patched} last_page={len(rows)}")
        if args.limit and total_patched>=args.limit:
            break
        if len(rows)<PAGE: break
    elapsed=time.time()-start
    print(f"Done. Patched {total_patched} docs in {elapsed:.1f}s")

if __name__=='__main__':
    main()
