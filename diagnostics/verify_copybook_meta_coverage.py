"""Verify that every filesystem .CPY basename has a corresponding meta doc in new_cobol_copybook_meta.

Outputs:
  JSON summary (printed)
  diagnostics/copybook_meta_missing_names.txt  (normalized names with no index doc)
  diagnostics/copybook_meta_extra_names.txt    (names present in index but no .CPY file)

Usage:
  python diagnostics/verify_copybook_meta_coverage.py [--root .] [--limit-index N]

After running you can ingest missing with:
  python ingest/build_copybook_meta.py --push --names-file diagnostics/copybook_meta_missing_names.txt --batch 200
"""
from __future__ import annotations
import os, sys, json, pathlib, re, requests, argparse

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview') or '2025-08-01-preview'
INDEX='new_cobol_copybook_meta'
MISSING_PATH='diagnostics/copybook_meta_missing_names.txt'
EXTRA_PATH='diagnostics/copybook_meta_extra_names.txt'

# Ensure repo root on path for program_catalog
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from ingest import program_catalog as catalog  # type: ignore

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def discover_files(root: str):
    root_path=pathlib.Path(root)
    out=set()
    for p in root_path.rglob('*.CPY'):
        if p.is_file():
            out.add(catalog.normalize_name(p.name))
    return out

def fetch_index_names(ep: str, key: str, limit: int|None=None):
    names=[]
    page=1000; skip=0
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'copybook_name'}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
        if r.status_code!=200:
            print('[FATAL] Index fetch failed', r.status_code, r.text[:300]); sys.exit(2)
        batch=[d.get('copybook_name') for d in r.json().get('value',[]) if d.get('copybook_name')]
        if not batch: break
        names.extend(batch)
        skip+=page
        if limit and len(names)>=limit:
            names=names[:limit]; break
    return names

def main():
    ap=argparse.ArgumentParser(description='Verify copybook meta coverage vs filesystem')
    ap.add_argument('--root', default='.', help='Filesystem root to scan')
    ap.add_argument('--limit-index', type=int, help='Stop after N index names (debug)')
    args=ap.parse_args()
    load_settings(); ep,key=resolve_search()
    fs_names=discover_files(args.root)
    index_names=set(fetch_index_names(ep,key,args.limit_index))
    missing=sorted(fs_names - index_names)
    extra=sorted(index_names - fs_names)
    coverage=(len(fs_names)-len(missing))/len(fs_names)*100 if fs_names else 100.0
    os.makedirs('diagnostics', exist_ok=True)
    with open(MISSING_PATH,'w',encoding='utf-8') as f:
        for n in missing: f.write(n+'\n')
    with open(EXTRA_PATH,'w',encoding='utf-8') as f:
        for n in extra: f.write(n+'\n')
    summary={
        'index': INDEX,
        'filesystem_copybooks': len(fs_names),
        'index_copybooks': len(index_names),
        'missing_in_index': len(missing),
        'extra_in_index': len(extra),
        'coverage_pct': round(coverage,2),
        'missing_sample': missing[:10],
        'extra_sample': extra[:10]
    }
    print(json.dumps(summary, indent=2))
    if missing:
        print(f"Missing names written to {MISSING_PATH} (use --names-file to ingest)")
    else:
        print('No missing names detected.')

if __name__=='__main__':
    main()
