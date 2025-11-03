#!/usr/bin/env python3
"""Delete all existing COBOL indexes & indexers so new schemas (with suggesters/facets) can be recreated.

WARNING: This causes a temporary search outage until reindexing completes.

Process:
 1. Delete indexers (they reference indexes)
 2. Delete indexes
 3. (Re)run create_indexes.py separately with --auto to rebuild & run ingestion.

Usage:
  python recreate_all_indexes.py
Optionally use --yes to skip confirmation.
"""
import json, sys, argparse, requests, time
API_VERSION="2024-07-01"
INDEXES=[
    'code-chunks','cobol-symbols','cobol-xrefs','cobol-calls','cobol-files',
    'cobol-paragraphs','cobol-facts','cobol-copybooks',
    'cobol-flow-edges-v2'
]
INDEXERS=[
    'idx-chunks','idx-symbols','idx-xrefs','idx-calls','idx-files',
    'idx-paragraphs','idx-facts','idx-copybooks'
    # note: no legacy idx-flow-edges; v2 ingestion handled by custom upload script
]

def load():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep=vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key=vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not (ep and key):
        print('Missing endpoint/key'); sys.exit(2)
    return ep.rstrip('/'), key

def del_resource(ep,key,kind,name):
    url=f"{ep}/{kind}('{name}')?api-version={API_VERSION}"
    r=requests.delete(url,headers={'api-key':key},timeout=30)
    if r.status_code in (200,204,404):
        print(f"Deleted {kind[:-1]} {name} (status {r.status_code})")
    else:
        print(f"Failed deleting {kind[:-1]} {name}: {r.status_code} {r.text[:200]}")

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--yes',action='store_true')
    args=ap.parse_args()
    ep,key=load()
    if not args.yes:
        print('About to DELETE indexes + indexers:')
        print(' Indexes :', ', '.join(INDEXES))
        print(' Indexers:', ', '.join(INDEXERS))
        resp=input('Type YES to proceed: ')
        if resp.strip()!='YES':
            print('Aborted.')
            sys.exit(0)
    for ixr in INDEXERS:
        del_resource(ep,key,'indexers',ixr)
    # Small delay to allow detach
    time.sleep(2)
    for idx in INDEXES:
        del_resource(ep,key,'indexes',idx)
    print('Deletion phase complete. Now run:')
    print('  python create_indexes.py --container aisearch --prefix S35-Source/ --auto')
