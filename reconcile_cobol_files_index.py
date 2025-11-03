import os, json, sys, argparse, requests

API='2024-07-01'
INDEX='cobol-files'

"""Reconcile cobol-files index vs canonical shard dataset.

Assumptions:
 - Canonical set = union of shard JSONL parts under local directory (default: files_shards_output)
 - Each line has a unique file_id; total expected = 9951 (or whatever is present).
 - Index currently reports more file_ids (11691) -> identify surplus for optional deletion.

Usage examples:
  python reconcile_cobol_files_index.py --shard-dir files_shards_output --export extras.jsonl
  python reconcile_cobol_files_index.py --shard-dir files_shards_output --delete-extras --batch 500

Deletion safety:
 - Uses '@search.action': 'delete' with only key.
 - Dry run unless --delete-extras flag set.
"""

def load_settings():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep=vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(2)
    return ep.rstrip('/'), key

def load_canonical_ids(shard_dir):
    ids=set()
    if not os.path.isdir(shard_dir):
        print('Shard dir not found:', shard_dir); sys.exit(2)
    for fname in os.listdir(shard_dir):
        if not fname.endswith('.jsonl'): continue
        path=os.path.join(shard_dir,fname)
        with open(path,'r',encoding='utf-8') as f:
            for line in f:
                line=line.strip()
                if not line: continue
                try:
                    obj=json.loads(line)
                except Exception:
                    continue
                fid=obj.get('file_id')
                if fid: ids.add(fid)
    return ids

def fetch_all_index_ids(ep,key):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    seen=set(); top=1000; skip=0
    while True:
        body={"search":"*","select":"file_id","top":top,"skip":skip}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
        if r.status_code!=200:
            print('Fetch error',r.status_code,r.text[:200]); break
        batch=r.json().get('value',[])
        if not batch: break
        for d in batch:
            fid=d.get('file_id')
            if fid: seen.add(fid)
        skip+=len(batch)
        if len(batch)<top: break
    return seen

def delete_ids(ep,key,ids):
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    batch=list(ids)
    for i in range(0,len(batch),500):
        payload={'value':[{'@search.action':'delete','file_id':fid} for fid in batch[i:i+500]]}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload)
        if r.status_code not in (200,201):
            print('Delete error',r.status_code,r.text[:200])
            return False
        print(f"Deleted {i+len(payload['value'])}/{len(batch)}")
    return True

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--shard-dir',default='files_shards_output')
    ap.add_argument('--export',help='Export extras to JSONL before any deletion')
    ap.add_argument('--delete-extras',action='store_true',help='Delete surplus index docs not in canonical set')
    args=ap.parse_args()

    ep,key=load_settings()
    canonical=load_canonical_ids(args.shard_dir)
    print('Canonical shard file_id count:', len(canonical))
    index_ids=fetch_all_index_ids(ep,key)
    print('Index file_id count:', len(index_ids))
    extras=index_ids-canonical
    missing=canonical-index_ids
    print('Surplus (index only) count:', len(extras))
    print('Missing (shard only) count:', len(missing))
    if args.export:
        with open(args.export,'w',encoding='utf-8') as f:
            for fid in sorted(extras):
                f.write(json.dumps({'file_id':fid})+'\n')
        print('Exported surplus file_ids to', args.export)
    if not args.delete_extras:
        print('Dry run complete (no deletions). Use --delete-extras to remove surplus.')
        return
    if extras:
        ok=delete_ids(ep,key,extras)
        if ok:
            print('Deletion completed.')
    else:
        print('No surplus docs to delete.')

if __name__=='__main__':
    main()
