import json, os, requests, argparse, sys

API_VERSION = "2024-07-01"
INDEX = "cobol-files"
PAGE = 1000
VECTOR_FIELD = "file_vector"
FLAG_FIELD = "has_vector"

def load_settings():
    try:
        return json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception:
        return {}

def get_ep_key():
    vals = load_settings()
    ep = os.environ.get('SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY') or vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

def fetch(ep,key,skip,select):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={"search":"*","top":PAGE,"skip":skip,"select":select}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--limit',type=int,help='Stop after examining this many docs (for quick spot check)')
    ap.add_argument('--dims',type=int,default=1536,help='Expected vector dimensionality')
    ap.add_argument('--no-select-vector',action='store_true',help='Only select flag to quickly count coverage if vector field is large')
    args=ap.parse_args()
    ep,key=get_ep_key()
    skip=0; total=0; with_vector=0; wrong_dims=0; flagged=0
    select = f"file_id,{FLAG_FIELD}" if args.no_select_vector else f"file_id,{FLAG_FIELD},{VECTOR_FIELD}"
    while True:
        batch=fetch(ep,key,skip,select)
        if not batch: break
        for doc in batch:
            total+=1
            if doc.get(FLAG_FIELD):
                flagged+=1
            vec=doc.get(VECTOR_FIELD)
            if isinstance(vec,list):
                with_vector+=1
                if args.dims and len(vec)!=args.dims:
                    wrong_dims+=1
            elif args.no_select_vector and doc.get(FLAG_FIELD):
                # Assume presence if flagged but vector not selected
                with_vector+=1
            if args.limit and total>=args.limit:
                break
        if args.limit and total>=args.limit:
            break
        skip+=len(batch)
        if len(batch)<PAGE:
            break
    print(f"Scanned docs: {total}")
    print(f"Docs flagged has_vector: {flagged}")
    print(f"Docs with vector list: {with_vector}")
    if wrong_dims:
        print(f"Docs with wrong dims: {wrong_dims}")
    if with_vector and args.dims and wrong_dims==0:
        print("All embedded vectors have expected dimensionality.")
    if with_vector==total:
        print("100% vector coverage.")
    else:
        print(f"Coverage: {with_vector/total*100:.2f}%")

if __name__=='__main__':
    main()
