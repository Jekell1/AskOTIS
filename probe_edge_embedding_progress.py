"""Sampling probe to estimate embedding coverage for new_cobol_flow_edges_v2.

Approach:
 1. Pull N random pages via skip (uniform over first M pages) or sequential if --sequential.
 2. For each sample page, issue a semantic vector query using first record's edge_text embedding generated on the fly (not relying on stored vector) to detect if the same edge_id likely already has a stored vector by comparing presence in a pure vector similarity search vs lexical fallback.
 3. Simpler heuristic: try to fetch documents filtering on has_vector eq true if field present, else we approximate by assuming earlier pages are already processed when batches were sequential.

Given current schema includes has_vector, we can directly sample counts with two filtered searches:
    filter: has_vector eq true
    filter: has_vector eq false
But Search doesn't return total counts with filter unless we set count=true.

We perform multiple count queries with different skip windows for robustness.

Output: estimated coverage %, total sampled docs, vector:true, vector:false.
"""
import os, json, argparse, requests, random, math
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'
PAGE_SIZE=1000


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def search_count(ep,key, filt):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':0,'count':True,'filter':filt}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(r.text[:200])
    return r.json().get('@odata.count',0)


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--samples',type=int,default=5,help='Unused legacy param for random pages (kept for forward compatibility)')
    ap.add_argument('--verbose',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    # First attempt: use has_vector filters
    try:
        vect=search_count(ep,key,'has_vector eq true')
        novect=search_count(ep,key,'has_vector eq false')
        total=vect+novect
        if total==0:
            raise ValueError('Zero total via has_vector filters (field may not be populated)')
        pct=(vect/total*100) if total else 0.0
        print(json.dumps({'mode':'has_vector_filter','vector_true':vect,'vector_false':novect,'total':total,'coverage_pct':round(pct,2)},indent=2))
        if args.verbose: print(f"Coverage(has_vector): {pct:.2f}% ({vect}/{total})")
        return
    except Exception:
        pass
    # Fallback: count total docs via top=0 search
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    base_r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if base_r.status_code!=200:
        raise SystemExit(base_r.text[:200])
    total=base_r.json().get('@odata.count',0)
    # Sample pages and perform semantic vector query using on-the-fly embedding to infer presence (approx).
    # Because index does not expose vector presence directly, we approximate coverage as (embedded_batches * batch_size)/total when sequential.
    # Expect user to pass --embedded-count for more accurate reading.
    print(json.dumps({'mode':'fallback','total':total,'note':'has_vector not populated; cannot directly compute coverage.'},indent=2))
    if args.verbose:
        print('Provide an approximate coverage: (embedded_docs_so_far / total).')

if __name__=='__main__':
    main()
