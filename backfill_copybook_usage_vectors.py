"""Backfill context_vector embeddings for new_cobol_copybook_usage where has_vector == false.

Usage:
  python backfill_copybook_usage_vectors.py --page 500 --batch 64
"""
from __future__ import annotations
import os,json,requests,argparse,time,random
from typing import List
from embedding_utils import batch_embed

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_usage'
VEC_FIELD='context_vector'
DIM=3072

# Index fields confirmed via live schema query; safe to include copybook_name_plain and context_snippet.
SELECT='usage_id,program_id,copybook_name,copybook_name_plain,context_snippet,has_vector'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch_batch(ep,key,top):
    """Fetch up to 'top' docs still needing vectors.

    NOTE: We must NOT use skip-based pagination with a filter that changes as we
    update documents (has_vector flips from false->true) because each update
    reshuffles the remaining result set and causes large portions to be skipped.
    Strategy: repeatedly pull the first page (skip=0) until no documents match.
    This guarantees eventual coverage without complex continuation tokens.
    """
    body={'search':'*','top':top,'select':SELECT,'filter':'has_vector eq false'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        raise SystemExit(r.text[:500])
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    headers={'api-key':key,'Content-Type':'application/json'}
    for i in range(0,len(docs),128):
        payload={'value':[{'@search.action':'merge',**d} for d in docs[i:i+128]]}
        r=requests.post(url,headers=headers,json=payload,timeout=60)
        if r.status_code not in (200,201): raise SystemExit(r.text[:300])

def build_text(d):
    return f"Program {d.get('program_id')} copies {d.get('copybook_name')} snippet: { (d.get('context_snippet') or '')[:900] }"[:1000]

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--page',type=int,default=400,help='Search page size (docs pulled per loop)')
    ap.add_argument('--batch',type=int,default=64,help='Embedding batch size (also upload chunk multiple of 128 internal)')
    ap.add_argument('--preview',action='store_true',help='Show a single transformed doc then exit')
    ap.add_argument('--max',type=int,help='Optional maximum documents to embed (for testing)')
    ap.add_argument('--sleep',type=float,default=0.0,help='Sleep seconds between outer fetch loops to reduce load')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    embedded=0; start=time.time(); loops=0
    while True:
        rows=fetch_batch(ep,key,args.page)
        if not rows:
            break
        loops+=1
        idx=0
        while idx < len(rows):
            batch=rows[idx:idx+args.batch]; idx+=args.batch
            if not batch:
                break
            if args.max and embedded >= args.max:
                print('[INFO] Reached max limit; stopping early')
                rows=[]; break
            texts=[build_text(r) for r in batch]
            attempt=0
            while True:
                try:
                    vecs=batch_embed(texts,target_dim=None,batch_size=len(batch))
                    break
                except Exception as e:
                    attempt+=1
                    if attempt>5: raise
                    back=min(60,2**attempt+random.random())
                    print(f"[WARN] embed error {e} retry {attempt} in {back:.1f}s")
                    time.sleep(back)
            payload=[]
            for r,v in zip(batch,vecs):
                if len(v)!=DIM:
                    if len(v)>DIM: v=v[:DIM]
                    else: v=v+[0.0]*(DIM-len(v))
                payload.append({'usage_id':r['usage_id'],VEC_FIELD:v,'has_vector':True})
            if args.preview:
                print(json.dumps({'preview':payload[0]},indent=2)); return
            upload(ep,key,payload); embedded+=len(payload)
            if embedded % max(256,args.batch*8)==0:
                rate=embedded/max(0.001,(time.time()-start))
                print(f"Progress embedded={embedded} rate={rate:.1f}/s loops={loops}")
        if args.sleep:
            time.sleep(args.sleep)
    dur=time.time()-start
    print(f"Done embedded={embedded} in {dur:.1f}s")

if __name__=='__main__':
    main()
