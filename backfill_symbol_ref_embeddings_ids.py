"""Backfill symbol ref embeddings for a provided list of ref_ids.

Usage:
  python backfill_symbol_ref_embeddings_ids.py --ids-file missing_symbol_refs.txt --embed-batch 64 --chunk 64
"""
from __future__ import annotations
import os, json, argparse, requests, time
from embedding_utils import batch_embed

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
VEC_FIELD='excerpt_vector'
HAS_FIELD='has_vector'

SELECT='ref_id,excerpt'


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items(): os.environ.setdefault(k,v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch(ep,key,ids):
    if not ids: return []
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    out=[]; CH=32
    for i in range(0,len(ids),CH):
        chunk=ids[i:i+CH]
        flt=' or '.join([f"ref_id eq '{rid}'" for rid in chunk])
        body={'search':'*','filter':f'({flt})','top':len(chunk),'select':SELECT}
        r=requests.post(url,headers=headers,json=body,timeout=60)
        if r.status_code!=200:
            raise SystemExit(r.text[:300])
        out.extend(r.json().get('value',[]))
    return out

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'value':[{'@search.action':'merge','ref_id':d['ref_id'],VEC_FIELD:d[VEC_FIELD],HAS_FIELD:True} for d in docs]}
    r=requests.post(url,headers=headers,json=payload,timeout=120)
    if r.status_code not in (200,201):
        raise SystemExit(r.text[:400])

def main():
    ap=argparse.ArgumentParser(description='Backfill symbol ref embeddings for specific IDs')
    ap.add_argument('--ids-file',required=True)
    ap.add_argument('--chunk',type=int,default=256,help='How many IDs to fetch+embed per outer loop')
    ap.add_argument('--embed-batch',type=int,default=64)
    ap.add_argument('--limit',type=int,default=0)
    args=ap.parse_args(); load(); ep,key=resolve()
    ids=[l.strip() for l in open(args.ids_file,'r',encoding='utf-8').read().splitlines() if l.strip()]
    if args.limit:
        ids=ids[:args.limit]
    start=time.time(); done=0
    while ids:
        current=ids[:args.chunk]; ids=ids[args.chunk:]
        rows=fetch(ep,key,current)
        if not rows:
            continue
        texts=[(r.get('excerpt') or '')[:512] for r in rows]
        vecs=batch_embed(texts,batch_size=args.embed_batch,target_dim=3072)
        for r,v in zip(rows,vecs):
            r[VEC_FIELD]=v
        upload(ep,key,rows)
        done+=len(rows)
        if done % 512==0:
            rate=done/max(0.001,(time.time()-start))
            print(f"Progress embedded={done} rate={rate:.1f}/s remaining={len(ids)}")
    dur=time.time()-start
    print(f"Done embedded={done} in {dur:.1f}s")

if __name__=='__main__':
    main()
