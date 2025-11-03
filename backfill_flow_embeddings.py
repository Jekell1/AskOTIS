"""Backfill or re-embed flow summaries for new_cobol_program_flows.

Provides parity flags with other embedding backfill scripts:
  --force        Re-embed ALL docs regardless of existing has_vector
  --ids P1 P2    Only (re)embed specified program_id values (with or without --force)

Typical use cases:
  python backfill_flow_embeddings.py --ids DAILY LNQUOT
  python backfill_flow_embeddings.py --force --ids DAILY
  python backfill_flow_embeddings.py --force   # full corpus refresh

The index is expected to have fields:
  program_id (key), flow_summary, flow_vector, has_vector

This script only touches flow_vector + has_vector.
"""
from __future__ import annotations
import os, json, argparse, time, requests
from embedding_utils import batch_embed

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'
VECTOR_FIELD='flow_vector'
HAS_FIELD='has_vector'
SUMMARY_FIELD='flow_summary'
VECTOR_DIM=int(os.getenv('FLOW_VECTOR_DIM','3072'))


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass


def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def fetch_batch(ep,key,skip,top, force=False, subset_ids=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    filters=[]
    if not force:
        filters.append(f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))")
    if subset_ids:
        or_list=' or '.join([f"program_id eq '{pid}'" for pid in sorted(subset_ids)])
        if or_list:
            filters.append(f"({or_list})")
    body={'search':'*','top':top,'skip':skip,'select':f'program_id,{SUMMARY_FIELD}'}
    if filters:
        body['filter']=' and '.join(filters)
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Fetch failed {r.status_code}: {r.text[:300]}")
    vals=r.json().get('value',[])
    if subset_ids:
        vals=[v for v in vals if v.get('program_id') in subset_ids]
    return vals


def fetch_by_ids(ep,key, ids):
    if not ids: return []
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    out=[]; lst=list(ids); CHUNK=32
    for i in range(0,len(lst),CHUNK):
        chunk=lst[i:i+CHUNK]
        or_list=' or '.join([f"program_id eq '{pid}'" for pid in chunk])
        body={'search':'*','top':len(chunk),'filter':f'({or_list})','select':f'program_id,{SUMMARY_FIELD}'}
        r=requests.post(url,headers=headers,json=body,timeout=60)
        if r.status_code!=200:
            raise SystemExit(f"ID fetch failed {r.status_code}: {r.text[:200]}")
        out.extend(r.json().get('value',[]))
    return out


def upload(ep,key, docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    for d in docs:
        d['@search.action']='merge'
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs},timeout=120)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")


def main():
    ap=argparse.ArgumentParser(description='Backfill flow summary embeddings (flow_vector)')
    ap.add_argument('--batch',type=int,default=200,help='Search paging size')
    ap.add_argument('--embed-batch',type=int,default=64,help='Embedding micro-batch size')
    ap.add_argument('--limit',type=int,default=0,help='Max docs to (re)embed (0=all)')
    ap.add_argument('--force',action='store_true',help='Re-embed all documents (ignore has_vector)')
    ap.add_argument('--ids',nargs='*',help='Specific program_id values to (re)embed')
    args=ap.parse_args(); load(); ep,key=resolve()

    subset_ids=set(args.ids) if args.ids else None
    start=time.time(); updated=0; scanned=0

    if subset_ids:
        rows=fetch_by_ids(ep,key, subset_ids)
        if not rows:
            print('No matching program_id docs for subset.'); return
        texts=[(r.get(SUMMARY_FIELD) or '')[:800] for r in rows]
        vecs=batch_embed(texts,batch_size=args.embed_batch,target_dim=VECTOR_DIM)
        payload=[{'program_id':r['program_id'],VECTOR_FIELD:v,HAS_FIELD:True} for r,v in zip(rows,vecs)]
        upload(ep,key,payload); updated=len(payload); scanned=len(rows)
    else:
        skip=0
        while True:
            if args.limit and updated>=args.limit: break
            rows=fetch_batch(ep,key, skip, args.batch, force=args.force, subset_ids=subset_ids)
            if not rows: break
            skip+=len(rows)
            if args.limit and (updated+len(rows))>args.limit:
                rows=rows[:args.limit-updated]
            texts=[(r.get(SUMMARY_FIELD) or '')[:800] for r in rows]
            vecs=batch_embed(texts,batch_size=args.embed_batch,target_dim=VECTOR_DIM)
            payload=[{'program_id':r['program_id'],VECTOR_FIELD:v,HAS_FIELD:True} for r,v in zip(rows,vecs)]
            upload(ep,key,payload)
            updated+=len(payload); scanned+=len(rows)
            if scanned % (args.batch*10)==0:
                print(f'Progress scanned={scanned} updated={updated}')
            if len(rows)<args.batch: break
    dur=time.time()-start
    print(f'Flow embedding backfill complete updated={updated} scanned={scanned} force={args.force} subset={(len(subset_ids) if subset_ids else 0)} in {dur:.1f}s')

if __name__=='__main__':
    main()
