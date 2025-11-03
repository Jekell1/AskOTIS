"""Backfill embeddings for new_cobol_program_deps.dependency_blob.

Usage:
  python search/backfill/backfill_embeddings_program_deps.py --batch 64

Environment expectations:
  SEARCH_ENDPOINT / AZURE_SEARCH_ENDPOINT
  SEARCH_KEY / AZURE_SEARCH_KEY
  (Optional) AZURE_OPENAI_ENDPOINT / OPENAI_ENDPOINT
  (Optional) AZURE_OPENAI_KEY / OPENAI_API_KEY
  (Optional) EMBEDDING_DEPLOYMENT (deployment or model name)

If embedding configuration is missing, script exits with guidance.
"""
from __future__ import annotations
import os, sys, json, argparse, time, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'

def resolve_search():
  ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
  key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
  if not ep or not key:
    print('[FATAL] Missing search endpoint/key'); sys.exit(1)
  return ep.rstrip('/'), key

def resolve_embedding():
  ep=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_ENDPOINT')
  key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
  deployment=os.getenv('EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_DEPLOYMENT') or 'text-embedding-3-large'
  if not ep or not key:
    print('[FATAL] Missing embedding endpoint/key environment variables. Set AZURE_OPENAI_ENDPOINT & AZURE_OPENAI_KEY or OPENAI_* equivalents.'); sys.exit(1)
  return ep.rstrip('/'), key, deployment

def fetch_batch(ep,key,top=100,skip=0,filter_clause=None):
  url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
  body={'search':'*','top':top,'skip':skip,'select':'program_id,dependency_blob,has_vector','count':True}
  if filter_clause: body['filter']=filter_clause
  r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
  if r.status_code!=200:
    print('[FATAL] search failed',r.status_code,r.text[:200]); sys.exit(1)
  data=r.json()
  return data.get('value',[]), data.get('@odata.count')

def get_embedding(ep,key,deployment,texts):
  # Azure OpenAI style
  if '/openai/' not in ep:
    ep=ep.rstrip('/')
  url=f"{ep}/openai/deployments/{deployment}/embeddings?api-version=2024-02-15-preview"
  payload={'input':texts}
  r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
  if r.status_code!=200:
    print('[ERROR] embedding failed',r.status_code,r.text[:200])
    return [None for _ in texts]
  data=r.json().get('data',[])
  vectors=[d.get('embedding') for d in data]
  return vectors

def upsert_vectors(search_ep,search_key,docs):
  if not docs: return
  url=f"{search_ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
  payload={'value':[{'@search.action':'merge',**d} for d in docs]}
  r=requests.post(url,headers={'api-key':search_key,'Content-Type':'application/json'},json=payload,timeout=90)
  if r.status_code not in (200,201):
    print('[ERROR] upsert failed',r.status_code,r.text[:200])

def main():
  ap=argparse.ArgumentParser(description='Backfill embeddings for program deps')
  ap.add_argument('--batch',type=int,default=64)
  ap.add_argument('--max',type=int,default=None,help='Max docs to process')
  ap.add_argument('--filter',help='OData filter override (optional)')
  args=ap.parse_args()
  search_ep,search_key=resolve_search()
  emb_ep,emb_key,deployment=resolve_embedding()
  processed=0; skip=0; page_size=args.batch
  filter_clause=args.filter or "has_vector ne true"
  while True:
    docs,total=fetch_batch(search_ep,search_key,top=page_size,skip=skip,filter_clause=filter_clause)
    if not docs:
      break
    payload_texts=[d['dependency_blob'] for d in docs]
    vectors=get_embedding(emb_ep,emb_key,deployment,payload_texts)
    upserts=[]
    for d,vec in zip(docs,vectors):
      if vec is None: continue
      upserts.append({'program_id':d['program_id'],'dependency_blob_vector':vec,'has_vector':True})
    upsert_vectors(search_ep,search_key,upserts)
    processed+=len(upserts)
    skip+=page_size
    print(f"[INFO] Embedded {processed} / {total if total is not None else '?'} (last page {len(upserts)} updated)")
    if args.max and processed>=args.max: break
    if len(docs)<page_size: break
  print('[OK] Embedding backfill complete')

if __name__=='__main__':
  main()
