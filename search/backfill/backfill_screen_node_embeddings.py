"""Backfill embeddings for new_cobol_screen_nodes.summary_text into summary_vector.

Usage:
  python search/backfill/backfill_screen_node_embeddings.py --batch 64
"""
from __future__ import annotations
import os, sys, json, argparse, requests, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_screen_nodes'
EMBED_DIM=int(os.getenv('SCREEN_VECTOR_DIM','3072'))

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY','AZURE_OPENAI_ENDPOINT','OPENAI_ENDPOINT']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key'); sys.exit(1)
    aep=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_ENDPOINT')
    akey=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('EMBEDDING_DEPLOYMENT') or os.getenv('EMBED_DEPLOYMENT')
    if not aep or not akey or not deployment:
        print('[FATAL] Missing embedding endpoint/key/deployment'); sys.exit(1)
    return ep.rstrip('/'), key, aep.rstrip('/'), akey, deployment

def fetch_batch(ep,key,top,skip):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'has_vector:false OR has_vector eq false','top':top,'skip':skip,'queryType':'full','select':'screen_id,summary_text'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('[ERROR] search batch',r.status_code,r.text[:200]); return []
    return r.json().get('value',[])

def embed(aep,akey,deployment,texts:list[str])->list[list[float]]:
    url=f"{aep}/openai/deployments/{deployment}/embeddings?api-version=2024-06-01"
    r=requests.post(url,headers={'api-key':akey,'Content-Type':'application/json'},json={'input':texts})
    if r.status_code!=200:
        print('[ERROR] embed',r.status_code,r.text[:200]); return []
    data=r.json().get('data',[])
    return [d['embedding'] for d in data]

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    actions=[{'@search.action':'mergeOrUpload', **d} for d in docs]
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':actions})
    if r.status_code!=200:
        print('[ERROR] upload vectors',r.status_code,r.text[:200])

def main():
    ap=argparse.ArgumentParser(description='Backfill screen node embeddings')
    ap.add_argument('--batch',type=int,default=64)
    ap.add_argument('--limit',type=int,default=5000)
    args=ap.parse_args(); load_local(); ep,key,aep,akey,deployment=resolve()
    total_embedded=0; skip=0; page=args.batch
    while total_embedded < args.limit:
        docs=fetch_batch(ep,key,page,0)  # always search subset; Azure Search filtering for has_vector false may not page reliably
        if not docs: break
        texts=[(d.get('summary_text') or '')[:4000] for d in docs]
        vecs=embed(aep,akey,deployment,texts)
        out=[]
        for d,v in zip(docs,vecs):
            out.append({'screen_id':d['screen_id'],'summary_vector':v,'has_vector':True})
        upload(ep,key,out)
        total_embedded+=len(out)
        print(f"[INFO] Embedded {total_embedded}")
        if len(docs)<page: break
        time.sleep(0.3)
    print('[OK] Embedding backfill complete')

if __name__=='__main__':
    main()
