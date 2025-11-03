"""Safer wrapper for backfilling screen node embeddings with coverage summary.

Steps:
 1. Resolve search endpoint/key (env or local.settings.json).
 2. Query count of docs with has_vector false.
 3. If zero -> exit success.
 4. Require embedding credentials (Azure OpenAI preferred, else OpenAI) before proceeding.
 5. Embed summary_text into summary_vector (dimension 3072) in batches.
 6. Print final coverage.
"""
from __future__ import annotations
import os, sys, json, argparse, time, requests
ROOT=os.path.abspath(os.path.join(os.path.dirname(__file__),'..','..'))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)
from secrets_loader import load_secrets
from typing import List, Dict, Any

INDEX='new_cobol_screen_nodes'
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
TARGET_DIM=3072

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    # multi-source load already happened via load_secrets if caller invoked it
    if not ep or not key:
        raise RuntimeError('Missing search endpoint/key')
    return ep.rstrip('/'), key

class Embedder:
    def __init__(self):
        # Attempt to hydrate from local.settings.json before reading env
        try:
            if os.path.isfile('local.settings.json'):
                vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
                for k,v in vals.items():
                    os.environ.setdefault(k,v)
        except Exception:
            pass
        self.azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_AZURE_ENDPOINT')
        self.azure_key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_AZURE_KEY')
        # allow alternate variable naming
        self.azure_deploy=os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT')
        self.openai_key=os.getenv('OPENAI_API_KEY')
        self.openai_model=os.getenv('OPENAI_EMBED_MODEL','text-embedding-3-large')
        if not (self.azure_ep and self.azure_key and self.azure_deploy) and not self.openai_key:
            raise RuntimeError('No embedding credentials (Azure or OpenAI) available.')
    def embed(self, texts:List[str])->List[List[float]]:
        if self.azure_ep and self.azure_key and self.azure_deploy:
            url=f"{self.azure_ep.rstrip('/')}/openai/deployments/{self.azure_deploy}/embeddings?api-version=2024-02-15-preview"
            r=requests.post(url,headers={'api-key':self.azure_key,'Content-Type':'application/json'},json={'input':texts})
            if r.status_code!=200:
                raise RuntimeError(f"Azure embed error {r.status_code}: {r.text[:180]}")
            return [d['embedding'] for d in r.json()['data']]
        # fallback
        url='https://api.openai.com/v1/embeddings'
        r=requests.post(url,headers={'Authorization':f'Bearer {self.openai_key}','Content-Type':'application/json'},json={'model':self.openai_model,'input':texts})
        if r.status_code!=200:
            raise RuntimeError(f"OpenAI embed error {r.status_code}: {r.text[:180]}")
            
        return [d['embedding'] for d in r.json()['data']]

def count_missing(ep,key):
    # facet on has_vector
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'facets':['has_vector,count:2']}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise RuntimeError(f"Facet query failed {r.status_code}: {r.text[:180]}")
    facets=r.json().get('@search.facets',{})
    buckets=facets.get('has_vector',[])
    total_missing=0; total_all=0
    for b in buckets:
        if b.get('value')==False: total_missing=b.get('count',0)
        total_all+=b.get('count',0)
    return total_missing,total_all

def fetch_docs(ep,key, top:int, skip:int):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':top,'skip':skip,'filter':'has_vector eq false','select':'screen_id,summary_text'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise RuntimeError(f"Search fetch failed {r.status_code}: {r.text[:160]}")
    return r.json().get('value',[])

def upload_vectors(ep,key, docs, vectors):
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    actions=[]
    for d,v in zip(docs,vectors):
        # Dimension reconciliation: slice or pad if mismatch
        if len(v)>TARGET_DIM:
            v=v[:TARGET_DIM]
        elif len(v)<TARGET_DIM:
            v=v + [0.0]*(TARGET_DIM-len(v))
        actions.append({'@search.action':'merge','screen_id':d['screen_id'],'summary_vector':v,'has_vector':True})
    for i in range(0,len(actions),500):
        chunk={'value':actions[i:i+500]}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=chunk)
        if r.status_code!=200:
            raise RuntimeError(f"Vector upload failed {r.status_code}: {r.text[:160]}")

def run(batch:int, limit:int|None, dry:bool):
    load_secrets(); ep,key=resolve_search()
    missing,total=count_missing(ep,key)
    if missing==0:
        print('No missing vectors. Coverage 100%. Total docs', total)
        return
    print(f'Missing vectors: {missing} / {total} (coverage {(total-missing)/max(1,total):.2%})')
    emb=Embedder()
    processed=0; skip=0
    while processed < missing:
        remaining=missing-processed
        if limit is not None:
            remaining=min(remaining, limit-processed)
        if remaining<=0: break
        fetch_size=min(batch*4, remaining)
        docs=fetch_docs(ep,key,fetch_size,skip)
        if not docs: break
        texts=[d['summary_text'][:4000] for d in docs]
        if dry:
            print(f"Would embed {len(docs)} docs (processed {processed}) sample='{texts[0][:100] if texts else ''}'")
        else:
            vectors=[]
            for i in range(0,len(texts),batch):
                sub=texts[i:i+batch]
                vectors.extend(emb.embed(sub))
                time.sleep(0.1)
            upload_vectors(ep,key,docs,vectors)
            print(f'Embedded {len(docs)} docs (total {processed+len(docs)})')
        processed+=len(docs)
        skip+=fetch_size
        if limit is not None and processed>=limit:
            break
    missing_after,_=count_missing(ep,key)
    print('Done. Remaining missing:', missing_after)

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--batch',type=int,default=64)
    ap.add_argument('--limit',type=int)
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args()
    try:
        run(args.batch,args.limit,args.dry_run)
    except Exception as e:
        print('[FATAL]',e)
        sys.exit(1)
