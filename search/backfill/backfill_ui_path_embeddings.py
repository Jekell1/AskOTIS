"""Backfill embeddings for UI path documents in `new_cobol_ui_paths`.

Embeds `path_json` (or synthesized textual summary) into `path_vector` (dimension 1536 expected).

Usage:
  python search/backfill/backfill_ui_path_embeddings.py --batch 64 --limit 500 --dry-run
  python search/backfill/backfill_ui_path_embeddings.py --batch 128 --where "has_vector eq false" --push

Environment (Azure OpenAI preferred):
  AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT
Fallback (OpenAI):
  OPENAI_API_KEY, OPENAI_EMBED_MODEL (default text-embedding-3-large)

Search service environment:
  AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY
"""
from __future__ import annotations
import os, sys, json, time, argparse, math, requests
# Ensure project root on path for direct invocation
ROOT=os.path.abspath(os.path.join(os.path.dirname(__file__),'..','..'))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)
from secrets_loader import load_secrets
from typing import List, Dict, Any

SEARCH_INDEX="new_cobol_ui_paths"
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
TARGET_DIM=1536

# ---------------- Embedding Provider -----------------
import base64

class Embedder:
    def __init__(self):
        # Centralized secret loading
        load_secrets()
        self.azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_AZURE_ENDPOINT')
        self.azure_key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_AZURE_KEY')
        self.azure_deploy=(os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('AZURE_OPENAI_EMBED_MODEL'))
        self.openai_key=os.getenv('OPENAI_API_KEY')
        self.openai_model=os.getenv('OPENAI_EMBED_MODEL','text-embedding-3-large')
        if not (self.azure_ep and self.azure_key and self.azure_deploy) and not self.openai_key:
            raise RuntimeError('No embedding credentials found (Azure or OpenAI). Provide Azure (AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT) or OpenAI (OPENAI_API_KEY, OPENAI_EMBED_MODEL).')

    def embed(self, texts: List[str]) -> List[List[float]]:
        if self.azure_ep and self.azure_key and self.azure_deploy:
            url=f"{self.azure_ep.rstrip('/')}/openai/deployments/{self.azure_deploy}/embeddings?api-version=2024-02-15-preview"
            headers={'api-key':self.azure_key,'Content-Type':'application/json'}
            resp=requests.post(url,headers=headers,json={'input':texts})
            if resp.status_code!=200:
                raise RuntimeError(f"Azure embedding error {resp.status_code}: {resp.text[:200]}")
            data=resp.json()['data']
            return [d['embedding'] for d in data]
        # OpenAI fallback
        url="https://api.openai.com/v1/embeddings"
        headers={'Authorization':f"Bearer {self.openai_key}", 'Content-Type':'application/json'}
        resp=requests.post(url,headers=headers,json={'model':self.openai_model,'input':texts})
        if resp.status_code!=200:
            raise RuntimeError(f"OpenAI embedding error {resp.status_code}: {resp.text[:200]}")
        data=resp.json()['data']
        return [d['embedding'] for d in data]

# ---------------- Search Helpers -----------------

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise RuntimeError('Missing search endpoint/key env vars')
    return ep.rstrip('/'), key

def fetch_batch(ep,key, where:str|None, top:int, skip:int)->List[Dict[str,Any]]:
    url=f"{ep}/indexes/{SEARCH_INDEX}/docs/search?api-version={API_VERSION}"
    filter_expr=where if where else None
    body={'search':'*','top':top,'skip':skip,'select':'path_id,path_json,start_program_id,end_program_id,hop_count,guard_summary,has_vector'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
    if r.status_code!=200:
        raise RuntimeError(f"Search query error {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def build_text(doc:Dict[str,Any])->str:
    # Prefer a concise textual form: start -> end (hop_count) guard_summary + flattened path_json
    parts=[f"PATH {doc.get('start_program_id')} -> {doc.get('end_program_id')} hops={doc.get('hop_count')}"]
    if doc.get('guard_summary'): parts.append(f"guards: {doc['guard_summary'][:180]}")
    pj=doc.get('path_json')
    try:
        steps=json.loads(pj) if isinstance(pj,str) else pj
        if isinstance(steps,list):
            step_names=[s.get('program') or s.get('screen') or s.get('id') for s in steps if isinstance(s,dict)]
            parts.append('sequence: '+' -> '.join([x for x in step_names if x][:20]))
    except Exception:
        pass
    return '; '.join(parts)[:4000]


def upload_vectors(ep,key, docs:List[Dict[str,Any]], vectors:List[List[float]]):
    url=f"{ep}/indexes/{SEARCH_INDEX}/docs/index?api-version={API_VERSION}"
    actions=[]
    for d,vec in zip(docs,vectors):
        if len(vec)>TARGET_DIM:
            vec=vec[:TARGET_DIM]
        elif len(vec)<TARGET_DIM:
            vec=vec + [0.0]*(TARGET_DIM-len(vec))
        actions.append({'@search.action':'merge', 'path_id':d['path_id'],'path_vector':vec,'has_vector':True})
    for i in range(0,len(actions),500):
        chunk={'value':actions[i:i+500]}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=chunk,timeout=60)
        if r.status_code!=200:
            raise RuntimeError(f"Vector upload failed {r.status_code}: {r.text[:200]}")

# ---------------- Main Backfill Logic -----------------

def backfill(where:str|None, limit:int|None, batch:int, dry_run:bool):
    load_secrets(); ep,key=resolve_search()
    emb=Embedder()
    processed=0; skip=0
    while True:
        remaining=None if limit is None else max(0, limit-processed)
        fetch_size=min(batch*4, remaining) if remaining is not None else batch*4
        if remaining is not None and remaining==0:
            break
        docs=fetch_batch(ep,key,where,fetch_size,skip)
        if not docs:
            break
        # If caller did not supply a filter, safeguard by removing already vectorized docs locally
        if where is None:
            docs=[d for d in docs if not d.get('has_vector')]
            if not docs:
                skip+=len(docs) if docs else fetch_size
                continue
        if limit is not None:
            docs=docs[:remaining]
        texts=[build_text(d) for d in docs]
        if dry_run:
            print(f"Would embed {len(docs)} docs (processed {processed}) sample: {texts[0][:120] if texts else ''}")
        else:
            # Embed in sub-batches of 'batch'
            vectors=[]
            for i in range(0,len(texts),batch):
                sub=texts[i:i+batch]
                vecs=emb.embed(sub)
                vectors.extend(vecs)
                time.sleep(0.1)
            upload_vectors(ep,key,docs,vectors)
            print(f"Embedded {len(docs)} docs; total processed {processed+len(docs)}")
        processed+=len(docs)
        if limit is not None and processed>=limit:
            break
        # Advance skip by number of docs actually retrieved (filter already applied server-side)
        skip+=len(docs)
    print('Backfill complete. Total processed',processed)

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--where', help='OData filter (default (has_vector eq false) or (has_vector eq null))', default=None)
    ap.add_argument('--limit', type=int, help='Limit number of docs processed')
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    ap.add_argument('--dry-run', action='store_true')
    args=ap.parse_args()
    where=args.where or '(has_vector eq false) or (has_vector eq null)'
    try:
        backfill(where, args.limit, args.batch, args.dry_run)
    except Exception as e:
        print('[FATAL]', e)
        sys.exit(1)
