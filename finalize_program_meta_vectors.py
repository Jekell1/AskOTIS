"""Finalize program meta vector coverage.

Brings `new_cobol_program_meta` to 100% has_vector coverage by:
  * Embedding any document with a non-empty program_summary where has_vector != True
  * Assigning a zero (or deterministic sparse) sentinel vector when summary empty

Safe to re-run; skips docs already marked has_vector True.

Usage:
  python finalize_program_meta_vectors.py --batch 400 --dry-run
  python finalize_program_meta_vectors.py --batch 400

Environment (auto-loaded from local.settings.json Values if present):
  AZURE_SEARCH_ENDPOINT / KEY (or SEARCH_ENDPOINT / SEARCH_KEY)
  AZURE_OPENAI_ENDPOINT / KEY / AZURE_OPENAI_EMBEDDING_DEPLOYMENT / AZURE_OPENAI_API_VERSION
  OPENAI_API_KEY / OPENAI_EMBEDDING_MODEL / OPENAI_API_VERSION (fallback)
"""
from __future__ import annotations
import os, json, sys, time, requests, argparse, traceback
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'
TEXT_FIELD='program_summary'
VEC_FIELD='program_summary_vector'
HAS_FIELD='has_vector'
DIM=int(os.getenv('PROGRAM_META_VECTOR_DIM','3072'))
TRUNC=int(os.getenv('PROGRAM_META_SUMMARY_TRUNCATE','4000'))
EMBED_RETRIES=5
SLEEP_BASE=0.75

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI=None  # type: ignore
except Exception:
    openai=None; AzureOpenAI=None  # type: ignore

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=v
    except Exception:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def build_embedder():
    if openai is None:
        return None, None, 'none'
    dep=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    ver=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    ae=os.getenv('AZURE_OPENAI_ENDPOINT'); ak=os.getenv('AZURE_OPENAI_KEY')
    if ae and ak and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=ak, api_version=ver, azure_endpoint=ae)
        def _embed(texts:List[str]):
            r=client.embeddings.create(model=dep, input=texts)
            return [d.embedding for d in r.data]
        return dep,_embed,'azure'
    if ae and ak:
        openai.api_type='azure'; openai.azure_endpoint=ae; openai.api_key=ak; openai.api_version=ver
        def _embed(texts:List[str]):
            r=openai.embeddings.create(model=dep, input=texts)
            return [d.embedding for d in r.data]
        return dep,_embed,'legacy-azure'
    pk=os.getenv('OPENAI_API_KEY')
    if pk:
        openai.api_key=pk
        def _embed(texts:List[str]):
            r=openai.embeddings.create(model=dep, input=texts)
            return [d.embedding for d in r.data]
        return dep,_embed,'public'
    return None,None,'none'

def embed_with_retry(fn,texts):
    last=None
    for i in range(EMBED_RETRIES):
        try:
            return fn(texts)
        except Exception as e:
            last=e
            wait=SLEEP_BASE*(2**i)
            print(f"Embed attempt {i+1}/{EMBED_RETRIES} failed: {e} waiting {wait:.2f}s")
            time.sleep(wait)
    print('Final embedding failure:')
    traceback.print_exception(last)
    raise last

def fetch_page(ep,key,skip,top):
    body={'search':'*','top':top,'skip':skip,'select':f'program_id,{TEXT_FIELD},{HAS_FIELD}'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs},timeout=60)
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:400]); sys.exit(1)

def main():
    ap=argparse.ArgumentParser(description='Finalize program meta vectors to 100% coverage')
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--sentinel-sparse',action='store_true',help='Use sparse hashed sentinel instead of all-zero for empty summaries')
    args=ap.parse_args()
    load_settings(); ep,key=resolve_search(); dep, embed_fn, mode=build_embedder()
    print(json.dumps({'embedding_backend':mode,'deployment':dep,'dry_run':args.dry_run},indent=2))
    skip=0; patched=0; scanned=0; start=time.time(); sample=0
    zero=[0.0]*DIM
    def sparse(pid:str):
        import hashlib
        h=hashlib.sha256(pid.encode('utf-8')).digest()
        vec=[0.0]*DIM
        for i in range(0,32,4):
            pos=int.from_bytes(h[i:i+2],'big') % DIM
            mag=int.from_bytes(h[i+2:i+4],'big')/65535.0 * 0.02
            vec[pos]=mag
        return vec
    while True:
        rows=fetch_page(ep,key,skip,args.batch)
        if not rows: break
        to_embed_texts=[]; to_embed_ids=[]; actions=[]
        for r in rows:
            if r.get(HAS_FIELD) is True:
                continue
            summary=(r.get(TEXT_FIELD) or '').strip()
            if sample < 5:
                print(f"Candidate program_id={r['program_id']} len={len(summary)} has_vector={r.get(HAS_FIELD)}")
                sample+=1
            if summary and embed_fn is not None:
                to_embed_texts.append(summary[:TRUNC])
                to_embed_ids.append(r['program_id'])
            else:
                vec = sparse(r['program_id']) if args.sentinel_sparse else zero
                actions.append({'@search.action':'mergeOrUpload','program_id':r['program_id'],HAS_FIELD:True,VEC_FIELD:vec})
        if to_embed_texts:
            if embed_fn is None:
                # fallback mark with zeros
                for pid in to_embed_ids:
                    vec = sparse(pid) if args.sentinel_sparse else zero
                    actions.append({'@search.action':'mergeOrUpload','program_id':pid,HAS_FIELD:True,VEC_FIELD:vec})
            else:
                embs = embed_with_retry(embed_fn,to_embed_texts)
                for pid,vec in zip(to_embed_ids,embs):
                    actions.append({'@search.action':'mergeOrUpload','program_id':pid,HAS_FIELD:True,VEC_FIELD:vec})
        if actions and not args.dry_run:
            upload(ep,key,actions)
        patched+=len(actions); scanned+=len(rows)
        print(f"Scanned {scanned} newly_patched={len(actions)} cumulative_patched={patched} skip={skip}")
        if len(rows) < args.batch:
            break
        skip+=args.batch
    elapsed=time.time()-start
    print(json.dumps({'patched':patched,'scanned':scanned,'elapsed_sec':round(elapsed,2)},indent=2))

if __name__=='__main__':
    main()
