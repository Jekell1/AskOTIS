"""Embed any remaining program_deps docs missing has_vector (regardless of blob length).

This is a complementary finisher to prior backfill and short-blob patch scripts.
It scans the entire `new_cobol_program_deps` index and for each document where
`has_vector` is not True, it either embeds the `dependency_blob` (if non-empty)
or writes a sentinel zero vector (length DIM). This should drive coverage to 100%.

Usage:
  python finalize_program_deps_remaining.py --batch 600
  python finalize_program_deps_remaining.py --dry-run

Exit codes:
  0 success (even if nothing to do)
  1 configuration / HTTP errors
"""
from __future__ import annotations
import os, json, sys, argparse, time, requests
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'
VECTOR_FIELD='dependency_blob_vector'
HAS_FIELD='has_vector'
DIM=int(os.getenv('PROGRAM_DEPS_VECTOR_DIM','3072'))
TRUNCATE=int(os.getenv('PROGRAM_DEPS_TEXT_TRUNCATE','4000'))

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
    except Exception: pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def resolve_embed():
    # Minimal repeated logic (no dependency on earlier scripts at runtime)
    try:
        import openai  # type: ignore
        try:
            from openai import AzureOpenAI  # type: ignore
        except Exception:  # noqa: BLE001
            AzureOpenAI=None
    except Exception:
        openai=None; AzureOpenAI=None  # type: ignore
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key=os.getenv('AZURE_OPENAI_KEY')
    if azure_ep and azure_key and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _embed(texts:List[str]):
            resp=client.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in resp.data]
        return deployment,_embed,'azure'
    if azure_ep and azure_key and openai is not None:  # legacy
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(texts:List[str]):
            resp=openai.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in resp.data]
        return deployment,_embed,'legacy-azure'
    public=os.getenv('OPENAI_API_KEY')
    if public and openai is not None:
        openai.api_key=public
        def _embed(texts:List[str]):
            resp=openai.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in resp.data]
        return deployment,_embed,'public'
    return None,None,'none'

def fetch(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':f'program_id,dependency_blob,{HAS_FIELD}'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Fetch error', r.status_code, r.text[:160], file=sys.stderr); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs},timeout=60)
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:160], file=sys.stderr); sys.exit(1)

def main():
    ap=argparse.ArgumentParser(description='Finalize ANY remaining missing vectors in program deps')
    ap.add_argument('--batch',type=int,default=600)
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--embed',action='store_true', help='Attempt to embed non-empty blobs (default if embedding creds resolved)')
    ap.add_argument('--sentinel',action='store_true', help='Force sentinel zero vector instead of embedding')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search(); deployment, embed_fn, mode=resolve_embed()
    if not args.embed and not args.sentinel:
        args.embed = bool(embed_fn)
        if not embed_fn:
            args.sentinel=True
    zero=[0.0]*DIM
    skip=0; patched=0; scanned=0; start=time.time(); sample=0
    print(json.dumps({'embedding_mode':mode,'deployment':deployment,'embed':args.embed,'sentinel':args.sentinel,'dry_run':args.dry_run},indent=2))
    while True:
        rows=fetch(ep,key,skip,args.batch)
        if not rows: break
        need=[]
        texts=[]; ids=[]
        for r in rows:
            if r.get(HAS_FIELD) is True:
                continue
            blob=(r.get('dependency_blob') or '')
            if sample<5:
                print(f"Remaining candidate program_id={r['program_id']} blob_len={len(blob)}")
                sample+=1
            if args.embed and not args.sentinel and blob.strip():
                texts.append(blob[:TRUNCATE])
                ids.append(r['program_id'])
            else:
                need.append({'@search.action':'mergeOrUpload','program_id':r['program_id'],HAS_FIELD:True,VECTOR_FIELD:zero})
        # Embed chunked
        if texts:
            for i in range(0,len(texts),64):
                chunk=texts[i:i+64]; idchunk=ids[i:i+64]
                if embed_fn:
                    embs=embed_fn(chunk)
                else:
                    embs=[zero]*len(chunk)  # fallback safety
                for pid,vec in zip(idchunk,embs):
                    need.append({'@search.action':'mergeOrUpload','program_id':pid,HAS_FIELD:True,VECTOR_FIELD:vec})
        if need and not args.dry_run:
            upload(ep,key,need)
        patched+=len(need); scanned+=len(rows)
        print(f"Scanned {scanned} newly_patched={len(need)} cumulative_patched={patched} skip={skip}")
        if len(rows)<args.batch:
            break
        skip+=args.batch
    elapsed=time.time()-start
    print(json.dumps({'patched':patched,'scanned':scanned,'elapsed_sec':round(elapsed,2)},indent=2))

if __name__=='__main__':
    main()
