"""Backfill embeddings for variable usage docs into usage_summary_vector.

Text basis: symbol_name + program_id + JSON sample (truncated) + top program ids snippet.
"""
from __future__ import annotations
import os, sys, json, time, argparse, requests, traceback

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'
VECTOR_FIELD='usage_summary_vector'
HAS_VECTOR='has_vector'
TEXT_TRUNCATE=int(os.getenv('VARIABLE_USAGE_TEXT_TRUNCATE','3000'))
BATCH=64
EMBED_RETRIES=5
RETRY_SLEEP=0.75

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI=None  # type: ignore
except ImportError:
    openai=None; AzureOpenAI=None  # type: ignore

def load():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def build_client():
    if openai is None: raise RuntimeError('openai not installed')
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key=os.getenv('AZURE_OPENAI_KEY')
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _embed(texts):
            resp=client.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in resp.data]
        print('AzureOpenAI deployment',deployment)
        return deployment,_embed
    if azure_ep and azure_key:
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(texts):
            resp=openai.embeddings.create(model=deployment,input=texts)
            return [d.embedding for d in resp.data]
        print('Legacy Azure embedding',deployment)
        return deployment,_embed
    public=os.getenv('OPENAI_API_KEY')
    if not public: raise RuntimeError('No embedding credentials')
    openai.api_key=public
    def _embed(texts):
        resp=openai.embeddings.create(model=deployment,input=texts)
        return [d.embedding for d in resp.data]
    print('Public OpenAI embedding',deployment)
    return deployment,_embed

def fetch(ep,key,skip,top,missing_only:bool):
    # If has_vector field exists, we can filter for those missing embeddings
    if missing_only:
        body={'search':'*','top':top,'skip':skip,'select':'symbol_name,symbol_id_global,program_id,data_type,read_count,write_count,param_in_count,param_out_count,first_write_program,first_write_location,'+HAS_VECTOR,
              'filter':f"({HAS_VECTOR} eq false) or ({HAS_VECTOR} eq null)"}
    else:
        body={'search':'*','top':top,'skip':skip,'select':'symbol_name,symbol_id_global,program_id,data_type,read_count,write_count,param_in_count,param_out_count,first_write_program,first_write_location,'+HAS_VECTOR}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,batch):
    if not batch: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':batch})
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:300]); sys.exit(1)

def embed_retry(fn,texts):
    last=None
    for i in range(EMBED_RETRIES):
        try:
            return fn(texts)
        except Exception as e:
            last=e; wait=RETRY_SLEEP*(2**i); print('Embed fail',i+1,'/',EMBED_RETRIES,str(e),'wait',f"{wait:.2f}s"); time.sleep(wait)
    print('Final embed failure'); traceback.print_exception(last); raise last

def main():
    ap=argparse.ArgumentParser(description='Backfill usage summary embeddings')
    ap.add_argument('--batch',type=int,default=BATCH)
    ap.add_argument('--limit',type=int,default=0)
    ap.add_argument('--resume-missing',action='store_true',help='Only process docs missing embeddings (has_vector false/null)')
    args=ap.parse_args()
    load(); ep,key=resolve(); deployment,emb=build_client()
    count=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if count.status_code!=200: print('Count failed',count.status_code,count.text[:200]); sys.exit(1)
    total=count.json().get('@odata.count',0)
    target=args.limit if args.limit and args.limit<total else total
    print(f"Total={total} target={target} batch={args.batch} deployment={deployment}")
    skip=0; processed=0; updated=0; start=time.time()
    if args.resume_missing:
        # Loop repeatedly fetching only missing docs (skip always 0 because result set shrinks)
        while True:
            rows=fetch(ep,key,0,args.batch,True)
            if not rows:
                break
            texts=[]; ids=[]; keys=[]
            for r in rows:
                vid=r.get('symbol_name')
                key_id=r.get('symbol_id_global')
                if not vid or not key_id: continue
                if r.get(HAS_VECTOR) is True:
                    continue
                prog=r.get('program_id') or ''
                dtype=r.get('data_type') or ''
                reads=r.get('read_count') or 0
                writes=r.get('write_count') or 0
                pin=r.get('param_in_count') or 0
                pout=r.get('param_out_count') or 0
                first_write_prog=r.get('first_write_program') or ''
                first_write_loc=r.get('first_write_location') or ''
                text=f"Variable: {vid}\nProgram: {prog}\nType: {dtype}\nReads: {reads} Writes: {writes} Params In: {pin} Out: {pout}\nFirst Write: {first_write_prog} @ {first_write_loc}"[:TEXT_TRUNCATE]
                texts.append(text); ids.append(vid); keys.append(key_id)
            if not ids:
                continue
            vecs=embed_retry(emb,texts)
            batch=[{'@search.action':'mergeOrUpload','symbol_id_global':k,VECTOR_FIELD:vec,HAS_VECTOR:True} for k,vec in zip(keys,vecs)]
            upload(ep,key,batch); updated+=len(batch)
            print(f"Resume mode embedded {len(batch)} (cumulative {updated})")
            time.sleep(0.05)
    else:
        while processed<target:
            rows=fetch(ep,key,skip,args.batch,False)
            if not rows: break
            texts=[]; ids=[]; keys=[]
            for r in rows:
                if processed>=target: break
                processed+=1
                vid=r.get('symbol_name')
                key_id=r.get('symbol_id_global')
                if not vid or not key_id: continue
                prog=r.get('program_id') or ''
                dtype=r.get('data_type') or ''
                reads=r.get('read_count') or 0
                writes=r.get('write_count') or 0
                pin=r.get('param_in_count') or 0
                pout=r.get('param_out_count') or 0
                first_write_prog=r.get('first_write_program') or ''
                first_write_loc=r.get('first_write_location') or ''
                text=f"Variable: {vid}\nProgram: {prog}\nType: {dtype}\nReads: {reads} Writes: {writes} Params In: {pin} Out: {pout}\nFirst Write: {first_write_prog} @ {first_write_loc}"[:TEXT_TRUNCATE]
                texts.append(text); ids.append(vid); keys.append(key_id)
            if texts:
                vecs=embed_retry(emb,texts)
                batch=[{'@search.action':'mergeOrUpload','symbol_id_global':k,VECTOR_FIELD:vec,HAS_VECTOR:True} for k,vec in zip(keys,vecs)]
                upload(ep,key,batch); updated+=len(batch)
            skip+=args.batch
            pct=(processed/target*100) if target else 100
            print(f"Processed {processed}/{target} ({pct:.2f}%) updated={updated}")
            time.sleep(0.05)
    print(f"Embedding backfill complete updated={updated} in {time.time()-start:.1f}s")

if __name__=='__main__':
    main()
