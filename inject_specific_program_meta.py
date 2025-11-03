"""Inject or repair specific program_meta documents (e.g., LONPF2, TIM360) with deterministic summaries
and on-the-fly embedding generation if Azure/OpenAI embedding credentials are available.

Usage:
  python inject_specific_program_meta.py --ids LONPF2 TIM360
  python inject_specific_program_meta.py --ids LONPF2 --force-replace

Behavior:
  * Fetch existing docs (to avoid overwriting richer fields unless --force-replace)
  * If missing or forced, construct a synthetic summary using call graph heuristics
    (optionally by querying calls index for outgoing/incoming counts).
  * Generate embedding (program_summary_vector, dim inferred from existing schema: 3072).
  * Upload via mergeOrUpload.

Security / Resilience:
  * Uses retry/backoff for embedding API
  * No secrets hardcoded (reads from local.settings.json or env)
  * Skips embedding if no summary text or no credentials.
"""
from __future__ import annotations
import os, sys, json, time, argparse, requests, traceback
from typing import List, Dict, Any

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
META_INDEX = 'new_cobol_program_meta'
CALLS_INDEX = 'new_cobol_calls'
EMBED_DIM = int(os.getenv('PROGRAM_META_VECTOR_DIM','3072'))
EMBED_RETRIES = 5
EMBED_SLEEP = 0.75

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:  # noqa: BLE001
        AzureOpenAI = None  # type: ignore
except ImportError:
    openai = None  # type: ignore
    AzureOpenAI = None  # type: ignore


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY',
                  'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT',
                  'AZURE_OPENAI_API_VERSION','OPENAI_API_KEY','OPENAI_EMBEDDING_MODEL','OPENAI_API_VERSION']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def build_embedder():
    if openai is None:
        return None
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key=os.getenv('AZURE_OPENAI_KEY')
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key:
        if AzureOpenAI is not None:
            client=AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
            def _embed(texts: List[str]):
                resp=client.embeddings.create(model=deployment, input=texts)
                return [d.embedding for d in resp.data]
            print(f"Using AzureOpenAI embedding deployment={deployment}")
            return _embed
        else:
            openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
            def _embed(texts: List[str]):
                resp=openai.embeddings.create(model=deployment,input=texts)
                return [d.embedding for d in resp.data]
            print(f"Using legacy Azure OpenAI embedding deployment={deployment}")
            return _embed
    pub_key=os.getenv('OPENAI_API_KEY')
    if pub_key:
        openai.api_key=pub_key
        def _embed(texts: List[str]):
            resp=openai.embeddings.create(model=deployment,input=texts)
            return [d.embedding for d in resp.data]
        print(f"Using public OpenAI embedding model={deployment}")
        return _embed
    print('No embedding credentials found; proceeding without vectors.')
    return None


def retry_embed(embed_fn, texts: List[str]):
    last=None
    for attempt in range(EMBED_RETRIES):
        try:
            return embed_fn(texts)
        except Exception as e:  # noqa: BLE001
            last=e
            wait=EMBED_SLEEP * (2**attempt)
            print(f"Embedding attempt {attempt+1} failed: {e}. retry in {wait:.2f}s")
            time.sleep(wait)
    print('Embedding ultimately failed; skipping vectors.')
    if last:
        traceback.print_exception(last)
    return None


def fetch_existing(ep,key,pid:str)->Dict[str,Any]|None:
    url=f"{ep}/indexes/{META_INDEX}/docs/search?api-version={API}"
    body={'search':'*','filter':f"program_id eq '{pid}'",'top':1,'select':'program_id,program_summary,outgoing_count,incoming_count,program_role,ui_flag'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
    if r.status_code!=200:
        print('Fetch existing failed',pid,r.status_code,r.text[:160])
        return None
    vals=r.json().get('value',[])
    return vals[0] if vals else None


def fetch_call_counts(ep,key,pid:str)->tuple[int,int]:
    # Count outgoing
    def _count(filter_expr, field):
        url=f"{ep}/indexes/{CALLS_INDEX}/docs/search?api-version={API}"
        body={'search':'*','filter':filter_expr,'top':0,'count':True,'select':field}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=40)
        if r.status_code!=200:
            return 0
        return int(r.json().get('@odata.count',0))
    outgoing=_count(f"caller_program eq '{pid}'",'callee_program')
    incoming=_count(f"callee_program eq '{pid}'",'caller_program')
    return outgoing,incoming


def synth_summary(pid:str,out_ct:int,in_ct:int)->str:
    parts=[f"Program {pid}"]
    parts.append(f"calls {out_ct} others" if out_ct else 'no outgoing calls')
    parts.append(f"called by {in_ct}" if in_ct else 'not called by others')
    # no depth info readily here (would require more graph queries) – keep concise
    return '; '.join(parts)


def upload(ep,key,docs:List[Dict[str,Any]]):
    if not docs: return
    url=f"{ep}/indexes/{META_INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:200]}")


def main():
    ap=argparse.ArgumentParser(description='Inject or repair specific program_meta docs')
    ap.add_argument('--ids', nargs='+', required=True)
    ap.add_argument('--force-replace', action='store_true', help='Overwrite existing summary/outgoing/incoming counts')
    ap.add_argument('--no-embed', action='store_true', help='Skip embedding even if creds available')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    embed_fn=None if args.no_embed else build_embedder()
    to_upload: List[Dict[str,Any]]=[]
    inject_ids=[pid.upper() for pid in args.ids]
    for pid in inject_ids:
        existing=fetch_existing(ep,key,pid)
        if existing and not args.force_replace:
            print(f"{pid}: already exists; skipping (use --force-replace to override)")
            continue
        out_ct,in_ct=fetch_call_counts(ep,key,pid)
        summary=synth_summary(pid,out_ct,in_ct)
        doc={'program_id':pid,'program_summary':summary,'outgoing_count':out_ct,'incoming_count':in_ct,'ui_flag':False,'program_role':'UNKNOWN'}
        to_upload.append(doc)
    # Embed after summaries assembled (batch for efficiency)
    if embed_fn and to_upload:
        texts=[d['program_summary'] for d in to_upload]
        vecs=retry_embed(embed_fn,texts)
        if vecs and all(vecs):
            for d,v in zip(to_upload,vecs):
                d['program_summary_vector']=v
    upload(ep,key,to_upload)
    print(f"Uploaded/merged {len(to_upload)} program_meta docs.")

if __name__=='__main__':
    main()
