"""Backfill embeddings for new_cobol_ui_paths.path_vector using concatenated normalized path text.

Strategy:
  - For each UI path doc missing vector (has_vector != true), build a textual representation:
      "PATH <start_program_id> -> <program_sequence...> | UI_COUNT=<ui_program_count> LEN=<length>"
  - Batch into embedding requests (size tuned by provider constraints).
  - Write path_vector (float array) + has_vector=true via mergeOrUpload.

Assumes Azure OpenAI embedding deployment or OpenAI API key present.
Environment variables used:
  OPENAI_API_KEY or AZURE_OPENAI_KEY / AZURE_OPENAI_ENDPOINT / OPENAI_EMBED_DEPLOYMENT
  Or FALLBACK_EMBED_MODEL (default text-embedding-3-large) for OSS-style naming.

Usage:
  python backfill_ui_path_vectors.py --batch-size 128 --top 2000 --preview
"""
from __future__ import annotations
import os, sys, json, argparse, time, math, requests
from typing import List, Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_ui_paths'
DEFAULT_MODEL = os.getenv('FALLBACK_EMBED_MODEL','text-embedding-3-large')
VECTOR_DIM = int(os.getenv('UI_PATH_VECTOR_DIM', '3072'))  # must match index schema

# ------------- helpers -------------

def load_local_settings():
    """Load local.settings.json Values into environment (non-destructively).

    Previous implementation only loaded a narrow allow‑list which omitted
    AZURE_OPENAI_* keys. That caused the script to incorrectly fall back to
    public OpenAI with an Azure key (leading to 401). We now load all keys
    so Azure embedding config is honored. We still avoid overwriting any
    variable the process already has (allows explicit overrides).
    """
    try:
        with open('local.settings.json','r') as f:
            vals = json.load(f).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k] = v
    except FileNotFoundError:
        print('[warn] local.settings.json not found; relying on ambient env')
    except Exception as e:
        print(f"[warn] Failed loading local.settings.json: {e}")

def resolve_search():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def resolve_embed():
    """Resolve embedding provider configuration.

    Supports multiple synonymous env var names so local.settings.json variants work:
      Deployment / model aliases checked in order:
        OPENAI_EMBED_DEPLOYMENT, EMBEDDING_DEPLOYMENT,
        AZURE_OPENAI_EMBED_DEPLOYMENT, AZURE_OPENAI_EMBEDDING_DEPLOYMENT,
        AZURE_OPENAI_EMBED_MODEL, AZURE_OPENAI_EMBEDDING_MODEL,
        OPENAI_EMBED_MODEL, OPENAI_EMBEDDING_MODEL.
    """
    # Collect possible deployment / model names
    deployment = (
        os.getenv('OPENAI_EMBED_DEPLOYMENT') or
        os.getenv('EMBEDDING_DEPLOYMENT') or
        os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or
        os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or
        os.getenv('AZURE_OPENAI_EMBED_MODEL') or
        os.getenv('AZURE_OPENAI_EMBEDDING_MODEL') or
        os.getenv('OPENAI_EMBED_MODEL') or
        os.getenv('OPENAI_EMBEDDING_MODEL') or
        None
    )

    # Azure OpenAI first (only if endpoint + key + deployment present)
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    if azure_ep and azure_key and deployment:
        print(f"[info] Using Azure OpenAI embedding deployment='{deployment}' (sanitized)")
        return 'azure', {'endpoint': azure_ep.rstrip('/'), 'key': azure_key, 'deployment': deployment}
    elif azure_ep or azure_key:
        # Partial azure config present but not usable
        print(f"[info] Azure OpenAI partially configured (endpoint? {bool(azure_ep)} key? {bool(azure_key)} deployment? {bool(deployment)}) -> falling back to public OpenAI if possible")

    # Public OpenAI fallback
    okey = os.getenv('OPENAI_API_KEY')
    if okey:
        model = deployment or DEFAULT_MODEL
        print(f"[info] Using OpenAI public embedding model='{model}' (sanitized)")
        return 'openai', {'api_key': okey, 'model': model}

    print('[FATAL] No embedding configuration found (set Azure or OpenAI embedding env vars).')
    sys.exit(1)

# ------------- search fetch -------------

def search_batch(ep, key, skip, top, filter_expr=None, select=None):
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    payload={'search':'*','top':top,'skip':skip}
    if select: payload['select']=select
    if filter_expr: payload['filter']=filter_expr
    r=requests.post(url, headers=headers, json=payload)
    if r.status_code!=200:
        raise RuntimeError(f"search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

# ------------- embedding providers -------------

def embed_texts(provider, cfg, texts: List[str]) -> List[List[float]]:
    import time
    max_retries = 5
    if provider=='azure':
        url = f"{cfg['endpoint']}/openai/deployments/{cfg['deployment']}/embeddings?api-version=2024-02-15-preview"
        headers={'api-key': cfg['key'],'Content-Type':'application/json'}
        for attempt in range(max_retries):
            try:
                r=requests.post(url, headers=headers, json={'input': texts}, timeout=120)
                if r.status_code==200:
                    data=r.json()
                    return [d['embedding'] for d in data.get('data',[])]
                elif r.status_code == 429 and attempt < max_retries - 1:
                    wait_time = (2 ** attempt) * 0.5
                    print(f"  Rate limit, retry {attempt+1}/{max_retries}, waiting {wait_time}s...")
                    time.sleep(wait_time)
                else:
                    raise RuntimeError(f"azure embed {r.status_code}: {r.text[:200]}")
            except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
                if attempt < max_retries - 1:
                    wait_time = (2 ** attempt) * 0.5
                    print(f"  Network error retry {attempt+1}/{max_retries}: {str(e)[:100]}, waiting {wait_time}s...")
                    time.sleep(wait_time)
                else:
                    raise
    if provider=='openai':
        url = 'https://api.openai.com/v1/embeddings'
        headers={'Authorization': f"Bearer {cfg['api_key']}", 'Content-Type':'application/json'}
        for attempt in range(max_retries):
            try:
                r=requests.post(url, headers=headers, json={'input': texts, 'model': cfg['model']}, timeout=120)
                if r.status_code==200:
                    data=r.json()
                    return [d['embedding'] for d in data.get('data',[])]
                elif r.status_code == 429 and attempt < max_retries - 1:
                    wait_time = (2 ** attempt) * 0.5
                    print(f"  Rate limit, retry {attempt+1}/{max_retries}, waiting {wait_time}s...")
                    time.sleep(wait_time)
                else:
                    raise RuntimeError(f"openai embed {r.status_code}: {r.text[:200]}")
            except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
                if attempt < max_retries - 1:
                    wait_time = (2 ** attempt) * 0.5
                    print(f"  Network error retry {attempt+1}/{max_retries}, waiting {wait_time}s...")
                    time.sleep(wait_time)
                else:
                    raise
    raise ValueError('Unknown provider')

# ------------- ingestion -------------

def upload_vectors(ep, key, docs: List[Dict[str,Any]]):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    actions=[{'@search.action':'mergeOrUpload', **d} for d in docs]
    import time
    for i in range(0,len(actions),500):
        batch = actions[i:i+500]
        max_retries = 5
        for attempt in range(max_retries):
            try:
                r=requests.post(url, headers=headers, json={'value': batch}, timeout=120)
                if r.status_code==200:
                    break
                elif attempt < max_retries - 1:
                    wait_time = (2 ** attempt) * 0.5
                    print(f"  Upload retry {attempt+1}/{max_retries} (status {r.status_code}), waiting {wait_time}s...")
                    time.sleep(wait_time)
                else:
                    raise RuntimeError(f"upload fail {r.status_code}: {r.text[:200]}")
            except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
                if attempt < max_retries - 1:
                    wait_time = (2 ** attempt) * 0.5
                    print(f"  Network error retry {attempt+1}/{max_retries}: {str(e)[:100]}, waiting {wait_time}s...")
                    time.sleep(wait_time)
                else:
                    raise

# ------------- main -------------

def build_text(doc: Dict[str,Any]) -> str:
    seq_json = doc.get('program_sequence_json') or '[]'
    try:
        seq = json.loads(seq_json)
    except Exception:
        seq=[]
    start = doc.get('start_program_id') or (seq[0] if seq else '')
    leaf = doc.get('end_program_id') or (seq[-1] if seq else '')
    ui_cnt = doc.get('ui_program_count') or 0
    length = doc.get('length') or len(seq)
    path_str = ' -> '.join(seq)
    return f"PATH {start} => {path_str} | START={start} END={leaf} UI={ui_cnt} LEN={length}"[:4000]

def main():
    ap = argparse.ArgumentParser(description='Backfill UI path embeddings.')
    ap.add_argument('--batch-size', type=int, default=128)
    ap.add_argument('--top', type=int, default=5000, help='Max docs to process')
    ap.add_argument('--preview', action='store_true')
    ap.add_argument('--force', action='store_true', help='Re-embed all docs ignoring has_vector flag')
    args = ap.parse_args()
    load_local_settings()
    ep,key=resolve_search()
    provider,cfg=resolve_embed()

    processed=0
    skip=0
    to_process=[]
    select='path_id,program_sequence_json,start_program_id,end_program_id,ui_program_count,length,has_vector'
    while processed < args.top:
        batch = search_batch(ep,key, skip, 1000, select=select)
        if not batch:
            break
        for b in batch:
            if not args.force and b.get('has_vector') is True:
                continue
            to_process.append(b)
            if len(to_process) >= args.top:
                break
        if len(batch)<1000:
            break
        skip += 1000
    if not to_process:
        print('No docs needing vectors.')
        return
    print(f"Preparing embeddings for {len(to_process)} docs (provider={provider})")
    if args.preview:
        for d in to_process[:3]:
            print(build_text(d))
        return

    total=len(to_process)
    for i in range(0,total,args.batch_size):
        chunk=to_process[i:i+args.batch_size]
        texts=[build_text(d) for d in chunk]
        embeds=embed_texts(provider,cfg,texts)
        if not embeds:
            print('[WARN] Empty embedding batch, aborting.')
            break
        # Trim/pad to target dim (defensive)
        fixed=[]
        for e in embeds:
            if len(e) > VECTOR_DIM:
                e = e[:VECTOR_DIM]
            elif len(e) < VECTOR_DIM:
                e = e + [0.0]*(VECTOR_DIM-len(e))
            fixed.append(e)
        upload_payload=[]
        for d,e in zip(chunk,fixed):
            upload_payload.append({'path_id': d['path_id'], 'path_vector': e, 'has_vector': True})
        upload_vectors(ep,key,upload_payload)
        print(f"Upserted vectors {i+len(chunk)}/{total}")
        time.sleep(0.3)
    print('Completed embedding backfill.')

if __name__ == '__main__':
    main()
