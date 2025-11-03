"""Backfill embeddings for new_cobol_screen_nodes.summary_text into summary_vector.

Prereqs:
  python add_vector_field_screen_nodes.py

Usage:
  python backfill_screen_node_embeddings.py [--resume-missing]
"""
from __future__ import annotations
import os, sys, json, time, argparse, requests
from typing import List
from embedding_utils import batch_embed, provider_info, embed_one

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_screen_nodes'
VECTOR_FIELD='summary_vector'
HAS_FIELD='has_vector'
SELECT_FIELDS='screen_id,summary_text,'+HAS_FIELD

BATCH=64
STREAM=512


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_batch(ep,key,skip:int,top:int,resume_missing:bool):
    headers={'api-key':key,'Content-Type':'application/json'}
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    if resume_missing:
        body={'search':'*','top':top,'skip':skip,'select':SELECT_FIELDS,'filter':f"({HAS_FIELD} eq false) or ({HAS_FIELD} eq null)"}
    else:
        body={'search':'*','top':top,'skip':skip,'select':SELECT_FIELDS}
    r=requests.post(url,headers=headers,json=body)
    if r.status_code!=200:
        print('Fetch failed',r.status_code,r.text[:200]); return []
    return r.json().get('value',[])


def upload(ep,key,docs:List[dict]):
    if not docs: return True
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    actions={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers=headers,json=actions)
    if r.status_code not in (200,201):
        print('Upload failed',r.status_code,r.text[:300]); return False
    return True


def main():
    p=argparse.ArgumentParser(description='Backfill screen node embeddings')
    p.add_argument('--resume-missing',action='store_true',help='Only embed screens missing vectors')
    p.add_argument('--force-reembed',action='store_true',help='Re-embed ALL screens (overwrites existing)')
    p.add_argument('--limit',type=int)
    p.add_argument('--dry-run',action='store_true')
    p.add_argument('--embed-batch-size',type=int,default=BATCH)
    p.add_argument('--stream-batch-size',type=int,default=STREAM)
    args=p.parse_args()
    load_settings(); ep,key=resolve()
    # determine dim via provider_info
    prov=provider_info()
    print('Embedding provider:',prov)
    # Determine native dimension by a one-off embed (fast) then compare to expected 3072
    try:
        test_vec=embed_one('dimension-probe', target_dim=None)
        native_dim=len(test_vec)
        if native_dim!=3072:
            print(f"[WARN] Native embedding dimension {native_dim} != expected 3072; vectors will be padded/truncated.")
    except Exception as e:
        print('[WARN] Could not probe native dimension:',e)
    skip=0
    total_processed=0
    
    # Determine which filter to use
    if args.force_reembed:
        resume_mode = False  # Fetch all screens
        print("Force re-embed mode: Processing ALL screens")
    elif args.resume_missing:
        resume_mode = True  # Only fetch screens without vectors
        print("Resume mode: Processing only screens without vectors")
    else:
        resume_mode = True  # Default: only missing
        print("Default mode: Processing only screens without vectors")
    
    while True:
        batch=fetch_batch(ep,key,skip,args.stream_batch_size,resume_mode)
        if not batch:
            break
        skip+=len(batch)
        if args.limit:
            remaining=args.limit-total_processed
            if remaining<=0:
                break
            batch=batch[:remaining]
        texts=[b.get('summary_text') or '' for b in batch]
        if args.dry_run:
            print('[DRY-RUN] would embed',len(texts))
            total_processed+=len(batch)
            continue
        vectors=batch_embed(texts,batch_size=args.embed_batch_size)
        upload_batch=[{'screen_id':b['screen_id'],VECTOR_FIELD:vec,HAS_FIELD:True} for b,vec in zip(batch,vectors)]
        ok=upload(ep,key,upload_batch)
        total_processed+=len(upload_batch)
        print(f"Processed {total_processed} screen nodes (last batch {len(upload_batch)})")
        if not ok:
            break
        if len(batch)<args.stream_batch_size:
            break
    print('DONE. Total processed:',total_processed)

if __name__=='__main__':
    main()
