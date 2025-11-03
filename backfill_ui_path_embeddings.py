"""Backfill embeddings for UI paths into path_vector.

Embedding text = compact representation of program sequence plus basic metrics.

Prereqs:
  Recreate index if path_vector not present.

Usage:
  python backfill_ui_path_embeddings.py [--resume-missing]
"""
from __future__ import annotations
import os, sys, json, argparse, requests, math
from typing import List
from embedding_utils import batch_embed

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_ui_paths'
VECTOR_FIELD='path_vector'
HAS='has_vector'


def load():
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
        raise SystemExit('missing endpoint/key')
    return ep.rstrip('/'), key


def fetch(ep,key,skip,top,resume):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':'path_id,program_sequence_json,length,ui_program_count,score,'+HAS}
    if resume:
        body['filter']=f"({HAS} eq false) or ({HAS} eq null)"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('fetch failed',r.status_code,r.text[:200]); return []
    return r.json().get('value',[])


def make_text(rec):
    seq=json.loads(rec.get('program_sequence_json') or '[]')
    seq_str=' -> '.join(seq)
    return f"PATH: {seq_str} | len={rec.get('length')} ui={rec.get('ui_program_count')} score={rec.get('score')}"[:4000]


def upload(ep,key,batch):
    if not batch: return True
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    acts={'value':[{'@search.action':'mergeOrUpload',**d} for d in batch]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=acts)
    if r.status_code not in (200,201):
        print('upload failed',r.status_code,r.text[:300]); return False
    return True


def main():
    p=argparse.ArgumentParser(description='Backfill UI path embeddings')
    p.add_argument('--resume-missing',action='store_true')
    p.add_argument('--dry-run',action='store_true')
    p.add_argument('--stream-batch-size',type=int,default=256)
    p.add_argument('--embed-batch-size',type=int,default=64)
    args=p.parse_args()
    load(); ep,key=resolve()
    skip=0; processed=0
    while True:
        docs=fetch(ep,key,skip,args.stream_batch_size,args.resume_missing)
        if not docs: break
        skip+=len(docs)
        texts=[make_text(d) for d in docs]
        if args.dry_run:
            print('[DRY] would embed',len(texts),'samples:')
            for t in texts[:2]:
                print('  ',t[:160])
            processed+=len(docs)
            continue
        vectors=batch_embed(texts,batch_size=args.embed_batch_size)
        out=[{'path_id':d['path_id'],VECTOR_FIELD:vec,HAS:True} for d,vec in zip(docs,vectors)]
        ok=upload(ep,key,out)
        processed+=len(out)
        print('Processed',processed,'paths (last',len(out),')')
        if not ok: break
        if len(docs)<args.stream_batch_size: break
    print('DONE. total processed',processed)

if __name__=='__main__':
    main()
