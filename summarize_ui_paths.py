"""Generate human-readable summaries for UI paths and store in path_summary_text.

Approach (Phase 1 - deterministic, no LLM call to keep dependency-free):
  * Convert program_sequence_json into a natural language sentence.
  * Include counts (length, UI program count) and any guards present.
  * Store summary in new field 'path_summary_text'.
  * Optionally (if --reembed) also produce embeddings (reusing path_vector) off the summary text.

If you later want true NL abstractive summaries, you can patch this to call an LLM.

Usage:
  python summarize_ui_paths.py --push [--reembed]
"""
from __future__ import annotations
import os, json, argparse, requests, math
from typing import List
from embedding_utils import batch_embed

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_ui_paths'
SUMMARY_FIELD='path_summary_text'
VECTOR_FIELD='path_vector'  # reuse same vector field if re-embedding
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


def fetch(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':'path_id,program_sequence_json,length,ui_program_count,guards_json,edge_freqs_json,'+HAS}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('fetch failed',r.status_code,r.text[:200]); return []
    return r.json().get('value',[])

def make_summary(rec):
    seq=json.loads(rec.get('program_sequence_json') or '[]')
    length=rec.get('length')
    ui_count=rec.get('ui_program_count')
    guards=json.loads(rec.get('guards_json') or '[]')
    guard_part = ''
    if guards:
        guard_part = ' Conditional branches detected.'
    seq_str=' -> '.join(seq)
    return f"Flow of {length} steps ({ui_count} UI) : {seq_str}.{guard_part}"[:4000]


def upload(ep,key,docs):
    if not docs: return True
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload)
    if r.status_code not in (200,201):
        print('upload failed',r.status_code,r.text[:300]); return False
    return True

if __name__=='__main__':
    ap=argparse.ArgumentParser(description='Summarize UI paths')
    ap.add_argument('--top',type=int,default=500)
    ap.add_argument('--reembed',action='store_true')
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--batch',type=int,default=256)
    ap.add_argument('--embed-batch',type=int,default=64)
    args=ap.parse_args()
    load(); ep,key=resolve()
    skip=0; total=0; out_docs=[]; embed_texts=[]; id_order=[]
    while True:
        batch=fetch(ep,key,skip,args.batch)
        if not batch: break
        skip+=len(batch)
        for rec in batch:
            summ=make_summary(rec)
            doc={'path_id':rec['path_id'],SUMMARY_FIELD:summ}
            if args.reembed:
                embed_texts.append(summ)
                id_order.append(rec['path_id'])
            out_docs.append(doc)
        if len(batch)<args.batch: break
    print('Prepared summaries for',len(out_docs),'paths')
    if args.reembed and embed_texts:
        vecs=batch_embed(embed_texts,batch_size=args.embed_batch)
        # merge vector + has flag into doc map
        vec_map={pid:vec for pid,vec in zip(id_order,vecs)}
        for d in out_docs:
            pid=d['path_id']
            if pid in vec_map:
                d[VECTOR_FIELD]=vec_map[pid]
                d[HAS]=True
    if args.push:
        # Upload in chunks
        for i in range(0,len(out_docs),400):
            up=upload(ep,key,out_docs[i:i+400])
            if not up:
                print('Stopping due to upload error.')
                break
        print('Upload done.')
    else:
        print('--dry-run first doc:', json.dumps(out_docs[0],indent=2)[:400])
