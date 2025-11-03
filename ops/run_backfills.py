#!/usr/bin/env python
"""Backfill wrapper (spec version).

Purpose:
  Run embedding backfills (missing-only) for the core semantic indexes, then
  enforce a minimum vector coverage threshold.

Targets:
  - new_cobol_program_meta.program_summary_vector
  - new_cobol_copybook_meta.summary_vector
  - new_cobol_program_deps.dependency_blob_vector
  - new_cobol_program_flows.flow_vector
  - new_cobol_symbol_refs.excerpt_vector
  - new_cobol_screen_nodes.summary_vector
  - new_cobol_ui_paths.path_vector (only if field exists)

Args:
  --batch <int>          Embedding batch size hint (passed to scripts / inline) (default 64)
  --min-coverage <float> Required coverage (0-1 or percent >1) (default 0.90)
  --only-index <name>    Limit processing/coverage to a single index
  --dry-run              Show planned actions; still prints current coverage if possible

Exit Codes:
  0 success (all processed indexes >= threshold)
  1 coverage failure
  2 fatal runtime / configuration error

Implementation Notes:
  * Prefers dedicated backfill scripts (idempotent) if present.
  * Falls back to inline embedding using Azure/OpenAI endpoint+key when script absent.
  * Uses has_vector facet where available for efficient coverage calc.
"""
from __future__ import annotations
import os, sys, json, argparse, subprocess, time, requests
from typing import List, Dict, Any, Tuple

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

TARGETS = [
    ('new_cobol_program_meta','program_summary','program_summary_vector','backfill_embeddings_program_meta.py'),
    ('new_cobol_copybook_meta','summary','summary_vector','backfill_embeddings_copybook_meta.py'),
    ('new_cobol_program_deps','dependency_blob','dependency_blob_vector','backfill_embeddings_program_deps.py'),
    ('new_cobol_program_flows','flow_summary','flow_vector','backfill_flow_embeddings.py'),
    ('new_cobol_symbol_refs','excerpt','excerpt_vector','backfill_symbol_ref_embeddings.py'),
    ('new_cobol_screen_nodes','summary_text','summary_vector','backfill_screen_node_embeddings.py'),
    ('new_cobol_ui_paths','guard_summary','path_vector','backfill_ui_path_embeddings.py'),
]

EMBED_ENDPOINT=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_ENDPOINT')
EMBED_KEY=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
EMBED_MODEL=os.getenv('EMBED_MODEL','text-embedding-3-large')

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','OPENAI_API_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key')
        sys.exit(2)
    return ep.rstrip('/'), key

def get_fields(ep,key,index)->set[str]:
    r=requests.get(f"{ep}/indexes/{index}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code!=200:
        return set()
    try:
        return {f['name'] for f in r.json().get('fields',[])}
    except Exception:
        return set()

def attempt_script(script:str,batch:int,dry:bool)->bool:
    if not os.path.isfile(script):
        return False
    # detect arg style
    arg='--batch-size'
    try:
        with open(script,'r',encoding='utf-8',errors='ignore') as f:
            head=f.read(4000)
            if '--batch ' in head:
                arg='--batch'
    except Exception:
        pass
    cmd=[sys.executable, script, arg, str(batch)]
    if dry:
        print('[DRY]', ' '.join(cmd))
        return True
    print('[RUN]', ' '.join(cmd))
    subprocess.run(cmd)
    return True

def count_total_with(ep,key,index)->Tuple[int,int]:
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'search':'*','top':0,'count':True,'facets':'has_vector,count:2'}
    r=requests.post(url,headers=headers,json=payload)
    if r.status_code!=200:
        return 0,0
    data=r.json(); total=data.get('count',0); with_vec=0
    for facet in data.get('facets',{}).get('has_vector',[]):
        if facet.get('value')==True:
            with_vec=facet.get('count',0)
    return total, with_vec

def fetch_missing_batch(ep,key,index,text_field,vec_field,batch)->List[Dict[str,Any]]:
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'search':'*','top':batch,'select':f"{text_field},{vec_field},has_vector",'filter':'has_vector eq false'}
    r=requests.post(url,headers=headers,json=payload)
    if r.status_code!=200:
        return []
    return [d for d in r.json().get('value',[]) if d.get(text_field) and (d.get('has_vector')==False or not d.get(vec_field))]

def embed_texts(texts:List[str])->List[List[float]]:
    if not EMBED_ENDPOINT or not EMBED_KEY:
        raise RuntimeError('Embedding endpoint/key not configured')
    if '.azure.com' in EMBED_ENDPOINT:
        dep=os.getenv('EMBED_DEPLOYMENT') or EMBED_MODEL
        url=f"{EMBED_ENDPOINT.rstrip('/')}/openai/deployments/{dep}/embeddings?api-version=2024-02-15-preview"
        r=requests.post(url,headers={'api-key':EMBED_KEY,'Content-Type':'application/json'},json={'input':texts})
    else:
        url='https://api.openai.com/v1/embeddings'
        r=requests.post(url,headers={'Authorization':f'Bearer {EMBED_KEY}','Content-Type':'application/json'},json={'input':texts,'model':EMBED_MODEL})
    if r.status_code!=200:
        raise RuntimeError(f'Embedding call failed {r.status_code}: {r.text[:160]}')
    data=r.json().get('data',[])
    return [d['embedding'] for d in data]

def merge_vectors(ep,key,index,docs,vec_field,vecs):
    url=f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    acts=[]
    for src,vec in zip(docs,vecs):
        key_field=None
        for cand in ['program_id','copybook_name','ref_id','screen_id','path_id','id']:
            if cand in src:
                key_field=cand; break
        if not key_field: continue
        acts.append({'@search.action':'mergeOrUpload', key_field: src[key_field], vec_field: vec, 'has_vector': True})
    if not acts: return
    for i in range(0,len(acts),500):
        chunk=acts[i:i+500]
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':chunk})
        if r.status_code!=200:
            print('[ERROR] merge chunk', index, r.status_code, r.text[:140])
            break

def process(ep,key,index,text_field,vec_field,script,batch,dry):
    # Skip ui_paths if vector field missing
    fields=get_fields(ep,key,index)
    if vec_field not in fields:
        print(f"[SKIP] {index} missing field {vec_field}")
        return count_total_with(ep,key,index)
    used_script=False
    if script:
        used_script=attempt_script(script,batch,dry)
    # If coverage still not full and script absent or incomplete, do inline top-up
    total,with_vec=count_total_with(ep,key,index)
    if dry:
        print(f"[INFO] {index} coverage after script (approx): {with_vec}/{total}")
    if total and with_vec<total and not dry:
        loops=0
        while with_vec<total and loops<40:
            loops+=1
            docs=fetch_missing_batch(ep,key,index,text_field,vec_field,min(batch,128))
            if not docs: break
            try:
                vecs=embed_texts([ (d.get(text_field) or '')[:8000] for d in docs ])
            except Exception as e:
                print('[ERROR] inline embedding failed', index, e)
                break
            merge_vectors(ep,key,index,docs,vec_field,vecs)
            time.sleep(0.15)
            total,with_vec=count_total_with(ep,key,index)
            if len(docs)<batch: break
    return count_total_with(ep,key,index)

def format_table(rows):
    header=f"{'Index':30} {'Total':>8} {'WithVec':>8} {'Coverage':>10}"
    print(header)
    print('-'*len(header))
    for idx,total,with_vec in rows:
        pct=(with_vec/total) if total else 0
        print(f"{idx:30} {total:8d} {with_vec:8d} {pct*100:9.2f}%")
    print('-'*len(header))

def main():
    ap=argparse.ArgumentParser(description='Run vector backfills & enforce coverage')
    ap.add_argument('--batch',type=int,default=64)
    ap.add_argument('--min-coverage',type=float,default=0.90)
    ap.add_argument('--only-index')
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args()
    load_local(); ep,key=resolve_search()
    threshold=args.min_coverage if args.min_coverage<=1 else args.min_coverage/100.0

    targets=[t for t in TARGETS if (not args.only_index or t[0]==args.only_index)]
    rows=[]
    for index,text_field,vec_field,script in targets:
        total,with_vec=process(ep,key,index,text_field,vec_field,script,args.batch,args.dry_run)
        rows.append((index,total,with_vec))

    print('\nCoverage Summary:')
    format_table(rows)
    failing=[idx for (idx,total,with_vec) in rows if total and (with_vec/total)<threshold]
    if failing:
        print(f"[FAIL] Below threshold ({threshold*100:.2f}%): {', '.join(failing)}")
        sys.exit(1)
    print(f"[OK] All indexes >= {threshold*100:.2f}%")

if __name__=='__main__':
    main()
