"""Finalize vector coverage for new_cobol_flow_edges_v2.

Problem: Some edge docs lack the has_vector flag entirely (unflagged remainder). Standard count of
has_vector eq false returns zero, but with_vector < total. This script scans all docs and:
  * If has_vector != True and edge_text non-empty -> embed edge_text into edge_vector
  * If edge_text empty/whitespace -> assign zero (or optional sparse sentinel) vector
  * Sets has_vector=True for every processed doc

Idempotent & resumable (simply re-runs scanning pages). Supports dry-run preview.

Usage:
  python finalize_flow_edges_v2_vectors.py --batch 256 --dry-run
  python finalize_flow_edges_v2_vectors.py --batch 256
  python finalize_flow_edges_v2_vectors.py --batch 256 --sentinel-sparse

Environment (auto loaded):
  SEARCH_ENDPOINT / SEARCH_KEY (or AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY)
  AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY (+ deployment vars) OR OPENAI_API_KEY
  EMBEDDINGS_DEPLOYMENT (default text-embedding-3-large)
"""
from __future__ import annotations
import os, json, sys, time, argparse, requests, traceback, hashlib
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'
TEXT_FIELD='edge_text'
VEC_FIELD='edge_vector'
HAS_FIELD='has_vector'
EMB_DEPLOY=os.getenv('EMBEDDINGS_DEPLOYMENT','text-embedding-3-large')
EMB_API_VER=os.getenv('AZURE_OPENAI_API_VERSION','2024-08-01-preview')
MAX_TEXT=int(os.getenv('FLOW_EDGE_EMBED_TEXT_LIMIT','6000'))
DIM_FALLBACK=int(os.getenv('FLOW_EDGE_VECTOR_DIM','3072'))
RETRIES=5

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_page(ep,key, top, skip, orderby=None):
    body={'search':'*','top':top,'select':f'edge_id,{TEXT_FIELD},{HAS_FIELD}'}
    if skip:
        body['skip']=skip
    if orderby:
        body['orderby']=orderby
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=90)
    if r.status_code!=200:
        raise RuntimeError(f'Fetch failed {r.status_code}: {r.text[:180]}')
    return r.json().get('value',[])

def fetch_page_keyset(ep,key, top, last_edge_id):
    # Keyset page using edge_id gt last_edge_id ordering ascending
    filt=None
    body={'search':'*','top':top,'select':f'edge_id,{TEXT_FIELD},{HAS_FIELD}','orderby':'edge_id asc'}
    if last_edge_id:
        # edge_id strictly greater than last (escape single quotes inside the id)
        esc=last_edge_id.replace("'","''")
        body['filter']=f"edge_id gt '{esc}'"
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=90)
    if r.status_code!=200:
        raise RuntimeError(f'Keyset fetch failed {r.status_code}: {r.text[:180]}')
    return r.json().get('value',[])

def upload(ep,key, docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    # Chunk to keep payload sizes friendly
    for i in range(0,len(docs),128):
        payload={'value':docs[i:i+128]}
        tries=0
        while True:
            r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=90)
            if r.status_code in (200,201): break
            tries+=1
            if tries>=3:
                raise RuntimeError(f'Upload failed {r.status_code}: {r.text[:180]}')
            time.sleep(1.5*tries)

def build_embedder():
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key=os.getenv('AZURE_OPENAI_KEY')
    if azure_ep and azure_key:
        def _embed(texts:List[str]):
            url=f"{azure_ep.rstrip('/')}/openai/deployments/{EMB_DEPLOY}/embeddings?api-version={EMB_API_VER}"
            r=requests.post(url,headers={'api-key':azure_key,'Content-Type':'application/json'},json={'input':texts},timeout=120)
            if r.status_code!=200:
                raise RuntimeError(f'Azure embed failed {r.status_code}: {r.text[:160]}')
            data=r.json().get('data',[])
            return [d['embedding'] for d in data]
        return _embed, 'azure'
    # Generic provider
    openai_key=os.getenv('OPENAI_API_KEY')
    base=os.getenv('OPENAI_API_BASE','https://api.openai.com/v1')
    model=os.getenv('EMBED_MODEL') or EMB_DEPLOY
    if openai_key:
        def _embed(texts:List[str]):
            r=requests.post(f"{base.rstrip('/')}/embeddings",headers={'Authorization':f'Bearer {openai_key}','Content-Type':'application/json'},json={'model':model,'input':texts},timeout=120)
            if r.status_code!=200:
                raise RuntimeError(f'OpenAI embed failed {r.status_code}: {r.text[:160]}')
            data=r.json().get('data',[])
            return [d['embedding'] for d in data]
        return _embed, 'openai'
    return None,'none'

def retry_embed(fn,texts):
    last=None
    for i in range(RETRIES):
        try:
            return fn(texts)
        except Exception as e:
            last=e; wait=0.8*(2**i)
            print(f'Embed attempt {i+1} failed: {e} waiting {wait:.2f}s')
            time.sleep(wait)
    raise last

def sparse_sentinel(edge_id:str, dim:int):
    h=hashlib.sha256(edge_id.encode('utf-8')).digest()
    vec=[0.0]*dim
    # map 8 positions
    for i in range(0,32,4):
        pos=int.from_bytes(h[i:i+2],'big') % dim
        mag=int.from_bytes(h[i+2:i+4],'big')/65535.0 * 0.015
        vec[pos]=mag
    return vec

def infer_dim(embed_fn):
    if embed_fn is None: return DIM_FALLBACK
    test=embed_fn(['dim probe'])
    if not test or not test[0]: return DIM_FALLBACK
    return len(test[0])

def main():
    ap=argparse.ArgumentParser(description='Finalize flow edge vectors to 100% coverage')
    ap.add_argument('--batch',type=int,default=256,help='Legacy: batch cap (ignored if --full-page)')
    ap.add_argument('--full-page',action='store_true',help='Process ALL missing docs in each page (embedding chunked)')
    ap.add_argument('--page',type=int,default=512,help='Underlying page fetch size')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--sentinel-sparse',action='store_true',help='Use sparse hashed sentinel instead of all-zero for empty text')
    ap.add_argument('--max-pages',type=int,default=0,help='Stop after this many pages (0=all)')
    ap.add_argument('--auto-keyset-after-skip',type=int,default=90000,help='Switch to keyset pagination when skip >= this value to avoid 100k skip ceiling')
    ap.add_argument('--progress-every',type=int,default=25,help='Every N pages, emit live coverage stats (extra count queries)')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search(); embed_fn, backend=build_embedder()
    print(json.dumps({'backend':backend,'deployment':EMB_DEPLOY,'dry_run':args.dry_run,'page_size':args.page,'batch_cap':args.batch},indent=2))
    dim=infer_dim(embed_fn)
    print(f'Inferred vector dim={dim}')
    skip=0; total_patched=0; pages=0; sampled=0
    using_keyset=False
    last_edge_id=None
    zero=[0.0]*dim
    start=time.time()
    while True:
        if not using_keyset:
            if skip >= args.auto_keyset_after_skip:
                using_keyset=True
                print(f"[pagination] Switching to keyset mode at skip={skip}")
        if using_keyset:
            rows=fetch_page_keyset(ep,key,args.page,last_edge_id)
        else:
            rows=fetch_page(ep,key,args.page,skip,orderby=None)
        if not rows: break
        pages+=1
        work_embed=[]; work_zero=[]
        for r in rows:
            if r.get(HAS_FIELD) is True:
                continue
            txt=(r.get(TEXT_FIELD) or '').strip()
            if sampled<5:
                print(f"Candidate edge_id={r['edge_id']} len={len(txt)} has_vector={r.get(HAS_FIELD)}")
                sampled+=1
            if txt:
                work_embed.append((r['edge_id'], txt[:MAX_TEXT]))
            else:
                work_zero.append(r['edge_id'])
            if (not args.full_page) and (len(work_embed)+len(work_zero) >= args.batch):
                break
        actions=[]
        # chunk embedding calls to manageable size (e.g., 256)
        if work_embed:
            if embed_fn is None:
                for eid,_ in work_embed:
                    vec=sparse_sentinel(eid,dim) if args.sentinel_sparse else zero
                    actions.append({'@search.action':'mergeOrUpload','edge_id':eid,HAS_FIELD:True,VEC_FIELD:vec})
            else:
                CHUNK=256
                for i in range(0,len(work_embed),CHUNK):
                    chunk=work_embed[i:i+CHUNK]
                    embeds=retry_embed(embed_fn,[t for _,t in chunk])
                    for (eid,_),vec in zip(chunk,embeds):
                        actions.append({'@search.action':'mergeOrUpload','edge_id':eid,HAS_FIELD:True,VEC_FIELD:vec})
        for eid in work_zero:
            vec=sparse_sentinel(eid,dim) if args.sentinel_sparse else zero
            actions.append({'@search.action':'mergeOrUpload','edge_id':eid,HAS_FIELD:True,VEC_FIELD:vec})
        if actions and not args.dry_run:
            upload(ep,key,actions)
        total_patched+=len(actions)
        if using_keyset:
            # update last_edge_id to max id in the batch
            if rows:
                last_edge_id = rows[-1]['edge_id']
        print(f"Page {pages} scanned={len(rows)} patched={len(actions)} total_patched={total_patched} mode={'keyset' if using_keyset else 'skip'} skip={skip} last_edge_id={last_edge_id if using_keyset else 'n/a'}")
        if len(rows) < args.page:
            break
        if using_keyset:
            # continue keyset until exhaustion
            pass
        else:
            skip+=args.page
        if args.max_pages and args.max_pages>0 and pages>=args.max_pages:
            break
        if args.progress_every and pages % args.progress_every == 0:
            try:
                # live coverage snapshot
                body_total={'search':'*','top':0,'count':True}
                body_vec={'search':'*','top':0,'count':True,'filter':f'{HAS_FIELD} eq true'}
                h={'api-key':key,'Content-Type':'application/json'}
                total_r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers=h,json=body_total,timeout=60)
                vec_r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers=h,json=body_vec,timeout=60)
                if total_r.status_code==200 and vec_r.status_code==200:
                    total_ct=total_r.json().get('@odata.count',0)
                    vec_ct=vec_r.json().get('@odata.count',0)
                    pct = (vec_ct/total_ct*100.0) if total_ct else 0.0
                    print(f"[progress] pages={pages} embedded_total_session={total_patched} coverage_now={pct:.2f}% ({vec_ct}/{total_ct})")
            except Exception as e:
                print(f"[progress-warn] coverage probe failed: {e}")
    elapsed=time.time()-start
    print(json.dumps({'patched':total_patched,'pages':pages,'elapsed_sec':round(elapsed,2)},indent=2))

if __name__=='__main__':
    main()
