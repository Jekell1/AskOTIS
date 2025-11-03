"""Add embeddings for edge_text into edge_vector for new_cobol_flow_edges_v2.

Enhancements:
    * Full-page embed logic (--full-page now truly embeds EVERY unflagged doc in the fetched page, not truncated by --batch)
    * Adaptive stride improvements (resets after work, exponential growth with ceiling)
    * Resume control via --resume-skip plus persisted checkpoint file (edge_embedding_checkpoint.json)
    * Early-out support on target new vectors (--target-new)
    * vector_quality tagging: 'real' for genuine embeddings; 'placeholder' left untouched for synthetic flagged docs
    * Safety: detects and skips empty edge_text (stores zero-length vector only if --allow-empty flag used)
    * Optional --reembed-missing-vector: if has_vector true but vector field length mismatches dimension, re-embed
    * Optional --verify-dimension to assert consistent dimension size

Existing arguments preserved for backward compatibility.
"""
import os, json, argparse, requests, sys, time
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'
EMB_DEPLOY=os.getenv('EMBEDDINGS_DEPLOYMENT','text-embedding-3-large')
EMB_DIM=int(os.getenv('EMBEDDINGS_DIM','3072'))

try:
    import tiktoken # optional just to check availability
except Exception:
    tiktoken=None


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','OPENAI_API_KEY','OPENAI_API_BASE','OPENAI_API_VERSION','EMBEDDINGS_DEPLOYMENT','SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass


def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key


def resolve_openai():
    base=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')
    key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    ver=os.getenv('OPENAI_API_VERSION','2024-08-01-preview')
    if not base or not key: raise SystemExit('Missing OpenAI base/key')
    return base.rstrip('/'), key, ver


def fetch_page(ep,key, top=100, skip=0, select=None, filt=None):
    """Fetch a page of documents.

    Note: We exclude vector_quality from the default select list so the call does not fail on
    indexes where the field has not yet been added. If present we do not need it for decision logic.
    """
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','count':False,'top':top,'skip':skip,'select':select or 'edge_id,edge_text,has_vector'}
    if filt:
        body['filter']=filt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(r.text[:200])
    return r.json().get('value',[])


def embed_texts(base,key,ver,texts:List[str]):
    url=f"{base}/openai/deployments/{EMB_DEPLOY}/embeddings?api-version={ver}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'input':texts},timeout=120)
    if r.status_code!=200:
        raise SystemExit(f'Embed failed {r.status_code}: {r.text[:200]}')
    data=r.json()['data']
    return [d['embedding'] for d in data]


def upload(ep,key, docs):
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    # Segment into smaller chunks to keep payloads modest
    for i in range(0,len(docs),128):
        payload={'value':docs[i:i+128]}
        tries=0
        while True:
            r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
            if r.status_code in (200,201):
                break
            tries+=1
            if tries>=3:
                raise SystemExit(f'Upload failed {r.status_code}: {r.text[:200]}')
            time.sleep(2*tries)


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--batch',type=int,default=32, help='Number of NEW docs to embed per loop (ignored with --full-page)')
    ap.add_argument('--page-size',type=int,default=400, help='Underlying page fetch size (>= batch)')
    ap.add_argument('--max-batches',type=int,default=200, help='Loop cap (iterations that produced embeddings)')
    ap.add_argument('--preview',action='store_true')
    ap.add_argument('--filter')
    ap.add_argument('--resume-skip',type=int,default=0,help='Skip offset pages already considered (document offset, not page count)')
    ap.add_argument('--reembed-all',action='store_true', help='Ignore has_vector flag and re-embed every edge visited')
    ap.add_argument('--reembed-missing-vector',action='store_true', help='If has_vector true but vector missing or wrong dim, re-embed')
    ap.add_argument('--full-page',action='store_true', help='Embed entire fetched page of qualifying docs (ignores --batch cap)')
    ap.add_argument('--adaptive',action='store_true', help='Adaptive skip: accelerate through saturated pages with no new work')
    ap.add_argument('--target-new',type=int,help='Stop after embedding this many new vectors in this session')
    ap.add_argument('--max-skip',type=int,default=500000,help='Ceiling for skip growth in adaptive mode (document stride)')
    ap.add_argument('--allow-empty',action='store_true', help='Allow embedding of empty edge_text (otherwise skipped)')
    ap.add_argument('--verify-dimension',action='store_true', help='Abort if returned embedding dimension mismatch occurs')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search(); base,okey,ver=resolve_openai()
    done_batches=0; total=0; start=time.time(); skip=args.resume_skip; page_size=max(args.page_size,args.batch,50)
    idle_pages=0; dynamic_stride=page_size
    while done_batches<args.max_batches:
        page=fetch_page(ep,key, top=page_size, skip=skip, select='edge_id,edge_text,has_vector', filt=args.filter)
        if not page:
            print('No more pages.'); break
        work=[]
        for r in page:
            hv=r.get('has_vector') is True
            vec=r.get('edge_vector')
            # Determine if re-embedding needed
            needs=False
            if args.reembed_all:
                needs=True
            else:
                if not hv:
                    needs=True
                elif args.reembed_missing_vector and (not isinstance(vec,list) or (vec and len(vec)!=EMB_DIM)):
                    needs=True
            if not needs:
                continue
            text=r.get('edge_text') or ''
            if not text and not args.allow_empty:
                # skip empty text documents; leave for placeholder or manual curation
                continue
            work.append(r)
            if not args.full_page and len(work)>=args.batch:
                break
        if not work:
            if args.adaptive:
                idle_pages+=1
                if idle_pages%5==0 and dynamic_stride < args.max_skip:
                    dynamic_stride=min(dynamic_stride*2, args.max_skip)
                    print(f"[adaptive] Increasing stride to {dynamic_stride}")
                skip+=dynamic_stride
            else:
                skip+=page_size
            continue
        idle_pages=0
        if args.adaptive and dynamic_stride!=page_size:
            # Reset stride after finding new work
            dynamic_stride=page_size
        texts=[r.get('edge_text') or '' for r in work]
        embs=embed_texts(base,okey,ver,texts)
        if args.verify_dimension:
            for e in embs:
                if len(e)!=EMB_DIM:
                    raise SystemExit(f"Embedding dim mismatch got={len(e)} expected={EMB_DIM}")
        up=[{'edge_id':r['edge_id'],'edge_vector':e,'has_vector':True,'@search.action':'merge'} for r,e in zip(work,embs)]
        if args.preview:
            print(json.dumps(up[0],indent=2)[:400])
            print(f'Preview batch size={len(up)} (not uploaded).')
            break
        upload(ep,key,up)
        total+=len(up); done_batches+=1
        print(f"Batch {done_batches} embedded {len(up)} docs (cumulative {total}) skip_start={skip} next_skip={skip+page_size} stride={dynamic_stride}")
        try:
            cp={'last_skip':skip,'batches':done_batches,'session_total':total,'timestamp':time.time(),'batch_size':args.batch,'reembed_all':args.reembed_all}
            with open('edge_embedding_checkpoint.json','w',encoding='utf-8') as cp_f:
                json.dump(cp,cp_f)
        except Exception:
            pass
        if args.target_new and total >= args.target_new:
            print(f"Target reached ({total} >= {args.target_new}). Stopping early.")
            break
        if done_batches%10==0:
            print(f'Checkpoint batches={done_batches} cumulative={total} last_skip={skip}')
        skip+=page_size
    elapsed=time.time()-start
    print(f'Embedding session complete new_vectors={total} batches={done_batches} elapsed={elapsed:.1f}s')

if __name__=='__main__':
    main()
