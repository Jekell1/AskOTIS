"""Backfill usage_summary_vector for new_cobol_variable_usage.

Strategy:
  - For docs missing has_vector or usage_summary_vector (null/empty), create a textual summary combining:
      * variable_id
      * usage_role
      * first_write_location (if present)
      * first_ref_program_id:line, last_ref_program_id:line
      * top 3 sample refs (program + direction + line)
  - Embed with embeddings deployment (EMBEDDINGS_DEPLOYMENT) and store vector.
  - Set has_vector=true.

Usage:
  python backfill_variable_usage_vectors.py --batch 128 --page 256
"""
from __future__ import annotations
import os, json, argparse, time, requests
from secrets_loader import load_secrets
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'
VEC_FIELD='usage_summary_vector'
HAS='has_vector'
EMB_DEPLOY=os.getenv('EMBEDDINGS_DEPLOYMENT','text-embedding-3-large')
MISSING_FILTER="(has_vector eq false) or (has_vector eq null)"
FALLBACK_PREFIX="FALLBACK USAGE SUMMARY:"


def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def resolve_openai():
    base=os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')
    key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    ver=os.getenv('OPENAI_API_VERSION','2024-08-01-preview')
    if not base or not key: raise SystemExit('Missing OpenAI base/key')
    return base.rstrip('/'), key, ver

def fetch_page(ep,key,top,skip, filter_expr: str | None = None):
    body={'search':'*','top':top,'skip':skip,'select':'symbol_id_global,usage_role,sample_refs_json,first_write_location,first_ref_program_id,first_ref_line_number,last_ref_program_id,last_ref_line_number,has_vector'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(r.text[:200])
    return r.json().get('value',[])

def embed_texts(base,key,ver,texts:List[str]):
    url=f"{base}/openai/deployments/{EMB_DEPLOY}/embeddings?api-version={ver}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'input':texts},timeout=120)
    if r.status_code!=200: raise SystemExit(f'Embed failed {r.status_code}: {r.text[:200]}')
    return [d['embedding'] for d in r.json()['data']]

def build_summary(doc):
    parts=[f"Variable {doc.get('symbol_id_global')}"]
    role=doc.get('usage_role');
    if role: parts.append(f"Role {role}")
    fw=doc.get('first_write_location')
    if fw: parts.append(f"FirstWrite {fw}")
    frp=doc.get('first_ref_program_id'); frl=doc.get('first_ref_line_number')
    lrp=doc.get('last_ref_program_id'); lrl=doc.get('last_ref_line_number')
    if frp and frl is not None: parts.append(f"FirstRef {frp}:{frl}")
    if lrp and lrl is not None: parts.append(f"LastRef {lrp}:{lrl}")
    # sample refs
    sample=[]
    try:
        if doc.get('sample_refs_json'):
            sample=json.loads(doc['sample_refs_json'])
    except Exception:
        sample=[]
    for s in sample[:3]:
        parts.append(f"Sample {s.get('program')} {s.get('direction')} line {s.get('line')}")
    return ' | '.join(parts)[:800]

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    for d in docs: d['@search.action']='merge'
    for i in range(0,len(docs),128):
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs[i:i+128]},timeout=60)
        if r.status_code not in (200,201): raise SystemExit(r.text[:200])

def main():
    ap=argparse.ArgumentParser(description='Backfill usage summary embeddings for variable usage index')
    ap.add_argument('--page',type=int,default=400)
    ap.add_argument('--batch',type=int,default=64)
    ap.add_argument('--limit',type=int,default=0)
    ap.add_argument('--preview',action='store_true')
    ap.add_argument('--all', dest='missing_only', action='store_false', help='Process all docs (not just missing)')
    ap.add_argument('--no-fallback-empty', dest='fallback_empty', action='store_false', help='Disable fallback embedding of empty usage summaries')
    ap.set_defaults(missing_only=True)
    ap.set_defaults(fallback_empty=True)
    args=ap.parse_args(); load_secrets(); ep,key=resolve(); base,okey,ver=resolve_openai()
    skip=0; total=0; embedded=0; start=time.time()
    filter_expr = MISSING_FILTER if args.missing_only else None
    while True:
        if args.limit and total>=args.limit: break
        page=fetch_page(ep,key,args.page,skip, filter_expr=filter_expr)
        if not page: break
        skip+=len(page)
        # Collect all docs needing embedding in this page
        pending=[d for d in page if (not d.get('has_vector')) or (not args.missing_only)]
        if not pending:
            continue
        idx=0
        while idx < len(pending):
            batch_docs=pending[idx:idx+args.batch]
            idx+=args.batch
            texts=[]
            for d in batch_docs:
                summary=build_summary(d)
                if not summary and args.fallback_empty:
                    summary=f"{FALLBACK_PREFIX} {d.get('symbol_id_global')}"[:800]
                texts.append(summary)
            embs=embed_texts(base,okey,ver,texts)
            payload=[{'symbol_id_global':d['symbol_id_global'],VEC_FIELD:e,HAS:True,'last_embedded_usage_role':d.get('usage_role')} for d,e in zip(batch_docs,embs)]
            if args.preview:
                print(json.dumps({'preview':payload[0]},indent=2)); return
            upload(ep,key,payload)
            embedded+=len(payload); total+=len(batch_docs)
            if embedded % (args.batch*10)==0:
                print(f"Progress embedded={embedded} scanned={total}")
        if len(page)<args.page: break
    dur=time.time()-start
    print(f"Done embedded={embedded} scanned={total} in {dur:.1f}s")

if __name__=='__main__':
    main()
