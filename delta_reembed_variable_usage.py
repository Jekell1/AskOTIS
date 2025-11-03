"""Delta re-embed variable usage docs whose usage_role changed (or never recorded) relative to last_embedded_usage_role.

Strategy:
  - Query pages selecting variable_id, usage_role, last_embedded_usage_role, usage_summary_vector, has_vector
  - Identify docs where usage_role != last_embedded_usage_role OR usage_summary_vector missing
  - Rebuild summary text (same logic as backfill script) and embed in batches
  - Upload: usage_summary_vector, has_vector=true, last_embedded_usage_role=usage_role

Usage:
  python delta_reembed_variable_usage.py --page 400 --batch 64 --limit 2000
"""
from __future__ import annotations
import os, json, argparse, time, requests
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'
VEC_FIELD='usage_summary_vector'
EMB_DEPLOY=os.getenv('EMBEDDINGS_DEPLOYMENT','text-embedding-3-large')

SELECT_FIELDS='variable_id,usage_role,sample_refs_json,first_write_location,first_ref_program_id,first_ref_line_number,last_ref_program_id,last_ref_line_number,last_embedded_usage_role,has_vector'


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items(): os.environ.setdefault(k,str(v))
    except Exception: pass

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

def fetch_page(ep,key,top,skip):
    body={'search':'*','top':top,'skip':skip,'select':SELECT_FIELDS}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(r.text[:200])
    return r.json().get('value',[])

def embed_texts(base,key,ver,texts:List[str]):
    url=f"{base}/openai/deployments/{EMB_DEPLOY}/embeddings?api-version={ver}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'input':texts},timeout=120)
    if r.status_code!=200: raise SystemExit(f'Embed failed {r.status_code}: {r.text[:200]}')
    return [d['embedding'] for d in r.json()['data']]

def build_summary(doc):
    parts=[f"Variable {doc.get('variable_id')}"]
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
    ap=argparse.ArgumentParser(description='Delta re-embed variable usage summaries')
    ap.add_argument('--page',type=int,default=400)
    ap.add_argument('--batch',type=int,default=64)
    ap.add_argument('--limit',type=int,default=0)
    ap.add_argument('--preview',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve(); base,okey,ver=resolve_openai()
    skip=0; embedded=0; scanned=0; start=time.time()
    while True:
        if args.limit and scanned>=args.limit: break
        page=fetch_page(ep,key,args.page,skip)
        if not page: break
        skip+=len(page)
        # Determine which need re-embed
        candidates=[]
        for d in page:
            scanned+=1
            role=d.get('usage_role')
            last=d.get('last_embedded_usage_role')
            vec_present=bool(d.get('has_vector'))
            if (role and role!=last) or not vec_present:
                candidates.append(d)
            if args.limit and scanned>=args.limit:
                break
        if not candidates:
            if len(page)<args.page: break
            continue
        if len(candidates)>args.batch:
            batch_docs=candidates[:args.batch]
        else:
            batch_docs=candidates
        texts=[build_summary(d) for d in batch_docs]
        embs=embed_texts(base,okey,ver,texts)
        payload=[{'variable_id':d['variable_id'],VEC_FIELD:e,'has_vector':True,'last_embedded_usage_role':d.get('usage_role')} for d,e in zip(batch_docs,embs)]
        if args.preview:
            print(json.dumps({'preview':payload[0]},indent=2)); break
        upload(ep,key,payload)
        embedded+=len(payload)
        if embedded % (args.batch*10)==0:
            print(f"Progress embedded={embedded} scanned={scanned}")
        if len(page)<args.page: break
    dur=time.time()-start
    print(f"Delta embedded={embedded} scanned={scanned} in {dur:.1f}s")

if __name__=='__main__':
    main()
