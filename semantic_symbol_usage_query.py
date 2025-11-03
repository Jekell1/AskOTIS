"""Semantic + keyword query across symbol refs and variable usage.

Given a natural language query (or raw variable name), we:
  * Embed query (if embedding env configured)
  * Vector search symbol refs (excerpt_vector)
  * Optional: vector search variable usage (usage_vector) if field exists
  * Fallback keyword search if vector query yields no results
  * Aggregate by symbol_name summarizing earliest line & programs

Usage:
  python semantic_symbol_usage_query.py --query "where is claim number validated" --top 15
  python semantic_symbol_usage_query.py --query POLNUM --per-symbol 5
"""
from __future__ import annotations
import os, json, argparse, requests, time, collections
from embedding_utils import embed_one

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
REF_INDEX='new_cobol_symbol_refs'
USAGE_INDEX='new_cobol_variable_usage'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def vector_query(ep,key,index,vector,field,select,top):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'vectorQueries':[{'kind':'vector','vector':vector,'fields':field,'k':top}], 'select':select}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        return []
    return r.json().get('value',[])

def keyword(ep,key,index,query,select,top):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':query,'top':top,'select':select}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return []
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Semantic symbol usage finder')
    ap.add_argument('--query',required=True)
    ap.add_argument('--top',type=int,default=30,help='Total symbol refs to retrieve')
    ap.add_argument('--per-symbol',type=int,default=3,help='Max refs kept per symbol in aggregation')
    ap.add_argument('--usage-top',type=int,default=10)
    args=ap.parse_args(); load(); ep,key=resolve()
    q=args.query.strip()
    try:
        vec=embed_one(q)
    except Exception:
        vec=None
    ref_hits=[]
    if vec:
        ref_hits=vector_query(ep,key,REF_INDEX,vec,'excerpt_vector','ref_id,symbol_name,program_id,line_number,excerpt,paragraph_name',args.top)
    if not ref_hits:
        ref_hits=keyword(ep,key,REF_INDEX,q,'ref_id,symbol_name,program_id,line_number,excerpt,paragraph_name',args.top)
    usage_hits=[]
    if vec:
        # Attempt vector search on variable usage (best effort; field may not exist)
        usage_hits=vector_query(ep,key,USAGE_INDEX,vec,'usage_vector','variable_id,read_count,write_count,total_refs,usage_role',args.usage_top)
    if not usage_hits:
        usage_hits=keyword(ep,key,USAGE_INDEX,q,'variable_id,read_count,write_count,total_refs,usage_role',args.usage_top)
    # Aggregate by symbol_name
    agg=collections.OrderedDict()
    for h in ref_hits:
        name=(h.get('symbol_name') or h.get('variable_id') or '').upper()
        if not name: continue
        lst=agg.setdefault(name,[])
        if len(lst)<args.per_symbol:
            lst.append({k:h.get(k) for k in ['ref_id','program_id','line_number','excerpt','paragraph_name']})
    # Compose report
    report={
        'query':q,
        'ref_hit_count':len(ref_hits),
        'usage_hit_count':len(usage_hits),
        'symbols':[
            {
                'symbol_name':name,
                'sample_refs':refs,
                'earliest_line': min([r.get('line_number') for r in refs if r.get('line_number') is not None] or [None])
            } for name,refs in agg.items()
        ],
        'variable_usage_hits':usage_hits
    }
    print(json.dumps(report,indent=2)[:200000])

if __name__=='__main__':
    main()
