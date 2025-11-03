"""Semantic probe for new_cobol_program_flows index.

Usage:
  python vector_probe_program_flows.py --q "loan payoff flow" --k 5
"""
from __future__ import annotations
import os, json, argparse, requests
from embedding_utils import embed_one

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'
VECTOR_FIELD='flow_vector'


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key


def vector_search(ep,key,vec,k,select):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'vectorQueries':[{'kind':'vector','vector':vec,'k':k,'fields':VECTOR_FIELD}], 'select':select}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise SystemExit(f"Search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])


def main():
    ap=argparse.ArgumentParser(description='Probe program flow semantic search')
    ap.add_argument('--q','--query',dest='query',required=True)
    ap.add_argument('--k',type=int,default=5)
    ap.add_argument('--truncate',type=int,default=160)
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    # Ensure vector matches index dimension (3072)
    vec=embed_one(args.query, target_dim=3072)
    hits=vector_search(ep,key,vec,args.k,'program_id,flow_summary,node_count,edge_count,max_depth,has_cycles')
    for i,h in enumerate(hits,1):
        summ=(h.get('flow_summary') or '')[:args.truncate]
        print(f"[{i}] {h.get('program_id')} nodes={h.get('node_count')} edges={h.get('edge_count')} depth={h.get('max_depth')} cyc={h.get('has_cycles')} :: {summ}")

if __name__=='__main__':
    main()
