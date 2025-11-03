"""Generate natural language summaries for program flow enrichment documents.

Inputs: JSON docs (from retrieve_chat_context or direct search) saved to a file OR fetched live.
Strategy:
  - Use enrichment fields to build narrative: entry points, key fanout hubs, depth, cycles, risk.
  - Optionally list example paths.

Usage:
  python flow_answer_synthesizer.py --in retrieval.json --program PROGRAM1 [--show-paths]
  python flow_answer_synthesizer.py --search PROGRAM1
"""
from __future__ import annotations
import os, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch_program(ep,key,program_id:str):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':program_id,'filter':f"program_id eq '{program_id}'",'top':1,'select':'program_id,node_count,edge_count,max_depth,has_cycles,entry_nodes_json,exit_nodes_json,high_fanout_nodes_json,path_samples_json,risk_score,perform_thru_unexpanded_count'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return None
    vals=r.json().get('value',[])
    return vals[0] if vals else None

def pj(raw):
    if not raw: return []
    try: return json.loads(raw)
    except Exception: return []

def synthesize(doc:dict, show_paths:bool=False)->str:
    entries=pj(doc.get('entry_nodes_json'))
    exits=pj(doc.get('exit_nodes_json'))
    hubs=pj(doc.get('high_fanout_nodes_json'))
    paths=pj(doc.get('path_samples_json'))
    risk=doc.get('risk_score')
    depth=doc.get('max_depth'); edges=doc.get('edge_count'); nodes=doc.get('node_count')
    cycles='contains cycles' if doc.get('has_cycles') else 'acyclic'
    thru=doc.get('perform_thru_unexpanded_count')
    parts=[f"Program {doc.get('program_id')} flow summary:"]
    parts.append(f"{nodes} paragraphs, {edges} perform edges, depth≈{depth}, {cycles}, risk score {risk}.")
    if entries: parts.append(f"Entry points: {', '.join(entries[:5])}{'...' if len(entries)>5 else ''}")
    if exits: parts.append(f"Exit nodes: {', '.join(exits[:5])}{'...' if len(exits)>5 else ''}")
    if hubs: parts.append(f"High fanout hubs: {', '.join(hubs)}")
    if thru: parts.append(f"Unexpanded PERFORM THRU occurrences: {thru}")
    if show_paths and paths:
        pretty=[]
        for p in paths[:4]:
            pretty.append(' -> '.join(p))
        parts.append('Example paths:\n  ' + '\n  '.join(pretty))
    return '\n'.join(parts)

def main():
    ap=argparse.ArgumentParser(description='Flow answer synthesizer')
    ap.add_argument('--in',dest='inp',help='Retrieval JSON file containing indexes.new_cobol_program_flows.vector_hits')
    ap.add_argument('--program',help='Program id to synthesize (used with --in or --search)')
    ap.add_argument('--search',action='store_true',help='Fetch program live from index instead of reading file')
    ap.add_argument('--show-paths',action='store_true')
    args=ap.parse_args(); load_settings()
    doc=None
    if args.search:
        if not args.program: raise SystemExit('--program required with --search')
        ep,key=resolve(); doc=fetch_program(ep,key,args.program)
    else:
        if not (args.inp and args.program): raise SystemExit('Need --in file and --program')
        data=json.load(open(args.inp,'r',encoding='utf-8'))
        hits=(data.get('indexes',{}).get('new_cobol_program_flows') or {}).get('vector_hits') or []
        for h in hits:
            if h.get('program_id')==args.program:
                doc=h; break
    if not doc:
        print('Program not found'); return
    print(synthesize(doc,args.show_paths))

if __name__=='__main__':
    main()
