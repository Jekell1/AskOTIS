"""Analyze enrichment coverage for new_cobol_program_flows.

Metrics collected:
  - total docs
  - % with entry nodes / exit nodes
  - % with path samples
  - % with high fanout nodes (>0)
  - average/max node & edge counts
  - perform_thru_unexpanded_count distribution
  - risk_score distribution (+ top N risky programs)
  - condensed vs full mermaid size reduction

Usage:
  python flow_enrichment_coverage.py [--limit 500] [--out summary.json] [--endpoint ... --key ...]

Notes:
  This is a read-only diagnostic script; it does NOT modify the index.
"""
from __future__ import annotations
import os, json, argparse, requests, math, statistics

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'


def load_local_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass


def resolve(ep_arg,key_arg):
    ep=ep_arg or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=key_arg or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def fetch_batch(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={
        'search':'*',
        'top':top,
        'skip':skip,
        'select':'program_id,node_count,edge_count,entry_nodes_json,exit_nodes_json,high_fanout_nodes_json,path_samples_json,perform_thru_unexpanded_count,risk_score,mermaid_flow,condensed_mermaid'
    }
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Search failed {r.status_code}: {r.text[:300]}")
    return r.json().get('value',[])


def parse_json_field(raw):
    if not raw: return []
    try:
        return json.loads(raw)
    except Exception:
        return []


def main():
    ap=argparse.ArgumentParser(description='Program flow enrichment coverage diagnostics')
    ap.add_argument('--endpoint'); ap.add_argument('--key')
    ap.add_argument('--page-size',type=int,default=1000)
    ap.add_argument('--limit',type=int,help='Max docs to scan')
    ap.add_argument('--out')
    args=ap.parse_args(); load_local_settings(); ep,key=resolve(args.endpoint,args.key)
    docs=[]; skip=0
    while True:
        batch=fetch_batch(ep,key,skip,args.page_size)
        if not batch: break
        docs.extend(batch)
        skip+=len(batch)
        if args.limit and len(docs)>=args.limit: break
        if len(batch)<args.page_size: break
    if args.limit: docs=docs[:args.limit]
    total=len(docs)
    if not total:
        print('No docs found.'); return
    with_entries=with_exits=with_paths=with_fanout=0
    node_counts=[]; edge_counts=[]; thrus=[]; risks=[]; reductions=[]
    top_risky=[]
    for d in docs:
        entries=parse_json_field(d.get('entry_nodes_json'))
        exits=parse_json_field(d.get('exit_nodes_json'))
        fanout=parse_json_field(d.get('high_fanout_nodes_json'))
        paths=parse_json_field(d.get('path_samples_json'))
        if entries: with_entries+=1
        if exits: with_exits+=1
        if paths: with_paths+=1
        if fanout: with_fanout+=1
        nc=d.get('node_count') or 0; ec=d.get('edge_count') or 0
        node_counts.append(nc); edge_counts.append(ec)
        thrus.append(d.get('perform_thru_unexpanded_count') or 0)
        r=d.get('risk_score')
        if isinstance(r,(int,float)): risks.append(r)
        full=d.get('mermaid_flow') or ''
        condensed=d.get('condensed_mermaid') or ''
        if full and condensed:
            reductions.append(1 - (len(condensed)/len(full) if len(full)>0 else 0))
        top_risky.append({'program_id':d.get('program_id'),'risk':r,'nodes':nc,'edges':ec})
    top_risky=[t for t in top_risky if isinstance(t['risk'],(int,float))]
    top_risky.sort(key=lambda x:x['risk'],reverse=True)
    summary={
        'total_docs':total,
        'pct_with_entry_nodes':round(with_entries/total*100,2),
        'pct_with_exit_nodes':round(with_exits/total*100,2),
        'pct_with_path_samples':round(with_paths/total*100,2),
        'pct_with_high_fanout':round(with_fanout/total*100,2),
        'avg_node_count':round(statistics.fmean(node_counts),2) if node_counts else 0,
        'max_node_count':max(node_counts) if node_counts else 0,
        'avg_edge_count':round(statistics.fmean(edge_counts),2) if edge_counts else 0,
        'max_edge_count':max(edge_counts) if edge_counts else 0,
        'avg_perform_thru_unexpanded':round(statistics.fmean(thrus),2) if thrus else 0,
        'max_perform_thru_unexpanded':max(thrus) if thrus else 0,
        'risk':{
            'count':len(risks),
            'avg':round(statistics.fmean(risks),3) if risks else 0,
            'p90':round(sorted(risks)[math.floor(0.9*len(risks))-1],3) if risks else 0,
            'p95':round(sorted(risks)[math.floor(0.95*len(risks))-1],3) if risks else 0,
            'max':max(risks) if risks else 0,
        },
        'mermaid_reduction':{
            'sampled':len(reductions),
            'avg_reduction_pct':round(statistics.fmean(reductions)*100,2) if reductions else 0,
            'p90_reduction_pct':round(sorted(reductions)[math.floor(0.9*len(reductions))-1]*100,2) if reductions else 0
        },
        'top_risky':top_risky[:10]
    }
    pretty=json.dumps(summary,indent=2)
    print(pretty)
    if args.out:
        open(args.out,'w',encoding='utf-8').write(pretty)
        print('Wrote',args.out)

if __name__=='__main__':
    main()
