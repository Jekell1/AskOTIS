"""Probe a single program's flow document in new_cobol_program_flows.

Usage:
  python probe_program_flow.py --program CRNOR2
  python probe_program_flow.py --programs CRNOR2,DEFI01 --show-edges 12

Outputs key metrics: node/edge counts, depth, cycles, risk, high fanout nodes, sample paths.
Optionally prints a trimmed edge list and top fanout adjacency.
"""
import os, json, argparse, requests, math
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'

FIELDS = 'program_id,node_count,edge_count,max_depth,has_cycles,risk_score,flow_summary,flow_nodes_json,flow_edges_json,high_fanout_nodes_json,entry_nodes_json,exit_nodes_json,path_samples_json'


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key


def fetch_program(ep,key,pid):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={
        'search': pid,
        'filter': f"program_id eq '{pid}'",
        'top': 1,
        'select': FIELDS
    }
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Search failed {r.status_code}: {r.text[:200]}")
    vals=r.json().get('value',[])
    return vals[0] if vals else None


def summarize(doc, show_edges, show_fanout):
    flow_edges=[]
    if doc.get('flow_edges_json'):
        try:
            flow_edges=json.loads(doc['flow_edges_json'])
        except Exception:
            pass
    edges_pairs=[(e.get('src'),e.get('dst')) for e in flow_edges if e.get('src') and e.get('dst')]
    fanout={}
    for s,d in edges_pairs:
        fanout.setdefault(s,set()).add(d)
    fanout_sizes=sorted(((n,len(v)) for n,v in fanout.items()), key=lambda x:x[1], reverse=True)
    high=doc.get('high_fanout_nodes_json')
    if high:
        try:
            high=json.loads(high)
        except Exception:
            high=[]
    entry=json.loads(doc.get('entry_nodes_json') or '[]') if doc.get('entry_nodes_json') else []
    exitn=json.loads(doc.get('exit_nodes_json') or '[]') if doc.get('exit_nodes_json') else []
    paths=json.loads(doc.get('path_samples_json') or '[]') if doc.get('path_samples_json') else []
    summary={
        'program_id': doc.get('program_id'),
        'node_count': doc.get('node_count'),
        'edge_count': doc.get('edge_count'),
        'max_depth': doc.get('max_depth'),
        'has_cycles': doc.get('has_cycles'),
        'risk_score': doc.get('risk_score'),
        'entries': entry[:6],
        'exits': exitn[:6],
        'high_fanout_nodes': high[:6],
        'top_fanout': fanout_sizes[:6],
        'sample_paths': paths[:4],
        'flow_summary': doc.get('flow_summary')
    }
    print(json.dumps(summary, indent=2))
    if show_edges:
        print(f"-- First {show_edges} edges --")
        for s,d in edges_pairs[:show_edges]:
            print(f"  {s} -> {d}")
    if show_fanout:
        print(f"-- Top {show_fanout} fanout adjacency --")
        for n,cnt in fanout_sizes[:show_fanout]:
            tgts=sorted(list(fanout.get(n,[])))[:12]
            print(f"  {n} ({cnt}) -> {', '.join(tgts)}")


def main():
    ap=argparse.ArgumentParser(description='Probe a program flow record')
    ap.add_argument('--program', help='Single program id')
    ap.add_argument('--programs', help='Comma-separated list of program ids')
    ap.add_argument('--show-edges', type=int, default=0)
    ap.add_argument('--show-fanout', type=int, default=5)
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    targets=[]
    if args.program: targets.append(args.program.upper())
    if args.programs:
        targets.extend([p.strip().upper() for p in args.programs.split(',') if p.strip()])
    if not targets:
        raise SystemExit('Provide --program or --programs')
    for pid in targets:
        print(f"=== {pid} ===")
        doc=fetch_program(ep,key,pid)
        if not doc:
            print('Not found')
            continue
        summarize(doc, args.show_edges, args.show_fanout)

if __name__=='__main__':
    main()
