"""Compute statistics/histograms for new_cobol_ui_paths coverage.

Usage:
  python probe_ui_path_stats.py [--k 10]

Outputs:
  - Total doc count
  - Length distribution (bucketed)
  - UI density distribution (ui_program_count/length)
  - Unique start/leaf program counts
  - Top K most common leaf programs
  - Path length percentiles
"""
from __future__ import annotations
import os, json, sys, math, requests, argparse, statistics
from typing import Dict, Any, List

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_ui_paths'

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch_all(ep, key, select=None):
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    payload={'search':'*','top':1000}
    if select:
        payload['select']=select
    out=[]
    skip=0
    while True:
        payload['skip']=skip
        r=requests.post(url, headers=headers, json=payload)
        if r.status_code!=200:
            print(f"[ERROR] status {r.status_code}: {r.text[:200]}")
            break
        data=r.json()
        batch=data.get('value',[])
        out.extend(batch)
        if len(batch)<payload['top']:
            break
        skip+=payload['top']
    return out

def percentile(data: List[int], p: float) -> float:
    if not data:
        return 0.0
    k = (len(data)-1) * p
    f = math.floor(k)
    c = math.ceil(k)
    if f==c:
        return float(data[int(k)])
    d0 = data[int(f)] * (c-k)
    d1 = data[int(c)] * (k-f)
    return float(d0+d1)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--k', type=int, default=10, help='Top K leaf programs by frequency to show.')
    args = ap.parse_args()
    load_local_settings()
    ep,key=resolve()
    docs = fetch_all(ep,key, select='path_id,start_program_id,end_program_id,length,ui_program_count')
    if not docs:
        print(json.dumps({'doc_count':0}))
        return
    lengths=[d.get('length') or 0 for d in docs]
    ui_density=[(d.get('ui_program_count') or 0)/ (d.get('length') or 1) for d in docs]
    lengths_sorted=sorted(lengths)
    buckets = [(2,3),(4,5),(6,8),(9,12),(13,18),(19,1000)]
    bucket_counts={f"{lo}-{hi}":0 for lo,hi in buckets}
    for L in lengths:
        for lo,hi in buckets:
            if lo <= L <= hi:
                bucket_counts[f"{lo}-{hi}"]+=1
                break
    leaf_counter={}
    for d in docs:
        leaf=d.get('end_program_id') or d.get('leaf_program_id')
        if leaf:
            leaf_counter[leaf]=leaf_counter.get(leaf,0)+1
    top_leaf=sorted(leaf_counter.items(), key=lambda x:x[1], reverse=True)[:args.k]
    stats_obj={
        'doc_count': len(docs),
        'unique_start_programs': len({d.get('start_program_id') for d in docs if d.get('start_program_id')}),
        'unique_end_programs': len({(d.get('end_program_id') or d.get('leaf_program_id')) for d in docs if (d.get('end_program_id') or d.get('leaf_program_id'))}),
        'length_min': min(lengths),
        'length_max': max(lengths),
        'length_mean': round(statistics.fmean(lengths),2) if lengths else 0,
        'length_median': statistics.median(lengths) if lengths else 0,
        'length_p90': percentile(lengths_sorted,0.90),
        'length_p95': percentile(lengths_sorted,0.95),
        'length_p99': percentile(lengths_sorted,0.99),
        'length_distribution': bucket_counts,
        'ui_density_mean': round(statistics.fmean(ui_density),3),
        'ui_density_median': round(statistics.median(ui_density),3),
        'ui_density_p90': round(percentile(sorted(ui_density),0.90),3),
        'top_leaf_programs': top_leaf
    }
    print(json.dumps(stats_obj, indent=2))

if __name__ == '__main__':
    main()
