"""Build system overview document(s) for the new_cobol_system_overview index.

This script reads the program meta and related indexes to compute macro level statistics.
We produce a single 'latest' overview (overview_id='latest') plus an optional timestamped snapshot.

Metrics:
  program_count: total programs in new_cobol_program_meta
  ui_program_count: count with ui_path_participant true OR program_role in UI-ish roles
  risk_program_count: count with risk_flag true
  avg_coverage_pct: mean of coverage percentage if available (currently approximated via paragraph/data item presence)
  coverage_bands_json: bucket counts (0, (0-25], (25-50], (50-75], (75-100])
  role_distribution_json: counts per program_role
  top_central_programs_json: top N by centrality_score
  top_fan_out_programs_json: top N by reach_out_size or outgoing call count fallback
  top_fan_in_programs_json: top N by reach_in_size or incoming call count fallback
  external_programs_json: distinct union of external_callees arrays

Approximation notes:
  - coverage percentage: if program_meta contains paragraph_count and data_item_count relative to something, adapt accordingly. For now we derive a synthetic coverage_pct = 100 when program_summary exists else 60 (placeholder) so downstream filtering works; refine as real metric becomes available.

Usage:
  python build_system_overview.py --push
"""
from __future__ import annotations
import os, sys, json, argparse, datetime, requests, math
from statistics import mean
from typing import List, Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_META_INDEX = 'new_cobol_program_meta'
SYSTEM_OVERVIEW_INDEX = 'new_cobol_system_overview'
BATCH_BYTES_LIMIT = 1000*1000 # not critical; single doc
TOP_N = 10


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def search_all(ep: str, key: str, index: str, select: str|None=None) -> List[Dict[str,Any]]:
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    results=[]
    payload={'search':'*','queryType':'simple','top':1000}
    if select:
        payload['select']=select
    skip=0
    while True:
        payload['skip']=skip
        r=requests.post(url, headers=headers, json=payload)
        if r.status_code!=200:
            print(f"[ERROR] search {index} {r.status_code}: {r.text[:200]}")
            break
        data=r.json()
        batch=data.get('value',[])
        results.extend(batch)
        if len(batch)<payload['top']:
            break
        skip+=payload['top']
    return results


def compute_metrics(programs: List[Dict[str,Any]]) -> Dict[str,Any]:
    program_count=len(programs)
    ui_program_count=0
    risk_program_count=0
    role_distribution={}
    coverage_values=[]
    fan_out_candidates=[]
    fan_in_candidates=[]
    central_candidates=[]
    external_set=set()

    for p in programs:
        role=p.get('program_role') or 'UNKNOWN'
        role_distribution[role]=role_distribution.get(role,0)+1
        if p.get('ui_path_participant') or role.lower() in {'ui','screen','presentation'}:
            ui_program_count+=1
        if p.get('risk_flag'):
            risk_program_count+=1
        # synthetic coverage metric
        coverage_pct=100.0 if p.get('program_summary') else 60.0
        coverage_values.append(coverage_pct)
        central_candidates.append((p.get('centrality_score') or 0.0, p.get('program_id')))
        fan_out=(p.get('reach_out_size') or 0)
        fan_in=(p.get('reach_in_size') or 0)
        fan_out_candidates.append((fan_out, p.get('program_id')))
        fan_in_candidates.append((fan_in, p.get('program_id')))
        for ext in (p.get('external_callees') or []):
            external_set.add(ext)

    avg_coverage_pct = mean(coverage_values) if coverage_values else 0.0

    def top_list(cands):
        return [ {'program_id':pid,'value':val} for val,pid in sorted(cands, reverse=True)[:TOP_N] ]

    coverage_bands={'0':0,'0_25':0,'25_50':0,'50_75':0,'75_100':0}
    for v in coverage_values:
        if v<=0: coverage_bands['0']+=1
        elif v<=25: coverage_bands['0_25']+=1
        elif v<=50: coverage_bands['25_50']+=1
        elif v<=75: coverage_bands['50_75']+=1
        else: coverage_bands['75_100']+=1

    metrics={
        'program_count': program_count,
        'ui_program_count': ui_program_count,
        'risk_program_count': risk_program_count,
        'avg_coverage_pct': round(avg_coverage_pct,2),
        'coverage_bands_json': json.dumps(coverage_bands),
        'role_distribution_json': json.dumps(role_distribution),
        'top_central_programs_json': json.dumps(top_list(central_candidates)),
        'top_fan_out_programs_json': json.dumps(top_list(fan_out_candidates)),
        'top_fan_in_programs_json': json.dumps(top_list(fan_in_candidates)),
        'external_programs_json': json.dumps(sorted(external_set)),
    }
    return metrics


def upload_docs(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs:
        return
    url=f"{ep}/indexes/{SYSTEM_OVERVIEW_INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    actions=[{'@search.action':'mergeOrUpload', **d} for d in docs]
    payload={'value': actions}
    r=requests.post(url, headers=headers, json=payload)
    if r.status_code!=200:
        print(f"[ERROR] upload {r.status_code}: {r.text[:400]}")
    else:
        print(f"Uploaded {len(docs)} overview document(s).")


def build_overview(ep: str, key: str, snapshot: bool) -> List[Dict[str,Any]]:
    progs = search_all(ep, key, PROGRAM_META_INDEX)
    metrics=compute_metrics(progs)
    now_iso=datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    docs=[]
    base_doc={'overview_id':'latest','generated_at':now_iso, **metrics}
    docs.append(base_doc)
    if snapshot:
        docs.append({'overview_id':now_iso,'generated_at':now_iso, **metrics})
    return docs


def main():
    ap = argparse.ArgumentParser(description='Build system overview metrics.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--push', action='store_true')
    ap.add_argument('--snapshot', action='store_true', help='Also store a timestamped snapshot doc')
    args=ap.parse_args()
    load_local_settings()
    ep,key=resolve_endpoint_key(args)
    docs=build_overview(ep,key,args.snapshot)
    print(json.dumps(docs, indent=2)[:2000])
    if args.push:
        upload_docs(ep,key,docs)

if __name__=='__main__':
    main()
