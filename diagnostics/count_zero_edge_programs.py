"""Count programs in new_cobol_program_flows with edge_count == 0 and provide an optional sample.

Usage:
  python diagnostics/count_zero_edge_programs.py                # default sample=10
  python diagnostics/count_zero_edge_programs.py --sample 25 --json

Outputs:
  - total_docs
  - zero_edge_count
  - zero_edge_pct
  - sample (program_id,node_count,max_depth,risk_score)

Relies on local.settings.json or environment variables for AZURE_SEARCH_ENDPOINT / KEY.
"""
from __future__ import annotations
import os, json, argparse, sys, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'
PAGE=512


def load_local_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key',file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def search_count(ep,key, filt=None):
    body={'search':'*','top':0,'count':True}
    if filt:
        body['filter']=filt
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
                    headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=90)
    if r.status_code!=200:
        raise RuntimeError(f"count failed {r.status_code}: {r.text[:200]}")
    return r.json().get('@odata.count',0)

def fetch_sample_zero(ep,key, limit:int):
    if limit<=0:
        return []
    body={'search':'*','top':limit,'filter':'edge_count eq 0',
          'select':'program_id,node_count,edge_count,max_depth,risk_score'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
                    headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=90)
    if r.status_code!=200:
        return []
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Count programs with edge_count == 0 in flow index')
    ap.add_argument('--sample',type=int,default=10,help='Sample size to fetch of zero-edge docs')
    ap.add_argument('--json',action='store_true',help='Emit JSON only')
    args=ap.parse_args(); load_local_settings(); ep,key=resolve()
    try:
        total=search_count(ep,key)
        zero=search_count(ep,key,'edge_count eq 0')
    except Exception as e:
        print('[FATAL] count error',e,file=sys.stderr); sys.exit(1)
    pct= (zero/total*100.0) if total else 0.0
    sample=fetch_sample_zero(ep,key,args.sample)
    result={'index':INDEX,'total_docs':total,'zero_edge_count':zero,'zero_edge_pct':round(pct,3),'sample':sample}
    if args.json:
        print(json.dumps(result,indent=2))
    else:
        print(f"Index: {INDEX}")
        print(f"Total docs: {total}")
        print(f"Zero-edge docs: {zero} ({pct:.3f}%)")
        if sample:
            print('Sample:')
            for d in sample:
                print(f"  {d.get('program_id')}: nodes={d.get('node_count')} depth={d.get('max_depth')} risk={d.get('risk_score')} edge_count={d.get('edge_count')}")
            if zero>len(sample):
                print(f"  ... ({zero-len(sample)} more omitted)")
    # Non-zero exit if too many zero-edge docs? (future gate) For now always 0.

if __name__=='__main__':
    main()
