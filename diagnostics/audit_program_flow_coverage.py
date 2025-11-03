"""Audit program flow coverage vs local .CBL files.

Steps:
  1. Enumerate local .CBL program IDs
  2. Page through new_cobol_program_flows index to collect present program_ids
  3. Identify missing & present counts
  4. For each missing program, analyze file existence, size, and presence of PROGRAM-ID.
  5. Emit JSON + summary to help diagnose why program flow ingestion skipped items.

Usage:
  python diagnostics/audit_program_flow_coverage.py --out diagnostics/_program_flow_audit.json
"""
from __future__ import annotations
import os, json, pathlib, argparse, time, requests, re
from typing import Tuple, List, Dict

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'
ROOT=pathlib.Path(__file__).resolve().parents[1]
PAGE_SIZE=1000
SKIP_LIMIT=100_000

RE_PROGRAM_ID=re.compile(r'\bPROGRAM-ID\b', re.IGNORECASE)

def load_settings():
    cfg=ROOT/'local.settings.json'
    if cfg.is_file():
        try:
            vals=json.load(cfg.open())['Values']
            for k,v in vals.items():
                os.environ.setdefault(k,v)
        except Exception:
            pass

def resolve()->Tuple[str,str]:
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def post(ep,key,body):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=90)
    if r.status_code!=200:
        raise RuntimeError(f"search error {r.status_code}: {r.text[:300]}")
    return r.json()

def enumerate_local_programs()->Dict[str,pathlib.Path]:
    mapping={}
    for p in ROOT.rglob('*.CBL'):
        if p.is_file():
            mapping[p.stem.upper()]=p
    return mapping

def enumerate_flow_index_programs(ep,key)->List[str]:
    prog_ids=[]
    skip=0
    while skip<SKIP_LIMIT:
        body={'search':'*','top':PAGE_SIZE,'skip':skip,'select':'program_id'}
        data=post(ep,key,body)
        rows=data.get('value',[])
        if not rows:
            break
        for r in rows:
            pid=r.get('program_id')
            if pid:
                prog_ids.append(pid.upper())
        skip+=PAGE_SIZE
        if len(rows)<PAGE_SIZE:
            break
    return prog_ids

def analyze_missing(local_map:Dict[str,pathlib.Path], present_set:set)->List[Dict[str,any]]:
    out=[]
    for pid,p in local_map.items():
        if pid in present_set:
            continue
        info={'program_id':pid,'path':str(p)}
        try:
            size=p.stat().st_size
            info['size']=size
            text=p.read_text(errors='ignore')
            info['has_program_id']=bool(RE_PROGRAM_ID.search(text[:2000]))
            # Quick heuristic: treat file too small (<150 bytes) as stub
            if size<150:
                info['reason']='stub_or_empty'
            elif not info['has_program_id']:
                info['reason']='no_program_id_clause_detected'
            else:
                info['reason']='unknown_not_indexed'
        except Exception as e:
            info['error']=str(e)
        out.append(info)
    return out

def main():
    ap=argparse.ArgumentParser(description='Audit program flow coverage')
    ap.add_argument('--out')
    ap.add_argument('--limit',type=int,help='Limit number of missing details emitted')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    local_map=enumerate_local_programs()
    flow_prog_ids=enumerate_flow_index_programs(ep,key)
    present_set=set(flow_prog_ids)
    missing=analyze_missing(local_map, present_set)
    summary={
        'total_local_programs': len(local_map),
        'total_in_flow_index': len(present_set),
        'missing_count': len(missing),
        'missing_sample': missing[: (args.limit or 50)],
        'generated_at': time.time(),
        'api_version': API
    }
    print('=== PROGRAM FLOW COVERAGE ===')
    print(f"Local programs: {summary['total_local_programs']}  Indexed: {summary['total_in_flow_index']}  Missing: {summary['missing_count']}")
    if summary['missing_sample']:
        print('Sample missing entries:')
        for m in summary['missing_sample'][:10]:
            print(f"  {m['program_id']} size={m.get('size')} reason={m.get('reason')}")
    if args.out:
        os.makedirs(os.path.dirname(args.out), exist_ok=True)
        json.dump(summary, open(args.out,'w',encoding='utf-8'), indent=2)
        print('Wrote', args.out)

if __name__=='__main__':
    main()
