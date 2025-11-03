"""Comprehensive validation of recent COBOL search uploads.

Focus areas:
  1. Symbol reference index (new_cobol_symbol_refs)
     - Total docs
     - Vectorized docs (has_vector)
     - Coverage %
     - Count of earliest write refs (is_first_write=true)
     - Sample of refs missing vectors (first 5)
  2. Variable usage index (new_cobol_variable_usage)
     - Docs having first_write_location
     - Docs missing first_write_location
     - % with first_write_location
     - Sample of those missing
  3. Cross-check: For sampled earliest-write symbol refs, ensure corresponding variable usage has first_write_location set (if symbol id mapping derivable).

Environment requirements:
  SEARCH_ENDPOINT / AZURE_SEARCH_ENDPOINT
  SEARCH_KEY / AZURE_SEARCH_KEY

Usage:
  python validate_recent_uploads.py                # human-readable
  python validate_recent_uploads.py --json         # machine JSON
  python validate_recent_uploads.py --samples 20   # adjust sample size
"""
from __future__ import annotations
import os, json, argparse, requests, random
from typing import Dict, Any, List
from secrets_loader import load_secrets

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
REF_INDEX='new_cobol_symbol_refs'
USAGE_INDEX='new_cobol_variable_usage'

def resolve():
    load_secrets()
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing SEARCH endpoint/key environment variables.')
    return ep.rstrip('/'), key

def search(ep:str,key:str,index:str,body:Dict[str,Any])->Dict[str,Any]:
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",
                    headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=180)
    if r.status_code!=200:
        raise RuntimeError(f"Search failure {index} {r.status_code}: {r.text[:200]}")
    return r.json()

def count(ep,key,index,flt=None):
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    return search(ep,key,index,body).get('@odata.count',0)

def sample_docs(ep,key,index,flt,select,top):
    body={'search':'*','filter':flt,'select':select,'top':top}
    return search(ep,key,index,body).get('value',[])

def validate_refs(ep,key,sample_size:int)->Dict[str,Any]:
    total=count(ep,key,REF_INDEX)
    vect=count(ep,key,REF_INDEX,'has_vector eq true')
    missing=vect is not None and total is not None and (total - vect)
    coverage=round(vect/total*100,4) if total else 0.0
    earliest=count(ep,key,REF_INDEX,'is_first_write eq true')
    missing_sample=sample_docs(ep,key,REF_INDEX,'has_vector eq false','ref_id,symbol_name,program_id,line_number',min(5,sample_size)) if total>vect else []
    earliest_sample=sample_docs(ep,key,REF_INDEX,'is_first_write eq true','ref_id,symbol_name,program_id,line_number,is_first_write',min(sample_size,25))
    return {
        'index': REF_INDEX,
        'total_docs': total,
        'vectorized_docs': vect,
        'missing_vectors': missing,
        'coverage_pct': coverage,
        'earliest_write_refs': earliest,
        'missing_vector_sample': missing_sample,
        'earliest_write_sample': earliest_sample,
    }

def validate_usage(ep,key,sample_size:int)->Dict[str,Any]:
    total=count(ep,key,USAGE_INDEX)
    with_fw=count(ep,key,USAGE_INDEX,'first_write_location ne null')
    without_fw=total-with_fw
    pct=round(with_fw/total*100,4) if total else 0.0
    missing_sample=sample_docs(
        ep,key,USAGE_INDEX,
        'first_write_location eq null',
        'symbol_id_global,symbol_name,program_id,read_count,write_count,first_write_program,first_write_location_line',
        min(5,sample_size)
    ) if without_fw>0 else []
    return {
        'index': USAGE_INDEX,
        'total_docs': total,
        'with_first_write_location': with_fw,
        'missing_first_write_location': without_fw,
        'first_write_location_pct': pct,
        'missing_first_write_sample': missing_sample,
    }

def cross_check(ep,key,earliest_refs:List[Dict[str,Any]],limit:int)->List[Dict[str,Any]]:
    # Heuristic: symbol_id_global is often PROGRAM::SYMBOL (or variant). Try program::symbol and symbol::program forms.
    out=[]
    for ref in earliest_refs[:limit]:
        sym=ref.get('symbol_name'); prog=ref.get('program_id')
        if not sym or not prog: continue
        candidates=[f"{prog}::{sym}",f"{sym}::{prog}"]
        status='not_found'
        found=None
        for cid in candidates:
            flt=f"symbol_id_global eq '{cid.lower()}'"
            try:
                body={'search':'*','filter':flt,'select':'symbol_id_global,first_write_location,first_write_program,first_write_location_line','top':1}
                r=search(ep,key,USAGE_INDEX,body).get('value',[])
            except Exception:
                r=[]
            if r:
                found=r[0]
                status='present' if found.get('first_write_location') else 'missing'
                break
        out.append({
            'ref_id':ref.get('ref_id'),
            'symbol_name':sym,
            'program_id':prog,
            'ref_line':ref.get('line_number'),
            'usage_first_write_status':status
        })
    return out

def main():
    ap=argparse.ArgumentParser(description='Validate recent COBOL search uploads.')
    ap.add_argument('--json',action='store_true')
    ap.add_argument('--samples',type=int,default=25,help='Sample size for earliest-write cross checks')
    args=ap.parse_args()
    ep,key=resolve()
    refs=validate_refs(ep,key,args.samples)
    usage=validate_usage(ep,key,args.samples)
    cross=cross_check(ep,key,refs['earliest_write_sample'],min(10,args.samples))
    report={'symbol_refs':refs,'variable_usage':usage,'cross_check':cross}
    if args.json:
        print(json.dumps(report,indent=2)); return
    print('VALIDATION SUMMARY')
    print(f"[Symbol Refs] total={refs['total_docs']} vectors={refs['vectorized_docs']} missing={refs['missing_vectors']} coverage={refs['coverage_pct']:.2f}% earliest_write_refs={refs['earliest_write_refs']}")
    if refs['missing_vector_sample']:
        print('  Sample missing vectors (ref_id, symbol, program, line):')
        for d in refs['missing_vector_sample']:
            print(f"    - {d.get('ref_id')} {d.get('symbol_name')} {d.get('program_id')}:{d.get('line_number')}")
    print(f"[Variable Usage] total={usage['total_docs']} with_first_write={usage['with_first_write_location']} missing={usage['missing_first_write_location']} pct={usage['first_write_location_pct']:.2f}%")
    if usage['missing_first_write_sample']:
        print('  Sample missing first_write_location:')
        for d in usage['missing_first_write_sample']:
            print(f"    - {d.get('symbol_id_global')} sym={d.get('symbol_name')} prog={d.get('program_id')} write_count={d.get('write_count')}")
    if cross:
        print('Cross-check earliest write refs -> usage first_write_location status:')
        for c in cross:
            print(f"  - {c['ref_id']} {c['symbol_name']} {c['program_id']}:{c['ref_line']} usage:{c['usage_first_write_status']}")

if __name__=='__main__':
    main()
