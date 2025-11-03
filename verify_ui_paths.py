"""Verify UI paths for a given start program in new_cobol_ui_paths.

Usage:
  python verify_ui_paths.py --program MAINMENU --sample 5
"""
from __future__ import annotations
import os, json, argparse, requests, sys
from secrets_loader import load_secrets

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_ui_paths'

SELECT='path_id,start_program_id,end_program_id,hop_count,guard_summary,path_json,is_placeholder,path_type'

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def query(ep,key,program,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    safe = program.replace("'","''")
    flt = f"start_program_id eq '{safe}'"
    body={'search':'*','filter':flt,'select':SELECT,'top':top,'count':True}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('[ERROR]',r.status_code,r.text[:300]); return 0,[]
    data=r.json(); return data.get('@odata.count',0), data.get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Verify UI paths for a start program')
    ap.add_argument('--program',required=True)
    ap.add_argument('--sample',type=int,default=5)
    ap.add_argument('--json',action='store_true')
    args=ap.parse_args(); load_secrets(); ep,key=resolve()
    prog=args.program.upper()
    total, rows=query(ep,key,prog,args.sample)
    if args.json:
        print(json.dumps({'program':prog,'total_paths':total,'sample':rows},indent=2)); return
    print(f"Program {prog} total_paths={total}")
    for r in rows:
        print(f"- hop={r.get('hop_count')} placeholder={r.get('is_placeholder')} type={r.get('path_type')} guard={(r.get('guard_summary') or '')[:80]}")

if __name__=='__main__':
    main()
