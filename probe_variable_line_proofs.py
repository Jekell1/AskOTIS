"""Probe: given a variable name (e.g., BT-BRANCH) fetch usage doc and line-precise reference proofs.

Usage:
  python probe_variable_line_proofs.py --variable BT-BRANCH --refs 5
"""
from __future__ import annotations
import os, json, argparse, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
USAGE_INDEX='new_cobol_variable_usage'
REF_INDEX='new_cobol_symbol_refs'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items(): os.environ.setdefault(k,str(v))
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search(ep,key,index,body):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return []
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Variable line-level proof probe')
    ap.add_argument('--variable',required=True)
    ap.add_argument('--refs',type=int,default=5)
    ap.add_argument('--suggest',action='store_true',help='If exact match not found, show similar variable_ids')
    args=ap.parse_args(); load(); ep,key=resolve()
    var=args.variable.upper()
    usage=search(ep,key,USAGE_INDEX,{'search':var,'filter':f"variable_id eq '{var}'",'top':1,'select':'variable_id,first_write_location,first_ref_program_id,first_ref_line_number,usage_role,symbol_id_global'})
    if not usage:
        print('No usage doc')
        if args.suggest:
            # Try token search using last token or whole term fragments
            token = var.split('-')[-1] if '-' in var else var
            # Broad search for token, then filter unique variable_ids
            cand = search(ep,key,USAGE_INDEX,{'search':token,'top':25,'select':'variable_id,read_count,write_count,usage_role'})
            seen=set()
            suggestions=[]
            for c in cand:
                vid=c.get('variable_id');
                if not vid or vid in seen: continue
                seen.add(vid)
                suggestions.append({
                    'variable_id':vid,
                    'role':c.get('usage_role'),
                    'reads':c.get('read_count'),
                    'writes':c.get('write_count')
                })
            if suggestions:
                print('Similar variable_ids:')
                for s in suggestions[:10]:
                    print(f"  - {s['variable_id']} (role={s['role']} R={s['reads']} W={s['writes']})")
        return
    u=usage[0]
    print('USAGE:',u)
    sidg=u.get('symbol_id_global')
    if not sidg:
        print('No symbol_id_global; cannot fetch refs.'); return
    refs=search(ep,key,REF_INDEX,{'search':'*','filter':f"symbol_id_global eq '{sidg}'",'top':args.refs,'select':'ref_id,program_id,file_path,line_number,op,paragraph_name,excerpt'})
    for r in refs:
        snippet=(r.get('excerpt') or '')[:140].replace('\n',' ')
        print(f"REF {r.get('op')} {r.get('program_id')} line {r.get('line_number')} para {r.get('paragraph_name')} :: {snippet}")

if __name__=='__main__':
    main()
