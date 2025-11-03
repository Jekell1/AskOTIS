"""Probe joins across variable usage, data items, and (optionally) symbol refs.

For a given variable name (or random sample), show:
  * Variable usage aggregate (if present)
  * Matching data items (item_name == variable_name) with symbol_id
  * Sample symbol refs (if index populated) for that symbol_id_global

Usage:
  python symbol_join_probe.py --variable CUSTOMER-NO --top-items 5 --top-refs 5
  python symbol_join_probe.py --random 3
"""
from __future__ import annotations
import os, sys, json, argparse, random, requests
from id_normalization import make_global_symbol_id

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
VAR_INDEX='new_cobol_variable_usage'
DATA_INDEX='new_cobol_data_items'
REF_INDEX='new_cobol_symbol_refs'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,index,body):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        return []
    return r.json().get('value',[])

def random_variables(ep,key,count):
    body={'search':'*','top':count,'select':'variable_id'}
    return [v.get('variable_id') for v in search(ep,key,VAR_INDEX,body) if v.get('variable_id')]

def probe_variable(ep,key,var,top_items,top_refs,show_programs):
    select='variable_id,read_count,write_count,symbol_id_global'
    if show_programs:
        select+=',program_readers,program_writers'
    var_u=search(ep,key,VAR_INDEX,{'search':var,'filter':f"variable_id eq '{var.upper()}'",'top':1,'select':select})
    var_doc=var_u[0] if var_u else None
    # Data items exact name match
    items=search(ep,key,DATA_INDEX,{'search':var,'filter':f"item_name eq '{var.upper()}'",'top':top_items,'select':'item_id,program_id,item_name,symbol_id,path'})
    refs=[]
    if var_doc and var_doc.get('symbol_id_global'):
        sidg=var_doc['symbol_id_global']
        refs=search(ep,key,REF_INDEX,{'search':'*','filter':f"symbol_id_global eq '{sidg}'",'top':top_refs,'select':'program_id,kind,line_number,excerpt'})
    return var_doc, items, refs

def main():
    ap=argparse.ArgumentParser(description='Symbol join probe')
    ap.add_argument('--variable')
    ap.add_argument('--random',type=int,default=0,help='Probe N random variables')
    ap.add_argument('--top-items',type=int,default=5)
    ap.add_argument('--top-refs',type=int,default=5)
    ap.add_argument('--show-programs',action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    vars_to_check=[]
    if args.variable:
        vars_to_check.append(args.variable)
    elif args.random:
        vars_to_check.extend(random_variables(ep,key,args.random))
    else:
        print('Provide --variable or --random N'); sys.exit(1)
    for var in vars_to_check:
        print(f"\n=== VARIABLE {var} ===")
        var_doc, items, refs = probe_variable(ep,key,var,args.top_items,args.top_refs,args.show_programs)
        if not var_doc:
            print('No variable usage aggregate found.')
        else:
            print('Usage:', {k:var_doc[k] for k in ('variable_id','read_count','write_count','symbol_id_global') if k in var_doc})
            if args.show_programs:
                readers=var_doc.get('program_readers') or []
                writers=var_doc.get('program_writers') or []
                print('Top Readers:', readers[:10])
                print('Top Writers:', writers[:10])
        if not items:
            print('No data items matched item_name.')
        else:
            for it in items:
                print('DataItem:', it.get('program_id'), it.get('item_name'), it.get('symbol_id'), 'path=', it.get('path'))
        if refs:
            for r in refs:
                snip=(r.get('excerpt') or '')[:100].replace('\n',' ')
                print('Ref:', r.get('program_id'), r.get('kind'), 'line', r.get('line_number'), snip)
        else:
            print('No symbol refs (or index empty).')

if __name__=='__main__':
    main()
