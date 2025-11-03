"""Diagnostics: probe symbol reference coverage and program/copybook indexing.

Functions:
  1. Probe symbol refs for given symbols (keyword + exact filter lookup)
  2. Coverage check: Ensure every local .CBL program has an entry in program_flows index.
  3. Coverage check: Ensure every local .CPY copybook appears in copybook usage index.

Output: JSON + human summary.

Usage:
  python diagnostics/probe_symbol_refs.py \
      --symbols BT-BRANCH,DS-BRANCH,TIM360,IRMAIN \
      --out diagnostics/_probe_symbol_refs.json

Assumptions:
  - Azure Search endpoint/key available in env or local.settings.json Values.
  - Index names:
       symbol refs: new_cobol_symbol_refs (fields: symbol_name,line_number,file_path,excerpt)
       program flows: new_cobol_program_flows (program_id)
       copybook usage: new_cobol_copybook_usage (copybook_name_plain)

Non-fatal errors recorded in 'errors' array.
"""
from __future__ import annotations
import os, json, argparse, re, requests, pathlib, time
from typing import List, Dict, Any, Tuple

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
SYMBOL_INDEX='new_cobol_symbol_refs'
PROGRAM_FLOW_INDEX='new_cobol_program_flows'
COPYBOOK_USAGE_INDEX='new_cobol_copybook_usage'

SELECT_SYMBOL='ref_id,program_id,symbol_name,line_number,file_path,paragraph_name,excerpt'
SELECT_PROGRAM='program_id'
SELECT_COPYBOOK='copybook_name_plain,program_id'

ROOT=pathlib.Path(__file__).resolve().parents[1]

# --- Env bootstrap ---

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
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    return ep.rstrip('/'), key

# --- HTTP helpers ---

def post_search(ep,key,index,body)->Dict[str,Any]:
    url=f"{ep}/indexes/{index}/docs/search?api-version={API}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        return {'error':r.text,'value':[]}
    return r.json()

# --- Symbol probing ---

def probe_symbol(ep,key,symbol:str)->Dict[str,Any]:
    sym=symbol.strip()
    if not sym: return {'symbol':symbol,'error':'empty symbol'}
    data={'symbol':sym}
    # Keyword search
    kw_body={'search':sym,'top':5,'select':SELECT_SYMBOL}
    kw=post_search(ep,key,SYMBOL_INDEX,kw_body)
    data['keyword_count']=len(kw.get('value',[]))
    data['keyword_sample']=kw.get('value',[])[:2]

    # Exact filter attempt; duplicate single quotes naive protection
    safe=sym.replace("'","''")
    filter_body={'search':'*','filter':f"symbol_name eq '{safe}'",'top':10,'select':SELECT_SYMBOL}
    flt=post_search(ep,key,SYMBOL_INDEX,filter_body)
    data['filter_count']=len(flt.get('value',[]))
    data['filter_sample']=flt.get('value',[])[:2]

    # Case variant attempt (upper) if different
    upper=sym.upper()
    if upper!=sym:
        safe_u=upper.replace("'","''")
        flt_u=post_search(ep,key,SYMBOL_INDEX,{'search':'*','filter':f"symbol_name eq '{safe_u}'",'top':10,'select':SELECT_SYMBOL})
        data['filter_upper_count']=len(flt_u.get('value',[]))
        if flt_u.get('value'): data['filter_upper_sample']=flt_u['value'][:2]

    # Hyphen split fallback: search each part
    if '-' in sym and data['keyword_count']==0 and data['filter_count']==0:
        parts=[p for p in sym.split('-') if p]
        part_hits=[]
        for p in parts:
            ph=post_search(ep,key,SYMBOL_INDEX,{'search':p,'top':2,'select':SELECT_SYMBOL})
            cnt=len(ph.get('value',[]))
            part_hits.append({'part':p,'count':cnt,'sample':ph.get('value',[])[:1]})
        data['part_probe']=part_hits

    return data

# --- Program coverage ---

def list_local_files()->Tuple[List[pathlib.Path],List[pathlib.Path]]:
    cbl=[]; cpy=[]
    for p in ROOT.rglob('*'):
        if not p.is_file():
            continue
        if p.suffix.upper()=='.CBL':
            cbl.append(p)
        elif p.suffix.upper()=='.CPY':
            cpy.append(p)
    return cbl,cpy

BATCH=15

def batched(seq,n):
    for i in range(0,len(seq),n):
        yield seq[i:i+n]

def fetch_program_presence(ep,key,program_ids:List[str])->Dict[str,bool]:
    present={pid:False for pid in program_ids}
    for chunk in batched(program_ids,BATCH):
        parts=[]
        for pid in chunk:
            safe=pid.replace("'","''")
            parts.append(f"program_id eq '{safe}'")
        filt=' or '.join(parts)
        body={'search':'*','filter':filt,'top':len(chunk)*2,'select':SELECT_PROGRAM}
        res=post_search(ep,key,PROGRAM_FLOW_INDEX,body)
        for row in res.get('value',[]):
            pid=row.get('program_id')
            if pid in present:
                present[pid]=True
        time.sleep(0.05)
    return present

def fetch_copybook_presence(ep,key,copybook_bases:List[str])->Dict[str,bool]:
    present={cb:False for cb in copybook_bases}
    for chunk in batched(copybook_bases,BATCH):
        parts=[]
        for cb in chunk:
            safe=cb.replace("'","''")
            # Accept plain or with .CPY extension since ingestion stores plain possibly with extension
            parts.append(f"(copybook_name_plain eq '{safe}' or copybook_name_plain eq '{safe}.CPY')")
        filt=' or '.join(parts)
        body={'search':'*','filter':filt,'top':len(chunk)*3,'select':SELECT_COPYBOOK}
        res=post_search(ep,key,COPYBOOK_USAGE_INDEX,body)
        for row in res.get('value',[]):
            name=row.get('copybook_name_plain')
            if name in present:
                present[name]=True
        time.sleep(0.05)
    return present

# --- Main ---

def main():
    ap=argparse.ArgumentParser(description='Probe symbol refs and coverage')
    ap.add_argument('--symbols',help='Comma separated symbol list')
    ap.add_argument('--out')
    ap.add_argument('--limit-programs',type=int,help='Limit number of programs checked (debug)')
    ap.add_argument('--limit-copybooks',type=int,help='Limit number of copybooks checked (debug)')
    args=ap.parse_args()

    load_settings(); ep,key=resolve()

    symbols=[s.strip() for s in (args.symbols.split(',') if args.symbols else []) if s.strip()]

    results={'api_version':API,'timestamp':time.time(),'symbols':symbols,'symbol_details':[], 'program_coverage':{},'copybook_coverage':{},'errors':[]}

    # Symbol probes
    for sym in symbols:
        try:
            results['symbol_details'].append(probe_symbol(ep,key,sym))
        except Exception as e:
            results['symbol_details'].append({'symbol':sym,'error':str(e)})

    # Local enumeration
    cbl_files, cpy_files = list_local_files()
    prog_ids=sorted({p.stem.upper() for p in cbl_files})
    copybook_ids=sorted({p.stem.upper() for p in cpy_files})
    if args.limit_programs:
        prog_ids=prog_ids[:args.limit_programs]
    if args.limit_copybooks:
        copybook_ids=copybook_ids[:args.limit_copybooks]

    # Program coverage
    try:
        prog_presence=fetch_program_presence(ep,key,prog_ids)
        missing_prog=[p for p,v in prog_presence.items() if not v]
        results['program_coverage']={'total':len(prog_ids),'missing':len(missing_prog),'missing_list':missing_prog[:100]}
    except Exception as e:
        results['errors'].append(f'program coverage error: {e}')

    # Copybook coverage (redefined): we only consider copybooks that are actually referenced at least once in code.
    try:
        # Fetch distinct referenced copybooks via faceting (top large enough) or iterative search batches.
        # If facet API unsupported, fallback to presence check only on repo copybooks.
        referenced=set()
        try:
            facet_body={'search':'*','facets':['copybook_name_plain,count:10000'],'top':0,'select':'copybook_name_plain'}
            facet_res=post_search(ep,key,COPYBOOK_USAGE_INDEX,facet_body)
            for f in facet_res.get('facets',{}).get('copybook_name_plain',[]):
                name=f.get('value');
                if name: referenced.add(str(name).upper())
        except Exception:
            referenced=set()
        if not referenced:
            # Fallback: treat all repo .CPY as potential, and mark coverage by direct presence lookup
            copy_presence=fetch_copybook_presence(ep,key,copybook_ids)
            missing_copy=[c for c,v in copy_presence.items() if not v]
            results['copybook_coverage']={'total':len(copybook_ids),'missing':len(missing_copy),'missing_list':missing_copy[:100],'referenced_total':0,'orphan_repo_copybooks':missing_copy[:50]}
        else:
            # Determine which repo copybooks are actually referenced and which referenced copybooks have docs
            referenced_upper=sorted(referenced)
            ref_presence=fetch_copybook_presence(ep,key,referenced_upper)
            missing_ref=[c for c,v in ref_presence.items() if not v]
            repo_only=[c for c in copybook_ids if c not in referenced]
            results['copybook_coverage']={
                'total_repo_cpy':len(copybook_ids),
                'referenced_total':len(referenced_upper),
                'referenced_missing':len(missing_ref),
                'referenced_missing_list':missing_ref[:100],
                'repo_only_unreferenced':repo_only[:100],
                'repo_only_unreferenced_count':len(repo_only)
            }
    except Exception as e:
        results['errors'].append(f'copybook coverage error: {e}')

    summary_lines=[]
    summary_lines.append('=== SYMBOL PROBE SUMMARY ===')
    for d in results['symbol_details']:
        sym=d['symbol']
        summary_lines.append(f"{sym}: kw={d.get('keyword_count')} filter={d.get('filter_count')} upper={d.get('filter_upper_count')} sample_line={(d.get('keyword_sample') or [{}])[0].get('line_number')}")
    summary_lines.append('\n=== PROGRAM COVERAGE ===')
    pc=results['program_coverage']; summary_lines.append(f"Programs total={pc.get('total')} missing={pc.get('missing')}")
    if pc.get('missing_list'): summary_lines.append('  Missing sample: '+', '.join(pc['missing_list'][:10]))
    summary_lines.append('\n=== COPYBOOK COVERAGE ===')
    cc=results['copybook_coverage']
    if 'referenced_total' in cc:
        if cc.get('referenced_total')==0:
            summary_lines.append(f"Referenced copybooks facet unavailable; repo .CPY files={cc.get('total')} missing_in_index={cc.get('missing')}")
        else:
            summary_lines.append(f"Repo .CPY={cc.get('total_repo_cpy')} referenced={cc.get('referenced_total')} referenced_missing={cc.get('referenced_missing')} repo_only_unreferenced={cc.get('repo_only_unreferenced_count')}")
        if cc.get('referenced_missing_list'):
            summary_lines.append('  Referenced missing sample: '+', '.join(cc['referenced_missing_list'][:10]))
        if cc.get('repo_only_unreferenced'):
            summary_lines.append('  Repo-only (never referenced) sample: '+', '.join(cc['repo_only_unreferenced'][:10]))
    else:
        summary_lines.append(f"Copybooks total={cc.get('total')} missing={cc.get('missing')}")
        if cc.get('missing_list'):
            summary_lines.append('  Missing sample: '+', '.join(cc['missing_list'][:10]))

    print('\n'.join(summary_lines))

    if args.out:
        os.makedirs(os.path.dirname(args.out), exist_ok=True)
        json.dump(results, open(args.out,'w',encoding='utf-8'), indent=2)
        print('Wrote', args.out)

if __name__=='__main__':
    main()
