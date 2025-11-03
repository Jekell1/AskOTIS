"""Analyze 'extra' copybook meta docs (present in index, absent as .CPY files).

Produces:
  diagnostics/copybook_meta_extra_analysis.json  (structured report)
  Prints a table-like summary to stdout.

Classification per extra name:
  - empty_paths: file_paths_json empty list
  - stale_all_missing: all listed paths do not exist locally
  - partial_exists: some paths exist
  - non_cpy_paths: paths exist but none end with .cpy (case-insensitive)
  - has_summary / no_summary flags

Suggested follow-up:
  Use deletion for clearly stale or empty docs, or re-ingest if paths appear after sync.
"""
from __future__ import annotations
import os, json, sys, argparse, requests, pathlib

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview') or '2025-08-01-preview'
INDEX='new_cobol_copybook_meta'
EXTRA_LIST='diagnostics/copybook_meta_extra_names.txt'
OUT_JSON='diagnostics/copybook_meta_extra_analysis.json'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_doc(ep,key,name:str):
    safe = name.replace("'","''")
    body={'search':'*','filter':f"copybook_name eq '{safe}'",'top':1,'select':'copybook_name,file_paths_json,summary,has_vector'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code!=200:
        return None, {'error':r.status_code,'text':r.text[:200]}
    val=r.json().get('value',[])
    return (val[0] if val else None), None

def classify(doc):
    paths=[]
    try:
        raw=doc.get('file_paths_json')
        if raw:
            paths=json.loads(raw)
            if not isinstance(paths,list): paths=[]
    except Exception:
        paths=[]
    existing=[p for p in paths if pathlib.Path(p).exists()]
    cpy_existing=[p for p in existing if p.lower().endswith('.cpy')]
    summary=(doc.get('summary') or '').strip()
    if not paths:
        status='empty_paths'
    elif not existing:
        status='stale_all_missing'
    elif existing and not cpy_existing:
        status='non_cpy_paths'
    elif existing and len(existing)<len(paths):
        status='partial_exists'
    else:
        status='valid_but_unexpected'  # Should not be in extra list if .CPY exists
    return {
        'status': status,
        'paths_total': len(paths),
        'paths_existing': len(existing),
        'paths_cpy_existing': len(cpy_existing),
        'has_summary': bool(summary),
        'summary_trunc': summary[:120]
    }

def main():
    load_settings(); ep,key=resolve_search()
    if not os.path.exists(EXTRA_LIST):
        print('Extra list not found, run verify_copybook_meta_coverage first.'); return
    names=[l.strip() for l in open(EXTRA_LIST,'r',encoding='utf-8') if l.strip()]
    report={'index':INDEX,'count':len(names),'entries':[]}
    for n in names:
        doc,err=fetch_doc(ep,key,n)
        if err:
            report['entries'].append({'name':n,'error':err}); continue
        if not doc:
            report['entries'].append({'name':n,'status':'missing_doc'}); continue
        meta=classify(doc)
        report['entries'].append({'name':n, **meta})
    # Derive aggregate stats
    agg={}
    for e in report['entries']:
        s=e.get('status','unknown')
        agg[s]=agg.get(s,0)+1
    report['status_counts']=agg
    os.makedirs('diagnostics', exist_ok=True)
    json.dump(report, open(OUT_JSON,'w',encoding='utf-8'), indent=2)
    print(json.dumps(report['status_counts'], indent=2))
    # Pretty print rows
    print('\nName | Status | total/exist/cpy | summary?')
    for e in report['entries']:
        print(f"{e['name']} | {e.get('status','?')} | {e.get('paths_total','-')}/{e.get('paths_existing','-')}/{e.get('paths_cpy_existing','-')} | {'Y' if e.get('has_summary') else 'N'}")
    print(f"Full report -> {OUT_JSON}")

if __name__=='__main__':
    main()
