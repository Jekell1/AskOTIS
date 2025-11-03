"""Verify UI path coverage quality and stats.

Outputs JSON summary:
  - total_paths
  - placeholder_paths
  - roots_covered
  - roots_with_hop3 (>=1 path hop_count>=3)
  - roots_placeholder_only
  - sample_missing_hop3
  - distribution (histogram of hop_count)

Usage:
  python diagnostics/verify_ui_paths_coverage.py
"""
from __future__ import annotations
import os, json, requests, collections, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_ui_paths'
META_INDEX='new_cobol_program_meta'

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_all(ep,key,index,select=None):
    out=[]; skip=0; top=1000
    url=f"{ep}/indexes/{index}/docs/search?api-version={API}"
    while True:
        body={'search':'*','top':top,'skip':skip}
        if select: body['select']=select
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
        if r.status_code!=200: break
        data=r.json().get('value',[])
        out.extend(data)
        if len(data)<top: break
        skip+=top
    return out

def main():
    load_local(); ep,key=resolve()
    paths=fetch_all(ep,key,INDEX,select='path_id,start_program_id,hop_count,is_placeholder,frequency_score_norm')
    meta=fetch_all(ep,key,META_INDEX,select='program_id,program_role')
    menu_roots={m['program_id'] for m in meta if (m.get('program_role') in ('DISPATCH','UI') or 'MENU' in (m.get('program_id') or '').upper())}
    by_root=collections.defaultdict(list)
    for p in paths:
        by_root[p.get('start_program_id')].append(p)
    roots_covered={r for r in by_root if r in menu_roots}
    roots_with_hop3={r for r,plist in by_root.items() if r in menu_roots and any((p.get('hop_count') or 0)>=3 for p in plist)}
    roots_with_hop2={r for r,plist in by_root.items() if r in menu_roots and any((p.get('hop_count') or 0)>=2 for p in plist)}
    placeholder_only={r for r,plist in by_root.items() if r in menu_roots and all(p.get('is_placeholder') for p in plist)}
    missing_hop3=sorted(list(menu_roots - roots_with_hop3))[:30]
    hop_hist=collections.Counter(p.get('hop_count') or 0 for p in paths)
    non_placeholder_roots={r for r,plist in by_root.items() if r in menu_roots and any(not p.get('is_placeholder') for p in plist)}
    avg_hop_per_root=0.0
    if by_root:
        avg_hop_per_root=sum(max(p.get('hop_count') or 0 for p in plist) for plist in by_root.values())/len(by_root)
    res={
        'total_paths':len(paths),
        'placeholder_paths':sum(1 for p in paths if p.get('is_placeholder')),
        'menu_root_count':len(menu_roots),
        'roots_covered':len(roots_covered),
        'roots_with_hop3':len(roots_with_hop3),
        'roots_with_hop2':len(roots_with_hop2),
        'roots_placeholder_only':len(placeholder_only),
        'non_placeholder_root_count':len(non_placeholder_roots),
        'pct_non_placeholder_roots': round(len(non_placeholder_roots)/len(menu_roots),4) if menu_roots else 0.0,
        'sample_missing_hop3':missing_hop3,
        'hop_count_hist':dict(sorted(hop_hist.items())),
        'avg_max_hop_per_root': round(avg_hop_per_root,3)
    }
    print(json.dumps(res,indent=2))

if __name__=='__main__':
    main()
