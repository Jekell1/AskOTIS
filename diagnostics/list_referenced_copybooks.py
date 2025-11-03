"""Enumerate distinct referenced copybooks from new_cobol_copybook_usage index.

Features:
  1. Attempt facet aggregation on copybook_name_plain (fast path)
  2. Fallback to paged scan (search + skip) building distinct set + frequency counts
  3. Compare against repo .CPY files to identify unreferenced (orphan) and referenced-but-missing definitions
  4. Emit JSON + human summary

Usage:
  python diagnostics/list_referenced_copybooks.py \
      --out diagnostics/_referenced_copybooks.json

Notes:
  - Paged scan may truncate if more than SKIP_LIMIT (Azure Search hard limit ~100k); script warns.
  - Frequency sample lists top-N most/least used copybooks to guide pruning / stoplist creation.
"""
from __future__ import annotations
import os, json, pathlib, time, argparse, requests, collections, sys
from typing import Dict, Any, List, Tuple

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_copybook_usage'
ROOT = pathlib.Path(__file__).resolve().parents[1]

PAGE_SIZE = 1000
MAX_DOCS = 200_000   # safety guard
SKIP_LIMIT = 100_000 # Azure Search skip practical ceiling

def load_settings():
    cfg = ROOT / 'local.settings.json'
    if cfg.is_file():
        try:
            vals = json.load(cfg.open())['Values']
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

def post(ep,key,body):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        raise RuntimeError(f"search error {r.status_code}: {r.text[:300]}")
    return r.json()

def attempt_facet(ep,key)->List[str]:
    try:
        body={'search':'*','facets':['copybook_name_plain,count:0'],'top':0,'select':'copybook_name_plain'}
        data=post(ep,key,body)
        facets=data.get('facets',{}).get('copybook_name_plain',[])
        names=[f.get('value') for f in facets if f.get('value')]
        if names:
            return names
    except Exception:
        return []
    return []

def paged_scan(ep,key)->Tuple[Dict[str,int], int, bool]:
    freq=collections.Counter()
    total_docs=0
    skip=0
    truncated=False
    while skip < MAX_DOCS:
        if skip >= SKIP_LIMIT:
            truncated=True
            break
        body={'search':'*','top':PAGE_SIZE,'skip':skip,'select':'copybook_name_plain','count':True}
        try:
            data=post(ep,key,body)
        except Exception as e:
            print(f"[WARN] paging aborted at skip={skip}: {e}")
            break
        vals=data.get('value',[])
        if not vals:
            break
        for row in vals:
            name=row.get('copybook_name_plain')
            if name:
                freq[name.upper()]+=1
        total_docs = data.get('@odata.count', total_docs + len(vals))
        skip += PAGE_SIZE
        if len(vals) < PAGE_SIZE:
            break
        if skip % (PAGE_SIZE*10)==0:
            print(f"[PAGE] scanned {skip} docs distinct={len(freq)}")
    return dict(freq), total_docs, truncated

def scan_repo_copybooks()->List[str]:
    out=[]
    for p in ROOT.rglob('*.CPY'):
        if p.is_file():
            out.append(p.name.upper())
    return out

def main():
    ap=argparse.ArgumentParser(description='Enumerate referenced copybooks')
    ap.add_argument('--out')
    ap.add_argument('--top',type=int,default=30,help='Top N frequency sample')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()

    start=time.time()
    facet_names=attempt_facet(ep,key)
    used_freq: Dict[str,int]
    total_docs=0
    truncated=False
    if facet_names:
        # If facet works we still want frequency counts -> run paged scan but we can cap earlier
        print(f"[INFO] Facet returned {len(facet_names)} distinct names; performing frequency scan...")
        used_freq, total_docs, truncated = paged_scan(ep,key)
    else:
        print('[INFO] Facet path empty; using full paged scan only')
        used_freq, total_docs, truncated = paged_scan(ep,key)

    distinct=list(used_freq.keys()) if used_freq else facet_names
    repo_copybooks=scan_repo_copybooks()
    repo_set=set(repo_copybooks)
    used_set=set(distinct)
    orphan_repo=sorted(repo_set - used_set)
    referenced_missing_file=sorted(used_set - repo_set)

    # Frequency samples
    most_used=sorted(used_freq.items(), key=lambda x:x[1], reverse=True)[:args.top]
    least_used=sorted(used_freq.items(), key=lambda x:x[1])[:args.top]

    data={
        'api_version':API,
        'total_usage_docs': total_docs,
        'distinct_referenced_count': len(used_set),
        'distinct_referenced_sample': distinct[:min(50,len(distinct))],
        'facet_available': bool(facet_names),
        'scan_truncated': truncated,
        'orphan_repo_copybook_count': len(orphan_repo),
        'orphan_repo_copybook_sample': orphan_repo[:50],
        'referenced_missing_file_count': len(referenced_missing_file),
        'referenced_missing_file_sample': referenced_missing_file[:50],
        'most_used': most_used,
        'least_used': least_used,
        'generated_at': time.time()
    }

    # Human summary
    print('=== REFERENCED COPYBOOK SUMMARY ===')
    print(f"Total usage docs: {total_docs}")
    print(f"Distinct referenced copybooks: {len(used_set)} (facet={'Y' if facet_names else 'N'})")
    if truncated:
        print('[WARN] Paged scan truncated at skip limit; counts may be low')
    print(f"Repo copybooks (definition files) detected: {len(repo_set)}")
    print(f"Orphan repo copybooks (never referenced): {len(orphan_repo)}")
    print(f"Referenced copybook names with no file present: {len(referenced_missing_file)}")
    if most_used:
        print('Top used copybooks:')
        for name,cnt in most_used[:10]:
            print(f"  {name}: {cnt}")
    if least_used:
        print('Low-frequency copybooks:')
        for name,cnt in least_used[:10]:
            print(f"  {name}: {cnt}")

    if args.out:
        os.makedirs(os.path.dirname(args.out), exist_ok=True)
        json.dump(data, open(args.out,'w',encoding='utf-8'), indent=2)
        print('Wrote', args.out)

if __name__=='__main__':
    main()
