"""Generate a frequency distribution of PARAGRAPH aliases from new_cobol_name_aliases.

Counts use canonical_occurrences (per canonical), returning top N plus summary stats.

Usage:
  python paragraph_frequency_report.py --top 50
  python paragraph_frequency_report.py --min-occ 25 --export freq_paragraphs.json
"""
from __future__ import annotations
import os, json, argparse, requests, statistics, math, sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
ALIAS_INDEX='new_cobol_name_aliases'

SELECT='canonical_name,alias,kind,canonical_occurrences'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{ALIAS_INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print('[ERROR]',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def fetch_all(ep,key):
    skip=0; page=3000
    counts={}  # canonical_name -> canonical_occurrences (max seen among its rows)
    while True:
        body={'search':'*','top':page,'skip':skip,'filter':"kind eq 'PARAGRAPH'",'select':SELECT}
        data=search(ep,key,body)
        rows=data.get('value',[])
        if not rows: break
        for r in rows:
            c=r['canonical_name']; occ=r.get('canonical_occurrences') or 0
            # store max (all variants share same intended canonical count; safe if repeated)
            if occ > counts.get(c,0):
                counts[c]=occ
        skip+=len(rows)
        if len(rows)<page: break
    return counts

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--top',type=int,default=40,help='Show top N by occurrence')
    ap.add_argument('--min-occ',type=int,default=0,help='Filter to canonicals with at least this occurrence count')
    ap.add_argument('--export',help='Write full frequency JSON to file')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    counts=fetch_all(ep,key)
    filtered={k:v for k,v in counts.items() if v>=args.min_occ}
    values=list(filtered.values())
    if not values:
        print('No paragraph counts found (did you ingest paragraphs?).'); return
    total=len(filtered)
    mean=statistics.mean(values)
    median=statistics.median(values)
    p95=statistics.quantiles(values,n=100)[94] if len(values)>=100 else max(values)
    top_sorted=sorted(filtered.items(), key=lambda x: x[1], reverse=True)[:args.top]
    report={
        'paragraph_canonical_total': total,
        'min_occ_threshold': args.min_occ,
        'mean_occurrences': mean,
        'median_occurrences': median,
        'p95_occurrences': p95,
        'max_occurrences': max(values),
        'top': top_sorted
    }
    print(json.dumps(report,indent=2))
    if args.export:
        with open(args.export,'w',encoding='utf-8') as f:
            json.dump({'counts':filtered,'report':report},f,indent=2)
        print(f"Exported full data to {args.export}")

if __name__=='__main__':
    main()
