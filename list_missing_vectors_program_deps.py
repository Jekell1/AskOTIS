"""List documents in new_cobol_program_deps missing vectors and classify reasons.

Reasons:
  * empty_blob: dependency_blob is empty or whitespace
  * nonempty_blob: dependency_blob has content (unexpected missing embedding)

Outputs summary counts and (optionally) writes a JSON report.

Usage:
  python list_missing_vectors_program_deps.py
  python list_missing_vectors_program_deps.py --out missing_vectors.json
  python list_missing_vectors_program_deps.py --limit 25
"""
from __future__ import annotations
import os, json, sys, requests, argparse

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_deps'


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass


def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_missing(ep,key,limit=0):
    out=[]; skip=0; top=500
    while True:
        body={
            'search':'*',
            'filter':'has_vector eq false',
            'top':top,
            'skip':skip,
            'select':'program_id,dependency_blob,outgoing_count,incoming_count,copybook_count,has_outgoing,has_incoming'
        }
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
        if r.status_code!=200:
            print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
        batch=r.json().get('value',[])
        if not batch:
            break
        out.extend(batch)
        skip+=top
        if limit and len(out)>=limit:
            return out[:limit]
        if len(batch)<top:
            break
    return out


def classify(rows):
    classified=[]
    for r in rows:
        blob=(r.get('dependency_blob') or '').strip()
        if not blob:
            reason='empty_blob'
        else:
            reason='nonempty_blob'
        classified.append({
            'program_id': r.get('program_id'),
            'reason': reason,
            'outgoing_count': r.get('outgoing_count'),
            'incoming_count': r.get('incoming_count'),
            'copybook_count': r.get('copybook_count'),
            'has_outgoing': r.get('has_outgoing'),
            'has_incoming': r.get('has_incoming'),
            'blob_snippet': blob[:160]
        })
    return classified


def summarize(classified):
    total=len(classified)
    empty=sum(1 for c in classified if c['reason']=='empty_blob')
    nonempty=total-empty
    return {
        'total_missing': total,
        'empty_blob': empty,
        'nonempty_blob': nonempty,
        'pct_empty': (empty/total*100 if total else 0.0),
        'pct_nonempty': (nonempty/total*100 if total else 0.0)
    }


def main():
    ap=argparse.ArgumentParser(description='List docs missing vectors & reasons')
    ap.add_argument('--out', help='Write full JSON report to file')
    ap.add_argument('--limit', type=int, default=0, help='Limit number of docs (for quick inspection)')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    rows=fetch_missing(ep,key,args.limit)
    classified=classify(rows)
    summary=summarize(classified)
    print('Summary:', json.dumps(summary, indent=2))
    sample=classified[:10]
    if sample:
        print('\nSample (first up to 10):')
        for s in sample:
            print(f"{s['program_id']}: reason={s['reason']} out={s['outgoing_count']} in={s['incoming_count']} copy={s['copybook_count']} snippet='{s['blob_snippet']}'")
    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump({'summary':summary,'items':classified},f,indent=2)
        print('Wrote report to', args.out)

if __name__=='__main__':
    main()
