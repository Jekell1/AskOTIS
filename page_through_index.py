#!/usr/bin/env python3
"""Page through an Azure AI Search index to enumerate documents.

Features:
 - Auto-detect key field from index schema
 - Uses search('*') with paging (top + skip)
 - Progress output every page
 - Optional duplicate key detection
 - Optional max docs limit and page count limit

Usage examples:
  python page_through_index.py --index cobol-facts --pages 5
  python page_through_index.py --index cobol-facts --max 50000 --verify

Notes:
 - Large full crawls (hundreds of thousands) will take time. You can Ctrl+C safely.
 - Stats docCount may lag during/after indexing; this script verifies actual retrievable docs.
"""
import argparse, json, sys, time, requests
API_VERSION = '2024-07-01'


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except FileNotFoundError:
        print('local.settings.json missing', file=sys.stderr); sys.exit(2)
    ep = vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key in local.settings.json', file=sys.stderr); sys.exit(2)
    return ep.rstrip('/'), key


def get_index_definition(ep,key,index):
    u=f"{ep}/indexes('{index}')?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=30)
    if r.status_code!=200:
        raise RuntimeError(f"Get index def failed {r.status_code}: {r.text[:200]}")
    return r.json()


def detect_key_field(defn):
    for f in defn.get('fields',[]):
        if f.get('key'):
            return f['name']
    raise RuntimeError('No key field detected')


def search_page(ep,key,index,top,skip):
    u=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    payload={'search':'*','top':top,'skip':skip,'count': True}
    r=requests.post(u,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Search failed {r.status_code}: {r.text[:200]}")
    j=r.json()
    return j.get('value',[]), j.get('@odata.count')


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--page-size', type=int, default=1000)
    ap.add_argument('--pages', type=int, default=0, help='Max pages to fetch (0 = unlimited until exhaustion / max)')
    ap.add_argument('--max', type=int, default=0, help='Stop after this many docs (0 = no limit)')
    ap.add_argument('--verify', action='store_true', help='Track keys and report duplicates')
    ap.add_argument('--sleep', type=float, default=0.0, help='Optional sleep between pages (seconds)')
    ap.add_argument('--hard-skip-limit', type=int, default=100000, help='Stop before Azure skip hard limit (default 100000)')
    args=ap.parse_args()

    ep,key=load_settings()
    defn=get_index_definition(ep,key,args.index)
    key_field=detect_key_field(defn)
    print(f"Index={args.index} key_field={key_field} page_size={args.page_size}")

    total_count=None
    seen=set() if args.verify else None
    cumulative=0
    page=0
    start=time.time()
    while True:
        if args.pages and page>=args.pages:
            break
        if args.max and cumulative>=args.max:
            break
        if args.hard_skip_limit and cumulative >= args.hard_skip_limit:
            print(f"Reached hard skip limit threshold ({args.hard_skip_limit}); stopping to avoid 400 error.")
            break
        docs, cnt = search_page(ep,key,args.index,args.page_size, cumulative)
        if total_count is None:
            total_count = cnt
        if not docs:
            print('No more docs returned; stopping.')
            break
        cumulative += len(docs)
        dups=0
        if seen is not None:
            for d in docs:
                k=d.get(key_field)
                if k in seen: dups+=1
                else: seen.add(k)
        first_key = docs[0].get(key_field)
        last_key = docs[-1].get(key_field)
        print(f"page={page} fetched={len(docs)} cumulative={cumulative} totalReported={total_count} firstKey={first_key} lastKey={last_key} dupsThisPage={dups}")
        page+=1
        if len(docs) < args.page_size:
            print('Final (partial) page reached.')
            break
        if args.max and cumulative>=args.max:
            break
        if args.sleep: time.sleep(args.sleep)

    dur=time.time()-start
    if seen is not None:
        print(f"Unique keys collected={len(seen)}")
    print(f"Done. Pages={page} DocsFetched={cumulative} ReportedTotal={total_count} Elapsed={dur:.1f}s")

if __name__=='__main__':
    main()
