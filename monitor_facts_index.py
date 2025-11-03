#!/usr/bin/env python3
"""Quick monitor for cobol-facts indexer progress vs index doc count.
Usage: python monitor_facts_index.py [--interval 15] [--loops 0]
Loops=0 means run until indexer not inProgress.
"""
import json, time, argparse, sys, requests, os
API_VERSION="2024-07-01"

def load():
    with open('local.settings.json','r',encoding='utf-8') as f:
        vals=json.load(f).get('Values',{})
    ep = vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key in local.settings.json', file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

def indexer_status(ep,key,name):
    u=f"{ep}/indexers('{name}')/status?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=30)
    j=r.json()
    last=j.get('lastResult') or {}
    return last.get('status'), last.get('itemsProcessed'), last.get('itemsFailed'), last.get('endTime')

def index_stats(ep,key,index):
    u=f"{ep}/indexes/{index}/stats?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=30)
    j=r.json()
    return j.get('documentCount'), j.get('storageSize')

def sample(ep,key,index):
    u=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r=requests.post(u,headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':1},timeout=30)
    if r.status_code!=200:
        return r.status_code,None
    v=r.json().get('value') or []
    return r.status_code, v[0] if v else None

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--interval',type=int,default=20)
    ap.add_argument('--loops',type=int,default=0)
    args=ap.parse_args()
    ep,key=load()
    loop=0
    print('Monitoring idx-facts -> cobol-facts (Ctrl+C to stop)')
    while True:
        st,processed,failed,endt = indexer_status(ep,key,'idx-facts')
        docs,storage = index_stats(ep,key,'cobol-facts')
        code, first = sample(ep,key,'cobol-facts')
        print(f"status={st} processed={processed} failed={failed} docs(stat)={docs} storage={storage} sampleCode={code} hasDoc={'Y' if first else 'N'}")
        if first and loop==0 and docs==0:
            # Doc searchable but stats still zero => eventual consistency / not finalized
            print(' note: sample returned doc but stats=0 (expected during active indexing)')
        if st not in ('inProgress','resetting','running'):
            print('Indexer finished.')
            break
        loop+=1
        if args.loops and loop>=args.loops:
            break
        time.sleep(args.interval)
