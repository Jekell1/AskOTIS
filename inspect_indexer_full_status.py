#!/usr/bin/env python3
"""Inspect full indexer status (top-level + lastResult + executionHistory) for given indexer names.
Usage: python inspect_indexer_full_status.py idx-chunks idx-symbols
"""
import sys,json,requests
API_VERSION="2024-07-01"

def load():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep=vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key=vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not (ep and key):
        print('Missing endpoint/key', file=sys.stderr); sys.exit(2)
    return ep.rstrip('/'), key

def get_indexer(ep,key,name):
    u=f"{ep}/indexers('{name}')?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=60)
    return r.status_code,r.json() if r.text else {}

def get_status(ep,key,name):
    u=f"{ep}/indexers('{name}')/status?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=60)
    return r.status_code,r.json() if r.text else {}

if __name__=='__main__':
    if len(sys.argv)<2:
        print('Provide indexer names')
        sys.exit(1)
    ep,key=load()
    for name in sys.argv[1:]:
        print(f"\n=== {name} ===")
        sc,meta=get_indexer(ep,key,name)
        if sc!=200:
            print(f"(meta) status={sc} body={meta}")
            continue
        print(f"definition.status: {meta.get('status')} isDisabled={meta.get('isDisabled')} dataSourceName={meta.get('dataSourceName')}")
        params=(meta.get('parameters') or {}).get('configuration') or {}
        if params:
            print('configuration:', params)
        sc2,stat=get_status(ep,key,name)
        if sc2!=200:
            print(f"(status) status={sc2} body={stat}")
            continue
        last=stat.get('lastResult') or {}
        print(f"lastResult.status: {last.get('status')} processed={last.get('itemsProcessed')} failed={last.get('itemsFailed')} error={last.get('errorMessage')}")
        hist=stat.get('executionHistory') or []
        print(f"history entries: {len(hist)}")
        for h in hist[:3]:
            print(f" - {h.get('status')} start={h.get('startTime')} end={h.get('endTime')} processed={h.get('itemsProcessed')} failed={h.get('itemsFailed')} error={h.get('errorMessage')}")
