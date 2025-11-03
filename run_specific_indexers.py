#!/usr/bin/env python3
"""Run specified Azure AI Search indexers and poll until completion.
Usage:
  python run_specific_indexers.py idx-chunks idx-symbols
"""
import sys, time, json, requests
API_VERSION="2024-07-01"

def load():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep=vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key=vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not (ep and key):
        print('Missing endpoint/key', file=sys.stderr); sys.exit(2)
    return ep.rstrip('/'), key

def run_indexer(ep,key,name):
    u=f"{ep}/indexers('{name}')/run?api-version={API_VERSION}"
    r=requests.post(u,headers={'api-key':key},timeout=30)
    if r.status_code not in (200,202):
        print(f"run {name} -> {r.status_code} {r.text[:200]}")
    else:
        print(f"Triggered {name}")

def status(ep,key,name):
    u=f"{ep}/indexers('{name}')/status?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=30)
    try:
        j=r.json()
    except Exception:
        return None
    last=j.get('lastResult') or {}
    return last.get('status'), last.get('itemsProcessed'), last.get('itemsFailed'), last.get('errorMessage')

if __name__=='__main__':
    if len(sys.argv)<2:
        print('Provide indexer names')
        sys.exit(1)
    ep,key=load()
    names=sys.argv[1:]
    for n in names:
        run_indexer(ep,key,n)
    done=set()
    while len(done)<len(names):
        for n in names:
            if n in done: continue
            st,proc,fail,err = status(ep,key,n) or (None,None,None,None)
            print(f"{n}: status={st} processed={proc} failed={fail}")
            if st and st not in ('inProgress','running','resetting'):
                done.add(n)
        if len(done)<len(names):
            time.sleep(10)
    print('All requested indexers finished.')
