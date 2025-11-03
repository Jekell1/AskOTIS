#!/usr/bin/env python3
"""Reset (full crawl) and run specified Azure AI Search indexers, polling until done.
Usage:
  python reset_and_run_indexers.py idx-chunks idx-symbols
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

def call(ep,key,method,path):
    u=f"{ep}{path}?api-version={API_VERSION}"
    r=getattr(requests,method.lower())(u,headers={'api-key':key,'Content-Type':'application/json'},timeout=60)
    if r.status_code not in (200,202):
        print(f"{method} {path} -> {r.status_code} {r.text[:200]}")
    return r

def reset_run(ep,key,name):
    print(f"Resetting {name} ...")
    call(ep,key,'POST',f"/indexers('{name}')/reset")
    print(f"Running {name} ...")
    call(ep,key,'POST',f"/indexers('{name}')/run")

def status(ep,key,name):
    u=f"{ep}/indexers('{name}')/status?api-version={API_VERSION}"
    r=requests.get(u,headers={'api-key':key},timeout=60)
    try: j=r.json()
    except Exception: return None
    last=j.get('lastResult') or {}
    return last.get('status'), last.get('itemsProcessed'), last.get('itemsFailed'), last.get('errorMessage')

if __name__=='__main__':
    if len(sys.argv)<2:
        print('Provide indexer names to reset+run'); sys.exit(1)
    ep,key=load()
    names=sys.argv[1:]
    for n in names: reset_run(ep,key,n)
    done=set();
    while len(done)<len(names):
        for n in names:
            if n in done: continue
            st,proc,fail,err = status(ep,key,n) or (None,None,None,None)
            print(f"{n}: status={st} processed={proc} failed={fail}")
            if st and st not in ('inProgress','running','resetting'):
                done.add(n)
        if len(done)<len(names): time.sleep(15)
    print('All done.')
