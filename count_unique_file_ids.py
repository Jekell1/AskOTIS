import json, os, sys, requests

API='2024-07-01'
INDEX='cobol-files'

def load():
    data=json.load(open('local.settings.json','r',encoding='utf-8'))
    return data.get('Values',{})

def main():
    vals=load()
    ep=vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(2)
    url=f"{ep.rstrip('/')}/indexes/{INDEX}/docs/search?api-version={API}"
    top=1000; skip=0; seen=set(); total=0
    while True:
        body={"search":"*","select":"file_id","top":top,"skip":skip}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
        if r.status_code!=200:
            print('Error',r.status_code,r.text[:200]); break
        batch=r.json().get('value',[])
        if not batch: break
        for d in batch:
            fid=d.get('file_id')
            if fid: seen.add(fid)
        total+=len(batch)
        skip+=len(batch)
        if len(batch)<top: break
    print('Batches scanned docs:', total)
    print('Unique file_id count:', len(seen))

if __name__=='__main__':
    main()
