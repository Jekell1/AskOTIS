import os, json, requests, sys
API='2024-07-01'
INDEX='cobol-copybooks'
FLAG='has_vector'
PAGE=1000

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception:
        vals={}
    for k,v in vals.items():
        os.environ.setdefault(k,v)
    ep=os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key=os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY'); sys.exit(2)
    return ep.rstrip('/'), key

def main():
    ep,key=load()
    headers={'api-key':key,'Content-Type':'application/json'}
    total=0; flagged=0
    skip=0
    while True:
        body={'search':'*','select':f'copybook_id,{FLAG}','top':PAGE,'skip':skip}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code!=200:
            print('Fetch error',r.status_code,r.text[:200]); break
        batch=r.json().get('value',[])
        if not batch:
            break
        for d in batch:
            total+=1
            if d.get(FLAG):
                flagged+=1
        skip+=len(batch)
        if len(batch)<PAGE:
            break
    print('Total docs:', total)
    print('Flagged has_vector:', flagged)
    if total and flagged==total:
        print('100% vector coverage (inferred via flag).')
    elif total:
        print(f'Coverage: {flagged/total*100:.2f}%')

if __name__=='__main__':
    main()
