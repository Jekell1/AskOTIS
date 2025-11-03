import json, os, requests, sys

API='2024-07-01'
INDEXER='idx-files'
DATASOURCE='ds-files'

def load_vals():
    data=json.load(open('local.settings.json','r',encoding='utf-8'))
    return data.get('Values',{})

def main():
    vals=load_vals()
    ep=vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(2)
    ep=ep.rstrip('/')
    hdr={'api-key':key}
    ixr=requests.get(f"{ep}/indexers/{INDEXER}?api-version={API}",headers=hdr)
    dsr=requests.get(f"{ep}/datasources/{DATASOURCE}?api-version={API}",headers=hdr)
    print('Indexer status',ixr.status_code)
    try: print(json.dumps(ixr.json(),indent=2)[:4000])
    except Exception: print(ixr.text[:1000])
    print('\nData source status',dsr.status_code)
    try: print(json.dumps(dsr.json(),indent=2)[:4000])
    except Exception: print(dsr.text[:1000])

if __name__=='__main__':
    main()
