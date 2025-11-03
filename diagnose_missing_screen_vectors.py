import os, json, requests
from secrets_loader import load_secrets
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_screen_nodes'

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        import sys; print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def main():
    load_secrets()
    ep,key=resolve()
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    # retrieve some docs missing vectors
    body={'search':'*','filter':'has_vector eq false','top':25,'select':'screen_id,summary_text'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    print('Status',r.status_code)
    if r.status_code!=200:
        print(r.text[:500]); return
    vals=r.json().get('value',[])
    print('Missing sample count',len(vals))
    for v in vals[:5]:
        print(v.get('screen_id'))

if __name__=='__main__':
    main()
