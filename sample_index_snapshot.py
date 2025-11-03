import os, json, requests, sys
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
EP=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
KEY=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
if not EP or not KEY:
    print('Missing endpoint/key in environment.')
    sys.exit(1)
EP=EP.rstrip('/')
headers={'api-key':KEY,'Content-Type':'application/json'}
indexes=['new_cobol_program_meta','new_cobol_calls','new_cobol_ui_paths']
summary={}
for idx in indexes:
    url=f"{EP}/indexes/{idx}/docs/search?api-version={API_VERSION}"
    payload={'search':'*','top':10,'count':True}
    r=requests.post(url,headers=headers,json=payload)
    if r.status_code!=200:
        print(idx,'ERROR',r.status_code,r.text[:200])
        continue
    data=r.json()
    count=data.get('@odata.count')
    samples=[]
    for doc in data.get('value',[]):
        samples.append({k:doc.get(k) for k in list(doc.keys())[:15]})
    summary[idx]={'count':count,'sample_docs':samples}
print(json.dumps(summary,indent=2))
