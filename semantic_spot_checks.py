import os, json, requests, env_autoload

env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
endpoint=os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
key=os.getenv('AZURE_SEARCH_KEY')
index='new-cobol-files'
api=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

queries=[
    'SQL CURSOR FETCH',
    'CICS SEND MAP',
    'COPY DFH',
    'SCREEN SECTION',
    'PARAGRAPH INITIALIZATION',
    'ERROR HANDLING ROUTINE',
    'DATE CONVERSION'
]
url=f"{endpoint}/indexes/{index}/docs/search?api-version={api}"

for q in queries:
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, data=json.dumps({'search':q,'top':5}))
    print(f"\n=== Query: {q} ===")
    if r.status_code!=200:
        print('FAIL', r.status_code, r.text[:160])
        continue
    jr=r.json()
    for hit in jr.get('value', []):
        pid=hit.get('programId');score=hit.get('@search.score');kind=hit.get('kind');path=hit.get('path')
        flags=[]
        if hit.get('hasSQL'): flags.append('SQL')
        if hit.get('hasCICS'): flags.append('CICS')
        if hit.get('hasScreens'): flags.append('SCR')
        summary=(hit.get('summary','') or '')[:130].replace('\n',' ')
        print(f"  {score:.4f} {pid} [{kind}] ({','.join(flags) or '-'} ) -> {path}")
        print('     ', summary)
