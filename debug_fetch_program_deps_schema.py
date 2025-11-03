import os, json, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
try:
    vals=json.load(open('local.settings.json'))['Values']
    for k,v in vals.items():
        os.environ.setdefault(k,str(v))
except Exception:
    pass
EP=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
KEY=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
if not EP or not KEY:
    print('Missing creds'); sys.exit(1)
EP=EP.rstrip('/')
url=f"{EP}/indexes/new_cobol_program_deps?api-version={API}"
r=requests.get(url,headers={'api-key':KEY})
print('STATUS',r.status_code)
if r.status_code==200:
    data=r.json()
    field_names=[f.get('name') for f in data.get('fields',[])]
    print('Field count', len(field_names))
    print('Has dependency_blob_vector?', 'dependency_blob_vector' in field_names)
    print('Has has_vector?', 'has_vector' in field_names)
    print('VectorSearch snippet:', json.dumps(data.get('vectorSearch',{}),indent=2)[:800])
