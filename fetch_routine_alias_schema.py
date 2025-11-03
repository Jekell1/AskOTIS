import os,json,requests
API='2024-07-01'; IDX='cobol-routine-aliases'
vals=json.load(open('local.settings.json')).get('Values',{})
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ['SEARCH_ENDPOINT']; KEY=os.environ['SEARCH_KEY']
url=f"{EP.rstrip('/')}/indexes/{IDX}?api-version={API}"
res=requests.get(url, headers={'api-key':KEY})
print('Status',res.status_code)
if res.status_code!=200:
    print(res.text[:400]); raise SystemExit()
idx=res.json()
print('VectorSearch block:', idx.get('vectorSearch'))
print('Field count:', len(idx.get('fields',[])))
open('cobol_routine_aliases_schema.json','w').write(json.dumps(idx,indent=2))
print('Saved cobol_routine_aliases_schema.json')
