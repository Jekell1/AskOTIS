import os, json, requests, sys
API='2024-07-01'
INDEX='cobol-routine-aliases'
try:
    vals=json.load(open('local.settings.json')).get('Values',{})
    for k,v in vals.items(): os.environ.setdefault(k,v)
except Exception: pass
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY: raise SystemExit('Missing search config')
headers={'api-key':KEY,'Content-Type':'application/json'}
url=f"{EP.rstrip('/')}/indexes/{INDEX}/docs/search?api-version={API}"
all_docs=[]; skip=0; page=1000
while True:
    body={'search':'*','top':page,'skip':skip}
    r=requests.post(url, headers=headers, json=body)
    if r.status_code!=200: raise SystemExit(r.text[:300])
    batch=r.json().get('value',[])
    if not batch: break
    all_docs.extend(batch)
    skip+=len(batch)
    if len(batch)<page: break
print('Exported', len(all_docs),'docs')
with open('routine_aliases_export.jsonl','w',encoding='utf-8') as f:
    for d in all_docs:
        f.write(json.dumps(d, ensure_ascii=False)+'\n')
print('Wrote routine_aliases_export.jsonl')
