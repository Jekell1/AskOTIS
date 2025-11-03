import os, json, requests
EXPORT_FILE='calls_reingest_raw.jsonl'
vals={}
try:
    vals=json.load(open('local.settings.json')).get('Values',{})
except Exception: pass
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY:
    raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
# load exported
export_ids=[]
with open(EXPORT_FILE,'r',encoding='utf-8') as f:
    for line in f:
        line=line.strip()
        if not line: continue
        try:
            obj=json.loads(line)
            export_ids.append(obj['call_id'])
        except Exception: pass
print('Export total IDs:', len(export_ids))
export_set=set(export_ids)
# fetch all indexed ids via paging
headers={'api-key':KEY,'Content-Type':'application/json'}
search_url=f"{EP.rstrip('/')}/indexes/cobol-calls/docs/search?api-version=2024-07-01"
all_ids=[]
skip=0
page=1000
while True:
    body={'search':'*','select':'call_id','top':page,'skip':skip}
    r=requests.post(search_url, headers=headers, json=body)
    if r.status_code!=200:
        print('Search page error', r.status_code, r.text[:200])
        break
    data=r.json()
    batch=[v['call_id'] for v in data.get('value',[]) if 'call_id' in v]
    all_ids.extend(batch)
    if len(batch)<page:
        break
    skip+=page
print('Indexed IDs fetched:', len(all_ids))
idx_set=set(all_ids)
missing=export_set-idx_set
print('Missing count:', len(missing))
if missing:
    sample=list(missing)[:10]
    print('Missing sample:', sample)
# write missing list
with open('missing_calls_ids.txt','w',encoding='utf-8') as f:
    for cid in sorted(missing):
        f.write(cid+'\n')
print('Wrote missing_calls_ids.txt')
