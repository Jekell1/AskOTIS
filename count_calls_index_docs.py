import os, json, requests
vals = {}
try:
    vals = json.load(open('local.settings.json')).get('Values', {})
except Exception:
    pass
for k,v in vals.items():
    os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY:
    raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
url=f"{EP.rstrip('/')}/indexes/cobol-calls/docs/search?api-version=2024-07-01"
headers={'api-key':KEY,'Content-Type':'application/json'}
resp = requests.post(url, headers=headers, json={'search':'*','count':True,'top':0})
print('Status:', resp.status_code)
print('Body:', resp.text[:500])
if resp.status_code==200:
    data = resp.json()
    print('Reported @odata.count =', data.get('@odata.count'))
