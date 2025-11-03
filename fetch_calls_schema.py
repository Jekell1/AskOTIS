import os, json, requests

# Load local settings
try:
    vals = json.load(open('local.settings.json','r')).get('Values', {})
    for k,v in vals.items():
        os.environ.setdefault(k, v)
except Exception:
    pass

EP = os.environ.get('SEARCH_ENDPOINT')
KEY = os.environ.get('SEARCH_KEY')
if not EP or not KEY:
    raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')

url = f"{EP.rstrip('/')}/indexes/cobol-calls?api-version=2024-07-01"
resp = requests.get(url, headers={'api-key': KEY})
print('Status:', resp.status_code)
if resp.status_code != 200:
    print(resp.text[:500])
    raise SystemExit()
idx = resp.json()
print('Name:', idx.get('name'))
print('Fields:')
for f in idx.get('fields', []):
    print(f" - {f['name']:20} type={f['type']:18} key={f.get('key')} searchable={f.get('searchable')} filterable={f.get('filterable')} sortable={f.get('sortable')} facetable={f.get('facetable')} dims={f.get('vectorSearchDimensions')}")
