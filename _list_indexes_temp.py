import json, requests, sys
vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
endpoint = (vals.get('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
key = vals.get('AZURE_SEARCH_KEY')
if not endpoint or not key:
    print('MISSING endpoint/key'); sys.exit(1)
url = f"{endpoint}/indexes?api-version=2024-07-01"
r = requests.get(url, headers={'api-key':key})
print('HTTP', r.status_code)
if r.status_code!=200:
    print(r.text[:600]); sys.exit(2)
for it in r.json().get('value',[]):
    print(it.get('name'))