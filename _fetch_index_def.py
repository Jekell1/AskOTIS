import os, json, requests, sys
idx = sys.argv[1] if len(sys.argv)>1 else 'new-cobol-files'
api_version = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
endpoint = (os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
key = os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY')
if not endpoint or not key:
    print('Missing SEARCH_ENDPOINT/SEARCH_KEY'); sys.exit(2)
url=f"{endpoint}/indexes/{idx}?api-version={api_version}"
resp = requests.get(url, headers={'api-key':key})
print('HTTP', resp.status_code)
print('Length', len(resp.text))
path=f'index_{idx}.json'
open(path,'w',encoding='utf-8').write(resp.text)
print('Saved to', path)
try:
    js=resp.json()
    fields=[{k:v for k,v in f.items() if k in ('name','type','retrievable','vectorSearchDimensions','dimensions','vectorSearchProfile')} for f in js.get('fields',[])]
    for f in fields:
        if f['name'].endswith('_r') or f['name'].endswith('Vector'):
            print(f)
except Exception as e:
    print('Could not parse JSON', e)
