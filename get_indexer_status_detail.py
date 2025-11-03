import os, sys, json, requests
API_VERSION='2024-07-01'
name = sys.argv[1] if len(sys.argv)>1 else 'idx-paragraphs'
ls_path='local.settings.json'
vals={}
if os.path.exists(ls_path):
    vals=json.load(open(ls_path,encoding='utf-8')).get('Values',{})
endpoint=(os.environ.get('SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
key=os.environ.get('SEARCH_KEY') or vals.get('SEARCH_KEY')
url=f"{endpoint}/indexers/{name}/status?api-version={API_VERSION}"
resp=requests.get(url,headers={'api-key':key})
print(resp.status_code)
if resp.status_code==200:
    js=resp.json()
    last=js.get('lastResult',{})
    print(json.dumps({
        'name': name,
        'status': last.get('status'),
        'errorMessage': last.get('errorMessage'),
        'itemsProcessed': last.get('itemsProcessed'),
        'itemsFailed': last.get('itemsFailed'),
        'warnings': last.get('warnings'),
        'startTime': last.get('startTime'),
        'endTime': last.get('endTime')
    }, indent=2))
else:
    print(resp.text[:500])
