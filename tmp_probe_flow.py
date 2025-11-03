import os, json, requests
from pathlib import Path
try:
 data=json.loads(Path('local.settings.json').read_text()); vals=data.get('Values',{})
 for k in ['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_ENDPOINT','SEARCH_KEY']:
  if k in vals and k not in os.environ: os.environ[k]=vals[k]
 if 'AZURE_SEARCH_ENDPOINT' not in os.environ and 'SEARCH_ENDPOINT' in os.environ:
  os.environ['AZURE_SEARCH_ENDPOINT']=os.environ['SEARCH_ENDPOINT']
 if 'AZURE_SEARCH_KEY' not in os.environ and 'SEARCH_KEY' in os.environ:
  os.environ['AZURE_SEARCH_KEY']=os.environ['SEARCH_KEY']
except Exception: pass

ep=os.getenv('AZURE_SEARCH_ENDPOINT'); key=os.getenv('AZURE_SEARCH_KEY')
url=f"{ep.rstrip('/')}/indexes/cobol-flow-edges-v2/docs/search?api-version=2024-05-01-preview"
body={"search":"*","top":5}
resp=requests.post(url,headers={"Content-Type":"application/json","api-key":key},json=body,timeout=30)
print('STATUS',resp.status_code)
print(resp.text[:4000])
