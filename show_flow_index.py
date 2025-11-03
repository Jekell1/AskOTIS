#!/usr/bin/env python3
import os,json,requests
API='2024-07-01';IDX='cobol-flow-edges-v2'
vals={}
if os.path.exists('local.settings.json'):
  try: vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
  except Exception: pass
endpoint=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
if not endpoint or not key:
  raise SystemExit('missing config')
url=f"{endpoint.rstrip('/')}/indexes/{IDX}?api-version={API}"
r=requests.get(url,headers={'api-key':key})
print('Status',r.status_code)
idx=r.json()
for f in idx.get('fields',[]):
  if any(k in f for k in ('vectorSearchDimensions','dimensions','vectorSearchProfile')) or f['name'] in ('edge_text','edge_vector','has_vector'):
    print('FIELD',json.dumps(f,ensure_ascii=False))
print('vectorSearch block:', json.dumps(idx.get('vectorSearch'),ensure_ascii=False))
