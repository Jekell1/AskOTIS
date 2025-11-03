import os, json, requests, pathlib, sys
vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
for k in ['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
    if k in vals and k not in os.environ:
        os.environ[k]=vals[k]
EP=os.environ['AZURE_SEARCH_ENDPOINT'].rstrip('/')
KEY=os.environ['AZURE_SEARCH_KEY']
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_meta'
extra_file=pathlib.Path('diagnostics/copybook_meta_extra_names.txt')
if not extra_file.exists():
    print('No extras file; nothing to delete.')
    sys.exit(0)
extras=[l.strip() for l in extra_file.read_text(encoding='utf-8').splitlines() if l.strip()]
if not extras:
    print('Extras file empty; nothing to delete.')
    sys.exit(0)
print(f'Deleting {len(extras)} extras...')
value=[{'@search.action':'delete','copybook_name':n} for n in extras]
resp=requests.post(f"{EP}/indexes/{INDEX}/docs/index?api-version={API}",headers={'api-key':KEY,'Content-Type':'application/json'},json={'value':value},timeout=60)
print('Status',resp.status_code)
print(resp.text[:500])
