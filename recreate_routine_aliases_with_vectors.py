import os,json,requests,sys
API='2024-07-01'; IDX='cobol-routine-aliases'
vals=json.load(open('local.settings.json')).get('Values',{})
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY: raise SystemExit('Missing search config')
headers={'api-key':KEY,'Content-Type':'application/json'}
# load previous schema snapshot
orig=json.load(open('cobol_routine_aliases_schema.json'))
orig.pop('@odata.etag', None)
fields=orig['fields']
# Add new fields if absent
existing_names={f['name'] for f in fields}
if 'alias_vector' not in existing_names:
    fields.append({
        'name':'alias_vector',
        'type':'Collection(Edm.Single)',
        'searchable':True,
        'filterable':False,
        'facetable':False,
        'sortable':False,
        'vectorSearchDimensions':1536,
        'vectorSearchConfiguration':'default'
    })
if 'has_vector' not in existing_names:
    fields.append({
        'name':'has_vector',
        'type':'Edm.Boolean',
        'searchable':False,
        'filterable':True,
        'facetable':True,
        'sortable':True
    })
# Add vectorSearch block if missing
if not orig.get('vectorSearch'):
    orig['vectorSearch']={
        'algorithms':[{'name':'default','kind':'hnsw','hnswParameters':{'m':4,'efConstruction':400,'efSearch':40}}],
        'profiles':[{'name':'default','algorithm':'default'}]
    }
# Delete old index
del_url=f"{EP.rstrip('/')}/indexes/{IDX}?api-version={API}"
print('Deleting existing index...')
r=requests.delete(del_url, headers={'api-key':KEY})
if r.status_code not in (204,202):
    print('Delete status', r.status_code, r.text[:200]); sys.exit(1)
# Create new
create_url=f"{EP.rstrip('/')}/indexes?api-version={API}"
print('Creating new index with vectors...')
cr=requests.post(create_url, headers=headers, json=orig)
print('Create status', cr.status_code)
if cr.status_code not in (200,201):
    print(cr.text[:500]); sys.exit(1)
print('Recreated index', IDX)
