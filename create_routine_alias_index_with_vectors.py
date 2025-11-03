import os,json,requests,sys
API='2024-07-01'; IDX='cobol-routine-aliases'
vals=json.load(open('local.settings.json')).get('Values',{})
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY: raise SystemExit('Missing search config')
headers={'api-key':KEY,'Content-Type':'application/json'}
try:
    schema=json.load(open('cobol_routine_aliases_schema.json'))
except Exception as e:
    raise SystemExit('Missing cached schema file: '+str(e))
# sanitize
schema.pop('@odata.etag', None)
# add vector fields if missing
names={f['name'] for f in schema['fields']}
if 'alias_vector' not in names:
    # Use legacy pattern: dimensions + vectorSearchProfile
    schema['fields'].append({
        'name':'alias_vector',
        'type':'Collection(Edm.Single)',
        'searchable':True,
        'filterable':False,
        'facetable':False,
        'sortable':False,
        'dimensions':1536,
        'vectorSearchProfile':'default'
    })
if 'has_vector' not in names:
    schema['fields'].append({'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':True})
if not schema.get('vectorSearch'):
    schema['vectorSearch']={
        'algorithms':[{'name':'default','kind':'hnsw','hnswParameters':{'m':8,'efConstruction':400,'efSearch':200}}],
        'profiles':[{'name':'default','algorithm':'default'}]
    }
# best effort delete
requests.delete(f"{EP.rstrip('/')}/indexes/{IDX}?api-version={API}", headers={'api-key':KEY})
cr=requests.post(f"{EP.rstrip('/')}/indexes?api-version={API}", headers=headers, json=schema)
print('Create status', cr.status_code)
if cr.status_code not in (200,201):
    print(cr.text[:500])
else:
    print('Index recreated with vector fields.')
