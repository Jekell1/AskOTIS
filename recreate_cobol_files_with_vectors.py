import os, json, requests, sys
API='2024-07-01'
INDEX='cobol-files'
DIM=1536

# Load settings
vals=json.load(open('local.settings.json')).get('Values',{})
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY: raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')

# Base schema (mirrors create_indexes.index_files())
fields=[
    {"name":"file_id","type":"Edm.String","key":True},
    {"name":"path","type":"Edm.String","searchable":True,"filterable":True},
    {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
    {"name":"program_id","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
    {"name":"lines","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
    {"name":"format","type":"Edm.String","filterable":True,"facetable":True},
    {"name":"procedure_using","type":"Collection(Edm.String)","searchable":True,"filterable":True,"facetable":True},
    {"name":"copybook","type":"Edm.Boolean","filterable":True,"facetable":True},
]
# Add vector + flag
fields.append({"name":"file_vector","type":"Collection(Edm.Single)","searchable":True,"filterable":False,"facetable":False,"sortable":False,"dimensions":DIM,"vectorSearchProfile":"vec-default"})
fields.append({"name":"has_vector","type":"Edm.Boolean","filterable":True,"facetable":True})

schema={
    'name':INDEX,
    'fields':fields,
    'suggesters':[{"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","program_id","path"]}],
    'vectorSearch':{
        'algorithms':[{'name':'hnsw','kind':'hnsw','hnswParameters':{'m':8,'efConstruction':400,'efSearch':200}}],
        'profiles':[{'name':'vec-default','algorithm':'hnsw'}]
    }
}

headers={'api-key':KEY,'Content-Type':'application/json'}
# Delete existing
requests.delete(f"{EP.rstrip('/')}/indexes/{INDEX}?api-version={API}", headers={'api-key':KEY})
resp=requests.post(f"{EP.rstrip('/')}/indexes?api-version={API}", headers=headers, json=schema)
print('Create status', resp.status_code)
if resp.status_code not in (200,201):
    print(resp.text[:500])
    sys.exit(1)
print('Index recreated with vector field file_vector')
