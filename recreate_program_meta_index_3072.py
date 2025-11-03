"""Recreate new_cobol_program_meta index with program_summary_vector dimension 3072.

Steps:
1. Load settings (local.settings.json if present).
2. Export existing index schema + documents.
3. Modify schema in-memory (set vector field to 3072 or add if missing).
4. Delete existing index.
5. Recreate index with new schema.
6. Re-upload documents (excluding any existing program_summary_vector values).

Does NOT run embedding backfill; run backfill_embeddings_program_meta.py afterwards.
"""
import os, json, sys, time, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'
VECTOR_FIELD='program_summary_vector'
NEW_DIM=3072
BATCH=1000

def load_settings():
    try:
        vals=json.load(open('local.settings.json')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def get_schema(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code!=200:
        print('Fetch schema failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json()

def list_all_docs(ep,key):
    all_docs=[]; skip=0
    while True:
        body={'search':'*','top':BATCH,'skip':skip,'queryType':'simple','select':'*'}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
        if r.status_code!=200:
            print('Doc fetch failed', r.status_code, r.text[:300]); sys.exit(1)
        batch=r.json().get('value',[])
        if not batch: break
        all_docs.extend(batch)
        skip+=BATCH
        if len(batch)<BATCH: break
    return all_docs

def adjust_schema(schema):
    # Normalize vectorSearch structure
    vs = schema.get('vectorSearch')
    if not isinstance(vs, dict):
        schema['vectorSearch']={'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}], 'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]}
    else:
        vs.setdefault('algorithms',[])
        if not any(a.get('name')=='hnsw-alg' for a in vs['algorithms']):
            vs['algorithms'].append({'name':'hnsw-alg','kind':'hnsw'})
        vs.setdefault('profiles',[])
        if not any(p.get('name')=='vprofile' for p in vs['profiles']):
            vs['profiles'].append({'name':'vprofile','algorithm':'hnsw-alg'})
    found=False
    for f in schema.get('fields',[]):
        if f.get('name')==VECTOR_FIELD:
            f['dimensions']=NEW_DIM
            f['type']='Collection(Edm.Single)'
            f['searchable']=True
            f['vectorSearchProfile']='vprofile'
            f['retrievable']=False
            found=True
            break
    if not found:
        schema['fields'].append({
            'name': VECTOR_FIELD,
            'type':'Collection(Edm.Single)',
            'searchable':True,
            'dimensions':NEW_DIM,
            'vectorSearchProfile':'vprofile',
            'retrievable':False
        })
    return schema

def delete_index(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code not in (204,404):
        print('Delete failed', r.status_code, r.text[:300]); sys.exit(1)


def create_index(ep,key,schema):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print('Create failed', r.status_code, r.text[:400]); sys.exit(1)


def upload_docs(ep,key,docs):
    # Strip vector field if present
    for d in docs:
        if VECTOR_FIELD in d:
            del d[VECTOR_FIELD]
    for i in range(0,len(docs),BATCH):
        chunk=docs[i:i+BATCH]
        payload={'value':[]}
        for d in chunk:
            d['@search.action']='upload'
            payload['value'].append(d)
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=payload)
        if r.status_code not in (200,201):
            print('Upload failed', r.status_code, r.text[:300]); sys.exit(1)
        print(f'Uploaded {min(i+BATCH,len(docs))}/{len(docs)}')

if __name__=='__main__':
    load_settings(); ep,key=resolve()
    print('Exporting current schema...')
    schema=get_schema(ep,key)
    open('program_meta_schema.before.json','w',encoding='utf-8').write(json.dumps(schema,indent=2))
    print('Exporting documents (this may take time)...')
    docs=list_all_docs(ep,key)
    open('program_meta_docs.export.json','w',encoding='utf-8').write(json.dumps(docs,indent=2))
    print(f'Exported {len(docs)} docs.')
    print('Adjusting schema for 3072-dim vector...')
    new_schema=adjust_schema(schema)
    open('program_meta_schema.3072.json','w',encoding='utf-8').write(json.dumps(new_schema,indent=2))
    print('Deleting old index...')
    delete_index(ep,key)
    time.sleep(2)
    print('Creating new index...')
    create_index(ep,key,new_schema)
    print('Re-uploading documents (without vectors)...')
    upload_docs(ep,key,docs)
    print('Recreation complete. Run backfill_embeddings_program_meta.py next.')
