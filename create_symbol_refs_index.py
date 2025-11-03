"""Create new_cobol_symbol_refs index (fine-grained variable/data item reference occurrences)."""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
# Embedding dimension (reduced from previous 3072 to shrink vector memory footprint). Override with SYMBOL_REF_VECTOR_DIM.
VECTOR_DIM=int(os.getenv('SYMBOL_REF_VECTOR_DIM','1536'))

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def exists(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    return r.status_code==200

def delete(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code not in (200,204,404):
        print('Delete failed', r.status_code, r.text[:200]); sys.exit(1)

def create(ep,key,force:bool):
    if exists(ep,key):
        if not force:
            print('Index exists (use --force)'); return
        delete(ep,key)
    schema={
        'name': INDEX,
        'fields': [
            {'name':'ref_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'retrievable':True},
            {'name':'program_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'symbol_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'symbol_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'symbol_id_global','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'kind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'op','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'normalized_kind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'line_number','type':'Edm.Int32','searchable':False,'filterable':True,'retrievable':True},
            {'name':'file_path','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
            {'name':'paragraph_name','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            # Standardize on excerpt_text (was excerpt in some earlier variants)
            {'name':'excerpt_text','type':'Edm.String','searchable':True,'filterable':False,'retrievable':True},
            {'name':'context_before','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
            {'name':'context_after','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
            {'name':'excerpt_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'ref-vector-profile'},
            {'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'first_in_program','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'is_first_write','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'cluster_key','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'ingested_at','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True},
            {'name':'updated_at','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True}
        ],
        'vectorSearch':{
            'algorithms':[{'name':'ref-hnsw','kind':'hnsw'}],
            'profiles':[{'name':'ref-vector-profile','algorithm':'ref-hnsw'}]
        },
        'semantic':{
            'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':None,'prioritizedContentFields':[{'fieldName':'excerpt_text'}]}}]
        },
        'corsOptions': {'allowedOrigins':['*']}
    }
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('Create failed', r.status_code, r.text[:400]); sys.exit(1)
    print(f'Created index {INDEX} (VECTOR_DIM={VECTOR_DIM})')

def main():
    ap=argparse.ArgumentParser(description='Create symbol refs index')
    ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--force',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve(args); create(ep,key,args.force)

if __name__=='__main__':
    main()
