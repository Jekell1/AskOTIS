"""Create v2 flow edges index with improved schema (new_cobol_flow_edges_v2)."""
import os, json, argparse, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'
VECTOR_DIM=3072

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def schema():
    return {
        'name': INDEX,
        'fields': [
            {'name':'edge_id','type':'Edm.String','key':True,'searchable':False,'filterable':True},
            {'name':'file_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True},
            {'name':'program_id','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'caller_para','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'target_para','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'resolved_target_para','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'edge_kind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True},
            {'name':'edge_subkind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True},
            {'name':'edge_text','type':'Edm.String','searchable':True},
            {'name':'line','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'edge_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vector-profile'},
            {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True},
            {'name':'ingested_at','type':'Edm.String','filterable':True,'sortable':True},
        ],
        'vectorSearch': {
            'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],
            'profiles':[{'name':'vector-profile','algorithm':'hnsw-alg'}]
        },
        'semantic':{
            'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':None,'prioritizedContentFields':[{'fieldName':'edge_text'}],'prioritizedKeywordsFields':[]}}]
        }
    }

def exists(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})
    return r.status_code==200

def create(ep,key,overwrite=False):
    if exists(ep,key):
        if not overwrite:
            print('Index exists; use --overwrite to recreate'); return
        dr=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})
        if dr.status_code not in (204,202):
            raise SystemExit(f'Delete failed {dr.status_code}: {dr.text[:200]}')
        print('Deleted existing index')
    cr=requests.post(f"{ep}/indexes?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=schema())
    if cr.status_code not in (200,201):
        raise SystemExit(f'Create failed {cr.status_code}: {cr.text[:200]}')
    print('Created', INDEX)

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve(args); create(ep,key,args.overwrite)
