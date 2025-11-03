"""Export per-index embedding footprint metrics to CSV.

Columns:
  index_name, vector_field, dimensions, has_vector_field, with_vector, without_vector, coverage_pct, approx_bytes

Usage:
  python export_embedding_footprint.py > embedding_footprint.csv
"""
from __future__ import annotations
import os, json, sys, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def list_indexes(ep,key):
    r=requests.get(f"{ep}/indexes?api-version={API}",headers={'api-key':key}); r.raise_for_status(); return r.json().get('value',[])

def get_index(ep,key,name):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API}",headers={'api-key':key}); r.raise_for_status(); return r.json()

def count(ep,key,index,filt):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True,'filter':filt})
    if r.status_code!=200: return None
    return r.json().get('@odata.count',0)

def detect_vectors(idx):
    out=[]
    for f in idx.get('fields',[]):
        dim=f.get('vectorSearchDimensions') or f.get('dimensions')
        if dim and f.get('type','').startswith('Collection(Edm.Single)'):
            out.append({'name':f['name'],'dim':dim})
    return out

def main():
    load(); ep,key=resolve()
    print('index_name,vector_field,dimensions,has_vector_field,with_vector,without_vector,coverage_pct,approx_bytes')
    for meta in list_indexes(ep,key):
        name=meta.get('name')
        try: idx=get_index(ep,key,name)
        except Exception: continue
        vectors=detect_vectors(idx)
        if not vectors: continue
        fields={f['name'] for f in idx.get('fields',[])}
        has_flag='has_vector' in fields
        with_v=None; without_v=None; cov=None
        if has_flag:
            with_v=count(ep,key,name,'has_vector eq true')
            without_v=count(ep,key,name,'has_vector eq false')
            if with_v is not None and without_v is not None and (with_v+without_v)>0:
                cov=round(with_v/(with_v+without_v)*100.0,2)
        for vf in vectors:
            est_bytes = (with_v or 0) * vf['dim'] * 4 if cov is not None else ''
            print(f"{name},{vf['name']},{vf['dim']},{has_flag},{with_v if with_v is not None else ''},{without_v if without_v is not None else ''},{cov if cov is not None else ''},{est_bytes}")

if __name__=='__main__':
    main()
