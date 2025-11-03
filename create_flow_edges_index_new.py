"""Create/update the new_cobol_flow_edges index (control/data flow edges with vectors).

Each edge document captures intra-program or inter-paragraph flow relationships.

Schema goals:
  - Edge-level text for semantic retrieval (edge_text)
  - Vector field (edge_vector) 3072-dim to align with existing embedding pipeline
  - Flags for classification and resolution metadata
  - Filter/sort fields kept minimal (line, edge_subkind, resolution_strategy)

Usage:
  python create_flow_edges_index_new.py [--overwrite]

Env fallback: AZURE_SEARCH_ENDPOINT/KEY or SEARCH_ENDPOINT/KEY or local.settings.json Values.
"""
import os, json, sys, argparse, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_flow_edges'
VECTOR_DIM = 3072

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def schema():
    return {
        "name": INDEX_NAME,
        "fields": [
            {"name":"edge_id","type":"Edm.String","key":True,"searchable":False,"filterable":True},
            {"name":"file_id","type":"Edm.String","searchable":False,"filterable":True},
            {"name":"program_id","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"caller_para","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"target_para","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"resolved_target_para","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"edge_subkind","type":"Edm.String","searchable":False,"filterable":True,"facetable":True},
            {"name":"edge_kind","type":"Edm.String","searchable":False,"filterable":True,"facetable":True},
            {"name":"resolution_strategy","type":"Edm.String","searchable":False,"filterable":True,"facetable":True},
            {"name":"line","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"edge_text","type":"Edm.String","searchable":True},
            {"name":"edge_vector","type":"Collection(Edm.Single)","searchable":True,"dimensions":VECTOR_DIM,"vectorSearchProfile":"vector-profile"},
            {"name":"has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"ingested_at","type":"Edm.String","filterable":True,"sortable":True},
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw-alg","kind":"hnsw"}],
            "profiles": [{"name":"vector-profile","algorithm":"hnsw-alg"}]
        },
        "semantic": {
            "configurations": [
                {"name":"semantic-default","prioritizedFields":{"titleField":None,"prioritizedContentFields":[{"fieldName":"edge_text"}],"prioritizedKeywordsFields":[]}}
            ]
        }
    }

def index_exists(ep: str, key: str) -> bool:
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200

def create_or_overwrite(ep: str, key: str, overwrite: bool):
    if index_exists(ep,key):
        if not overwrite:
            print(f"Index {INDEX_NAME} exists (skip). Use --overwrite to recreate.")
            return
        dr = requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
        if dr.status_code not in (204,202):
            raise SystemExit(f"Delete failed {dr.status_code}: {dr.text[:200]}")
        print(f"Deleted {INDEX_NAME}")
    cr = requests.post(f"{ep}/indexes?api-version={API_VERSION}", headers={'api-key': key,'Content-Type':'application/json'}, json=schema())
    if cr.status_code not in (200,201):
        raise SystemExit(f"Create failed {cr.status_code}: {cr.text[:400]}")
    print(f"Created {INDEX_NAME}")

def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_flow_edges index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--overwrite', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    create_or_overwrite(ep, key, args.overwrite)

if __name__ == '__main__':
    main()
