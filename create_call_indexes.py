"""Create or update Azure AI Search indexes for COBOL call graph.

Indexes:
 1. new_cobol_calls (edge-level call occurrences with snippet vectors)
 2. new_cobol_program_meta (aggregated per-program metadata)

Usage:
  python create_call_indexes.py --endpoint $AZURE_SEARCH_ENDPOINT --key $AZURE_SEARCH_KEY \
      [--no-program-meta] [--overwrite]

Environment fallback: SEARCH_ENDPOINT / SEARCH_KEY or values from local.settings.json.
"""
from __future__ import annotations
import os, json, argparse, requests, sys
from typing import Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
CALLS_INDEX = 'new_cobol_calls'
PROGRAM_META_INDEX = 'new_cobol_program_meta'
VECTOR_DIM = 3072  # Align with embedding dimension used elsewhere


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
        print('Missing search endpoint/key (provide --endpoint/--key or set env).', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def base_common_fields():
    return [
        {"name":"ingested_at","type":"Edm.String","filterable":True,"facetable":False,"searchable":False,"sortable":True},
    ]


def calls_index_schema() -> Dict[str, Any]:
    return {
        "name": CALLS_INDEX,
        "fields": [
            {"name":"call_id","type":"Edm.String","key":True,"searchable":False,"filterable":True,"sortable":False,"facetable":False},
            {"name":"caller_program","type":"Edm.String","searchable":True,"filterable":True,"facetable":True,"sortable":False},
            {"name":"callee_program","type":"Edm.String","searchable":True,"filterable":True,"facetable":True,"sortable":False},
            {"name":"file_id","type":"Edm.String","searchable":False,"filterable":True,"facetable":False,"sortable":False},
            {"name":"file_path","type":"Edm.String","searchable":True,"filterable":False,"facetable":False,"sortable":False},
            {"name":"line","type":"Edm.Int32","filterable":True,"sortable":True,"facetable":False},
            {"name":"col","type":"Edm.Int32","filterable":True,"sortable":True,"facetable":False},
            {"name":"occurrence","type":"Edm.Int32","filterable":True,"sortable":True,"facetable":False},
            {"name":"call_type","type":"Edm.String","filterable":True,"facetable":True,"searchable":False},
            {"name":"is_dynamic","type":"Edm.Boolean","filterable":True,"facetable":True,"searchable":False},
            {"name":"snippet","type":"Edm.String","searchable":True,"filterable":False,"sortable":False,"facetable":False},
            {"name":"snippet_vector","type":"Collection(Edm.Single)","searchable":True,"dimensions":VECTOR_DIM,"vectorSearchProfile":"vector-profile"},
            {"name":"has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"call_hash","type":"Edm.String","searchable":False,"filterable":True,"facetable":False,"sortable":False},
            *base_common_fields()
        ],
        "vectorSearch": {
            "algorithms": [
                {"name": "hnsw-alg", "kind": "hnsw"}
            ],
            "profiles": [
                {"name": "vector-profile", "algorithm": "hnsw-alg"}
            ]
        },
        "semantic": {
            "configurations": [
                {
                    "name": "semantic-default",
                    "prioritizedFields": {
                        "titleField": None,
                        "prioritizedContentFields": [{"fieldName": "snippet"}],
                        "prioritizedKeywordsFields": []
                    }
                }
            ]
        }
    }


def program_meta_index_schema() -> Dict[str, Any]:
    return {
        "name": PROGRAM_META_INDEX,
        "fields": [
            {"name":"program_id","type":"Edm.String","key":True,"searchable":True,"filterable":True,"facetable":True},
            {"name":"outgoing_count","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"incoming_count","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"unique_callees","type":"Collection(Edm.String)","searchable":True,"filterable":True,"facetable":False},
            {"name":"unique_callers","type":"Collection(Edm.String)","searchable":True,"filterable":True,"facetable":False},
            {"name":"has_cycles","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"call_depth_score","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"sample_call_lines","type":"Edm.String","searchable":True},
            # --- New enrichment fields (2025-09) ---
            {"name":"program_summary","type":"Edm.String","searchable":True,"filterable":False,"facetable":False,"sortable":False},
            {"name":"program_role","type":"Edm.String","searchable":False,"filterable":True,"facetable":True,"sortable":False},
            {"name":"flow_graph_json","type":"Edm.String","searchable":False,"filterable":False,"facetable":False,"sortable":False},
            {"name":"ui_flag","type":"Edm.Boolean","searchable":False,"filterable":True,"facetable":True,"sortable":False},
            {"name":"input_screen_paths_json","type":"Edm.String","searchable":False,"filterable":False,"facetable":False,"sortable":False},
            # --- Advanced graph & risk enrichment fields ---
            {"name":"reach_out_size","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"reach_in_size","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"centrality_score","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"risk_flag","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"dynamic_call_ratio","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"external_callee_count","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"external_callees","type":"Collection(Edm.String)","searchable":False,"filterable":True,"facetable":False},
            {"name":"ui_path_participant","type":"Edm.Boolean","filterable":True,"facetable":True},
            # --- Coverage & paragraph enrichment fields (may be populated in later merge step) ---
            {"name":"total_lines","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"covered_lines","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"coverage_pct","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"paragraph_count","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"avg_paragraph_length","type":"Edm.Double","filterable":False,"sortable":True},
            {"name":"median_paragraph_length","type":"Edm.Double","filterable":False,"sortable":True},
            {"name":"max_paragraph_length","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"gap_count","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"largest_gap_length","type":"Edm.Int32","filterable":True,"sortable":True},
            {"name":"classification","type":"Edm.String","searchable":False,"filterable":True,"facetable":True,"sortable":False},
            {"name":"coverage_ingested_at","type":"Edm.String","searchable":False,"filterable":True,"facetable":False,"sortable":True},
            # --- Vector fields for program summary embedding ---
            {"name":"summary_vector","type":"Collection(Edm.Single)","searchable":True,"dimensions":VECTOR_DIM,"vectorSearchProfile":"vector-profile"},
            {"name":"has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
            *base_common_fields()
        ],
    "vectorSearch": {
        "algorithms": [
            {"name": "hnsw-alg", "kind": "hnsw"}
        ],
        "profiles": [
            {"name": "vector-profile", "algorithm": "hnsw-alg"}
        ]
    },
    "semantic": {"configurations":[{"name":"semantic-default","prioritizedFields":{"titleField":None,"prioritizedContentFields":[{"fieldName":"sample_call_lines"}],"prioritizedKeywordsFields":[]}}]},
    }


def index_exists(ep: str, key: str, name: str) -> bool:
    r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200


def create_or_overwrite(ep: str, key: str, schema: Dict[str, Any], overwrite: bool):
    name = schema['name']
    exists = index_exists(ep, key, name)
    if exists and not overwrite:
        print(f"Index '{name}' exists (skip). Use --overwrite to recreate if schema changed (e.g., added col/occurrence fields).")
        return
    if exists and overwrite:
        dr = requests.delete(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key': key})
        if dr.status_code not in (204,202):
            raise SystemExit(f"Failed deleting {name}: {dr.status_code} {dr.text[:200]}")
        print(f"Deleted existing index {name}")
    cr = requests.post(f"{ep}/indexes?api-version={API_VERSION}", headers={'api-key': key, 'Content-Type':'application/json'}, json=schema)
    if cr.status_code not in (200,201):
        raise SystemExit(f"Failed creating {name}: {cr.status_code} {cr.text[:500]}")
    print(f"Created index {name}")


def main():
    parser = argparse.ArgumentParser(description='Create COBOL call graph indexes.')
    parser.add_argument('--endpoint')
    parser.add_argument('--key')
    parser.add_argument('--no-program-meta', action='store_true')
    parser.add_argument('--overwrite', action='store_true')
    args = parser.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    # Create calls index first
    create_or_overwrite(ep, key, calls_index_schema(), args.overwrite)
    if not args.no_program_meta:
        create_or_overwrite(ep, key, program_meta_index_schema(), args.overwrite)

if __name__ == '__main__':
    main()
