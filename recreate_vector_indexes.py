#!/usr/bin/env python3
"""Delete and recreate vector-enabled indexes (chunks, symbols, paragraphs, calls, xrefs).

Steps:
 1. Load local.settings.json for endpoint/key.
 2. Delete indexes if they exist.
 3. POST new definitions (including vectorSearch block, semantic optional).
 4. Print success + next instructions (re-push docs, run backfill_vectors.py).

Safe: Fails fast on HTTP errors. Only touches the target indexes listed.
"""
import json, sys, requests, time, argparse

API_VERSION = "2024-07-01"
# Extended to include additional high-value indexes for vector search
TARGET_INDEXES = [
    "code-chunks",
    "cobol-symbols",
    "cobol-paragraphs",
    "cobol-calls",
    "cobol-xrefs"
]

def load_settings():
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT'))
    key = (vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY'))
    if not ep or not key:
        raise SystemExit("Missing AZURE_SEARCH_ENDPOINT/KEY in local.settings.json")
    return ep.rstrip('/'), key

def delete_index(ep, key, name):
    url = f"{ep}/indexes/{name}?api-version={API_VERSION}"
    r = requests.delete(url, headers={'api-key':key})
    if r.status_code not in (204,404):
        raise RuntimeError(f"Delete {name} failed {r.status_code}: {r.text[:200]}")
    print(f"Deleted (or absent) {name}")

def create_index(ep, key, payload):
    url = f"{ep}/indexes?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=180)
    if r.status_code >= 300:
        raise RuntimeError(f"Create {payload['name']} failed {r.status_code}: {r.text[:400]}")
    print(f"Created {payload['name']}")

def chunk_index_payload(dim):
    return {
        "name": "code-chunks",
        "fields": [
            {"name":"chunk_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"path","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"scope","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"start_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"end_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"text","type":"Edm.String","searchable":True},
            {"name":"text_vector","type":"Collection(Edm.Single)","dimensions":dim,"vectorSearchProfile":"vec-default","retrievable":True}
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","path","program_id"]}
        ],
        "vectorSearch": {
            "algorithms":[{"name":"hnsw","kind":"hnsw"}],
            "profiles":[{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def symbols_index_payload(dim):
    return {
        "name": "cobol-symbols",
        "fields": [
            {"name":"item_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"path","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"qualified_name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"section","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"level","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"pic","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"usage","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"occurs_low","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"occurs_high","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"depends_on","type":"Edm.String","filterable":True},
            {"name":"redefines","type":"Edm.String","filterable":True},
            {"name":"renames","type":"Edm.String","filterable":True},
            {"name":"value","type":"Edm.String","filterable":True},
            {"name":"start_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"end_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"name_vector","type":"Collection(Edm.Single)","dimensions":dim,"vectorSearchProfile":"vec-default","retrievable":True}
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","qualified_name","path"]}
        ],
        "vectorSearch": {
            "algorithms":[{"name":"hnsw","kind":"hnsw"}],
            "profiles":[{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def paragraphs_index_payload(dim):
    return {
        "name": "cobol-paragraphs",
        "fields": [
            {"name":"para_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"start_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"end_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"name_vector","type":"Collection(Edm.Single)","dimensions":dim,"vectorSearchProfile":"vec-default","retrievable":True}
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name"]}
        ],
        "vectorSearch": {
            "algorithms":[{"name":"hnsw","kind":"hnsw"}],
            "profiles":[{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def calls_index_payload(dim):
    return {
        "name": "cobol-calls",
        "fields": [
            {"name":"call_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"caller_para","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"callee_program","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"callee_data_name","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"is_dynamic","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"snippet","type":"Edm.String","searchable":True},
            {"name":"snippet_vector","type":"Collection(Edm.Single)","dimensions":dim,"vectorSearchProfile":"vec-default","retrievable":True}
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["callee_program","callee_data_name","caller_para"]}
        ],
        "vectorSearch": {
            "algorithms":[{"name":"hnsw","kind":"hnsw"}],
            "profiles":[{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def xrefs_index_payload(dim):
    return {
        "name": "cobol-xrefs",
        "fields": [
            {"name":"xref_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"path","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"qualified_name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"simple_name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"direction","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"start_col","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"end_col","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"snippet","type":"Edm.String","searchable":True},
            {"name":"snippet_vector","type":"Collection(Edm.Single)","dimensions":dim,"vectorSearchProfile":"vec-default","retrievable":True}
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["simple_name","qualified_name","path"]}
        ],
        "vectorSearch": {
            "algorithms":[{"name":"hnsw","kind":"hnsw"}],
            "profiles":[{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--dim', type=int, default=1536, help='Vector dimension (use 3072 for text-embedding-3-large)')
    args = ap.parse_args()
    ep, key = load_settings()
    print(f"Endpoint: {ep}")
    print(f"Recreating indexes with vector dimension {args.dim}")
    for ix in TARGET_INDEXES:
        delete_index(ep,key,ix)
        time.sleep(0.15)
    # (Re)create all with uniform vector profile naming
    create_index(ep,key,chunk_index_payload(args.dim))
    create_index(ep,key,symbols_index_payload(args.dim))
    create_index(ep,key,paragraphs_index_payload(args.dim))
    create_index(ep,key,calls_index_payload(args.dim))
    create_index(ep,key,xrefs_index_payload(args.dim))
    print(f"""
Next steps:
 1. Re-push documents:
     python push_chunks_symbols.py --container aisearch --prefix S35-Source/ --which chunks --batch-size 500 --max-text-bytes 30000
     python push_chunks_symbols.py --container aisearch --prefix S35-Source/ --which symbols --batch-size 500 --dedupe
 2. Backfill embeddings:
     python backfill_vectors.py --indexes code-chunks,cobol-symbols --batch 64 --embed-deployment text-embedding-3-large
 3. Sample vector query:
     curl -X POST "$SEARCH_ENDPOINT/indexes/code-chunks/docs/search?api-version=2024-07-01" \
            -H "api-key: $SEARCH_KEY" -H "Content-Type: application/json" \
            -d '{{"vectorQueries":[{{"vector":[0.0,0.0],"k":3,"fields":"text_vector"}}],"search":"*","top":3}}'
""")

if __name__ == '__main__':
    main()
