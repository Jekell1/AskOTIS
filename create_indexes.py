#!/usr/bin/env python3
"""
create_indexes.py — COBOL JSONL → Azure AI Search

Creates:
  - Data sources (pointing at JSONL folder, not individual blobs)
  - Indexes (code-chunks, cobol-symbols, cobol-xrefs, cobol-calls)
  - Indexers with jsonLines parsing + field mappings

Run:
  python create_indexes.py --container aisearch --prefix S35-Source/ --auto
"""

import argparse, json, os, sys, time, requests, hashlib
from typing import Dict, List
from azure.storage.blob import BlobServiceClient

API_VERSION = "2024-07-01"

# ------------------------------
# Config
# ------------------------------
def load_local_settings(path="local.settings.json") -> Dict[str, str]:
    if not os.path.exists(path):
        raise FileNotFoundError(f"{path} not found")
    raw = json.load(open(path, "r", encoding="utf-8"))
    vals = raw.get("Values", {})
    if not vals.get("AzureWebJobsStorage"):
        raise RuntimeError("Missing AzureWebJobsStorage")
    if not (vals.get("AZURE_SEARCH_ENDPOINT") or vals.get("SEARCH_ENDPOINT")):
        raise RuntimeError("Missing AZURE_SEARCH_ENDPOINT/SEARCH_ENDPOINT")
    if not (vals.get("AZURE_SEARCH_KEY") or vals.get("SEARCH_KEY")):
        raise RuntimeError("Missing AZURE_SEARCH_KEY/SEARCH_KEY")
    return vals

# ------------------------------
# REST helpers
# ------------------------------
def _path(kind, name): return f"/{kind}('{name}')"

def sreq(endpoint, key, method, path, body=None, dry=False):
    url = f"{endpoint.rstrip('/')}{path}{'&' if '?' in path else '?'}api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type": "application/json"}
    if dry:
        print(f"DRY {method} {url}")
        if body: print(json.dumps(body)[:500])
        return None
    r = getattr(requests, method.lower())(url, headers=headers, json=body, timeout=120)
    if r.status_code >= 400:
        raise RuntimeError(f"{method} {url} -> {r.status_code} {r.text[:500]}")
    return r.json() if r.text else None

# ------------------------------
# Index schemas
# ------------------------------
def index_code_chunks():
    return {
        "name": "code-chunks",
        "fields": [
            {"name": "chunk_id","type":"Edm.String","key":True},
            {"name": "file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name": "path","type":"Edm.String","searchable":True,"filterable":True},
            {"name": "program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name": "scope","type":"Edm.String","filterable":True,"facetable":True},
            {"name": "name","type":"Edm.String","searchable":True,"filterable":True},
            {"name": "start_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name": "end_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name": "text","type":"Edm.String","searchable":True,"filterable":False,"sortable":False,"facetable":False},
            # Vector embedding field for semantic / hybrid retrieval
            # Updated for 2024-07-01 API: use vectorSearchProfile and make vector field searchable
            {"name": "text_vector","type": "Collection(Edm.Single)","searchable": True,"filterable": False,"facetable": False,"sortable": False,"dimensions": 3072,"vectorSearchProfile": "vec-default"},
            {"name": "has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","path","program_id"]}
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw","kind":"hnsw"}],
            "profiles": [{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

# ------------------------------------------------------------------
# Minimal helper functions (parity with search_setup/create_indexes.py)
# Added so tests importing from root-level create_indexes succeed even
# if they expect helper-style functions defined in the alternate module.
# ------------------------------------------------------------------
def create_hnsw_algorithm_config():
    return {
        "name": "hnsw-algo",
        "kind": "hnsw",
        "parameters": {"m": 24, "efConstruction": 200}
    }

def create_hnsw_vector_profile():
    return {"name": "vprof", "algorithm": "hnsw-algo"}

def create_semantic_configuration():
    return {
        "name": "semcfg",
        "prioritizedFields": {
            "titleField": {"fieldName": "name"},
            "contentFields": [{"fieldName": "text"}, {"fieldName": "path"}],
            "keywordsFields": []
        }
    }

def create_complete_index_payload(index_name, fields, include_vector_search=False, include_semantic_config=False):
    payload = {"name": index_name, "fields": fields}
    if include_vector_search:
        payload["vectorSearch"] = {
            "algorithms": [create_hnsw_algorithm_config()],
            "profiles": [create_hnsw_vector_profile()]
        }
    if include_semantic_config:
        payload["semantic"] = {"configurations": [create_semantic_configuration()]}
    return payload

def index_files():
    return {
        "name": "cobol-files",
        "fields": [
            {"name":"file_id","type":"Edm.String","key":True},
            {"name":"path","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"program_id","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"lines","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"format","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"procedure_using","type":"Collection(Edm.String)","searchable":True,"filterable":True,"facetable":True},
            {"name":"copybook","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","program_id","path"]}
        ]
    }

def index_paragraphs():
    return {
        "name": "cobol-paragraphs",
        "fields": [
            # Make key filterable/sortable to enable key-based paging in vector backfill
            {"name":"para_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"start_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"end_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            # Enrichment fields (additive; safe to ignore if unpopulated)
            {"name":"role","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"phase","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"phase_seq","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"role_confidence","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"phase_confidence","type":"Edm.Double","filterable":True,"sortable":True},
            # Vector embedding for paragraph name (short identifier semantics)
            {"name":"name_vector","type":"Collection(Edm.Single)","searchable":True,"filterable":False,"facetable":False,"sortable":False,"dimensions":3072,"vectorSearchProfile":"vec-default"},
            {"name": "has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name"]}
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw","kind":"hnsw"}],
            "profiles": [{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def index_facts():
    return {
        "name": "cobol-facts",
        "fields": [
            {"name":"fact_id","type":"Edm.String","key":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"para","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"snippet","type":"Edm.String","searchable":True},
            {"name":"callee","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"callee_data_name","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"is_dynamic","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"using_raw","type":"Edm.String","searchable":True},
            {"name":"target","type":"Edm.String","searchable":True},
            {"name":"source_raw","type":"Edm.String","searchable":True},
            {"name":"expr_raw","type":"Edm.String","searchable":True},
            # Enriched action semantics
            {"name":"action_role","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"posting_type","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"gating_cond","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"fact_confidence","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"sources","type":"Collection(Edm.String)","searchable":True,"filterable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["callee","callee_data_name","snippet"]}
        ]
    }


def index_flow_edges_v2():
    """Enriched flow edges index (non-destructive new version).

    Added fields:
      - raw_target: original unresolved target token
      - resolved_target_para: canonical resolved paragraph/section name
      - resolved: boolean resolution flag
      - confidence: numeric confidence of resolution
      - edge_subkind: refined classification (perform-paragraph, perform-section, goto-label, etc.)
      - caller_program_id / target_program_id / file_program_id
      - family_key: normalized family grouping token
      - resolution_strategy: strategy label (exact, normalized-equals, prefix-match, fuzzy, ambiguous, none)
      - version: schema/enrichment version tag
      - created_ts: ISO 8601 timestamp of enrichment
    """
    return {
        "name": "cobol-flow-edges-v2",
        "fields": [
            {"name":"edge_id","type":"Edm.String","key":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"caller_para","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"raw_target","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"resolved_target_para","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"resolved","type":"Edm.Boolean","filterable":True,"facetable":True},
            {"name":"confidence","type":"Edm.Double","filterable":True,"facetable":True,"sortable":True},
            {"name":"edge_subkind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"caller_program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"target_program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"file_program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"family_key","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"resolution_strategy","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"version","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"created_ts","type":"Edm.String","filterable":True,"sortable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["caller_para","resolved_target_para","family_key"]}
        ]
    }

def index_routine_aliases():
    """Alias / family mapping index for routine name normalization."""
    return {
        "name": "cobol-routine-aliases",
        "fields": [
            {"name":"alias","type":"Edm.String","key":True},
            {"name":"family_key","type":"Edm.String","filterable":True,"facetable":True,"searchable":True},
            {"name":"canonical_target","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"candidate_targets","type":"Collection(Edm.String)","searchable":True,"filterable":True},
            {"name":"confidence","type":"Edm.Double","filterable":True,"facetable":True,"sortable":True},
            {"name":"alias_kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"version","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"created_ts","type":"Edm.String","filterable":True,"sortable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["alias","family_key","canonical_target"]}
        ]
    }

def index_summaries():
    """Index for generated semantic summaries of routines / files / families."""
    return {
        "name": "cobol-summaries",
        "fields": [
            {"name":"summary_id","type":"Edm.String","key":True},
            {"name":"entity_kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"family_key","type":"Edm.String","filterable":True,"facetable":True,"searchable":True},
            {"name":"program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"source_ids","type":"Collection(Edm.String)","filterable":True},
            {"name":"summary_text","type":"Edm.String","searchable":True},
            {"name":"evidence_json","type":"Edm.String","searchable":False},
            {"name":"version","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"created_ts","type":"Edm.String","filterable":True,"sortable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","family_key","program_id"]}
        ]
    }

def index_copybooks():
    return {
        "name": "cobol-copybooks",
        "fields": [
            {"name":"copybook_id","type":"Edm.String","key":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"parent_path","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"copybook_name","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"replacing_clause","type":"Edm.String","searchable":True},
            # Variant grouping enrichment
            {"name":"canonical_base","type":"Edm.String","searchable":True,"filterable":True,"facetable":True},
            {"name":"variant_kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"variant_group_id","type":"Edm.String","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["copybook_name"]}
        ]
    }

def index_symbols():
    return {
        "name": "cobol-symbols",
        "fields": [
            {"name":"item_id","type":"Edm.String","key":True},
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
            # Vector field representing embedding of best identifier text (e.g., qualified_name)
            {"name": "name_vector","type": "Collection(Edm.Single)","searchable": True,"filterable": False,"facetable": False,"sortable": False,"dimensions": 3072,"vectorSearchProfile": "vec-default"},
            {"name": "has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","qualified_name","path"]}
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw","kind":"hnsw"}],
            "profiles": [{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def index_xrefs():
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
            # Vector embedding over snippet (fallbacks in backfill if empty)
            {"name":"snippet_vector","type":"Collection(Edm.Single)","searchable":True,"filterable":False,"facetable":False,"sortable":False,"dimensions":3072,"vectorSearchProfile":"vec-default"},
            {"name": "has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["simple_name","qualified_name","path"]}
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw","kind":"hnsw"}],
            "profiles": [{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

def index_calls():
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
            # Vector embedding for call snippet (or synthesized text)
            {"name":"snippet_vector","type":"Collection(Edm.Single)","searchable":True,"filterable":False,"facetable":False,"sortable":False,"dimensions":3072,"vectorSearchProfile":"vec-default"},
            {"name": "has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["callee_program","callee_data_name","caller_para"]}
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw","kind":"hnsw"}],
            "profiles": [{"name":"vec-default","algorithm":"hnsw"}]
        }
    }

# ------------------------------
# Data sources & indexers
# ------------------------------
def make_blob_ds(name, conn_str, container, prefix):
    return {
        "name": name,
        "type": "azureblob",
        "credentials": {"connectionString": conn_str},
        "container": {"name": container, "query": prefix}
    }

def make_indexer(name, ds, idx, fieldnames):
    fm = [{"sourceFieldName": f, "targetFieldName": f} for f in fieldnames]
    return {
        "name": name,
        "dataSourceName": ds,
        "targetIndexName": idx,
    "parameters": {"configuration": {"parsingMode":"jsonLines","indexedFileNameExtensions":".jsonl","failOnUnsupportedContentType":False,"dataToExtract":"contentAndMetadata"}},
        "fieldMappings": fm,
        "schedule": None
    }

# ------------------------------
# Upsert helpers
# ------------------------------
def upsert_data_source(endpoint, key, ds_body, dry=False):
    print(f"🗂 Data source: {ds_body['name']}")
    try:
        try:
            sreq(endpoint,key,"POST","/datasources",ds_body,dry=dry)
            return
        except RuntimeError as e:
            msg=str(e)
            if 'already exists' in msg:
                sreq(endpoint,key,"PUT",_path("datasources",ds_body["name"]),ds_body,dry=dry)
                return
            if '403' in msg:
                # Fallback: check existence then try PUT
                try:
                    existing = sreq(endpoint,key,"GET",_path("datasources",ds_body["name"]))
                    if existing:
                        print("   ↪️  Exists, trying PUT after 403 POST")
                        sreq(endpoint,key,"PUT",_path("datasources",ds_body["name"]),ds_body,dry=dry)
                        return
                except Exception:
                    pass
                print("   ⚠️ 403 creating data source; continuing (assume insufficient create perms but existing ds usable)")
                return
    except RuntimeError as e:
        if "already exists" in str(e):
            sreq(endpoint,key,"PUT",_path("datasources",ds_body["name"]),ds_body,dry=dry)
        else: raise

def upsert_index(endpoint, key, index_body, dry=False):
    name = index_body["name"]
    print(f"📚 Index: {name}")
    try:
        sreq(endpoint,key,"POST","/indexes",index_body,dry=dry)
    except RuntimeError as e:
        msg = str(e)
        if "already exists" in msg or "ResourceNameAlreadyInUse" in msg:
            try:
                sreq(endpoint,key,"PUT",_path("indexes",name),index_body,dry=dry)
            except RuntimeError as e2:
                if "CannotChangeExistingField" in str(e2):
                    print(f"⚠️  Schema conflict on index '{name}', deleting + recreating...")
                    sreq(endpoint,key,"DELETE",_path("indexes",name),None,dry=dry)
                    sreq(endpoint,key,"POST","/indexes",index_body,dry=dry)
                else:
                    raise
        elif "403" in msg:
            # Attempt to detect if index already exists (query perms) or try PUT creation
            print(f"⚠️  403 on POST create for index '{name}'. Attempting existence check + PUT fallback...")
            try:
                existing = sreq(endpoint,key,"GET",_path("indexes",name))
                if existing:
                    print(f"   ↪️  Index '{name}' exists (GET succeeded). Skipping create.")
                    return
            except Exception as eg:
                print(f"   ↪️  GET existence check failed: {eg}")
            try:
                sreq(endpoint,key,"PUT",_path("indexes",name),index_body,dry=dry)
                print(f"   ↪️  PUT fallback succeeded for '{name}'.")
            except Exception as ep:
                print(f"   ❌ PUT fallback also failed for '{name}': {ep}")
                raise
        else:
            raise

def upsert_indexer(endpoint, key, idx_body, dry=False):
    print(f"🧰 Indexer: {idx_body['name']}")
    try:
        sreq(endpoint,key,"POST","/indexers",idx_body,dry=dry)
    except RuntimeError as e:
        if "already exists" in str(e):
            sreq(endpoint,key,"PUT",_path("indexers",idx_body["name"]),idx_body,dry=dry)
        else: raise

# ------------------------------
# Main
# ------------------------------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--container",required=True)
    ap.add_argument("--prefix",required=True)
    ap.add_argument("--auto",action="store_true")
    ap.add_argument("--key",help="Override search admin key (takes precedence over settings file)")
    args = ap.parse_args()

    vals = load_local_settings()
    ep = (
        os.environ.get("AZURE_SEARCH_ENDPOINT") or
        os.environ.get("SEARCH_ENDPOINT") or
        vals.get("AZURE_SEARCH_ENDPOINT") or
        vals.get("SEARCH_ENDPOINT")
    )
    key_source = ""
    if args.key:
        key = args.key.strip()
        key_source = "--key arg"
    elif os.environ.get("AZURE_SEARCH_KEY"):
        key = os.environ.get("AZURE_SEARCH_KEY").strip()
        key_source = "env:AZURE_SEARCH_KEY"
    elif os.environ.get("SEARCH_KEY"):
        key = os.environ.get("SEARCH_KEY").strip()
        key_source = "env:SEARCH_KEY"
    else:
        key = (vals.get("AZURE_SEARCH_KEY") or vals.get("SEARCH_KEY")).strip()
        key_source = "local.settings.json"
    blob_conn = vals["AzureWebJobsStorage"]

    # Debug: print a short hash of the key so we can confirm which key the script is using (without exposing full key)
    try:
        khash = hashlib.sha256(key.encode()).hexdigest()[:16]
        print(f"🔐 Using search key source={key_source} hash={khash}")
    except Exception:
        pass

    base_prefix = f"{args.prefix.rstrip('/')}/JSONL"

    # Transitional helper: copy flat JSONL into per-dataset subfolders if parser hasn't been updated yet.
    dataset_files = {
        "chunks": ("chunks", "chunks.jsonl"),
        "symbols": ("symbols", "data_items.jsonl"),
        "xrefs": ("xrefs", "xrefs.jsonl"),
        "calls": ("calls", "calls.jsonl"),
        "files": ("files", "files.jsonl"),
        "paragraphs": ("paragraphs", "paragraphs.jsonl"),
        "facts": ("procedure_facts", "procedure_facts.jsonl"),
        "flow_edges": ("flow_edges", "flow_edges.jsonl"),
        "copybooks": ("copybooks", "copybooks.jsonl"),
    }
    bsc = BlobServiceClient.from_connection_string(blob_conn)
    cc = bsc.get_container_client(args.container)
    existing = {b.name for b in cc.list_blobs(name_starts_with=base_prefix)}
    # IMPORTANT: use a different loop variable name to avoid shadowing the admin 'key'
    for dataset_name,(folder,fname) in dataset_files.items():
        flat = f"{base_prefix}/{fname}"
        nested = f"{base_prefix}/{folder}/{fname}"
        if nested in existing:
            continue
        if flat in existing:
            try:
                print(f"↪️  Seed folder copy {flat} -> {nested}")
                data = cc.get_blob_client(flat).download_blob().readall()
                cc.get_blob_client(nested).upload_blob(data, overwrite=True)
            except Exception as e:
                print(f"⚠️ seed copy failed for {fname}: {e}")

    # NOTE: Parser currently writes flat files (e.g. paragraphs.jsonl) directly under base_prefix
    # not under per-entity subfolders for paragraphs/facts/flow_edges/copybooks. Point data sources
    # directly at the flat file paths so indexers can ingest.
    # Use folder prefixes (with trailing slash) so indexer enumerates all future shard files.
    ds = {
        # Remove trailing slash (diagnostic) so prefix matches both folder and similarly prefixed blobs
        "chunks": make_blob_ds("ds-chunks", blob_conn, args.container, f"{base_prefix}/chunks"),
        "symbols": make_blob_ds("ds-symbols", blob_conn, args.container, f"{base_prefix}/symbols"),
        "xrefs":  make_blob_ds("ds-xrefs",  blob_conn, args.container, f"{base_prefix}/xrefs"),
        "calls":  make_blob_ds("ds-calls",  blob_conn, args.container, f"{base_prefix}/calls"),
        "files":  make_blob_ds("ds-files",  blob_conn, args.container, f"{base_prefix}/files"),
        # use folder prefixes so any future split files are picked up
        "paragraphs": make_blob_ds("ds-paragraphs", blob_conn, args.container, f"{base_prefix}/paragraphs"),
        "facts": make_blob_ds("ds-facts", blob_conn, args.container, f"{base_prefix}/procedure_facts"),
        "flow_edges": make_blob_ds("ds-flow-edges", blob_conn, args.container, f"{base_prefix}/flow_edges"),
        "copybooks": make_blob_ds("ds-copybooks", blob_conn, args.container, f"{base_prefix}/copybooks"),
    }

    idx_defs = {
        "chunks": index_code_chunks(),
        "symbols": index_symbols(),
        "xrefs": index_xrefs(),
        "calls": index_calls(),
        "files": index_files(),
        "paragraphs": index_paragraphs(),
        "facts": index_facts(),
    # legacy flow_edges index removed; use v2 only
        "copybooks": index_copybooks(),
    }

    # Optional enriched indexes (created when env flag FLOW_V2=1 or ALWAYS_CREATE_V2 set)
    if os.environ.get("FLOW_V2") == "1" or os.environ.get("ALWAYS_CREATE_V2") == "1":
        idx_defs["flow_edges_v2"] = index_flow_edges_v2()
        idx_defs["routine_aliases"] = index_routine_aliases()
        # create summaries index if summarization pipeline is enabled flag
        if os.environ.get("SUMMARIES") == "1":
            idx_defs["summaries"] = index_summaries()


    # Indexers
    ix = {
        "chunks": make_indexer("idx-chunks","ds-chunks","code-chunks",["chunk_id","file_id","path","program_id","scope","name","start_line","end_line","text"]),
        "symbols": make_indexer("idx-symbols","ds-symbols","cobol-symbols",["item_id","file_id","path","program_id","name","qualified_name","section","level","pic","usage","occurs_low","occurs_high","depends_on","redefines","renames","value","start_line","end_line"]),
        "xrefs": make_indexer("idx-xrefs","ds-xrefs","cobol-xrefs",["xref_id","file_id","path","program_id","qualified_name","simple_name","kind","direction","line","start_col","end_col","snippet"]),
        "calls": make_indexer("idx-calls","ds-calls","cobol-calls",["call_id","file_id","caller_para","callee_program","callee_data_name","is_dynamic","line","snippet"]),
        "files": make_indexer("idx-files","ds-files","cobol-files",["file_id","path","name","program_id","lines","format","procedure_using","copybook"]),
        "paragraphs": make_indexer("idx-paragraphs","ds-paragraphs","cobol-paragraphs",["para_id","file_id","name","kind","start_line","end_line"]),
        "facts": make_indexer("idx-facts","ds-facts","cobol-facts",["fact_id","file_id","para","line","kind","snippet","callee","callee_data_name","is_dynamic","using_raw","target","source_raw","expr_raw"]),
        "flow_edges": make_indexer("idx-flow-edges","ds-flow-edges","cobol-flow-edges",["edge_id","file_id","caller_para","target_para","line","kind"]),
        "copybooks": make_indexer("idx-copybooks","ds-copybooks","cobol-copybooks",["copybook_id","file_id","parent_path","copybook_name","line","replacing_clause"]),
    }

    if "flow_edges_v2" in idx_defs:
        # v2 edges and alias index are pushed via direct upload scripts (not blob indexers),
        # so we do NOT create indexers here; just create the indexes above.
        pass

    for v in ds.values(): upsert_data_source(ep,key,v)
    for v in idx_defs.values(): upsert_index(ep,key,v)
    for v in ix.values(): upsert_indexer(ep,key,v)

    if args.auto:
        # Reset indexers to force full re-crawl with sanitized IDs
        for v in ix.values():
            try:
                sreq(ep,key,"POST",_path("indexers",v["name"])+"/reset")
            except Exception as e:
                print(f"⚠️ reset {v['name']} failed: {e}")
        for v in ix.values():
            sreq(ep,key,"POST",_path("indexers",v["name"])+"/run")
        time.sleep(5)
        for v in ix.values():
            st = sreq(ep,key,"GET",_path("indexers",v["name"])+"/status")
            last = (st or {}).get("lastResult",{})
            print(f"=== {v['name']} ===")
            print(" status:",last.get("status"))
            print(" itemsProcessed:",last.get("itemsProcessed"))
            print(" itemsFailed:",last.get("itemsFailed"))
            if last.get("errorMessage"): print(" error:",last["errorMessage"])

if __name__=="__main__":
    main()

