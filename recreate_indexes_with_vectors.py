"""Recreate empty COBOL indexes with vector capabilities.

Usage:
  python recreate_indexes_with_vectors.py [--force]

By default only recreates indexes whose documentCount == 0. Use --force to drop even if non-empty (DANGEROUS).
Relies on SEARCH_ENDPOINT / SEARCH_KEY (or AZURE_SEARCH_* env) or local.settings.json.
"""
import os, json, argparse, requests, sys, pathlib

API_VERSION = "2024-07-01"
VECTOR_DIM = 1536
INDEX_SPECS = {
    "cobol-symbols": {
        "description": "COBOL data item & symbol metadata with semantic name vector",
        "fields": [
            {"name": "item_id", "type": "Edm.String", "key": True, "filterable": True, "sortable": False},
            {"name": "file_id", "type": "Edm.String", "filterable": True},
            {"name": "path", "type": "Edm.String", "searchable": True, "retrievable": True},
            {"name": "program_id", "type": "Edm.String", "filterable": True},
            {"name": "name", "type": "Edm.String", "searchable": True, "retrievable": True},
            {"name": "qualified_name", "type": "Edm.String", "searchable": True},
            {"name": "section", "type": "Edm.String", "filterable": True},
            {"name": "level", "type": "Edm.Int32", "filterable": True},
            {"name": "pic", "type": "Edm.String", "searchable": True},
            {"name": "usage", "type": "Edm.String", "filterable": True},
            {"name": "start_line", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "end_line", "type": "Edm.Int32", "filterable": True},
            {"name": "name_vector", "type": "Collection(Single)", "searchable": False, "vectorSearchDimensions": VECTOR_DIM, "vectorSearchProfile": "default"},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True}
        ]
    },
    "cobol-paragraphs": {
        "description": "COBOL paragraph boundaries and semantic vectors",
        "fields": [
            {"name": "para_id", "type": "Edm.String", "key": True},
            {"name": "file_id", "type": "Edm.String", "filterable": True},
            {"name": "name", "type": "Edm.String", "searchable": True},
            {"name": "kind", "type": "Edm.String", "filterable": True},
            {"name": "start_line", "type": "Edm.Int32", "filterable": True},
            {"name": "end_line", "type": "Edm.Int32", "filterable": True},
            {"name": "name_vector", "type": "Collection(Single)", "vectorSearchDimensions": VECTOR_DIM, "vectorSearchProfile": "default"},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True}
        ]
    },
    "cobol-xrefs": {
        "description": "Cross references definitions <-> usages with snippet semantic vector",
        "fields": [
            {"name": "xref_id", "type": "Edm.String", "key": True},
            {"name": "file_id", "type": "Edm.String", "filterable": True},
            {"name": "path", "type": "Edm.String", "searchable": True},
            {"name": "program_id", "type": "Edm.String", "filterable": True},
            {"name": "qualified_name", "type": "Edm.String", "searchable": True},
            {"name": "simple_name", "type": "Edm.String", "searchable": True},
            {"name": "kind", "type": "Edm.String", "filterable": True},
            {"name": "direction", "type": "Edm.String", "filterable": True},
            {"name": "line", "type": "Edm.Int32", "filterable": True},
            {"name": "snippet", "type": "Edm.String", "searchable": True},
            {"name": "snippet_vector", "type": "Collection(Single)", "vectorSearchDimensions": VECTOR_DIM, "vectorSearchProfile": "default"},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True}
        ]
    },
    "cobol-calls": {
        "description": "Call relationships extracted from CALL statements",
        "fields": [
            {"name": "call_id", "type": "Edm.String", "key": True},
            {"name": "file_id", "type": "Edm.String", "filterable": True},
            {"name": "caller_para", "type": "Edm.String", "filterable": True},
            {"name": "callee_program", "type": "Edm.String", "searchable": True},
            {"name": "callee_data_name", "type": "Edm.String", "searchable": True},
            {"name": "is_dynamic", "type": "Edm.Boolean", "filterable": True},
            {"name": "line", "type": "Edm.Int32", "filterable": True},
            {"name": "snippet", "type": "Edm.String", "searchable": True},
            {"name": "snippet_vector", "type": "Collection(Single)", "vectorSearchDimensions": VECTOR_DIM, "vectorSearchProfile": "default"},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True}
        ]
    },
    "code-chunks": {
        "description": "Code chunks for semantic retrieval (RAG)",
        "fields": [
            {"name": "chunk_id", "type": "Edm.String", "key": True},
            {"name": "file_id", "type": "Edm.String", "filterable": True},
            {"name": "path", "type": "Edm.String", "searchable": True},
            {"name": "program_id", "type": "Edm.String", "filterable": True},
            {"name": "scope", "type": "Edm.String", "filterable": True},
            {"name": "name", "type": "Edm.String", "searchable": True},
            {"name": "start_line", "type": "Edm.Int32", "filterable": True},
            {"name": "end_line", "type": "Edm.Int32", "filterable": True},
            {"name": "text", "type": "Edm.String", "searchable": True},
            {"name": "text_vector", "type": "Collection(Single)", "vectorSearchDimensions": VECTOR_DIM, "vectorSearchProfile": "default"},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True}
        ]
    }
}

VECTOR_SEARCH_BLOCK = {
    "vectorSearch": {
        "algorithms": [
            {"name": "hnsw", "kind": "hnsw", "parameters": {"m": 4, "efConstruction": 400}}
        ],
        "profiles": [
            {"name": "default", "algorithm": "hnsw"}
        ]
    }
}


def load_settings():
    try:
        data = json.loads(pathlib.Path('local.settings.json').read_text(encoding='utf-8'))
        vals = data.get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def get_stats(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}/stats?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code == 404:
        return None
    return r.json() if r.status_code == 200 else None


def delete_index(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r = requests.delete(url, headers={"api-key": key})
    if r.status_code not in (204, 404):
        raise RuntimeError(f"Delete {name} failed: {r.status_code} {r.text[:200]}")


def create_index(endpoint: str, key: str, name: str, spec: dict):
    body = {
        "name": name,
        "fields": spec['fields'],
        **VECTOR_SEARCH_BLOCK,
        "description": spec.get('description')
    }
    url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r = requests.put(url, headers={"api-key": key, "Content-Type": "application/json"}, json=body)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Create {name} failed: {r.status_code} {r.text[:300]}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--force', action='store_true', help='Recreate even if docs > 0 (DANGEROUS)')
    parser.add_argument('--only', help='Comma list subset of indexes')
    args = parser.parse_args()

    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not endpoint or not key:
        print('Missing SEARCH_ENDPOINT / SEARCH_KEY')
        return 1

    targets = list(INDEX_SPECS.keys())
    if args.only:
        subset = [x.strip() for x in args.only.split(',') if x.strip()]
        targets = [t for t in targets if t in subset]

    for name in targets:
        print(f"Processing {name}...")
        stats = get_stats(endpoint, key, name)
        recreate = False
        if stats is None:
            print(f"  Index not found -> will create")
            recreate = True
        else:
            doc_count = stats.get('documentCount')
            if doc_count == 0 or args.force:
                print(f"  Existing empty index (docs={doc_count}) -> recreate")
                recreate = True
            else:
                print(f"  Skipping (documents present: {doc_count})")
        if recreate:
            delete_index(endpoint, key, name)
            try:
                create_index(endpoint, key, name, INDEX_SPECS[name])
                print(f"  Created {name} ✅")
            except Exception as e:
                print(f"  ERROR creating {name}: {e}")
    return 0

if __name__ == '__main__':
    raise SystemExit(main())
