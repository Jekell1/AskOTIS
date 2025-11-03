"""Create or optionally overwrite the Azure AI Search index: new_cobol_program_meta

Requirements implemented:
- Uses only stdlib (urllib) for REST calls (no extra deps)
- Reads env vars AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY (optionally AZURE_SEARCH_API_VERSION)
- Schema fields:
    program_id (key, filterable, facetable, searchable)
    program_name (searchable, filterable)
    file_paths_json (non-searchable)
    program_summary (searchable)
    program_summary_vector (Collection(Edm.Single), dimensions=3072, vectorSearchProfile="vprofile")
    has_vector (Boolean)
    updated_at (String)
- vectorSearch config with profile "vprofile" using HNSW
- semantic config "semCfg" prioritizing program_summary
- CLI flag --overwrite to delete before recreate
- Prints: "created/updated: new_cobol_program_meta" on success
- Exits non‑zero on HTTP errors or missing configuration

Usage:
    python search/indexes/create_program_meta_index.py [--overwrite]

Environment:
    AZURE_SEARCH_ENDPOINT=https://<service>.search.windows.net
    AZURE_SEARCH_KEY=<admin-or-query-key-with-manage-rights>
    (optional) AZURE_SEARCH_API_VERSION=2025-08-01-preview (default used if unset)
"""
from __future__ import annotations
import os, sys, json, argparse, urllib.request, urllib.error
from typing import Any, Dict

INDEX_NAME = "new_cobol_program_meta"
DEFAULT_API_VERSION = os.getenv("AZURE_SEARCH_API_VERSION", "2025-08-01-preview")


def env_or_fail(name: str) -> str:
    val = os.getenv(name)
    if not val:
        print(f"Missing required environment variable: {name}", file=sys.stderr)
        sys.exit(1)
    return val


def build_schema() -> Dict[str, Any]:
    return {
        "name": INDEX_NAME,
        "fields": [
            {"name": "program_id", "type": "Edm.String", "key": True, "searchable": True, "filterable": True, "facetable": True},
            {"name": "program_name", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": False},
            {"name": "file_paths_json", "type": "Edm.String", "searchable": False, "filterable": False, "facetable": False, "sortable": False},
            {"name": "program_summary", "type": "Edm.String", "searchable": True, "filterable": False, "facetable": False, "sortable": False},
            {"name": "program_summary_vector", "type": "Collection(Edm.Single)", "searchable": True, "dimensions": 3072, "vectorSearchProfile": "vprofile"},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True, "facetable": True},
            {"name": "updated_at", "type": "Edm.String", "searchable": False, "filterable": True, "facetable": False, "sortable": True},
        ],
        "vectorSearch": {
            "algorithms": [
                {"name": "hnsw-alg", "kind": "hnsw"}
            ],
            "profiles": [
                {"name": "vprofile", "algorithm": "hnsw-alg"}
            ]
        },
        "semantic": {
            "configurations": [
                {
                    "name": "semCfg",
                    "prioritizedFields": {
                        "titleField": None,
                        "prioritizedContentFields": [{"fieldName": "program_summary"}],
                        "prioritizedKeywordsFields": []
                    }
                }
            ]
        }
    }


def http_request(method: str, url: str, key: str, data: Any | None = None, expected: set[int] | None = None) -> tuple[int, str]:
    headers = {
        "api-key": key,
        "Content-Type": "application/json"
    }
    body_bytes = None
    if data is not None:
        body_bytes = json.dumps(data).encode("utf-8")
    req = urllib.request.Request(url, data=body_bytes, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req) as resp:  # nosec B310 (intentional external call)
            status = resp.getcode()
            text = resp.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:  # pragma: no cover - network path
        status = e.code
        text = e.read().decode("utf-8", errors="replace") if e.fp else e.reason
    except urllib.error.URLError as e:  # pragma: no cover
        print(f"Network error contacting Azure Search: {e}", file=sys.stderr)
        sys.exit(1)
    if expected and status not in expected:
        print(f"HTTP {method} {url} failed: {status} {text[:500]}", file=sys.stderr)
        sys.exit(1)
    return status, text


def index_exists(endpoint: str, key: str) -> bool:
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={DEFAULT_API_VERSION}"
    status, _ = http_request("GET", url, key, None, expected=None)
    return status == 200


def delete_index(endpoint: str, key: str):
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={DEFAULT_API_VERSION}"
    status, _ = http_request("DELETE", url, key, None, expected={200, 202, 204, 404})
    if status == 404:
        return


def create_index(endpoint: str, key: str, schema: Dict[str, Any]):
    url = f"{endpoint}/indexes?api-version={DEFAULT_API_VERSION}"
    http_request("POST", url, key, schema, expected={200, 201})


def main():
    parser = argparse.ArgumentParser(description=f"Create or overwrite {INDEX_NAME} index")
    parser.add_argument("--overwrite", action="store_true", help="Delete and recreate index if it exists")
    args = parser.parse_args()

    endpoint = env_or_fail("AZURE_SEARCH_ENDPOINT").rstrip("/")
    key = env_or_fail("AZURE_SEARCH_KEY")

    exists = index_exists(endpoint, key)
    if exists and not args.overwrite:
        # Still print required message (treat as up-to-date)
        print(f"created/updated: {INDEX_NAME}")
        return
    if exists and args.overwrite:
        delete_index(endpoint, key)
    schema = build_schema()
    create_index(endpoint, key, schema)
    print(f"created/updated: {INDEX_NAME}")


if __name__ == "__main__":  # pragma: no cover
    main()
