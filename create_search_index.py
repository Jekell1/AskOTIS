#!/usr/bin/env python3
"""
Create Azure AI Search index for COBOL code
"""
import json
import requests
from pathlib import Path

# Load configuration
config_path = Path(__file__).parent / "local.settings.json"
with open(config_path, 'r') as f:
    config = json.load(f)

values = config["Values"]
search_endpoint = values["SEARCH_ENDPOINT"]
search_key = values["SEARCH_KEY"]

print(f"Creating COBOL index on search service: {search_endpoint}")

# Headers
headers = {
    "api-key": search_key,
    "Content-Type": "application/json"
}

# API endpoint
url = f"{search_endpoint}/indexes/cobol-index?api-version=2024-07-01"

# Index schema
index_schema = {
    "name": "cobol-index",
    "fields": [
        { "name": "id", "type": "Edm.String", "key": True, "searchable": False },
        { "name": "repo_path", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": False, "sortable": True },
        { "name": "line", "type": "Edm.Int32", "filterable": True, "sortable": True },
        { "name": "code", "type": "Edm.String", "searchable": True, "analyzer": "standard.lucene" },
        { "name": "symbol_name", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": False },
        { "name": "symbol_kind", "type": "Edm.String", "searchable": False, "filterable": True, "facetable": True },
        { "name": "calls", "type": "Collection(Edm.String)", "searchable": True, "filterable": True },
        { "name": "embeddings", "type": "Collection(Edm.Single)", "searchable": True, "dimensions": 1536, "vectorSearchProfile": "openai" }
    ],
    "vectorSearch": {
        "algorithms": [{ "name": "hnsw", "kind": "hnsw" }],
        "profiles": [{ "name": "openai", "algorithm": "hnsw" }]
    }
}

print(f"Making PUT request to: {url}")
print(f"Index schema: {json.dumps(index_schema, indent=2)}")

try:
    response = requests.put(url, headers=headers, json=index_schema)
    
    print(f"\nResponse Status: {response.status_code}")
    print(f"Response Headers: {dict(response.headers)}")
    
    if response.content:
        try:
            response_json = response.json()
            print(f"Response Body: {json.dumps(response_json, indent=2)}")
        except json.JSONDecodeError:
            print(f"Response Body (text): {response.text}")
    
    if response.status_code in [200, 201]:
        print("\n✅ Index created successfully!")
        print("The index is ready to accept COBOL code documents with the following fields:")
        print("  - id: Unique identifier for each line")
        print("  - repo_path: File path in repository")
        print("  - line: Line number")
        print("  - code: COBOL code content (searchable)")
        print("  - symbol_name: Function/variable names (optional)")
        print("  - symbol_kind: Type of symbol (optional)")
        print("  - calls: Function calls (optional)")
        print("  - embeddings: Vector embeddings for semantic search (optional)")
    else:
        print(f"\n❌ Failed to create index. Status: {response.status_code}")
        
except requests.RequestException as e:
    print(f"❌ Request failed: {e}")
except Exception as e:
    print(f"❌ Error: {e}")
