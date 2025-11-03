"""
Create and populate new_cobol_scn_analysis index from scn_files_analysis.json

This index provides searchable, AI-analyzed screen descriptions with:
- Actual _SCN.CPY filenames (not hash IDs)
- OpenAI-generated descriptions
- Menu/screen information
- Calling programs
- Vector embeddings for semantic search
"""
import json
import os
import requests
from typing import Dict, Any, List

API_VERSION = '2023-11-01'
INDEX_NAME = 'new_cobol_scn_analysis'

def load_config():
    """Load Azure Search credentials from local.settings.json"""
    with open('local.settings.json', 'r') as f:
        config = json.load(f).get('Values', {})
    return config

def create_index(endpoint: str, key: str) -> bool:
    """Create the new_cobol_scn_analysis index with schema"""
    
    index_schema = {
        "name": INDEX_NAME,
        "fields": [
            {"name": "id", "type": "Edm.String", "key": True, "filterable": True},
            {"name": "filename", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": True},
            {"name": "scn_name", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": True},
            {"name": "path", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "description", "type": "Edm.String", "searchable": True},
            {"name": "menu_screen_info", "type": "Edm.String", "searchable": True},
            {"name": "purpose", "type": "Edm.String", "searchable": True},
            {"name": "calling_programs", "type": "Collection(Edm.String)", "searchable": True, "filterable": True},
            {"name": "caller_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "has_menu_options", "type": "Edm.Boolean", "filterable": True},
            {"name": "doc_type", "type": "Edm.String", "filterable": True},
            {"name": "generated_at", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            # Vector field for semantic search
            {
                "name": "description_vector",
                "type": "Collection(Edm.Single)",
                "searchable": True,
                "vectorSearchDimensions": 1536,
                "vectorSearchProfileName": "default-vector-profile"
            }
        ],
        "vectorSearch": {
            "profiles": [
                {
                    "name": "default-vector-profile",
                    "algorithm": "default-algorithm"
                }
            ],
            "algorithms": [
                {
                    "name": "default-algorithm",
                    "kind": "hnsw"
                }
            ]
        },
        "semantic": {
            "configurations": [
                {
                    "name": "default-semantic-config",
                    "prioritizedFields": {
                        "titleField": {"fieldName": "filename"},
                        "contentFields": [
                            {"fieldName": "description"},
                            {"fieldName": "menu_screen_info"},
                            {"fieldName": "purpose"}
                        ],
                        "keywordsFields": [
                            {"fieldName": "scn_name"},
                            {"fieldName": "calling_programs"}
                        ]
                    }
                }
            ]
        }
    }
    
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    # Check if index exists
    check_response = requests.get(url, headers=headers)
    if check_response.status_code == 200:
        print(f"✅ Index '{INDEX_NAME}' already exists")
        return True
    
    # Create index
    response = requests.put(url, headers=headers, json=index_schema)
    
    if response.status_code in (200, 201):
        print(f"✅ Created index '{INDEX_NAME}'")
        return True
    else:
        print(f"❌ Failed to create index: {response.status_code}")
        print(response.text)
        return False

def transform_document(scn_doc: Dict[str, Any]) -> Dict[str, Any]:
    """Transform SCN analysis document to index format"""
    
    filename = scn_doc['filename']
    scn_name = filename.replace('_SCN.CPY', '').replace('.CPY', '')
    
    # Create unique ID
    doc_id = filename.replace('.', '_').replace('/', '_')
    
    # Check if has menu options
    menu_info = scn_doc.get('menu_screen_info', '')
    has_menu_options = 'Menu Options:' in menu_info or any(
        f'{i}.' in menu_info for i in range(1, 21)
    )
    
    return {
        'id': doc_id,
        'filename': filename,
        'scn_name': scn_name,
        'path': scn_doc['path'],
        'description': scn_doc['description'],
        'menu_screen_info': menu_info,
        'purpose': scn_doc.get('purpose', ''),
        'calling_programs': scn_doc['calling_programs'],
        'caller_count': scn_doc['caller_count'],
        'has_menu_options': has_menu_options,
        'doc_type': 'scn_analysis',
        'generated_at': '2025-10-30T00:00:00Z',
        # Note: description_vector would be added by separate embedding script
    }

def upload_documents(endpoint: str, key: str, documents: List[Dict[str, Any]], batch_size: int = 100):
    """Upload documents to index in batches"""
    
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    total = len(documents)
    for i in range(0, total, batch_size):
        batch = documents[i:i+batch_size]
        
        payload = {
            'value': [
                {'@search.action': 'mergeOrUpload', **doc}
                for doc in batch
            ]
        }
        
        response = requests.post(url, headers=headers, json=payload)
        
        if response.status_code in (200, 201):
            end_idx = min(i + batch_size, total)
            print(f"✅ Uploaded {i+1}-{end_idx} of {total}")
        else:
            print(f"❌ Failed batch {i//batch_size + 1}: {response.status_code}")
            print(response.text[:500])

def main():
    print("="*80)
    print("CREATE AND POPULATE new_cobol_scn_analysis INDEX")
    print("="*80)
    
    # Load config
    config = load_config()
    endpoint = config['AZURE_SEARCH_ENDPOINT'].rstrip('/')
    key = config['AZURE_SEARCH_KEY']
    
    # Create index
    print("\n1. Creating index...")
    if not create_index(endpoint, key):
        return
    
    # Load SCN analysis data
    print("\n2. Loading scn_files_analysis.json...")
    with open('scn_files_analysis.json', 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    scn_files = data.get('scn_files', [])
    print(f"   Found {len(scn_files)} SCN files")
    
    # Transform documents
    print("\n3. Transforming documents...")
    documents = [transform_document(doc) for doc in scn_files]
    print(f"   Transformed {len(documents)} documents")
    
    # Upload to index
    print("\n4. Uploading to Azure Search...")
    upload_documents(endpoint, key, documents)
    
    # Summary
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    print(f"Index: {INDEX_NAME}")
    print(f"Documents: {len(documents)}")
    print(f"Searchable fields: filename, scn_name, description, menu_screen_info, purpose")
    print(f"Filterable: calling_programs, caller_count, has_menu_options")
    print("\nNext steps:")
    print("  1. Add vector embeddings with backfill_scn_embeddings.py")
    print("  2. Test searches: python test_scn_search.py")
    print("  3. Integrate with chatbot RAG")
    print("="*80)

if __name__ == "__main__":
    main()
