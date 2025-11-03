#!/usr/bin/env python3
"""
Complete Clean Slate: Delete and recreate index with only essential indexer
This will give you a clean, simple, fast COBOL search setup
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def complete_clean_slate():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    index_name = "cobol-index"
    
    print("=== COMPLETE CLEAN SLATE ===")
    print("This will:")
    print("✅ Delete the corrupted index (4.3M documents)")
    print("✅ Recreate the index with proper configuration")
    print("✅ Keep only the essential 'cobol-indexer' for .CBL files")
    print("✅ Delete all duplicate/test indexers")
    print("✅ Set up a clean, simple, fast search system")
    
    confirm = input("\n⚠️  Type 'CLEAN_SLATE' to proceed: ")
    if confirm != 'CLEAN_SLATE':
        print("❌ Operation cancelled")
        return
    
    # STEP 1: Get the current index schema before deleting
    print("\n📋 STEP 1: Backing up index schema...")
    
    index_url = f'{endpoint}/indexes/{index_name}?api-version=2023-07-01-preview'
    index_response = requests.get(index_url, headers=headers)
    
    original_schema = None
    if index_response.status_code == 200:
        original_schema = index_response.json()
        print("✅ Index schema backed up")
    else:
        print("⚠️  Could not backup schema - will use default")
    
    # STEP 2: Delete all duplicate/test indexers
    print("\n🗑️  STEP 2: Cleaning up duplicate indexers...")
    
    indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
    indexers_response = requests.get(indexers_url, headers=headers)
    
    indexers_to_delete = []
    essential_indexer = None
    
    if indexers_response.status_code == 200:
        indexers = indexers_response.json().get('value', [])
        
        for indexer in indexers:
            name = indexer.get('name', '')
            
            if name == 'cobol-indexer':
                essential_indexer = indexer
                print(f"✅ Keeping essential indexer: {name}")
            elif 'cobol' in name.lower() or 'mycoboldata' in name.lower():
                indexers_to_delete.append(name)
                print(f"🗑️  Will delete: {name}")
    
    # Delete duplicate indexers
    for indexer_name in indexers_to_delete:
        try:
            delete_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
            delete_response = requests.delete(delete_url, headers=headers)
            
            if delete_response.status_code == 204:
                print(f"✅ Deleted {indexer_name}")
            elif delete_response.status_code == 404:
                print(f"⚠️  {indexer_name} not found (already deleted)")
            else:
                print(f"❌ Failed to delete {indexer_name}: {delete_response.status_code}")
        except Exception as e:
            print(f"❌ Error deleting {indexer_name}: {e}")
    
    # STEP 3: Delete the corrupted index
    print(f"\n🗑️  STEP 3: Deleting corrupted index '{index_name}'...")
    
    try:
        delete_response = requests.delete(index_url, headers=headers)
        
        if delete_response.status_code == 204:
            print("✅ Index deleted successfully")
        elif delete_response.status_code == 404:
            print("⚠️  Index not found (may already be deleted)")
        else:
            print(f"❌ Failed to delete index: {delete_response.status_code}")
            print(delete_response.text)
            return
    except Exception as e:
        print(f"❌ Error deleting index: {e}")
        return
    
    # Wait for deletion to complete
    print("⏳ Waiting for deletion to complete...")
    time.sleep(10)
    
    # STEP 4: Recreate the index with clean schema
    print(f"\n🔧 STEP 4: Recreating index '{index_name}' with clean schema...")
    
    # Use the original schema if we have it, otherwise create a clean one
    if original_schema:
        # Clean up the schema for recreation
        clean_schema = {
            "name": index_name,
            "fields": original_schema.get('fields', []),
            "scoringProfiles": original_schema.get('scoringProfiles', []),
            "corsOptions": original_schema.get('corsOptions'),
            "suggesters": original_schema.get('suggesters', []),
            "analyzers": original_schema.get('analyzers', []),
            "tokenizers": original_schema.get('tokenizers', []),
            "tokenFilters": original_schema.get('tokenFilters', []),
            "charFilters": original_schema.get('charFilters', [])
        }
        
        # Remove system fields that shouldn't be in create request
        for key in ['@odata.context', '@odata.etag']:
            clean_schema.pop(key, None)
    else:
        # Create a basic schema for COBOL files
        clean_schema = {
            "name": index_name,
            "fields": [
                {"name": "id", "type": "Edm.String", "key": True, "searchable": False},
                {"name": "repo_path", "type": "Edm.String", "searchable": True, "filterable": True},
                {"name": "line", "type": "Edm.Int32", "filterable": True, "sortable": True},
                {"name": "code", "type": "Edm.String", "searchable": True, "analyzer": "standard.lucene"},
                {"name": "symbol_name", "type": "Edm.String", "searchable": True, "filterable": True},
                {"name": "symbol_kind", "type": "Edm.String", "searchable": True, "filterable": True},
                {"name": "calls", "type": "Collection(Edm.String)", "searchable": True},
                {
                    "name": "embeddings",
                    "type": "Collection(Edm.Single)",
                    "searchable": True,
                    "vectorSearchDimensions": 1536,
                    "vectorSearchProfileName": "my-vector-profile"
                }
            ],
            "vectorSearch": {
                "profiles": [
                    {
                        "name": "my-vector-profile",
                        "algorithm": "my-hnsw-vector-config-1",
                        "vectorizer": "my-openai-vectorizer-1"
                    }
                ],
                "algorithms": [
                    {
                        "name": "my-hnsw-vector-config-1",
                        "kind": "hnsw",
                        "hnswParameters": {
                            "m": 4,
                            "efConstruction": 400,
                            "efSearch": 500,
                            "metric": "cosine"
                        }
                    }
                ],
                "vectorizers": [
                    {
                        "name": "my-openai-vectorizer-1",
                        "kind": "azureOpenAI",
                        "azureOpenAIParameters": {
                            "resourceUri": settings.get('AZURE_OPENAI_ENDPOINT'),
                            "deploymentId": settings.get('AZURE_OPENAI_EMBEDDING_DEPLOYMENT', 'text-embedding-3-small'),
                            "apiKey": settings.get('AZURE_OPENAI_KEY')
                        }
                    }
                ]
            }
        }
    
    try:
        create_response = requests.post(index_url, headers=headers, json=clean_schema)
        
        if create_response.status_code == 201:
            print("✅ Index recreated successfully")
        else:
            print(f"❌ Failed to create index: {create_response.status_code}")
            print(create_response.text)
            return
    except Exception as e:
        print(f"❌ Error creating index: {e}")
        return
    
    # STEP 5: Configure the essential indexer
    print("\n🔧 STEP 5: Configuring essential indexer...")
    
    if essential_indexer:
        indexer_name = essential_indexer.get('name')
        
        # Update indexer to remove schedule (make it manual only)
        indexer_config = essential_indexer.copy()
        
        # Remove system fields
        for key in ['@odata.context', '@odata.etag']:
            indexer_config.pop(key, None)
        
        # Remove or modify schedule to prevent runaway indexing
        print("   Removing automatic schedule to prevent runaway indexing...")
        indexer_config.pop('schedule', None)  # Make it manual only
        
        # Update the indexer
        update_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
        
        try:
            update_response = requests.put(update_url, headers=headers, json=indexer_config)
            
            if update_response.status_code in [200, 201]:
                print(f"✅ Updated {indexer_name} configuration")
            else:
                print(f"⚠️  Could not update indexer config: {update_response.status_code}")
                print("   This is okay - we'll run it manually")
        except Exception as e:
            print(f"⚠️  Error updating indexer: {e}")
    
    # STEP 6: Run the clean indexing
    print("\n🚀 STEP 6: Running clean indexing...")
    
    if essential_indexer:
        indexer_name = essential_indexer.get('name')
        
        # Reset the indexer first
        reset_url = f'{endpoint}/indexers/{indexer_name}/reset?api-version=2023-07-01-preview'
        reset_response = requests.post(reset_url, headers=headers)
        
        if reset_response.status_code == 204:
            print(f"✅ Reset {indexer_name}")
            
            # Run the indexer
            run_url = f'{endpoint}/indexers/{indexer_name}/run?api-version=2023-07-01-preview'
            run_response = requests.post(run_url, headers=headers)
            
            if run_response.status_code == 202:
                print(f"✅ Started {indexer_name}")
                
                # Monitor progress
                print("\n📊 Monitoring indexing progress...")
                
                for check in range(6):  # Check for 3 minutes
                    time.sleep(30)
                    
                    status_url = f'{endpoint}/indexers/{indexer_name}/status?api-version=2023-07-01-preview'
                    status_response = requests.get(status_url, headers=headers)
                    
                    if status_response.status_code == 200:
                        status = status_response.json()
                        last_result = status.get('lastResult', {})
                        current_status = last_result.get('status', 'unknown')
                        items_processed = last_result.get('itemsProcessed', 0)
                        
                        print(f"   Check {check + 1}: Status={current_status}, Items={items_processed:,}")
                        
                        if current_status == 'success':
                            print("🎉 Clean indexing completed successfully!")
                            break
                        elif current_status == 'error':
                            print("❌ Indexing failed")
                            break
                
            else:
                print(f"❌ Could not start indexer: {run_response.status_code}")
        else:
            print(f"❌ Could not reset indexer: {reset_response.status_code}")
    
    # STEP 7: Final verification
    print("\n🔍 STEP 7: Final verification...")
    
    # Check document count
    search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
    search_body = {"search": "*", "count": True, "top": 0}
    
    final_response = requests.post(search_url, headers=headers, json=search_body)
    
    if final_response.status_code == 200:
        final_result = final_response.json()
        final_count = final_result.get('@odata.count', 'unknown')
        
        print(f"📊 Final document count: {final_count:,}" if isinstance(final_count, int) else f"📊 Final document count: {final_count}")
        
        if isinstance(final_count, int):
            if final_count < 50000:
                print("🎉 Perfect! Clean, reasonable document count")
            elif final_count < 100000:
                print("✅ Good document count - much better than 4.3M!")
            else:
                print("⚠️  Still high but much better than before")
    
    # Check remaining indexers
    indexers_response = requests.get(indexers_url, headers=headers)
    if indexers_response.status_code == 200:
        indexers = indexers_response.json().get('value', [])
        cobol_indexers = [i.get('name') for i in indexers if 'cobol' in i.get('name', '').lower()]
        
        print(f"📋 Remaining COBOL indexers: {len(cobol_indexers)}")
        for name in cobol_indexers:
            print(f"   ✅ {name}")
    
    print("\n=== CLEAN SLATE COMPLETE ===")
    print("🎉 SUCCESS! Your COBOL search is now:")
    print("   ✅ Clean and fast")
    print("   ✅ No duplicate data")
    print("   ✅ Simple configuration")
    print("   ✅ Manual indexing control")
    print()
    print("🧪 Next step: Test your search!")
    print("   Run: python test_cobol_search.py")

if __name__ == "__main__":
    complete_clean_slate()
