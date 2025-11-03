#!/usr/bin/env python3
"""
Final fix - use page splitting with small chunks to capture more content
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def fix_final():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== FINAL COBOL INDEXER FIX ===")
    
    # Create skillset with page-based splitting (small pages)
    skillset_name = "cobol-page-splitter"
    skillset_url = f'{endpoint}/skillsets/{skillset_name}?api-version=2023-07-01-preview'
    
    skillset_config = {
        "name": skillset_name,
        "description": "Split COBOL files into small pages",
        "skills": [
            {
                "@odata.type": "#Microsoft.Skills.Text.SplitSkill",
                "name": "splitpages",
                "description": "Split COBOL content into small chunks",
                "context": "/document",
                "defaultLanguageCode": "en",
                "textSplitMode": "pages",
                "maximumPageLength": 300,  # Minimum allowed size
                "inputs": [
                    {
                        "name": "text",
                        "source": "/document/content"
                    }
                ],
                "outputs": [
                    {
                        "name": "textItems",
                        "targetName": "chunks"
                    }
                ]
            }
        ]
    }
    
    print("🔧 Creating page-splitting skillset...")
    
    try:
        skillset_response = requests.put(skillset_url, headers=headers, json=skillset_config)
        
        if skillset_response.status_code in [200, 201]:
            print("✅ Skillset created")
            
            # Update indexer
            indexer_name = "cobol-indexer"
            indexer_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
            
            indexer_response = requests.get(indexer_url, headers=headers)
            
            if indexer_response.status_code == 200:
                indexer_config = indexer_response.json()
                
                # Clean config
                for key in ['@odata.context', '@odata.etag']:
                    indexer_config.pop(key, None)
                
                # Add skillset
                indexer_config['skillsetName'] = skillset_name
                
                # Update mappings
                indexer_config['fieldMappings'] = [
                    {
                        "sourceFieldName": "metadata_storage_path",
                        "targetFieldName": "id",
                        "mappingFunction": {"name": "base64Encode"}
                    },
                    {
                        "sourceFieldName": "metadata_storage_path",
                        "targetFieldName": "repo_path"
                    }
                ]
                
                indexer_config['outputFieldMappings'] = [
                    {
                        "sourceFieldName": "/document/chunks/*",
                        "targetFieldName": "code"
                    }
                ]
                
                # Update indexer
                indexer_update = requests.put(indexer_url, headers=headers, json=indexer_config)
                
                if indexer_update.status_code in [200, 201, 204]:
                    print("✅ Indexer updated")
                    
                    # Reset and run
                    reset_url = f'{endpoint}/indexers/{indexer_name}/reset?api-version=2023-07-01-preview'
                    requests.post(reset_url, headers=headers)
                    
                    run_url = f'{endpoint}/indexers/{indexer_name}/run?api-version=2023-07-01-preview'
                    run_response = requests.post(run_url, headers=headers)
                    
                    if run_response.status_code == 202:
                        print("✅ Indexer running with chunking!")
                        print("📊 Check progress: python quick_status_check.py")
                    else:
                        print(f"❌ Run failed: {run_response.status_code}")
                else:
                    print(f"❌ Update failed: {indexer_update.status_code}")
                    print(indexer_update.text)
            else:
                print(f"❌ Get indexer failed: {indexer_response.status_code}")
                
        else:
            print(f"❌ Skillset failed: {skillset_response.status_code}")
            print(skillset_response.text)
            
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    fix_final()
