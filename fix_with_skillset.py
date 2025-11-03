#!/usr/bin/env python3
"""
Advanced fix for COBOL indexer - use skillset to split files into chunks
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def fix_with_skillset():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== ADVANCED COBOL INDEXER FIX ===")
    print("🎯 Creating skillset to properly chunk COBOL files")
    
    # Step 1: Create skillset for text splitting
    skillset_name = "cobol-text-splitter"
    skillset_url = f'{endpoint}/skillsets/{skillset_name}?api-version=2023-07-01-preview'
    
    skillset_config = {
        "name": skillset_name,
        "description": "Split COBOL files into lines for proper indexing",
        "skills": [
            {
                "@odata.type": "#Microsoft.Skills.Text.SplitSkill",
                "name": "splitlines",
                "description": "Split COBOL content into individual lines",
                "context": "/document",
                "defaultLanguageCode": "en",
                "textSplitMode": "lines",
                "maximumPageLength": 200,  # Characters per line
                "inputs": [
                    {
                        "name": "text",
                        "source": "/document/content"
                    }
                ],
                "outputs": [
                    {
                        "name": "textItems",
                        "targetName": "lines"
                    }
                ]
            }
        ]
    }
    
    print("🔧 Creating text splitting skillset...")
    
    try:
        skillset_response = requests.put(skillset_url, headers=headers, json=skillset_config)
        
        if skillset_response.status_code in [200, 201]:
            print("✅ Skillset created successfully")
        else:
            print(f"❌ Failed to create skillset: {skillset_response.status_code}")
            print(skillset_response.text)
            return
            
    except Exception as e:
        print(f"❌ Error creating skillset: {e}")
        return
    
    # Step 2: Update indexer to use the skillset
    print("🔧 Updating indexer to use skillset...")
    
    indexer_name = "cobol-indexer"
    indexer_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
    
    try:
        # Get current config
        indexer_response = requests.get(indexer_url, headers=headers)
        
        if indexer_response.status_code == 200:
            indexer_config = indexer_response.json()
            
            # Remove system fields
            for key in ['@odata.context', '@odata.etag']:
                indexer_config.pop(key, None)
            
            # Add skillset
            indexer_config['skillsetName'] = skillset_name
            
            # Update field mappings
            indexer_config['fieldMappings'] = [
                {
                    "sourceFieldName": "metadata_storage_path",
                    "targetFieldName": "id",
                    "mappingFunction": {
                        "name": "base64Encode"
                    }
                },
                {
                    "sourceFieldName": "metadata_storage_path",
                    "targetFieldName": "repo_path"
                }
            ]
            
            # Add output field mappings for skillset results
            indexer_config['outputFieldMappings'] = [
                {
                    "sourceFieldName": "/document/lines/*",
                    "targetFieldName": "code"
                }
            ]
            
            # Update indexer
            indexer_update_response = requests.put(indexer_url, headers=headers, json=indexer_config)
            
            if indexer_update_response.status_code in [200, 201, 204]:
                print("✅ Indexer updated with skillset")
            else:
                print(f"❌ Failed to update indexer: {indexer_update_response.status_code}")
                print(indexer_update_response.text)
                return
                
        else:
            print(f"❌ Could not get indexer config: {indexer_response.status_code}")
            return
            
    except Exception as e:
        print(f"❌ Error updating indexer: {e}")
        return
    
    # Step 3: Reset and run
    print("🔄 Resetting and running indexer...")
    
    try:
        # Reset
        reset_url = f'{endpoint}/indexers/{indexer_name}/reset?api-version=2023-07-01-preview'
        reset_response = requests.post(reset_url, headers=headers)
        
        if reset_response.status_code == 204:
            print("✅ Indexer reset successful")
            
            # Run
            run_url = f'{endpoint}/indexers/{indexer_name}/run?api-version=2023-07-01-preview'
            run_response = requests.post(run_url, headers=headers)
            
            if run_response.status_code == 202:
                print("✅ Indexer started with line-splitting skillset!")
                print("\n📋 This should now:")
                print("   • Split each COBOL file into individual lines")
                print("   • Index each line as a separate document")
                print("   • Allow searching within file content")
                print("\n📊 Monitor progress with: python quick_status_check.py")
                
            else:
                print(f"❌ Failed to run indexer: {run_response.status_code}")
        else:
            print(f"❌ Failed to reset indexer: {reset_response.status_code}")
            
    except Exception as e:
        print(f"❌ Error running indexer: {e}")

if __name__ == "__main__":
    fix_with_skillset()
