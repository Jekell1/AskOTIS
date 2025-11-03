#!/usr/bin/env python3
"""
Fix the COBOL indexer to parse full file content
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def fix_cobol_indexer():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== FIXING COBOL INDEXER CONFIGURATION ===")
    
    # Step 1: Check current indexer configuration
    indexer_name = "cobol-indexer"
    indexer_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
    
    print("📋 Current indexer configuration:")
    try:
        response = requests.get(indexer_url, headers=headers)
        
        if response.status_code == 200:
            current_config = response.json()
            
            print(f"   Name: {current_config.get('name')}")
            print(f"   Data Source: {current_config.get('dataSourceName')}")
            print(f"   Target Index: {current_config.get('targetIndexName')}")
            
            # Check parameters
            params = current_config.get('parameters', {})
            print(f"   Parameters: {json.dumps(params, indent=6)}")
            
            # Check if there's a skillset (for text processing)
            skillset = current_config.get('skillsetName')
            print(f"   Skillset: {skillset if skillset else 'None (this is the problem!)'}")
            
        else:
            print(f"❌ Could not get indexer config: {response.status_code}")
            return
    except Exception as e:
        print(f"❌ Error checking indexer: {e}")
        return
    
    # Step 2: Create or update skillset for proper COBOL parsing
    print(f"\n🔧 Creating enhanced skillset for COBOL parsing...")
    
    skillset_name = "cobol-skillset"
    skillset_url = f'{endpoint}/skillsets/{skillset_name}?api-version=2023-07-01-preview'
    
    # Enhanced skillset for COBOL files
    skillset_config = {
        "name": skillset_name,
        "description": "Enhanced COBOL file processing with line-by-line parsing",
        "skills": [
            {
                "@odata.type": "#Microsoft.Skills.Text.SplitSkill",
                "name": "splitlines",
                "description": "Split COBOL files into individual lines",
                "context": "/document",
                "defaultLanguageCode": "en",
                "textSplitMode": "lines",
                "maximumPageLength": 1000,  # Max characters per chunk
                "pageOverlapLength": 0,     # No overlap needed for line splitting
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
            },
            {
                "@odata.type": "#Microsoft.Skills.Text.V3.EntityRecognitionSkill",
                "name": "extractentities",
                "description": "Extract COBOL entities and keywords",
                "context": "/document/lines/*",
                "categories": ["Organization", "Person", "Location"],
                "defaultLanguageCode": "en",
                "inputs": [
                    {
                        "name": "text",
                        "source": "/document/lines/*"
                    }
                ],
                "outputs": [
                    {
                        "name": "entities",
                        "targetName": "entities"
                    }
                ]
            }
        ],
        "cognitiveServices": None  # Use free tier
    }
    
    try:
        skillset_response = requests.put(skillset_url, headers=headers, json=skillset_config)
        
        if skillset_response.status_code in [200, 201]:
            print("✅ Skillset created/updated successfully")
        else:
            print(f"❌ Failed to create skillset: {skillset_response.status_code}")
            print(skillset_response.text)
            return
    except Exception as e:
        print(f"❌ Error creating skillset: {e}")
        return
    
    # Step 3: Update indexer to use the skillset and proper field mappings
    print(f"\n🔧 Updating indexer configuration...")
    
    # Enhanced indexer configuration
    enhanced_config = current_config.copy()
    
    # Remove system fields
    for key in ['@odata.context', '@odata.etag']:
        enhanced_config.pop(key, None)
    
    # Add skillset
    enhanced_config['skillsetName'] = skillset_name
    
    # Enhanced field mappings for line-by-line processing
    enhanced_config['fieldMappings'] = [
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
    
    # Output field mappings for processed content
    enhanced_config['outputFieldMappings'] = [
        {
            "sourceFieldName": "/document/lines/*",
            "targetFieldName": "code"
        },
        {
            "sourceFieldName": "/document/lines/*/entities",
            "targetFieldName": "entities"
        }
    ]
    
    # Enhanced parameters for better parsing
    enhanced_config['parameters'] = {
        "batchSize": 50,        # Smaller batches for better processing
        "maxFailedItems": 10,   # Allow some failures
        "maxFailedItemsPerBatch": 5,
        "configuration": {
            "dataToExtract": "contentAndMetadata",
            "parsingMode": "default",
            "indexedFileNameExtensions": ".cbl,.CBL,.cobol,.COBOL",
            "excludedFileNameExtensions": ".log,.tmp",
            "firstLineContainsHeaders": False,
            "delimitedTextDelimiter": "\n"
        }
    }
    
    # Remove schedule to prevent automatic running during testing
    enhanced_config.pop('schedule', None)
    
    try:
        update_response = requests.put(indexer_url, headers=headers, json=enhanced_config)
        
        if update_response.status_code in [200, 201]:
            print("✅ Indexer configuration updated successfully")
        else:
            print(f"❌ Failed to update indexer: {update_response.status_code}")
            print(update_response.text)
            return
    except Exception as e:
        print(f"❌ Error updating indexer: {e}")
        return
    
    # Step 4: Test the updated indexer
    print(f"\n🧪 Testing updated indexer...")
    
    confirm = input("Do you want to run the updated indexer now? (yes/no): ")
    if confirm.lower() in ['yes', 'y']:
        
        # Reset indexer first
        reset_url = f'{endpoint}/indexers/{indexer_name}/reset?api-version=2023-07-01-preview'
        reset_response = requests.post(reset_url, headers=headers)
        
        if reset_response.status_code == 204:
            print("✅ Indexer reset successful")
            
            # Run the indexer
            run_url = f'{endpoint}/indexers/{indexer_name}/run?api-version=2023-07-01-preview'
            run_response = requests.post(run_url, headers=headers)
            
            if run_response.status_code == 202:
                print("✅ Indexer started - monitor progress with monitor_indexer_progress.py")
                print("\n📋 What this should fix:")
                print("   • Parse full COBOL files instead of just first lines")
                print("   • Extract individual lines as separate documents")
                print("   • Better entity recognition for COBOL keywords")
                print("   • Improved search capabilities")
                
            else:
                print(f"❌ Failed to run indexer: {run_response.status_code}")
        else:
            print(f"❌ Failed to reset indexer: {reset_response.status_code}")
    else:
        print("⏳ Indexer updated but not run. Use 'python monitor_indexer_progress.py' when ready.")

if __name__ == "__main__":
    fix_cobol_indexer()
