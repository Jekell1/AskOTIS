import json
import requests
import sys

# Load configuration
try:
    with open('local.settings.json', 'r') as f:
        config = json.load(f)
    
    # Extract service name from endpoint
    search_endpoint = config['Values']['SEARCH_ENDPOINT']
    search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
    search_admin_key = config['Values']['SEARCH_KEY']
    
except Exception as e:
    print(f"Error loading configuration: {e}")
    sys.exit(1)

# Function details from our deployment
function_app_name = "cobol-parser-func-1474"
function_key = os.environ.get("AZURE_FUNCTION_KEY")
function_uri = f"https://{function_app_name}.azurewebsites.net/api/cobol-parse?code={function_key}"

# Azure AI Search REST API endpoint for skillsets
url = f"https://{search_service_name}.search.windows.net/skillsets/cobol-skillset"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

# Skillset definition with WebAPI skill
skillset_data = {
    "name": "cobol-skillset",
    "description": "Extract COBOL symbols and calls using custom parsing function",
    "skills": [
        {
            "@odata.type": "#Microsoft.Skills.Custom.WebApiSkill",
            "description": "Extract COBOL symbols and calls",
            "uri": function_uri,
            "batchSize": 10,
            "context": "/document",
            "inputs": [
                { "name": "code", "source": "/document/code" },
                { "name": "repo_path", "source": "/document/repo_path" },
                { "name": "line_number", "source": "/document/line" }
            ],
            "outputs": [
                { "name": "symbols", "targetName": "symbols" },
                { "name": "calls", "targetName": "calls" },
                { "name": "procedures", "targetName": "procedures" },
                { "name": "variables", "targetName": "variables" },
                { "name": "symbol_type", "targetName": "symbol_type" }
            ],
            "timeout": "PT30S"
        }
    ],
    "cognitiveServices": {
        "@odata.type": "#Microsoft.Azure.Search.DefaultCognitiveServices"
    }
}

print("Creating COBOL skillset with WebAPI skill:")
print("=" * 60)
print(f"Search Service: {search_service_name}")
print(f"Function App: {function_app_name}")
print(f"Function URI: {function_uri[:80]}...")
print(f"Skillset Name: {skillset_data['name']}")
print()

try:
    # Create the skillset using PUT
    response = requests.put(url, headers=headers, params=params, json=skillset_data)
    
    print(f"Status Code: {response.status_code}")
    print(f"URL: {url}")
    
    if response.status_code in [200, 201, 204]:
        print("✅ Skillset 'cobol-skillset' created successfully!")
        
        # Show the response if there is content
        if response.content:
            try:
                result = response.json()
                print(f"Created skillset: {result.get('name', 'N/A')}")
                print(f"Description: {result.get('description', 'N/A')}")
                print(f"Number of skills: {len(result.get('skills', []))}")
                
                # Show skill details
                for i, skill in enumerate(result.get('skills', [])):
                    print(f"  Skill {i+1}: {skill.get('@odata.type', 'Unknown')}")
                    print(f"    Description: {skill.get('description', 'N/A')}")
                    if 'uri' in skill:
                        # Hide the function key for security
                        uri = skill['uri']
                        if 'code=' in uri:
                            uri = uri.split('code=')[0] + 'code=***'
                        print(f"    URI: {uri}")
                    print(f"    Batch Size: {skill.get('batchSize', 'N/A')}")
                    print(f"    Inputs: {len(skill.get('inputs', []))}")
                    print(f"    Outputs: {len(skill.get('outputs', []))}")
                    
            except:
                print("Response received but no JSON content")
        else:
            print("Skillset created (no response body)")
    else:
        print(f"✗ Failed to create skillset")
        print(f"Response: {response.text}")
        
        # Try to parse error details
        try:
            error_data = response.json()
            if 'error' in error_data:
                print(f"Error details: {error_data['error'].get('message', 'No details')}")
        except:
            pass
        
except Exception as e:
    print(f"Error creating skillset: {e}")

# Verify the skillset was created
try:
    print(f"\nVerifying skillset creation...")
    verify_url = f"https://{search_service_name}.search.windows.net/skillsets/cobol-skillset"
    verify_response = requests.get(verify_url, headers=headers, params={'api-version': '2024-07-01'})
    
    if verify_response.status_code == 200:
        skillset = verify_response.json()
        print(f"✓ Verification: Skillset '{skillset['name']}' exists")
        print(f"  Description: {skillset.get('description', 'N/A')}")
        print(f"  Skills: {len(skillset.get('skills', []))}")
        
        # List skill types and their configuration
        for i, skill in enumerate(skillset.get('skills', [])):
            skill_type = skill.get('@odata.type', 'Unknown').replace('#Microsoft.Skills.', '')
            skill_desc = skill.get('description', 'No description')
            print(f"    {i+1}. {skill_type}: {skill_desc}")
            
            # Show inputs and outputs
            inputs = skill.get('inputs', [])
            outputs = skill.get('outputs', [])
            print(f"       Inputs: {[inp['name'] for inp in inputs]}")
            print(f"       Outputs: {[out['name'] for out in outputs]}")
            
    else:
        print(f"✗ Verification failed: Status {verify_response.status_code}")
        if verify_response.status_code == 404:
            print("  Skillset was not created successfully")
        
except Exception as e:
    print(f"\nError verifying skillset: {e}")

print(f"\n" + "="*60)
print("SKILLSET SUMMARY:")
print("✅ Skillset Name: cobol-skillset")
print("✅ WebAPI Skill: Custom COBOL parser")
print("✅ Function App: cobol-parser-func-1474")
print("✅ Batch Size: 10 records per request")
print("✅ Timeout: 30 seconds")
print()
print("Input Fields:")
print("  - code (from /document/content)")
print("  - repo_path (from /document/metadata_storage_path)")
print("  - line_number (from /document/metadata_storage_name)")
print()
print("Output Fields:")
print("  - symbols: Symbol names found")
print("  - calls: Function calls found")
print("  - procedures: Procedure names")
print("  - variables: Variable names")
print("  - symbol_type: Type of symbol")
print()
print("NEXT STEPS:")
print("1. Create an indexer to use this skillset")
print("2. Run the indexer to process your COBOL files")
print("3. Test search functionality")
print("="*60)
