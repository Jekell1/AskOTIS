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

# Azure AI Search REST API endpoint for skillsets
url = f"https://{search_service_name}.search.windows.net/skillsets/cobol-skillset"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

# NOTE: Update these placeholders with your actual Azure Function details  
FUNCTION_APP_NAME = "cobol-parser-func-1474"        # Your Function App name
FUNCTION_KEY = os.environ.get("AZURE_FUNCTION_KEY")  # Your Function key

# Complete skillset definition based on your specification
skillset_data = {
    "name": "cobol-skillset",
    "description": "Extract COBOL symbols and calls using custom parsing",
    "skills": [
        {
            "@odata.type": "#Microsoft.Skills.Custom.WebApiSkill",
            "description": "Extract COBOL symbols and calls",
            "uri": f"https://{FUNCTION_APP_NAME}.azurewebsites.net/api/cobol-parse?code={FUNCTION_KEY}",
            "batchSize": 10,
            "context": "/document",
            "inputs": [
                { "name": "code", "source": "/document/code" },
                { "name": "repo_path", "source": "/document/repo_path" },
                { "name": "line_number", "source": "/document/line_number" }
            ],
            "outputs": [
                { "name": "symbols", "targetName": "symbols" },
                { "name": "calls", "targetName": "calls" },
                { "name": "procedures", "targetName": "procedures" },
                { "name": "variables", "targetName": "variables" }
            ],
            "httpHeaders": {
                "Content-Type": "application/json"
            },
            "timeout": "PT30S"
        }
    ],
    "cognitiveServices": {
        "@odata.type": "#Microsoft.Azure.Search.DefaultCognitiveServices"
    }
}

# Check if placeholders are still present
if "your-function-app" in skillset_data["skills"][0]["uri"]:
    print("⚠️  WARNING: You need to update the Function App details!")
    print("   Please update FUNCTION_APP_NAME and FUNCTION_KEY in the script")
    print("   Current URI:", skillset_data["skills"][0]["uri"])
    print()
    
    response = input("Do you want to proceed with placeholders? (y/N): ")
    if response.lower() != 'y':
        print("Exiting. Please update the Function App details and try again.")
        sys.exit(1)

try:
    # Create the skillset using PUT
    print(f"Creating skillset: {skillset_data['name']}")
    print(f"Function URI: {skillset_data['skills'][0]['uri']}")
    print(f"Target URL: {url}")
    
    response = requests.put(url, headers=headers, params=params, json=skillset_data)
    
    print(f"\nStatus Code: {response.status_code}")
    
    if response.status_code in [200, 201, 204]:
        print("✓ Skillset 'cobol-skillset' created successfully!")
        
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
                        print(f"    URI: {skill['uri']}")
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
            
            if 'uri' in skill:
                uri = skill['uri']
                # Hide function key for security
                if 'code=' in uri:
                    uri = uri.split('code=')[0] + 'code=***'
                print(f"       URI: {uri}")
    else:
        print(f"✗ Verification failed: Status {verify_response.status_code}")
        if verify_response.status_code == 404:
            print("  Skillset was not created successfully")
        
except Exception as e:
    print(f"\nError verifying skillset: {e}")

print(f"\n" + "="*60)
print("NEXT STEPS:")
print("1. Update FUNCTION_APP_NAME and FUNCTION_KEY in this script")
print("2. Create an Azure Function that accepts the expected inputs")
print("3. The Function should return JSON with: symbols, calls, procedures, variables")
print("4. Re-run this script after updating the configuration")
print("="*60)
