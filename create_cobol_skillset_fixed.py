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
    
    # Get OpenAI configuration
    openai_endpoint = config['Values']['AZURE_OPENAI_ENDPOINT']
    openai_key = config['Values']['AZURE_OPENAI_KEY']
    
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

# Skillset definition with resolved configuration values
skillset_data = {
    "name": "cobol-skillset",
    "description": "Skillset for processing COBOL code files",
    "skills": [
        {
            "@odata.type": "#Microsoft.Skills.Text.SplitSkill",
            "description": "Split COBOL code into manageable chunks",
            "context": "/document",
            "textSplitMode": "pages",
            "maximumPageLength": 2000,
            "pageOverlapLength": 200,
            "inputs": [
                { "name": "text", "source": "/document/content" }
            ],
            "outputs": [
                { "name": "textItems", "targetName": "pages" }
            ]
        },
        {
            "@odata.type": "#Microsoft.Skills.Text.AzureOpenAIEmbeddingSkill",
            "description": "Generate embeddings for COBOL code chunks",
            "context": "/document/pages/*",
            "resourceUri": openai_endpoint,
            "apiKey": openai_key,
            "deploymentId": "text-embedding-ada-002",
            "inputs": [
                { "name": "text", "source": "/document/pages/*" }
            ],
            "outputs": [
                { "name": "embedding", "targetName": "vector" }
            ]
        }
    ],
    "cognitiveServices": {
        "@odata.type": "#Microsoft.Azure.Search.DefaultCognitiveServices"
    }
}

try:
    # Create the skillset using PUT
    response = requests.put(url, headers=headers, params=params, json=skillset_data)
    
    print(f"Status Code: {response.status_code}")
    print(f"URL: {url}")
    
    if response.status_code in [200, 201, 204]:
        print("✓ Skillset 'cobol-skillset' created successfully!")
        
        # Show the response if there is content
        if response.content:
            try:
                result = response.json()
                print(f"Created skillset: {result.get('name', 'N/A')}")
                print(f"Description: {result.get('description', 'N/A')}")
                print(f"Number of skills: {len(result.get('skills', []))}")
            except:
                print("Response received but no JSON content")
        else:
            print("Skillset created (no response body)")
    else:
        print(f"✗ Failed to create skillset")
        print(f"Response: {response.text}")
        
except Exception as e:
    print(f"Error creating skillset: {e}")

# Verify the skillset was created
try:
    verify_url = f"https://{search_service_name}.search.windows.net/skillsets/cobol-skillset"
    verify_response = requests.get(verify_url, headers=headers, params={'api-version': '2024-07-01'})
    
    if verify_response.status_code == 200:
        skillset = verify_response.json()
        print(f"\n✓ Verification: Skillset '{skillset['name']}' exists")
        print(f"  Description: {skillset.get('description', 'N/A')}")
        print(f"  Skills: {len(skillset.get('skills', []))}")
        
        # List skill types
        for i, skill in enumerate(skillset.get('skills', [])):
            skill_type = skill.get('@odata.type', 'Unknown')
            skill_desc = skill.get('description', 'No description')
            print(f"    {i+1}. {skill_type.replace('#Microsoft.Skills.', '')}: {skill_desc}")
    else:
        print(f"\n✗ Verification failed: Status {verify_response.status_code}")
        
except Exception as e:
    print(f"\nError verifying skillset: {e}")
