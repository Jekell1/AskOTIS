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
url = f"https://{search_service_name}.search.windows.net/skillsets"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2023-11-01'
}

try:
    # List all skillsets
    response = requests.get(url, headers=headers, params=params)
    print(f"Status Code: {response.status_code}")
    
    if response.status_code == 200:
        skillsets = response.json()
        print(f"Found {len(skillsets['value'])} skillsets:")
        print()
        
        for skillset in skillsets['value']:
            name = skillset.get('name', 'Unknown')
            description = skillset.get('description', 'No description')
            skills_count = len(skillset.get('skills', []))
            
            print(f"Name: {name}")
            print(f"Description: {description}")
            print(f"Number of skills: {skills_count}")
            
            # Show skill types
            if skillset.get('skills'):
                skill_types = [skill.get('@odata.type', 'Unknown') for skill in skillset['skills']]
                print(f"Skill types: {', '.join(set(skill_types))}")
            
            print("-" * 50)
    else:
        print(f"Error listing skillsets: {response.text}")
        
except Exception as e:
    print(f"Error: {e}")
