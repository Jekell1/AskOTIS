"""Query the line count for APIPAY program."""
import json
import requests
import env_autoload

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT'].rstrip('/')
key = settings['SEARCH_KEY']
index_name = 'new-cobol-files'

print("🔍 SEARCHING FOR APIPAY LINE COUNT")
print("=" * 60)

search_url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Search for APIPAY
search_body = {
    "search": "APIPAY",
    "searchFields": "programId",
    "select": "programId,sloc,path,kind",
    "top": 10
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    docs = results.get('value', [])
    
    if docs:
        print(f"\nFound {len(docs)} file(s) matching 'APIPAY':\n")
        for doc in docs:
            prog = doc.get('programId', 'N/A')
            sloc = doc.get('sloc', 0)
            path = doc.get('path', 'N/A')
            kind = doc.get('kind', 'N/A')
            print(f"Program: {prog}")
            print(f"Lines:   {sloc:,}")
            print(f"Kind:    {kind}")
            print(f"Path:    {path}")
            print("-" * 60)
    else:
        print("❌ No files found matching 'APIPAY'")
        
        # Try broader search
        print("\nTrying broader search (contains APIPAY)...")
        search_body = {
            "search": "*",
            "filter": "search.ismatch('APIPAY', 'programId')",
            "select": "programId,sloc,path,kind",
            "top": 10
        }
        response = requests.post(search_url, headers=headers, json=search_body)
        if response.status_code == 200:
            results = response.json()
            docs = results.get('value', [])
            if docs:
                print(f"\nFound {len(docs)} file(s):\n")
                for doc in docs:
                    prog = doc.get('programId', 'N/A')
                    sloc = doc.get('sloc', 0)
                    path = doc.get('path', 'N/A')
                    kind = doc.get('kind', 'N/A')
                    print(f"Program: {prog}")
                    print(f"Lines:   {sloc:,}")
                    print(f"Kind:    {kind}")
                    print(f"Path:    {path}")
                    print("-" * 60)
            else:
                print("No files found")
else:
    print(f"❌ Error: {response.status_code}")
    print(response.text)
