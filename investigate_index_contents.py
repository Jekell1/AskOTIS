import json
import requests
import sys

# Load configuration
try:
    with open('local.settings.json', 'r') as f:
        config = json.load(f)
    
    search_endpoint = config['Values']['SEARCH_ENDPOINT']
    search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
    search_admin_key = config['Values']['SEARCH_KEY']
    
except Exception as e:
    print(f"Error loading configuration: {e}")
    sys.exit(1)

url = f"https://{search_service_name}.search.windows.net/indexes/cobol-index/docs/search"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

print("INVESTIGATING WHAT FILES ARE ACTUALLY IN THE INDEX:")
print("=" * 60)

# 1. Get a sample of documents to see what files are indexed
sample_search = {
    "search": "*",
    "select": "repo_path,line,code",
    "top": 10
}

print("1. SAMPLE OF INDEXED FILES:")
try:
    response = requests.post(url, headers=headers, params=params, json=sample_search)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        print(f"✅ Found {len(results)} sample documents")
        
        if results:
            files_found = set()
            for doc in results:
                repo_path = doc.get('repo_path', '')
                filename = repo_path.split('/')[-1]
                files_found.add(filename)
                
            print(f"\nSample files in index:")
            for i, filename in enumerate(sorted(files_found), 1):
                print(f"  {i}. {filename}")
                
        else:
            print("No documents found!")
            
    else:
        print(f"❌ Error: {response.text}")
        
except Exception as e:
    print(f"❌ Exception: {e}")

print("-" * 60)

# 2. Search specifically for ACUMEM file
acumem_file_search = {
    "search": "ACUMEM",
    "searchFields": ["repo_path"],  # Search only in the repo_path field
    "select": "repo_path,line,code",
    "top": 5
}

print("2. SEARCHING FOR ACUMEM IN FILE PATHS:")
try:
    response = requests.post(url, headers=headers, params=params, json=acumem_file_search)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        print(f"✅ Found {len(results)} files with ACUMEM in path")
        
        if results:
            for i, doc in enumerate(results, 1):
                print(f"  {i}. {doc.get('repo_path', '').split('/')[-1]}")
                print(f"     Line: {doc.get('line')}")
                print(f"     Code: {doc.get('code')[:60]}...")
                print()
        else:
            print("❌ No ACUMEM files found in paths")
            
    else:
        print(f"❌ Error: {response.text}")
        
except Exception as e:
    print(f"❌ Exception: {e}")

print("-" * 60)

# 3. Look for terms that we know work from previous tests
working_search = {
    "search": "IDENTIFICATION",
    "select": "repo_path,line,code",
    "top": 3
}

print("3. TESTING WITH KNOWN WORKING TERM 'IDENTIFICATION':")
try:
    response = requests.post(url, headers=headers, params=params, json=working_search)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        print(f"✅ Found {len(results)} results for 'IDENTIFICATION'")
        
        if results:
            for i, doc in enumerate(results, 1):
                print(f"  {i}. File: {doc.get('repo_path', '').split('/')[-1]}")
                print(f"     Line: {doc.get('line')}")
                print(f"     Code: {doc.get('code')}")
                print()
                
                # Let's search for content in this specific file
                print(f"     🔍 Let's test searching within this specific file...")
                
        else:
            print("❌ Even 'IDENTIFICATION' not found - something's wrong!")
            
    else:
        print(f"❌ Error: {response.text}")
        
except Exception as e:
    print(f"❌ Exception: {e}")

print("-" * 60)

# 4. Check total document count
count_search = {
    "search": "*",
    "count": True,
    "top": 0  # Don't return documents, just count
}

print("4. TOTAL DOCUMENT COUNT:")
try:
    response = requests.post(url, headers=headers, params=params, json=count_search)
    
    if response.status_code == 200:
        result = response.json()
        total_count = result.get('@odata.count', 'Unknown')
        
        print(f"✅ Total documents in index: {total_count}")
        
    else:
        print(f"❌ Error: {response.text}")
        
except Exception as e:
    print(f"❌ Exception: {e}")

print("=" * 60)
print("ANALYSIS:")
print("If ACUMEM terms aren't found, it could mean:")
print("1. ✅ The ACUMEM.CBL file isn't in the indexed blob storage")
print("2. ✅ The file is there but the indexer hasn't processed it yet")
print("3. ✅ The file is processed but the Azure Function isn't extracting the terms properly")
print("Let's check the indexer status next...")
print("=" * 60)
