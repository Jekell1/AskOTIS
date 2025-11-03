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

# Test searches to understand what we have in the index
url = f"https://{search_service_name}.search.windows.net/indexes/cobol-index/docs/search"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

print("ANALYZING INDEXED COBOL CONTENT:")
print("=" * 60)

# 1. Search for CUSTOMER in any field
customer_search = {
    "search": "CUSTOMER",
    "select": "repo_path,line,code,symbol_name,symbol_kind,parsed_content",
    "top": 10
}

print("1. Searching for 'CUSTOMER' in any field...")
try:
    response = requests.post(url, headers=headers, params=params, json=customer_search)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        print(f"Found {len(results)} results containing 'CUSTOMER'")
        
        if results:
            for i, doc in enumerate(results[:3], 1):  # Show first 3
                print(f"\nResult {i}:")
                print(f"  File: {doc.get('repo_path', '').split('/')[-1]}")
                print(f"  Line: {doc.get('line', 'N/A')}")
                print(f"  Code: {doc.get('code', 'N/A')[:80]}...")
                print(f"  Symbol Name: {doc.get('symbol_name', 'N/A')}")
                print(f"  Symbol Kind: {doc.get('symbol_kind', 'N/A')}")
                
                # Check if parsed_content exists
                parsed_content = doc.get('parsed_content')
                if parsed_content:
                    try:
                        parsed = json.loads(parsed_content)
                        print(f"  Parsed Content Keys: {list(parsed.keys())}")
                        if 'symbol_name' in parsed:
                            print(f"  Parsed Symbol Name: {parsed.get('symbol_name')}")
                        if 'symbol_kind' in parsed:
                            print(f"  Parsed Symbol Kind: {parsed.get('symbol_kind')}")
                    except:
                        print(f"  Parsed Content (raw): {parsed_content[:100]}...")
                else:
                    print("  Parsed Content: N/A")
                    
    else:
        print(f"Search failed: {response.status_code} - {response.text}")
        
except Exception as e:
    print(f"Error: {e}")

print(f"\n" + "="*60)

# 2. Look for documents with populated symbol fields
symbol_search = {
    "search": "*",
    "filter": "symbol_name ne null and symbol_kind ne null",
    "select": "repo_path,line,code,symbol_name,symbol_kind",
    "top": 5
}

print("2. Searching for documents with populated symbol fields...")
try:
    response = requests.post(url, headers=headers, params=params, json=symbol_search)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        print(f"Found {len(results)} documents with symbol data")
        
        if results:
            for i, doc in enumerate(results, 1):
                print(f"\nDocument {i}:")
                print(f"  File: {doc.get('repo_path', '').split('/')[-1]}")
                print(f"  Line: {doc.get('line', 'N/A')}")
                print(f"  Code: {doc.get('code', 'N/A')[:60]}...")
                print(f"  Symbol Name: {doc.get('symbol_name', 'N/A')}")
                print(f"  Symbol Kind: {doc.get('symbol_kind', 'N/A')}")
        else:
            print("No documents found with populated symbol fields")
            print("This indicates the Azure Function isn't extracting symbol information")
            
    else:
        print(f"Search failed: {response.status_code} - {response.text}")
        
except Exception as e:
    print(f"Error: {e}")

print(f"\n" + "="*60)

# 3. Check what fields actually exist in documents
field_check = {
    "search": "*",
    "select": "*",  # Get all fields
    "top": 1
}

print("3. Checking all available fields in a sample document...")
try:
    response = requests.post(url, headers=headers, params=params, json=field_check)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        if results:
            doc = results[0]
            print("Available fields in sample document:")
            for field, value in doc.items():
                if not field.startswith('@'):  # Skip metadata fields
                    print(f"  {field}: {str(value)[:100]}..." if len(str(value)) > 100 else f"  {field}: {value}")
        else:
            print("No documents found")
            
    else:
        print(f"Search failed: {response.status_code} - {response.text}")
        
except Exception as e:
    print(f"Error: {e}")

print(f"\n" + "="*60)
print("DIAGNOSIS:")
print("If symbol_name and symbol_kind are showing as 'None' or empty,")
print("the Azure Function needs to be updated to extract and return these fields.")
print("Let's check the Function App next...")
