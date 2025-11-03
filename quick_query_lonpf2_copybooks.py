"""Quick query to check copybook-usage index for LONPF2."""
import os
import sys
import json
import requests

INDEX = 'new_cobol_copybook_usage'
API_VERSION = '2025-08-01-preview'

def load_settings():
    """Load credentials from local.settings.json if not in environment."""
    try:
        with open('local.settings.json', 'r', encoding='utf-8') as f:
            settings = json.load(f)
            values = settings.get('Values', {})
            
            print(f"📄 Found local.settings.json with {len(values)} settings")
            
            # Set environment variables from settings (always load, even if already in env)
            for key in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 
                       'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
                if key in values:
                    os.environ[key] = values[key]
                    # Show masked value for keys
                    if 'KEY' in key:
                        val_display = '*' * 20 + values[key][-4:] if len(values[key]) > 4 else '****'
                    else:
                        val_display = values[key]
                    print(f"   ✅ {key}: {val_display}")
                else:
                    print(f"   ❌ {key}: not in settings")
    except FileNotFoundError:
        print("⚠️  local.settings.json not found")
    except Exception as e:
        print(f"⚠️  Error loading settings: {e}")

def get_credentials():
    """Get Azure Search endpoint and key from environment."""
    endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not endpoint or not key:
        print("❌ Missing AZURE_SEARCH_ENDPOINT or AZURE_SEARCH_KEY")
        print("   Checked env variables:")
        print(f"   - AZURE_SEARCH_ENDPOINT: {'SET' if os.getenv('AZURE_SEARCH_ENDPOINT') else 'NOT SET'}")
        print(f"   - SEARCH_ENDPOINT: {'SET' if os.getenv('SEARCH_ENDPOINT') else 'NOT SET'}")
        print(f"   - AZURE_SEARCH_KEY: {'SET' if os.getenv('AZURE_SEARCH_KEY') else 'NOT SET'}")
        print(f"   - SEARCH_KEY: {'SET' if os.getenv('SEARCH_KEY') else 'NOT SET'}")
        sys.exit(1)
    
    return endpoint.rstrip('/'), key

def query_lonpf2_copybooks(endpoint, key):
    """Query copybook-usage index for LONPF2."""
    url = f"{endpoint}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    
    # Query for all LONPF2 copybook usage records
    body = {
        'search': '*',
        'filter': "program_id eq 'LONPF2'",
        'select': 'program_id,copybook_name,line_number',
        'top': 500  # Should be enough to get all
    }
    
    headers = {
        'api-key': key,
        'Content-Type': 'application/json'
    }
    
    print(f"🔍 Querying {INDEX} for program LONPF2...")
    print(f"   URL: {url}")
    print(f"   Filter: {body['filter']}\n")
    
    try:
        response = requests.post(url, headers=headers, json=body, timeout=60)
        response.raise_for_status()
        
        data = response.json()
        results = data.get('value', [])
        
        # Extract unique copybook names
        copybook_names = sorted(set(r['copybook_name'] for r in results))
        
        print(f"✅ Query successful!")
        print(f"📊 Results: {len(results)} total records")
        print(f"📚 Unique copybooks: {len(copybook_names)}")
        print()
        print("=" * 80)
        print("COPYBOOKS IN INDEX:")
        print("=" * 80)
        for i, cb in enumerate(copybook_names, 1):
            print(f"{i:3}. {cb}")
        print("=" * 80)
        
        # Save results
        with open('lonpf2_copybook_index_results.json', 'w') as f:
            json.dump({
                'query': 'LONPF2 copybooks',
                'total_records': len(results),
                'unique_copybooks': len(copybook_names),
                'copybooks': copybook_names,
                'records': results
            }, f, indent=2)
        
        print(f"\n💾 Full results saved to: lonpf2_copybook_index_results.json")
        
        return copybook_names
        
    except requests.exceptions.RequestException as e:
        print(f"❌ Query failed: {e}")
        if hasattr(e, 'response') and e.response is not None:
            print(f"   Status: {e.response.status_code}")
            print(f"   Response: {e.response.text[:500]}")
        sys.exit(1)

if __name__ == '__main__':
    print("=" * 80)
    print("LONPF2 Copybook Index Coverage Check")
    print("=" * 80)
    print()
    
    # Load credentials
    load_settings()
    endpoint, key = get_credentials()
    
    print(f"🔗 Endpoint: {endpoint}")
    print(f"🔑 Key: {'*' * 20}{key[-4:]}\n")
    
    # Query
    copybooks = query_lonpf2_copybooks(endpoint, key)
    
    print()
    print("=" * 80)
    print("COMPARISON:")
    print("=" * 80)
    print(f"Expected (from source code): 248 copybooks")
    print(f"Found in index:               {len(copybooks)} copybooks")
    print(f"Missing:                      {248 - len(copybooks)} copybooks ({((248 - len(copybooks)) / 248 * 100):.1f}%)")
    print("=" * 80)
