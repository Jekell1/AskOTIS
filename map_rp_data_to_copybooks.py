"""Use the data_items index to map RP data references to copybooks."""

import requests
import json
import os

# Load credentials
def load_settings():
    vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
    for k in ['SEARCH_ENDPOINT', 'SEARCH_KEY', 'AZURE_SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY']:
        if k in vals and k not in os.environ:
            os.environ[k] = vals[k]

load_settings()

endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

# Data items from RP flow (from ground_truth_rp_analysis.py output)
# This is a subset - we'd need to run that script and capture all 549
sample_data_items = [
    'LP-TRCD', 'HOLD-LP-TRCD', 'RV-LP-TRCD',  # Transaction codes
    'LP-REFNO', 'LP-REFCD',  # Reference fields
    'BATCH-REFCD', 'BRANCH',  # Batch and branch
    'WS-TRANS-TYPE', 'WS-TRAN-CODE',  # Working storage fields
]

print("="*80)
print("MAPPING RP DATA ITEMS TO COPYBOOKS")
print("="*80)

# Query data_items index for LONPF2
url = f"{endpoint}/indexes/new_cobol_data_items/docs/search?api-version=2025-08-01-preview"

body = {
    "search": "*",
    "filter": f"program_id eq 'LONPF2'",
    "select": "data_item_name,copybook_name,program_id",
    "top": 1000  # Get all data items for LONPF2
}

headers = {
    "Content-Type": "application/json",
    "api-key": key
}

try:
    response = requests.post(url, headers=headers, json=body, timeout=30)
    response.raise_for_status()
    result = response.json()
    
    docs = result.get('value', [])
    print(f"✅ Retrieved {len(docs)} data item definitions for LONPF2\n")
    
    # Build mapping: data_item_name -> copybook_name
    data_to_copybook = {}
    for doc in docs:
        data_name = doc.get('data_item_name', '').upper()
        copybook = doc.get('copybook_name', '')
        if data_name and copybook:
            data_to_copybook[data_name] = copybook
    
    print(f"📊 Built mapping for {len(data_to_copybook)} data items\n")
    
    # Map sample RP data items to copybooks
    print("Sample RP data items mapped to copybooks:")
    print("-" * 80)
    
    rp_copybooks = set()
    for data_item in sample_data_items:
        copybook = data_to_copybook.get(data_item.upper(), 'NOT FOUND')
        rp_copybooks.add(copybook)
        print(f"  {data_item:30s} -> {copybook}")
    
    rp_copybooks.discard('NOT FOUND')
    
    print(f"\n📚 Unique copybooks for sample RP data items: {len(rp_copybooks)}")
    print(f"\n⚠️  This is a SAMPLE. Full analysis would:")
    print(f"   1. Run ground_truth_rp_analysis.py to get all 549 data items")
    print(f"   2. Map all 549 to copybooks using this data_items index")
    print(f"   3. Return unique copybooks -> TRUE ground truth")
    
    # Show what we'd need
    print(f"\n{'='*80}")
    print("NEXT STEP: Integrate the two scripts")
    print(f"{'='*80}")
    print("""
To get the true ground truth:
1. Run ground_truth_rp_analysis.py to extract all data items from RP flow
2. Query data_items index with those 549 data items
3. Get copybook names for each
4. Count unique copybooks

This would give us the EXACT answer to:
"Which copybooks are used when LONPF2 handles RP transactions?"
""")
    
except Exception as e:
    print(f"❌ Error: {e}")
