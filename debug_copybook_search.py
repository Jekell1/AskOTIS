"""Quick test of copybook_usage index search."""
import requests
from otis_rag import Config

config = Config()

headers = {
    'Content-Type': 'application/json',
    'api-key': config.search_key
}

# Test 1: Search for just the base name
print("\n" + "="*80)
print("TEST 1: Keyword search for 'LPBW1IN'")
print("="*80)
body1 = {
    "search": "LPBW1IN",
    "top": 5,
    "select": "*"
}
url = f"{config.search_endpoint}/indexes/new_cobol_copybook_usage/docs/search?api-version=2024-07-01"
resp1 = requests.post(url, headers=headers, json=body1)
print(f"Status: {resp1.status_code}")
data1 = resp1.json()
print(f"Results: {len(data1.get('value', []))}")
if data1.get('value'):
    print(f"First result: {data1['value'][0]}")

# Test 2: Search for the full name with extension
print("\n" + "="*80)
print("TEST 2: Keyword search for 'LPBW1IN.CPY'")
print("="*80)
body2 = {
    "search": "LPBW1IN.CPY",
    "top": 5,
    "select": "*"
}
resp2 = requests.post(url, headers=headers, json=body2)
print(f"Status: {resp2.status_code}")
data2 = resp2.json()
print(f"Results: {len(data2.get('value', []))}")
if data2.get('value'):
    print(f"First result copybook_name: {data2['value'][0].get('copybook_name')}")
    print(f"First result program_id: {data2['value'][0].get('program_id')}")

# Test 3: Wildcard search
print("\n" + "="*80)
print("TEST 3: Wildcard search")
print("="*80)
body3 = {
    "search": "*",
    "top": 5,
    "select": "*",
    "filter": "copybook_name eq 'LPBW1IN.CPY'"
}
resp3 = requests.post(url, headers=headers, json=body3)
print(f"Status: {resp3.status_code}")
data3 = resp3.json()
print(f"Results: {len(data3.get('value', []))}")
if data3.get('value'):
    for i, doc in enumerate(data3['value'][:3], 1):
        print(f"  {i}. {doc.get('program_id')} uses {doc.get('copybook_name')} at line {doc.get('line_number')}")
