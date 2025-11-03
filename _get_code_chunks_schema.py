#!/usr/bin/env python3
"""
Get the schema of new_code_chunks index to see what fields exist.
"""

import json
import requests

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

url = f"{settings['SEARCH_ENDPOINT']}/indexes/new_code_chunks?api-version=2025-08-01-preview"

r = requests.get(url, headers={'api-key': settings['SEARCH_KEY']})

if r.status_code == 200:
    schema = r.json()
    
    print("=" * 80)
    print("new_code_chunks INDEX SCHEMA")
    print("=" * 80)
    
    fields = schema.get('fields', [])
    
    print(f"\nTotal fields: {len(fields)}\n")
    
    print("Field Details:")
    print("-" * 80)
    
    for field in fields:
        name = field.get('name')
        field_type = field.get('type')
        searchable = field.get('searchable', False)
        filterable = field.get('filterable', False)
        sortable = field.get('sortable', False)
        facetable = field.get('facetable', False)
        retrievable = field.get('retrievable', True)
        
        flags = []
        if searchable: flags.append('S')
        if filterable: flags.append('F')
        if sortable: flags.append('O')
        if facetable: flags.append('X')
        if retrievable: flags.append('R')
        
        flags_str = ''.join(flags) if flags else '-'
        
        print(f"  {name:30s} {field_type:20s} [{flags_str:10s}]")
    
    print("\nFlag Legend:")
    print("  S = Searchable, F = Filterable, O = Sortable, X = Facetable, R = Retrievable")
    
    # Sample a document to see actual field values
    print("\n" + "=" * 80)
    print("SAMPLE DOCUMENT")
    print("=" * 80)
    
    search_url = f"{settings['SEARCH_ENDPOINT']}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"
    
    body = {
        'search': '*',
        'top': 1
    }
    
    r2 = requests.post(search_url, headers={'api-key': settings['SEARCH_KEY']}, json=body)
    docs = r2.json().get('value', [])
    
    if docs:
        doc = docs[0]
        print("\nSample document fields:")
        for key, value in doc.items():
            if key.startswith('@'):
                continue
            
            # Truncate long values
            if isinstance(value, str) and len(value) > 100:
                value_display = value[:100] + '...'
            elif isinstance(value, list) and len(value) > 3:
                value_display = f"[{len(value)} items] {value[:3]}"
            else:
                value_display = value
            
            print(f"  {key}: {value_display}")

else:
    print(f"Error: {r.status_code}")
    print(r.text)
