#!/usr/bin/env python3
"""Simple check for vector field existence."""

import requests
import os
from secrets_loader import load_secrets

def main():
    load_secrets()
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    # Test one index that we know worked
    index_name = 'new_cobol_program_meta'
    
    url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'top': 3,
        'select': 'program_id,has_vector'
    }
    
    response = requests.post(
        url,
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=30
    )
    
    if response.status_code == 200:
        data = response.json()
        docs = data.get('value', [])
        
        print(f"Found {len(docs)} records:")
        for doc in docs:
            print(f"  {doc.get('program_id')}: has_vector={doc.get('has_vector')}")
    else:
        print(f"Error: {response.status_code} - {response.text}")

if __name__ == '__main__':
    main()