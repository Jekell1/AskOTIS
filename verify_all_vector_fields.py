#!/usr/bin/env python3
"""Check ALL vector fields in COBOL indexes to ensure retriever has correct names."""

import requests
import json

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT']
key = settings['SEARCH_KEY']
headers = {'api-key': key, 'Content-Type': 'application/json'}

# All indexes used in OTIS RAG
indexes = [
    'code-chunks',
    'new_cobol_program_meta',
    'new_cobol_paragraphs',
    'new_cobol_data_items',
    'new_cobol_variable_usage',
    'new_cobol_calls',
    'new_cobol_program_flows',
    'new_cobol_ui_paths',
    'new_cobol_copybook_meta',
    'new_cobol_symbol_refs'
]

# Current mapping in retriever.py
retriever_mapping = {
    'code-chunks': 'text_vector',
    'new_cobol_program_meta': 'summary_vector',
    'new_cobol_paragraphs': 'para_vector',
    'new_cobol_data_items': 'vector',
    'new_cobol_variable_usage': 'usage_summary_vector',
    'new_cobol_calls': 'call_summary_vector',
    'new_cobol_program_flows': 'flow_summary_vector',
    'new_cobol_ui_paths': 'sequence_vector',
    'new_cobol_copybook_meta': 'copybook_summary_vector',
    'new_cobol_symbol_refs': 'reference_vector'
}

print("\n" + "="*70)
print("VECTOR FIELD VERIFICATION FOR OTIS RAG")
print("="*70)

all_correct = True

for index_name in indexes:
    url = f'{endpoint}/indexes/{index_name}?api-version=2025-08-01-preview'
    
    try:
        response = requests.get(url, headers=headers, timeout=10)
        
        if response.status_code == 200:
            schema = response.json()
            
            # Find vector fields
            vector_fields = []
            for field in schema.get('fields', []):
                if field.get('type') == 'Collection(Edm.Single)':
                    vector_fields.append(field['name'])
            
            expected = retriever_mapping.get(index_name, '???')
            
            if vector_fields:
                actual = vector_fields[0]  # Assume first vector field
                
                if actual == expected:
                    print(f"✅ {index_name:<30} → {actual}")
                else:
                    print(f"❌ {index_name:<30} → Expected: {expected}, Actual: {actual}")
                    all_correct = False
            else:
                print(f"⚠️  {index_name:<30} → No vector fields found")
        else:
            print(f"❌ {index_name:<30} → Error {response.status_code}")
            all_correct = False
    
    except Exception as e:
        print(f"❌ {index_name:<30} → Exception: {e}")
        all_correct = False

print("="*70)
if all_correct:
    print("✅ ALL VECTOR FIELD MAPPINGS ARE CORRECT!")
else:
    print("❌ SOME MAPPINGS NEED TO BE FIXED")
print("="*70 + "\n")
