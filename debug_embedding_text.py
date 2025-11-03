#!/usr/bin/env python3
"""Debug script to test embedding text creation"""

import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

def create_embedding_text(record):
    """Create comprehensive text for embedding generation."""
    parts = []
    
    # Caller and callee context
    if record.get('caller_program'):
        parts.append(f"Caller: {record['caller_program']}")
    
    if record.get('callee_program'):
        parts.append(f"Callee: {record['callee_program']}")
    
    # Reference type and category
    ref_type = record.get('reference_type', '')
    if ref_type:
        parts.append(f"Type: {ref_type}")
    
    category = record.get('category', '')
    if category:
        parts.append(f"Category: {category}")
    
    # Join parts
    text = " | ".join(parts)
    return text

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)

endpoint = config['Values']['SEARCH_ENDPOINT']
key = config['Values']['SEARCH_KEY']

client = SearchClient(endpoint=endpoint, index_name='new_cobol_calls', credential=AzureKeyCredential(key))

# Get first few records without embeddings
result = client.search(search_text='*', filter='has_vector eq false', top=3, select='call_id,caller_program,callee_program,reference_type,category,enhanced_data')

for i, doc in enumerate(result):
    print(f"\nRecord {i+1}:")
    print(f"  call_id: {doc.get('call_id')}")
    
    try:
        text = create_embedding_text(doc)
        print(f"  Generated text: {text}")
    except Exception as e:
        print(f"  ERROR creating text: {e}")
        print(f"  Record data: {doc}")