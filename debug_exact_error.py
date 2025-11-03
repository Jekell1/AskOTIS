#!/usr/bin/env python3
"""Debug script to trace exactly where the Invalid format string error occurs"""

import json
import sys
import os
import traceback
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from embedding_utils import batch_embed

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

# Get the exact first record that's failing
result = client.search(search_text='*', filter='has_vector eq false', top=1, select='call_id,caller_program,callee_program,reference_type,category,enhanced_data')

for doc in result:
    record_id = doc.get('call_id', 'unknown')
    print(f"Testing record: {record_id}")
    
    try:
        print("Step 1: Creating text...")
        text = create_embedding_text(doc)
        print(f"Generated text: '{text}'")
        
        print("Step 2: Generating embedding...")
        embeddings = batch_embed([text], target_dim=3072)
        print(f"Embedding generated successfully: {len(embeddings[0])} dimensions")
        
    except Exception as e:
        print(f"ERROR at step: {e}")
        print(f"Exception type: {type(e)}")
        print("Full traceback:")
        traceback.print_exc()
    break