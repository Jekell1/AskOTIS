"""
Dedicated processor for screen nodes embeddings.
"""
import os
import json
import requests
import time
from typing import Dict, List
from secrets_loader import load_secrets

def get_embedding(config: Dict, text: str):
    """Generate embedding for text."""
    url = f"{config['openai_endpoint']}/openai/deployments/{config['embedding_deployment']}/embeddings?api-version=2023-05-15"
    
    try:
        response = requests.post(
            url,
            headers={
                'api-key': config['openai_key'],
                'Content-Type': 'application/json'
            },
            json={
                'input': text,
                'model': config.get('embedding_model', 'text-embedding-3-large')
            },
            timeout=30
        )
        
        if response.status_code == 200:
            return response.json()['data'][0]['embedding']
        else:
            return None
    except Exception as e:
        return None

def get_records_for_embedding(config: Dict, limit: int) -> List[Dict]:
    """Get screen node records that need embeddings."""
    url = f"{config['search_endpoint']}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false',
        'top': limit,
        'select': '*'
    }
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        if response.status_code == 200:
            return response.json().get('value', [])
        else:
            return []
    except Exception as e:
        return []

def create_text_for_embedding(record: Dict) -> str:
    """Create text for embedding from screen node record."""
    text_parts = []
    
    # Add summary text
    if record.get('summary_text'):
        text_parts.append(record['summary_text'])
    
    # Add screen name
    if record.get('screen_name'):
        text_parts.append(f"Screen: {record['screen_name']}")
    
    # Add program ID
    if record.get('program_id'):
        text_parts.append(f"Program: {record['program_id']}")
    
    # Add screen type
    if record.get('screen_type'):
        text_parts.append(f"Type: {record['screen_type']}")
    
    # Add field information
    if record.get('fields_json'):
        try:
            fields = json.loads(record['fields_json'])
            if fields:
                field_names = [f.get('name', '') for f in fields[:5]]  # First 5 fields
                text_parts.append(f"Fields: {', '.join(field_names)}")
        except:
            pass
    
    return ' '.join(text_parts)[:8000]  # Limit to 8000 chars

def update_record_with_embedding(config: Dict, record: Dict, embedding: List[float]):
    """Update record with embedding vector."""
    url = f"{config['search_endpoint']}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
    update_doc = {
        "@search.action": "mergeOrUpload",
        "screen_id": record['screen_id'],
        "summary_vector": embedding,
        "has_vector": True
    }
    
    body = {"value": [update_doc]}
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        return response.status_code in [200, 201]
    except Exception as e:
        return False

def process_screen_nodes_embeddings():
    """Process embeddings for screen nodes."""
    load_secrets()
    
    config = {
        'search_endpoint': os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
        'openai_endpoint': os.getenv('AZURE_OPENAI_ENDPOINT'),
        'openai_key': os.getenv('AZURE_OPENAI_KEY'),
        'embedding_deployment': os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT'),
        'embedding_model': 'text-embedding-3-large'
    }
    
    print("🔄 PROCESSING SCREEN NODES EMBEDDINGS")
    print("=" * 50)
    
    batch_size = 50
    total_processed = 0
    successful = 0
    failed = 0
    
    while True:
        # Get next batch of records
        records = get_records_for_embedding(config, batch_size)
        
        if not records:
            print("✅ All records processed!")
            break
        
        print(f"\n📦 Processing batch of {len(records)} records...")
        
        batch_successful = 0
        batch_failed = 0
        
        for i, record in enumerate(records):
            try:
                # Create text for embedding
                text = create_text_for_embedding(record)
                
                # Generate embedding
                embedding = get_embedding(config, text)
                
                if embedding:
                    # Update record
                    if update_record_with_embedding(config, record, embedding):
                        batch_successful += 1
                        successful += 1
                    else:
                        batch_failed += 1
                        failed += 1
                else:
                    batch_failed += 1
                    failed += 1
                
                # Progress indicator
                if (i + 1) % 10 == 0:
                    print(f"   Progress: {i + 1}/{len(records)}")
                
                # Rate limiting
                time.sleep(0.1)
                
            except Exception as e:
                batch_failed += 1
                failed += 1
                print(f"   Error processing record {record.get('screen_id', 'unknown')}: {str(e)}")
        
        total_processed += len(records)
        
        print(f"   Batch results: {batch_successful} successful, {batch_failed} failed")
        print(f"   Total progress: {total_processed} processed")
        
        # Brief pause between batches
        time.sleep(2)
    
    print("\n" + "=" * 50)
    print("🎉 SCREEN NODES PROCESSING COMPLETE!")
    print(f"   Total processed: {total_processed}")
    print(f"   Successful: {successful}")
    print(f"   Failed: {failed}")
    print(f"   Success rate: {(successful/total_processed*100):.1f}%" if total_processed > 0 else "0%")

if __name__ == "__main__":
    process_screen_nodes_embeddings()