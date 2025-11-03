#!/usr/bin/env python3
"""
Robust Screen Nodes Embedding Processor
More resilient to interruptions and network issues
"""

import json
import time
import requests
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import sys
import os
from secrets_loader import load_secrets

def load_config():
    """Load configuration from environment variables after loading secrets"""
    load_secrets()
    
    config = {
        'search_endpoint': os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
        'openai_endpoint': os.getenv('AZURE_OPENAI_ENDPOINT'),
        'openai_key': os.getenv('AZURE_OPENAI_KEY'),
        'embedding_deployment': os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT'),
        'embedding_model': 'text-embedding-3-large'
    }
    
    return config

def get_embedding_with_retry(config, text, max_retries=3):
    """Get embedding with retry logic and better error handling"""
    
    url = f"{config['openai_endpoint']}/openai/deployments/{config['embedding_deployment']}/embeddings?api-version=2023-05-15"
    headers = {
        "Content-Type": "application/json",
        "api-key": config['openai_key']
    }
    data = {
        "input": text,
        "model": config.get('embedding_model', 'text-embedding-3-large')
    }
    
    for attempt in range(max_retries):
        try:
            print(f"      🔄 Embedding attempt {attempt + 1}/{max_retries}")
            response = requests.post(url, headers=headers, json=data, timeout=60)
            
            if response.status_code == 200:
                result = response.json()
                return result['data'][0]['embedding']
            elif response.status_code == 429:
                print("      ⏳ Rate limited, waiting 30 seconds...")
                time.sleep(30)
                continue
            else:
                print(f"      ⚠️ API Error {response.status_code}: {response.text}")
                if attempt < max_retries - 1:
                    time.sleep(10)
                    continue
                else:
                    return None
                    
        except requests.exceptions.RequestException as e:
            print(f"      ⚠️ Network error: {str(e)}")
            if attempt < max_retries - 1:
                print(f"      ⏳ Waiting 15 seconds before retry...")
                time.sleep(15)
                continue
            else:
                print(f"      ❌ Failed after {max_retries} attempts")
                return None
        except Exception as e:
            print(f"      ❌ Unexpected error: {str(e)}")
            return None
    
    return None

def process_screen_nodes_embeddings():
    """Process screen nodes embeddings with robust error handling"""
    
    print("🔄 ROBUST SCREEN NODES EMBEDDING PROCESSOR")
    print("=" * 50)
    
    # Load configuration
    config = load_config()
    
    # Initialize search client
    search_client = SearchClient(
        endpoint=config['search_endpoint'],
        index_name="new_cobol_screen_nodes",
        credential=AzureKeyCredential(config['search_key'])
    )
    
    batch_size = 10  # Smaller batches for more resilience
    processed_count = 0
    error_count = 0
    
    try:
        while True:
            # Get next batch of records without vectors
            try:
                search_results = search_client.search(
                    search_text="*",
                    filter="has_vector eq false",
                    top=batch_size,
                    select=["screen_id", "screen_name", "summary_text", "program_id", "screen_type", "fields_json"]
                )
                
                results_list = list(search_results)
                
                if not results_list:
                    print("\n✅ All records processed! No more records without vectors.")
                    break
                
                print(f"\n📦 Processing batch of {len(results_list)} records...")
                
            except Exception as e:
                print(f"❌ Error fetching batch: {str(e)}")
                error_count += 1
                if error_count > 5:
                    print("❌ Too many consecutive errors, stopping.")
                    break
                time.sleep(30)
                continue
            
            # Process each record in the batch
            batch_updates = []
            
            for i, result in enumerate(results_list):
                try:
                    print(f"   Progress: {i + 1}/{len(results_list)} - {result.get('screen_name', 'unnamed')}")
                    
                    # Create embedding text
                    embedding_text = f"COBOL Screen: {result.get('screen_name', 'unnamed')}"
                    if result.get('summary_text'):
                        embedding_text += f" - {result['summary_text']}"
                    if result.get('screen_type'):
                        embedding_text += f" (Type: {result['screen_type']})"
                    embedding_text += f" from program {result.get('program_id', 'unknown')}"
                    
                    # Get embedding with retry logic
                    embedding = get_embedding_with_retry(config, embedding_text)
                    
                    if embedding:
                        # Prepare update document
                        update_doc = {
                            "screen_id": result["screen_id"],
                            "summary_vector": embedding,
                            "has_vector": True
                        }
                        batch_updates.append(update_doc)
                        processed_count += 1
                        print(f"      ✅ Embedded successfully")
                    else:
                        print(f"      ❌ Failed to get embedding, skipping")
                        error_count += 1
                    
                    # Small delay between API calls
                    time.sleep(2)
                    
                except Exception as e:
                    print(f"      ❌ Error processing record: {str(e)}")
                    error_count += 1
                    continue
            
            # Upload batch updates
            if batch_updates:
                try:
                    print(f"   📤 Uploading {len(batch_updates)} updates...")
                    search_client.upload_documents(batch_updates)
                    print(f"   ✅ Batch uploaded successfully")
                    
                    # Reset error count after successful batch
                    error_count = 0
                    
                except Exception as e:
                    print(f"   ❌ Error uploading batch: {str(e)}")
                    error_count += 1
            
            # Progress report
            print(f"\n📊 Session Progress: {processed_count} records processed")
            
            # Check if too many errors
            if error_count > 10:
                print("⚠️ Too many errors encountered, taking a longer break...")
                time.sleep(60)
                error_count = 0
            
            # Brief pause between batches
            time.sleep(5)
            
    except KeyboardInterrupt:
        print(f"\n⏸️ Process interrupted by user")
        print(f"📊 Processed {processed_count} records in this session")
    except Exception as e:
        print(f"\n❌ Unexpected error: {str(e)}")
        print(f"📊 Processed {processed_count} records before error")
    
    print(f"\n🏁 EMBEDDING SESSION COMPLETE")
    print(f"   Records processed: {processed_count}")
    print(f"   Errors encountered: {error_count}")

if __name__ == "__main__":
    process_screen_nodes_embeddings()