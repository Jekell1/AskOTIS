"""
Backfill vector embeddings for new_cobol_screens index
"""
import json
import os
from openai import AzureOpenAI
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import time

INDEX_NAME = 'new_cobol_screens'

def load_config():
    with open('local.settings.json', 'r') as f:
        return json.load(f).get('Values', {})

def get_embedding(client, text: str, deployment: str) -> list:
    """Generate embedding for text"""
    try:
        response = client.embeddings.create(
            input=text,
            model=deployment
        )
        return response.data[0].embedding
    except Exception as e:
        print(f"   ❌ Embedding error: {e}")
        return None

def main():
    print("="*80)
    print("BACKFILL EMBEDDINGS - new_cobol_screens")
    print("="*80)
    
    config = load_config()
    
    # Azure Search client
    search_client = SearchClient(
        config['AZURE_SEARCH_ENDPOINT'],
        INDEX_NAME,
        AzureKeyCredential(config['AZURE_SEARCH_KEY'])
    )
    
    # Azure OpenAI client
    openai_client = AzureOpenAI(
        api_key=config['AZURE_OPENAI_KEY'],
        api_version='2024-02-15-preview',
        azure_endpoint=config['AZURE_OPENAI_ENDPOINT']
    )
    
    # Use small model (1536 dimensions) to match index config
    embedding_deployment = config.get('AZURE_OPENAI_EMBEDDING_DEPLOYMENT_SMALL', 'text-embedding-3-small')
    print(f"   Using embedding model: {embedding_deployment}")
    
    print(f"\n📊 Fetching documents without embeddings...")
    
    # Get all documents (unified index fields)
    results = search_client.search(
        search_text='*',
        select=['id', 'scn_filename', 'screen_name', 'ai_description', 'ai_menu_screen_info', 'ai_purpose'],
        top=2000  # Increased to handle all docs
    )
    
    docs_to_process = []
    for doc in results:
        # Check if has embedding (would need to check vector field)
        docs_to_process.append(doc)
    
    total = len(docs_to_process)
    print(f"   Found {total} documents to process")
    
    if total == 0:
        print("✅ All documents already have embeddings!")
        return
    
    print(f"\n🔄 Generating embeddings...")
    
    updated = 0
    failed = 0
    
    for i, doc in enumerate(docs_to_process, 1):
        scn_name = doc.get('scn_filename', doc.get('screen_name', 'Unknown'))
        print(f"   {i}/{total}: {scn_name}", end='', flush=True)
        
        # Combine text for embedding (handle None values)
        parts = [
            doc.get('scn_filename', ''),
            doc.get('screen_name', ''),
            doc.get('ai_description', ''),
            doc.get('ai_menu_screen_info', ''),
            doc.get('ai_purpose', '')
        ]
        text_for_embedding = '\n'.join([p for p in parts if p])
        text_for_embedding = text_for_embedding[:8000]  # Limit size
        
        # Generate embedding
        embedding = get_embedding(openai_client, text_for_embedding, embedding_deployment)
        
        if embedding:
            # Update document
            try:
                update_doc = {
                    'id': doc['id'],
                    'content_vector': embedding
                }
                
                search_client.merge_or_upload_documents([{
                    '@search.action': 'merge',
                    **update_doc
                }])
                
                updated += 1
                print(" ✅")
            except Exception as e:
                print(f" ❌ Upload failed: {e}")
                failed += 1
        else:
            failed += 1
            print(" ❌")
        
        # Rate limiting
        if i % 10 == 0:
            time.sleep(1)
    
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    print(f"Total processed: {total}")
    print(f"✅ Successfully updated: {updated}")
    print(f"❌ Failed: {failed}")
    print("="*80)

if __name__ == "__main__":
    main()
