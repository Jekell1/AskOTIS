"""
Ingest only LPMENU_SCN.CPY to debug missing chunk issue.
"""
import json
import logging
from pathlib import Path
from ingestion_common import split_chunks, stable_hash
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from openai import AzureOpenAI

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def main():
    # Load config
    with open('local.settings.json') as f:
        cfg = json.load(f)['Values']
    
    endpoint = cfg['SEARCH_ENDPOINT']
    key = cfg['SEARCH_KEY']
    
    # Initialize clients
    search_client = SearchClient(endpoint, 'new_code_chunks', AzureKeyCredential(key))
    openai_client = AzureOpenAI(
        api_key=cfg['AZURE_OPENAI_KEY'],
        api_version=cfg.get('AZURE_OPENAI_API_VERSION', '2023-05-15'),
        azure_endpoint=cfg['AZURE_OPENAI_ENDPOINT']
    )
    
    # Read LPMENU_SCN.CPY
    lpmenu_path = Path('cobol_src/LIBLP/LPMENU_SCN.CPY')
    print(f"\n📁 Reading: {lpmenu_path}")
    
    with open(lpmenu_path, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    print(f"✅ Read {len(lines)} lines")
    
    # Generate chunks
    normalized_path = str(lpmenu_path).replace('\\', '/').lower()
    program_id = stable_hash([normalized_path])
    
    print(f"🔑 Program ID: {program_id}")
    print(f"🔑 Normalized path: {normalized_path}")
    
    chunks = split_chunks(lines, window=25)
    print(f"\n📦 Generated {len(chunks)} chunks:")
    
    docs = []
    for chunk in chunks:
        chunk_text = chunk['text']
        chunk_id = stable_hash([program_id, str(chunk['start_line']), str(chunk['end_line'])])
        
        print(f"  - Lines {chunk['start_line']}-{chunk['end_line']}: {len(chunk_text)} chars")
        if 'M A S T E R' in chunk_text:
            print(f"    ✨ Contains 'M A S T E R' text!")
        
        doc = {
            'chunk_id': chunk_id,
            'program_id': program_id,
            'path': normalized_path,
            'start_line': chunk['start_line'],
            'end_line': chunk['end_line'],
            'text': chunk_text,
            'text_vector': None  # Will embed below
        }
        docs.append(doc)
    
    # Embed all chunks
    print(f"\n🔢 Embedding {len(docs)} chunks...")
    texts = [d['text'] for d in docs]
    
    response = openai_client.embeddings.create(
        input=texts,
        model=cfg.get('AZURE_OPENAI_EMBEDDING_DEPLOYMENT', 'text-embedding-ada-002')
    )
    
    for i, doc in enumerate(docs):
        doc['text_vector'] = response.data[i].embedding
    
    print("✅ Embeddings complete")
    
    # Upload to Azure Search
    print(f"\n📤 Uploading {len(docs)} chunks to Azure Search...")
    
    result = search_client.merge_or_upload_documents(documents=docs)
    
    success_count = sum(1 for r in result if r.succeeded)
    failed_count = len(result) - success_count
    
    print(f"✅ Success: {success_count}")
    print(f"❌ Failed: {failed_count}")
    
    if failed_count > 0:
        for r in result:
            if not r.succeeded:
                print(f"   Failed chunk_id: {r.key}, Error: {r.error_message}")
    
    # Verify in index
    print(f"\n🔍 Verifying in index...")
    results = list(search_client.search(
        search_text="*",
        filter=f"program_id eq '{program_id}'",
        select=['chunk_id', 'start_line', 'end_line', 'path'],
        top=10
    ))
    
    print(f"Found {len(results)} chunks in index:")
    for r in results:
        print(f"  - Lines {r['start_line']}-{r['end_line']}")
    
    # Check for MASTER MENU
    master_results = list(search_client.search(
        search_text="M A S T E R",
        filter=f"program_id eq '{program_id}'",
        select=['start_line', 'end_line'],
        top=5
    ))
    
    if master_results:
        print(f"\n✅ 'M A S T E R' text found in {len(master_results)} chunks")
    else:
        print(f"\n❌ 'M A S T E R' text NOT FOUND in index")

if __name__ == '__main__':
    main()
