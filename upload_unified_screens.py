"""
Upload all unified screen documents to new_cobol_screens index
"""
import json
import requests
from create_unified_screens_index import load_config, merge_screen_data

API_VERSION = '2024-07-01'
INDEX_NAME = 'new_cobol_screens'

def upload_batch(endpoint, key, documents, batch_size=100):
    """Upload documents in batches with retry"""
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    total = len(documents)
    uploaded = 0
    failed = 0
    
    for i in range(0, total, batch_size):
        batch = documents[i:i+batch_size]
        
        payload = {
            'value': [
                {'@search.action': 'mergeOrUpload', **doc}
                for doc in batch
            ]
        }
        
        try:
            response = requests.post(url, headers=headers, json=payload, timeout=30)
            
            if response.status_code in (200, 201):
                uploaded += len(batch)
                end_idx = min(i + batch_size, total)
                print(f"   ✅ Uploaded {i+1}-{end_idx}/{total}")
            else:
                failed += len(batch)
                print(f"   ❌ Failed batch {i//batch_size + 1}: {response.status_code}")
                print(f"      {response.text[:200]}")
        except Exception as e:
            failed += len(batch)
            print(f"   ❌ Network error at batch {i//batch_size + 1}: {str(e)[:100]}")
            print(f"      Continuing...")
    
    return uploaded, failed

def main():
    print("="*80)
    print("UPLOAD ALL DOCUMENTS TO new_cobol_screens")
    print("="*80)
    
    config = load_config()
    endpoint = config['AZURE_SEARCH_ENDPOINT'].rstrip('/')
    key = config['AZURE_SEARCH_KEY']
    
    # Load scn_analysis data
    print("\n1️⃣  Loading scn_files_analysis.json...")
    with open('scn_files_analysis.json', 'r', encoding='utf-8') as f:
        scn_data = json.load(f)
    scn_files = scn_data.get('scn_files', [])
    print(f"   ✅ {len(scn_files)} SCN records")
    
    # Create unified documents (SCN only for now)
    print("\n2️⃣  Creating unified documents...")
    unified_docs = []
    
    for scn_doc in scn_files:
        unified = merge_screen_data(scn_doc=scn_doc, node_doc=None)
        if unified:
            unified_docs.append(unified)
    
    print(f"   ✅ {len(unified_docs)} documents prepared")
    
    # Upload
    print(f"\n3️⃣  Uploading to Azure Search...")
    uploaded, failed = upload_batch(endpoint, key, unified_docs)
    
    print("\n" + "="*80)
    print("📊 UPLOAD SUMMARY")
    print("="*80)
    print(f"Total documents: {len(unified_docs)}")
    print(f"✅ Uploaded: {uploaded}")
    print(f"❌ Failed: {failed}")
    
    if failed == 0:
        print("\n✨ SUCCESS! All documents uploaded.")
        print("\n📋 You can now search the index:")
        print("   python -c \"from azure.search.documents import SearchClient; ...")
        print("\n💡 Next steps:")
        print("   1. Test searches (python test_scn_search.py)")
        print("   2. Add vector embeddings (python backfill_screen_embeddings.py)")
        print("   3. Integrate with RAG chatbot")
    
    print("="*80)

if __name__ == "__main__":
    main()
