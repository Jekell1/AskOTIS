import os
import requests
import json
from datetime import datetime
from secrets_loader import load_secrets

# Load configuration
load_secrets()
search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

headers = {
    'api-key': search_key,
    'Content-Type': 'application/json'
}

print(f"🔍 COMPREHENSIVE AZURE SEARCH STATUS")
print(f"📅 {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
print(f"🌐 Endpoint: {search_endpoint}")
print("=" * 80)

def get_index_count(index_name):
    """Get document count for an index"""
    try:
        count_url = f"{search_endpoint}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
        response = requests.get(count_url, headers=headers)
        if response.status_code == 200:
            return int(response.text)
        else:
            return f"Error: {response.status_code}"
    except Exception as e:
        return f"Error: {str(e)}"

def get_sample_document(index_name):
    """Get a sample document to show structure"""
    try:
        search_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        body = {'search': '*', 'top': 1, 'select': '*'}
        response = requests.post(search_url, headers=headers, json=body)
        if response.status_code == 200:
            result = response.json()
            if result.get('value'):
                return result['value'][0]
        return None
    except Exception as e:
        return None

def check_embeddings_status(index_name):
    """Check embedding status for indexes that have vector fields"""
    try:
        # Check for documents without embeddings
        search_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        
        # Try different embedding field names
        embedding_fields = ['embedding', 'contentVector', 'vector']
        
        for field in embedding_fields:
            no_embedding_query = {
                "search": "*",
                "filter": f"not search.ismatch('*', '{field}')",
                "count": True,
                "top": 0
            }
            response = requests.post(search_url, headers=headers, json=no_embedding_query)
            if response.status_code == 200:
                data = response.json()
                no_embedding_count = data.get("@odata.count", 0)
                
                # Get total count
                total_count = get_index_count(index_name)
                if isinstance(total_count, int):
                    with_embedding_count = total_count - no_embedding_count
                    completion_percentage = (with_embedding_count / total_count * 100) if total_count > 0 else 0
                    return {
                        'field': field,
                        'total': total_count,
                        'with_embeddings': with_embedding_count,
                        'without_embeddings': no_embedding_count,
                        'completion_rate': completion_percentage
                    }
        
        return None
    except Exception as e:
        return f"Error: {str(e)}"

# Get list of all indexes
try:
    indexes_url = f"{search_endpoint}/indexes?api-version=2023-11-01"
    indexes_response = requests.get(indexes_url, headers=headers)
    
    if indexes_response.status_code != 200:
        print(f"❌ Failed to get indexes: {indexes_response.status_code} - {indexes_response.text}")
        exit(1)
    
    indexes_data = indexes_response.json()
    indexes = indexes_data.get('value', [])
    
    print(f"📊 FOUND {len(indexes)} INDEXES")
    print()
    
    for i, index in enumerate(indexes, 1):
        index_name = index['name']
        print(f"📋 INDEX {i}: {index_name}")
        print(f"   {'─' * 60}")
        
        # Get document count
        doc_count = get_index_count(index_name)
        print(f"   📈 Document count: {doc_count:,}" if isinstance(doc_count, int) else f"   📈 Document count: {doc_count}")
        
        # Check embedding status
        embedding_status = check_embeddings_status(index_name)
        if embedding_status and isinstance(embedding_status, dict):
            print(f"   🎯 Embedding field: {embedding_status['field']}")
            print(f"   ✅ With embeddings: {embedding_status['with_embeddings']:,}")
            print(f"   ❌ Without embeddings: {embedding_status['without_embeddings']:,}")
            print(f"   📊 Completion rate: {embedding_status['completion_rate']:.1f}%")
        elif embedding_status:
            print(f"   🎯 Embedding check: {embedding_status}")
        else:
            print(f"   🎯 Embeddings: No vector fields detected")
        
        # Get sample document structure
        sample_doc = get_sample_document(index_name)
        if sample_doc:
            print(f"   📝 Sample document fields:")
            for field_name in sorted(sample_doc.keys()):
                field_value = sample_doc[field_name]
                if isinstance(field_value, list) and len(field_value) > 0:
                    if isinstance(field_value[0], (int, float)):
                        print(f"      🔢 {field_name}: [vector with {len(field_value)} dimensions]")
                    else:
                        print(f"      📋 {field_name}: [list with {len(field_value)} items]")
                elif isinstance(field_value, str):
                    preview = field_value[:50] + "..." if len(field_value) > 50 else field_value
                    print(f"      📄 {field_name}: {preview}")
                elif field_value is None:
                    print(f"      ⭕ {field_name}: null")
                else:
                    print(f"      🔸 {field_name}: {type(field_value).__name__}")
        else:
            print(f"   📝 Sample document: Could not retrieve")
        
        print()
    
    # Summary
    print("=" * 80)
    print("📊 SUMMARY")
    total_docs = 0
    total_with_embeddings = 0
    indexes_with_embeddings = 0
    
    for index in indexes:
        index_name = index['name']
        doc_count = get_index_count(index_name)
        if isinstance(doc_count, int):
            total_docs += doc_count
        
        embedding_status = check_embeddings_status(index_name)
        if embedding_status and isinstance(embedding_status, dict):
            total_with_embeddings += embedding_status['with_embeddings']
            if embedding_status['completion_rate'] > 0:
                indexes_with_embeddings += 1
    
    print(f"   📈 Total documents across all indexes: {total_docs:,}")
    print(f"   🎯 Total documents with embeddings: {total_with_embeddings:,}")
    print(f"   📋 Indexes with embeddings: {indexes_with_embeddings}/{len(indexes)}")
    
    if total_docs > 0:
        overall_completion = (total_with_embeddings / total_docs * 100)
        print(f"   📊 Overall embedding completion: {overall_completion:.1f}%")

except Exception as e:
    print(f"❌ Error: {str(e)}")