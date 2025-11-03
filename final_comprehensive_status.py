import os
import requests
from datetime import datetime
from secrets_loader import load_secrets

load_secrets()
search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

headers = {
    'api-key': search_key,
    'Content-Type': 'application/json'
}

def get_total_docs(index_name):
    """Get total document count for an index"""
    try:
        count_url = f"{search_endpoint}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
        response = requests.get(count_url, headers=headers, timeout=30)
        return int(response.text) if response.status_code == 200 else 0
    except:
        return 0

def check_embeddings_with_has_vector(index_name):
    """Check embeddings using has_vector flag"""
    try:
        search_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        query = {
            "search": "*",
            "filter": "has_vector eq false",
            "count": True,
            "top": 0
        }
        response = requests.post(search_url, headers=headers, json=query, timeout=30)
        if response.status_code == 200:
            data = response.json()
            return data.get("@odata.count", 0)
        return None
    except:
        return None

def check_embeddings_with_vector_field(index_name, vector_field):
    """Check embeddings using vector field existence"""
    try:
        search_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        query = {
            "search": "*",
            "filter": f"{vector_field} eq null",
            "count": True,
            "top": 0
        }
        response = requests.post(search_url, headers=headers, json=query, timeout=30)
        if response.status_code == 200:
            data = response.json()
            return data.get("@odata.count", 0)
        return None
    except:
        return None

print(f"🌟 COMPLETE EMBEDDING STATUS REPORT")
print(f"📅 {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
print("=" * 80)

# All indexes in your system
all_indexes = [
    'new_cobol_screen_nodes',      # Already confirmed 100%
    'new-cobol-files',             # Already confirmed 100%
    'new_code_chunks',             # Already confirmed 100%
    'new_cobol_paragraphs',        # Already confirmed 100%
    'new_cobol_data_items',        # Already confirmed 100%
    'code-chunks',                 # Already confirmed 100%
    'new_cobol_symbol_refs',       # Just confirmed 100%
    'new_cobol_flow_edges_v2',
    'new_cobol_copybook_usage',
    'new_cobol_program_copybook_edges',
    'new_cobol_variable_usage',
    'cobol-xrefs',
    'new_cobol_name_aliases',
    'cobol-symbols',
    'new_cobol_menu_trees',
    'new_cobol_program_flows',
    'new_cobol_program_meta',
    'new_cobol_program_deps',
    'new_cobol_program_inventory',
    'new_cobol_copybook_meta',
    'new_cobol_calls',
    'new_cobol_ui_paths',
    'cobol-facts-v3',
    'cobol-facts-v3l'
]

total_docs_all = 0
total_embedded_all = 0
complete_indexes = 0
partial_indexes = 0
empty_indexes = 0

print("📊 DETAILED STATUS BY INDEX:")
print()

for index_name in all_indexes:
    total_docs = get_total_docs(index_name)
    
    if total_docs == 0:
        print(f"⭕ {index_name}: Empty or inaccessible")
        empty_indexes += 1
        continue
    
    # Try has_vector flag first
    pending_has_vector = check_embeddings_with_has_vector(index_name)
    
    if pending_has_vector is not None:
        # has_vector flag exists
        with_embeddings = total_docs - pending_has_vector
        completion_rate = (with_embeddings / total_docs * 100) if total_docs > 0 else 0
        
        if pending_has_vector == 0:
            status_icon = "✅"
            complete_indexes += 1
        else:
            status_icon = "🔄"
            partial_indexes += 1
            
        print(f"{status_icon} {index_name}")
        print(f"   📈 {total_docs:,} total, {with_embeddings:,} embedded, {pending_has_vector:,} pending")
        print(f"   📊 {completion_rate:.1f}% complete")
        
        total_docs_all += total_docs
        total_embedded_all += with_embeddings
        
    else:
        # Try vector field approach
        pending_vector = check_embeddings_with_vector_field(index_name, 'contentVector')
        
        if pending_vector is not None:
            with_embeddings = total_docs - pending_vector
            completion_rate = (with_embeddings / total_docs * 100) if total_docs > 0 else 0
            
            if pending_vector == 0:
                status_icon = "✅"
                complete_indexes += 1
            else:
                status_icon = "🔄"
                partial_indexes += 1
                
            print(f"{status_icon} {index_name}")
            print(f"   📈 {total_docs:,} total, {with_embeddings:,} embedded, {pending_vector:,} pending")
            print(f"   📊 {completion_rate:.1f}% complete")
            
            total_docs_all += total_docs
            total_embedded_all += with_embeddings
        else:
            # No embeddings detected
            print(f"❌ {index_name}")
            print(f"   📈 {total_docs:,} total, 0 embedded, {total_docs:,} pending")
            print(f"   📊 0.0% complete")
            
            total_docs_all += total_docs
            partial_indexes += 1
    
    print()

print("=" * 80)
print("🎯 FINAL COMPREHENSIVE SUMMARY:")
print(f"   📋 Total indexes checked: {len(all_indexes)}")
print(f"   ✅ Complete indexes: {complete_indexes}")
print(f"   🔄 Partial indexes: {partial_indexes}")
print(f"   ⭕ Empty/inaccessible: {empty_indexes}")
print()
print(f"   📈 Total documents: {total_docs_all:,}")
print(f"   ✅ Total embedded: {total_embedded_all:,}")
print(f"   ❌ Total pending: {(total_docs_all - total_embedded_all):,}")

if total_docs_all > 0:
    overall_completion = (total_embedded_all / total_docs_all * 100)
    print(f"   📊 Overall completion: {overall_completion:.1f}%")
    
    if overall_completion >= 99.9:
        print()
        print("🎉🎉🎉 MISSION ACCOMPLISHED! 🎉🎉🎉")
        print("🌟 COMPREHENSIVE EMBEDDING COVERAGE ACHIEVED!")
        print(f"✨ {total_embedded_all:,} documents with embeddings across your entire COBOL codebase!")
    elif overall_completion >= 95:
        print()
        print("🎯 EXCELLENT PROGRESS!")
        print("🚀 Ready for advanced RAG capabilities!")
    else:
        pending = total_docs_all - total_embedded_all
        print(f"   🔄 Work remaining: {pending:,} documents")