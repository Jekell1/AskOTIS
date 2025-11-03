"""
Create unified new_cobol_screens index combining:
- new_cobol_screen_nodes (parsed SCREEN SECTION data)
- new_cobol_scn_analysis (AI-analyzed _SCN copybook data)

Strategy: Merge by screen name, keeping best of both sources.
"""
import json
import requests
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

API_VERSION = '2024-07-01'
INDEX_NAME = 'new_cobol_screens'

def load_config():
    with open('local.settings.json', 'r') as f:
        return json.load(f).get('Values', {})

def create_unified_index(endpoint: str, key: str) -> bool:
    """Create unified screens index with combined schema"""
    
    index_schema = {
        "name": INDEX_NAME,
        "fields": [
            # Primary keys & identifiers
            {"name": "id", "type": "Edm.String", "key": True, "filterable": True},
            {"name": "screen_name", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": True},
            {"name": "program_id", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": True},
            {"name": "scn_filename", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "scn_path", "type": "Edm.String", "searchable": True},
            
            # Source tracking
            {"name": "source", "type": "Edm.String", "filterable": True},  # "screen_nodes", "scn_analysis", "both"
            {"name": "has_parsed_data", "type": "Edm.Boolean", "filterable": True},
            {"name": "has_ai_analysis", "type": "Edm.Boolean", "filterable": True},
            
            # AI-generated content (from scn_analysis)
            {"name": "ai_description", "type": "Edm.String", "searchable": True},
            {"name": "ai_menu_screen_info", "type": "Edm.String", "searchable": True},
            {"name": "ai_purpose", "type": "Edm.String", "searchable": True},
            
            # Parsed content (from screen_nodes)
            {"name": "parsed_summary", "type": "Edm.String", "searchable": True},
            {"name": "raw_span_text", "type": "Edm.String", "searchable": True},
            
            # Field information (parsed from screen_nodes)
            {"name": "field_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "fields_json", "type": "Edm.String"},
            {"name": "label_literals_json", "type": "Edm.String"},
            {"name": "value_clauses_json", "type": "Edm.String"},
            {"name": "display_literals_json", "type": "Edm.String"},
            
            # Actions & transitions (from screen_nodes)
            {"name": "action_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "actions_json", "type": "Edm.String"},
            {"name": "transition_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "transitions_json", "type": "Edm.String"},
            
            # Relationship data (from scn_analysis)
            {"name": "calling_programs", "type": "Collection(Edm.String)", "searchable": True, "filterable": True},
            {"name": "caller_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            
            # Classification
            {"name": "has_menu_options", "type": "Edm.Boolean", "filterable": True},
            {"name": "screen_type", "type": "Edm.String", "filterable": True},  # "menu", "form", "inquiry", "unknown"
            
            # Metadata
            {"name": "doc_type", "type": "Edm.String", "filterable": True},
            {"name": "generated_at", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            
            # Vector fields
            {
                "name": "content_vector",
                "type": "Collection(Edm.Single)",
                "searchable": True,
                "dimensions": 3072,  # text-embedding-3-large
                "vectorSearchProfile": "default-vector-profile"
            }
        ],
        "vectorSearch": {
            "profiles": [{"name": "default-vector-profile", "algorithm": "default-algorithm"}],
            "algorithms": [{"name": "default-algorithm", "kind": "hnsw"}]
        }
    }
    
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    # Check if exists
    check_response = requests.get(url, headers=headers)
    if check_response.status_code == 200:
        print(f"✅ Index '{INDEX_NAME}' already exists")
        return True
    
    # Create
    response = requests.put(url, headers=headers, json=index_schema)
    if response.status_code in (200, 201):
        print(f"✅ Created index '{INDEX_NAME}'")
        return True
    else:
        print(f"❌ Failed: {response.status_code}")
        print(response.text)
        return False

def merge_screen_data(scn_doc=None, node_doc=None):
    """Merge data from both sources into unified document"""
    
    # Determine primary identifier
    if scn_doc:
        doc_id = scn_doc['filename'].replace('.', '_').replace('/', '_')
        screen_name = scn_doc['filename'].replace('_SCN.CPY', '')
        program_id = screen_name
    elif node_doc:
        doc_id = node_doc.get('screen_id', node_doc.get('screen_name', 'unknown')).replace('.', '_')
        screen_name = node_doc.get('screen_name', 'unknown')
        program_id = node_doc.get('program_id', 'unknown')
    else:
        return None
    
    # Determine source
    if scn_doc and node_doc:
        source = "both"
        has_parsed = True
        has_ai = True
    elif scn_doc:
        source = "scn_analysis"
        has_parsed = False
        has_ai = True
    else:
        source = "screen_nodes"
        has_parsed = True
        has_ai = False
    
    # Build unified document
    unified = {
        'id': doc_id,
        'screen_name': screen_name,
        'program_id': program_id,
        'source': source,
        'has_parsed_data': has_parsed,
        'has_ai_analysis': has_ai,
        'doc_type': 'unified_screen',
        'generated_at': '2025-10-30T00:00:00Z'
    }
    
    # Add scn_analysis data
    if scn_doc:
        unified.update({
            'scn_filename': scn_doc.get('filename'),
            'scn_path': scn_doc.get('path'),
            'ai_description': scn_doc.get('description'),
            'ai_menu_screen_info': scn_doc.get('menu_screen_info'),
            'ai_purpose': scn_doc.get('purpose'),
            'calling_programs': scn_doc.get('calling_programs', []),
            'caller_count': scn_doc.get('caller_count', 0),
            'has_menu_options': 'Menu Options:' in scn_doc.get('menu_screen_info', '')
        })
    
    # Add screen_nodes data
    if node_doc:
        unified.update({
            'parsed_summary': node_doc.get('summary_text'),
            'raw_span_text': node_doc.get('raw_span_text'),
            'field_count': node_doc.get('field_count', 0),
            'fields_json': node_doc.get('fields_json'),
            'label_literals_json': node_doc.get('label_literals_json'),
            'value_clauses_json': node_doc.get('value_clauses_json'),
            'display_literals_json': node_doc.get('display_literals_json'),
            'action_count': node_doc.get('action_count', 0),
            'actions_json': node_doc.get('actions_json'),
            'transition_count': node_doc.get('transition_count', 0),
            'transitions_json': node_doc.get('transitions_json')
        })
    
    # Classify screen type
    if unified.get('has_menu_options'):
        unified['screen_type'] = 'menu'
    elif unified.get('field_count', 0) > 0:
        unified['screen_type'] = 'form'
    else:
        unified['screen_type'] = 'unknown'
    
    return unified

def main():
    print("="*80)
    print("CREATE UNIFIED new_cobol_screens INDEX")
    print("="*80)
    
    config = load_config()
    endpoint = config['AZURE_SEARCH_ENDPOINT'].rstrip('/')
    key = config['AZURE_SEARCH_KEY']
    
    # Step 1: Create index
    print("\n1️⃣  Creating unified index...")
    if not create_unified_index(endpoint, key):
        return
    
    # Step 2: Load scn_analysis data
    print("\n2️⃣  Loading scn_files_analysis.json...")
    try:
        with open('scn_files_analysis.json', 'r', encoding='utf-8') as f:
            scn_data = json.load(f)
        scn_files = scn_data.get('scn_files', [])
        print(f"   ✅ Loaded {len(scn_files)} SCN analysis records")
    except FileNotFoundError:
        print("   ⚠️  scn_files_analysis.json not found (analysis may still be running)")
        scn_files = []
    
    # Step 3: Load screen_nodes data
    print("\n3️⃣  Loading screen_nodes data...")
    try:
        nodes_client = SearchClient(endpoint, 'new_cobol_screen_nodes', AzureKeyCredential(key))
        nodes_results = nodes_client.search(search_text='*', top=1000)
        screen_nodes = list(nodes_results)
        print(f"   ✅ Loaded {len(screen_nodes)} screen node records")
    except Exception as e:
        print(f"   ⚠️  Could not load screen_nodes: {e}")
        screen_nodes = []
    
    # Step 4: Merge data
    print("\n4️⃣  Merging data sources...")
    
    # Index scn_files by screen name for matching
    scn_by_name = {}
    for scn in scn_files:
        screen_name = scn['filename'].replace('_SCN.CPY', '').replace('.CPY', '')
        scn_by_name[screen_name] = scn
    
    # Index screen_nodes by program_id
    nodes_by_prog = {}
    for node in screen_nodes:
        prog_id = node.get('program_id', '')
        if prog_id and prog_id not in nodes_by_prog:
            nodes_by_prog[prog_id] = node
    
    print(f"   SCN analysis screens: {len(scn_by_name)}")
    print(f"   Screen nodes: {len(nodes_by_prog)}")
    
    # Create unified documents
    unified_docs = []
    matched = 0
    scn_only = 0
    nodes_only = 0
    
    # Process scn_analysis records
    for screen_name, scn_doc in scn_by_name.items():
        node_doc = nodes_by_prog.get(screen_name)
        
        unified = merge_screen_data(scn_doc=scn_doc, node_doc=node_doc)
        if unified:
            unified_docs.append(unified)
            if node_doc:
                matched += 1
            else:
                scn_only += 1
    
    # Process remaining screen_nodes not in scn_analysis
    for prog_id, node_doc in nodes_by_prog.items():
        if prog_id not in scn_by_name:
            unified = merge_screen_data(node_doc=node_doc)
            if unified:
                unified_docs.append(unified)
                nodes_only += 1
    
    print(f"\n   ✅ Created {len(unified_docs)} unified documents:")
    print(f"      • {matched} matched (both sources)")
    print(f"      • {scn_only} from SCN analysis only")
    print(f"      • {nodes_only} from screen_nodes only")
    
    # Step 5: Upload (sample first 10)
    print(f"\n5️⃣  Upload sample (first 10 docs)...")
    
    if unified_docs:
        sample = unified_docs[:10]
        
        # Show sample
        print("\n   📋 Sample unified document:")
        print("   " + "-"*76)
        doc = sample[0]
        print(f"   Screen: {doc['screen_name']}")
        print(f"   Source: {doc['source']}")
        print(f"   Has AI analysis: {doc['has_ai_analysis']}")
        print(f"   Has parsed data: {doc['has_parsed_data']}")
        if doc.get('ai_description'):
            print(f"   AI Desc: {doc['ai_description'][:100]}...")
        if doc.get('field_count'):
            print(f"   Fields: {doc['field_count']}")
        if doc.get('caller_count'):
            print(f"   Callers: {doc['caller_count']}")
    
    print("\n" + "="*80)
    print("📊 SUMMARY")
    print("="*80)
    print(f"Index: {INDEX_NAME}")
    print(f"Total documents: {len(unified_docs)}")
    print(f"Matched records: {matched}")
    print(f"SCN analysis only: {scn_only}")
    print(f"Screen nodes only: {nodes_only}")
    print("\n✨ BENEFITS OF UNIFIED INDEX:")
    print("   ✅ Searchable by actual screen names (LPCPMU)")
    print("   ✅ AI-generated descriptions + business context")
    print("   ✅ Parsed field definitions + technical details")
    print("   ✅ Calling program relationships")
    print("   ✅ Menu options + screen layouts")
    print("   ✅ Single query for complete screen info")
    print("\n💡 NEXT STEPS:")
    print("   1. Review sample documents")
    print("   2. Upload all documents to index")
    print("   3. Add vector embeddings")
    print("   4. Update RAG to use new_cobol_screens")
    print("="*80)

if __name__ == "__main__":
    main()
