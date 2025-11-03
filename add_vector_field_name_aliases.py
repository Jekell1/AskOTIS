"""Add vector field and has_vector field to new_cobol_name_aliases index.

This script adds:
  * alias_vector (Collection(Edm.Single), dimensions 3072)
  * has_vector (Edm.Boolean) for tracking embedding status

Usage:
  python add_vector_field_name_aliases.py
"""
import os, json, requests
from secrets_loader import load_secrets

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_name_aliases'
VECTOR_FIELD = 'alias_vector'
HAS_FIELD = 'has_vector'
DIMENSIONS = 3072

def resolve_search():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('❌ Missing SEARCH_ENDPOINT/SEARCH_KEY')
    return ep.rstrip('/'), key

def main():
    print("=" * 80)
    print("ADD VECTOR FIELDS TO NAME_ALIASES INDEX")
    print("=" * 80)
    print()
    
    load_secrets()
    ep, key = resolve_search()
    
    # Get current index definition
    print(f"📥 Fetching current index definition...")
    url = f"{ep}/indexes/{INDEX}?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    
    if r.status_code != 200:
        print(f"❌ Error fetching index: {r.status_code}")
        print(r.text[:500])
        return
    
    index_def = r.json()
    current_fields = index_def.get('fields', [])
    
    # Check if fields already exist
    existing_vector = any(f['name'] == VECTOR_FIELD for f in current_fields)
    existing_has = any(f['name'] == HAS_FIELD for f in current_fields)
    
    if existing_vector and existing_has:
        print(f"✅ Vector fields already exist!")
        print(f"   • {VECTOR_FIELD}")
        print(f"   • {HAS_FIELD}")
        return
    
    # Add vector profile if needed
    vector_search = index_def.get('vectorSearch') or {}
    profiles = vector_search.get('profiles') or []
    
    profile_name = 'name-alias-vector-profile'
    if not any(p['name'] == profile_name for p in profiles):
        print(f"📊 Adding vector search profile: {profile_name}")
        
        # Add algorithm config if needed
        algorithms = vector_search.get('algorithms') or []
        algo_name = 'name-alias-hnsw-config'
        if not any(a['name'] == algo_name for a in algorithms):
            algorithms.append({
                'name': algo_name,
                'kind': 'hnsw',
                'hnswParameters': {
                    'metric': 'cosine',
                    'm': 4,
                    'efConstruction': 400,
                    'efSearch': 500
                }
            })
            vector_search['algorithms'] = algorithms
        
        # Add profile
        profiles.append({
            'name': profile_name,
            'algorithm': algo_name
        })
        vector_search['profiles'] = profiles
        index_def['vectorSearch'] = vector_search
    
    # Add new fields
    new_fields = []
    
    if not existing_vector:
        print(f"➕ Adding vector field: {VECTOR_FIELD} ({DIMENSIONS} dimensions)")
        new_fields.append({
            'name': VECTOR_FIELD,
            'type': 'Collection(Edm.Single)',
            'searchable': True,
            'dimensions': DIMENSIONS,
            'vectorSearchProfile': profile_name
        })
    
    if not existing_has:
        print(f"➕ Adding tracking field: {HAS_FIELD}")
        new_fields.append({
            'name': HAS_FIELD,
            'type': 'Edm.Boolean',
            'filterable': True,
            'facetable': False
        })
    
    # Add new fields to index definition
    index_def['fields'].extend(new_fields)
    
    # Update index
    print(f"\n📤 Updating index...")
    r = requests.put(
        url,
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=index_def
    )
    
    if r.status_code in (200, 201, 204):
        print(f"✅ Successfully added vector fields!")
        print(f"\n📋 New fields:")
        for field in new_fields:
            print(f"   • {field['name']} ({field['type']})")
        print()
        print(f"🎯 Next steps:")
        print(f"   1. Run: python complete_name_aliases_embeddings.py --batch 128")
        print(f"   2. Monitor progress with coverage checks")
        print()
    else:
        print(f"❌ Error updating index: {r.status_code}")
        print(r.text[:1000])

if __name__ == '__main__':
    main()
