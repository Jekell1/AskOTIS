#!/usr/bin/env python3
"""
INDEX SCHEMA INSPECTOR
Checks the actual schema configuration for vector fields.
"""

import requests
import os
import json
from typing import Dict
from secrets_loader import load_secrets

def load_config():
    """Load configuration."""
    load_secrets()
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
    }

def inspect_index_schema(config: Dict, index_name: str):
    """Inspect the schema of an index."""
    url = f"{config['search_endpoint']}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code == 200:
            schema = response.json()
            
            print(f"\n📋 INDEX: {index_name}")
            print("=" * 60)
            
            fields = schema.get('fields', [])
            vector_fields = []
            key_field = None
            
            print(f"Total fields: {len(fields)}")
            print("\nFIELD DETAILS:")
            print("-" * 40)
            
            for field in fields:
                name = field.get('name')
                field_type = field.get('type')
                key = field.get('key', False)
                searchable = field.get('searchable', False)
                filterable = field.get('filterable', False)
                retrievable = field.get('retrievable', True)
                
                if key:
                    key_field = name
                
                # Check if this is a vector field
                if field_type == 'Collection(Edm.Single)' or 'vector' in name.lower():
                    vector_fields.append({
                        'name': name,
                        'type': field_type,
                        'retrievable': retrievable,
                        'searchable': searchable,
                        'filterable': filterable
                    })
                
                status_icons = []
                if key:
                    status_icons.append("🔑")
                if searchable:
                    status_icons.append("🔍")
                if filterable:
                    status_icons.append("🔽")
                if not retrievable:
                    status_icons.append("🚫")
                
                print(f"  {name:25} | {field_type:20} | {' '.join(status_icons)}")
            
            print(f"\n🔑 KEY FIELD: {key_field}")
            print(f"\n📊 VECTOR FIELDS FOUND: {len(vector_fields)}")
            
            if vector_fields:
                print("VECTOR FIELD DETAILS:")
                print("-" * 40)
                for vf in vector_fields:
                    print(f"  📈 {vf['name']:25}")
                    print(f"     Type: {vf['type']}")
                    print(f"     Retrievable: {vf['retrievable']}")
                    print(f"     Searchable: {vf['searchable']}")
                    print(f"     Filterable: {vf['filterable']}")
                    print()
            else:
                print("❌ NO VECTOR FIELDS FOUND!")
            
            return {
                'key_field': key_field,
                'vector_fields': vector_fields,
                'total_fields': len(fields)
            }
        else:
            print(f"❌ Error fetching schema: {response.status_code}")
            return None
            
    except Exception as e:
        print(f"❌ Exception fetching schema: {e}")
        return None

def inspect_priority_schemas():
    """Inspect schemas for Priority 1 indexes."""
    config = load_config()
    
    print("🔍 INDEX SCHEMA INSPECTION")
    print("=" * 60)
    print(f"Azure Search: {config['search_endpoint']}")
    
    # Priority 1 indexes
    indexes = [
        'new_cobol_program_flows',
        'new_cobol_program_meta', 
        'new_cobol_program_deps',
        'new_cobol_screen_nodes',
        'new_cobol_ui_paths'
    ]
    
    vector_summary = []
    
    for index in indexes:
        result = inspect_index_schema(config, index)
        if result:
            vector_summary.append({
                'index': index,
                'vector_fields': [vf['name'] for vf in result['vector_fields']],
                'key_field': result['key_field']
            })
    
    print(f"\n" + "=" * 60)
    print("📊 VECTOR FIELDS SUMMARY")
    print("=" * 60)
    
    for summary in vector_summary:
        print(f"\n🗂️ {summary['index']}")
        print(f"   Key: {summary['key_field']}")
        if summary['vector_fields']:
            print(f"   Vectors: {', '.join(summary['vector_fields'])}")
        else:
            print(f"   Vectors: ❌ NONE FOUND")

if __name__ == '__main__':
    inspect_priority_schemas()