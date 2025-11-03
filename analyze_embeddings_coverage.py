#!/usr/bin/env python3
"""
Analyze embeddings coverage across all COBOL indexes.
Identifies which indexes have vector fields and their current population status.
"""

import requests
import os
import json
from typing import Dict, List, Tuple

def load_config():
    """Load Azure Search endpoint and key."""
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json', 'r', encoding='utf-8') as f:
                vals = json.load(f).get('Values', {})
        except Exception:
            pass
    
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    
    endpoint = first('AZURE_SEARCH_ENDPOINT', 'SearchEndpoint')
    key = first('AZURE_SEARCH_KEY', 'SearchKey')
    
    if not endpoint or not key:
        raise ValueError("Missing Azure Search configuration")
    
    return endpoint, key

def get_all_indexes(endpoint: str, key: str) -> List[str]:
    """Get list of all COBOL indexes."""
    url = f"{endpoint}/indexes?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': key},
            timeout=30
        )
        
        if response.status_code == 200:
            data = response.json()
            indexes = []
            for index_info in data.get('value', []):
                name = index_info.get('name', '')
                if name.startswith('new_cobol'):
                    indexes.append(name)
            return sorted(indexes)
        else:
            print(f"Error getting indexes: {response.status_code}")
            return []
            
    except Exception as e:
        print(f"Exception getting indexes: {e}")
        return []

def get_index_schema(endpoint: str, key: str, index_name: str) -> Dict:
    """Get the schema for an index."""
    url = f"{endpoint}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': key},
            timeout=30
        )
        
        if response.status_code == 200:
            return response.json()
        else:
            return {}
            
    except Exception as e:
        return {}

def get_embeddings_stats(endpoint: str, key: str, index_name: str, vector_fields: List[str]) -> Dict:
    """Get embeddings statistics for an index."""
    stats = {}
    
    # Get total count
    count_url = f"{endpoint}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
    try:
        response = requests.get(count_url, headers={'api-key': key}, timeout=30)
        total_count = int(response.text.strip()) if response.status_code == 200 else 0
    except:
        total_count = 0
    
    stats['total_records'] = total_count
    
    # Check embeddings status for each vector field
    for field in vector_fields:
        # Count records with vectors (has_vector=true or vector field is not null)
        search_url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        
        # Try has_vector field first
        body = {
            'search': '*',
            'filter': 'has_vector eq true',
            'top': 0
        }
        
        try:
            response = requests.post(
                search_url,
                headers={'api-key': key, 'Content-Type': 'application/json'},
                json=body,
                timeout=30
            )
            
            if response.status_code == 200:
                data = response.json()
                with_vectors = data.get('@odata.count', 0)
            else:
                with_vectors = 0
                
        except:
            with_vectors = 0
        
        # If has_vector field doesn't exist, try checking if vector field is populated
        if with_vectors == 0 and total_count > 0:
            # Sample a few records to see if vectors exist
            sample_body = {
                'search': '*',
                'select': field,
                'top': 10
            }
            
            try:
                response = requests.post(
                    search_url,
                    headers={'api-key': key, 'Content-Type': 'application/json'},
                    json=sample_body,
                    timeout=30
                )
                
                if response.status_code == 200:
                    data = response.json()
                    docs = data.get('value', [])
                    vector_found = any(doc.get(field) for doc in docs)
                    if vector_found:
                        # Estimate based on sample
                        with_vectors = total_count  # Assume all have vectors if sample does
                        
            except:
                pass
        
        stats[f'{field}_with_vectors'] = with_vectors
        stats[f'{field}_missing_vectors'] = max(0, total_count - with_vectors)
        if total_count > 0:
            stats[f'{field}_coverage_pct'] = round((with_vectors / total_count) * 100, 1)
        else:
            stats[f'{field}_coverage_pct'] = 0.0
    
    return stats

def main():
    print("=== EMBEDDINGS COVERAGE ANALYSIS ===")
    
    endpoint, key = load_config()
    
    # Get all COBOL indexes
    print("\n1. Discovering COBOL indexes...")
    indexes = get_all_indexes(endpoint, key)
    print(f"Found {len(indexes)} COBOL indexes")
    
    embeddings_summary = []
    
    # Analyze each index
    print("\n2. Analyzing embeddings coverage...")
    for index_name in indexes:
        print(f"\nAnalyzing {index_name}...")
        
        # Get schema to find vector fields
        schema = get_index_schema(endpoint, key, index_name)
        fields = schema.get('fields', [])
        
        vector_fields = []
        for field in fields:
            field_type = field.get('type', '')
            if field_type == 'Collection(Edm.Single)' or 'vector' in field.get('name', '').lower():
                vector_fields.append(field.get('name'))
        
        if not vector_fields:
            print(f"  No vector fields found")
            continue
            
        print(f"  Vector fields: {', '.join(vector_fields)}")
        
        # Get embeddings statistics
        stats = get_embeddings_stats(endpoint, key, index_name, vector_fields)
        
        for field in vector_fields:
            coverage_pct = stats.get(f'{field}_coverage_pct', 0.0)
            with_vectors = stats.get(f'{field}_with_vectors', 0)
            total = stats.get('total_records', 0)
            missing = stats.get(f'{field}_missing_vectors', 0)
            
            status = "✓ COMPLETE" if coverage_pct >= 99.5 else "✗ PARTIAL" if coverage_pct > 0 else "✗ MISSING"
            
            print(f"    {status} | {field}: {with_vectors:,}/{total:,} ({coverage_pct}%)")
            
            embeddings_summary.append({
                'index': index_name,
                'field': field,
                'total': total,
                'with_vectors': with_vectors,
                'missing': missing,
                'coverage_pct': coverage_pct,
                'status': status
            })
    
    # Summary report
    print("\n" + "="*80)
    print("EMBEDDINGS COVERAGE SUMMARY")
    print("="*80)
    
    incomplete_indexes = []
    
    for item in embeddings_summary:
        status_icon = "✓" if item['coverage_pct'] >= 99.5 else "✗"
        print(f"{status_icon} {item['index']:35} | {item['field']:20} | {item['with_vectors']:,}/{item['total']:,} ({item['coverage_pct']}%)")
        
        if item['coverage_pct'] < 99.5:
            incomplete_indexes.append(item)
    
    if incomplete_indexes:
        print(f"\nINCOMPLETE EMBEDDINGS ({len(incomplete_indexes)} fields):")
        for item in incomplete_indexes:
            print(f"  → {item['index']}.{item['field']}: {item['missing']:,} records need embeddings")
        
        print(f"\nNEXT STEPS:")
        print("1. Generate embeddings for records missing vectors")
        print("2. Update has_vector flags where applicable")
        print("3. Verify vector quality and dimensions")
    else:
        print("\n🎉 ALL INDEXES HAVE COMPLETE EMBEDDINGS! 🎉")

if __name__ == '__main__':
    main()