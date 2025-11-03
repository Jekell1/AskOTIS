#!/usr/bin/env python3
"""
CHECK SEARCH SERVICE CONFIGURATION
Checks Azure Search service tier and configures appropriate sharding.
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
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    }

def check_service_info(config: Dict):
    """Check Azure Search service configuration."""
    print("🔍 CHECKING AZURE SEARCH SERVICE")
    print("=" * 50)
    
    # Get service statistics
    url = f"{config['search_endpoint']}/servicestats?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code == 200:
            stats = response.json()
            print(f"Service: {config['search_endpoint']}")
            print(f"Counters: {json.dumps(stats.get('counters', {}), indent=2)}")
            print(f"Limits: {json.dumps(stats.get('limits', {}), indent=2)}")
            return stats
        else:
            print(f"❌ Error getting service stats: {response.status_code}")
            return None
            
    except Exception as e:
        print(f"❌ Exception: {e}")
        return None

def check_existing_indexes(config: Dict):
    """Check existing index configurations."""
    print(f"\n🔍 CHECKING EXISTING INDEX CONFIGURATIONS")
    print("=" * 50)
    
    url = f"{config['search_endpoint']}/indexes?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"❌ Error fetching indexes: {response.status_code}")
            return []
            
        indexes = response.json().get('value', [])
        
        for index in indexes:
            if 'cobol' in index['name'].lower():
                print(f"\n📋 Index: {index['name']}")
                
                # Check for vector fields
                vector_fields = []
                total_fields = len(index.get('fields', []))
                
                for field in index.get('fields', []):
                    if field.get('type') == 'Collection(Edm.Single)':
                        dimensions = field.get('dimensions', 'unknown')
                        vector_fields.append(f"{field['name']} ({dimensions}d)")
                
                print(f"   Fields: {total_fields} total")
                if vector_fields:
                    print(f"   Vector fields: {', '.join(vector_fields)}")
                else:
                    print(f"   Vector fields: None")
                
                # Get document count
                count_url = f"{config['search_endpoint']}/indexes/{index['name']}/docs/$count?api-version=2023-11-01"
                try:
                    count_response = requests.get(
                        count_url,
                        headers={'api-key': config['search_key']},
                        timeout=30
                    )
                    if count_response.status_code == 200:
                        doc_count = int(count_response.text)
                        print(f"   Documents: {doc_count:,}")
                        
                        # Estimate storage size
                        if vector_fields:
                            vector_dims = sum(int(vf.split('(')[1].split('d')[0]) for vf in vector_fields if '(' in vf)
                            vector_size_mb = (doc_count * vector_dims * 4) / (1024 * 1024)  # 4 bytes per float
                            print(f"   Est. vector storage: {vector_size_mb:.1f} MB")
                    else:
                        print(f"   Documents: Unable to get count")
                except:
                    print(f"   Documents: Error getting count")
        
        return indexes
        
    except Exception as e:
        print(f"❌ Exception checking indexes: {e}")
        return []

def recommend_sharding_strategy(doc_count: int, vector_dims: int):
    """Recommend sharding strategy based on size."""
    print(f"\n💡 SHARDING RECOMMENDATIONS")
    print("=" * 50)
    
    # Calculate storage requirements
    vector_size_mb = (doc_count * vector_dims * 4) / (1024 * 1024)
    print(f"Document count: {doc_count:,}")
    print(f"Vector dimensions: {vector_dims}")
    print(f"Estimated vector storage: {vector_size_mb:.1f} MB")
    
    # Azure Search limits (typical)
    # Standard tier: ~3GB per partition
    # Storage Basic: ~2GB per partition
    partition_limit_mb = 3000  # Conservative estimate
    
    if vector_size_mb > partition_limit_mb:
        recommended_partitions = int((vector_size_mb / partition_limit_mb) + 1)
        print(f"\n⚠️  Size exceeds single partition limit ({partition_limit_mb} MB)")
        print(f"🎯 Recommended: {recommended_partitions} partitions/shards")
        print(f"   - Split by document ID prefix")
        print(f"   - Each shard: ~{doc_count // recommended_partitions:,} documents")
        print(f"   - Each shard: ~{vector_size_mb / recommended_partitions:.1f} MB")
        return recommended_partitions
    else:
        print(f"✅ Size fits in single partition")
        return 1

def analyze_current_situation():
    """Analyze current Azure Search situation."""
    config = load_config()
    
    print("🔍 AZURE SEARCH ANALYSIS")
    print("=" * 60)
    
    # Check service configuration
    service_stats = check_service_info(config)
    
    # Check existing indexes
    indexes = check_existing_indexes(config)
    
    # Focus on screen_nodes and ui_paths
    target_indexes = ['new_cobol_screen_nodes', 'new_cobol_ui_paths']
    
    for index_name in target_indexes:
        print(f"\n🎯 ANALYZING {index_name}")
        print("-" * 40)
        
        # Get document count for this index
        count_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
        try:
            count_response = requests.get(
                count_url,
                headers={'api-key': config['search_key']},
                timeout=30
            )
            if count_response.status_code == 200:
                doc_count = int(count_response.text)
                print(f"Documents: {doc_count:,}")
                
                # Recommend sharding for 3072 dimensions
                recommend_sharding_strategy(doc_count, 3072)
            else:
                print(f"❌ Cannot get document count: {count_response.status_code}")
        except Exception as e:
            print(f"❌ Error getting document count: {e}")

if __name__ == '__main__':
    analyze_current_situation()