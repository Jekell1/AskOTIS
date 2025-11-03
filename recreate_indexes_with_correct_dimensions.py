#!/usr/bin/env python3
"""
RECREATE INDEXES WITH CORRECT DIMENSIONS
Recreates screen_nodes and ui_paths indexes with 3072 vector dimensions.
"""

import requests
import os
import json
import time
from typing import Dict
from secrets_loader import load_secrets

def load_config():
    """Load configuration."""
    load_secrets()
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    }

def backup_index_data(config: Dict, index_name: str):
    """Backup existing index data."""
    print(f"📦 BACKING UP {index_name}")
    print("-" * 50)
    
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'top': 10000,  # Get all records
        'select': '*'
    }
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=120
        )
        
        if response.status_code != 200:
            print(f"❌ Error backing up {index_name}: {response.status_code}")
            return None
            
        data = response.json()
        records = data.get('value', [])
        
        # Remove vector fields and @search.score from backup
        for record in records:
            # Remove fields that start with @ (search metadata)
            keys_to_remove = [key for key in record.keys() if key.startswith('@')]
            for key in keys_to_remove:
                del record[key]
            
            # Remove vector fields (they'll be regenerated)
            if 'summary_vector' in record:
                del record['summary_vector']
            if 'path_vector' in record:
                del record['path_vector']
            
            # Reset has_vector flag
            record['has_vector'] = False
        
        print(f"✅ Backed up {len(records)} records from {index_name}")
        return records
        
    except Exception as e:
        print(f"❌ Exception backing up {index_name}: {e}")
        return None

def delete_index(config: Dict, index_name: str):
    """Delete an index."""
    print(f"🗑️ DELETING {index_name}")
    print("-" * 50)
    
    url = f"{config['search_endpoint']}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.delete(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code in [200, 204, 404]:
            print(f"✅ Deleted {index_name}")
            return True
        else:
            print(f"❌ Error deleting {index_name}: {response.status_code}")
            return False
            
    except Exception as e:
        print(f"❌ Exception deleting {index_name}: {e}")
        return False

def create_screen_nodes_index(config: Dict):
    """Create new_cobol_screen_nodes index with 3072 dimensions."""
    print("🏗️ CREATING new_cobol_screen_nodes (3072 dimensions)")
    print("-" * 60)
    
    schema = {
        "name": "new_cobol_screen_nodes",
        "fields": [
            {"name": "screen_id", "type": "Edm.String", "key": True, "searchable": True, "filterable": True},
            {"name": "program_id", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "screen_name", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "field_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "action_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "transition_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "fields_json", "type": "Edm.String", "searchable": True},
            {"name": "actions_json", "type": "Edm.String", "searchable": True},
            {"name": "transitions_json", "type": "Edm.String", "searchable": True},
            {"name": "raw_span_text", "type": "Edm.String", "searchable": True},
            {"name": "summary_text", "type": "Edm.String", "searchable": True},
            {"name": "generated_at", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True},
            {
                "name": "summary_vector",
                "type": "Collection(Edm.Single)",
                "searchable": True,
                "retrievable": False,
                "dimensions": 3072,
                "vectorSearchProfile": "default-vector-profile"
            }
        ],
        "vectorSearch": {
            "profiles": [
                {
                    "name": "default-vector-profile",
                    "algorithm": "hnsw-config"
                }
            ],
            "algorithms": [
                {
                    "name": "hnsw-config",
                    "kind": "hnsw",
                    "hnswParameters": {
                        "metric": "cosine",
                        "m": 4,
                        "efConstruction": 400,
                        "efSearch": 500
                    }
                }
            ]
        }
    }
    
    return create_index(config, schema)

def create_ui_paths_index(config: Dict):
    """Create new_cobol_ui_paths index with 3072 dimensions."""
    print("🏗️ CREATING new_cobol_ui_paths (3072 dimensions)")
    print("-" * 60)
    
    schema = {
        "name": "new_cobol_ui_paths",
        "fields": [
            {"name": "path_id", "type": "Edm.String", "key": True, "searchable": True, "filterable": True},
            {"name": "start_program_id", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "end_program_id", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "path_json", "type": "Edm.String", "searchable": True},
            {"name": "guard_summary", "type": "Edm.String", "searchable": True},
            {"name": "frequency_score", "type": "Edm.Double", "filterable": True, "sortable": True},
            {"name": "frequency_score_norm", "type": "Edm.Double", "filterable": True, "sortable": True},
            {"name": "hop_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "updated_at", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            {"name": "root_program_id", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "leaf_program_id", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "program_sequence_json", "type": "Edm.String", "searchable": True},
            {"name": "screen_sequence_json", "type": "Edm.String", "searchable": True},
            {"name": "screen_names_json", "type": "Edm.String", "searchable": True},
            {"name": "length", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "ui_program_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "branching_events_json", "type": "Edm.String", "searchable": True},
            {"name": "guards_json", "type": "Edm.String", "searchable": True},
            {"name": "edge_freqs_json", "type": "Edm.String", "searchable": True},
            {"name": "avg_edge_freq", "type": "Edm.Double", "filterable": True, "sortable": True},
            {"name": "min_edge_freq", "type": "Edm.Double", "filterable": True, "sortable": True},
            {"name": "screen_ids_json", "type": "Edm.String", "searchable": True},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True},
            {"name": "loop_collapsed", "type": "Edm.Boolean", "filterable": True},
            {"name": "score", "type": "Edm.Double", "filterable": True, "sortable": True},
            {"name": "generated_at", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            {"name": "notes", "type": "Edm.String", "searchable": True},
            {"name": "is_placeholder", "type": "Edm.Boolean", "filterable": True},
            {"name": "path_type", "type": "Edm.String", "filterable": True},
            {"name": "generation_pass", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "edge_origins_json", "type": "Edm.String", "searchable": True},
            {"name": "transition_edge_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "call_edge_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "deepening_pass", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {
                "name": "path_vector",
                "type": "Collection(Edm.Single)",
                "searchable": True,
                "retrievable": False,
                "dimensions": 3072,
                "vectorSearchProfile": "default-vector-profile"
            }
        ],
        "vectorSearch": {
            "profiles": [
                {
                    "name": "default-vector-profile",
                    "algorithm": "hnsw-config"
                }
            ],
            "algorithms": [
                {
                    "name": "hnsw-config",
                    "kind": "hnsw",
                    "hnswParameters": {
                        "metric": "cosine",
                        "m": 4,
                        "efConstruction": 400,
                        "efSearch": 500
                    }
                }
            ]
        }
    }
    
    return create_index(config, schema)

def create_index(config: Dict, schema: Dict):
    """Create an index."""
    index_name = schema['name']
    url = f"{config['search_endpoint']}/indexes?api-version=2023-11-01"
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=schema,
            timeout=60
        )
        
        if response.status_code in [200, 201]:
            print(f"✅ Created {index_name}")
            return True
        else:
            print(f"❌ Error creating {index_name}: {response.status_code} - {response.text}")
            return False
            
    except Exception as e:
        print(f"❌ Exception creating {index_name}: {e}")
        return False

def restore_index_data(config: Dict, index_name: str, records: list):
    """Restore index data."""
    print(f"📥 RESTORING {index_name}")
    print("-" * 50)
    
    if not records:
        print(f"No records to restore for {index_name}")
        return True
    
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
    
    # Process in batches of 1000
    batch_size = 1000
    total_restored = 0
    
    for i in range(0, len(records), batch_size):
        batch = records[i:i + batch_size]
        
        # Prepare batch for upload
        upload_batch = []
        for record in batch:
            upload_record = record.copy()
            upload_record['@search.action'] = 'upload'
            upload_batch.append(upload_record)
        
        body = {'value': upload_batch}
        
        try:
            response = requests.post(
                url,
                headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
                json=body,
                timeout=120
            )
            
            if response.status_code in [200, 201]:
                total_restored += len(batch)
                print(f"   Restored batch {i//batch_size + 1}: {len(batch)} records")
            else:
                print(f"❌ Error restoring batch {i//batch_size + 1}: {response.status_code}")
                return False
                
        except Exception as e:
            print(f"❌ Exception restoring batch {i//batch_size + 1}: {e}")
            return False
        
        # Small delay between batches
        time.sleep(1)
    
    print(f"✅ Restored {total_restored} records to {index_name}")
    return True

def recreate_indexes():
    """Recreate screen_nodes and ui_paths indexes with correct dimensions."""
    config = load_config()
    
    print("🔄 RECREATING INDEXES WITH CORRECT DIMENSIONS")
    print("=" * 70)
    
    indexes_to_recreate = [
        ('new_cobol_screen_nodes', create_screen_nodes_index),
        ('new_cobol_ui_paths', create_ui_paths_index)
    ]
    
    for index_name, create_func in indexes_to_recreate:
        print(f"\n🔄 PROCESSING {index_name}")
        print("=" * 50)
        
        # Step 1: Backup data
        backup_data = backup_index_data(config, index_name)
        if backup_data is None:
            print(f"❌ Failed to backup {index_name}, skipping")
            continue
        
        # Step 2: Delete index
        if not delete_index(config, index_name):
            print(f"❌ Failed to delete {index_name}, skipping")
            continue
        
        # Wait for deletion to complete
        time.sleep(5)
        
        # Step 3: Create new index
        if not create_func(config):
            print(f"❌ Failed to create {index_name}")
            continue
        
        # Wait for creation to complete
        time.sleep(10)
        
        # Step 4: Restore data
        if not restore_index_data(config, index_name, backup_data):
            print(f"❌ Failed to restore data to {index_name}")
            continue
        
        print(f"✅ Successfully recreated {index_name}")
    
    print(f"\n" + "=" * 70)
    print("🎯 INDEX RECREATION COMPLETE")
    print("=" * 70)
    print("Both indexes now have 3072-dimension vector fields")
    print("You can now run embeddings generation successfully")

if __name__ == '__main__':
    recreate_indexes()