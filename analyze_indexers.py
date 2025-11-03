#!/usr/bin/env python3
"""
Analyze which indexers are actually needed
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def analyze_indexers():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== INDEXER ANALYSIS ===")
    
    # Get all indexers
    indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
    indexers_response = requests.get(indexers_url, headers=headers)
    
    if indexers_response.status_code == 200:
        indexers = indexers_response.json().get('value', [])
        
        print(f"Found {len(indexers)} total indexers\n")
        
        cobol_indexers = []
        for indexer in indexers:
            name = indexer.get('name', '')
            if 'cobol' in name.lower():
                cobol_indexers.append(indexer)
        
        print(f"COBOL-related indexers: {len(cobol_indexers)}\n")
        
        for indexer in cobol_indexers:
            name = indexer.get('name', '')
            data_source = indexer.get('dataSourceName', 'unknown')
            target_index = indexer.get('targetIndexName', 'unknown')
            schedule = indexer.get('schedule')
            
            print(f"📋 {name}")
            print(f"   Data Source: {data_source}")
            print(f"   Target Index: {target_index}")
            print(f"   Schedule: {schedule if schedule else 'Manual only'}")
            
            # Get data source details
            if data_source != 'unknown':
                ds_url = f'{endpoint}/datasources/{data_source}?api-version=2023-07-01-preview'
                ds_response = requests.get(ds_url, headers=headers)
                
                if ds_response.status_code == 200:
                    ds_config = ds_response.json()
                    ds_type = ds_config.get('type', 'unknown')
                    container = ds_config.get('container', {})
                    
                    print(f"   📂 Data Source Type: {ds_type}")
                    if container:
                        print(f"   📁 Container: {container.get('name', 'unknown')}")
                        print(f"   🔍 Query/Path: {container.get('query', 'none')}")
                        
                        # Determine file types being indexed
                        query = container.get('query', '') or ''
                        if '.jsonl' in query.lower() or 'json' in query.lower():
                            print("   📄 File Types: JSONL files")
                        elif '.cbl' in query.lower() or 'cobol' in query.lower():
                            print("   📄 File Types: COBOL (.CBL) files")
                        elif query == '':
                            print("   📄 File Types: All files in container")
                        else:
                            print(f"   📄 File Types: Unknown from path '{query}'")
            
            # Get indexer status
            status_url = f'{endpoint}/indexers/{name}/status?api-version=2023-07-01-preview'
            status_response = requests.get(status_url, headers=headers)
            
            if status_response.status_code == 200:
                status = status_response.json()
                last_result = status.get('lastResult', {})
                current_status = last_result.get('status', 'unknown')
                items_processed = last_result.get('itemsProcessed', 0)
                
                print(f"   📊 Status: {current_status}")
                print(f"   📈 Items Processed: {items_processed:,}" if isinstance(items_processed, int) else f"   📈 Items Processed: {items_processed}")
            
            print()
    
    print("=== RECOMMENDATIONS ===")
    
    # Check what files we actually have in blob storage
    print("\nLet me check what files are actually in your blob storage...")
    
    # Get data sources to understand what we're working with
    ds_url = f'{endpoint}/datasources?api-version=2023-07-01-preview'
    ds_response = requests.get(ds_url, headers=headers)
    
    if ds_response.status_code == 200:
        data_sources = ds_response.json().get('value', [])
        
        for ds in data_sources:
            name = ds.get('name', '')
            if 'cobol' in name.lower():
                container = ds.get('container', {})
                container_name = container.get('name', 'unknown')
                query = container.get('query', 'none') or 'entire container'
                
                print(f"📦 Data Source: {name}")
                print(f"   Container: {container_name}")
                print(f"   Path: {query}")
                
                # Analyze what this data source is for
                query_lower = query.lower() if query and query != 'none' else ''
                if 'jsonl' in query_lower or 'json' in query_lower:
                    print("   ❓ Purpose: Indexes JSONL files (converted COBOL)")
                    print("   💭 Question: Do we need JSONL files if we have original .CBL files?")
                elif '.cbl' in query_lower or 'source' in query_lower:
                    print("   ✅ Purpose: Indexes original COBOL (.CBL) files")
                    print("   💭 This is essential - we need the original source code")
                elif query == 'entire container':
                    print("   ⚠️  Purpose: Indexes ALL files in container")
                    print("   💭 This may include both .CBL and .JSONL files")
    
    print("\n=== ANSWER: Do we need the JSON indexer? ===")
    print("🤔 It depends on your workflow:")
    print()
    print("✅ KEEP JSON indexer if:")
    print("   - JSONL files have additional processed data (symbols, calls, etc.)")
    print("   - JSONL files have better structure for search")
    print("   - You use the JSONL format for specific analysis")
    print()
    print("❌ REMOVE JSON indexer if:")
    print("   - JSONL files are just converted .CBL files")
    print("   - Original .CBL files contain all the information you need")
    print("   - You want to eliminate duplicate/redundant data")
    print()
    print("🚨 PROBLEM WE JUST SOLVED:")
    print("   - The JSON indexer was running every 4 hours")
    print("   - It was creating millions of duplicate documents")
    print("   - We deleted it to stop the runaway indexing")
    print()
    print("💡 RECOMMENDATION:")
    print("   - Start with just the .CBL indexer (cobol-indexer)")
    print("   - Test if search works well with original COBOL files")
    print("   - Only add JSON indexer back if you need specific JSONL features")

if __name__ == "__main__":
    analyze_indexers()
