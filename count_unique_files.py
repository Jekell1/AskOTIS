#!/usr/bin/env python3
"""
Count unique COBOL files in the index
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def count_unique_cobol_files():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    index_name = "cobol-index"
    
    print("=== COUNTING UNIQUE COBOL FILES ===")
    
    try:
        # Get all unique repo_path values (files)
        search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
        
        # Use faceting to get unique file counts
        search_body = {
            "search": "*",
            "facets": ["repo_path,count:1000"],  # Get up to 1000 unique files
            "top": 0,  # Don't return documents, just facets
            "count": True
        }
        
        response = requests.post(search_url, headers=headers, json=search_body)
        
        if response.status_code == 200:
            result = response.json()
            total_docs = result.get('@odata.count', 0)
            facets = result.get('@search.facets', {})
            
            print(f"📊 Total documents in index: {total_docs:,}")
            
            if 'repo_path' in facets:
                file_facets = facets['repo_path']
                unique_files = len(file_facets)
                
                print(f"📁 Total unique COBOL files: {unique_files:,}")
                print(f"📈 Average lines per file: {total_docs / unique_files:.1f}")
                
                print(f"\n📋 Sample files (first 20):")
                for i, facet in enumerate(file_facets[:20]):
                    filename = facet['value'].split('/')[-1]  # Get just filename
                    count = facet['count']
                    print(f"   {i+1:2d}. {filename:<20} ({count:,} lines)")
                
                if unique_files > 20:
                    print(f"   ... and {unique_files - 20:,} more files")
                
                # Show some statistics
                line_counts = [facet['count'] for facet in file_facets]
                min_lines = min(line_counts)
                max_lines = max(line_counts)
                avg_lines = sum(line_counts) / len(line_counts)
                
                print(f"\n📊 File size statistics:")
                print(f"   Smallest file: {min_lines:,} lines")
                print(f"   Largest file: {max_lines:,} lines")
                print(f"   Average file: {avg_lines:.1f} lines")
                
            else:
                print("❌ Could not get file facets")
                
        else:
            print(f"❌ Search failed: {response.status_code}")
            print(response.text)
            
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    count_unique_cobol_files()
