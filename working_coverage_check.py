#!/usr/bin/env python3
"""
Simple Coverage Check Using Working Patterns

This script uses the same patterns as the working comprehensive_status_report.py
to check source code and vector coverage.
"""

import requests
import os
from typing import Dict, List, Any
from secrets_loader import load_secrets

def load_config():
    """Load configuration using the working pattern."""
    load_secrets()
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
    }

def get_index_details(config: Dict, index_name: str) -> Dict[str, Any]:
    """Get detailed index information using working API patterns."""
    print(f"\n🔍 ANALYZING {index_name.upper()}")
    print("=" * 50)
    
    # Get total count using the working pattern
    count_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
    
    try:
        total_response = requests.get(
            count_url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        total_count = int(total_response.text) if total_response.status_code == 200 else 0
    except Exception as e:
        print(f"❌ Error getting total count: {str(e)}")
        return {"index": index_name, "error": str(e)}
    
    print(f"📊 Total records: {total_count:,}")
    
    if total_count == 0:
        print("⚪ Index is empty")
        return {"index": index_name, "total": 0, "status": "EMPTY"}
    
    # Get vector count using the working pattern
    search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    vector_body = {
        'search': '*',
        'filter': 'has_vector eq true',
        'top': 0,
        'count': True
    }
    
    try:
        vector_response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=vector_body,
            timeout=30
        )
        vector_count = vector_response.json().get('@odata.count', 0) if vector_response.status_code == 200 else 0
    except Exception as e:
        print(f"⚠️  Error getting vector count: {str(e)}")
        vector_count = 0
    
    # Calculate coverage
    coverage = (vector_count / total_count * 100) if total_count > 0 else 0
    no_vector_count = total_count - vector_count
    
    print(f"✅ With vectors: {vector_count:,}")
    print(f"❌ Without vectors: {no_vector_count:,}")
    print(f"📈 Coverage: {coverage:.1f}%")
    
    # Get sample data to analyze source code coverage
    sample_body = {
        'search': '*',
        'top': 20,
        'select': 'program_id,program_name,source_program,from_program,to_program,file_path,source_file,filename'
    }
    
    unique_programs = set()
    unique_files = set()
    available_fields = []
    
    try:
        sample_response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=sample_body,
            timeout=30
        )
        
        if sample_response.status_code == 200:
            sample_data = sample_response.json()
            docs = sample_data.get('value', [])
            
            if docs:
                # Get available fields from first document
                first_doc = docs[0]
                available_fields = [k for k in first_doc.keys() if not k.startswith('@')]
                
                # Extract unique programs and files
                for doc in docs:
                    # Program identifiers
                    for field in ['program_id', 'program_name', 'source_program', 'from_program', 'to_program']:
                        if field in doc and doc[field]:
                            unique_programs.add(str(doc[field]))
                    
                    # File identifiers
                    for field in ['file_path', 'source_file', 'filename']:
                        if field in doc and doc[field]:
                            unique_files.add(str(doc[field]))
    
    except Exception as e:
        print(f"⚠️  Error getting sample data: {str(e)}")
    
    print(f"📁 Unique programs (sample): {len(unique_programs)}")
    print(f"📂 Unique files (sample): {len(unique_files)}")
    print(f"🏷️  Available fields: {len(available_fields)}")
    
    # Show sample data
    if unique_programs:
        sample_progs = sorted(list(unique_programs))[:5]
        print(f"📋 Sample programs: {', '.join(sample_progs)}")
    
    if unique_files:
        sample_files = sorted(list(unique_files))[:3]
        print(f"📋 Sample files: {', '.join([f.split('/')[-1] if '/' in f else f for f in sample_files])}")
    
    # Determine status
    if coverage >= 99.5:
        status = "COMPLETE"
    elif coverage >= 95:
        status = "NEARLY_COMPLETE"
    elif coverage >= 50:
        status = "PARTIAL"
    elif coverage > 0:
        status = "STARTED"
    else:
        status = "NO_VECTORS"
    
    print(f"🎯 Status: {status}")
    
    return {
        "index": index_name,
        "total": total_count,
        "vectors": vector_count,
        "no_vectors": no_vector_count,
        "coverage": coverage,
        "unique_programs": len(unique_programs),
        "unique_files": len(unique_files),
        "fields": len(available_fields),
        "status": status,
        "sample_programs": list(unique_programs)[:5],
        "sample_files": list(unique_files)[:3]
    }

def main():
    """Main function to check source code and vector coverage."""
    print("🔍 SOURCE CODE AND VECTOR COVERAGE CHECK")
    print("=" * 60)
    print("Using working API patterns from comprehensive_status_report.py")
    
    # Load config using working pattern
    config = load_config()
    
    if not config['search_endpoint'] or not config['search_key']:
        print("❌ Missing Azure Search configuration")
        return
    
    print(f"🔗 Azure Search: {config['search_endpoint']}")
    
    # Priority 1 indexes for detailed analysis
    priority1_indexes = [
        "new_cobol_program_meta",
        "new_cobol_program_flows", 
        "new_cobol_program_deps",
        "new_cobol_screen_nodes",
        "new_cobol_ui_paths"
    ]
    
    results = []
    
    # Analyze Priority 1 in detail
    print(f"\n🎯 PRIORITY 1 DETAILED ANALYSIS")
    for index_name in priority1_indexes:
        result = get_index_details(config, index_name)
        results.append(result)
    
    # Quick check of other indexes
    other_indexes = [
        "new_cobol_calls",
        "new_cobol_copybook_meta",
        "new_cobol_copybook_usage", 
        "new_cobol_data_items",
        "new_cobol_paragraphs",
        "new_cobol_flow_edges_v2",
        "new_cobol_symbol_refs"
    ]
    
    print(f"\n📊 QUICK CHECK OF OTHER INDEXES")
    print("-" * 40)
    
    for index_name in other_indexes:
        count_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
        try:
            response = requests.get(count_url, headers={'api-key': config['search_key']}, timeout=15)
            count = int(response.text) if response.status_code == 200 else 0
            print(f"   {index_name:25}: {count:,} records")
        except Exception as e:
            print(f"   {index_name:25}: Error - {str(e)}")
    
    # Summary of Priority 1
    print(f"\n" + "=" * 60)
    print("📊 PRIORITY 1 SUMMARY")
    print("=" * 60)
    
    print("INDEX                | RECORDS  | VECTORS  | COVERAGE | PROGRAMS | FILES | STATUS")
    print("-" * 85)
    
    total_records = 0
    total_vectors = 0
    
    for result in results:
        if "error" in result:
            print(f"{result['index']:18} | ERROR    |          |          |          |       | ERROR")
        elif result.get("total", 0) == 0:
            print(f"{result['index']:18} | 0        | 0        | 0.0%     | 0        | 0     | EMPTY")
        else:
            total_records += result["total"]
            total_vectors += result["vectors"]
            print(f"{result['index']:18} | {result['total']:8,} | {result['vectors']:8,} | {result['coverage']:6.1f}% | {result['unique_programs']:8} | {result['unique_files']:5} | {result['status']}")
    
    print("-" * 85)
    overall_coverage = (total_vectors / total_records * 100) if total_records > 0 else 0
    print(f"{'TOTAL':18} | {total_records:8,} | {total_vectors:8,} | {overall_coverage:6.1f}% |          |       |")
    
    # Source code insights
    print(f"\n🧩 SOURCE CODE INSIGHTS")
    print("-" * 40)
    
    all_programs = set()
    all_files = set()
    
    for result in results:
        if "sample_programs" in result:
            all_programs.update(result["sample_programs"])
        if "sample_files" in result:
            all_files.update(result["sample_files"])
    
    print(f"📁 Total unique programs found: {len(all_programs)}")
    print(f"📂 Total unique files found: {len(all_files)}")
    
    if all_programs:
        print(f"📋 Sample programs: {', '.join(sorted(list(all_programs))[:10])}")
    
    # Recommendations
    incomplete = [r for r in results if r.get("coverage", 0) < 100 and r.get("total", 0) > 0]
    if incomplete:
        print(f"\n🎯 COMPLETION RECOMMENDATIONS:")
        for result in incomplete:
            remaining = result.get("no_vectors", 0)
            print(f"   - {result['index']:20}: {remaining:,} records need vectors")

if __name__ == "__main__":
    main()