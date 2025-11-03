#!/usr/bin/env python3
"""
Simple Source Code and Vector Coverage Check

This script checks the current COBOL indexes for source code and vector coverage
using the working patterns from comprehensive_status_report.py
"""

import json
import requests
import time
from typing import Dict, List, Any

# Azure Search configuration (using the working endpoint from status report)
search_service_name = "az-use1-ai-search"
search_key = "nC5NRGV8FzDK7eOPa7eaQM94h6EZ3Ys66XlmMJq8h3AzSeAhpqxo"
endpoint = f"https://{search_service_name}.search.windows.net"

# Current COBOL indexes based on the working status report
COBOL_INDEXES = [
    # Priority 1 (Core Indexes)
    "new_cobol_program_meta",
    "new_cobol_program_flows", 
    "new_cobol_program_deps",
    "new_cobol_screen_nodes",
    "new_cobol_ui_paths",
    
    # Priority 2 (Medium Volume)
    "new_cobol_calls",
    "new_cobol_copybook_meta",
    "new_cobol_copybook_usage", 
    "new_cobol_data_items",
    "new_cobol_paragraphs",
    
    # Priority 3 (High Volume)
    "new_cobol_flow_edges_v2",
    "new_cobol_symbol_refs"
]

def check_index_details(index_name: str) -> Dict[str, Any]:
    """Check detailed coverage and source code information for an index."""
    print(f"\n🔍 DETAILED ANALYSIS: {index_name.upper()}")
    print("=" * 60)
    
    url = f"{endpoint}/indexes/{index_name}/docs/search"
    headers = {
        "Content-Type": "application/json",
        "api-key": search_key
    }
    
    try:
        # Get total count
        total_payload = {"search": "*", "count": True, "top": 0}
        total_response = requests.post(url, headers=headers, json=total_payload, timeout=30)
        
        if total_response.status_code != 200:
            print(f"❌ Failed to access index: {total_response.status_code}")
            return {"index": index_name, "error": f"HTTP {total_response.status_code}"}
        
        total_result = total_response.json()
        total_count = total_result.get("@odata.count", 0)
        print(f"📊 Total records: {total_count:,}")
        
        if total_count == 0:
            print("⚪ Index is empty")
            return {"index": index_name, "total": 0, "status": "EMPTY"}
        
        # Get vector coverage
        vector_payload = {"search": "*", "filter": "has_vector eq true", "count": True, "top": 0}
        vector_response = requests.post(url, headers=headers, json=vector_payload, timeout=30)
        
        vector_count = 0
        if vector_response.status_code == 200:
            vector_result = vector_response.json()
            vector_count = vector_result.get("@odata.count", 0)
        
        no_vector_count = total_count - vector_count
        coverage = (vector_count / total_count * 100) if total_count > 0 else 0
        
        print(f"✅ With vectors: {vector_count:,}")
        print(f"❌ Without vectors: {no_vector_count:,}")
        print(f"📈 Coverage: {coverage:.1f}%")
        
        # Get sample data to analyze source code coverage
        sample_payload = {"search": "*", "top": 20}
        sample_response = requests.post(url, headers=headers, json=sample_payload, timeout=30)
        
        unique_programs = set()
        unique_files = set()
        sample_fields = []
        
        if sample_response.status_code == 200:
            sample_result = sample_response.json()
            docs = sample_result.get("value", [])
            
            if docs:
                # Analyze first document for available fields
                first_doc = docs[0]
                sample_fields = [k for k in first_doc.keys() if not k.startswith("@")]
                
                # Extract program and file information
                for doc in docs:
                    # Common program identifier fields
                    for prog_field in ["program_id", "program_name", "source_program", "from_program", "to_program"]:
                        if prog_field in doc and doc[prog_field]:
                            unique_programs.add(str(doc[prog_field]))
                    
                    # Common file identifier fields  
                    for file_field in ["file_path", "source_file", "filename", "file_name"]:
                        if file_field in doc and doc[file_field]:
                            unique_files.add(str(doc[file_field]))
        
        print(f"📁 Unique programs (sample): {len(unique_programs)}")
        print(f"📂 Unique files (sample): {len(unique_files)}")
        print(f"🏷️  Available fields: {len(sample_fields)}")
        
        # Show some sample programs and files
        if unique_programs:
            sample_progs = list(unique_programs)[:5]
            print(f"📋 Sample programs: {', '.join(sample_progs)}")
        
        if unique_files:
            sample_files = list(unique_files)[:3]
            print(f"📋 Sample files: {', '.join(sample_files)}")
        
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
            "fields": len(sample_fields),
            "status": status
        }
        
    except Exception as e:
        print(f"❌ Error: {str(e)}")
        return {"index": index_name, "error": str(e)}

def generate_summary(results: List[Dict]) -> None:
    """Generate comprehensive summary."""
    print("\n" + "=" * 80)
    print("📊 SOURCE CODE AND VECTOR COVERAGE SUMMARY")
    print("=" * 80)
    
    # Summary table
    print("INDEX                    | RECORDS  | VECTORS  | COVERAGE | PROGRAMS | FILES | STATUS")
    print("-" * 95)
    
    total_records = 0
    total_vectors = 0
    
    for result in results:
        if "error" in result:
            print(f"{result['index']:22} | ERROR    |          |          |          |       | ERROR")
        elif result.get("total", 0) == 0:
            print(f"{result['index']:22} | 0        | 0        | 0.0%     | 0        | 0     | EMPTY")
        else:
            total_records += result["total"]
            total_vectors += result["vectors"]
            print(f"{result['index']:22} | {result['total']:8,} | {result['vectors']:8,} | {result['coverage']:6.1f}% | {result['unique_programs']:8} | {result['unique_files']:5} | {result['status']}")
    
    print("-" * 95)
    overall_coverage = (total_vectors / total_records * 100) if total_records > 0 else 0
    print(f"{'TOTAL':22} | {total_records:8,} | {total_vectors:8,} | {overall_coverage:6.1f}% |          |       |")
    
    # Category breakdown
    complete = [r for r in results if r.get("status") == "COMPLETE"]
    nearly_complete = [r for r in results if r.get("status") == "NEARLY_COMPLETE"]
    partial = [r for r in results if r.get("status") == "PARTIAL"]
    started = [r for r in results if r.get("status") == "STARTED"]
    no_vectors = [r for r in results if r.get("status") == "NO_VECTORS"]
    empty = [r for r in results if r.get("status") == "EMPTY"]
    errors = [r for r in results if "error" in r]
    
    print(f"\n📈 STATUS BREAKDOWN:")
    print(f"   ✅ Complete: {len(complete)} indexes")
    print(f"   🟡 Nearly Complete: {len(nearly_complete)} indexes") 
    print(f"   🟠 Partial: {len(partial)} indexes")
    print(f"   🔴 Started: {len(started)} indexes")
    print(f"   ⚫ No Vectors: {len(no_vectors)} indexes")
    print(f"   ⚪ Empty: {len(empty)} indexes")
    print(f"   ❌ Errors: {len(errors)} indexes")
    
    # Recommendations
    print(f"\n🎯 RECOMMENDATIONS:")
    
    needs_work = [r for r in results if r.get("coverage", 0) < 100 and r.get("total", 0) > 0]
    if needs_work:
        print(f"   📋 Indexes needing completion:")
        for result in sorted(needs_work, key=lambda x: x.get("coverage", 0)):
            remaining = result.get("no_vectors", 0)
            print(f"     - {result['index']:25}: {remaining:,} records remaining ({result.get('coverage', 0):.1f}% done)")
    
    if complete:
        complete_records = sum(r["total"] for r in complete)
        print(f"   🎉 {len(complete)} indexes fully complete with {complete_records:,} total records!")

def main():
    """Main function to check all indexes."""
    print("🔍 SOURCE CODE AND VECTOR COVERAGE ANALYSIS")
    print("=" * 80)
    print("Checking all COBOL indexes for data and vector coverage...")
    
    results = []
    
    # Analyze priority 1 first
    priority1 = ["new_cobol_program_meta", "new_cobol_program_flows", "new_cobol_program_deps", 
                 "new_cobol_screen_nodes", "new_cobol_ui_paths"]
    
    print("\n🎯 PRIORITY 1 ANALYSIS")
    for index_name in priority1:
        result = check_index_details(index_name)
        results.append(result)
        time.sleep(0.5)  # Brief pause
    
    # Quick check of other indexes
    other_indexes = [idx for idx in COBOL_INDEXES if idx not in priority1]
    
    print(f"\n📊 QUICK STATUS CHECK OF REMAINING {len(other_indexes)} INDEXES")
    print("-" * 60)
    
    for index_name in other_indexes:
        # Just get basic counts for the remaining indexes
        url = f"{endpoint}/indexes/{index_name}/docs/search"
        headers = {"Content-Type": "application/json", "api-key": search_key}
        
        try:
            total_payload = {"search": "*", "count": True, "top": 0}
            response = requests.post(url, headers=headers, json=total_payload, timeout=15)
            
            if response.status_code == 200:
                result = response.json()
                total = result.get("@odata.count", 0)
                print(f"   {index_name:25}: {total:,} records")
                results.append({"index": index_name, "total": total, "vectors": 0, "coverage": 0, "status": "CHECKED"})
            else:
                print(f"   {index_name:25}: Error {response.status_code}")
                results.append({"index": index_name, "error": f"HTTP {response.status_code}"})
                
        except Exception as e:
            print(f"   {index_name:25}: {str(e)}")
            results.append({"index": index_name, "error": str(e)})
    
    # Generate final summary
    generate_summary(results)

if __name__ == "__main__":
    main()