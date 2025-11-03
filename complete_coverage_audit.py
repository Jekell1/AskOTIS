#!/usr/bin/env python3
"""
Complete Source Code and Vector Coverage Audit

This script provides a detailed analysis of all COBOL indexes to check:
1. Source code coverage - which programs/files are indexed
2. Vector coverage - which records have embeddings
3. Data quality metrics
4. Index-specific statistics
"""

import json
import requests
import time
from typing import Dict, List, Any, Tuple
from collections import defaultdict

# Azure Search configuration
search_service_name = "wrld-cobol-search"
search_key = "nC5NRGV8FzDK7eOPa7eaQM94h6EZ3Ys66XlmMJq8h3AzSeAhpqxo"
endpoint = f"https://{search_service_name}.search.windows.net"

# All COBOL indexes to analyze
COBOL_INDEXES = [
    "screen-nodes", "ui-paths", "copy-statements", "cross-references", 
    "data-items", "function-calls", "includes", "io-operations", 
    "procedure-divisions", "program-summaries", "section-summaries", 
    "using-statements", "variable-definitions", "working-storage", 
    "linkage-sections", "file-definitions", "record-definitions"
]

def make_api_request(url: str, payload: Dict, retries: int = 3) -> Dict:
    """Make API request with retry logic."""
    headers = {
        "Content-Type": "application/json",
        "api-key": search_key
    }
    
    for attempt in range(retries):
        try:
            response = requests.post(url, headers=headers, json=payload, timeout=30)
            if response.status_code == 200:
                return response.json()
            elif response.status_code == 404:
                return {"error": "Index not found"}
            else:
                print(f"  ⚠️  API error {response.status_code} on attempt {attempt + 1}")
                if attempt < retries - 1:
                    time.sleep(2)
        except Exception as e:
            print(f"  ⚠️  Request error on attempt {attempt + 1}: {str(e)}")
            if attempt < retries - 1:
                time.sleep(2)
    
    return {"error": "Request failed after retries"}

def analyze_index_coverage(index_name: str) -> Dict[str, Any]:
    """Analyze source code and vector coverage for an index."""
    print(f"\n🔍 ANALYZING {index_name.upper()}")
    print("=" * 60)
    
    url = f"{endpoint}/indexes/{index_name}/docs/search"
    
    # Get total record count
    total_payload = {"search": "*", "count": True, "top": 0}
    total_result = make_api_request(url, total_payload)
    
    if "error" in total_result:
        print(f"❌ {total_result['error']}")
        return {"index": index_name, "error": total_result["error"]}
    
    total_count = total_result.get("@odata.count", 0)
    print(f"📊 Total records: {total_count:,}")
    
    if total_count == 0:
        return {
            "index": index_name,
            "total_records": 0,
            "status": "EMPTY"
        }
    
    # Get vector coverage
    vector_payload = {"search": "*", "filter": "has_vector eq true", "count": True, "top": 0}
    vector_result = make_api_request(url, vector_payload)
    vector_count = vector_result.get("@odata.count", 0) if "error" not in vector_result else 0
    
    # Get records without vectors
    no_vector_payload = {"search": "*", "filter": "has_vector eq false", "count": True, "top": 0}
    no_vector_result = make_api_request(url, no_vector_payload)
    no_vector_count = no_vector_result.get("@odata.count", 0) if "error" not in no_vector_result else 0
    
    # Calculate coverage metrics
    vector_coverage = (vector_count / total_count * 100) if total_count > 0 else 0
    
    print(f"✅ With vectors: {vector_count:,}")
    print(f"❌ Without vectors: {no_vector_count:,}")
    print(f"📈 Vector coverage: {vector_coverage:.1f}%")
    
    # Analyze source code coverage by getting sample data
    sample_payload = {"search": "*", "top": 100, "select": "program_id,file_path,source_file"}
    sample_result = make_api_request(url, sample_payload)
    
    unique_programs = set()
    unique_files = set()
    
    if "error" not in sample_result and "value" in sample_result:
        for doc in sample_result["value"]:
            # Check for program identifiers
            if "program_id" in doc and doc["program_id"]:
                unique_programs.add(doc["program_id"])
            
            # Check for file paths
            for field in ["file_path", "source_file", "file_name"]:
                if field in doc and doc[field]:
                    unique_files.add(doc[field])
    
    # Determine index status
    if vector_coverage >= 99.5:
        status = "COMPLETE"
    elif vector_coverage >= 95:
        status = "NEARLY_COMPLETE"
    elif vector_coverage >= 50:
        status = "PARTIAL"
    elif vector_coverage > 0:
        status = "STARTED"
    else:
        status = "NO_VECTORS"
    
    print(f"📁 Unique programs (sample): {len(unique_programs)}")
    print(f"📂 Unique files (sample): {len(unique_files)}")
    print(f"🎯 Status: {status}")
    
    return {
        "index": index_name,
        "total_records": total_count,
        "vector_count": vector_count,
        "no_vector_count": no_vector_count,
        "vector_coverage": vector_coverage,
        "unique_programs_sample": len(unique_programs),
        "unique_files_sample": len(unique_files),
        "status": status
    }

def analyze_data_quality(index_name: str) -> Dict[str, Any]:
    """Analyze data quality metrics for an index."""
    url = f"{endpoint}/indexes/{index_name}/docs/search"
    
    # Sample records to analyze field population
    sample_payload = {"search": "*", "top": 50}
    sample_result = make_api_request(url, sample_payload)
    
    if "error" in sample_result or "value" not in sample_result:
        return {"index": index_name, "error": "Cannot analyze quality"}
    
    docs = sample_result["value"]
    if not docs:
        return {"index": index_name, "no_data": True}
    
    # Analyze field population
    field_stats = defaultdict(int)
    total_docs = len(docs)
    
    for doc in docs:
        for field, value in doc.items():
            if field.startswith("@"):  # Skip metadata fields
                continue
            if value and str(value).strip():
                field_stats[field] += 1
    
    # Calculate field population percentages
    field_coverage = {
        field: (count / total_docs * 100) 
        for field, count in field_stats.items()
    }
    
    # Identify well-populated and sparse fields
    well_populated = {k: v for k, v in field_coverage.items() if v >= 90}
    sparse_fields = {k: v for k, v in field_coverage.items() if v < 50}
    
    return {
        "index": index_name,
        "sample_size": total_docs,
        "field_coverage": field_coverage,
        "well_populated_fields": len(well_populated),
        "sparse_fields": len(sparse_fields),
        "total_fields": len(field_coverage)
    }

def analyze_source_code_distribution(results: List[Dict]) -> None:
    """Analyze source code distribution across indexes."""
    print(f"\n📁 SOURCE CODE DISTRIBUTION ANALYSIS")
    print("=" * 60)
    
    # Collect program and file counts
    total_programs = 0
    total_files = 0
    
    for result in results:
        if "error" not in result and result.get("status") != "EMPTY":
            total_programs += result.get("unique_programs_sample", 0)
            total_files += result.get("unique_files_sample", 0)
    
    print(f"📊 Estimated unique programs across indexes: {total_programs}")
    print(f"📊 Estimated unique files across indexes: {total_files}")
    
    # Show distribution by index type
    high_program_indexes = [r for r in results if r.get("unique_programs_sample", 0) > 20]
    high_file_indexes = [r for r in results if r.get("unique_files_sample", 0) > 10]
    
    if high_program_indexes:
        print(f"\n📈 Indexes with high program diversity:")
        for result in sorted(high_program_indexes, key=lambda x: x.get("unique_programs_sample", 0), reverse=True):
            print(f"   {result['index']:18}: {result['unique_programs_sample']} programs")
    
    if high_file_indexes:
        print(f"\n📈 Indexes with high file diversity:")
        for result in sorted(high_file_indexes, key=lambda x: x.get("unique_files_sample", 0), reverse=True):
            print(f"   {result['index']:18}: {result['unique_files_sample']} files")

def generate_coverage_summary(results: List[Dict]) -> None:
    """Generate overall coverage summary."""
    print("\n" + "=" * 80)
    print("📊 COMPREHENSIVE COVERAGE SUMMARY")
    print("=" * 80)
    
    # Sort by completion status
    complete = [r for r in results if r.get("status") == "COMPLETE"]
    nearly_complete = [r for r in results if r.get("status") == "NEARLY_COMPLETE"]
    partial = [r for r in results if r.get("status") == "PARTIAL"]
    started = [r for r in results if r.get("status") == "STARTED"]
    no_vectors = [r for r in results if r.get("status") == "NO_VECTORS"]
    empty = [r for r in results if r.get("status") == "EMPTY"]
    errors = [r for r in results if "error" in r]
    
    # Summary table
    print("INDEX                | RECORDS  | VECTORS  | COVERAGE | STATUS")
    print("-" * 80)
    
    for result in results:
        if "error" in result:
            print(f"{result['index']:18} | ERROR    |          |          | ERROR")
        elif result.get("status") == "EMPTY":
            print(f"{result['index']:18} | 0        | 0        | 0.0%     | EMPTY")
        else:
            print(f"{result['index']:18} | {result['total_records']:8,} | {result['vector_count']:8,} | {result['vector_coverage']:6.1f}% | {result['status']}")
    
    print("-" * 80)
    
    # Category summaries
    if complete:
        print(f"✅ COMPLETE ({len(complete)}): {', '.join([r['index'] for r in complete])}")
    if nearly_complete:
        print(f"🟡 NEARLY COMPLETE ({len(nearly_complete)}): {', '.join([r['index'] for r in nearly_complete])}")
    if partial:
        print(f"🟠 PARTIAL ({len(partial)}): {', '.join([r['index'] for r in partial])}")
    if started:
        print(f"🔴 STARTED ({len(started)}): {', '.join([r['index'] for r in started])}")
    if no_vectors:
        print(f"⚫ NO VECTORS ({len(no_vectors)}): {', '.join([r['index'] for r in no_vectors])}")
    if empty:
        print(f"⚪ EMPTY ({len(empty)}): {', '.join([r['index'] for r in empty])}")
    if errors:
        print(f"❌ ERRORS ({len(errors)}): {', '.join([r['index'] for r in errors])}")
    
    # Overall statistics
    valid_results = [r for r in results if "error" not in r and r.get("status") != "EMPTY"]
    if valid_results:
        total_records = sum(r["total_records"] for r in valid_results)
        total_vectors = sum(r["vector_count"] for r in valid_results)
        overall_coverage = (total_vectors / total_records * 100) if total_records > 0 else 0
        
        print(f"\n📈 OVERALL STATISTICS:")
        print(f"   Total Records: {total_records:,}")
        print(f"   Total Vectors: {total_vectors:,}")
        print(f"   Overall Coverage: {overall_coverage:.1f}%")
        print(f"   Indexes Analyzed: {len(results)}")
        print(f"   Complete Indexes: {len(complete)}")

def main():
    """Main analysis function."""
    print("🔍 COMPLETE SOURCE CODE AND VECTOR COVERAGE AUDIT")
    print("=" * 80)
    print("Analyzing all COBOL indexes for completeness and data quality...")
    
    results = []
    
    # Analyze each index
    for index_name in COBOL_INDEXES:
        try:
            result = analyze_index_coverage(index_name)
            results.append(result)
            
            # Brief pause between indexes
            time.sleep(1)
            
        except Exception as e:
            print(f"❌ Error analyzing {index_name}: {str(e)}")
            results.append({"index": index_name, "error": str(e)})
    
    # Generate summaries
    generate_coverage_summary(results)
    analyze_source_code_distribution(results)
    
    # Priority recommendations
    print(f"\n🎯 PRIORITY RECOMMENDATIONS:")
    
    # Identify indexes needing attention
    needs_work = [r for r in results if r.get("vector_coverage", 0) < 95 and r.get("total_records", 0) > 0]
    
    if needs_work:
        print("   Indexes needing vector completion:")
        for result in sorted(needs_work, key=lambda x: x.get("vector_coverage", 0)):
            remaining = result.get("no_vector_count", 0)
            print(f"   - {result['index']:18}: {remaining:,} records need vectors ({result.get('vector_coverage', 0):.1f}% complete)")
    else:
        print("   🎉 All indexes with data have excellent vector coverage!")
    
    # Check for empty indexes
    empty_indexes = [r for r in results if r.get("status") == "EMPTY"]
    if empty_indexes:
        print(f"\n   Empty indexes (may need data ingestion):")
        for result in empty_indexes:
            print(f"   - {result['index']}")
    
    # Success summary
    successful_indexes = [r for r in results if r.get("status") in ["COMPLETE", "NEARLY_COMPLETE"]]
    if successful_indexes:
        total_successful_records = sum(r["total_records"] for r in successful_indexes)
        total_successful_vectors = sum(r["vector_count"] for r in successful_indexes)
        print(f"\n🎉 SUCCESS METRICS:")
        print(f"   Successfully vectorized indexes: {len(successful_indexes)}")
        print(f"   Total vectorized records: {total_successful_vectors:,}")
        print(f"   Coverage in successful indexes: {(total_successful_vectors/total_successful_records*100):.1f}%")

if __name__ == "__main__":
    main()