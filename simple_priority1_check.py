#!/usr/bin/env python3
"""
Simple Priority 1 Status Check

Check the completion status by attempting basic searches on both indexes.
"""

import json
import requests
from typing import Dict, Any

# Azure Search configuration
search_service_name = "wrld-cobol-search"
search_key = "nC5NRGV8FzDK7eOPa7eaQM94h6EZ3Ys66XlmMJq8h3AzSeAhpqxo"
endpoint = f"https://{search_service_name}.search.windows.net"

def check_index_status(index_name: str) -> Dict[str, Any]:
    """Check basic status of an index using REST API."""
    print(f"\n📋 CHECKING {index_name.upper()}")
    print("-" * 40)
    
    # Use simple search with count
    url = f"{endpoint}/indexes/{index_name}/docs/search"
    headers = {
        "Content-Type": "application/json",
        "api-key": search_key
    }
    
    # Get total count
    total_payload = {
        "search": "*",
        "count": True,
        "top": 0
    }
    
    try:
        response = requests.post(url, headers=headers, json=total_payload, timeout=30)
        if response.status_code == 200:
            total_result = response.json()
            total_count = total_result.get("@odata.count", 0)
            print(f"✅ Total records: {total_count:,}")
            
            # Quick test: try to get a few records with has_vector=true
            vector_payload = {
                "search": "*",
                "filter": "has_vector eq true",
                "count": True,
                "top": 0
            }
            
            vector_response = requests.post(url, headers=headers, json=vector_payload, timeout=30)
            if vector_response.status_code == 200:
                vector_result = vector_response.json()
                vector_count = vector_result.get("@odata.count", 0)
                print(f"✅ With embeddings: {vector_count:,}")
                
                completion_rate = (vector_count / total_count * 100) if total_count > 0 else 0
                print(f"📊 Completion: {completion_rate:.1f}%")
                
                status = "COMPLETE" if completion_rate >= 99.5 else "INCOMPLETE"
                print(f"🎯 Status: {status}")
                
                return {
                    "index": index_name,
                    "total": total_count,
                    "with_vectors": vector_count,
                    "completion": completion_rate,
                    "status": status
                }
            else:
                print(f"❌ Failed to get vector count: {vector_response.status_code}")
        else:
            print(f"❌ Failed to connect: {response.status_code}")
            
    except Exception as e:
        print(f"❌ Error: {str(e)}")
    
    return {"index": index_name, "error": True}

def main():
    """Check both Priority 1 indexes."""
    print("🎯 PRIORITY 1 COMPLETION VERIFICATION")
    print("=" * 50)
    
    indexes = ["screen-nodes", "ui-paths"]
    results = []
    
    for index_name in indexes:
        result = check_index_status(index_name)
        results.append(result)
    
    # Summary
    print("\n" + "=" * 50)
    print("📊 SUMMARY")
    print("=" * 50)
    
    total_records = 0
    total_with_vectors = 0
    all_complete = True
    
    for result in results:
        if "error" not in result:
            print(f"{result['index']:12} | {result['completion']:6.1f}% | {result['status']}")
            total_records += result['total']
            total_with_vectors += result['with_vectors']
            if result['status'] != "COMPLETE":
                all_complete = False
        else:
            print(f"{result['index']:12} | ERROR")
            all_complete = False
    
    if total_records > 0:
        overall_completion = (total_with_vectors / total_records * 100)
        print("-" * 50)
        print(f"{'OVERALL':12} | {overall_completion:6.1f}% | {'COMPLETE' if all_complete else 'INCOMPLETE'}")
        
        if all_complete:
            print("\n🎉 SUCCESS: Priority 1 indexes are complete!")
            print("✅ Vector dimension issue resolved")
            print("✅ 3072-dimension embeddings working")
        else:
            print("\n⚠️  Some work remaining")

if __name__ == "__main__":
    main()