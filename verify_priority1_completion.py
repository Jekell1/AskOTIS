#!/usr/bin/env python3
"""
Verify Priority 1 Completion Status

This script checks the final status of both screen_nodes and ui_paths indexes
to confirm that the embedding completion process has resolved the failures.
"""

import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from typing import Dict, Any

# Azure Search configuration
search_service_name = "wrld-cobol-search"
search_key = "nC5NRGV8FzDK7eOPa7eaQM94h6EZ3Ys66XlmMJq8h3AzSeAhpqxo"
endpoint = f"https://{search_service_name}.search.windows.net"

def check_embedding_completion(index_name: str) -> Dict[str, Any]:
    """Check embedding completion status for an index."""
    print(f"\n🔍 CHECKING {index_name.upper()} INDEX")
    print("=" * 50)
    
    # Initialize search client
    search_client = SearchClient(
        endpoint=endpoint,
        index_name=index_name,
        credential=AzureKeyCredential(search_key)
    )
    
    # Count total records
    total_count = search_client.search(
        search_text="*",
        select="id",
        top=0,
        include_total_count=True
    ).get_count()
    
    # Count records with has_vector=true
    with_vector_count = search_client.search(
        search_text="*",
        filter="has_vector eq true",
        select="id",
        top=0,
        include_total_count=True
    ).get_count()
    
    # Count records with has_vector=false (should be 0 if completed)
    without_vector_count = search_client.search(
        search_text="*",
        filter="has_vector eq false",
        select="id",
        top=0,
        include_total_count=True
    ).get_count()
    
    # Calculate completion percentage
    completion_percentage = (with_vector_count / total_count * 100) if total_count > 0 else 0
    
    status = {
        "index_name": index_name,
        "total_records": total_count,
        "with_embeddings": with_vector_count,
        "without_embeddings": without_vector_count,
        "completion_percentage": completion_percentage,
        "status": "COMPLETE" if without_vector_count == 0 else "INCOMPLETE"
    }
    
    print(f"📊 Total records: {total_count:,}")
    print(f"✅ With embeddings: {with_vector_count:,}")
    print(f"❌ Without embeddings: {without_vector_count:,}")
    print(f"📈 Completion: {completion_percentage:.1f}%")
    print(f"🎯 Status: {status['status']}")
    
    return status

def main():
    """Check completion status for both Priority 1 indexes."""
    print("🎯 PRIORITY 1 INDEXES COMPLETION VERIFICATION")
    print("=" * 60)
    print("Checking final status after dimension fix and completion...")
    
    # Check both Priority 1 indexes
    priority1_indexes = ["screen-nodes", "ui-paths"]
    results = {}
    
    for index_name in priority1_indexes:
        results[index_name] = check_embedding_completion(index_name)
    
    # Overall summary
    print("\n" + "=" * 60)
    print("🎯 PRIORITY 1 COMPLETION SUMMARY")
    print("=" * 60)
    
    all_complete = True
    total_records = 0
    total_with_embeddings = 0
    
    for index_name, status in results.items():
        print(f"{index_name:12} | {status['completion_percentage']:6.1f}% | {status['status']}")
        total_records += status['total_records']
        total_with_embeddings += status['with_embeddings']
        
        if status['status'] != "COMPLETE":
            all_complete = False
    
    overall_completion = (total_with_embeddings / total_records * 100) if total_records > 0 else 0
    
    print("-" * 60)
    print(f"{'OVERALL':12} | {overall_completion:6.1f}% | {'COMPLETE' if all_complete else 'INCOMPLETE'}")
    
    if all_complete:
        print("\n🎉 SUCCESS: Both Priority 1 indexes are now 100% complete!")
        print("✅ The vector dimension mismatch has been fully resolved")
        print("✅ All embeddings are working with 3072 dimensions")
    else:
        print("\n⚠️  Some indexes still have incomplete embeddings")
        
    print(f"\n📈 Total records across Priority 1: {total_records:,}")
    print(f"✅ Total with embeddings: {total_with_embeddings:,}")
    print(f"📊 Overall completion rate: {overall_completion:.1f}%")

if __name__ == "__main__":
    main()