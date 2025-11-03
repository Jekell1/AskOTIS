#!/usr/bin/env python3
"""
Fix data quality issues in screen nodes index by cleaning and re-populating
"""

import os
import json
import requests
from secrets_loader import load_secrets

def fix_screen_nodes_data_quality():
    """Fix the data quality issues in screen nodes index."""
    print("🔧 FIXING SCREEN NODES DATA QUALITY")
    print("=" * 50)
    
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    search_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    # Step 1: Count current state
    print("\n📊 CURRENT STATE ANALYSIS:")
    
    total_body = {"search": "*", "top": 0, "count": True}
    response = requests.post(search_url, headers=headers, json=total_body)
    total_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    good_body = {"search": "*", "filter": "screen_name ne null", "top": 0, "count": True}
    response = requests.post(search_url, headers=headers, json=good_body)
    good_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    bad_count = total_count - good_count
    
    print(f"   Total records: {total_count}")
    print(f"   Good records (with data): {good_count}")
    print(f"   Bad records (null data): {bad_count}")
    
    # Step 2: Delete bad records
    print(f"\n🗑️  DELETING {bad_count} BAD RECORDS:")
    
    # Get all bad record IDs
    bad_records_body = {
        "search": "*",
        "filter": "screen_name eq null",
        "top": 1000,  # Azure Search max
        "select": "screen_id"
    }
    
    deleted_count = 0
    
    while True:
        response = requests.post(search_url, headers=headers, json=bad_records_body)
        
        if response.status_code != 200:
            print(f"   ❌ Error fetching bad records: {response.status_code}")
            break
            
        results = response.json().get('value', [])
        
        if not results:
            print(f"   ✅ No more bad records to delete")
            break
        
        # Prepare delete batch
        delete_docs = []
        for record in results:
            delete_docs.append({
                "@search.action": "delete",
                "screen_id": record["screen_id"]
            })
        
        # Delete batch
        delete_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
        delete_body = {"value": delete_docs}
        
        delete_response = requests.post(delete_url, headers=headers, json=delete_body)
        
        if delete_response.status_code in [200, 201]:
            batch_size = len(delete_docs)
            deleted_count += batch_size
            print(f"   🗑️  Deleted batch: {batch_size} records (total: {deleted_count})")
        else:
            print(f"   ❌ Failed to delete batch: {delete_response.status_code}")
            print(f"   Error: {delete_response.text[:200]}")
            break
    
    print(f"\n✅ CLEANUP COMPLETE:")
    print(f"   Deleted: {deleted_count} bad records")
    print(f"   Remaining: {good_count} good records")
    
    # Step 3: Verify cleanup
    print(f"\n🔍 VERIFYING CLEANUP:")
    
    # Check remaining counts
    response = requests.post(search_url, headers=headers, json=total_body)
    final_total = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    response = requests.post(search_url, headers=headers, json=good_body)
    final_good = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    final_bad = final_total - final_good
    
    print(f"   Final total records: {final_total}")
    print(f"   Final good records: {final_good}")
    print(f"   Final bad records: {final_bad}")
    
    if final_bad == 0:
        print(f"   🎉 SUCCESS: All bad records removed!")
        
        # Update has_vector flags for remaining records
        print(f"\n🔄 UPDATING VECTOR FLAGS:")
        
        # Set has_vector = false for all remaining records so they can get new embeddings
        update_body = {
            "search": "*",
            "top": 1000,
            "select": "screen_id"
        }
        
        response = requests.post(search_url, headers=headers, json=update_body)
        
        if response.status_code == 200:
            results = response.json().get('value', [])
            
            # Prepare update batch
            update_docs = []
            for record in results:
                update_docs.append({
                    "@search.action": "merge",
                    "screen_id": record["screen_id"],
                    "has_vector": False,
                    "summary_vector": None
                })
            
            # Update batch
            update_response = requests.post(delete_url, headers=headers, json={"value": update_docs})
            
            if update_response.status_code in [200, 201]:
                print(f"   ✅ Reset vector flags for {len(update_docs)} records")
            else:
                print(f"   ⚠️  Warning: Failed to reset vector flags: {update_response.status_code}")
    else:
        print(f"   ⚠️  WARNING: {final_bad} bad records still remain")
    
    print(f"\n🎉 DATA QUALITY FIX COMPLETE!")
    print(f"   The index now contains only {final_good} valid records")
    print(f"   You can now re-run embedding generation for clean, meaningful results")

if __name__ == "__main__":
    fix_screen_nodes_data_quality()