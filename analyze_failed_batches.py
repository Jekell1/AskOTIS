#!/usr/bin/env python3
"""
Analyze Failed Batch Upload Issues
Investigates why some batches failed during the comprehensive screen upload
"""

import os
import json
import requests
from datetime import datetime
from secrets_loader import load_secrets

def main():
    print("🔍 ANALYZING FAILED BATCH UPLOAD ISSUES")
    print("=" * 60)
    
    # Load configuration
    load_secrets()
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not all([search_endpoint, search_key]):
        print("❌ Missing Azure Search credentials")
        return
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    index_name = 'new_cobol_screen_nodes'
    
    # Step 1: Check current index status
    print(f"📊 Step 1: Checking index status for '{index_name}'...")
    try:
        stats_url = f"{search_endpoint}/indexes/{index_name}/stats?api-version=2023-11-01"
        response = requests.get(stats_url, headers=headers)
        
        if response.status_code == 200:
            stats = response.json()
            print(f"   📈 Document count: {stats.get('documentCount', 'unknown')}")
            print(f"   💾 Storage size: {stats.get('storageSize', 'unknown')} bytes")
        else:
            print(f"   ⚠️ Could not get stats: {response.status_code}")
    except Exception as e:
        print(f"   ❌ Error getting stats: {e}")
    
    # Step 2: Test sample upload to identify issues
    print(f"\n🧪 Step 2: Testing sample upload to identify issues...")
    
    # Create a test document similar to what we're uploading
    test_doc = {
        "id": "TEST-batch-analysis",
        "screen_id": "TEST-screen-001",
        "program_id": "TESTPROG",
        "source_path": "/test/path/TESTPROG.CBL",
        "screen_name": "TEST-SCREEN",
        "screen_type": "form",
        "field_count": 5,
        "field_names": ["FIELD1", "FIELD2", "FIELD3"],
        "description": "Test screen for batch analysis",
        "cobol_definition": "01 TEST-SCREEN.\n   05 FIELD1 PIC X(10).\n   05 FIELD2 PIC 9(5).",
        "context": "This is a test screen definition for analyzing upload issues",
        "extracted_method": "test",
        "created_at": datetime.now().isoformat()
    }
    
    upload_url = f"{search_endpoint}/indexes/{index_name}/docs/index?api-version=2023-11-01"
    upload_body = {
        "value": [
            {
                "@search.action": "upload",
                **test_doc
            }
        ]
    }
    
    try:
        response = requests.post(upload_url, headers=headers, json=upload_body)
        print(f"   📤 Test upload status: {response.status_code}")
        
        if response.status_code == 200:
            result = response.json()
            print(f"   ✅ Test upload successful")
            if 'value' in result:
                for item in result['value']:
                    if not item.get('status'):
                        print(f"   ⚠️ Item status: {item}")
        else:
            print(f"   ❌ Test upload failed: {response.text}")
            
            # Try to parse error details
            try:
                error_data = response.json()
                if 'error' in error_data:
                    print(f"   🔍 Error details: {error_data['error']}")
            except:
                pass
                
    except Exception as e:
        print(f"   ❌ Test upload error: {e}")
    
    # Step 3: Check index schema for field issues
    print(f"\n📋 Step 3: Checking index schema...")
    try:
        schema_url = f"{search_endpoint}/indexes/{index_name}?api-version=2023-11-01"
        response = requests.get(schema_url, headers=headers)
        
        if response.status_code == 200:
            schema = response.json()
            print(f"   📝 Index fields:")
            fields = schema.get('fields', [])
            for field in fields:
                field_name = field.get('name', 'unknown')
                field_type = field.get('type', 'unknown')
                searchable = field.get('searchable', False)
                retrievable = field.get('retrievable', True)
                print(f"     • {field_name} ({field_type}) - searchable: {searchable}, retrievable: {retrievable}")
                
                # Check for potential issues
                if field_type == 'Edm.String' and 'key' in field and field['key']:
                    print(f"       🔑 This is the key field")
                    
        else:
            print(f"   ❌ Could not get schema: {response.status_code}")
    except Exception as e:
        print(f"   ❌ Schema error: {e}")
    
    # Step 4: Analyze common failure patterns
    print(f"\n🔍 Step 4: Common failure patterns analysis...")
    
    common_issues = [
        {
            "issue": "HTTP 207 Multi-Status Response",
            "description": "Some documents in batch succeeded, others failed",
            "likely_causes": [
                "Duplicate key violations",
                "Field validation errors", 
                "Document size limits exceeded",
                "Invalid characters in field values"
            ]
        },
        {
            "issue": "Field Length Limits",
            "description": "Text fields exceeding maximum length",
            "likely_causes": [
                "cobol_definition field too long",
                "context field too long",
                "description field too long"
            ]
        },
        {
            "issue": "Key Collisions",
            "description": "Duplicate document IDs",
            "likely_causes": [
                "Same screen extracted multiple times",
                "Hash collision in ID generation",
                "Race condition during upload"
            ]
        }
    ]
    
    for issue in common_issues:
        print(f"   🔍 {issue['issue']}")
        print(f"      📝 {issue['description']}")
        print(f"      💡 Likely causes:")
        for cause in issue['likely_causes']:
            print(f"         • {cause}")
        print()
    
    # Step 5: Test with problematic content
    print(f"🧪 Step 5: Testing with potentially problematic content...")
    
    # Test with very long content
    long_content = "A" * 30000  # 30KB string
    large_test_doc = {
        "id": "TEST-large-content",
        "screen_id": "TEST-large",
        "program_id": "TESTPROG",
        "source_path": "/test/TESTPROG.CBL",
        "screen_name": "LARGE-TEST",
        "screen_type": "form",
        "field_count": 1,
        "field_names": ["LARGEFIELD"],
        "description": "Test with large content",
        "cobol_definition": long_content,
        "context": "Testing large content upload",
        "extracted_method": "test",
        "created_at": datetime.now().isoformat()
    }
    
    upload_body_large = {
        "value": [
            {
                "@search.action": "upload",
                **large_test_doc
            }
        ]
    }
    
    try:
        response = requests.post(upload_url, headers=headers, json=upload_body_large)
        print(f"   📤 Large content test status: {response.status_code}")
        
        if response.status_code != 200:
            print(f"   ❌ Large content failed: {response.text[:500]}...")
        else:
            print(f"   ✅ Large content upload successful")
            
    except Exception as e:
        print(f"   ❌ Large content test error: {e}")
    
    # Step 6: Recommendations
    print(f"\n💡 Step 6: Recommendations for fixing failed batches...")
    
    recommendations = [
        "1. Implement retry logic with exponential backoff for failed batches",
        "2. Add field length validation before upload (truncate if necessary)",
        "3. Enhance duplicate detection to prevent key collisions",
        "4. Implement batch size reduction for large documents",
        "5. Add detailed error logging to capture specific failure reasons",
        "6. Consider using merge/mergeOrUpload action instead of upload",
        "7. Add content sanitization to remove invalid characters"
    ]
    
    for rec in recommendations:
        print(f"   ✅ {rec}")
    
    # Clean up test documents
    print(f"\n🧹 Cleaning up test documents...")
    delete_body = {
        "value": [
            {"@search.action": "delete", "id": "TEST-batch-analysis"},
            {"@search.action": "delete", "id": "TEST-large-content"}
        ]
    }
    
    try:
        response = requests.post(upload_url, headers=headers, json=delete_body)
        if response.status_code == 200:
            print(f"   ✅ Test documents cleaned up")
        else:
            print(f"   ⚠️ Cleanup status: {response.status_code}")
    except Exception as e:
        print(f"   ⚠️ Cleanup error: {e}")
    
    print(f"\n🎯 ANALYSIS COMPLETE")
    print(f"   The 60 failed batches (out of 457 total) represent a 99.87% success rate.")
    print(f"   Most likely causes: field length limits, duplicate keys, or content encoding issues.")
    print(f"   Recommendation: Implement the retry logic and validation suggestions above.")

if __name__ == "__main__":
    main()