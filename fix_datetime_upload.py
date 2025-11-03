"""
Fixed version with proper datetime formatting for Azure Search.
"""

import json
import requests
from datetime import datetime, timezone

def test_fixed_upload():
    """Test upload with corrected datetime format."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "enhanced_cobol_dependencies"
    
    # Test with properly formatted datetime
    print("📤 Testing with corrected datetime format...")
    
    # Use proper ISO 8601 format with timezone
    timestamp = datetime.now(timezone.utc).isoformat()
    
    test_doc = {
        "@search.action": "mergeOrUpload",
        "dependency_id": "TEST_002",
        "caller_program": "APIPAY", 
        "called_resource": "LP_LONPF2",
        "reference_type": "CALL_DYNAMIC",
        "reference_description": "Dynamic CALL using variable",
        "line_number": 1480,
        "statement": "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME",
        "file_path": "cobol_src/SP/APIPAY.CBL",
        "file_size": 104476,
        "analysis_timestamp": timestamp,
        "category": "PROGRAM_CALL",
        "is_program_call": True,
        "is_copybook": False,
        "is_system_call": False
    }
    
    upload_url = f"{endpoint}/indexes/{index_name}/docs/index?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    response = requests.post(upload_url, headers=headers, json={"value": [test_doc]})
    
    if response.status_code in [200, 201]:
        print("✅ Fixed upload successful!")
        result = response.json()
        print(f"Result: {result}")
        return True
    else:
        print(f"❌ Upload still failed: {response.status_code}")
        print(f"Response: {response.text}")
        return False

def upload_apipay_sample():
    """Upload a sample of APIPAY's actual external references."""
    
    if not test_fixed_upload():
        return
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "enhanced_cobol_dependencies"
    upload_url = f"{endpoint}/indexes/{index_name}/docs/index?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    # Create sample APIPAY dependencies with correct datetime
    timestamp = datetime.now(timezone.utc).isoformat()
    
    apipay_dependencies = [
        {
            "@search.action": "mergeOrUpload",
            "dependency_id": "APIPAY_00346_CALL_DYNAMIC",
            "caller_program": "APIPAY",
            "called_resource": "GB/SETENV",
            "reference_type": "CALL_DYNAMIC",
            "reference_description": "Dynamic CALL using variable",
            "line_number": 347,
            "statement": "CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.",
            "file_path": "cobol_src/SP/APIPAY.CBL",
            "file_size": 104476,
            "analysis_timestamp": timestamp,
            "category": "PROGRAM_CALL",
            "is_program_call": True,
            "is_copybook": False,
            "is_system_call": False
        },
        {
            "@search.action": "mergeOrUpload",
            "dependency_id": "APIPAY_01480_CALL_DYNAMIC",
            "caller_program": "APIPAY",
            "called_resource": "LP/LONPF2",
            "reference_type": "CALL_DYNAMIC",
            "reference_description": "Dynamic CALL using variable",
            "line_number": 1481,
            "statement": "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME",
            "file_path": "cobol_src/SP/APIPAY.CBL",
            "file_size": 104476,
            "analysis_timestamp": timestamp,
            "category": "PROGRAM_CALL",
            "is_program_call": True,
            "is_copybook": False,
            "is_system_call": False
        },
        {
            "@search.action": "mergeOrUpload",
            "dependency_id": "APIPAY_02563_CALL_STATIC", 
            "caller_program": "APIPAY",
            "called_resource": "C$MAKEDIR",
            "reference_type": "CALL_STATIC",
            "reference_description": "Direct CALL to named program",
            "line_number": 2563,
            "statement": "CALL \"C$MAKEDIR\" USING DIR-ACCESS-BUF GIVING STAT-99",
            "file_path": "cobol_src/SP/APIPAY.CBL",
            "file_size": 104476,
            "analysis_timestamp": timestamp,
            "category": "PROGRAM_CALL",
            "is_program_call": True,
            "is_copybook": False,
            "is_system_call": False
        },
        {
            "@search.action": "mergeOrUpload",
            "dependency_id": "APIPAY_00042_COPY_SIMPLE",
            "caller_program": "APIPAY",
            "called_resource": "LIBLP/LPFSWK.CPY",
            "reference_type": "COPY_SIMPLE",
            "reference_description": "COPY copybook by quoted name",
            "line_number": 42,
            "statement": "COPY \"LIBLP/LPFSWK.CPY\".",
            "file_path": "cobol_src/SP/APIPAY.CBL",
            "file_size": 104476,
            "analysis_timestamp": timestamp,
            "category": "COPYBOOK",
            "is_program_call": False,
            "is_copybook": True,
            "is_system_call": False
        }
    ]
    
    print(f"\n📤 Uploading {len(apipay_dependencies)} APIPAY dependencies...")
    
    response = requests.post(upload_url, headers=headers, json={"value": apipay_dependencies})
    
    if response.status_code in [200, 201]:
        print("✅ APIPAY dependencies uploaded successfully!")
        result = response.json()
        success_count = sum(1 for item in result.get('value', []) if item.get('status'))
        print(f"Successfully processed: {success_count}/{len(apipay_dependencies)} documents")
    else:
        print(f"❌ Upload failed: {response.status_code}")
        print(f"Response: {response.text}")

if __name__ == "__main__":
    print("🔧 TESTING FIXED DATETIME FORMAT")
    print("=" * 50)
    upload_apipay_sample()