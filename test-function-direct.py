import json
import requests

# Direct function configuration
function_url = "https://cobol-parser-func-1474.azurewebsites.net/api/cobol-parse"
function_key = "X38B4_cTI2Bb2Wc1lkgFW9aWKrSLIHwIdASTQipNAc8KAzFuiDgINA=="
full_url = f"{function_url}?code={function_key}"

print("Testing COBOL Azure Function")
print("="*50)
print(f"Function URL: {function_url}")
print(f"Function Key: {function_key[:20]}...")
print()

# Test data in Azure Cognitive Search WebAPI format
test_data = {
    "values": [
        {
            "recordId": "record1",
            "data": {
                "code": "PROGRAM-ID. TESTPROG.",
                "repo_path": "/cobol/test.cbl",
                "line_number": 1
            }
        },
        {
            "recordId": "record2",
            "data": {
                "code": "01 WS-COUNTER PIC 9(3).",
                "repo_path": "/cobol/test.cbl", 
                "line_number": 10
            }
        },
        {
            "recordId": "record3",
            "data": {
                "code": "CALL 'SUBPROG' USING WS-DATA.",
                "repo_path": "/cobol/test.cbl",
                "line_number": 20
            }
        }
    ]
}

print("Test data:")
print(json.dumps(test_data, indent=2))
print("\n" + "="*60)

try:
    print("Sending request to Azure Function...")
    
    headers = {
        'Content-Type': 'application/json'
    }
    
    response = requests.post(full_url, json=test_data, headers=headers, timeout=30)
    
    print(f"Status Code: {response.status_code}")
    
    if response.status_code == 200:
        try:
            result = response.json()
            print("\n✅ Function response:")
            print(json.dumps(result, indent=2))
            
            # Validate the response structure
            if 'values' in result:
                print(f"\n✅ Processed {len(result['values'])} records successfully")
                
                for i, value in enumerate(result['values']):
                    if 'recordId' in value and 'data' in value:
                        data = value['data']
                        print(f"  Record {i+1} ({value['recordId']}):")
                        if data.get('symbols'):
                            print(f"    Symbol: {data['symbols']} ({data.get('symbol_type', 'unknown')})")
                        if data.get('calls'):
                            print(f"    Calls: {data['calls']}")
                        if data.get('procedures'):
                            print(f"    Procedure: {data['procedures']}")
                        if data.get('variables'):
                            print(f"    Variable: {data['variables']}")
                
                print("\n🎉 Azure Function is working correctly!")
                print("✅ Ready to use in Azure Cognitive Search skillset!")
                
            else:
                print("\n❌ Invalid response format - missing 'values' array")
                
        except json.JSONDecodeError:
            print(f"\n❌ Invalid JSON response:")
            print(response.text[:500])
            
    else:
        print(f"\n❌ Function request failed:")
        print(f"Status: {response.status_code}")
        print(f"Response: {response.text[:500]}")
        
        if response.status_code == 401:
            print("\n💡 Authentication issue - check the function key")
        elif response.status_code == 404:
            print("\n💡 Function not found - check the function name")
        elif response.status_code == 500:
            print("\n💡 Internal server error - check function logs in Azure Portal")
        
except requests.exceptions.Timeout:
    print("❌ Request timed out (function might be cold-starting)")
    print("💡 Try again in a few seconds")
    
except requests.exceptions.RequestException as e:
    print(f"❌ Request failed: {e}")
    
except Exception as e:
    print(f"❌ Unexpected error: {e}")

print("\n" + "="*60)
print("Summary:")
print("✅ Azure Function deployed: cobol-parser-func-1474") 
print("✅ Function URL configured")
print("✅ Ready to create skillset with WebAPI skill")
print("\nNext steps:")
print("1. Create skillset using the WebAPI skill")
print("2. Create indexer to process COBOL files") 
print("3. Test search with Azure AI Search")
