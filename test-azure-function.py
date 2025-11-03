import json
import requests
import sys

# Load the function configuration
try:
    with open('function-deployment-config.json', 'r') as f:
        config = json.load(f)
    
    function_url = config['fullUrl']  # URL with the function key
    print(f"Testing Azure Function: {config['functionAppName']}")
    print(f"URL: {function_url}")
    print()
    
except Exception as e:
    print(f"Error loading function configuration: {e}")
    print("Make sure the function was deployed successfully.")
    sys.exit(1)

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
    
    response = requests.post(function_url, json=test_data, headers=headers, timeout=30)
    
    print(f"Status Code: {response.status_code}")
    print(f"Response Headers: {dict(response.headers)}")
    
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
                print("\n🎉 Function is working correctly!")
            else:
                print("\n❌ Invalid response format - missing 'values' array")
                
        except json.JSONDecodeError:
            print(f"\n❌ Invalid JSON response:")
            print(response.text)
            
    else:
        print(f"\n❌ Function request failed:")
        print(f"Status: {response.status_code}")
        print(f"Response: {response.text}")
        
        if response.status_code == 401:
            print("\n💡 This might be an authentication issue. Check the function key.")
        elif response.status_code == 404:
            print("\n💡 Function not found. Make sure it was deployed correctly.")
        elif response.status_code == 500:
            print("\n💡 Internal server error. Check the function logs in Azure Portal.")
        
except requests.exceptions.Timeout:
    print("❌ Request timed out. The function might be cold-starting.")
    print("💡 Try again in a few seconds.")
    
except requests.exceptions.RequestException as e:
    print(f"❌ Request failed: {e}")
    
except Exception as e:
    print(f"❌ Unexpected error: {e}")

print("\n" + "="*60)
print("Next steps:")
print("1. If the test passed, update your skillset with this function URL")
print("2. Create an indexer to process your COBOL files")
print("3. Check Azure Portal for detailed function logs if there were errors")
