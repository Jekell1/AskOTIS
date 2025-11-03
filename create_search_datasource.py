#!/usr/bin/env python3
"""
Create Azure AI Search data source for COBOL code JSONL files
"""
import json
import requests
import os
from pathlib import Path

# Load configuration from local.settings.json
config_path = Path(__file__).parent / "local.settings.json"
with open(config_path, 'r') as f:
    config = json.load(f)

values = config["Values"]

# Extract values
search_endpoint = values["SEARCH_ENDPOINT"]
search_key = values["SEARCH_KEY"]
storage_connection = values["AzureWebJobsStorage"]

# Parse search service name from endpoint
search_name = search_endpoint.replace("https://", "").replace(".search.windows.net", "")

print(f"Creating data source on search service: {search_name}")
print(f"Using storage connection string for: waazuse1aistorage")

# API endpoint
url = f"{search_endpoint}/datasources/cobol-ds?api-version=2024-07-01"

# Headers
headers = {
    "api-key": search_key,
    "Content-Type": "application/json"
}

# Request body - using 'aisearch' container where your JSONL files are located
data = {
    "name": "cobol-ds",
    "type": "azureblob",
    "credentials": {
        "connectionString": storage_connection
    },
    "container": {
        "name": "aisearch"
    },
    "dataDeletionDetectionPolicy": {
        "@odata.type": "#Microsoft.Azure.Search.NativeBlobSoftDeleteDeletionDetectionPolicy"
    }
}

print(f"Making PUT request to: {url}")
print(f"Request body: {json.dumps(data, indent=2)}")

try:
    response = requests.put(url, headers=headers, json=data)
    
    print(f"\nResponse Status: {response.status_code}")
    print(f"Response Headers: {dict(response.headers)}")
    
    if response.content:
        try:
            response_json = response.json()
            print(f"Response Body: {json.dumps(response_json, indent=2)}")
        except json.JSONDecodeError:
            print(f"Response Body (text): {response.text}")
    
    if response.status_code in [200, 201]:
        print("\n✅ Data source created successfully!")
    else:
        print(f"\n❌ Failed to create data source. Status: {response.status_code}")
        
except requests.RequestException as e:
    print(f"❌ Request failed: {e}")
except Exception as e:
    print(f"❌ Error: {e}")
