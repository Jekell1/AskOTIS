#!/usr/bin/env python3
"""
Simple Azure OpenAI diagnostics
"""

import json
import requests

def load_settings():
    """Load settings from local.settings.json"""
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except Exception as e:
        print(f"❌ Error loading settings: {e}")
        return {}

def test_endpoint_reachability(endpoint):
    """Test if the endpoint is reachable"""
    try:
        # Just test basic connectivity (this will likely return 401 but that's expected)
        response = requests.get(f"{endpoint}/openai/models", timeout=10)
        print(f"✅ Endpoint reachable (status: {response.status_code})")
        return True
    except requests.exceptions.RequestException as e:
        print(f"❌ Endpoint not reachable: {e}")
        return False

def main():
    print("🔍 Azure OpenAI Diagnostics\n")
    
    settings = load_settings()
    if not settings:
        return
    
    endpoint = settings.get('AZURE_OPENAI_ENDPOINT')
    key = settings.get('AZURE_OPENAI_KEY')
    deployment = settings.get('AZURE_OPENAI_DEPLOYMENT')
    
    print(f"Configuration:")
    print(f"  Endpoint: {endpoint}")
    print(f"  Deployment: {deployment}")
    print(f"  Key: {'Present (' + key[:10] + '...)' if key else 'Missing'}")
    print()
    
    # Test endpoint reachability
    print("Testing endpoint reachability...")
    test_endpoint_reachability(endpoint)
    
    print(f"\n💡 Troubleshooting tips:")
    print(f"1. Verify the endpoint URL is correct for your Azure OpenAI resource")
    print(f"2. Check that the API key is valid and not expired")  
    print(f"3. Ensure the 'o3-pro' deployment exists in your Azure OpenAI resource")
    print(f"4. Verify your Azure subscription has access to the o3-pro model")
    print(f"5. Check that the resource is in the correct region")

if __name__ == "__main__":
    main()
