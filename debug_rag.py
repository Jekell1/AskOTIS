#!/usr/bin/env python3
"""
Debug the RAG chatbot issue
"""

import json
import requests
import time

def debug_rag_issue():
    # Load settings
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        LOCAL_SETTINGS = settings.get('Values', {})

    SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT')  
    SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')
    AZURE_OPENAI_ENDPOINT = LOCAL_SETTINGS.get('AZURE_OPENAI_ENDPOINT')
    AZURE_OPENAI_KEY = LOCAL_SETTINGS.get('AZURE_OPENAI_KEY')
    AZURE_OPENAI_DEPLOYMENT = LOCAL_SETTINGS.get('AZURE_OPENAI_DEPLOYMENT', 'gpt-4')

    print("=== DEBUGGING RAG ISSUE ===")
    print(f"🔍 Search Endpoint: {SEARCH_ENDPOINT}")
    print(f"🤖 OpenAI Endpoint: {AZURE_OPENAI_ENDPOINT}")
    print(f"📡 OpenAI Deployment: {AZURE_OPENAI_DEPLOYMENT}")
    print()

    # Test 1: Basic search
    print("🧪 Test 1: Basic Search")
    search_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
    headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
    
    search_body = {
        'search': 'PROCEDURE DIVISION',
        'top': 3,
        'select': 'repo_path,code'
    }
    
    start_time = time.time()
    try:
        response = requests.post(search_url, headers=headers, json=search_body, timeout=10)
        search_time = time.time() - start_time
        
        if response.status_code == 200:
            results = response.json()
            docs = results.get('value', [])
            print(f"   ✅ Search works: {len(docs)} results in {search_time:.2f}s")
            
            if docs:
                sample_code = docs[0].get('code', '')[:200]
                print(f"   📄 Sample: {sample_code}...")
        else:
            print(f"   ❌ Search failed: {response.status_code}")
            
    except Exception as e:
        print(f"   ❌ Search error: {e}")
        
    print()

    # Test 2: Azure OpenAI
    print("🧪 Test 2: Azure OpenAI")
    if not AZURE_OPENAI_ENDPOINT or not AZURE_OPENAI_KEY:
        print("   ❌ Azure OpenAI not configured")
        return
        
    try:
        from openai import AzureOpenAI
        
        client = AzureOpenAI(
            api_key=AZURE_OPENAI_KEY,
            api_version="2024-02-01",
            azure_endpoint=AZURE_OPENAI_ENDPOINT
        )
        
        start_time = time.time()
        response = client.chat.completions.create(
            model=AZURE_OPENAI_DEPLOYMENT,
            messages=[
                {"role": "user", "content": "Say 'Hello, this is a test' in 5 words or less."}
            ],
            max_tokens=50,
            timeout=15
        )
        
        openai_time = time.time() - start_time
        
        if response.choices:
            print(f"   ✅ Azure OpenAI works in {openai_time:.2f}s")
            print(f"   📤 Response: {response.choices[0].message.content}")
        else:
            print(f"   ❌ Azure OpenAI: No response")
            
    except Exception as e:
        print(f"   ❌ Azure OpenAI error: {e}")
        
    print()

    # Test 3: Combined flow
    print("🧪 Test 3: Combined RAG Flow")
    
    if not AZURE_OPENAI_ENDPOINT or not AZURE_OPENAI_KEY:
        print("   ⚠️  Skipping - Azure OpenAI not available")
        return
        
    try:
        # Get search results
        search_results = requests.post(search_url, headers=headers, json=search_body, timeout=10)
        if search_results.status_code != 200:
            print(f"   ❌ Search failed: {search_results.status_code}")
            return
            
        docs = search_results.json().get('value', [])
        if not docs:
            print("   ❌ No search results")
            return
            
        # Create simple context
        context = f"COBOL Code: {docs[0].get('code', '')[:500]}"
        
        prompt = f"""Briefly explain what this COBOL code does:

{context}

Answer in 2-3 sentences."""

        start_time = time.time()
        response = client.chat.completions.create(
            model=AZURE_OPENAI_DEPLOYMENT,
            messages=[
                {"role": "user", "content": prompt}
            ],
            max_tokens=200,
            timeout=20
        )
        
        rag_time = time.time() - start_time
        
        if response.choices:
            print(f"   ✅ RAG flow works in {rag_time:.2f}s")
            print(f"   💬 Analysis: {response.choices[0].message.content[:200]}...")
        else:
            print(f"   ❌ RAG flow: No response")
            
    except Exception as e:
        print(f"   ❌ RAG flow error: {e}")
        
    print("\n=== DEBUG COMPLETE ===")

if __name__ == "__main__":
    debug_rag_issue()
