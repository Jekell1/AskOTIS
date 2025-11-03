"""Inspect the actual documents being retrieved for menu queries."""
import requests
import json

url = "https://func-otis-rag.azurewebsites.net/api/query"
payload = {
    "question": "What are the user choices in the main user menu to OTIS?",
    "k": 10,  # Get top 10 docs
    "include_sources": True  # Make sure we get source docs
}

response = requests.post(url, json=payload)
data = response.json()

print("Full response structure:")
print(json.dumps(data, indent=2, default=str)[:1000])
print("\n" + "="*80)

# Try to find sources in the response
sources = data.get("sources", [])
if not sources and "result" in data:
    sources = data["result"].get("sources", [])
if not sources:
    print("No sources returned!")
else:
    for i, doc in enumerate(sources[:5], 1):  # Look at first 5
        print(f"\n--- Document {i} ---")
        print(f"Score: {doc.get('@search.score', 'N/A')}")
        
        # Print available fields
        for key, value in doc.items():
            if key.startswith("@"):
                continue
            if isinstance(value, str) and len(value) > 200:
                print(f"{key}: {value[:200]}...")
            else:
                print(f"{key}: {value}")
        print()
