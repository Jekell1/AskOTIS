"""
Ask RAG to find PGMENU screen copybook
"""
import requests
import json

url = "https://func-otis-rag.azurewebsites.net/api/query"

question = "Show me the contents of PGMENU_SCN copybook or PGMENU screen definition"

payload = {
    "question": question,
    "conversation_id": "find_pgmenu_screen"
}

print(f"Testing: {question}\n")
print("=" * 80)

response = requests.post(url, json=payload)

if response.status_code == 200:
    result = response.json()
    print("\n📝 RESPONSE:")
    print(result.get('answer', 'No answer'))
else:
    print(f"❌ Error: {response.status_code}")
    print(response.text)
