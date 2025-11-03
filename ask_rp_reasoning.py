"""Ask RAG to explain how it determined 178 copybooks for RP."""

import requests
import json

FUNCTION_URL = "https://func-otis-rag.azurewebsites.net/api/query"

question = """You said LONPF2 uses 178 copybooks when handling RP transactions. 
How did you determine this number? What RP logic did you identify, and which copybooks 
are used in that logic? Please be specific about the RP entry points you found."""

print("="*80)
print("Asking RAG to Explain Its Reasoning")
print("="*80)
print(f"Question: {question}\n")

try:
    response = requests.post(
        FUNCTION_URL,
        json={"question": question},
        headers={"Content-Type": "application/json"},
        timeout=120
    )
    response.raise_for_status()
    result = response.json()
    
    answer = result.get("answer", "")
    
    print(f"{'='*80}")
    print("ANSWER:")
    print(f"{'='*80}")
    print(answer)
    
    # Save
    with open("rp_reasoning_explanation.json", "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)
    print(f"\n💾 Saved to: rp_reasoning_explanation.json")
    
except Exception as e:
    print(f"❌ ERROR: {e}")
