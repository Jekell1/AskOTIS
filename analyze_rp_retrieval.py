"""Check what indexes were actually queried."""
import json

with open('rp_copybooks_debug.json', encoding='utf-8') as f:
    data = json.load(f)

print("="*80)
print("RETRIEVAL ANALYSIS")
print("="*80)

timing = data.get('timing', {})
print(f"\n📊 Question type: {timing.get('question_type')}")
print(f"📄 Total documents retrieved: {timing.get('num_documents')}")
print(f"⏱️  Retrieval time: {timing.get('retrieve_seconds')}s")

# Check answer for index references
answer = data.get('answer', '')
print(f"\n🔍 Checking answer for index references...")

if 'Index: copybook_usage' in answer or 'copybook_usage' in answer.lower():
    print("   ✅ References copybook_usage index")
else:
    print("   ❌ NO copybook_usage index reference")

if 'Index: code' in answer:
    print("   ⚠️  References code index (semantic search)")

# Look for specific patterns
if 'program_id eq' in answer.lower() or 'filter' in answer.lower():
    print("   ✅ Used filtered query")
else:
    print("   ❌ NO filtered query used")

# Check how many copybooks mentioned
import re
copybook_names = re.findall(r'\[\[FILE:([^\]]+\.CPY)', answer)
print(f"\n📚 Copybooks mentioned in answer: {len(set(copybook_names))}")
print(f"   First 10: {list(set(copybook_names))[:10]}")

# Check if it says "11" or "288"
if '288' in answer:
    print("\n✅ Mentions 288 (complete list)")
elif '11' in answer:
    print("\n⚠️  Mentions only 11 (semantic search results, not filtered)")
