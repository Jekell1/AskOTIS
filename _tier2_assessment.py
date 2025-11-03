#!/usr/bin/env python3
"""Tier 2 embeddings assessment and planning."""
import requests, json

vals = json.load(open('local.settings.json')).get('Values', {})
ep = vals.get('AZURE_SEARCH_ENDPOINT', '').rstrip('/')
key = vals.get('AZURE_SEARCH_KEY', '')

print("=" * 80)
print("TIER 2 EMBEDDINGS ASSESSMENT")
print("=" * 80)

tier2_indexes = [
    {
        'index': 'new_cobol_calls',
        'vector_field': 'snippet_vector',
        'text_field': 'snippet',
        'description': 'Call site code snippets'
    },
    {
        'index': 'new_cobol_copybook_meta',
        'vector_field': 'summary_vector',
        'text_field': 'description',  # or summary field
        'description': 'Copybook metadata summaries'
    },
    {
        'index': 'new_cobol_copybook_usage',
        'vector_field': 'context_vector',
        'text_field': 'context',  # context around copybook usage
        'description': 'Copybook usage contexts'
    },
    {
        'index': 'new_cobol_paragraphs',
        'vector_field': 'para_vector',
        'text_field': 'para_text',  # or snippet
        'description': 'Paragraph code/descriptions'
    }
]

total_docs = 0
print("\nTier 2 Indexes:")
print(f"{'Index':<35} {'Vector Field':<25} {'Docs':>10} {'Status'}")
print("-" * 80)

for item in tier2_indexes:
    url = f"{ep}/indexes/{item['index']}/docs/search?api-version=2023-11-01"
    r = requests.post(url, headers={'api-key': key}, json={'search': '*', 'top': 0, 'count': True})
    
    if r.status_code == 200:
        count = r.json().get('@odata.count', 0)
        total_docs += count
        print(f"{item['index']:<35} {item['vector_field']:<25} {count:>10,} ⏳ TODO")
    else:
        print(f"{item['index']:<35} {item['vector_field']:<25} {'ERROR':>10} ❌")

print("-" * 80)
print(f"{'TOTAL TIER 2':<35} {'':<25} {total_docs:>10,}")

print(f"\n{'=' * 80}")
print("TIER 2 EXECUTION PLAN")
print("=" * 80)

print("""
APPROACH:
1. Check/create backfill scripts for each index
2. Run embeddings in order (smallest to largest for quick wins)
3. Use proven patterns from Tier 1:
   - Batch size: 32-64 (network stability)
   - Retry logic: 5 attempts with exponential backoff
   - Progress reporting every batch
   - OVERWRITE env var support

EXECUTION ORDER (quick wins first):
1. copybook_meta.summary_vector     (7,817 docs)   ~4 mins
2. calls.snippet_vector            (15,788 docs)   ~8 mins
3. copybook_usage.context_vector  (114,307 docs)  ~60 mins
4. paragraphs.para_vector         (224,655 docs) ~120 mins

ESTIMATED TOTAL TIME: ~3.2 hours

BATCH EXECUTION STRATEGY:
- Start with copybook_meta and calls (can run in parallel)
- Monitor network stability
- Proceed to larger indexes sequentially
""")

print("\n" + "=" * 80)
print("READY TO START TIER 2?")
print("=" * 80)
print("Run these commands in order (or parallel where noted):\n")
print("1. Copybook Meta (small, ~4 mins):")
print("   python backfill_embeddings_copybook_meta.py")
print("\n2. Calls (small-medium, ~8 mins):")
print("   python backfill_embeddings_calls.py")
print("\n3. Copybook Usage (medium, ~60 mins):")
print("   python backfill_embeddings_copybook_usage.py")
print("\n4. Paragraphs (large, ~120 mins):")
print("   python backfill_embeddings_paragraphs.py")
