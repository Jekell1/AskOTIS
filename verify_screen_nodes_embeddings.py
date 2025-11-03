"""Verify screen_nodes embeddings are complete"""
import os, sys, json, requests

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_settings()

ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

url = f"{ep}/indexes/new_cobol_screen_nodes/docs/search?api-version=2025-08-01-preview"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Check total count
body = {'search': '*', 'top': 0, 'count': True}
r = requests.post(url, headers=headers, json=body)
total = r.json().get('@odata.count', 0)

# Check has_vector=true count
body2 = {'search': '*', 'top': 0, 'count': True, 'filter': 'has_vector eq true'}
r2 = requests.post(url, headers=headers, json=body2)
embedded = r2.json().get('@odata.count', 0)

print(f"Total screen nodes: {total}")
print(f"With embeddings: {embedded}")
print(f"Coverage: {embedded/total*100:.1f}%")

if embedded == total:
    print("\n✓ SUCCESS: All screen nodes have embeddings!")
else:
    print(f"\n⚠ WARNING: {total - embedded} screens missing embeddings")
