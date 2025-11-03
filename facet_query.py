import os, json, requests, sys, pathlib, traceback

def load_creds():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if ep and key:
        return ep, key
    # fallback to local.settings.json
    ls = pathlib.Path('local.settings.json')
    if ls.exists():
        try:
            data = json.loads(ls.read_text(encoding='utf-8'))
            vals = data.get('Values', {}) or {}
            ep = ep or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
            key = key or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
        except Exception as ex:
            print('Failed reading local.settings.json:', ex)
    return ep, key

endpoint, key = load_creds()
if not endpoint or not key:
    print('Missing endpoint/key (env + local.settings.json fallback failed).')
    sys.exit(2)

endpoint = endpoint.rstrip('/')
index = sys.argv[1] if len(sys.argv) > 1 else 'cobol-facts-v3l'
print(f"Faceting index '{index}' at service '{endpoint}'")
url = f"{endpoint}/indexes/{index}/docs/search?api-version=2024-05-01-preview"
body = {"search": "*", "facets": ["program_id,count:500"], "top": 0}
try:
    resp = requests.post(url, json=body, headers={'api-key': key, 'Content-Type': 'application/json'}, timeout=30)
    print('Status:', resp.status_code)
    if resp.status_code != 200:
        print('Error body:', resp.text[:1000])
        sys.exit(3)
    data = resp.json()
    facets = data.get('@search.facets') or data.get('facets') or {}
    prog_facet = facets.get('program_id') or []
    # Print top 30 by count
    sorted_f = sorted(prog_facet, key=lambda x: x.get('count',0), reverse=True)
    print('Top program_id counts:')
    for entry in sorted_f[:30]:
        print(f"  {entry.get('value')}: {entry.get('count')}")
    print(f"Total distinct programs returned: {len(prog_facet)}")
except Exception as ex:
    print('Facet query failed:', ex)
    traceback.print_exc()
    sys.exit(4)
