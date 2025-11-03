import os, json, sys, time, requests

"""Export all documents from a given Azure AI Search index to a JSONL file.

Usage (PowerShell):
  pwsh> python export_index_docs.py --index cobol-files --out backup_cobol_files_pre_ingest.jsonl

Features:
 - Paginates with search * using skip/top (API enforced max 1000 per page)
 - Retries transient failures
 - Streams output so large indexes are safe
 - Optionally select only certain fields

Notes:
 - For very large indexes, a continuation token approach (search.after) would be better, but
   for current scale (< 20k docs) skip paging is acceptable.
 - Respects local.settings.json for SEARCH_ENDPOINT / SEARCH_KEY.
"""

API_VERSION = "2024-07-01"
PAGE_SIZE = 1000
RETRIES = 4

def load_settings():
    path = os.path.join(os.getcwd(), 'local.settings.json')
    if os.path.exists(path):
        try:
            data = json.load(open(path,'r',encoding='utf-8'))
            return data.get('Values', {})
        except Exception:
            return {}
    return {}

def get_endpoint_key():
    vals = load_settings()
    ep = (os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT') or
          vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT'))
    key = (os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY') or
           vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY'))
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY env or local.settings.json values', file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

def fetch_page(ep, key, index, skip, select):
    body = {"search": "*", "top": PAGE_SIZE, "skip": skip}
    if select:
        body['select'] = select
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    for attempt in range(RETRIES):
        r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body, timeout=60)
        if r.status_code == 200:
            return r.json().get('value', [])
        if r.status_code >= 500:
            time.sleep(1 + attempt)
            continue
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:200]}")
    raise RuntimeError('Exceeded retries for page fetch')

def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', required=True, help='Index name to export')
    ap.add_argument('--out', required=True, help='Output JSONL file path')
    ap.add_argument('--select', help='Comma-separated list of fields to select (optional)')
    args = ap.parse_args()

    ep, key = get_endpoint_key()
    select = None
    if args.select:
        # basic sanitize
        select = ','.join([p.strip() for p in args.select.split(',') if p.strip()])

    print(f"Exporting index '{args.index}' -> {args.out}")
    total = 0
    skip = 0
    with open(args.out, 'w', encoding='utf-8') as f:
        while True:
            page = fetch_page(ep, key, args.index, skip, select)
            if not page:
                break
            for doc in page:
                f.write(json.dumps(doc, ensure_ascii=False) + '\n')
            total += len(page)
            skip += len(page)
            print(f" {total} docs exported...")
            if len(page) < PAGE_SIZE:
                break
    print(f"Done. Exported {total} documents.")

if __name__ == '__main__':
    main()
