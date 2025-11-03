"""Create an Azure AI Search index to hold a program inventory for enrichment.

Fields:
  program_id              (key) canonical program name
  copybook_count          number of distinct copybooks referenced
  paragraph_count         number of distinct paragraphs detected
  usage_rows              total usage rows seen (from new_cobol_copybook_usage)
  last_ingested_at        timestamp
  metrics_json            serialized metrics (flex field)

Run:
  python create_program_inventory_index.py --force
"""
from __future__ import annotations
import os, json, argparse, requests, time

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_inventory'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def delete_index(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})
    if r.status_code not in (200,204,404):
        raise SystemExit(f"Delete failed {r.status_code}: {r.text[:300]}")

SCHEMA = {
  "name": INDEX,
  "fields": [
    {"name":"program_id","type":"Edm.String","key":True,"searchable":False,"filterable":True,"facetable":False,"sortable":False},
    {"name":"copybook_count","type":"Edm.Int32","filterable":True,"facetable":False,"sortable":True},
    {"name":"paragraph_count","type":"Edm.Int32","filterable":True,"facetable":False,"sortable":True},
    {"name":"usage_rows","type":"Edm.Int32","filterable":True,"facetable":False,"sortable":True},
    {"name":"last_ingested_at","type":"Edm.String","searchable":False,"filterable":True,"sortable":True},
    {"name":"metrics_json","type":"Edm.String","searchable":False,"filterable":False,"facetable":False,"sortable":False}
  ]
}

def create(ep,key):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=SCHEMA,timeout=60)
    if r.status_code not in (200,201):
        raise SystemExit(f"Create failed {r.status_code}: {r.text[:400]}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--force',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    if args.force:
        print('Deleting existing index (if any)...')
        delete_index(ep,key)
    print('Creating index...')
    create(ep,key)
    print('Done.')

if __name__=='__main__':
    main()
