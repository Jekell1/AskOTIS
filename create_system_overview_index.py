"""Create the new_cobol_system_overview index.

Single-document (or very low cardinality) index capturing macro system metrics.
We allow multiple snapshots by keying on overview_id (e.g., 'latest', timestamp forms).

Fields:
  overview_id (key)
  generated_at (string ISO)
  program_count
  ui_program_count
  risk_program_count
  avg_coverage_pct
  coverage_bands_json         (JSON describing distribution buckets)
  role_distribution_json      (counts per program_role)
  top_central_programs_json   (array of {program_id, centrality_score})
  top_fan_out_programs_json   (array of {program_id, outgoing_count})
  top_fan_in_programs_json    (array of {program_id, incoming_count})
  external_programs_json      (unique external callees encountered)
  notes                       (optional text)

Usage:
  python create_system_overview_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests
from typing import Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_system_overview'


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def index_exists(ep: str, key: str) -> bool:
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200

def delete_index(ep: str, key: str):
    if index_exists(ep, key):
        r = requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
        if r.status_code not in (200,204):
            print(f"[WARN] Delete failed {r.status_code}: {r.text[:200]}")
        else:
            print('Deleted existing index.')

def create_index(ep: str, key: str, overwrite: bool):
    if index_exists(ep, key):
        if not overwrite:
            print('Index exists (use --overwrite to recreate).')
            return
        delete_index(ep, key)
    schema: Dict[str, Any] = {
        'name': INDEX_NAME,
        'fields': [
            {'name':'overview_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'facetable':False,'sortable':False},
            {'name':'generated_at','type':'Edm.String','filterable':True,'sortable':True},
            {'name':'program_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'ui_program_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'risk_program_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'avg_coverage_pct','type':'Edm.Double','filterable':True,'sortable':True},
            {'name':'coverage_bands_json','type':'Edm.String'},
            {'name':'role_distribution_json','type':'Edm.String'},
            {'name':'top_central_programs_json','type':'Edm.String'},
            {'name':'top_fan_out_programs_json','type':'Edm.String'},
            {'name':'top_fan_in_programs_json','type':'Edm.String'},
            {'name':'external_programs_json','type':'Edm.String'},
            {'name':'notes','type':'Edm.String'}
        ],
        'semantic': {
            'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':None,'prioritizedContentFields':[]}}]
        }
    }
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key': key,'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print(f"[FATAL] Create failed {r.status_code}: {r.text[:400]}")
        sys.exit(1)
    print(f"Created index {INDEX_NAME}")


def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_system_overview index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--overwrite', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    create_index(ep, key, args.overwrite)

if __name__ == '__main__':
    main()
