"""Ensure dependency_blob_vector field exists (idempotent add) for new_cobol_program_deps.

If the field already exists, script exits successfully.
Otherwise it re-creates the index schema with the field appended (Azure AI Search
requires full index recreation to add fields). NOTE: This will DROP existing
documents unless you export & re-import; we warn the user before proceeding.

Because our initial create script already includes the vector field, this script
mostly serves future schema evolution safety.
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'

def resolve(args):
  ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
  key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
  if not ep or not key:
    print('[FATAL] Missing endpoint/key'); sys.exit(1)
  return ep.rstrip('/'), key

def fetch_index(ep,key):
  r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
  if r.status_code!=200:
    print('[FATAL] fetch failed',r.status_code,r.text[:200]); sys.exit(1)
  return r.json()

def recreate(ep,key,definition):
  # Delete then PUT (destructive)
  d=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
  if d.status_code not in (200,204):
    print('[FATAL] delete failed',d.status_code,d.text[:200]); sys.exit(1)
  c=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=definition)
  if c.status_code not in (200,201):
    print('[FATAL] recreate failed',c.status_code,c.text[:300]); sys.exit(1)
  print('[OK] Recreated index with vector field (documents were reset)')

def main():
  ap=argparse.ArgumentParser(description='Add vector field to program deps index if missing')
  ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--force',action='store_true')
  args=ap.parse_args(); ep,key=resolve(args)
  data=fetch_index(ep,key)
  fields=data.get('fields',[])
  present=any(f.get('name')=='dependency_blob_vector' for f in fields)
  if present:
    print('[OK] dependency_blob_vector already present; nothing to do.')
    return
  if not args.force:
    print('[WARN] Field missing; re-create would drop documents. Re-run with --force to proceed.')
    return
  # Append field definition
  fields.append({'name':'dependency_blob_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':3072,'vectorSearchProfile':'vprofile'})
  data['fields']=fields
  recreate(ep,key,data)

if __name__=='__main__':
  main()
