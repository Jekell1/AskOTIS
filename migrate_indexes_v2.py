#!/usr/bin/env python3
"""migrate_indexes_v2.py

Shadow-migrate selected Azure AI Search indexes to new *-v2 versions that include
added enrichment fields without recomputing embeddings.

Current target indexes:
  - cobol-paragraphs -> cobol-paragraphs-v2
  - cobol-copybooks  -> cobol-copybooks-v2
  - cobol-facts      -> cobol-facts-v2

Steps per index:
 1. Fetch existing index definition
 2. Patch fields list to include enrichment fields (if not already present)
 3. Create new index (fail if exists unless --force)
 4. Export all docs (paged) using search (select=*)
 5. Upload to new index (carry vector arrays)
 6. (Optional) Assign an alias if --alias supplied (will create or move alias)

After migration run enrichment scripts against the *-v2 indexes (adjust their constants or add --suffix flag later if needed).

IMPORTANT: Requires admin key.
"""
import os, sys, json, time, argparse, requests, copy
from typing import Dict, List

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
ENDPOINT = os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT")
KEY = os.environ.get("AZURE_SEARCH_KEY") or os.environ.get("SEARCH_KEY")

session = requests.Session()
HEADERS = {"api-key": KEY, "Content-Type": "application/json"}

ENRICH_FIELDS = {
    "cobol-paragraphs": [
        {"name": "role", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
        {"name": "phase", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
        {"name": "phase_seq", "type": "Edm.Int32", "filterable": True, "sortable": True },
        {"name": "role_confidence", "type": "Edm.Double", "filterable": True, "sortable": True },
        {"name": "phase_confidence", "type": "Edm.Double", "filterable": True, "sortable": True },
    ],
    "cobol-copybooks": [
        {"name": "canonical_base", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
        {"name": "variant_kind", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
        {"name": "variant_group_id", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
    ],
    "cobol-facts": [
        {"name": "action_role", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
        {"name": "posting_type", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True },
        {"name": "gating_cond", "type": "Edm.String", "searchable": True },
        {"name": "fact_confidence", "type": "Edm.Double", "filterable": True, "sortable": True },
        {"name": "sources", "type": "Collection(Edm.String)", "searchable": False, "filterable": True },
    ],
}

def get_index_def(name: str) -> Dict:
    url = f"{ENDPOINT}/indexes/{name}?api-version={API_VERSION}"
    r = session.get(url, headers=HEADERS, timeout=30)
    if r.status_code == 404:
        raise SystemExit(f"Index {name} not found")
    r.raise_for_status()
    return r.json()


def field_exists(fields: List[Dict], fname: str) -> bool:
    return any(f.get('name') == fname for f in fields)


def patch_fields(src_def: Dict, base_name: str) -> Dict:
    new_def = copy.deepcopy(src_def)
    new_def['name'] = base_name + '-v2'
    added = []
    enrich = ENRICH_FIELDS.get(base_name, [])
    for f in enrich:
        if not field_exists(new_def['fields'], f['name']):
            new_def['fields'].append(f)
            added.append(f['name'])
    return new_def, added


def create_index(defn: Dict, force: bool):
    url = f"{ENDPOINT}/indexes/{defn['name']}?api-version={API_VERSION}"
    r = session.get(url, headers=HEADERS, timeout=20)
    if r.status_code == 200:
        if not force:
            print(f"Index {defn['name']} already exists (skip). Use --force to overwrite.")
            return False
        # delete first
        dr = session.delete(url, headers=HEADERS, timeout=30)
        if dr.status_code >= 300:
            raise SystemExit(f"Failed delete existing {defn['name']}: {dr.status_code} {dr.text[:200]}")
        time.sleep(2)
    # remove read-only props
    for k in ['@odata.etag']:
        if k in defn:
            del defn[k]
    cr = session.put(url, headers=HEADERS, json=defn, timeout=120)
    if cr.status_code >= 300:
        raise SystemExit(f"Create failed {defn['name']}: {cr.status_code} {cr.text[:300]}")
    print(f"Created index {defn['name']}")
    return True


def export_docs(name: str, top=1000):
    url = f"{ENDPOINT}/indexes/{name}/docs/search?api-version={API_VERSION}"
    docs = []
    next_params = None
    while True:
        body = {"search": "*", "top": top, "select": "*"}
        if next_params:
            body['@search.nextPageParameters'] = next_params
        r = session.post(url, headers=HEADERS, json=body, timeout=90)
        r.raise_for_status()
        data = r.json()
        batch = data.get('value', [])
        docs.extend(batch)
        next_params = data.get('@search.nextPageParameters')
        if not next_params or not batch:
            break
    print(f"Exported {len(docs)} docs from {name}")
    return docs


def upload_docs(name: str, docs: List[Dict]):
    if not docs:
        return
    url = f"{ENDPOINT}/indexes/{name}/docs/index?api-version={API_VERSION}"
    for i in range(0, len(docs), 900):
        batch_docs = docs[i:i+900]
        # Remove scoring/stat fields
        cleaned = []
        for d in batch_docs:
            for k in list(d.keys()):
                if k.startswith('@search.'):
                    del d[k]
            cleaned.append({"@search.action": "upload", **d})
        payload = {"value": cleaned}
        r = session.post(url, headers=HEADERS, json=payload, timeout=180)
        if r.status_code >= 300:
            raise SystemExit(f"Upload error {r.status_code} {r.text[:300]}")
        print(f"Uploaded {len(cleaned)} to {name}")


def attach_alias(alias: str, new_index: str):
    # Maintains alias mapping by replacing if exists
    url = f"{ENDPOINT}/aliases/{alias}?api-version={API_VERSION}"
    body = {"name": alias, "indexes": [new_index]}
    r = session.put(url, headers=HEADERS, json=body, timeout=30)
    if r.status_code >= 300:
        raise SystemExit(f"Alias attach failed {alias}: {r.status_code} {r.text[:200]}")
    print(f"Alias {alias} -> {new_index}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--force', action='store_true', help='Overwrite existing *-v2 indexes')
    ap.add_argument('--alias', action='store_true', help='Create/update alias with base name')
    ap.add_argument('--indexes', nargs='*', default=['cobol-paragraphs','cobol-copybooks','cobol-facts'])
    args = ap.parse_args()
    if not ENDPOINT or not KEY:
        print('Missing AZURE_SEARCH_ENDPOINT or AZURE_SEARCH_KEY env vars', file=sys.stderr)
        sys.exit(2)
    for base in args.indexes:
        print(f"=== Migrating {base} -> {base}-v2 ===")
        src = get_index_def(base)
        new_def, added = patch_fields(src, base)
        if added:
            print(f"Added fields: {added}")
        else:
            print("No new fields needed (maybe already present?)")
        created = create_index(new_def, force=args.force)
        if created:
            docs = export_docs(base)
            upload_docs(base+'-v2', docs)
            if args.alias:
                attach_alias(base, base+'-v2')
        else:
            print(f"Skipped creation for {base}-v2 (already exists)")
    print("Done.")

if __name__ == '__main__':
    main()
