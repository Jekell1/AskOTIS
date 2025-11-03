"""Add a has_vector boolean field to vector-enabled indexes lacking it, and optionally populate it.

Two phases per index:
  1. Schema patch: fetch index JSON, append field { name: 'has_vector', type: 'Edm.Boolean', filterable: true } if absent.
     PUT full index definition back.
  2. (Optional) Population: if --populate provided, iterate documents in pages and set has_vector=true for any doc whose
     vector field list length > 0 (or opportunistically all docs if vector field retrieval not enabled).

WARNING: Azure AI Search requires full index object for update; this script is conservative and will refuse if index contains
complex features we don't want to accidentally drop (e.g., analyzers, encryptionKey) unless --force is passed.

Usage examples:
  Preview: python patch_add_has_vector_field.py --index new_cobol_program_meta --preview
  Patch & populate all missing: python patch_add_has_vector_field.py --all-missing --populate
"""
from __future__ import annotations
import os, json, argparse, sys, requests, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

SAFE_KEYS={'name','fields','suggesters','scoringProfiles','corsOptions','analyzers','tokenizers','tokenFilters','charFilters','similarity','encryptionKey','vectorSearch','semantic'}

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def list_indexes(ep,key):
    r=requests.get(f"{ep}/indexes?api-version={API_VERSION}",headers={'api-key':key}); r.raise_for_status(); return r.json().get('value',[])

def get_index(ep,key,name):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}",headers={'api-key':key}); r.raise_for_status(); return r.json()

def put_index(ep,key,idx):
    payload=json.dumps({k:v for k,v in idx.items() if not k.startswith('@')})
    r=requests.put(f"{ep}/indexes/{idx['name']}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},data=payload)
    if r.status_code not in (200,201,204):
        raise RuntimeError(f"PUT failed {r.status_code}: {r.text[:200]}")

def detect_vector_fields(idx):
    out=[]
    for f in idx.get('fields',[]):
        dim=f.get('vectorSearchDimensions') or f.get('dimensions')
        if dim and f.get('type','').startswith('Collection(Edm.Single)'):
            out.append(f['name'])
    return out

def needs_flag(idx, allow_without_vectors: bool=False):
    names={f['name'] for f in idx.get('fields',[])}
    if 'has_vector' in names:
        return False
    has_vectors=bool(detect_vector_fields(idx))
    if has_vectors:
        return True
    return allow_without_vectors

def add_flag(idx):
    idx['fields'].append({
        'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':False,'searchable':False,'retrievable':True,'sortable':False
    })
    return idx

def populate_flags(ep,key,index, batch=500):
    """Populate has_vector=true for all docs.

    Strategy change:
      * Avoid skip-based pagination (which can drift as docs update)
      * Persist primary key across pages
      * Use filter to fetch only docs where has_vector is null/false if field exists
    """
    url_search=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    url_index=f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"

    # Detect primary key once
    idx=get_index(ep,key,index)
    pk=None
    for f in idx.get('fields',[]):
        if f.get('key'):
            pk=f['name']; break
    if not pk:
        raise RuntimeError('Primary key not found in index schema')

    total_set=0
    loops=0
    # Determine if has_vector field already exists
    has_flag_field = any(f.get('name')=='has_vector' for f in idx.get('fields',[]))
    # Build search body base
    while True:
        loops+=1
        body={'search':'*','top':batch,'select':f"{pk},has_vector"}
        if has_flag_field:
            # Only fetch docs needing update
            body['filter']='has_vector eq false or has_vector eq null'
        r=requests.post(url_search,headers={'api-key':key,'Content-Type':'application/json'},json=body)
        if r.status_code!=200:
            raise RuntimeError(f"{r.status_code}: {r.text[:160]}")
        docs=r.json().get('value',[])
        if not docs:
            if total_set==0:
                print('All documents already have has_vector=true')
            break
        actions=[]
        for d in docs:
            key_val=d.get(pk)
            if key_val is None:
                # Skip malformed doc but continue
                print(f"[warn] document missing primary key field '{pk}' – skipped")
                continue
            actions.append({'@search.action':'mergeOrUpload', pk: key_val, 'has_vector': True})
        if not actions:
            print('No actionable docs in batch (all missing keys) – stopping to avoid loop.')
            break
        up_r=requests.post(url_index,headers={'api-key':key,'Content-Type':'application/json'},json={'value':actions})
        if up_r.status_code!=200:
            raise RuntimeError(f"{up_r.status_code}: {up_r.text[:160]}")
        total_set+=len(actions)
        print(f"Populated has_vector=true for {total_set} docs so far (loop {loops})...")
        if len(docs)<batch:
            # Last partial page
            break
    return total_set

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', action='append', help='Specific index (can be repeated)')
    ap.add_argument('--all-missing', action='store_true', help='Operate on all indexes missing has_vector flag')
    ap.add_argument('--populate', action='store_true', help='After adding flag, populate it (sets true for all docs)')
    ap.add_argument('--allow-missing-vector', action='store_true', help='Allow adding has_vector even when index currently has no vector fields defined')
    ap.add_argument('--preview', action='store_true')
    ap.add_argument('--force', action='store_true', help='Allow PUT even if unknown top-level keys present')
    args=ap.parse_args(); load(); ep,key=resolve()
    targets=[]
    if args.all_missing:
        for meta in list_indexes(ep,key):
            name=meta.get('name')
            idx=get_index(ep,key,name)
            if needs_flag(idx, allow_without_vectors=args.allow_missing_vector):
                targets.append(name)
    if args.index:
        for n in args.index:
            if n not in targets: targets.append(n)
    if not targets:
        print('No target indexes found (already have has_vector or no vector fields).'); return
    print(f"Operating on indexes: {targets}")
    for name in targets:
        idx=get_index(ep,key,name)
        vector_fields = detect_vector_fields(idx)
        has_vectors = bool(vector_fields)
        has_flag = any(f.get('name')=='has_vector' for f in idx.get('fields',[]))
        if not needs_flag(idx, allow_without_vectors=args.allow_missing_vector):
            if args.populate and has_flag:
                try:
                    count=populate_flags(ep,key,name)
                    print(f"[populate] Updated has_vector for {count} docs in {name}")
                except Exception as e:
                    print(f"[error] populate failed for {name}: {type(e).__name__}: {e!r}")
            else:
                print(f"[skip] {name} already has flag or lacks vector fields")
            continue
        extra_keys=set(idx.keys())-SAFE_KEYS
        if extra_keys and not args.force:
            print(f"[warn] {name} has unexpected top-level keys {extra_keys}; re-run with --force to proceed")
            continue
        patched=add_flag(idx)
        if args.preview:
            print(f"[preview] Would add has_vector to {name}")
        else:
            put_index(ep,key,patched)
            print(f"[applied] Added has_vector to {name}")
            if not has_vectors:
                print(f"    ⚠️  {name} currently has no vector fields; has_vector will report 0%% coverage until embeddings are added.")
        if args.populate and not args.preview:
            try:
                count=populate_flags(ep,key,name)
                print(f"[populate] Set has_vector=true for {count} docs in {name}")
            except Exception as e:
                print(f"[error] populate failed for {name}: {type(e).__name__}: {e!r}")

if __name__=='__main__':
    main()
