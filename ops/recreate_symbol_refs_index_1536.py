"""Orchestrate recreation of new_cobol_symbol_refs at reduced vector dimension (default 1536) and optional ingest + backfill.

Usage examples:
  # Just recreate index (delete + create) at 1536
  python ops/recreate_symbol_refs_index_1536.py --force-delete

  # Recreate, ingest refs from sources, then force backfill embeddings
  python ops/recreate_symbol_refs_index_1536.py --force-delete --ingest --backfill --ingest-src-root cobol_src

  # Recreate with custom dimension 1024
  python ops/recreate_symbol_refs_index_1536.py --force-delete --dimension 1024

  # Recreate and run backfill using 4 shards (hex) embedding batch 96
  python ops/recreate_symbol_refs_index_1536.py --force-delete --backfill --shards 4 --embed-batch 96

This script intentionally keeps network operations simple (requests) and delegates heavy lifting to
existing scripts (create_symbol_refs_index.py, enrich_variable_usage_refs.py, backfill_symbol_ref_embeddings.py).
"""
from __future__ import annotations
import os, sys, argparse, json, time, requests, subprocess, pathlib

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
VECTOR_FIELD='excerpt_vector'

# ---------------- helpers -----------------

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key (ensure local.settings.json or env vars).')
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch_index(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code==200:
        return r.json()
    return None

def current_dimension(ep,key):
    data=fetch_index(ep,key)
    if not data: return None
    for f in data.get('fields',[]):
        if f.get('name')==VECTOR_FIELD:
            return f.get('dimensions')
    return None

def delete_index(ep,key):
    try:
        r=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key},timeout=120)
    except KeyboardInterrupt:
        print('\n[ABORT] Delete interrupted by user. Index may or may not have been removed. Re-run with --force-delete to ensure clean state.')
        sys.exit(1)
    except Exception as e:
        print('[FATAL] Delete request failed:', e); sys.exit(1)
    if r.status_code not in (200,204,404):
        print('[FATAL] Delete failed', r.status_code, r.text[:300]); sys.exit(1)
    if r.status_code in (200,204):
        print('[OK] Deleted existing index')
    else:
        print('[INFO] Index did not exist (nothing to delete)')


def create_index(ep,key,dimension:int):
    schema={
        'name': INDEX,
        'fields':[{'name':'ref_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'retrievable':True},
                  {'name':'program_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'symbol_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'symbol_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'symbol_id_global','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'kind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'op','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'normalized_kind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'line_number','type':'Edm.Int32','searchable':False,'filterable':True,'retrievable':True},
                  {'name':'file_path','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
                  {'name':'paragraph_name','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'excerpt_text','type':'Edm.String','searchable':True,'filterable':False,'retrievable':True},
                  {'name':'context_before','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
                  {'name':'context_after','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
                  {'name':VECTOR_FIELD,'type':'Collection(Edm.Single)','searchable':True,'dimensions':dimension,'vectorSearchProfile':'ref-vector-profile'},
                  {'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'first_in_program','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'is_first_write','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'cluster_key','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
                  {'name':'ingested_at','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True},
                  # Added to align with enrichment script which emits 'updated_at'
                  {'name':'updated_at','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True}],
        'vectorSearch':{'algorithms':[{'name':'ref-hnsw','kind':'hnsw'}],'profiles':[{'name':'ref-vector-profile','algorithm':'ref-hnsw'}]},
    'semantic':{'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':None,'prioritizedContentFields':[{'fieldName':'excerpt_text'}]}}]},
        'corsOptions':{'allowedOrigins':['*']}
    }
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('[FATAL] Create failed', r.status_code, r.text[:400]); sys.exit(1)
    print(f"[OK] Created index {INDEX} dimension={dimension}")

# --------------- Orchestration ---------------

def run_cmd(parts:list[str]):
    print('[RUN]', ' '.join(parts))
    rc=subprocess.call(parts)
    if rc!=0:
        print(f'[FATAL] Command failed rc={rc}: {parts}')
        sys.exit(rc)


def main():
    ap=argparse.ArgumentParser(description='Recreate symbol refs index at reduced dimension and optionally ingest/backfill')
    ap.add_argument('--dimension',type=int,default=int(os.getenv('SYMBOL_REF_VECTOR_DIM','1536')))  # orchestrator default
    ap.add_argument('--force-delete',action='store_true',help='Delete existing index before create')
    ap.add_argument('--skip-create',action='store_true',help='Skip delete/create (e.g., only run backfill)')
    ap.add_argument('--ingest',action='store_true',help='Run enrichment ingestion after (re)create')
    ap.add_argument('--ingest-src-root',default='cobol_src',help='Source root for ingestion script')
    ap.add_argument('--backfill',action='store_true',help='Run embedding backfill after ingestion')
    ap.add_argument('--shards',type=int,default=0,help='If >0, launch that many shard processes sequentially (hex shard mode)')
    ap.add_argument('--embed-batch',type=int,default=64)
    ap.add_argument('--dry-run',action='store_true',help='Print intended actions only')
    args=ap.parse_args(); load_local(); ep,key=resolve()

    # Propagate dimension via env so called scripts use it
    os.environ['SYMBOL_REF_VECTOR_DIM']=str(args.dimension)

    if not args.skip_create:
        dim=current_dimension(ep,key)
        if dim is not None:
            print(f"[INFO] Existing index dimension={dim}")
            if dim==args.dimension and not args.force_delete:
                print('[INFO] Dimension already matches requested and --force-delete not set; skipping recreate.')
            else:
                if not args.force_delete:
                    print('[ABORT] Index exists; use --force-delete to drop it.')
                    sys.exit(1)
                if args.dry_run:
                    print('[DRY-RUN] Would delete index')
                else:
                    delete_index(ep,key)
        if args.force_delete or dim is None:
            if args.dry_run:
                print(f'[DRY-RUN] Would create index with dimension {args.dimension}')
            else:
                create_index(ep,key,args.dimension)

    if args.ingest:
        ingest_cmd=['python','ingest/enrich_variable_usage_refs.py','--push','--src-root',args.ingest_src_root]
        if args.dry_run:
            print('[DRY-RUN] Would run:', ' '.join(ingest_cmd))
        else:
            run_cmd(ingest_cmd)

    if args.backfill:
        backfill_base=['python','backfill_symbol_ref_embeddings.py','--force','--embed-batch',str(args.embed_batch)]
        if args.shards and args.shards>0:
            # Run shards sequentially (user may parallelize manually if desired)
            for i in range(args.shards):
                shard_cmd=backfill_base + ['--hex-shards',str(args.shards),'--hex-shard-index',str(i)]
                if args.dry_run:
                    print('[DRY-RUN] Would run shard:', ' '.join(shard_cmd))
                else:
                    run_cmd(shard_cmd)
        else:
            if args.dry_run:
                print('[DRY-RUN] Would run:', ' '.join(backfill_base))
            else:
                run_cmd(backfill_base)

    print('[DONE] Orchestration complete.')

if __name__=='__main__':
    main()
