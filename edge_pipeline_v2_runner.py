"""Orchestrate creation + extraction + upload + embedding for new_cobol_flow_edges_v2.

Steps:
 1. Create index (idempotent skip on 409)
 2. Run extractor (produces JSONL/flow_edges_v2.jsonl)
 3. Upload pilot subset (limit) for validation
 4. Probe sample
 5. If --full then upload all and run embeddings backfill
"""
import os, subprocess, sys, json, argparse, time, shutil, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'

CREATE_SCRIPT='create_flow_edges_index_v2.py'
EXTRACT_SCRIPT='extract_flow_edges_v2.py'
UPLOAD_SCRIPT='upload_flow_edges_v2.py'
PROBE_SCRIPT='probe_flow_edges_v2.py'
EMB_SCRIPT='backfill_edge_vectors_v2.py'


def run_py(script, *args):
    if not os.path.exists(script):
        raise SystemExit(f'Missing script {script}')
    cmd=[sys.executable, script, *args]
    print('Running', ' '.join(cmd))
    res=subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode!=0:
        print(res.stdout)
        print(res.stderr)
        raise SystemExit(f'{script} failed')
    return res.stdout.strip()


def load_settings():
    """Load local.settings.json Values into environment if present."""
    try:
        data=json.load(open('local.settings.json','r',encoding='utf-8'))
        vals=data.get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def index_exists():
    load_settings()
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Skipping index existence check (missing endpoint/key).')
        return False
    url=f"{ep.rstrip('/')}/indexes/{INDEX}?api-version={API}"
    try:
        r=requests.get(url,headers={'api-key':key},timeout=30)
        return r.status_code==200
    except Exception as ex:
        print(f'Index existence check failed: {ex}')
        return False


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--full',action='store_true',help='Run full upload + embedding')
    ap.add_argument('--pilot-limit',type=int,default=20000)
    ap.add_argument('--skip-extract',action='store_true')
    ap.add_argument('--skip-create',action='store_true')
    ap.add_argument('--skip-upload',action='store_true')
    ap.add_argument('--skip-embed',action='store_true')
    ap.add_argument('--preview-embed',action='store_true')
    args=ap.parse_args()

    # 1. Create index
    if not args.skip_create:
        if index_exists():
            print(f'Index {INDEX} already exists - leaving as is.')
        else:
            run_py(CREATE_SCRIPT)
    # 2. Extract
    if not args.skip_extract:
        run_py(EXTRACT_SCRIPT)
    # 3. Upload
    if not args.skip_upload:
        if args.full:
            run_py(UPLOAD_SCRIPT, '--limit','0')
        else:
            run_py(UPLOAD_SCRIPT, '--limit', str(args.pilot_limit))
    # 4. Probe
    print(run_py(PROBE_SCRIPT, '--top','10'))
    # 5. Embeddings
    if not args.skip_embed:
        if args.full:
            if args.preview_embed:
                run_py(EMB_SCRIPT,'--preview')
            else:
                run_py(EMB_SCRIPT)
        else:
            print('Skipping embeddings (pilot mode).')

    print('Edge pipeline v2 complete.')

if __name__=='__main__':
    main()
