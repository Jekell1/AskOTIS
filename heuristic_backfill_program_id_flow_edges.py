"""Heuristic backfill of missing program_id in new_cobol_flow_edges.

Strategy:
 1. Collect known program ids from new_cobol_program_meta (optional cache to file).
 2. Page through edges where program_id eq '' (empty) in batches of 1000.
 3. Derive candidate = upper(edge_id.split('-')[0]). If candidate in known set -> patch.
 4. Optionally fall back to file_id if present and in known set.
 5. Upload mergeOrUpload docs in sub-batches (default 500) until limit reached.

Safe: Idempotent; only touches docs with blank program_id.

Usage:
  python heuristic_backfill_program_id_flow_edges.py --limit 50000 --batch 500
  python heuristic_backfill_program_id_flow_edges.py --dry-run --limit 2000
"""
from __future__ import annotations
import os, json, argparse, requests, sys, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
EDGE_INDEX='new_cobol_flow_edges'
META_INDEX='new_cobol_program_meta'
PAGE=1000

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch_program_ids(ep,key):
    out=set(); skip=0
    while True:
        body={'search':'*','top':1000,'skip':skip,'select':'program_id'}
        r=requests.post(f"{ep}/indexes/{META_INDEX}/docs/search?api-version={API_VERSION}",
                        headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200:
            print('[WARN] meta search failed', r.status_code, r.text[:160]); break
        vals=r.json().get('value',[])
        if not vals: break
        for v in vals:
            pid=(v.get('program_id') or '').upper()
            if pid: out.add(pid)
        if len(vals)<1000: break
        skip+=1000
    return out

def fetch_edges_missing_program(ep,key, skip):
    body={'search':'*','filter':"program_id eq ''",'top':PAGE,'skip':skip,'select':'edge_id,file_id,program_id'}
    r=requests.post(f"{ep}/indexes/{EDGE_INDEX}/docs/search?api-version={API_VERSION}",
                    headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Edge search failed {r.status_code}: {r.text[:160]}")
    return r.json().get('value',[])

def upload_patches(ep,key, docs):
    if not docs: return
    payload={'value':docs}
    r=requests.post(f"{ep}/indexes/{EDGE_INDEX}/docs/index?api-version={API_VERSION}",
                    headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:160]}")

def derive(edge_id):
    if not edge_id: return None
    part=edge_id.split('-')[0].upper()
    if part and part.isalnum():
        return part
    return None

def main():
    ap=argparse.ArgumentParser(description='Heuristic backfill program_id in flow edges.')
    ap.add_argument('--limit',type=int,help='Max docs to patch')
    ap.add_argument('--batch',type=int,default=500,help='Upload sub-batch size')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--no-program-cache',action='store_true',help='Skip writing program cache file')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    print('Collecting known program ids...')
    progs=fetch_program_ids(ep,key)
    print('Known programs:', len(progs))
    if not args.no_program_cache:
        json.dump(sorted(progs), open('program_ids_cache.json','w'), indent=2)
    skip=0; total_patched=0; scanned=0
    start=time.time();
    while True:
        rows=fetch_edges_missing_program(ep,key, skip)
        if not rows: break
        skip+=len(rows)
        scanned+=len(rows)
        out=[]
        for r in rows:
            if args.limit and total_patched>=args.limit: break
            if r.get('program_id'): continue
            edge_id=r.get('edge_id'); file_id=(r.get('file_id') or '').upper()
            cand=derive(edge_id)
            pid=None
            if cand and cand in progs:
                pid=cand
            elif file_id and file_id in progs:
                pid=file_id
            if pid:
                out.append({'@search.action':'mergeOrUpload','edge_id':edge_id,'program_id':pid})
        # Upload in sub-batches
        for i in range(0,len(out),args.batch):
            chunk=out[i:i+args.batch]
            if not chunk: continue
            if args.dry_run:
                print('[DRY-RUN] would upload', len(chunk))
            else:
                upload_patches(ep,key,chunk)
            total_patched+=len(chunk)
            if args.limit and total_patched>=args.limit: break
        print(f"scanned={scanned} patched={total_patched} last_page={len(rows)} skip={skip}")
        if args.limit and total_patched>=args.limit: break
        if len(rows)<PAGE: break
    elapsed=time.time()-start
    print(f"Done. Patched {total_patched} docs in {elapsed:.1f}s (scanned {scanned})")

if __name__=='__main__':
    main()
