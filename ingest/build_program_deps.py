"""Materialize per-program dependency docs for new_cobol_program_deps (SPEC VERSION).

Behavior:
    * Discover program universe via program_catalog.discover_program_ids(endpoint,key,src_root).
    * Aggregate calls_out (caller -> callee) & calls_in from new_cobol_calls (if index exists).
    * Aggregate copybooks_used from new_cobol_copybook_usage (if index exists) using copybook_name or copybook_name_plain.
    * external_programs = outbound call targets not in discovered universe.
    * Always create one document per discovered program (leaf: calls_out_json == "[]").
    * Compute counts: outgoing_count, incoming_count, external_count, copybook_count.
    * Build dependency_blob narrative (concise, deterministic) and optionally embed inline (--embed-inline) into dependency_blob_vector (dim=3072) else leave has_vector False for later backfill.
    * Idempotent (mergeOrUpload) in batched uploads with retry + exponential backoff.
    * Flags (required set): --src-root | --all | --only | --resume-from | --batch | --embed-inline | --dry-run.

Acceptance:
    * Coverage ≥95% (docs / discovered programs).
    * If --embed-inline used, vector coverage ≥90%.

Example:
    python ingest/build_program_deps.py --src-root cobol_src --all --batch 500 --embed-inline
"""
from __future__ import annotations
import os, sys, json, argparse, datetime, requests, time, pathlib, random
from typing import Dict, Set, List, Tuple

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
CALLS_INDEX='new_cobol_calls'
USAGE_INDEX='new_cobol_copybook_usage'
OUTPUT_INDEX='new_cobol_program_deps'
VECTOR_DIM=int(os.getenv('PROGRAM_DEPS_VECTOR_DIM','3072'))

from ingest import program_catalog as catalog
from embedding_utils import batch_embed

# Root path (repo) for settings auto-load
ROOT = pathlib.Path(__file__).resolve().parents[1]

def load_local_settings():
    """Load local.settings.json (preferred) or local.settings.template.json into env.

    Does not overwrite pre-existing env vars so manual exports take precedence.
    Mirrors logic in diagnostics/compare_program_catalog_vs_deps.py for consistency.
    """
    for candidate in ('local.settings.json','local.settings.template.json'):
        fp = ROOT / candidate
        if not fp.is_file():
            continue
        try:
            data = json.load(fp.open('r', encoding='utf-8'))
            vals = data.get('Values', {})
            loaded = 0
            for k,v in vals.items():
                if k not in os.environ and isinstance(v,str) and v:
                    os.environ[k] = v
                    loaded += 1
            print(f"[settings] loaded {loaded} value(s) from {candidate}")
            break
        except Exception as e:  # pragma: no cover
            print(f"[settings] failed to parse {candidate}: {e}", file=sys.stderr)
            continue

def resolve_search(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('[FATAL] Missing search endpoint/key (pass --endpoint/--key or set env)')
    return ep.rstrip('/'), key

def search_post(ep,key,index,body):
    url=f"{ep}/indexes/{index}/docs/search.post.search?api-version={API_VERSION}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code==404:
        # Treat missing source index as empty; caller will handle gracefully
        return {'value':[]}
    if r.status_code!=200:
        raise SystemExit(f"[FATAL] search {index} failed {r.status_code}: {r.text[:300]}")
    return r.json()

def fetch_all_calls(ep,key)->List[dict]:
    out=[]; skip=0; page=1000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'caller_program,callee_program'}
        js=search_post(ep,key,CALLS_INDEX,body)
        vals=js.get('value',[])
        if not vals: break
        out.extend(vals)
        if len(vals)<page: break
        skip+=len(vals)
    return out

def fetch_copybook_usage(ep,key)->List[dict]:
    out=[]; skip=0; page=1000; MAX_SKIP=100000
    # Attempt to select both possible field names; some services reject unknown selects with 400.
    selectors=[
        'program_id,copybook_name,copybook_name_plain',
        'program_id,copybook_name'
    ]
    selector_idx=0
    while True:
        if skip>=MAX_SKIP:
            print(f"[WARN] Truncating copybook usage fetch at skip={skip} (service limit). Retrieved {len(out)} rows.")
            break
        body={'search':'*','top':page,'skip':skip,'select':selectors[selector_idx]}
        try:
            js=search_post(ep,key,USAGE_INDEX,body)
        except SystemExit as e:
            # If first selector caused 400 due to copybook_name_plain, downgrade and retry once
            msg=str(e)
            if 'copybook_name_plain' in selectors[selector_idx] and selector_idx==0 and '400' in msg:
                print('[INFO] Retrying copybook usage fetch without copybook_name_plain field (not present in index).')
                selector_idx=1
                skip=0
                out=[]
                continue
            raise
        vals=js.get('value',[])
        if not vals:
            break
        out.extend(vals)
        if len(vals)<page:
            break
        skip+=len(vals)
    return out

def fetch_existing_program_deps_ids(ep,key)->Set[str]:
    out:set[str]=set(); skip=0; page=1000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'program_id'}
        js=search_post(ep,key,OUTPUT_INDEX,body)
        vals=js.get('value',[])
        if not vals: break
        for v in vals:
            pid=v.get('program_id')
            if pid: out.add(pid.upper())
        if len(vals)<page: break
        skip+=len(vals)
    return out

def fetch_program_deps_docs(ep,key)->List[dict]:
    """Fetch all program deps docs (program_id, has_vector, dependency_blob) for backfill operations."""
    out=[]; skip=0; page=1000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'program_id,has_vector,dependency_blob'}
        js=search_post(ep,key,OUTPUT_INDEX,body)
        vals=js.get('value',[])
        if not vals: break
        out.extend(vals)
        if len(vals)<page: break
        skip+=len(vals)
    return out

def build_graph(calls:List[dict], normalizer) -> Tuple[Dict[str,Set[str]], Dict[str,Set[str]]]:
    outgoing: Dict[str,Set[str]]={}
    incoming: Dict[str,Set[str]]={}
    for row in calls:
        caller = normalizer(row.get('caller_program','') or '')
        callee = normalizer(row.get('callee_program','') or '')
        if not caller or not callee: continue
        if caller==callee: continue
        outgoing.setdefault(caller,set()).add(callee)
        incoming.setdefault(callee,set()).add(caller)
    return outgoing, incoming

def build_copybook_map(usage_rows:List[dict], normalizer)->Dict[str,Set[str]]:
    m:Dict[str,Set[str]]={}
    for r in usage_rows:
        p = normalizer(r.get('program_id','') or '')
        c = (r.get('copybook_name') or r.get('copybook_name_plain') or '')
        if not p or not c: continue
        m.setdefault(p,set()).add(normalizer(c))
    return m

def compose_blob(prog:str, co:Set[str], ci:Set[str], ext:Set[str], cbs:Set[str])->str:
    # Keep under ~500 chars for embedding efficiency.
    parts=[]
    parts.append(f"Program {prog} has {len(co)} outgoing and {len(ci)} incoming static call(s).")
    if co:
        parts.append(f"Calls: {', '.join(sorted(list(co))[:12])}{'...' if len(co)>12 else ''}.")
    if ci:
        parts.append(f"Called by: {', '.join(sorted(list(ci))[:12])}{'...' if len(ci)>12 else ''}.")
    if ext:
        parts.append(f"External targets: {', '.join(sorted(list(ext))[:10])}{'...' if len(ext)>10 else ''}.")
    if cbs:
        parts.append(f"Uses {len(cbs)} copybook(s): {', '.join(sorted(list(cbs))[:10])}{'...' if len(cbs)>10 else ''}.")
    return ' '.join(parts)

def chunk(seq, n):
    for i in range(0,len(seq),n):
        yield seq[i:i+n]

def upsert_batch(ep,key,docs:List[dict]):
    if not docs: return
    url=f"{ep}/indexes/{OUTPUT_INDEX}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    for attempt in range(0,3):
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=120)
        if r.status_code in (200,201):
            return
        if r.status_code>=500 and attempt<2:
            backoff=0.4*(2**attempt)+random.random()*0.2
            print(f"[WARN] upsert retry {attempt+1} status={r.status_code} sleep={backoff:.2f}s")
            time.sleep(backoff)
            continue
        print('[ERROR] upsert failed',r.status_code,r.text[:300]); return

def embed_blobs_inline(blobs:List[str], embed_batch:int) -> List[List[float]]:
    return batch_embed(blobs, batch_size=embed_batch, target_dim=VECTOR_DIM)

def main():
    ap=argparse.ArgumentParser(description='Build program dependency docs (spec-compliant).')
    ap.add_argument('--endpoint'); ap.add_argument('--key')
    ap.add_argument('--src-root',default='cobol_src')
    group=ap.add_mutually_exclusive_group(required=True)
    group.add_argument('--all',action='store_true')
    group.add_argument('--only',help='Comma-separated subset')
    ap.add_argument('--resume-from',help='Skip lexicographically < PROGRAM_ID (normalized)')
    ap.add_argument('--batch',type=int,default=500,help='Upload & embedding batch size')
    ap.add_argument('--embed-batch',type=int,default=64,help='Micro-batch size for embeddings')
    ap.add_argument('--embed-inline',action='store_true',help='Embed dependency_blob inline')
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args()

    load_local_settings()

    # Discover full universe (programs + copybooks for completeness/metrics)
    programs = catalog.discover_program_ids(args.endpoint, args.key, args.src_root, disable_index=False)
    copybooks_universe = catalog.discover_copybook_names(args.endpoint, args.key, args.src_root, disable_index=False)
    prog_set = set(programs)
    if args.only:
        subset={p.strip().upper() for p in args.only.split(',') if p.strip()}
        programs=[p for p in programs if p in subset]
    elif not args.all:
        print('[FATAL] Provide --all or --only'); sys.exit(1)
    if args.resume_from:
        resume_norm=catalog.normalize_name(args.resume_from)
        programs=[p for p in programs if p>=resume_norm]
    print(f"[INFO] Target program count: {len(programs)} (universe {len(prog_set)}) | copybooks discovered: {len(copybooks_universe)}")

    # Resolve search & fetch dependency sources
    ep,key=resolve_search(args)
    existing_deps=fetch_existing_program_deps_ids(ep,key)  # used for idempotent skip if doc identical? (not heavy)
    print('[INFO] Fetching call edges...')
    calls=fetch_all_calls(ep,key)
    outgoing,incoming=build_graph(calls,catalog.normalize_name)
    print(f"[INFO] Loaded {len(calls)} call edges (programs w/ outgoing: {len(outgoing)})")
    print('[INFO] Fetching copybook usage...')
    usage_rows=fetch_copybook_usage(ep,key)
    copybook_map=build_copybook_map(usage_rows,catalog.normalize_name)
    print(f"[INFO] Loaded {len(usage_rows)} usage rows (programs w/ copybooks: {len(copybook_map)})")

    # Inline embedding uses batch_embed; no external endpoint config required.

    docs_to_upsert=[]
    now=datetime.datetime.utcnow().isoformat()+'Z'
    processed=0
    for prog in programs:
        if not prog:  # skip empty normalization artifacts
            continue
        co=outgoing.get(prog,set())
        ci=incoming.get(prog,set())
        cbs=copybook_map.get(prog,set())
        external={c for c in co if c not in prog_set}
        blob=compose_blob(prog,co,ci,external,cbs)
        doc={
            'program_id':prog,
            'calls_out_json':json.dumps(sorted(co)),
            'calls_in_json':json.dumps(sorted(ci)),
            'external_programs_json':json.dumps(sorted(external)),
            'copybooks_used_json':json.dumps(sorted(cbs)),
            'dependency_blob':blob,
            'outgoing_count':len(co),
            'incoming_count':len(ci),
            'external_count':len(external),
            'copybook_count':len(cbs),
            'has_vector':False,
            'updated_at':now
        }
        docs_to_upsert.append(doc)
        processed+=1
    print(f"[INFO] Prepared {processed} docs")

    if args.dry_run:
        print(json.dumps({'sample':docs_to_upsert[:2],'total':len(docs_to_upsert)},indent=2))
        coverage=(len(docs_to_upsert)/len(prog_set)*100.0) if prog_set else 0.0
        print(f"[METRIC] coverage={coverage:.2f}% (target >=95%)")
        return

    # Embedding inline (batch) if requested
    vectorized=0
    if args.embed_inline and docs_to_upsert:
        total=len(docs_to_upsert); done=0
        for chunk_docs in chunk(docs_to_upsert,args.batch):
            texts=[d['dependency_blob'] for d in chunk_docs]
            vecs=embed_blobs_inline(texts,args.embed_batch)
            for d,v in zip(chunk_docs,vecs):
                if v is not None:
                    d['dependency_blob_vector']=v
                    d['has_vector']=True
                    vectorized+=1
            upsert_batch(ep,key,chunk_docs)
            done+=len(chunk_docs)
            print(f"[INLINE] {done}/{total} ({done/total*100:.1f}%) vectors={vectorized}")
        print('[OK] Inline embedding + upsert complete')
    else:
        for chunk_docs in chunk(docs_to_upsert,args.batch):
            upsert_batch(ep,key,chunk_docs)
        print('[OK] Upsert complete (no inline embedding)')

    effective_prog_set = {p for p in prog_set if p}
    coverage=(len(docs_to_upsert)/len(effective_prog_set)*100.0) if effective_prog_set else 0.0
    vector_cov=(vectorized/len(docs_to_upsert)*100.0) if docs_to_upsert else 0.0
    print(f"[METRIC] coverage={coverage:.2f}% (target>=95%) vector_coverage={vector_cov:.2f}% (target>=90% if embed-inline)")
    if coverage<95:
        print('[WARN] Coverage below 95% target')
    if args.embed_inline and vector_cov<90:
        print('[WARN] Vector coverage below 90% target')

if __name__=='__main__':
    main()
