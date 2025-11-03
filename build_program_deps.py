"""Aggregate per-program dependency document for new_cobol_program_deps.

Combines:
  * new_cobol_calls: caller_program -> callee_program edges
  * new_cobol_program_meta: (for role, ui_flag to classify screens) and universe of known programs
  * new_cobol_copybook_usage: program_id -> copybook_name occurrences

Outputs one document per program_id with compact JSON string arrays plus counts.

Usage:
  python build_program_deps.py [--endpoint ... --key ...] [--top-calls N]

Incremental updates: run again; we mergeOrUpload by program_id.

"""
from __future__ import annotations
import os, json, argparse, sys, requests, time, collections
from typing import Dict, Any, List, Set

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
CALLS_INDEX = 'new_cobol_calls'
META_INDEX = 'new_cobol_program_meta'
COPY_USAGE_INDEX = 'new_cobol_copybook_usage'
OUTPUT_INDEX = 'new_cobol_program_deps'


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
        print('Missing search endpoint/key.', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def rest_search(ep: str, key: str, index: str, body: Dict[str,Any]):
    """POST search helper with graceful 404 -> empty result handling."""
    url = f"{ep}/indexes/{index}/docs/search.post.search?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body)
    if r.status_code == 404:
        print(f"[WARN] Source index {index} not found – treating as empty.")
        return {'value': []}
    if r.status_code != 200:
        raise SystemExit(f"Search {index} failed {r.status_code}: {r.text[:300]}")
    return r.json()


def fetch_all_program_meta(ep,key) -> Dict[str,Dict[str,Any]]:
    out = {}
    skip = 0
    while True:
        body = {'search':'*','top':1000,'skip':skip,'select':'program_id,program_role,ui_flag'}
        js = rest_search(ep,key,META_INDEX,body)
        vals = js.get('value',[])
        for v in vals:
            pid = (v.get('program_id') or '').upper()
            if pid:
                out[pid] = v
        if len(vals)<1000:
            break
        skip += 1000
    return out


def fetch_all_calls(ep,key, top_limit=None):
    out=[]; skip=0
    while True:
        remaining = None if top_limit is None else max(0, top_limit - len(out))
        if remaining == 0:
            break
        top = 1000 if remaining is None else min(1000, remaining)
        body = {'search':'*','top':top,'skip':skip,'select':'caller_program,callee_program'}
        js = rest_search(ep,key,CALLS_INDEX,body)
        vals = js.get('value',[])
        out.extend(vals)
        if len(vals)<top:
            break
        skip += top
    return out


def fetch_copy_usage(ep,key):
    # We'll facet on copybook_name maybe expensive; instead page selecting program_id,copybook_name.
    out=[]; skip=0
    while True:
        if skip >= 100000:  # service limit safeguard
            break
        body={'search':'*','top':1000,'skip':skip,'select':'program_id,copybook_name'}
        js = rest_search(ep,key,COPY_USAGE_INDEX,body)
        vals=js.get('value',[])
        out.extend(vals)
        if len(vals)<1000:
            break
        skip+=1000
    return out


def upload_docs(ep,key,docs:List[Dict[str,Any]]):
    url=f"{ep}/indexes/{OUTPUT_INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    for i in range(0,len(docs),500):
        batch={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs[i:i+500]]}
        r=requests.post(url,headers=headers,json=batch)
        if r.status_code not in (200,201):
            raise SystemExit(f"Upload failed {r.status_code}: {r.text[:400]}")


def main():
    ap=argparse.ArgumentParser(description='Build program dependency aggregate index docs.')
    ap.add_argument('--endpoint'); ap.add_argument('--key')
    ap.add_argument('--top-calls',type=int)
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--min-coverage',type=float,default=0.90,help='Warn if produced docs / meta programs < threshold (default 0.90)')
    args=ap.parse_args()
    load_local_settings(); ep,key=resolve_endpoint_key(args)

    print('Loading program meta universe ...')
    meta=fetch_all_program_meta(ep,key)
    meta_count=len(meta)
    print(f"Meta programs: {meta_count}")

    print('Fetching call edges ...')
    calls=fetch_all_calls(ep,key,top_limit=args.top_calls)
    print(f"Call edges fetched: {len(calls)}")

    print('Fetching copybook usage ...')
    copy_usage=fetch_copy_usage(ep,key)
    print(f"Copy usage rows: {len(copy_usage)}")

    outgoing=collections.defaultdict(set)
    incoming=collections.defaultdict(set)
    for c in calls:
        a=(c.get('caller_program') or '').upper(); b=(c.get('callee_program') or '').upper()
        if a and b:
            outgoing[a].add(b); incoming[b].add(a)

    copybooks_by_prog=collections.defaultdict(set)
    for row in copy_usage:
        p=(row.get('program_id') or '').upper(); cb=(row.get('copybook_name') or '').upper()
        if p and cb:
            copybooks_by_prog[p].add(cb)

    known_programs=set(meta.keys()) | set(outgoing.keys()) | set(incoming.keys())

    # Determine external callees (targets not recognized as known program) per source
    external_by_prog=collections.defaultdict(set)
    for a, outs in outgoing.items():
        for b in outs:
            if b not in known_programs:
                external_by_prog[a].add(b)

    # Screens touched: any callee with ui_flag true or role UI
    screens_flag=set(p for p,v in meta.items() if v.get('ui_flag') or (v.get('program_role')=='UI'))

    updated=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())

    docs=[]
    for prog in sorted(known_programs):
        outs=outgoing.get(prog,set()); ins=incoming.get(prog,set())
        copybooks=copybooks_by_prog.get(prog,set())
        externals=external_by_prog.get(prog,set())
        screens=sorted([c for c in outs if c in screens_flag])
        role=(meta.get(prog) or {}).get('program_role')
        dep_blob_parts=[prog,'ROLE:'+str(role or ''),'OUT:'+','.join(sorted(list(outs))[:50]),'IN:' + ','.join(sorted(list(ins))[:50]), 'COPY:'+','.join(sorted(list(copybooks))[:50])]
        if externals:
            dep_blob_parts.append('EXT:'+','.join(sorted(list(externals))[:30]))
        doc={
            'program_id': prog,
            'calls_out_json': json.dumps(sorted(list(outs))),
            'calls_in_json': json.dumps(sorted(list(ins))),
            'external_programs_json': json.dumps(sorted(list(externals))),
            'copybooks_used_json': json.dumps(sorted(list(copybooks))),
            'dependency_blob': ' ; '.join(dep_blob_parts),
            'outgoing_count': len(outs),
            'incoming_count': len(ins),
            'copybook_count': len(copybooks),
            'external_count': len(externals),
            'has_vector': False,
            'updated_at': updated
        }
        docs.append(doc)

    produced=len(docs)
    coverage = (produced/meta_count) if meta_count else 1.0
    print(f"Prepared {produced} dependency docs (coverage={coverage*100:.2f}% of meta universe)")
    if meta_count and coverage < args.min_coverage:
        print(f"[WARN] Coverage below threshold {coverage*100:.2f}% < {args.min_coverage*100:.0f}%")
    if args.dry_run:
        for d in docs[:5]:
            print(json.dumps(d, indent=2)[:400])
        print('Dry run complete.')
        return

    print('Uploading ...')
    upload_docs(ep,key,docs)
    print('Dependency aggregation complete.')

if __name__=='__main__':
    main()
