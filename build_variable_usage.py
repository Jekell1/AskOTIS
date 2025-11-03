"""Aggregate variable usage statistics from cobol-xrefs into new_cobol_variable_usage.

Logic:
  1. Page through cobol-xrefs selecting: simple_name, program_id, direction, line, snippet (if present)
  2. Normalize variable name to uppercase (variable_id)
  3. Tally read / write based on direction mapping:
       direction in {read, param_in} -> read_count
       direction in {write, param_out} -> write_count
     'param_in' increments param_in_count; 'param_out' increments param_out_count
  4. Maintain sets of program_ids per category (bounded)
  5. Keep up to N sample references (direction + program + line + snippet[:160])
            'activity_count': st['read_count'] + st['write_count'] + st['param_in_count'] + st['param_out_count'],
  6. After processing, compute total_refs = read_count + write_count (not adding param counts separately)
  7. Upload in batches mergeOrUpload

Usage:
  python build_variable_usage.py [--endpoint ... --key ...] [--batch 500] [--top N]

            'last_embedded_usage_role': None,
Requires that new_cobol_variable_usage index already exists (run create_variable_usage_index.py first).
"""
from __future__ import annotations
import os, sys, json, argparse, requests, time
from typing import Dict, Any, List
from id_normalization import make_global_symbol_id

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
XREF_INDEX = 'cobol-xrefs'
TARGET_INDEX = 'new_cobol_variable_usage'
PAGE = 1000
SAMPLE_PER_VAR = 12


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


def fetch_xrefs(ep: str, key: str, top_limit: int | None):
    skip = 0
    total = 0
    while True:
        remaining = None if top_limit is None else max(0, top_limit - total)
        if remaining == 0:
            break
        top = PAGE if remaining is None else min(PAGE, remaining)
        body = {
            'search': '*',
            'top': top,
            'skip': skip,
            'select': 'simple_name,program_id,direction,line,snippet'
        }
        url = f"{ep}/indexes/{XREF_INDEX}/docs/search.post.search?api-version={API_VERSION}"
        r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body)
        if r.status_code != 200:
            raise SystemExit(f"Xref fetch failed {r.status_code}: {r.text[:300]}")
        batch = r.json().get('value', [])
        if not batch:
            break
        for b in batch:
            yield b
        total += len(batch)
        if len(batch) < top:
            break
        skip += top


def aggregate(ep: str, key: str, top_limit: int | None):
    stats: Dict[str, Dict[str, Any]] = {}
    for rec in fetch_xrefs(ep, key, top_limit):
        name = (rec.get('simple_name') or '').strip()
        if not name:
            continue
        var_id = name.upper()
        direction = (rec.get('direction') or '').lower()
        program_id = (rec.get('program_id') or '').upper()
        line = rec.get('line')
        snippet = rec.get('snippet') or ''
        st = stats.setdefault(var_id, {
            'variable_id': var_id,
            'read_count': 0,
            'write_count': 0,
            'param_in_count': 0,
            'param_out_count': 0,
            'program_readers': set(),
            'program_writers': set(),
            'program_params_in': set(),
            'program_params_out': set(),
            'sample': [],
            'first_write_program': None,
            'first_write_line': None
        })
        # Treat 'definition' and 'reference' as read-like occurrences so variables that only appear
        # in declarations or generic references are not shown with zero activity.
        if direction in {'read','param_in','definition','reference'}:
            st['read_count'] += 1
            if program_id: st['program_readers'].add(program_id)
        if direction in {'write','param_out'}:
            st['write_count'] += 1
            if program_id: st['program_writers'].add(program_id)
            # capture earliest write
            if st['first_write_line'] is None or (isinstance(line,int) and line is not None and line < st['first_write_line']):
                st['first_write_line'] = line if isinstance(line,int) else None
                st['first_write_program'] = program_id
        if direction == 'param_in':
            st['param_in_count'] += 1
            if program_id: st['program_params_in'].add(program_id)
        if direction == 'param_out':
            st['param_out_count'] += 1
            if program_id: st['program_params_out'].add(program_id)
        if len(st['sample']) < SAMPLE_PER_VAR:
            st['sample'].append({
                'program': program_id,
                'direction': direction,
                'line': line,
                'snippet': snippet[:160]
            })
    # finalize
    out_docs: List[Dict[str, Any]] = []
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    for var_id, st in stats.items():
        # usage_role heuristic
        role = 'MIXED'
        if st['write_count'] == 0 and (st['read_count'] > 0 or st['param_in_count'] > 0):
            role = 'READ_ONLY'
        elif st['read_count'] == 0 and (st['write_count'] > 0 or st['param_out_count'] > 0):
            role = 'WRITE_ONLY'
        elif st['param_in_count'] > 0 and st['param_out_count'] == 0 and st['write_count'] == 0:
            role = 'INPUT_PARAM'
        elif st['param_out_count'] > 0 and st['param_in_count'] == 0 and st['read_count'] == 0:
            role = 'OUTPUT_PARAM'
        gid = make_global_symbol_id(var_id)
        doc = {
            'variable_id': var_id,
            'symbol_id_global': gid,
            'symbol_id': gid,  # placeholder (same as global for now)
            'read_count': st['read_count'],
            'write_count': st['write_count'],
            'param_in_count': st['param_in_count'],
            'param_out_count': st['param_out_count'],
            'total_refs': st['read_count'] + st['write_count'],
            'program_readers': sorted(list(st['program_readers']))[:1000],
            'program_writers': sorted(list(st['program_writers']))[:1000],
            'program_params_in': sorted(list(st['program_params_in']))[:500],
            'program_params_out': sorted(list(st['program_params_out']))[:500],
            'sample_refs_json': json.dumps(st['sample'], separators=(',',':')),
            'usage_role': role,
            'first_write_location': (f"{st['first_write_program']}:{st['first_write_line']}" if st['first_write_program'] else None),
            'ingested_at': ts
        }
        out_docs.append(doc)
    return out_docs


def upload(ep: str, key: str, docs: List[Dict[str, Any]], batch: int):
    url = f"{ep}/indexes/{TARGET_INDEX}/docs/index?api-version={API_VERSION}"
    for i in range(0, len(docs), batch):
        slice_docs = docs[i:i+batch]
        payload = { 'value': [ {'@search.action':'mergeOrUpload', **d} for d in slice_docs ] }
        r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=payload)
        if r.status_code not in (200,201):
            raise SystemExit(f"Upload failed {r.status_code}: {r.text[:400]}")
        print(f"Uploaded {i+len(slice_docs)}/{len(docs)} variable usage docs")


def main():
    ap = argparse.ArgumentParser(description='Build variable usage aggregation index docs.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--batch', type=int, default=500)
    ap.add_argument('--top', type=int, help='Limit number of xref docs processed')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    print('Aggregating variable usage from xrefs...')
    docs = aggregate(ep, key, args.top)
    print(f'Aggregated {len(docs)} variables')
    if not docs:
        print('No documents to upload.')
        return
    upload(ep, key, docs, args.batch)
    print('Variable usage aggregation complete.')

if __name__ == '__main__':
    main()
