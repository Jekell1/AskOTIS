"""Ingest cross references with snippet vectors into cobol-xrefs index.

Approximate implementation: derives xrefs from symbol + call occurrences until
full xref generation pipeline is integrated. Adds --dry-run / --limit.
"""
import os, json, argparse, requests, logging, time
from typing import List, Dict, Any
from embedding_utils import batch_embed, provider_info
from search_schema_utils import get_index_field_dimension
from ingestion_common import SourceWalker, extract_calls, extract_data_names, stable_hash

API_VERSION = "2024-07-01"
INDEX_NAME = "cobol-xrefs"


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def extract_xrefs_from_sources(root: str, include_copybooks: bool=False, tag_source_kind: bool=False) -> List[Dict[str, Any]]:
    walker = SourceWalker(root)
    out: List[Dict[str, Any]] = []
    for fp in walker.iter_files():
        ext=fp.suffix.lower()
        allowed={'.cbl','.cob'}
        if include_copybooks:
            allowed.update({'.cpy','.copy'})
        if ext not in allowed:
            continue
        try:
            lines = fp.read_text(encoding='utf-8', errors='ignore').splitlines(True)
        except Exception:
            continue
        program_id = stable_hash([str(fp)])
        origin_kind = 'copybook' if ext in {'.cpy','.copy'} else 'program'
        # treat data names as definitions, calls as references just for demo
        for d in extract_data_names(lines):
            rec={
                'xref_id': stable_hash(['DEF', str(fp), d['name'], str(d['line'])]),
                'file_id': program_id,
                'path': str(fp),
                'program_id': program_id,
                'qualified_name': d['name'],
                'simple_name': d['name'],
                'kind': 'data',
                'direction': 'definition',
                'line': d['line'],
                'start_col': None,
                'end_col': None,
                'snippet': lines[d['line']-1].strip()[:400],
                'snippet_vector': None,
                'has_vector': False
            }
            if tag_source_kind:
                rec['origin_kind']=origin_kind
            out.append(rec)
        for c in extract_calls(lines, str(fp)):
            rec={
                'xref_id': stable_hash(['CALL', str(fp), c['called_program'], str(c['line']), str(c.get('col')), str(c.get('occurrence'))]),
                'file_id': program_id,
                'path': str(fp),
                'program_id': program_id,
                'qualified_name': c['called_program'],
                'simple_name': c['called_program'],
                'kind': 'call',
                'direction': 'reference',
                'line': c['line'],
                'col': c.get('col'),
                'start_col': None,
                'end_col': None,
                'snippet': c['snippet'],
                'snippet_vector': None,
                'has_vector': False
            }
            if tag_source_kind:
                rec['origin_kind']=origin_kind
            out.append(rec)
    return out


def push(ep: str, key: str, docs: List[Dict]):
    if not docs:
        return
    url = f"{ep.rstrip('/')}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type": "application/json"}
    payload = {"value": [{"@search.action": "mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")


def main():
    parser = argparse.ArgumentParser(description='Ingest approximate cross references (data defs + calls).')
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--limit', type=int, default=None)
    parser.add_argument('--verbose', action='store_true')
    parser.add_argument('--stream-batch-size', type=int, default=2000, help='Documents per embed+upload streaming batch')
    parser.add_argument('--embed-batch-size', type=int, default=64, help='Internal embedding micro-batch size')
    parser.add_argument('--include-copybooks', action='store_true', help='Also scan copybooks (.cpy/.copy)')
    parser.add_argument('--tag-source-kind', action='store_true', help='Add origin_kind field (program|copybook) to docs')
    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
    load_settings()
    ep = os.environ.get('SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY')
    root = os.environ.get('COBOL_SOURCE_ROOT') or 'cobol_src'
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    if not os.path.isdir(root):
        print(f"Source root '{root}' not found. Skipping xref ingestion.")
        return
    xrefs = extract_xrefs_from_sources(root, include_copybooks=args.include_copybooks, tag_source_kind=args.tag_source_kind)
    if args.limit:
        xrefs = xrefs[:args.limit]
    print(f"Parsed {len(xrefs)} xref docs from {root}")
    if not xrefs:
        return
    expected_dim = get_index_field_dimension(ep, key, INDEX_NAME, 'snippet_vector')
    if args.verbose:
        logging.info(f"Index {INDEX_NAME} snippet_vector expected_dim={expected_dim}")

    stream_size = max(1, args.stream_batch_size)
    total_docs = len(xrefs)
    started = time.time()
    uploaded = 0

    if args.dry_run:
        demo_slice = xrefs[: min(min(stream_size, 50), total_docs)]
        texts = [x.get('snippet') or x.get('qualified_name') or '' for x in demo_slice]
        vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
        if expected_dim is not None and vecs and len(vecs[0]) != expected_dim:
            raise SystemExit(f"Vector dimension mismatch (dry-run); expected {expected_dim} got {len(vecs[0])}")
        for x,v in zip(demo_slice, vecs):
            x['snippet_vector'] = v
            x['has_vector'] = True
        print('--dry-run sample (first 3):')
        for d in demo_slice[:3]:
            print(json.dumps({k:d[k] for k in ['xref_id','kind','qualified_name','line','has_vector']}, indent=2))
        print(f"(Dry-run) Prepared {len(demo_slice)} / {total_docs} docs (not uploaded)")
        return

    for offset in range(0, total_docs, stream_size):
        slice_docs = xrefs[offset: offset + stream_size]
        texts = [x.get('snippet') or x.get('qualified_name') or '' for x in slice_docs]
        t0 = time.time()
        vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
        if expected_dim is not None and vecs and len(vecs[0]) != expected_dim:
            raise SystemExit(f"Vector dimension mismatch in streaming batch; expected {expected_dim} got {len(vecs[0])}")
        for x,v in zip(slice_docs, vecs):
            x['snippet_vector'] = v
            x['has_vector'] = True
        for i in range(0, len(slice_docs), 500):
            push(ep, key, slice_docs[i:i+500])
        uploaded += len(slice_docs)
        elapsed = time.time() - started
        batch_time = time.time() - t0
        rate = uploaded / elapsed if elapsed > 0 else 0
        remaining = total_docs - uploaded
        eta = remaining / rate if rate > 0 else float('inf')
        print(f"[STREAM] Uploaded {uploaded}/{total_docs} (+{len(slice_docs)}) batch_time={batch_time:.2f}s elapsed={elapsed:.1f}s rate={rate:.1f} docs/s ETA={eta/60:.1f}m")
    print("XRef streaming ingestion complete. Provider=", provider_info())

if __name__ == '__main__':
    main()
