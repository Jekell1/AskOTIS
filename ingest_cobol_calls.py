"""Ingest call relationships with snippet vectors into new_cobol_calls index.

Updated to align with new index naming and extended schema.
"""
import os, json, argparse, requests, logging, time, datetime, re
from typing import List, Dict, Any
from embedding_utils import batch_embed, provider_info
from search_schema_utils import get_index_field_dimension
from ingestion_common import SourceWalker, extract_calls, stable_hash

API_VERSION = "2024-07-01"
INDEX_NAME = "new_cobol_calls"


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


CALL_LITERAL_PATTERN = re.compile(r"CALL\s+['\"]([A-Z0-9_\-]+)['\"]", re.IGNORECASE)
CALL_DYNAMIC_PATTERN = re.compile(r"CALL\s+([A-Z0-9_][A-Z0-9_\-]*)", re.IGNORECASE)

def extract_calls_from_sources(root: str, include_copybooks: bool=False, tag_source_kind: bool=False) -> List[Dict[str, Any]]:
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
        file_name_stem = fp.stem
        origin_kind = 'copybook' if ext in {'.cpy','.copy'} else 'program'
        for c in extract_calls(lines, str(fp)):
            # Determine dynamic vs static: static if literal quoted form present on that line
            raw_line = lines[c['line']-1]
            is_static = bool(CALL_LITERAL_PATTERN.search(raw_line))
            is_dynamic = not is_static
            call_type = 'dynamic' if is_dynamic else 'static'
            rec = {
                'call_id': c['call_id'],
                'file_id': program_id,
                'caller_program': file_name_stem.upper(),
                'callee_program': c['called_program'],
                    # 'caller_para': caller_para,  # intentionally omitted (schema mismatch)
                'is_dynamic': is_dynamic,
                'line': c['line'],
                'col': c.get('col'),
                'occurrence': c.get('occurrence'),
                'snippet': c['snippet'],
                'snippet_vector': None,
                'has_vector': False,
                'file_path': str(fp),
                'call_type': call_type,
                'call_hash': stable_hash([c['called_program'], str(fp), str(c['line']), str(c.get('col')), str(c.get('occurrence'))]),
                'ingested_at': None
            }
            if tag_source_kind:
                # Only add if user requested; avoids schema mismatch if index not updated.
                rec['origin_kind'] = origin_kind
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
    parser = argparse.ArgumentParser(description='Ingest COBOL call relationships.')
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--limit', type=int, default=None)
    parser.add_argument('--verbose', action='store_true')
    parser.add_argument('--stream-batch-size', type=int, default=2000, help='Documents per embed+upload streaming batch')
    parser.add_argument('--embed-batch-size', type=int, default=64, help='Internal embedding micro-batch size')
    parser.add_argument('--export-jsonl', help='Export parsed (pre-embedding) call docs to JSONL')
    parser.add_argument('--recreate-index', action='store_true', help='Drop and recreate index before ingestion (to avoid legacy colliding IDs)')
    parser.add_argument('--include-copybooks', action='store_true', help='Also scan copybook files (.cpy/.copy) for CALL statements')
    parser.add_argument('--tag-source-kind', action='store_true', help='Add origin_kind field (program|copybook) to docs (ensure index schema updated first)')
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
        print(f"Source root '{root}' not found. Skipping call ingestion.")
        return
    calls = extract_calls_from_sources(root, include_copybooks=args.include_copybooks, tag_source_kind=args.tag_source_kind)
    if args.limit:
        calls = calls[:args.limit]
    print(f"Parsed {len(calls)} call docs from {root}")
    # optional export before embedding
    if args.export_jsonl:
        with open(args.export_jsonl, 'w', encoding='utf-8') as f:
            for d in calls:
                f.write(json.dumps(d, ensure_ascii=False) + '\n')
        print(f"Exported raw calls to {args.export_jsonl}")
    if not calls:
        return
    expected_dim = get_index_field_dimension(ep, key, INDEX_NAME, 'snippet_vector')
    if args.verbose:
        logging.info(f"Index {INDEX_NAME} snippet_vector expected_dim={expected_dim}")

    stream_size = max(1, args.stream_batch_size)
    total_docs = len(calls)
    started = time.time()
    uploaded = 0

    if args.recreate_index:
        # capture existing schema then recreate
        idx_url = f"{ep.rstrip('/')}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
        get_r = requests.get(idx_url, headers={'api-key': key})
        if get_r.status_code != 200:
            raise SystemExit(f"Failed to fetch index schema for recreate: {get_r.status_code} {get_r.text[:200]}")
        schema = get_r.json()
        schema.pop('@odata.etag', None)
        # delete
        del_r = requests.delete(idx_url, headers={'api-key': key})
        if del_r.status_code not in (204,202):
            raise SystemExit(f"Failed to delete index: {del_r.status_code} {del_r.text[:200]}")
        # create
        create_url = f"{ep.rstrip('/')}/indexes?api-version={API_VERSION}"
        cr_r = requests.post(create_url, headers={'api-key': key, 'Content-Type':'application/json'}, json=schema)
        if cr_r.status_code not in (200,201):
            raise SystemExit(f"Failed to recreate index: {cr_r.status_code} {cr_r.text[:200]}")
        print("Recreated index", INDEX_NAME)

    if args.dry_run:
        demo_slice = calls[: min(min(stream_size, 50), total_docs)]
        texts = [c.get('snippet') or c.get('callee_program') or '' for c in demo_slice]
        vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
        if expected_dim is not None and vecs and len(vecs[0]) != expected_dim:
            raise SystemExit(f"Vector dimension mismatch (dry-run); expected {expected_dim} got {len(vecs[0])}")
        for c,v in zip(demo_slice, vecs):
            c['snippet_vector'] = v
            c['has_vector'] = True
        print('--dry-run sample (first 3):')
        for d in demo_slice[:3]:
            print(json.dumps({k:d[k] for k in ['call_id','callee_program','line','has_vector']}, indent=2))
        print(f"(Dry-run) Prepared {len(demo_slice)} / {total_docs} docs (not uploaded)")
        return

    ingest_ts = datetime.datetime.utcnow().isoformat(timespec='seconds') + 'Z'
    for offset in range(0, total_docs, stream_size):
        slice_docs = calls[offset: offset + stream_size]
        texts = [c.get('snippet') or c.get('callee_program') or '' for c in slice_docs]
        t0 = time.time()
        vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
        if expected_dim is not None and vecs and len(vecs[0]) != expected_dim:
            raise SystemExit(f"Vector dimension mismatch in streaming batch; expected {expected_dim} got {len(vecs[0])}")
        for c,v in zip(slice_docs, vecs):
            c['snippet_vector'] = v
            c['has_vector'] = True
            c['ingested_at'] = ingest_ts
        for i in range(0, len(slice_docs), 500):
            push(ep, key, slice_docs[i:i+500])
        uploaded += len(slice_docs)
        elapsed = time.time() - started
        batch_time = time.time() - t0
        rate = uploaded / elapsed if elapsed > 0 else 0
        remaining = total_docs - uploaded
        eta = remaining / rate if rate > 0 else float('inf')
        print(f"[STREAM] Uploaded {uploaded}/{total_docs} (+{len(slice_docs)}) batch_time={batch_time:.2f}s elapsed={elapsed:.1f}s rate={rate:.1f} docs/s ETA={eta/60:.1f}m")
    print("Calls streaming ingestion complete. Provider=", provider_info())

if __name__ == '__main__':
    main()
