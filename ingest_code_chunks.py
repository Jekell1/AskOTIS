"""Ingest code chunks into code-chunks index with vectors.

Uses ingestion_common.split_chunks across COBOL sources at COBOL_SOURCE_ROOT.
Adds flags: --window, --dry-run, --limit.
"""
import os, json, argparse, requests, logging, time
from typing import List, Dict, Any
from embedding_utils import batch_embed, provider_info
from search_schema_utils import get_index_field_dimension
from ingestion_common import SourceWalker, split_chunks, split_chunks_overlapping, stable_hash
from datetime import datetime

API_VERSION = "2024-07-01"
INDEX_NAME = "new_code_chunks"

def load_settings():
    try:
        data = json.load(open('local.settings.json','r'))
        vals = data.get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def extract_code_chunks_from_sources(root: str, window: int, stride: int | None, include_copybooks: bool=False, tag_source_kind: bool=False) -> List[Dict[str, Any]]:
    walker = SourceWalker(root)
    out: List[Dict[str, Any]] = []
    files_processed = 0
    files_skipped = 0
    print(f"[INFO] Starting file extraction from {root}")
    for fp in walker.iter_files():
        ext=fp.suffix.lower()
        allowed={'.cbl','.cob'}
        if include_copybooks:
            allowed.update({'.cpy','.copy'})
        if ext not in allowed:
            continue
        try:
            lines = fp.read_text(encoding='utf-8', errors='ignore').splitlines(True)
            files_processed += 1
            if files_processed % 500 == 0:
                print(f"[EXTRACTION] Processed {files_processed} files, generated {len(out)} chunks so far...")
        except Exception as e:
            files_skipped += 1
            logging.warning(f"Failed to read {fp}: {e}")
            continue
        # Normalize path to prevent duplicate hashes from path separator differences
        program_id = stable_hash([str(fp).replace('\\', '/').lower()])
        splitter_chunks = split_chunks_overlapping(lines, window=window, stride=stride) if stride else split_chunks(lines, window=window)
        origin_kind='copybook' if ext in {'.cpy','.copy'} else 'program'
        for ch in splitter_chunks:
            rec={
                'chunk_id': ch['chunk_id'],
                'file_id': program_id,
                'path': str(fp),
                'program_id': program_id,
                'scope': 'file',
                'name': fp.name,
                'start_line': ch['start_line'],
                'end_line': ch['end_line'],
                'text': ch['text'],
                'window_index': ch.get('window_index'),
                'window_size': ch.get('window_size', window),
                'stride': ch.get('stride', stride or window),
                'created_at': datetime.utcnow().isoformat(timespec='seconds') + 'Z',
                'tokens_estimate': int(len(ch['text']) / 4) if ch.get('text') else 0,
                'text_vector': None,
                'has_vector': False
            }
            if tag_source_kind:
                rec['origin_kind']=origin_kind
            out.append(rec)
    if files_skipped > 0:
        logging.warning(f"[WARNING] Extraction summary: {files_processed} files processed, {files_skipped} files skipped due to errors")
    print(f"[EXTRACTION COMPLETE] Processed {files_processed} files, generated {len(out)} total chunks")
    return out

def upload_docs(endpoint: str, key: str, docs: List[Dict]) -> tuple[int, int]:
    """Upload documents to Azure Search. Returns (success_count, failure_count)."""
    if not docs:
        print("No documents to upload.")
        return (0, 0)
    url = f"{endpoint.rstrip('/')}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type": "application/json"}
    batch = {"value": [{"@search.action": "mergeOrUpload", **d} for d in docs]}
    
    try:
        r = requests.post(url, headers=headers, json=batch, timeout=60)
    except Exception as e:
        logging.error(f"❌ Upload request failed: {e}")
        return (0, len(docs))
    
    if r.status_code not in (200, 201, 207):
        logging.error(f"❌ Upload failed {r.status_code}: {r.text[:300]}")
        return (0, len(docs))
    
    # 207 = Multi-Status (partial success) - check for actual errors
    if r.status_code == 207:
        result = r.json()
        success_count = sum(1 for item in result.get('value', []) if item.get('status'))
        failure_count = len(docs) - success_count
        if failure_count > 0:
            errors = [item for item in result.get('value', []) if not item.get('status')]
            logging.warning(f"⚠️  Partial success: {success_count}/{len(docs)} docs uploaded, {failure_count} failed")
            for err in errors[:3]:  # Show first 3 errors
                logging.error(f"   Error: {err.get('errorMessage', 'Unknown error')}")
        print(f"Uploaded {success_count}/{len(docs)} docs to {INDEX_NAME}")
        return (success_count, failure_count)
    else:
        print(f"Uploaded {len(docs)} docs to {INDEX_NAME}")
        return (len(docs), 0)

def main():
    parser = argparse.ArgumentParser(description='Ingest code chunks from COBOL sources.')
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--limit', type=int, default=None)
    parser.add_argument('--skip', type=int, default=0, help='Skip first N documents (for resume)')
    parser.add_argument('--window', type=int, default=25, help='Lines per chunk window')
    parser.add_argument('--verbose', action='store_true')
    parser.add_argument('--stride', type=int, default=None, help='Stride (lines) between chunk starts; if omitted or >= window, no overlap')
    parser.add_argument('--stream-batch-size', type=int, default=1500, help='Documents per embed+upload streaming batch')
    parser.add_argument('--embed-batch-size', type=int, default=64, help='Internal embedding micro-batch size')
    parser.add_argument('--include-copybooks', action='store_true', help='Also include copybook files (.cpy/.copy) in chunking')
    parser.add_argument('--tag-source-kind', action='store_true', help='Add origin_kind (program|copybook) to chunk docs')
    parser.add_argument('--no-embed', action='store_true', help='Skip embedding, upload chunks without vectors for faster initial ingest')
    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY')
    root = os.environ.get('COBOL_SOURCE_ROOT') or 'cobol_src'
    if not endpoint or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    if not os.path.isdir(root):
        print(f"Source root '{root}' not found. Skipping chunk ingestion.")
        return
    records = extract_code_chunks_from_sources(root, args.window, args.stride, include_copybooks=args.include_copybooks, tag_source_kind=args.tag_source_kind)
    if args.skip:
        print(f"Skipping first {args.skip} documents (resume mode)")
        records = records[args.skip:]
    if args.limit:
        records = records[:args.limit]
    print(f"Extracted {len(records)} chunk docs from {root} window={args.window}")
    if not records:
        return
    expected_dim = get_index_field_dimension(endpoint, key, INDEX_NAME, 'text_vector')
    if args.verbose:
        logging.info(f"Index {INDEX_NAME} text_vector expected_dim={expected_dim}")

    stream_size = max(1, args.stream_batch_size)
    total_docs = len(records)
    started = time.time()
    uploaded = 0
    failed = 0

    if args.dry_run:
        demo_slice = records[: min(min(stream_size, 40), total_docs)]
        texts = [r.get('text','') for r in demo_slice]
        vectors = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
        if expected_dim is not None and vectors and len(vectors[0]) != expected_dim:
            raise SystemExit(f"Vector dimension mismatch (dry-run); expected {expected_dim} got {len(vectors[0])}")
        for rec, vec in zip(demo_slice, vectors):
            rec['text_vector'] = vec
            rec['has_vector'] = True
        print('--dry-run sample (first 2):')
        for d in demo_slice[:2]:
            print(json.dumps({k:d.get(k) for k in ['chunk_id','name','start_line','end_line','window_index','window_size','stride','tokens_estimate','has_vector']}, indent=2))
        print(f"(Dry-run) Prepared {len(demo_slice)} / {total_docs} docs (not uploaded)")
        return

    for offset in range(0, total_docs, stream_size):
        slice_docs = records[offset: offset + stream_size]
        t0 = time.time()
        
        if not args.no_embed:
            texts = [r.get('text','') for r in slice_docs]
            try:
                vectors = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
                if expected_dim is not None and vectors and len(vectors[0]) != expected_dim:
                    raise SystemExit(f"Vector dimension mismatch in streaming batch; expected {expected_dim} got {len(vectors[0])}")
                for rec, vec in zip(slice_docs, vectors):
                    rec['text_vector'] = vec
                    rec['has_vector'] = True
            except Exception as e:
                logging.error(f"[FAIL] Embedding failed for batch at offset {offset}: {e}")
                failed += len(slice_docs)
                continue
        else:
            # Skip embedding - upload chunks without vectors
            for rec in slice_docs:
                rec['text_vector'] = None
                rec['has_vector'] = False
        
        # Upload in sub-batches of 500
        batch_success = 0
        batch_failed = 0
        for i in range(0, len(slice_docs), 500):
            try:
                success, failure = upload_docs(endpoint, key, slice_docs[i:i+500])
                batch_success += success
                batch_failed += failure
            except Exception as e:
                logging.error(f"❌ Upload exception for sub-batch at {i}: {e}")
                batch_failed += len(slice_docs[i:i+500])
        
        uploaded += batch_success
        failed += batch_failed
        elapsed = time.time() - started
        batch_time = time.time() - t0
        rate = uploaded / elapsed if elapsed > 0 else 0
        remaining = total_docs - uploaded - failed
        eta = remaining / rate if rate > 0 else float('inf')
        status = f"[OK] {uploaded} success" if failed == 0 else f"[OK] {uploaded} success, [FAIL] {failed} failed"
        print(f"[STREAM] {status} (+{len(slice_docs)} processed) batch_time={batch_time:.2f}s elapsed={elapsed:.1f}s rate={rate:.1f} docs/s ETA={eta/60:.1f}m")
    
    print(f"\n{'='*80}")
    print(f"Code chunks streaming ingestion complete. Provider={provider_info()}")
    print(f"[SUCCESS] Uploaded: {uploaded}/{total_docs} documents")
    if failed > 0:
        print(f"[FAILED] Failed: {failed}/{total_docs} documents")
        print(f"[WARNING] Index may be incomplete!")
    print(f"{'='*80}\n")

if __name__ == '__main__':
    main()
