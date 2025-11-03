"""Ingest COBOL symbols into cobol-symbols index with vectors.

Updated: now uses ingestion_common to parse real COBOL sources under COBOL_SOURCE_ROOT.

Features:
 - --dry-run: Show sample docs and counts without uploading
 - Skips if no source root or no symbols detected
 - Batches embeddings + uploads
"""
import os, json, argparse, requests, logging, time
from typing import List, Dict, Any
from embedding_utils import batch_embed, provider_info
from ingestion_common import SourceWalker, build_symbol_docs, stable_hash

API_VERSION = "2024-07-01"
INDEX_NAME = "cobol-symbols"


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def extract_symbols_from_sources(root: str, include_copybooks: bool=False, tag_source_kind: bool=False) -> List[Dict[str, Any]]:
    walker = SourceWalker(root)
    docs: List[Dict[str, Any]] = []
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
        sym_docs = build_symbol_docs(fp, lines, program_id)
        if tag_source_kind:
            for d in sym_docs:
                d['origin_kind'] = origin_kind
        docs.extend(sym_docs)
    return docs


def fetch_index_vector_dim(endpoint: str, key: str) -> int | None:
    """Fetch the expected vector dimension for name_vector field from index schema."""
    url = f"{endpoint.rstrip('/')}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    headers = {"api-key": key}
    try:
        r = requests.get(url, headers=headers, timeout=30)
        if r.status_code >= 300:
            return None
        data = r.json()
        for f in data.get('fields', []):
            if f.get('name') == 'name_vector':
                return f.get('dimensions') or f.get('dimension')
    except Exception:
        return None
    return None


def upload(endpoint: str, key: str, batch_docs: List[Dict], verbose: bool=False):
    if not batch_docs:
        return
    url = f"{endpoint.rstrip('/')}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type": "application/json"}
    payload = {"value": [{"@search.action": "mergeOrUpload", **d} for d in batch_docs]}
    start = time.time()
    r = requests.post(url, headers=headers, json=payload, timeout=60)
    if verbose:
        logging.info(f"POST {url} status={r.status_code} batch={len(batch_docs)} elapsed={time.time()-start:.2f}s")
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")


def main():
    parser = argparse.ArgumentParser(description='Ingest COBOL symbols into Azure AI Search index.')
    parser.add_argument('--dry-run', action='store_true', help='Parse & embed but do not upload.')
    parser.add_argument('--limit', type=int, default=None, help='Limit number of symbols for test runs.')
    parser.add_argument('--verbose', action='store_true', help='Verbose logging for troubleshooting')
    parser.add_argument('--tag-source-kind', action='store_true', help='Add origin_kind field (program|copybook) (ensure index schema updated)')
    parser.add_argument('--include-copybooks', action='store_true', help='Also scan copybooks (.cpy/.copy) for symbols')
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
        print(f"Source root '{root}' not found. Skipping symbol ingestion.")
        return
    symbols = extract_symbols_from_sources(root, include_copybooks=args.include_copybooks, tag_source_kind=args.tag_source_kind)
    if args.limit:
        symbols = symbols[:args.limit]
    print(f"Parsed {len(symbols)} symbol docs from sources under {root}")
    if not symbols:
        return
    texts = [ (s.get('name') or s.get('qualified_name') or '') for s in symbols]
    expected_dim = fetch_index_vector_dim(ep, key)
    if expected_dim is None:
        print("Warning: could not determine index vector dimension; using embedding_utils default")
    vecs = batch_embed(texts, target_dim=expected_dim)
    # Guard: verify every vector has expected_dim length if expected_dim set
    if expected_dim is not None:
        bad = [len(v) for v in vecs if len(v) != expected_dim]
        if bad:
            raise SystemExit(f"Vector dimension mismatch after adjustment. Expected {expected_dim} got samples {bad[:3]}")
    for s, v in zip(symbols, vecs):
        s['name_vector'] = v
        s['has_vector'] = True
    if args.dry_run:
        print("--dry-run active: showing first 3 docs with vector dims")
        for d in symbols[:3]:
            print(json.dumps({k:d[k] for k in ['item_id','name','start_line','end_line','has_vector']}, indent=2))
        return
    B=500
    total=0
    for i in range(0, len(symbols), B):
        batch = symbols[i:i+B]
        upload(ep, key, batch, verbose=args.verbose)
        total += len(batch)
        if args.verbose:
            logging.info(f"Progress symbols {total}/{len(symbols)}")
        else:
            print(f"Uploaded {total}/{len(symbols)}")
    print("Symbols ingestion complete. Provider=", provider_info())

if __name__ == '__main__':
    main()
