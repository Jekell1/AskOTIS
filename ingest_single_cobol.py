#!/usr/bin/env python3
"""ingest_single_cobol.py

Ingest a single COBOL source file's paragraphs (and optionally chunks/symbols) into
an Azure AI Search index (defaults to cobol-paragraphs-v2).

Usage:
    python ingest_single_cobol.py --file "path/to/ORDERS.CBL" [--index cobol-paragraphs-v2] [--include-chunks] [--include-symbols] [--dry]

Resolves existing file_id by searching cobol-files index for program name; falls back
to stable hash of file path.

Requires env (or local.settings.json Values): SEARCH_ENDPOINT, SEARCH_KEY
Embeds paragraph names via embedding_utils.batch_embed.
"""
import os, sys, json, argparse, requests, pathlib, logging
from typing import List, Dict, Any

from ingestion_common import stable_hash, extract_paragraphs, build_chunk_docs, build_symbol_docs
from embedding_utils import batch_embed, provider_info
from search_schema_utils import get_index_field_dimension

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")
FILES_INDEX = "cobol-files"
DEFAULT_PARAS_INDEX = "cobol-paragraphs-v2"
CHUNKS_INDEX = "code-chunks"  # (optional future use if we push chunks too)

session = requests.Session()

def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def resolve_existing_file_id(endpoint: str, key: str, program_hint: str) -> str | None:
    if not program_hint:
        return None
    url = f"{endpoint.rstrip('/')}/indexes/{FILES_INDEX}/docs/search?api-version={API_VERSION}"
    body = {"search": program_hint, "top": 5}
    r = session.post(url, headers={"api-key": key, "Content-Type": "application/json"}, json=body, timeout=30)
    if r.status_code >= 300:
        return None
    for d in r.json().get('value', []):
        name = (d.get('name') or '').upper()
        prog = (d.get('program_id') or '').upper()
        if program_hint.upper() in {name.replace('.CBL',''), prog}:
            return d.get('file_id') or d.get('id')
    return None


def embed_paragraphs(endpoint: str, key: str, index_name: str, paras: List[Dict[str,Any]], batch_size: int = 64):
    if not paras:
        return
    dim = get_index_field_dimension(endpoint, key, index_name.replace('-v2',''), 'name_vector') or get_index_field_dimension(endpoint, key, index_name, 'name_vector')
    texts = [p['name'] for p in paras]
    vecs = batch_embed(texts, target_dim=dim, batch_size=batch_size)
    for p,v in zip(paras, vecs):
        p['name_vector'] = v
        p['has_vector'] = True


def upload(endpoint: str, key: str, index_name: str, docs: List[Dict[str,Any]]):
    if not docs:
        return 0
    url = f"{endpoint.rstrip('/')}/indexes/{index_name}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type": "application/json"}
    payload = {"value": [{"@search.action": "mergeOrUpload", **d} for d in docs]}
    r = session.post(url, headers=headers, json=payload, timeout=120)
    if r.status_code >= 300:
        raise SystemExit(f"Upload error {r.status_code}: {r.text[:400]}")
    return len(docs)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--file', required=True, help='Path to COBOL source file (.CBL/.COB)')
    ap.add_argument('--index', default=DEFAULT_PARAS_INDEX, help='Paragraph index name (default cobol-paragraphs-v2)')
    ap.add_argument('--program', default=None, help='Explicit program name override (defaults to filename stem)')
    ap.add_argument('--include-chunks', action='store_true', help='Also ingest file-scope window chunks into code-chunks index (no embedding here)')
    ap.add_argument('--include-symbols', action='store_true', help='Also ingest data-name symbols into cobol-symbols if available')
    ap.add_argument('--with-text', action='store_true', help='Include paragraph text field (auto true if index ends with -v3)')
    ap.add_argument('--dry', action='store_true')
    ap.add_argument('--verbose', action='store_true')
    args = ap.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')

    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT / SEARCH_KEY')

    path = pathlib.Path(args.file)
    if not path.is_file():
        raise SystemExit(f"File not found: {path}")

    raw = path.read_text(encoding='utf-8', errors='ignore').splitlines(True)
    # Use filename stem as program hint unless overridden
    program_hint = args.program or path.stem.upper()

    existing_file_id = resolve_existing_file_id(endpoint, key, program_hint)
    if existing_file_id:
        file_id = existing_file_id
    else:
        # fallback stable hash to remain deterministic
        file_id = stable_hash([str(path)])
        if args.verbose:
            logging.info(f"No existing file_id found; using hashed file_id {file_id}")

    # Build paragraph docs
    extracted = extract_paragraphs(raw)
    paragraph_docs: List[Dict[str,Any]] = []
    want_text = args.with_text or args.index.endswith('-v3')
    for p in extracted:
        rec = {
            'para_id': p['para_id'],
            'file_id': file_id,
            'name': p['name'],
            'kind': 'paragraph',
            'start_line': p['start_line'],
            'end_line': p['end_line'],
            'name_vector': None,
            'has_vector': False
        }
        if want_text:
            # Slice lines for paragraph body; guard large paragraphs
            sl = max(1, p['start_line']); el = p['end_line']
            snippet = ''.join(raw[sl-1:el])
            if len(snippet) > 12000:
                snippet = snippet[:12000]
            rec['text'] = snippet
        paragraph_docs.append(rec)

    print(f"Discovered {len(paragraph_docs)} paragraphs in {path.name}")

    if args.dry:
        for d in paragraph_docs[:10]:
            print(json.dumps(d, indent=2))
        print(f"(Dry) Paragraph docs prepared: {len(paragraph_docs)}")
        return

    # Embed + upload
    embed_paragraphs(endpoint, key, args.index, paragraph_docs)
    uploaded = upload(endpoint, key, args.index, paragraph_docs)
    print(f"Uploaded {uploaded} paragraphs to {args.index} (provider={provider_info()})")

    # Optionally chunks (lightweight; no embedding so user can embed later in batch job)
    if args.include_chunks:
        from ingestion_common import split_chunks
        # reuse file_id for consistency
        chunks = []
        # We repurpose split_chunks logic via build_chunk_docs
        from ingestion_common import build_chunk_docs
        chunks = build_chunk_docs(path, raw, file_id)
        # Upload chunks without vectors
        if chunks:
            c_url = f"{endpoint.rstrip('/')}/indexes/{CHUNKS_INDEX}/docs/index?api-version={API_VERSION}"
            body = {"value": [{"@search.action":"mergeOrUpload", **c} for c in chunks]}
            r = session.post(c_url, headers={"api-key": key, "Content-Type":"application/json"}, json=body, timeout=120)
            if r.status_code >= 300:
                print(f"Chunk upload error {r.status_code}: {r.text[:300]}")
            else:
                print(f"Uploaded {len(chunks)} chunks to {CHUNKS_INDEX}")

    if args.include_symbols:
        from ingestion_common import build_symbol_docs
        symbols = build_symbol_docs(path, raw, file_id)
        if symbols:
            s_url = f"{endpoint.rstrip('/')}/indexes/cobol-symbols/docs/index?api-version={API_VERSION}"
            body = {"value": [{"@search.action":"mergeOrUpload", **s} for s in symbols]}
            r = session.post(s_url, headers={"api-key": key, "Content-Type":"application/json"}, json=body, timeout=120)
            if r.status_code >= 300:
                print(f"Symbol upload error {r.status_code}: {r.text[:300]}")
            else:
                print(f"Uploaded {len(symbols)} symbols to cobol-symbols")

if __name__ == '__main__':
    main()
