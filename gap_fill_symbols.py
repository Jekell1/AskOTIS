#!/usr/bin/env python3
"""Gap fill embeddings for cobol-symbols.

Find symbol documents where name_vector is missing (null/empty/short) but either qualified_name or name has usable text.
Optionally embed only those docs to improve coverage without reprocessing everything.

Usage:
  python gap_fill_symbols.py --dry-run            # report counts only
  python gap_fill_symbols.py --apply              # perform embedding for all gaps
  python gap_fill_symbols.py --apply --max-missing 5000  # limit number of gaps processed

Requirements: same local.settings.json as backfill_vectors.py
"""
import argparse, time, json, os
from typing import List, Dict, Any

import backfill_vectors as bv  # reuse existing helpers

INDEX = "cobol-symbols"
KEY_FIELD = "item_id"
PRIMARY = "qualified_name"
FALLBACK = "name"
VECTOR = "name_vector"
API_VERSION = bv.API_VERSION

SAFE_PAGE_SIZE = 100  # keep consistent with backfill script


def iter_docs(search_ep: str, search_key: str):
    """Yield pages of docs ascending by key using key-based paging."""
    last_key = None
    while True:
        filter_expr = None
        if last_key is not None:
            esc = last_key.replace("'", "''")
            filter_expr = f"{KEY_FIELD} gt '{esc}'"
        select_fields = f"{KEY_FIELD},{PRIMARY},{FALLBACK},{VECTOR}"
        page = bv.search_docs(search_ep, search_key, INDEX, select_fields, SAFE_PAGE_SIZE,
                               orderby=f"{KEY_FIELD} asc", filter_expr=filter_expr)
        if not page:
            break
        for d in page:
            yield d
        last_key = page[-1][KEY_FIELD]


def is_missing_vector(doc: Dict[str, Any]) -> bool:
    vec = doc.get(VECTOR)
    if not isinstance(vec, list) or len(vec) < 10:
        # Only count as gap if we still have some text content to embed
        txt = (doc.get(PRIMARY) or "").strip() or (doc.get(FALLBACK) or "").strip()
        if txt:
            return True
    return False


def choose_text(doc: Dict[str, Any]) -> str:
    for f in (PRIMARY, FALLBACK):
        v = doc.get(f)
        if isinstance(v, str) and v.strip():
            return v[:7000]
    return ""


def gap_fill(apply: bool, batch: int, max_missing: int, embed_dep: str):
    search_ep, search_key, openai_ep, openai_key = bv.load_settings()
    scanned = 0
    missing_ids: List[str] = []
    missing_texts: List[str] = []
    start = time.time()

    for doc in iter_docs(search_ep, search_key):
        scanned += 1
        if is_missing_vector(doc):
            text = choose_text(doc)
            if text:
                missing_ids.append(doc[KEY_FIELD])
                missing_texts.append(text)
        # Flush if batching and applying
        if apply and missing_texts and (len(missing_texts) >= batch):
            _embed_and_upload(search_ep, search_key, openai_ep, openai_key, embed_dep, missing_ids, missing_texts)
            missing_ids.clear(); missing_texts.clear()
        if max_missing and len(missing_ids) >= max_missing and not apply:
            break  # dry-run limit
        if max_missing and apply and (len(missing_ids) + 0) >= max_missing:  # +0 clarity
            # Finish remaining pending batch then stop
            pass
    # final flush
    if apply and missing_texts:
        _embed_and_upload(search_ep, search_key, openai_ep, openai_key, embed_dep, missing_ids, missing_texts)
        missing_ids.clear(); missing_texts.clear()

    dur = time.time() - start
    return {
        "scanned": scanned,
        "gaps": len(missing_ids) if not apply else "(applied)",
        "elapsed_sec": round(dur, 1)
    }


def _embed_and_upload(search_ep, search_key, openai_ep, openai_key, embed_dep, ids: List[str], texts: List[str]):
    vecs = bv.embed_texts(openai_ep, openai_key, texts, embed_dep)
    docs = [{"@search.action": "mergeOrUpload", KEY_FIELD: k, VECTOR: v} for k, v in zip(ids, vecs)]
    bv.upload_vectors(search_ep, search_key, INDEX, docs)
    print(f"gap-fill: uploaded {len(docs)} vectors")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--apply', action='store_true', help='Perform embedding updates (otherwise dry-run).')
    ap.add_argument('--batch', type=int, default=64)
    ap.add_argument('--max-missing', type=int, help='Process at most this many missing vectors (useful for staged runs).')
    ap.add_argument('--embed-deployment', help='Override embedding deployment (defaults like backfill script).')
    args = ap.parse_args()
    embed_dep = args.embed_deployment or bv.DEFAULT_EMBED_DEPLOYMENT
    stats = gap_fill(args.apply, args.batch, args.max_missing or 0, embed_dep)
    print(json.dumps(stats, indent=2))

if __name__ == '__main__':
    main()
