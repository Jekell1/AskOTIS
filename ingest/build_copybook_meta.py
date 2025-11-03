"""Build / upgrade copybook meta docs for new_cobol_copybook_meta (spec version).

Spec Behavior:
    * Discover canonical copybook names via program_catalog.discover_copybook_names(src_root).
    * For each copybook, glob filesystem for all matching .CPY paths (case-insensitive) → file_paths_json.
    * Generate deterministic summary from first N non-comment, non-blank lines across its files (default N=3).
    * Upsert one doc per copybook with fields: copybook_name, file_paths_json, summary, include_count (None placeholder), updated_at, has_vector, summary_vector (optional inline embedding).
    * Optional inline embedding: --embed-inline (calls embedding_utils.batch_embed for summary text) sets has_vector True; else has_vector False for later backfill.
    * Idempotent (mergeOrUpload) batched; retried with exponential backoff.

Commands (expected):
    python search/indexes/create_copybook_meta_index.py --overwrite
    python ingest/build_copybook_meta.py --src-root cobol_src --push --batch 500 --embed-inline
    python search/backfill/add_vector_field_copybook_meta.py
    python search/backfill/backfill_embeddings_copybook_meta.py --batch 64

Acceptance:
    * Non-zero doc count.
    * After embedding (inline or backfill) has_vector ≥90%.
"""
from __future__ import annotations
import os, json, argparse, sys, time, pathlib, re

# Ensure repository root on sys.path when run as a script
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from typing import List, Dict, Any

import requests  # assume available in this environment

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_copybook_meta'
VECTOR_DIM = 3072
DEFAULT_MAX_BATCH = 500

COMMENT_PAT = re.compile(r'^(\s*\*|\s*//|\s*/\*|\s*\*>|\s*\*\*)')  # COBOL + generic comment markers heuristic

# Import discovery & normalization utilities
from ingest import program_catalog as catalog
from embedding_utils import batch_embed


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def collect_files_for_copybook(root: str, canonical_name: str) -> List[str]:
    rels: List[str] = []
    root_path = pathlib.Path(root)
    if not root_path.exists():
        return rels
    for p in root_path.rglob('*.cpy'):
        if not p.is_file():
            continue
        norm = catalog.normalize_name(p.name)
        if norm == canonical_name:
            rels.append(str(p.as_posix()))
    return rels


def extract_summary_sample(path_list: List[str], max_lines: int = 3) -> str:
    for path in path_list:
        try:
            with open(path,'r',encoding='utf-8',errors='ignore') as f:
                lines = []
                for line in f:
                    if len(lines) >= max_lines:
                        break
                    if not line.strip():
                        continue
                    if COMMENT_PAT.match(line):
                        continue
                    lines.append(line.strip())
                if lines:
                    return ' '.join(lines)
        except Exception:
            continue
    return ''


def chunked(seq, size):
    for i in range(0,len(seq),size):
        yield seq[i:i+size]


def upsert_docs(ep: str, key: str, docs: List[Dict[str,Any]], max_batch: int):
    url = f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type':'application/json'}
    for batch in chunked(docs, max_batch):
        payload = {'value': [ {'@search.action':'mergeOrUpload', **d} for d in batch ]}
        for attempt in range(3):
            r = requests.post(url, headers=headers, json=payload, timeout=120)
            if r.status_code in (200,201):
                print(f"[UPLOAD] +{len(batch)} docs")
                break
            if r.status_code >=500 and attempt<2:
                backoff=0.4*(2**attempt)
                print(f"[WARN] upload retry {attempt+1} status={r.status_code} sleep={backoff:.2f}s")
                time.sleep(backoff)
                continue
            print(f"[ERROR] Upload failed {r.status_code}: {r.text[:300]}")
            break


def build_docs(root: str, copybook_names: List[str], summary_lines:int) -> List[Dict[str,Any]]:
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    out: List[Dict[str,Any]] = []
    for name in copybook_names:
        files = collect_files_for_copybook(root, name)
        summary = extract_summary_sample(files, max_lines=summary_lines)
        if not summary:
            summary=f"Copybook {name} (no non-comment sample lines)."  # deterministic fallback
        out.append({
            'copybook_name': name,
            'file_paths_json': json.dumps(files, separators=(',',':')),
            'summary': summary[:800],
            'include_count': None,
            'updated_at': ts,
            'has_vector': False
        })
    return out


def main():
    ap = argparse.ArgumentParser(description='Build copybook meta docs (spec-compliant).')
    ap.add_argument('--endpoint'); ap.add_argument('--key')
    ap.add_argument('--src-root', default='cobol_src', help='Root containing COBOL sources & copybooks')
    ap.add_argument('--push', action='store_true')
    ap.add_argument('--batch', type=int, default=DEFAULT_MAX_BATCH)
    ap.add_argument('--summary-lines', type=int, default=3)
    ap.add_argument('--embed-inline', action='store_true', help='Embed summaries inline (sets has_vector True)')
    ap.add_argument('--embed-batch', type=int, default=64, help='Micro-batch size for embedding')
    ap.add_argument('--limit', type=int, help='Limit number of copybooks (debug)')
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    catalog.load_aliases()  # optional aliases
    copybooks = catalog.discover_copybook_names(args.endpoint, args.key, args.src_root)
    if args.limit:
        copybooks=copybooks[:args.limit]
    print(f"Discovered {len(copybooks)} copybooks (limit applied: {args.limit if args.limit else 'no'})")
    docs = build_docs(args.src_root, copybooks, args.summary_lines)
    if not docs:
        print('No documents prepared (nothing to do).')
        return
    print(f"Prepared {len(docs)} docs. Sample: {docs[0] if docs else {}}")
    if args.embed_inline:
        texts=[d['summary'] for d in docs]
        vecs=batch_embed(texts,batch_size=args.embed_batch,target_dim=VECTOR_DIM)
        for d,v in zip(docs,vecs):
            d['summary_vector']=v; d['has_vector']=True
    else:
        # Do NOT include summary_vector field at all when not embedding; sending null violates
        # Azure Search non-null collection constraint. Backfill will add vectors later.
        for d in docs:
            d['has_vector']=False
    if args.dry_run or not args.push:
        print('(Dry run) Not uploading. Vector coverage:', f"{sum(1 for d in docs if d['has_vector'])/len(docs)*100:.1f}%")
        return
    ep,key=resolve_endpoint_key(args)
    upsert_docs(ep,key,docs,args.batch)
    cov=sum(1 for d in docs if d['has_vector'])/len(docs)*100.0
    print(f"Done. Vector coverage {cov:.2f}%")

if __name__ == '__main__':
    main()
