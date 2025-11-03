"""Ingest flow edges into new_cobol_flow_edges with semantic vectors.

Input discovery:
  Prefers JSONL/flow_edges_enriched.jsonl then JSONL/flow_edges.jsonl

Each JSON line should contain (flexible naming handled):
  edge_id, file_id, program_id, caller_para, target_para, resolved_target_para,
  edge_subkind, edge_kind (optional), resolution_strategy, line, edge_text

Missing fields are defaulted; vectors computed over edge_text fallback chain.

Usage:
  python ingest_flow_edges.py [--limit N] [--dry-run]
"""
import os, json, argparse, time, datetime, requests, sys, string
from typing import List, Dict, Any, Optional
from embedding_utils import batch_embed, provider_info
from search_schema_utils import get_index_field_dimension

INDEX_NAME = 'new_cobol_flow_edges'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

PREFERRED_FILES = [
    'JSONL/flow_edges_enriched.jsonl',
    'JSONL/flow_edges.jsonl'
]

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def choose_source_file() -> str:
    for p in PREFERRED_FILES:
        if os.path.isfile(p):
            return p
    raise SystemExit('No flow edges JSONL source found (looked for ' + ', '.join(PREFERRED_FILES) + ')')

CHECKPOINT_FILE = 'flow_edges_ingest.ckpt'

def sanitize_text(txt: str, max_len: int = 8000) -> str:
    if not txt:
        return ''
    # Keep printable plus whitespace, replace others with space
    allowed = set(string.printable) | {'\t','\n','\r'}
    cleaned = ''.join(c if c in allowed else ' ' for c in txt)
    if len(cleaned) > max_len:
        cleaned = cleaned[:max_len]
    return cleaned

def load_edges(path: str, limit: Optional[int]) -> List[Dict[str,Any]]:
    out: List[Dict[str,Any]] = []
    used_ids = set()

    def sanitize_key(raw: str, fallback_index: int) -> str:
        if not raw:
            raw = f"edge_{fallback_index}"
        allowed = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_=")
        cleaned = ''.join(c if c in allowed else '_' for c in raw)
        if not cleaned:
            cleaned = f"edge_{fallback_index}"
        # ensure uniqueness if sanitization caused collision
        if cleaned in used_ids:
            suffix = 1
            base = cleaned[:240]  # keep reasonable length
            new_id = f"{base}_{suffix}"
            while new_id in used_ids:
                suffix += 1
                new_id = f"{base}_{suffix}"
            cleaned = new_id
        used_ids.add(cleaned)
        return cleaned
    with open(path,'r',encoding='utf-8',errors='ignore') as f:
        for i,line in enumerate(f):
            if limit and len(out) >= limit:
                break
            line=line.strip()
            if not line:
                continue
            try:
                js = json.loads(line)
            except Exception:
                continue
            # Normalize keys
            raw_edge_id = js.get('edge_id') or js.get('id') or f"edge_{i}"
            rec = {
                'edge_id': sanitize_key(raw_edge_id, i),
                'file_id': js.get('file_id') or js.get('program_hash'),
                'program_id': (js.get('program_id') or js.get('program') or js.get('prog') or '').upper(),
                'caller_para': js.get('caller_para') or js.get('caller') or '',
                'target_para': js.get('target_para') or js.get('target') or '',
                'resolved_target_para': js.get('resolved_target_para') or js.get('resolved_target') or '',
                'edge_subkind': js.get('edge_subkind') or js.get('subkind') or '',
                'edge_kind': js.get('edge_kind') or js.get('kind') or '',
                'resolution_strategy': js.get('resolution_strategy') or js.get('strategy') or '',
                'line': js.get('line') or js.get('line_number') or 0,
                'edge_text': js.get('edge_text') or js.get('text') or js.get('raw') or ''
            }
            rec['edge_text'] = rec['edge_text'] or f"{rec['caller_para']} -> {rec['target_para']} ({rec['edge_subkind']})"
            rec['edge_text'] = sanitize_text(rec['edge_text'])
            rec['edge_vector'] = None
            rec['has_vector'] = False
            rec['ingested_at'] = None
            out.append(rec)
    return out

def chunk_upload(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs:
        return
    url = f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type":"application/json"}
    payload = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

def embed_with_resilience(texts: List[str], expected_dim: Optional[int], embed_batch: int) -> List[List[float]]:
    try:
        return batch_embed(texts, target_dim=expected_dim, batch_size=embed_batch)
    except Exception as e:
        print(f"[WARN] Batch embed failure ({len(texts)} docs): {e} -> retrying individually", file=sys.stderr)
        vecs: List[List[float]] = []
        zero_vec: List[float] = [0.0]*(expected_dim or 10)
        for t in texts:
            try:
                v = batch_embed([t], target_dim=expected_dim, batch_size=1)[0]
            except Exception as ie:
                print(f"[WARN] Single embed failure, zero vector fallback: {ie}", file=sys.stderr)
                v = zero_vec
            vecs.append(v)
        return vecs

def read_checkpoint() -> int:
    try:
        return int(open(CHECKPOINT_FILE,'r').read().strip())
    except Exception:
        return 0

def write_checkpoint(offset: int):
    try:
        with open(CHECKPOINT_FILE,'w') as f:
            f.write(str(offset))
    except Exception:
        pass

def main():
    ap = argparse.ArgumentParser(description='Ingest flow edges into new_cobol_flow_edges index.')
    ap.add_argument('--limit', type=int)
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--batch-size', type=int, default=2000)
    ap.add_argument('--embed-batch-size', type=int, default=64)
    ap.add_argument('--resume', action='store_true', help='Resume from checkpoint file if present')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key()
    source = choose_source_file()
    edges = load_edges(source, args.limit)
    print(f"Loaded {len(edges)} edges from {source}")
    if not edges:
        return
    expected_dim = get_index_field_dimension(ep, key, INDEX_NAME, 'edge_vector')
    if expected_dim is None:
        print('Warning: could not determine vector field dimension (index missing?)')
    if args.dry_run:
        demo = edges[: min(5, len(edges))]
        texts = [d['edge_text'] for d in demo]
        vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
        for d,v in zip(demo, vecs):
            d['edge_vector'] = v
            d['has_vector'] = True
        print('Dry-run sample:')
        for d in demo:
            print(json.dumps({k:d[k] for k in ['edge_id','program_id','edge_subkind','has_vector']}, indent=2))
        return

    ingest_ts = datetime.datetime.utcnow().isoformat(timespec='seconds') + 'Z'
    total = len(edges)
    bs = max(1, args.batch_size)
    uploaded = 0
    start = time.time()
    start_offset = 0
    if args.resume:
        start_offset = read_checkpoint()
        if start_offset >= total:
            print(f"Checkpoint {start_offset} >= total {total}; nothing to do.")
            return
        if start_offset > 0:
            print(f"Resuming from offset {start_offset}")
    for off in range(start_offset, total, bs):
        slice_docs = edges[off: off+bs]
        texts = [d['edge_text'] for d in slice_docs]
        vecs = embed_with_resilience(texts, expected_dim, args.embed_batch_size)
        if expected_dim is not None and vecs and len(vecs[0]) != expected_dim:
            raise SystemExit(f"Vector dimension mismatch expected {expected_dim} got {len(vecs[0])}")
        for d,v in zip(slice_docs, vecs):
            d['edge_vector'] = v
            d['has_vector'] = True
            d['ingested_at'] = ingest_ts
        # sub-chunk to 500 for API safety
        for i in range(0, len(slice_docs), 500):
            chunk_upload(ep, key, slice_docs[i:i+500])
        uploaded += len(slice_docs)
        write_checkpoint(off + len(slice_docs))
        elapsed = time.time() - start
        rate = uploaded/elapsed if elapsed>0 else 0
        remaining = total - uploaded
        eta = remaining / rate if rate>0 else 0
        print(f"[FLOW] Uploaded {uploaded}/{total} rate={rate:.1f} docs/s ETA={eta/60:.1f}m")
    print('Flow edges ingestion complete. Provider=', provider_info())

if __name__ == '__main__':
    main()
