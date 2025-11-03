#!/usr/bin/env python3
"""ingest_cobol_files.py

Scan one or more COBOL source roots and upsert full file documents into the
`cobol-files-v1` index (or name overridden by COBOL_FILE_INDEX env).

Features:
  * Computes SHA1 hash; skips unchanged files unless --force
  * Derives program_name from first PROGRAM-ID or fallback to filename stem
  * Optional embedding of full text (truncated) if --embed flag and embedding creds configured
  * Batches uploads (default batch size 32)
  * Batch embeddings with adaptive dimension compression & rate limiting
  * Retries with exponential backoff on 429/5xx
  * Supports dry-run to preview actions
  * Optional --sample N to process only first N discovered files (after sorting by path)

Environment:
  SEARCH_ENDPOINT, SEARCH_KEY required
  COBOL_SRC_ROOTS optional (semicolon or comma list); else use --root
  AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY / AZURE_OPENAI_EMBED_DEPLOYMENT (or AZURE_OPENAI_EMBED_MODEL)
  FACT_FILE_EMBED_DIM expected embedding dimension for index vector field (default 1536)

Usage Examples:
  python ingest_cobol_files.py --root ../COBOL --embed --max-embed-chars 20000
  python ingest_cobol_files.py --embed --sample 50 --embed-batch-size 8 --embed-delay-seconds 0.3
"""
import os, sys, re, json, argparse, hashlib, time, requests, math, random
from pathlib import Path
from typing import List, Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
INDEX_NAME = os.getenv('COBOL_FILE_INDEX','cobol-files-v1')
VECTOR_FIELD = 'full_text_vector'
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\s+([A-Z0-9_-]+)\.', re.IGNORECASE)
COBOL_EXTS = {'.cbl','.cob','.cobol','.cpy','.cb2'}
EMBED_MODEL = (os.getenv('AZURE_OPENAI_EMBED_MODEL') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 'text-embedding-3-large')
EXPECTED_DIM = int(os.getenv('FACT_FILE_EMBED_DIM','1536'))

DEFAULT_EMBED_BATCH = int(os.getenv('EMBED_BATCH_SIZE','8'))
DEFAULT_EMBED_DELAY = float(os.getenv('EMBED_DELAY_SECONDS','0.25'))  # seconds between batch calls
DEFAULT_EMBED_MAX_RETRIES = int(os.getenv('EMBED_MAX_RETRIES','5'))
BACKOFF_BASE = float(os.getenv('EMBED_BACKOFF_BASE','1.5'))
BACKOFF_JITTER = float(os.getenv('EMBED_BACKOFF_JITTER','0.1'))


def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

def sha1_bytes(data: bytes) -> str:
    return hashlib.sha1(data).hexdigest()


def find_program_name(text: str, path: Path) -> str:
    m = PROGRAM_ID_RE.search(text)
    if m:
        return m.group(1).upper()
    return path.stem.upper()


def gather_files(roots: List[Path]) -> List[Path]:
    files = []
    for r in roots:
        if not r.exists():
            continue
        for p in r.rglob('*'):
            if p.is_file() and p.suffix.lower() in COBOL_EXTS:
                files.append(p)
    return files


def get_existing_docs(ep: str, key: str, ids: List[str]) -> Dict[str, Dict[str,Any]]:
    # Use search with filter on file_id eq '...' OR ... (chunk if necessary)
    out = {}
    headers={'api-key': key,'Content-Type':'application/json'}
    base=f"{ep}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    # chunk ids to avoid long filter
    for i in range(0,len(ids),12):
        chunk = ids[i:i+12]
        filt = ' or '.join([f"file_id eq '{fid}'" for fid in chunk])
        body={"search":"*","filter": filt, "top": len(chunk), "select":"file_id,file_hash"}
        r=requests.post(base, headers=headers, json=body, timeout=30)
        if r.status_code>=300:
            raise SystemExit(f"Lookup failed {r.status_code}: {r.text[:300]}")
        for v in r.json().get('value',[]):
            out[v['file_id']] = v
    return out


def compress_embedding(vec: List[float]) -> List[float]:
    if len(vec) == EXPECTED_DIM:
        return vec
    if len(vec) == EXPECTED_DIM * 2:
        return [(vec[i] + vec[i+1]) / 2.0 for i in range(0, len(vec), 2)]
    raise RuntimeError(f"Embedding dimension {len(vec)} != expected {EXPECTED_DIM} or double; abort")


def embed_batch(texts: List[str]) -> List[List[float]]:
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key = os.getenv('AZURE_OPENAI_KEY')
    if not (azure_ep and azure_key):
        raise RuntimeError('Missing Azure OpenAI embedding credentials')
    url = f"{azure_ep.rstrip('/')}/openai/deployments/{EMBED_MODEL}/embeddings?api-version=2024-02-15-preview"
    payload={'input': texts}
    delay = DEFAULT_EMBED_DELAY
    for attempt in range(DEFAULT_EMBED_MAX_RETRIES):
        r = requests.post(url, headers={'api-key': azure_key,'Content-Type':'application/json'}, json=payload, timeout=120)
        if r.status_code < 300:
            data = r.json().get('data',[])
            if len(data) != len(texts):
                raise RuntimeError(f"Embedding batch size mismatch returned={len(data)} expected={len(texts)}")
            out=[]
            for obj in data:
                emb = obj['embedding']
                emb = compress_embedding(emb)
                out.append(emb)
            return out
        if r.status_code in (429,500,502,503,504):
            sleep_time = delay * (BACKOFF_BASE ** attempt) + random.uniform(0, BACKOFF_JITTER)
            print(f"Embedding batch retry {attempt+1}/{DEFAULT_EMBED_MAX_RETRIES} status={r.status_code} sleeping {sleep_time:.2f}s", file=sys.stderr)
            time.sleep(sleep_time)
            continue
        raise RuntimeError(f"Embedding error {r.status_code}: {r.text[:400]}")
    raise RuntimeError(f"Embedding failed after {DEFAULT_EMBED_MAX_RETRIES} retries (last status {r.status_code})")


def batch_upload(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs:
        return
    url = f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=payload, timeout=120)
    if r.status_code>=300:
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:500]}" )
    js=r.json()
    failed=[v for v in js.get('value',[]) if not v.get('status')]
    if failed:
        raise SystemExit(f"Some documents failed: {failed[:3]}")


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--root', action='append', help='Root folder containing COBOL sources (can repeat)')
    ap.add_argument('--batch', type=int, default=32)
    ap.add_argument('--embed', action='store_true', help='Generate embedding for truncated full_text')
    ap.add_argument('--max-embed-chars', type=int, default=24000, help='Truncate text before embedding to limit tokens')
    ap.add_argument('--embed-batch-size', type=int, default=DEFAULT_EMBED_BATCH)
    ap.add_argument('--embed-delay-seconds', type=float, default=DEFAULT_EMBED_DELAY, help='Base delay between embedding requests')
    ap.add_argument('--force', action='store_true', help='Upload even if hash unchanged')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--sample', type=int, help='Only process first N source files (after sorting)')
    args = ap.parse_args()

    # Use a local variable for embed delay (avoid global reassignment that triggered SyntaxError)
    effective_embed_delay = args.embed_delay_seconds

    load_settings()
    ep=os.getenv('SEARCH_ENDPOINT'); key=os.getenv('SEARCH_KEY')
    if not (ep and key):
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)
    ep=ep.rstrip('/')

    roots=[]
    if args.root:
        roots=[Path(r) for r in args.root]
    else:
        env_roots=os.getenv('COBOL_SRC_ROOTS','')
        if env_roots:
            for part in re.split(r'[;,]', env_roots):
                part=part.strip()
                if part:
                    roots.append(Path(part))
    if not roots:
        print('No roots specified (use --root or COBOL_SRC_ROOTS).', file=sys.stderr); sys.exit(2)

    files=gather_files(roots)
    files.sort()
    if args.sample:
        files = files[:args.sample]
    if not files:
        print('No COBOL files found.')
        return

    # Prepare ids
    file_ids=[]; meta=[]
    for p in files:
        data=p.read_bytes()
        h=sha1_bytes(data)
        try:
            text=data.decode('utf-8','replace')
        except Exception:
            text=data.decode('latin-1','replace')
        program=find_program_name(text,p)
        lines=text.count('\n')+1
        fid=h  # use content hash as file_id (dedup identical copies)
        file_ids.append(fid)
        meta.append({'path': str(p), 'file_id': fid, 'hash': h, 'program': program, 'text': text, 'lines': lines})

    existing=get_existing_docs(ep, key, file_ids)
    to_upload=[]
    embed_targets=[]  # list of (index in to_upload, truncated_text)

    for m in meta:
        existing_doc=existing.get(m['file_id'])
        if not args.force and existing_doc and existing_doc.get('file_hash')==m['hash']:
            continue
        doc={
            'file_id': m['file_id'],
            'program_name': m['program'],
            'path': m['path'],
            'length_lines': m['lines'],
            'file_hash': m['hash'],
            'ingest_timestamp': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
            'has_vectors': False,
            'full_text': m['text']
        }
        if args.embed:
            trunc=m['text'][:args.max_embed_chars]
            embed_targets.append( (len(to_upload), trunc) )
        to_upload.append(doc)

    if args.dry_run:
        print(f"Would upload {len(to_upload)} documents (new/changed) out of {len(meta)} scanned; embedding={'on' if args.embed else 'off'}")
        if args.embed:
            print(f"Embedding targets: {len(embed_targets)} (batch size {args.embed_batch_size})")
        for d in to_upload[:5]:
            print('  ', d['path'], 'lines=', d['length_lines'])
        return

    # Perform embeddings in batches
    if args.embed and embed_targets:
        batch_size=args.embed_batch_size
        current_batch_texts=[]; current_batch_refs=[]
        for ref_idx, text in embed_targets:
            current_batch_texts.append(text)
            current_batch_refs.append(ref_idx)
            if len(current_batch_texts)>=batch_size:
                # Temporarily override delay used inside embed_batch by monkey-patching DEFAULT_EMBED_DELAY if needed
                prev_delay = DEFAULT_EMBED_DELAY
                try:
                    globals()['DEFAULT_EMBED_DELAY'] = effective_embed_delay
                    vectors=embed_batch(current_batch_texts)
                finally:
                    globals()['DEFAULT_EMBED_DELAY'] = prev_delay
                for pos,vec in enumerate(vectors):
                    doc_index=current_batch_refs[pos]
                    to_upload[doc_index][VECTOR_FIELD]=vec
                    to_upload[doc_index]['has_vectors']=True
                current_batch_texts.clear(); current_batch_refs.clear()
                # modest pacing provided in embed_batch backoff/delay handling
        if current_batch_texts:
            prev_delay = DEFAULT_EMBED_DELAY
            try:
                globals()['DEFAULT_EMBED_DELAY'] = effective_embed_delay
                vectors=embed_batch(current_batch_texts)
            finally:
                globals()['DEFAULT_EMBED_DELAY'] = prev_delay
            for pos,vec in enumerate(vectors):
                doc_index=current_batch_refs[pos]
                to_upload[doc_index][VECTOR_FIELD]=vec
                to_upload[doc_index]['has_vectors']=True

    # Batch upload docs
    uploaded=0
    for i in range(0,len(to_upload), args.batch):
        batch=to_upload[i:i+args.batch]
        batch_upload(ep, key, batch)
        uploaded+=len(batch)
        print(f"Uploaded batch {i//args.batch + 1} ({len(batch)} docs) total={uploaded}")
    print(f"Completed. Uploaded {uploaded} documents (scanned {len(meta)} total). Embeddings={'yes' if args.embed else 'no'}")

if __name__=='__main__':
    main()
