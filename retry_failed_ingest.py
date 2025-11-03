"""Retry failed document ingests using bulk_ingest_failures.log.

Process:
 1. Read bulk_ingest_failures.log (JSON lines with 'key').
 2. Build mapping id -> path by scanning source roots and recomputing stable ids (same logic as bulk_ingest_cobol_files).
 3. For each failed id that matches a discovered path, rebuild full document (same metadata extraction) and re-upload in batches.
 4. Writes a new log retry_failures.log for any persistent failures.

Usage:
  python retry_failed_ingest.py --roots . --batch-size 32
  python retry_failed_ingest.py --roots cobol_src another_root --limit 500

Notes:
 - Requires that the stable_id algorithm hasn't changed since the original ingestion.
 - If an id isn't found in current filesystem scan, it's reported as 'orphan'.
 - Embeddings: by default uses deterministic fallback unless --embed flag is passed to attempt Azure OpenAI embeddings.
"""
from __future__ import annotations
import os, json, argparse, time, re, hashlib, struct
from pathlib import Path
from typing import List, Dict, Set
import requests
import env_autoload

INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
FAIL_LOG = 'bulk_ingest_failures.log'
RETRY_LOG = 'retry_failures.log'
SUPPORTED_EXT = {'.cbl', '.cob', '.cobol', '.cpy', '.copy'}
VECTOR_DIMS = 3072

PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\.?\s+([A-Z0-9_-]+)', re.IGNORECASE)
COPY_RE = re.compile(r'COPY\s+([A-Z0-9_-]+)', re.IGNORECASE)
EXEC_SQL_RE = re.compile(r'EXEC\s+SQL', re.IGNORECASE)
CICS_RE = re.compile(r'EXEC\s+CICS', re.IGNORECASE)
SCREEN_RE = re.compile(r'SCREEN\s+SECTION', re.IGNORECASE)
PARA_RE = re.compile(r'^(\s{0,7}[A-Z0-9_-]+)\.', re.MULTILINE)
DEFINE_RE = re.compile(r'(?:01|77|78)\s+([A-Z0-9_-]+)')
REF_RE = re.compile(r'\b([A-Z][A-Z0-9_-]{2,})\b')

# ------- Embeddings (optional) -------

def have_azure_openai() -> bool:
    return all(os.getenv(k) for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'])

def embed_text_azure_openai(texts: List[str]) -> List[List[float]]:
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_OPENAI_KEY')
    dep = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    url = f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json={'input': texts}, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding batch failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    out = [d['embedding'] for d in data['data']]
    for v in out:
        if len(v) != VECTOR_DIMS:
            raise ValueError(f"Embedding dimension mismatch: {len(v)} != {VECTOR_DIMS}")
    return out

def embed_text_fallback(text: str) -> List[float]:
    seed = hashlib.sha256(text.encode('utf-8')).digest()
    floats: List[float] = []
    while len(floats) < VECTOR_DIMS:
        for i in range(0, len(seed), 4):
            if len(floats) >= VECTOR_DIMS:
                break
            chunk = seed[i:i+4]
            if len(chunk) < 4:
                chunk = chunk.ljust(4,b'\0')
            val = struct.unpack('!I', chunk)[0]
            floats.append(((val % 1000)/1000.0) - 0.5)
        seed = hashlib.sha256(seed).digest()
    return floats

# ------- ID + Document Reconstruction -------

def stable_id(program: str, path: Path) -> str:
    rel = path.as_posix()
    h = hashlib.sha1(rel.encode('utf-8')).hexdigest()[:10]
    return f"{program}-{h}"

def extract_program_id(text: str, fallback: str) -> str:
    m = PROGRAM_ID_RE.search(text)
    if m:
        return m.group(1).upper()
    return fallback.upper()

def discover_paths(roots: List[str]) -> List[Path]:
    out: List[Path] = []
    for root in roots:
        p = Path(root)
        if not p.exists():
            continue
        for f in p.rglob('*'):
            if f.is_file() and f.suffix.lower() in SUPPORTED_EXT:
                out.append(f)
    return out

def summarize_content(program: str, text: str) -> str:
    lines = [ln.strip() for ln in text.splitlines() if ln.strip() and not ln.strip().startswith('*')][:5]
    return f"Program {program} summary: {' '.join(lines)[:800]}"

def build_doc(path: Path, use_embeddings: bool) -> Dict:
    raw = path.read_text(errors='ignore')
    program_guess = path.stem.upper()
    program = extract_program_id(raw, program_guess)
    paragraphs = sorted({m.group(1).strip().split()[0] for m in PARA_RE.finditer(raw) if len(m.group(1).strip())>0})[:200]
    defines = sorted({m.group(1).upper() for m in DEFINE_RE.finditer(raw)})[:400]
    refs = []
    for tok in set(REF_RE.findall(raw)):
        if tok not in defines and len(tok) > 3:
            refs.append(tok)
    refs = refs[:400]
    copybooks = sorted({m.group(1).upper() for m in COPY_RE.finditer(raw)})[:100]
    has_sql = bool(EXEC_SQL_RE.search(raw))
    has_cics = bool(CICS_RE.search(raw))
    has_screens = bool(SCREEN_RE.search(raw))
    summary = summarize_content(program, raw)
    # placeholder vector; will be replaced in outer batch if embeddings on
    vector = embed_text_fallback(summary)
    ext = path.suffix.lower()
    kind = 'copybook' if ext in {'.cpy', '.copy'} else 'program'
    return {
        '@search.action': 'mergeOrUpload',
        'id': stable_id(program, path),
        'programId': program,
        'kind': kind,
        'path': path.as_posix(),
        'language': 'COBOL',
        'sloc': raw.count('\n') + 1,
        'hasSQL': has_sql,
        'hasCICS': has_cics,
        'hasScreens': has_screens,
        'paragraphs': paragraphs,
        'definesNames': defines,
        'referencesNames': refs,
        'copybooksUsed': copybooks,
        'summary': summary,
        'content': raw,
        'contentVector': vector,
        'commit': 'retry-ingest',
        'updatedAt': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
    }

# ------- Upload -------

def upload_batch(docs: List[Dict]):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json={'value': docs}, timeout=120)
    if r.status_code >= 300:
        raise RuntimeError(f"Batch upload failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    fails = [v for v in data.get('value', []) if not v.get('status')]
    return fails

# ------- Main -------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True)
    ap.add_argument('--batch-size', type=int, default=32)
    ap.add_argument('--limit', type=int, default=0, help='Optional limit on number of failed IDs to retry')
    ap.add_argument('--embed', action='store_true', help='Attempt real embedding regeneration via Azure OpenAI if configured')
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])

    if not Path(FAIL_LOG).exists():
        raise SystemExit(f"Failure log {FAIL_LOG} not found")

    # Collect failed keys
    failed_keys: List[str] = []
    with open(FAIL_LOG, 'r', encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except Exception:
                continue
            k = obj.get('key')
            if k:
                failed_keys.append(k)
    if not failed_keys:
        print('No failed keys found in log.')
        return
    print(f"Loaded {len(failed_keys)} failed keys from log")
    if args.limit:
        failed_keys = failed_keys[:args.limit]
        print(f"Limiting to first {len(failed_keys)} keys")

    # Discover filesystem paths & build id->path map
    paths = discover_paths(args.roots)
    id_map: Dict[str, Path] = {}
    for p in paths:
        raw = p.read_text(errors='ignore')
        program_guess = p.stem.upper()
        program = extract_program_id(raw, program_guess)
        sid = stable_id(program, p)
        id_map[sid] = p
    print(f"Built mapping for {len(id_map)} discovered IDs")

    # Partition into found / orphan
    to_retry: List[Path] = []
    orphan: List[str] = []
    for k in failed_keys:
        if k in id_map:
            to_retry.append(id_map[k])
        else:
            orphan.append(k)
    print(f"Matched {len(to_retry)} failed IDs to filesystem paths; {len(orphan)} orphan IDs")
    if orphan:
        with open('retry_orphans.log', 'w', encoding='utf-8') as fh:
            for o in orphan:
                fh.write(o+'\n')

    docs: List[Dict] = []
    for p in to_retry:
        docs.append(build_doc(p, args.embed and have_azure_openai()))
    print(f"Prepared {len(docs)} documents for retry")

    if args.embed and have_azure_openai():
        print('Generating embeddings for retry set...')
        for i in range(0, len(docs), args.batch_size):
            batch = docs[i:i+args.batch_size]
            texts = [d['summary'] + '\n' + d['content'][:4000] for d in batch]
            vectors = embed_text_azure_openai(texts)
            for d, v in zip(batch, vectors):
                d['contentVector'] = v

    if args.dry_run:
        print('Dry run: no uploads performed.')
        return

    start = time.time()
    total_fail: List[Dict] = []
    uploaded = 0
    for i in range(0, len(docs), args.batch_size):
        batch = docs[i:i+args.batch_size]
        fails = upload_batch(batch)
        uploaded += (len(batch) - len(fails))
        if fails:
            for f in fails:
                f['attempt'] = 'retry'
            total_fail.extend(fails)
        if (i//args.batch_size) % 10 == 0:
            elapsed = time.time() - start
            rate = uploaded/elapsed if elapsed else 0
            print(f"Progress: uploaded_ok={uploaded} remaining={len(docs)-uploaded} rate={rate:.2f}/s")
    if total_fail:
        with open(RETRY_LOG, 'w', encoding='utf-8') as fh:
            for f in total_fail:
                fh.write(json.dumps(f)+'\n')
        print(f"Completed with {len(total_fail)} persistent failures (see {RETRY_LOG}).")
    else:
        print('All retry documents uploaded successfully.')

if __name__ == '__main__':
    main()
