#!/usr/bin/env python3
"""ingest_cobol_file_chunks.py

Segment COBOL source files into semantic chunks and index them into the chunk index
(`cobol-file-chunks-v1` by default) with optional embedding generation.

Enhancements:
  * Adaptive dimension handling (3072->1536) already present.
  * Added batch embedding with retry/backoff and pacing to respect rate limits.
  * CLI controls: --embed-batch-size, --embed-delay-seconds, --max-embed-chars (truncate each chunk before embedding if desired).

"""
import os, sys, re, json, argparse, hashlib, time, math, requests, random
from pathlib import Path
from typing import List, Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
INDEX_NAME = os.getenv('COBOL_FILE_CHUNK_INDEX','cobol-file-chunks-v1')
VECTOR_FIELD = 'chunk_text_vector'
INDEX_VECTOR_DIM = int(os.getenv('FACT_FILE_EMBED_DIM','1536'))
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\s+([A-Z0-9_-]+)\.', re.IGNORECASE)
COBOL_EXTS = {'.cbl','.cob','.cobol','.cpy','.cb2'}
EMBED_MODEL = (os.getenv('AZURE_OPENAI_EMBED_MODEL') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 'text-embedding-3-large')
DEFAULT_EMBED_BATCH = int(os.getenv('CHUNK_EMBED_BATCH','16'))
DEFAULT_EMBED_DELAY = float(os.getenv('CHUNK_EMBED_DELAY_SECONDS','0.2'))
DEFAULT_EMBED_MAX_RETRIES = int(os.getenv('CHUNK_EMBED_MAX_RETRIES','5'))
BACKOFF_BASE = float(os.getenv('CHUNK_EMBED_BACKOFF_BASE','1.5'))
BACKOFF_JITTER = float(os.getenv('CHUNK_EMBED_BACKOFF_JITTER','0.15'))


def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k]=v
        except Exception:
            pass

def sha1_bytes(data: bytes) -> str:
    import hashlib
    return hashlib.sha1(data).hexdigest()

def find_program_name(text: str, path: Path) -> str:
    m = PROGRAM_ID_RE.search(text)
    if m:
        return m.group(1).upper()
    return path.stem.upper()

def gather_files(roots: List[Path]) -> List[Path]:
    files=[]
    for r in roots:
        if not r.exists():
            continue
        for p in r.rglob('*'):
            if p.is_file() and p.suffix.lower() in COBOL_EXTS:
                files.append(p)
    return files

def chunk_text(text: str, chunk_chars: int, overlap: int) -> List[Dict[str,int]]:
    spans=[]; n=len(text); start=0
    while start < n:
        end=min(start+chunk_chars, n)
        if end < n:
            nl=text.find('\n', end)
            if 0 <= nl < end + 300:
                end=nl+1
        spans.append({'start': start, 'end': end})
        if end>=n: break
        start = max(end - overlap, 0)
        if start==end: break
    return spans

def compress_embedding(vec: List[float]) -> List[float]:
    if len(vec)==INDEX_VECTOR_DIM:
        return vec
    if len(vec)==INDEX_VECTOR_DIM*2:
        return [(vec[i]+vec[i+1])/2.0 for i in range(0,len(vec),2)]
    raise RuntimeError(f"Embedding dimension {len(vec)} incompatible with index {INDEX_VECTOR_DIM}")

def embed_batch(texts: List[str], adaptive: Dict[str,Any]) -> List[List[float]]:
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key = os.getenv('AZURE_OPENAI_KEY')
    if not (azure_ep and azure_key):
        raise RuntimeError('Missing Azure OpenAI embedding credentials')
    url = f"{azure_ep.rstrip('/')}/openai/deployments/{EMBED_MODEL}/embeddings?api-version=2024-02-15-preview"
    payload={'input': texts}
    for attempt in range(DEFAULT_EMBED_MAX_RETRIES):
        r = requests.post(url, headers={'api-key': azure_key,'Content-Type':'application/json'}, json=payload, timeout=120)
        if r.status_code < 300:
            data=r.json().get('data',[])
            if len(data)!=len(texts):
                raise RuntimeError(f"Embedding batch mismatch returned={len(data)} expected={len(texts)}")
            out=[]
            for obj in data:
                emb=compress_embedding(obj['embedding'])
                out.append(emb)
            # success -> reset 429 streak and gently decay delay
            if adaptive['consec_429']>0:
                adaptive['consec_429']=0
                adaptive['delay']=max(DEFAULT_EMBED_DELAY, adaptive['delay']*0.7)
            return out
        if r.status_code in (429,500,502,503,504):
            if r.status_code==429:
                adaptive['consec_429']+=1
                # Increase delay every time, reduce batch periodically
                adaptive['delay']=min(6.0, adaptive['delay']*1.3 + random.uniform(0,0.1))
                if adaptive['consec_429'] in (3,6,9) and adaptive['batch']>4:
                    adaptive['batch']=max(4, int(adaptive['batch']*0.75))
                    print(f"ADAPT: Reducing embed batch size to {adaptive['batch']} after {adaptive['consec_429']} consecutive 429s", file=sys.stderr)
            sleep_time = adaptive['delay'] * (BACKOFF_BASE ** attempt) + random.uniform(0, BACKOFF_JITTER)
            print(f"Embedding batch retry {attempt+1}/{DEFAULT_EMBED_MAX_RETRIES} status={r.status_code} sleeping {sleep_time:.2f}s (delay={adaptive['delay']:.2f} batch={adaptive['batch']})", file=sys.stderr)
            time.sleep(sleep_time)
            continue
        raise RuntimeError(f"Embedding error {r.status_code}: {r.text[:400]}")
    raise RuntimeError(f"Embedding failed after {DEFAULT_EMBED_MAX_RETRIES} retries")

def fetch_existing_chunks(ep: str, key: str, file_id: str) -> Dict[int, Dict[str,Any]]:
    url=f"{ep}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    filt=f"file_id eq '{file_id}'"
    body={"search":"*","filter":filt,"select":"chunk_id,chunk_ordinal,has_vector","top":1000}
    r=requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code>=300:
        raise SystemExit(f"Fetch existing chunks failed {r.status_code}: {r.text[:300]}")
    out={}
    for v in r.json().get('value',[]):
        out[v['chunk_ordinal']]=v
    return out

def batch_upload(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs: return
    url=f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=payload, timeout=120)
    if r.status_code>=300:
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:400]}")
    js=r.json(); failed=[v for v in js.get('value',[]) if not v.get('status')]
    if failed: raise SystemExit(f"Some chunk docs failed: {failed[:3]}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--root', action='append', help='Root folder containing COBOL sources (can repeat)')
    ap.add_argument('--chunk-chars', type=int, default=3800)
    ap.add_argument('--overlap', type=int, default=550)
    ap.add_argument('--batch', type=int, default=64)
    ap.add_argument('--embed', action='store_true')
    ap.add_argument('--embed-batch-size', type=int, default=DEFAULT_EMBED_BATCH)
    ap.add_argument('--embed-delay-seconds', type=float, default=DEFAULT_EMBED_DELAY)
    ap.add_argument('--max-embed-chars', type=int, help='Optional truncate for each chunk before embedding')
    ap.add_argument('--force', action='store_true', help='Recreate & re-embed all chunks')
    ap.add_argument('--sample', type=int, help='Only process first N files discovered')
    ap.add_argument('--program-filter', help='Substring (case-insensitive) or regex (if starts with ^) to include programs')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--skip-existing-check', action='store_true', help='Skip querying index for existing chunks (faster for first full load)')
    ap.add_argument('--progress-interval', type=int, default=25, help='Print a progress line every N files')
    args=ap.parse_args()

    load_settings()
    ep=os.getenv('SEARCH_ENDPOINT'); key=os.getenv('SEARCH_KEY')
    if not (ep and key):
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)
    ep=ep.rstrip('/')

    roots=[]
    if args.root: roots=[Path(r) for r in args.root]
    else:
        env_roots=os.getenv('COBOL_SRC_ROOTS','')
        if env_roots:
            for part in re.split(r'[;,]', env_roots):
                part=part.strip();
                if part: roots.append(Path(part))
    if not roots:
        print('No roots specified (use --root or COBOL_SRC_ROOTS).', file=sys.stderr); sys.exit(2)

    files=gather_files(roots); files.sort()
    discovered=len(files)
    if args.sample: files=files[:args.sample]
    print(f"Discovered {discovered} COBOL candidate files. Processing {len(files)} (sample={args.sample or 'all'}).")

    prog_regex=None; substring=None
    if args.program_filter:
        if args.program_filter.startswith('^'): prog_regex=re.compile(args.program_filter, re.IGNORECASE)
        else: substring=args.program_filter.lower()

    total_chunks=0; new_chunks=0; embedded=0
    upload_buffer=[]; embed_texts=[]; embed_refs=[]
    adaptive={'delay': args.embed_delay_seconds, 'consec_429':0, 'batch': args.embed_batch_size}
    resume_after=int(os.getenv('CHUNK_RESUME_AFTER','0'))

    for file_idx,p in enumerate(files, start=1):
        if resume_after and file_idx <= resume_after:
            if file_idx % max(1,args.progress_interval)==0:
                print(f"Skipping (resume) files {file_idx}/{len(files)}")
            continue
        data=p.read_bytes(); sha=sha1_bytes(data)
        try: text=data.decode('utf-8','replace')
        except Exception: text=data.decode('latin-1','replace')
        program=find_program_name(text,p)
        if prog_regex and not prog_regex.search(program): continue
        if substring and substring not in program.lower(): continue
        file_id=sha
        spans=chunk_text(text, args.chunk_chars, args.overlap)
        if args.force or args.skip_existing_check:
            existing={}
        else:
            try:
                existing=fetch_existing_chunks(ep,key,file_id)
            except KeyboardInterrupt:
                raise
            except Exception as ex:
                print(f"WARN fetch_existing failed for {program} ({file_id[:8]}...) -> {ex}. Proceeding as new.", file=sys.stderr)
                existing={}
        for ord_idx, span in enumerate(spans):
            total_chunks+=1
            chunk_id=f"{file_id}-{ord_idx}"; already=existing.get(ord_idx)
            need_vector=args.embed and (args.force or not already or not already.get('has_vector'))
            chunk_txt=text[span['start']:span['end']]
            if args.max_embed_chars and need_vector:
                chunk_txt=chunk_txt[:args.max_embed_chars]
            doc={
                'chunk_id': chunk_id,
                'file_id': file_id,
                'program_name': program,
                'chunk_ordinal': ord_idx,
                'start_char': span['start'],
                'end_char': span['end'],
                'ingest_timestamp': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
                'has_vector': False,
                'chunk_text': chunk_txt if not need_vector else chunk_txt  # truncated if applied
            }
            if need_vector:
                embed_refs.append(len(upload_buffer))  # position to patch later
                embed_texts.append(chunk_txt)
            if args.force or not already:
                upload_buffer.append(doc); new_chunks+=1
        # periodic embedding + upload flush to bound memory
        if len(embed_texts) >= adaptive['batch'] * 4:  # accumulate a few batches
            if args.embed and embed_texts:
                # process embeddings in sub-batches
                for i in range(0,len(embed_texts), adaptive['batch']):
                    batch_texts=embed_texts[i:i+adaptive['batch']]
                    vectors=embed_batch(batch_texts, adaptive)
                    for j,vec in enumerate(vectors):
                        global_index=embed_refs[i+j]
                        upload_buffer[global_index][VECTOR_FIELD]=vec
                        upload_buffer[global_index]['has_vector']=True
                        embedded+=1
                embed_texts.clear(); embed_refs.clear()
            if len(upload_buffer)>=args.batch:
                if args.dry_run:
                    print(f"[DRY] Would upload {len(upload_buffer)} chunks (intermediate flush)")
                else:
                    batch_upload(ep,key,upload_buffer)
                    print(f"Uploaded {len(upload_buffer)} chunks (progress files {file_idx}/{len(files)})")
                upload_buffer.clear()

        if file_idx % max(1, args.progress_interval) == 0:
            print(f"Progress: files {file_idx}/{len(files)} total_chunks={total_chunks} new={new_chunks} pending_upload={len(upload_buffer)} pending_embed={len(embed_texts)}")

    # Final embedding for remaining
    if args.embed and embed_texts:
        for i in range(0,len(embed_texts), adaptive['batch']):
            batch_texts=embed_texts[i:i+adaptive['batch']]
            vectors=embed_batch(batch_texts, adaptive)
            for j,vec in enumerate(vectors):
                global_index=embed_refs[i+j]
                upload_buffer[global_index][VECTOR_FIELD]=vec
                upload_buffer[global_index]['has_vector']=True
                embedded+=1

    # Final upload
    if upload_buffer:
        if args.dry_run:
            print(f"[DRY] Would upload {len(upload_buffer)} chunks (final flush)")
        else:
            batch_upload(ep,key,upload_buffer)
            print(f"Uploaded final {len(upload_buffer)} chunks")

    print(f"Done. Files processed={len(files)} total_chunks={total_chunks} new_or_updated={new_chunks} embedded={embedded}")

if __name__=='__main__':
    main()
