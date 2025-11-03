"""Build copybook usage occurrences into new_cobol_copybook_usage.

Behavior:
  * Walk all COBOL programs under --src-root (default cobol_src) matching **/*.cbl, **/*.CBL, etc.
  * Detect COPY statements:  COPY <name> [REPLACING ...]. (Case-insensitive; handles period at end.)
  * Emit usage documents with fields (aligned to index schema + requested extras):
       usage_id (PROGRAM|line|copybook_name) deterministic
       program_id
       copybook_name (as written)
       normalized_copybook_name (upper, hyphen/space/underscore -> single hyphen)
       section (DATA / PROCEDURE / WORKING-STORAGE / LINKAGE / other best-effort)
       paragraph_name (if in PROCEDURE division)
       line_start, line_end (line of COPY; line_end == line_start for simple forms)
       file_path
       inclusion_order (incremental per program in source order)
       raw_copy_line (original line text)
       context_snippet (±N lines around, default 2 before/after) trimmed 500 chars
       has_replacing_clause (bool)
       context_vector (3072)  (not embedded here) -> has_vector False initially
       has_vector (False)
       updated_at (UTC ISO8601)
  * Idempotent via mergeOrUpload.
  * Batching & retry with backoff.

Usage:
  python ingest/build_copybook_usage.py --src-root cobol_src --push --batch 1000
  python ingest/build_copybook_usage.py --src-root cobol_src --dry-run
"""
from __future__ import annotations
import os, sys, json, argparse, time, pathlib, re, random
import hashlib
import requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_usage'
VECTOR_DIM=3072
DEFAULT_CONTEXT_RADIUS=2
MAX_SNIPPET_CHARS=500
MAX_RETRIES=3

# Accept forms:
#   COPY FOO.
#   COPY "LIB/FOO.CPY".
#   COPY 'FOO'.
# Capture raw token including optional path and extension without surrounding quotes.
COPY_PATTERN=re.compile(r'^\s*COPY\s+"?\'?([A-Z0-9_/\\\.-]+)"?\'?(.*)$', re.IGNORECASE)
PARA_LABEL_RE=re.compile(r'^(?P<label>[A-Z0-9][A-Z0-9-]{1,63})\.$', re.IGNORECASE)
SECTION_HEADERS=[
    ('WORKING-STORAGE SECTION','WORKING-STORAGE'),
    ('LINKAGE SECTION','LINKAGE'),
    ('FILE SECTION','FILE'),
    ('LOCAL-STORAGE SECTION','LOCAL-STORAGE'),
    ('DATA DIVISION','DATA'),
    ('PROCEDURE DIVISION','PROCEDURE')
]
COMMENT_PREFIXES=('*','*>','//')


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def discover_cobol_files(root: str):
    p=pathlib.Path(root)
    if not p.exists():
        return []
    exts={'.cbl','.cob','.cpy'}
    return [fp for fp in p.rglob('*') if fp.suffix.lower() in exts]


def normalize_copybook_name(raw: str) -> str:
    up=raw.upper().strip()
    up=re.sub(r'[\s_]+','-', up)
    up=up.strip('-')
    return up

def sanitize_key_component(raw: str) -> str:
    """Return a string containing only allowed key charset (A-Z0-9_-=). Others -> '-'."""
    up=raw.upper()
    # Replace path separators & dots first
    up=re.sub(r'[./\\]','-', up)
    up=re.sub(r'[^A-Z0-9_\-=]','-', up)
    # Collapse consecutive dashes
    up=re.sub(r'-{2,}','-', up)
    return up.strip('-') or 'X'


def make_usage_id(program_id: str, line: int, copybook: str) -> str:
    # Construct a key using only allowed characters (letters, digits, underscore, dash, equals)
    base=f"{program_id}-{line}-{copybook.upper()}"
    base=re.sub(r'[^A-Z0-9_\-=]','-', base)
    if len(base) > 120:
        h=hashlib.sha1(base.encode()).hexdigest()[:8]
        base=base[:110]+"-"+h
    return base.strip('-')


def current_section_tracker():
    sec=''
    proc=False
    def update(line: str):
        nonlocal sec, proc
        up=line.upper()
        for hdr,label in SECTION_HEADERS:
            if hdr in up:
                sec=label
                if label=='PROCEDURE':
                    proc=True
        return sec, proc
    return update


def extract_paragraph(line: str):
    m=PARA_LABEL_RE.match(line.strip())
    return m.group('label').upper() if m else ''


def backoff_sleep(i):
    time.sleep(min(4, 0.3 * (2 ** i) + random.random()*0.1))


def build_docs_for_file(fp: pathlib.Path, context_radius: int):
    try:
        text=fp.read_text(encoding='utf-8',errors='ignore')
    except Exception:
        return []
    lines=text.splitlines()
    program_id=fp.stem.upper()
    section_updater=current_section_tracker()
    paragraph=''
    docs=[]
    inclusion_counter=0
    for idx,raw in enumerate(lines, start=1):
        stripped=raw.rstrip('\n')
        up=stripped.upper()
        # Skip pure comment lines early
        if stripped.strip().startswith(COMMENT_PREFIXES):
            continue
        # Section detection
        section, in_proc = section_updater(up)
        if in_proc:
            para_candidate=extract_paragraph(up)
            if para_candidate:
                paragraph=para_candidate
                continue
        m=COPY_PATTERN.match(stripped)
        if not m:
            continue
        copy_name=m.group(1).strip().strip('"').strip("'")
        tail=m.group(2) or ''
        has_replacing='REPLACING' in tail.upper()
        normalized=normalize_copybook_name(copy_name)
        key_component=sanitize_key_component(normalized)
        inclusion_counter+=1
        start=max(1, idx-context_radius)
        end=min(len(lines), idx+context_radius)
        snippet='\n'.join(lines[start-1:end])[:MAX_SNIPPET_CHARS]
        usage_id=make_usage_id(program_id, idx, key_component)
        copybook_plain = copy_name.split('/')[-1].split('\\')[-1]
        # NOTE: Ensure only fields declared in index schema are sent to avoid 400 errors.
        doc={
            '@search.action':'mergeOrUpload',
            'usage_id':usage_id,
            'program_id':program_id,
            'program_name': program_id,
            'copybook_name':copy_name,
            'normalized_copybook_name':normalized,
            'copybook_name_plain':copybook_plain,
            'paragraph_name':paragraph if section=='PROCEDURE' else '',
            'section':section,
            'line_number':idx,
            'line_start':idx,
            'line_end':idx,
            'file_path':str(fp),
            'inclusion_order':inclusion_counter,
            'raw_copy_line':stripped,
            'context_snippet':snippet,
            'expansion_present': False,
            'has_replacing_clause':has_replacing,
            'program_classification': '',
            'has_vector': False,
            'ingested_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
        }
        docs.append(doc)
    return docs


def upload(ep,key,batch):
    if not batch: return 0
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'value':batch}
    for attempt in range(MAX_RETRIES):
        r=requests.post(url,headers=headers,json=payload,timeout=60)
        if r.status_code in (200,201):
            return len(batch)
        if r.status_code>=500 and attempt<MAX_RETRIES-1:
            print(f"[WARN] upload retry {attempt+1} status={r.status_code}")
            backoff_sleep(attempt)
            continue
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")
    return 0


def main():
    ap=argparse.ArgumentParser(description='Build copybook usage index from COBOL sources')
    ap.add_argument('--src-root',default='cobol_src')
    ap.add_argument('--batch',type=int,default=1000)
    ap.add_argument('--context-radius',type=int,default=DEFAULT_CONTEXT_RADIUS)
    ap.add_argument('--limit-files',type=int,default=0)
    ap.add_argument('--limit-usages',type=int,default=0)
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()

    files=discover_cobol_files(args.src_root)
    if args.limit_files:
        files=files[:args.limit_files]
    total_docs=0; uploaded=0; buf=[]
    for fp in files:
        docs=build_docs_for_file(fp, args.context_radius)
        for d in docs:
            if args.limit_usages and total_docs>=args.limit_usages:
                break
            buf.append(d); total_docs+=1
            if len(buf)>=args.batch:
                if args.push and not args.dry_run:
                    uploaded+=upload(ep,key,buf); buf.clear()
        if args.limit_usages and total_docs>=args.limit_usages:
            break
    # flush remaining
    if buf and args.push and not args.dry_run:
        uploaded+=upload(ep,key,buf); buf.clear()

    print(f"Discovered usages={total_docs} uploaded={uploaded} push={args.push} dry_run={args.dry_run}")
    if not args.push:
        print('Sample doc:', json.dumps(buf[:1],indent=2) if buf else '(no remaining buffer)')

if __name__=='__main__':
    main()
