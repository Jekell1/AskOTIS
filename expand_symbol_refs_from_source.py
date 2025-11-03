"""Expand symbol_refs from 4.8% to ~100% by parsing COBOL source files.

This script:
1. Finds all programs NOT yet in symbol_refs (9,218 missing programs)
2. Parses them using cobolparser-clean.py logic (xrefs extraction)
3. Uploads symbol references to new_cobol_symbol_refs index

Approach: Import the xref extraction logic from cobolparser-clean.py
and create symbol_refs documents for all programs.

Usage:
  python expand_symbol_refs_from_source.py --batch 100
  python expand_symbol_refs_from_source.py --batch 100 --limit 500 --dry-run
"""
from __future__ import annotations
import os, sys, json, argparse, time, requests, pathlib, re
from typing import List, Dict, Set
from id_normalization import make_global_symbol_id, make_scoped_symbol_id, make_ref_id, normalize_program_id

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
SYMBOL_REFS_INDEX = 'new_cobol_symbol_refs'
PROGRAM_META_INDEX = 'new_cobol_program_meta'

# Import COBOL parsing utilities
def parse_cobol_line(line: str) -> str:
    """Remove sequence numbers and indicators from COBOL line."""
    if len(line) >= 72:
        line = line[:72]  # Truncate at column 72
    if len(line) >= 7:
        # Remove sequence number area (cols 1-6) and indicator area (col 7)
        line = line[6:]
        if line and line[0] in {'*', '/', '-'}:
            if line[0] in {'*', '/'}:
                return ''  # Comment line
            line = line[1:]  # Continuation
    return line.strip()

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r')).get('Values', {})
        for k, v in vals.items():
            if k not in os.environ:
                os.environ[k] = str(v)
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[ERROR] Missing endpoint/key')
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch_programs_with_symbol_refs(ep: str, key: str) -> Set[str]:
    """Get set of programs already in symbol_refs."""
    programs = set()
    skip = 0
    while True:
        body = {
            'search': '*',
            'facets': ['program_id,count:100000'],
            'top': 0,
            'skip': skip
        }
        r = requests.post(
            f"{ep}/indexes/{SYMBOL_REFS_INDEX}/docs/search?api-version={API_VERSION}",
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json=body
        )
        if r.status_code != 200:
            print(f'[ERROR] Fetch failed: {r.status_code} {r.text[:200]}')
            sys.exit(1)
        
        data = r.json()
        facets = data.get('@search.facets', {}).get('program_id', [])
        if not facets:
            break
        
        for facet in facets:
            prog = facet.get('value')
            if prog:
                programs.add(prog.upper())
        
        # Check if more data
        if len(facets) < 100000:
            break
        skip += 100000
    
    return programs

def fetch_all_programs(ep: str, key: str) -> Set[str]:
    """Get all program IDs from program_meta."""
    programs = set()
    skip = 0
    page_size = 1000
    while True:
        body = {
            'search': '*',
            'select': 'program_id',
            'top': page_size,
            'skip': skip
        }
        r = requests.post(
            f"{ep}/indexes/{PROGRAM_META_INDEX}/docs/search?api-version={API_VERSION}",
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json=body
        )
        if r.status_code != 200:
            print(f'[WARN] Fetch program_meta failed: {r.status_code}')
            break
        
        docs = r.json().get('value', [])
        if not docs:
            break
        
        for doc in docs:
            pid = doc.get('program_id')
            if pid:
                programs.add(pid.upper())
        
        if len(docs) < page_size:
            break
        skip += page_size
    
    return programs

def discover_cobol_files(root: str) -> Dict[str, pathlib.Path]:
    """Map program names to file paths."""
    mapping = {}
    p = pathlib.Path(root)
    if not p.exists():
        return mapping
    
    extensions = {'.cbl', '.cob', '.cobol'}
    for fp in p.rglob('*'):
        if fp.suffix.lower() in extensions:
            stem = fp.stem.upper()
            if stem not in mapping:
                mapping[stem] = fp
    
    return mapping

def extract_symbol_refs_from_file(file_path: pathlib.Path, program_id: str) -> List[Dict]:
    """Parse COBOL file and extract symbol references (xrefs)."""
    try:
        text = file_path.read_text(encoding='utf-8', errors='ignore')
    except Exception as e:
        print(f'[WARN] Failed to read {file_path}: {e}')
        return []
    
    lines = text.splitlines()
    refs = []
    
    # Regex patterns for symbol usage
    RE_MOVE = re.compile(r'\bMOVE\s+([A-Z0-9\-]+(?:\([^\)]+\))?)\s+TO\s+([A-Z0-9\-]+(?:\([^\)]+\))?)', re.IGNORECASE)
    RE_COMPUTE = re.compile(r'\bCOMPUTE\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_ADD_SUB = re.compile(r'\b(ADD|SUBTRACT)\s+([A-Z0-9\-]+)\s+(?:TO|FROM)\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_MULTIPLY_DIVIDE = re.compile(r'\b(MULTIPLY|DIVIDE)\s+([A-Z0-9\-]+)\s+BY\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_IF = re.compile(r'\bIF\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_READ = re.compile(r'\bREAD\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_WRITE = re.compile(r'\bWRITE\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_CALL_USING = re.compile(r'\bCALL\s+[\'"]([^\'"]+)[\'"]\s+USING\s+(.+)', re.IGNORECASE)
    RE_ACCEPT = re.compile(r'\bACCEPT\s+([A-Z0-9\-]+)', re.IGNORECASE)
    RE_DISPLAY = re.compile(r'\bDISPLAY\s+([A-Z0-9\-]+)', re.IGNORECASE)
    
    current_para = None
    in_procedure = False
    
    for line_num, raw_line in enumerate(lines, start=1):
        line = parse_cobol_line(raw_line)
        if not line:
            continue
        
        # Track procedure division
        if re.match(r'^\s*PROCEDURE\s+DIVISION', line, re.IGNORECASE):
            in_procedure = True
            continue
        
        # Track current paragraph
        if in_procedure:
            para_match = re.match(r'^\s*([A-Z0-9\-]+)\.$', line, re.IGNORECASE)
            if para_match:
                current_para = para_match.group(1).upper()
                continue
        
        if not in_procedure:
            continue
        
        # Extract symbol references
        
        # MOVE statement (source=READ, target=WRITE)
        move_match = RE_MOVE.search(line)
        if move_match:
            source = move_match.group(1).split('(')[0].strip()  # Remove subscripts
            target = move_match.group(2).split('(')[0].strip()
            refs.append({
                'symbol_name': source,
                'kind': 'READ',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
            refs.append({
                'symbol_name': target,
                'kind': 'WRITE',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # COMPUTE (target=WRITE)
        compute_match = RE_COMPUTE.search(line)
        if compute_match:
            target = compute_match.group(1)
            refs.append({
                'symbol_name': target,
                'kind': 'WRITE',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # ADD/SUBTRACT (both operands=READ, target=WRITE)
        addsub_match = RE_ADD_SUB.search(line)
        if addsub_match:
            operand = addsub_match.group(2)
            target = addsub_match.group(3)
            refs.append({
                'symbol_name': operand,
                'kind': 'READ',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
            refs.append({
                'symbol_name': target,
                'kind': 'WRITE',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # MULTIPLY/DIVIDE
        muldiv_match = RE_MULTIPLY_DIVIDE.search(line)
        if muldiv_match:
            operand = muldiv_match.group(2)
            target = muldiv_match.group(3)
            refs.append({
                'symbol_name': operand,
                'kind': 'READ',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
            refs.append({
                'symbol_name': target,
                'kind': 'WRITE',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # IF (condition=READ)
        if_match = RE_IF.search(line)
        if if_match:
            var = if_match.group(1)
            if not var.upper() in {'THEN', 'AND', 'OR', 'NOT'}:
                refs.append({
                    'symbol_name': var,
                    'kind': 'READ',
                    'line_number': line_num,
                    'excerpt': line[:80],
                    'paragraph_name': current_para or ''
                })
        
        # READ (file=READ)
        read_match = RE_READ.search(line)
        if read_match:
            file_name = read_match.group(1)
            refs.append({
                'symbol_name': file_name,
                'kind': 'READ',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # WRITE (record=WRITE)
        write_match = RE_WRITE.search(line)
        if write_match:
            record = write_match.group(1)
            refs.append({
                'symbol_name': record,
                'kind': 'WRITE',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # CALL USING (parameters=PARAM_IN)
        call_match = RE_CALL_USING.search(line)
        if call_match:
            params = call_match.group(2)
            for param in re.findall(r'([A-Z0-9\-]+)', params, re.IGNORECASE):
                if param.upper() not in {'BY', 'REFERENCE', 'CONTENT', 'VALUE'}:
                    refs.append({
                        'symbol_name': param,
                        'kind': 'PARAM_IN',
                        'line_number': line_num,
                        'excerpt': line[:80],
                        'paragraph_name': current_para or ''
                    })
        
        # ACCEPT (input=WRITE)
        accept_match = RE_ACCEPT.search(line)
        if accept_match:
            var = accept_match.group(1)
            refs.append({
                'symbol_name': var,
                'kind': 'WRITE',
                'line_number': line_num,
                'excerpt': line[:80],
                'paragraph_name': current_para or ''
            })
        
        # DISPLAY (output=READ)
        display_match = RE_DISPLAY.search(line)
        if display_match:
            var = display_match.group(1)
            if not var.startswith('"') and not var.startswith("'"):
                refs.append({
                    'symbol_name': var,
                    'kind': 'READ',
                    'line_number': line_num,
                    'excerpt': line[:80],
                    'paragraph_name': current_para or ''
                })
    
    return refs

def build_symbol_ref_doc(program_id: str, ref: Dict, file_path: str, first_seen: Set) -> Dict:
    """Build a symbol_refs document from extracted reference."""
    pid = normalize_program_id(program_id)
    sname = ref['symbol_name'].strip().upper()
    kind = ref['kind'].upper()
    line = ref['line_number']
    
    gid = make_global_symbol_id(sname)
    sid = make_scoped_symbol_id(pid, sname)
    ref_id = make_ref_id(pid, line, sname, kind)
    
    first_key = (pid, sname)
    is_first = first_key not in first_seen
    if is_first:
        first_seen.add(first_key)
    
    return {
        '@search.action': 'mergeOrUpload',
        'ref_id': ref_id,
        'program_id': pid,
        'symbol_name': sname,
        'symbol_id': sid,
        'symbol_id_global': gid,
        'kind': kind,
        'op': kind,
        'normalized_kind': 'READ' if kind in ('READ', 'PARAM_IN') else ('WRITE' if kind in ('WRITE', 'PARAM_OUT') else kind),
        'line_number': line,
        'file_path': file_path,
        'paragraph_name': ref.get('paragraph_name', ''),
        'excerpt_text': ref.get('excerpt', ''),
        'context_before': '',
        'context_after': '',
        'has_vector': False,
        'first_in_program': is_first,
        'cluster_key': f"{pid}|{sname}",
        'ingested_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    }

def upload_batch(ep: str, key: str, batch: List[Dict]):
    """Upload batch of documents to symbol_refs index."""
    if not batch:
        return
    r = requests.post(
        f"{ep}/indexes/{SYMBOL_REFS_INDEX}/docs/index?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'value': batch}
    )
    if r.status_code not in (200, 201):
        print(f'[ERROR] Upload failed: {r.status_code} {r.text[:300]}')
        sys.exit(1)

def main():
    ap = argparse.ArgumentParser(description='Expand symbol_refs from 4.8% to ~100%')
    ap.add_argument('--src-root', default='cobol_src', help='COBOL source directory')
    ap.add_argument('--batch', type=int, default=100, help='Batch size for uploads')
    ap.add_argument('--limit', type=int, default=0, help='Process at most N programs (0=all)')
    ap.add_argument('--dry-run', action='store_true', help='Count refs but do not upload')
    args = ap.parse_args()
    
    load_settings()
    ep, key = resolve()
    
    print('[1/5] Fetching programs with existing symbol_refs...')
    existing_programs = fetch_programs_with_symbol_refs(ep, key)
    print(f'      Found {len(existing_programs)} programs with symbol_refs')
    
    print('[2/5] Fetching all programs from program_meta...')
    all_programs = fetch_all_programs(ep, key)
    print(f'      Found {len(all_programs)} total programs')
    
    missing_programs = all_programs - existing_programs
    print(f'      Missing: {len(missing_programs)} programs ({len(missing_programs)/len(all_programs)*100:.1f}%)')
    
    print(f'[3/5] Discovering COBOL files in {args.src_root}...')
    file_mapping = discover_cobol_files(args.src_root)
    print(f'      Found {len(file_mapping)} COBOL files')
    
    print('[4/5] Extracting symbol references...')
    processed = 0
    uploaded = 0
    batch = []
    first_seen = set()
    start = time.time()
    
    for program_id in sorted(missing_programs):
        if args.limit and processed >= args.limit:
            break
        
        file_path = file_mapping.get(program_id)
        if not file_path:
            continue
        
        processed += 1
        refs = extract_symbol_refs_from_file(file_path, program_id)
        
        for ref in refs:
            doc = build_symbol_ref_doc(program_id, ref, str(file_path), first_seen)
            batch.append(doc)
        
        if len(batch) >= args.batch:
            if not args.dry_run:
                upload_batch(ep, key, batch)
                uploaded += len(batch)
            batch.clear()
            
            elapsed = time.time() - start
            rate = processed / elapsed if elapsed > 0 else 0
            print(f'      Progress: {processed}/{len(missing_programs)} programs '
                  f'({uploaded} refs uploaded, {rate:.1f} prog/sec)')
    
    if batch and not args.dry_run:
        upload_batch(ep, key, batch)
        uploaded += len(batch)
    
    elapsed = time.time() - start
    print(f'\n[5/5] COMPLETE')
    print(f'      Processed: {processed} programs')
    print(f'      Uploaded: {uploaded} symbol references')
    print(f'      Time: {elapsed:.1f}s ({processed/elapsed:.1f} prog/sec)')
    print(f'      Dry run: {args.dry_run}')

if __name__ == '__main__':
    main()
