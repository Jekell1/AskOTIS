#!/usr/bin/env python3
"""Extract control-flow edges directly from COBOL source files stored in Azure Blob Storage.

Reads configuration from environment/local.settings.json keys:
  SOURCE_STORAGE_ACCOUNT
  SOURCE_BLOB_CONTAINER
  SOURCE_CODE_PREFIX      (e.g. S35-Source/)
  SOURCE_FLOW_EDGES_JSONL (output path in local workspace, default JSONL/flow_edges.jsonl)

For each *.cbl|*.cob|*.cobol blob beneath prefix, downloads text (streaming) and applies same
heuristics as extract_flow_edges.py, emitting one JSON line per edge.

Usage:
  python extract_flow_edges_from_blob.py --max-files 0 --progress 200

Options:
  --max-files N   Limit number of source blobs processed (0 = no limit)
  --progress K    Print a progress line every K blobs
  --out PATH      Override output JSONL path
  --min-edges N   Warn if edges below N
  --filter PROGRAM Substring filter (case-insensitive) applied to blob name (e.g. TIM360) to narrow processing
  --include-unresolved  Include edges whose caller paragraph is unknown (default true)
  --skip-unresolved     Exclude edges with caller_para == <<NONE>> (overrides include)

Prereqs: pip install azure-storage-blob
"""
import os, re, json, argparse, sys, hashlib
from pathlib import Path
from typing import Iterable, List, Tuple

try:
    from azure.storage.blob import BlobServiceClient
except ImportError:
    print("Missing dependency azure-storage-blob. Install with: pip install azure-storage-blob", file=sys.stderr)
    sys.exit(1)

PARA_DEF_RE = re.compile(r'^\s*([A-Z0-9][A-Z0-9-]*)(\.)\s*(?:\*.*)?$')
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\.?\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
PERFORM_THRU_RE = re.compile(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\s+THRU\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
PERFORM_RE = re.compile(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
CALL_RE = re.compile(r'\bCALL\s+"?([A-Z0-9][A-Z0-9-]*)"?', re.IGNORECASE)
GOTO_RE = re.compile(r'\bGO\s+TO\s+([A-Z0-9][A-Z0-9-]*)', re.IGNORECASE)
SECTION_RE = re.compile(r'\bSECTION\b', re.IGNORECASE)
COBOL_EXTS = {'.cbl', '.cob', '.cobol'}

API_VERSION = "2024-07-01"

def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    cfg = {
        'account': first('SOURCE_STORAGE_ACCOUNT'),
        'container': first('SOURCE_BLOB_CONTAINER'),
        'code_prefix': first('SOURCE_CODE_PREFIX','SOURCE_CODE_FOLDER','CODE_PREFIX') or '',
        'edges_out': first('SOURCE_FLOW_EDGES_JSONL') or 'JSONL/flow_edges.jsonl',
        'conn_str': first('DEPLOYMENT_STORAGE_CONNECTION_STRING','AzureWebJobsStorage')
    }
    missing = [k for k,v in cfg.items() if not v and k not in ('code_prefix','edges_out')]
    if missing:
        print(f"Missing required configuration values: {missing}", file=sys.stderr)
        sys.exit(2)
    return cfg


def list_cobol_blobs(bsc: BlobServiceClient, container: str, prefix: str) -> Iterable[str]:
    container_client = bsc.get_container_client(container)
    for blob in container_client.list_blobs(name_starts_with=prefix):
        name = blob.name
        lower = name.lower()
        for ext in COBOL_EXTS:
            if lower.endswith(ext):
                yield name
                break


def download_blob_text(bsc: BlobServiceClient, container: str, name: str) -> str:
    """Download blob as text, tolerating encoding issues.

    Some azure-storage-blob versions do not accept an 'errors' argument for content_as_text.
    We first attempt utf-8 decode via content_as_text without errors arg; on failure fall back to manual bytes decode ignoring errors.
    """
    bc = bsc.get_blob_client(container=container, blob=name)
    stream = bc.download_blob(max_concurrency=1)
    try:
        return stream.content_as_text(encoding='utf-8')
    except TypeError:
        # Older API may not accept encoding; try default
        try:
            return stream.content_as_text()
        except Exception:
            data = stream.readall()
            return data.decode('utf-8', 'ignore')
    except Exception:
        data = stream.readall()
        return data.decode('utf-8', 'ignore')


def extract_edges(text: str) -> Tuple[List[dict], int]:
    lines = text.splitlines()
    program_id = None
    paragraph_set = set()
    current_para = None
    edges = []
    for idx, line in enumerate(lines, start=1):
        if program_id is None:
            mprog = PROGRAM_ID_RE.search(line)
            if mprog:
                program_id = mprog.group(1).upper()
        mpara = PARA_DEF_RE.match(line.rstrip())
        if mpara:
            token = mpara.group(1).upper()
            if not SECTION_RE.search(line):
                current_para = token
                paragraph_set.add(token)
            continue
        upper_line = line.upper()
        # PERFORM THRU
        mthru = PERFORM_THRU_RE.search(upper_line)
        if mthru:
            tgt = mthru.group(1).upper()
            kind = 'perform'
            if tgt not in paragraph_set:
                kind += ':unresolved'
            edges.append((current_para, tgt, idx, kind))
            continue
        # PERFORM simple
        mperf = PERFORM_RE.search(upper_line)
        if mperf:
            tgt = mperf.group(1).upper()
            kind = 'perform'
            if tgt not in paragraph_set:
                kind += ':unresolved'
            edges.append((current_para, tgt, idx, kind))
            continue
        # CALL
        mcall = CALL_RE.search(upper_line)
        if mcall:
            tgt = mcall.group(1).upper()
            edges.append((current_para, tgt, idx, 'call'))
            continue
        # GO TO
        mgoto = GOTO_RE.search(upper_line)
        if mgoto:
            tgt = mgoto.group(1).upper()
            kind = 'goto'
            if tgt not in paragraph_set:
                kind += ':unresolved'
            edges.append((current_para, tgt, idx, kind))
            continue
    out_records = []
    for caller, target, line_no, kind in edges:
        caller_norm = caller or '<<NONE>>'
        edge_id = f"{(program_id or 'FILE')[:8]}-{line_no}-{caller_norm}-{target}"[:120]
        rec = {
            'edge_id': edge_id,
            'file_id': (program_id or 'FILE')[:32],
            'caller_para': caller_norm,
            'target_para': target,
            'line': line_no,
            'kind': kind,
        }
        if program_id:
            rec['program_id'] = program_id
        out_records.append(rec)
    return out_records, len(edges)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--max-files', type=int, default=0, help='Process at most N COBOL blobs (0 = unlimited)')
    ap.add_argument('--progress', type=int, default=200, help='Progress log interval in files')
    ap.add_argument('--out', default=None, help='Override output JSONL path')
    ap.add_argument('--min-edges', type=int, default=0, help='Warn if total edges below this')
    ap.add_argument('--filter', default=None, help='Case-insensitive substring filter on blob name')
    ap.add_argument('--skip-unresolved', action='store_true', help='Skip edges with caller_para == <<NONE>>')
    args = ap.parse_args()

    cfg = load_settings()
    out_path = Path(args.out or cfg['edges_out'])
    out_path.parent.mkdir(parents=True, exist_ok=True)

    bsc = BlobServiceClient.from_connection_string(cfg['conn_str'])

    total_edges = 0
    total_files = 0
    written_records = 0

    with out_path.open('w', encoding='utf-8') as outf:
        for i, blob_name in enumerate(list_cobol_blobs(bsc, cfg['container'], cfg['code_prefix'])):
            if args.filter and args.filter.lower() not in blob_name.lower():
                continue
            try:
                text = download_blob_text(bsc, cfg['container'], blob_name)
            except Exception as ex:
                print(f"WARN: failed download {blob_name}: {ex}", file=sys.stderr)
                continue
            recs, edges_count = extract_edges(text)
            if args.skip_unresolved:
                recs = [r for r in recs if r['caller_para'] != '<<NONE>>']
            for r in recs:
                r['blob_name'] = blob_name
                outf.write(json.dumps(r, ensure_ascii=False) + '\n')
            total_edges += edges_count
            written_records += len(recs)
            total_files += 1
            if i % max(1, args.progress) == 0 and i > 0:
                print(f"[progress] processed {i} blobs, edges so far {total_edges}, records written {written_records}")
            if args.max_files and total_files >= args.max_files:
                break
    print(f"Finished: files={total_files} edges_detected={total_edges} records_written={written_records} output={out_path}")
    if args.min_edges and total_edges < args.min_edges:
        print(f"NOTE: total edges {total_edges} < min-edges {args.min_edges}")

if __name__ == '__main__':
    main()
