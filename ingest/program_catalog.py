"""Comprehensive COBOL Program & Copybook Catalog Builder.

Goals:
  * Discover EVERY COBOL Program (*.CBL / *.COB) and Copybook (*.CPY)
  * Normalize names into a canonical stable identifier (PROGRAM_ID / COPYBOOK_ID)
  * Merge discovery sources (indexes + filesystem) with precedence & alias resolution
  * Provide coverage stats (e.g., % of filesystem artifacts represented in indexes)

Discovery Sources (best-effort; any may be absent):
  1. new_cobol_program_meta   (program_id)
  2. new_cobol_calls           (caller_program, callee_program)
  3. new_cobol_copybook_usage  (program_id, copybook_name_plain)
  4. Filesystem scan (recursive) for *.CBL, *.COB, *.CPY (case-insensitive)

Alias Resolution:
  Optional JSON file (--alias-json) may define canonical remappings. Supported shapes:
    {"PROGRAM": {"OLDNAME": "NEWNAME", ...}, "COPYBOOK": { ... }}
    or a single flat mapping interpreted as applying to both kinds.
  Keys & values are raw; we apply normalization to both during load so that the mapping
  is in terms of normalized identifiers.

Output (printed JSON unless --output specified):
  {
    "programs": {
       "filesystem_count": int,
       "index_count": int,
       "combined_count": int,
       "missing_in_index": [...],   # present on disk, absent from ANY index-derived source
       "missing_on_disk": [...],    # present in indexes, no matching file discovered
       "alias_mapped": int
    },
    "copybooks": { ... analogous ... },
    "timestamp": ISO8601
  }

CLI (spec simple summary mode by default):
    python ingest/program_catalog.py [--endpoint ... --key ...] [--alias-json path] [--root path]

Outputs (default):
    Programs discovered: N
    Copybooks discovered: M
    Sample programs: [...first 10...]
    Sample copybooks: [...first 10...]

Extended JSON catalog still available with --full-json (optionally --output path).

This script is intentionally tolerant: if endpoint/key aren't available it will still
produce a catalog purely from the filesystem.
"""
from __future__ import annotations
import os, re, json, argparse, sys, time, pathlib
from typing import Dict, Set, Iterable, Tuple, List, Optional

try:
    import requests  # type: ignore
except Exception:  # pragma: no cover
    requests = None  # offline mode will skip index sources

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_META_INDEX = 'new_cobol_program_meta'
CALLS_INDEX = 'new_cobol_calls'
COPYBOOK_USAGE_INDEX = 'new_cobol_copybook_usage'
COPYBOOK_META_INDEX = 'new_cobol_copybook_meta'  # optional (primary for copybooks if present)

NON_ALNUM_RE = re.compile(r'[^A-Za-z0-9]+')
EXT_RE = re.compile(r'\.(CBL|COB|CPY)$', re.IGNORECASE)

# Directory name (case-insensitive) denylist segments we must ignore during filesystem walk
DENYLIST_DIRS = {'.git', '.venv', 'build', 'out', 'bin'}

# ---------------- Normalization ---------------- #

def normalize_name(raw: str) -> str:
        """Normalize a COBOL artifact name to canonical form (spec aligned).

        Spec Requirements (2025-09-30):
            * Uppercase
            * Strip extensions: .CBL / .COB / .CPY
            * Collapse any run of [ space, period, underscore, hyphen ] to single '-'
            * Trim leading/trailing separators
        Note: Earlier implementation collapsed all non-alphanumerics; retained behaviour for safety
        except we now explicitly target the required char class first to avoid unintentionally
        removing meaningful characters if they ever appear (e.g., plus in generated names).
        """
        if raw is None:
                return ''
        name = os.path.basename(raw).strip()
        name = EXT_RE.sub('', name)
        # Primary collapse of required set
        name = re.sub(r'[ ._\-]+', '-', name)
        # Safety: collapse any residual non-alnum runs (legacy behaviour) to a hyphen to keep canonical stability
        name = NON_ALNUM_RE.sub('-', name)
        name = name.strip('-')
        return name.upper()

# ---------------- Azure Search Helpers ---------------- #

def _resolve_endpoint_key(args) -> Tuple[Optional[str], Optional[str]]:
    # Prefer explicit args, then AZURE_* env. Only fall back to SEARCH_* if at least one AZURE_* var exists.
    explicit_ep = args.endpoint
    explicit_key = args.key
    azure_ep = os.getenv('AZURE_SEARCH_ENDPOINT')
    azure_key = os.getenv('AZURE_SEARCH_KEY')
    fallback_ep = os.getenv('SEARCH_ENDPOINT')
    fallback_key = os.getenv('SEARCH_KEY')

    if explicit_ep and explicit_key:
        return explicit_ep.rstrip('/'), explicit_key
    if azure_ep and azure_key:
        return azure_ep.rstrip('/'), azure_key
    # Only use generic fallback if at least one AZURE var is present (prevents tests from unintentionally pulling indexes)
    if (azure_ep or azure_key) and fallback_ep and fallback_key:
        return fallback_ep.rstrip('/'), fallback_key
    return None, None

def _search_iter(ep: str, key: str, index: str, select: str) -> Iterable[dict]:
    """Iterate all docs selecting given fields (best-effort)."""
    if requests is None:
        return []  # type: ignore
    skip = 0
    page = 1000
    url = f"{ep}/indexes/{index}/docs/search.post.search?api-version={API_VERSION}"
    while True:
        body = {"search": "*", "top": page, "skip": skip, "select": select}
        try:
            r = requests.post(url, headers={"api-key": key, "Content-Type": "application/json"}, json=body, timeout=120)
        except Exception:
            break
        if r.status_code != 200:
            break
        js = r.json()
        vals = js.get('value', [])
        if not vals:
            break
        for row in vals:
            yield row
        if len(vals) < page:
            break
        skip += len(vals)

# Distinct extraction functions

def fetch_program_ids_from_meta(ep: str, key: str) -> Set[str]:
    out: Set[str] = set()
    for row in _search_iter(ep, key, PROGRAM_META_INDEX, 'program_id'):
        pid = row.get('program_id')
        if pid:
            out.add(normalize_name(pid))
    return out

def fetch_program_ids_from_calls(ep: str, key: str) -> Set[str]:
    out: Set[str] = set()
    for row in _search_iter(ep, key, CALLS_INDEX, 'caller_program,callee_program'):
        for field in ('caller_program','callee_program'):
            v = row.get(field)
            if v:
                out.add(normalize_name(v))
    return out

def fetch_program_and_copybooks_from_usage(ep: str, key: str) -> Tuple[Set[str], Set[str]]:
    programmes: Set[str] = set(); copybooks: Set[str] = set()
    for row in _search_iter(ep, key, COPYBOOK_USAGE_INDEX, 'program_id,copybook_name_plain'):
        p = row.get('program_id'); c = row.get('copybook_name_plain')
        if p:
            programmes.add(normalize_name(p))
        if c:
            copybooks.add(normalize_name(c))
    return programmes, copybooks

# ---------------- Filesystem Discovery ---------------- #

def discover_files(root: str) -> Tuple[Set[str], Set[str]]:
    """Filesystem discovery respecting denylist directories.

    Any path containing a denylisted directory segment (case-insensitive) is skipped.
    """
    program_set: Set[str] = set()
    copybook_set: Set[str] = set()
    root_path = pathlib.Path(root)
    if not root_path.exists():
        return program_set, copybook_set
    deny_lower = {d.lower() for d in DENYLIST_DIRS}
    for path in root_path.rglob('*'):
        if not path.is_file():
            continue
        # Skip if any ancestor directory is denylisted
        try:
            if any(part.lower() in deny_lower for part in path.relative_to(root_path).parts[:-1]):
                continue
        except Exception:
            # relative_to may fail if symlink bizarre; skip path silently
            continue
        suffix = path.suffix.lower()
        if suffix in ('.cbl', '.cob'):
            program_set.add(normalize_name(path.name))
        elif suffix == '.cpy':
            copybook_set.add(normalize_name(path.name))
    return program_set, copybook_set

# ---------------- Alias Loading ---------------- #

def load_alias_map(path: Optional[str]) -> Tuple[Dict[str,str], Dict[str,str]]:
    program_alias: Dict[str,str] = {}
    copybook_alias: Dict[str,str] = {}
    if not path:
        return program_alias, copybook_alias
    try:
        data = json.load(open(path,'r',encoding='utf-8'))
    except Exception as e:  # pragma: no cover
        print(f"[WARN] Failed to load alias JSON '{path}': {e}", file=sys.stderr)
        return program_alias, copybook_alias
    # Defensive: if the root is not a dict, ignore (older experimental formats could be list/array)
    if not isinstance(data, dict):
        return program_alias, copybook_alias
    # If top-level has PROGRAM / COPYBOOK keys
    if 'PROGRAM' in data or 'COPYBOOK' in data:
        for kind, target in (('PROGRAM', program_alias), ('COPYBOOK', copybook_alias)):
            mapping = data.get(kind, {}) or {}
            if isinstance(mapping, dict):
                for k,v in mapping.items():
                    if not isinstance(k,str) or not isinstance(v,str):
                        continue
                    nk = normalize_name(k); nv = normalize_name(v)
                    if nk and nv:
                        target[nk] = nv
    else:
        # Treat flat dict as both kinds
        for k,v in data.items():
            if not isinstance(k,str) or not isinstance(v,str):
                continue
            nk = normalize_name(k); nv = normalize_name(v)
            if nk and nv:
                program_alias[nk] = nv
                copybook_alias[nk] = nv
    return program_alias, copybook_alias

# ---------------- Catalog Assembly ---------------- #

def apply_aliases(names: Set[str], alias_map: Dict[str,str]) -> Tuple[Set[str], int]:
    mapped: Set[str] = set()
    alias_count = 0
    for n in names:
        if n in alias_map:
            mapped.add(alias_map[n])
            alias_count += 1
        else:
            mapped.add(n)
    return mapped, alias_count

# ---------------- Main Orchestration ---------------- #

def build_catalog(args) -> dict:
    ep, key = _resolve_endpoint_key(args)
    index_programs: Set[str] = set()
    index_copybooks: Set[str] = set()

    # Collect from indexes (best-effort)
    if ep and key and requests is not None:
        if args.verbose: print('[INFO] Fetching program IDs from program meta index...')
        index_programs |= fetch_program_ids_from_meta(ep, key)
        if args.verbose: print(f'  +{len(index_programs)} (program_meta)')
        if args.verbose: print('[INFO] Fetching program IDs from calls index...')
        index_programs |= fetch_program_ids_from_calls(ep, key)
        if args.verbose: print(f'  total {len(index_programs)} after calls')
        if args.verbose: print('[INFO] Fetching programs & copybooks from copybook usage index...')
        prog_from_usage, copy_from_usage = fetch_program_and_copybooks_from_usage(ep, key)
        index_programs |= prog_from_usage
        index_copybooks |= copy_from_usage
        if args.verbose: print(f'  programs now {len(index_programs)} copybooks {len(index_copybooks)}')
    else:
        if args.verbose: print('[WARN] Endpoint/key or requests missing; skipping index discovery.')

    # Filesystem discovery
    fs_programs, fs_copybooks = discover_files(args.root)
    if args.verbose:
        print(f'[INFO] Filesystem discovered programs={len(fs_programs)} copybooks={len(fs_copybooks)}')

    # Load aliases
    prog_alias_map, copy_alias_map = load_alias_map(args.alias_json)

    # Apply aliases
    index_programs_mapped, alias_prog_count = apply_aliases(index_programs, prog_alias_map)
    fs_programs_mapped, alias_prog_fs = apply_aliases(fs_programs, prog_alias_map)
    index_copybooks_mapped, alias_cpy_count = apply_aliases(index_copybooks, copy_alias_map)
    fs_copybooks_mapped, alias_cpy_fs = apply_aliases(fs_copybooks, copy_alias_map)

    # Combined sets
    combined_programs = index_programs_mapped | fs_programs_mapped
    combined_copybooks = index_copybooks_mapped | fs_copybooks_mapped

    missing_in_index_prog = sorted(fs_programs_mapped - index_programs_mapped)
    missing_on_disk_prog = sorted(index_programs_mapped - fs_programs_mapped)
    missing_in_index_cpy = sorted(fs_copybooks_mapped - index_copybooks_mapped)
    missing_on_disk_cpy = sorted(index_copybooks_mapped - fs_copybooks_mapped)

    catalog = {
        'programs': {
            'filesystem_count': len(fs_programs_mapped),
            'index_count': len(index_programs_mapped),
            'combined_count': len(combined_programs),
            'missing_in_index': missing_in_index_prog[:200],  # truncate for readability
            'missing_on_disk': missing_on_disk_prog[:200],
            'alias_mapped': alias_prog_count + alias_prog_fs,
            'coverage_pct_index_vs_fs': (100.0 * (len(fs_programs_mapped) - len(missing_in_index_prog)) / len(fs_programs_mapped)) if fs_programs_mapped else None
        },
        'copybooks': {
            'filesystem_count': len(fs_copybooks_mapped),
            'index_count': len(index_copybooks_mapped),
            'combined_count': len(combined_copybooks),
            'missing_in_index': missing_in_index_cpy[:200],
            'missing_on_disk': missing_on_disk_cpy[:200],
            'alias_mapped': alias_cpy_count + alias_cpy_fs,
            'coverage_pct_index_vs_fs': (100.0 * (len(fs_copybooks_mapped) - len(missing_in_index_cpy)) / len(fs_copybooks_mapped)) if fs_copybooks_mapped else None
        },
        'timestamp': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    }
    return catalog

# ---------------------------------------------------------------------------
# Spec-required simple discovery & resolution API
# ---------------------------------------------------------------------------

_ALIAS_PROGRAM_MAP: Dict[str,str] = {}
_ALIAS_COPYBOOK_MAP: Dict[str,str] = {}
_ALIAS_LOADED_FROM: Optional[str] = None

def load_aliases(path: str = 'data/aliases.json') -> None:
    """Load aliases (idempotent). Wraps load_alias_map, populating module-level caches.

    Missing file is silently ignored.
    """
    global _ALIAS_PROGRAM_MAP, _ALIAS_COPYBOOK_MAP, _ALIAS_LOADED_FROM
    if path == _ALIAS_LOADED_FROM:
        return
    if not os.path.exists(path):
        _ALIAS_PROGRAM_MAP = {}
        _ALIAS_COPYBOOK_MAP = {}
        _ALIAS_LOADED_FROM = path
        return
    p_map, c_map = load_alias_map(path)
    _ALIAS_PROGRAM_MAP = p_map
    _ALIAS_COPYBOOK_MAP = c_map
    _ALIAS_LOADED_FROM = path

def _maybe_resolve_endpoint_key(endpoint: Optional[str], key: Optional[str]) -> Tuple[Optional[str], Optional[str]]:
    class ArgsTmp:  # lightweight wrapper
        def __init__(self, endpoint, key):
            self.endpoint = endpoint
            self.key = key
    return _resolve_endpoint_key(ArgsTmp(endpoint, key))

def discover_program_ids(endpoint: Optional[str] = None, key: Optional[str] = None, src_root: str = 'cobol_src', disable_index: bool = False) -> List[str]:
    """Discover program IDs.

    Preference:
      1. Indexes (program_meta, calls) if accessible
      2. Filesystem glob under src_root for *.CBL / *.COB
    """
    ep, k = (None, None) if disable_index else _maybe_resolve_endpoint_key(endpoint, key)
    discovered: Set[str] = set()
    if ep and k and requests is not None:
        discovered |= fetch_program_ids_from_meta(ep, k)
        discovered |= fetch_program_ids_from_calls(ep, k)
    if not discovered:
        fs_programs, _ = discover_files(src_root)
        discovered |= fs_programs
    return sorted(discovered)

def fetch_copybook_names_from_meta(ep: str, key: str) -> Set[str]:
    out: Set[str] = set()
    for row in _search_iter(ep, key, COPYBOOK_META_INDEX, 'copybook_name'):
        v = row.get('copybook_name')
        if v:
            out.add(normalize_name(v))
    return out

def discover_copybook_names(endpoint: Optional[str] = None, key: Optional[str] = None, src_root: str = 'cobol_src', disable_index: bool = False) -> List[str]:
    """Discover copybook names.

    Preference:
      1. Copybook meta + usage indexes
      2. Filesystem glob under src_root for *.CPY
    """
    ep, k = (None, None) if disable_index else _maybe_resolve_endpoint_key(endpoint, key)
    discovered: Set[str] = set()
    if ep and k and requests is not None:
        discovered |= fetch_copybook_names_from_meta(ep, k)
        _, usage_copybooks = fetch_program_and_copybooks_from_usage(ep, k)
        discovered |= usage_copybooks
    if not discovered:
        _, fs_copybooks = discover_files(src_root)
        discovered |= fs_copybooks
    return sorted(discovered)

def resolve_program(name: str) -> str:
    """Normalize then apply alias mapping for a program name."""
    n = normalize_name(name)
    return _ALIAS_PROGRAM_MAP.get(n, n)

def resolve_copybook(name: str) -> str:
    """Normalize then apply alias mapping for a copybook name."""
    n = normalize_name(name)
    return _ALIAS_COPYBOOK_MAP.get(n, n)


def main():  # pragma: no cover - CLI wrapper
    ap = argparse.ArgumentParser(description='Discover & normalize COBOL programs and copybooks.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--alias-json', help='Alias JSON (flat or {PROGRAM:{},COPYBOOK:{}})')
    ap.add_argument('--src-root', default='cobol_src', help='Filesystem root for fallback scan (default: cobol_src)')
    # Backward compatibility legacy flag
    ap.add_argument('--root', help='(Deprecated) Use --src-root instead')
    ap.add_argument('--full-json', action='store_true', help='Emit full catalog JSON instead of simple counts')
    ap.add_argument('--no-index', action='store_true', help='Disable index-based discovery (filesystem only)')
    ap.add_argument('--output', help='Optional file path for full JSON')
    ap.add_argument('--verbose', action='store_true')
    args = ap.parse_args()

    # Harmonize legacy --root to new --src-root if supplied
    if args.root and not args.src_root:
        args.src_root = args.root
    if args.root and args.root != args.src_root and args.verbose:
        print('[WARN] --root is deprecated; prefer --src-root', file=sys.stderr)

    # Load aliases (non-fatal)
    if args.alias_json:
        load_aliases(args.alias_json)
    else:
        load_aliases()  # default path if exists

    if args.full_json:
        catalog = build_catalog(args)
        js = json.dumps(catalog, indent=2)
        if args.output:
            with open(args.output,'w',encoding='utf-8') as f:
                f.write(js)
            print(f'[INFO] Catalog written to {args.output} ({len(js)} bytes)')
        else:
            print(js)
        return

    # Simple spec output mode
    programs = discover_program_ids(args.endpoint, args.key, args.src_root, disable_index=args.no_index)
    copybooks = discover_copybook_names(args.endpoint, args.key, args.src_root, disable_index=args.no_index)
    print(f"Programs discovered: {len(programs)}")
    print(f"Copybooks discovered: {len(copybooks)}")
    print(f"Sample programs: {programs[:10]}")
    print(f"Sample copybooks: {copybooks[:10]}")

if __name__ == '__main__':
    main()
