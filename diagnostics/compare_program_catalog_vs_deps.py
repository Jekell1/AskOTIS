"""Diagnostics: Compare program catalog coverage vs new_cobol_program_deps index.

Outputs:
  - Total discovered programs (catalog)
  - Programs with dependency doc
  - Missing dependency docs (sample)
  - Extra dependency docs (not in catalog) (sample)
  - Coverage %
  - Optional embedding coverage stats (has_vector counts)

Usage:
  python diagnostics/compare_program_catalog_vs_deps.py \
      --endpoint $env:AZURE_SEARCH_ENDPOINT --key $env:AZURE_SEARCH_KEY \
      --out diagnostics/_catalog_vs_deps.json

Flags:
  --no-index-catalog  Force catalog to use filesystem-only fallback (for offline tests)
  --limit N           Limit number of discovered programs (debug)
  --show-missing      Print full missing list (otherwise sample)

Environment:
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY (or pass explicitly)

Requires indexes:
  - new_cobol_program_deps (fields: program_id, has_vector)
  - Sources used by program_catalog (program_meta, calls, usage) unless --no-index-catalog
"""
from __future__ import annotations
import os, json, argparse, sys, requests, time, pathlib
from typing import List, Dict, Any

import sys, pathlib
ROOT = pathlib.Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
from ingest import program_catalog as catalog

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
DEPS_INDEX = 'new_cobol_program_deps'
ROOT = pathlib.Path(__file__).resolve().parents[1]

def load_local_settings():
    """Load local.settings.json (preferred) or local.settings.template.json into env without overwriting existing vars."""
    for candidate in ('local.settings.json','local.settings.template.json'):
        fp = ROOT / candidate
        if not fp.is_file():
            continue
        try:
            data = json.load(fp.open('r', encoding='utf-8'))
            vals = data.get('Values', {})
            loaded = 0
            for k,v in vals.items():
                if k not in os.environ and isinstance(v, str) and v:
                    os.environ[k] = v
                    loaded += 1
            print(f"[settings] loaded {loaded} value(s) from {candidate}")
            break  # stop after first successful load
        except Exception as e:
            print(f"[settings] failed to parse {candidate}: {e}", file=sys.stderr)
            continue


def resolve_search(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def search_all_program_deps(ep: str, key: str) -> List[Dict[str,Any]]:
    out = []
    skip = 0
    page = 1000
    url = f"{ep}/indexes/{DEPS_INDEX}/docs/search.post.search?api-version={API_VERSION}"
    while True:
        body = {"search":"*","top":page,"skip":skip,"select":"program_id,has_vector"}
        r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=120)
        if r.status_code != 200:
            print(f"[ERROR] deps search failed {r.status_code}: {r.text[:300]}")
            break
        js = r.json()
        vals = js.get('value',[])
        if not vals:
            break
        out.extend(vals)
        if len(vals) < page:
            break
        skip += len(vals)
    return out


def main():
    ap = argparse.ArgumentParser(description='Compare catalog program coverage vs dependency docs.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--no-index-catalog', action='store_true', help='Disable index-based discovery for catalog (filesystem fallback)')
    ap.add_argument('--limit', type=int)
    ap.add_argument('--show-missing', action='store_true')
    ap.add_argument('--out')
    args = ap.parse_args()

    # Attempt to load local settings before resolving search credentials
    load_local_settings()
    ep, key = resolve_search(args)

    # Discover program catalog set
    programs = catalog.discover_program_ids(args.endpoint, args.key, '.', disable_index=args.no_index_catalog)
    if args.limit:
        programs = programs[:args.limit]
    catalog_set = {p for p in programs if p}

    # Fetch dependency docs
    deps_docs = search_all_program_deps(ep, key)
    deps_set = { (d.get('program_id') or '').upper() for d in deps_docs if d.get('program_id') }

    missing = sorted(catalog_set - deps_set)
    extra = sorted(deps_set - catalog_set)

    has_vec = sum(1 for d in deps_docs if d.get('has_vector'))
    total_deps = len(deps_docs)

    coverage = ( (len(catalog_set)-len(missing)) / len(catalog_set) * 100.0 ) if catalog_set else 0.0

    report = {
        'timestamp': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
        'catalog_programs': len(catalog_set),
        'deps_docs': total_deps,
        'deps_has_vector': has_vec,
        'deps_vector_coverage_pct': (has_vec/total_deps*100.0) if total_deps else 0.0,
        'missing_count': len(missing),
        'extra_count': len(extra),
        'coverage_pct': coverage,
        'missing_sample': missing[:25],
        'extra_sample': extra[:25]
    }

    # Console summary
    print('=== Catalog vs Program Deps Coverage ===')
    print(f"Catalog programs: {report['catalog_programs']}")
    print(f"Dependency docs: {report['deps_docs']} (vector coverage {report['deps_vector_coverage_pct']:.1f}%)")
    print(f"Coverage: {report['coverage_pct']:.2f}%  Missing: {report['missing_count']}  Extra: {report['extra_count']}")
    if missing:
        if args.show_missing:
            print('Missing list:')
            for m in missing:
                print('  -', m)
        else:
            print('Missing sample:', ', '.join(missing[:15]))
    if extra:
        print('Extra sample:', ', '.join(extra[:15]))

    if args.out:
        os.makedirs(os.path.dirname(args.out), exist_ok=True)
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump(report,f,indent=2)
        print('Wrote', args.out)

if __name__ == '__main__':
    main()
