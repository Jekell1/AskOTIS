"""Verify and inspect COPY statement usage documents in the new_cobol_copybook_usage index.

Primary goals:
  * Quickly answer: "Who uses SCREEN.CPY?" (or any other copybook)
  * Show distinct programs, counts, paragraph/section distribution
  * Report embedding coverage (has_vector true/false)
  * Optional raw sample output

Usage examples:
  python verify_copybook_usage.py --copybook SCREEN.CPY
  python verify_copybook_usage.py --copybook screen --no-ext   # treat provided token as base name (adds .CPY)
  python verify_copybook_usage.py --copybook SCREEN.CPY --sample 10 --show-paragraphs
  python verify_copybook_usage.py --copybook SCREEN.CPY --json > screen_usage.json

Notes:
  * Matches are case-insensitive; copybook name is normalized to uppercase.
  * Filter uses copybook_name_plain eq 'NAME.CPY' OR copybook_name eq 'NAME.CPY' to catch path variants.
  * normalized_copybook_name is NOT filterable in the index schema, so we do not rely on it.
  * Paginates through all results (1k page size) using skip until exhausted.
"""
from __future__ import annotations
import os, sys, json, argparse, requests, time
from collections import Counter, defaultdict

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_copybook_usage'
PAGE = 1000  # Azure Cognitive Search max per request

SELECT = 'usage_id,program_id,program_name,copybook_name,copybook_name_plain,section,paragraph_name,line_start,line_end,file_path,has_vector'


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key (set AZURE_SEARCH_ENDPOINT & AZURE_SEARCH_KEY or provide local.settings.json)')
        sys.exit(1)
    return ep.rstrip('/'), key


def fetch_all(ep: str, key: str, copybook: str):
    # Build filter (uppercase name with .CPY extension)
    name = copybook.upper()
    if not name.endswith('.CPY'):
        name = name + '.CPY'
    filt = f"(copybook_name_plain eq '{name}') or (copybook_name eq '{name}')"
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type':'application/json'}
    all_rows = []
    skip = 0
    total = None
    while True:
        body = {
            'search': '*',
            'filter': filt,
            'top': PAGE,
            'skip': skip,
            'select': SELECT,
            'count': True
        }
        r = requests.post(url, headers=headers, json=body, timeout=120)
        if r.status_code != 200:
            print('[ERROR] Query failed', r.status_code, r.text[:500])
            sys.exit(2)
        data = r.json()
        if total is None:
            total = data.get('@odata.count')
        rows = data.get('value', [])
        all_rows.extend(rows)
        if len(rows) < PAGE:
            break
        skip += PAGE
    return name, total or len(all_rows), all_rows


def analyze(rows):
    programs = Counter(r.get('program_id') for r in rows)
    sections = Counter(r.get('section') for r in rows)
    paragraphs = Counter(r.get('paragraph_name') for r in rows if r.get('paragraph_name'))
    has_vec = sum(1 for r in rows if r.get('has_vector'))
    no_vec = len(rows) - has_vec
    return {
        'usage_doc_count': len(rows),
        'distinct_programs': len(programs),
        'program_usage_top10': programs.most_common(10),
        'section_distribution': sections.most_common(),
        'paragraphs_top15': paragraphs.most_common(15),
        'embedding': {'with_vector': has_vec, 'without_vector': no_vec}
    }


def main():
    ap = argparse.ArgumentParser(description='Verify copybook usage occurrences for a given copybook')
    ap.add_argument('--copybook', required=True, help='Copybook name token (with or without .CPY)')
    ap.add_argument('--no-ext', action='store_true', help='Treat provided token as basename (append .CPY even if dot present)')
    ap.add_argument('--sample', type=int, default=0, help='Print N sample usage rows')
    ap.add_argument('--show-paragraphs', action='store_true', help='Include paragraph distribution in stdout (always in JSON)')
    ap.add_argument('--json', action='store_true', help='Emit full JSON summary to stdout (samples + stats)')
    ap.add_argument('--raw', action='store_true', help='Include raw rows (can be large) in JSON output')
    args = ap.parse_args()

    token = args.copybook.strip().upper()
    if args.no_ext and token.endswith('.CPY'):
        token = token[:-4]
    if not token.endswith('.CPY'):
        token = token + '.CPY'

    load_local_settings()
    ep, key = resolve_endpoint_key()
    t0 = time.time()
    name, total_reported, rows = fetch_all(ep, key, token)
    stats = analyze(rows)
    elapsed = time.time() - t0

    if args.json:
        out = {
            'query_copybook': name,
            'reported_total': total_reported,
            'retrieved_docs': len(rows),
            'stats': stats,
            'sample': rows[:args.sample] if args.sample else [],
        }
        if args.raw:
            out['rows'] = rows
        print(json.dumps(out, indent=2))
        return

    print(f"Copybook: {name}")
    print(f"Usage docs: {stats['usage_doc_count']} (reported={total_reported}) distinct_programs={stats['distinct_programs']} time={elapsed:.2f}s")
    print(f"Embedding coverage: {stats['embedding']['with_vector']} with vectors, {stats['embedding']['without_vector']} without")
    print("Top programs (up to 10):")
    for prog,count in stats['program_usage_top10']:
        print(f"  {prog}: {count}")
    print("Section distribution:")
    for sec,count in stats['section_distribution']:
        print(f"  {sec}: {count}")
    if args.show_paragraphs:
        print("Paragraphs (top 15):")
        for para,count in stats['paragraphs_top15']:
            print(f"  {para}: {count}")
    if args.sample:
        print(f"\nSample {args.sample} docs (truncated fields):")
        for r in rows[:args.sample]:
            print(json.dumps({k:r.get(k) for k in ('usage_id','program_id','file_path','line_start','copybook_name_plain','section','paragraph_name','has_vector')}, indent=2))

if __name__ == '__main__':
    main()
