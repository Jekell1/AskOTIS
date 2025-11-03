#!/usr/bin/env python3
"""expand_facts.py

Enrich fused fact results with paragraph text and optional +/- context paragraphs.

Input: JSON array from hybrid_fact_search.py (stdin or file) where each item has `fact_text` possibly
containing 'paragraph <NAME>' pattern. We derive paragraph names and fetch from paragraph index.

Strategy:
  1. Extract candidate paragraph identifiers via regex from fact_text (e.g., 'paragraph NAME').
  2. Query paragraph index filtering by program_id (if available) OR search on paragraph name.
  3. Collect main paragraph plus optional previous/next paragraphs for context window.
  4. Merge into enriched JSON output.

Usage:
    python hybrid_fact_search.py --index cobol-facts-v3l --program ORDERS --question "reversal" > facts.json
    python expand_facts.py --program ORDERS --paragraph-index cobol-paragraphs-v3 < facts.json > facts_expanded.json

"""
import os, sys, json, argparse, re, requests

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")
PARA_SELECT = 'para_id,name,kind,start_line,end_line,text'

PARA_NAME_RE = re.compile(r'paragraph\s+([A-Z0-9-]+)', re.IGNORECASE)

def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

def azure_search(ep, key, index, search, flt=None, top=5, select=None):
    url = f"{ep.rstrip('/')}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body = { 'search': search if search else '*', 'top': top }
    if select:
        body['select'] = select
    if flt:
        body['filter'] = flt
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Paragraph search error {r.status_code}: {r.text[:400]}")
    return r.json().get('value', [])

def build_para_lookup(ep, key, para_index, program_id, names):
    out = {}
    for name in sorted(names):
        vals = azure_search(ep, key, para_index, name, flt=None, top=5, select=PARA_SELECT)
        chosen = None
        for v in vals:
            if v.get('name','').upper() == name.upper():
                chosen = v; break
        if not chosen and vals:
            chosen = vals[0]
        if chosen:
            out[name.upper()] = chosen
    return out

def context_paras(ep, key, para_index, program_id, para_doc, window):
    if window <= 0 or not para_doc:
        return []
    # We approximate context by searching for program + nearby line ranges.
    # Since Azure Search doesn't support numeric range queries without indexing numeric fields as filterable, we fetch a broader set then filter client-side.
    start = para_doc.get('start_line'); end = para_doc.get('end_line')
    if start is None or end is None:
        return []
    # Without program_id field in select/filter, retrieve a broader slice and rely on name proximity
    vals = azure_search(ep, key, para_index, '*', flt=None, top=200, select=PARA_SELECT)
    res = []
    for v in vals:
        s = v.get('start_line'); e = v.get('end_line')
        if s is None or e is None:
            continue
        # If the paragraph range is within window steps by start_line ordering heuristic
        if (s < start and start - s <= window) or (s > end and s - end <= window):
            res.append(v)
    # Keep a small bounded set
    return sorted(res, key=lambda x: x.get('start_line') or 0)[:2*window]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--paragraph-index', default='cobol-paragraphs-v3')
    ap.add_argument('--program', help='Program id (improves filtering)')
    ap.add_argument('--context-window', type=int, default=3, help='Approx line-distance threshold for neighboring paragraphs')
    ap.add_argument('--input', help='Input JSON file (defaults to stdin)')
    ap.add_argument('--output', help='Output file (defaults to stdout)')
    args = ap.parse_args()
    load_settings()
    ep = os.getenv('SEARCH_ENDPOINT'); key = os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)

    data = None
    if args.input:
        data = json.load(open(args.input,'r'))
    else:
        data = json.load(sys.stdin)
    if not isinstance(data, list):
        print('Input must be JSON array', file=sys.stderr); sys.exit(3)

    # Collect paragraph names
    para_names = set()
    for item in data:
        ft = item.get('fact_text') or ''
        for m in PARA_NAME_RE.finditer(ft):
            para_names.add(m.group(1))

    para_lookup = build_para_lookup(ep, key, args.paragraph_index, args.program, para_names)

    enriched = []
    for item in data:
        ft = item.get('fact_text') or ''
        names = [m.group(1).upper() for m in PARA_NAME_RE.finditer(ft)]
        attached = []
        contexts = []
        for nm in names:
            pd = para_lookup.get(nm)
            if pd:
                attached.append(pd)
                ctx = context_paras(ep, key, args.paragraph_index, args.program, pd, args.context_window)
                contexts.extend(ctx)
        # Deduplicate context by para_id
        seen = set(p.get('para_id') for p in attached)
        final_ctx = []
        for c in contexts:
            pid = c.get('para_id')
            if pid and pid not in seen:
                seen.add(pid)
                final_ctx.append(c)
        item_out = dict(item)
        item_out['paragraphs'] = attached
        item_out['context_paragraphs'] = final_ctx
        enriched.append(item_out)

    if args.output:
        json.dump(enriched, open(args.output,'w'), indent=2)
    else:
        print(json.dumps(enriched, indent=2))

if __name__ == '__main__':
    main()
