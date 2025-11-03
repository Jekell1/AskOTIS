#!/usr/bin/env python3
"""fact_paragraph_join.py

Join facts in cobol-facts(-suffix) with their source paragraphs from cobol-paragraphs(-suffix).
Outputs JSON lines (or pretty) including paragraph text for each source para_id.

Usage:
    python fact_paragraph_join.py ORDERS --index-suffix v3 --limit 10
    python fact_paragraph_join.py INVENTORY --index-suffix v3 --fact-id <id>

Environment:
  SEARCH_ENDPOINT / SEARCH_KEY
"""
import os, sys, json, argparse, requests
from typing import List, Dict

API_VERSION = os.environ.get('AZURE_SEARCH_API_VERSION','2024-07-01')
FACT_BASE = 'cobol-facts'
PARA_BASE = 'cobol-paragraphs'

session = requests.Session()


def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

def search_index(index: str, body: Dict):
    ep = (os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r = session.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Search error {r.status_code}: {r.text[:400]}")
    return r.json().get('value',[])

def fetch_facts(program: str, fact_index: str, fact_id: str|None, limit: int):
    if fact_id:
        # Filter by fact_id
        return search_index(fact_index, {"search":"*", "filter": f"fact_id eq '{fact_id}'", "top": 1})
    flt = f"program_id eq '{program}'"
    return search_index(fact_index, {"search":"*", "filter": flt, "top": limit or 50})

def fetch_paragraphs(para_index: str, para_ids: List[str]):
    out=[]
    if not para_ids:
        return out
    # Batch filter (Azure Search OR chain)
    for i in range(0, len(para_ids), 32):
        subset = para_ids[i:i+32]
        flt = ' or '.join([f"para_id eq '{p}'" for p in subset])
        vals = search_index(para_index, {"search":"*", "filter": flt, "top": len(subset)})
        out.extend(vals)
    # index by id
    return {p['para_id']: p for p in out if p.get('para_id')}


def enrich_facts(facts: List[Dict], para_index: str):
    # gather all para ids
    all_ids = []
    for f in facts:
        for sid in f.get('sources', []) or []:
            if sid not in all_ids:
                all_ids.append(sid)
    para_map = fetch_paragraphs(para_index, all_ids)
    enriched = []
    for f in facts:
        paras = [para_map.get(pid) for pid in f.get('sources', [])]
        enriched.append({
            **f,
            'paragraphs': [p for p in paras if p]
        })
    return enriched

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('program')
    ap.add_argument('--index-suffix', default='')
    ap.add_argument('--limit', type=int, default=15)
    ap.add_argument('--fact-id')
    ap.add_argument('--pretty', action='store_true')
    args = ap.parse_args()
    load_settings()
    suffix = args.index_suffix
    if suffix and not suffix.startswith('-'):
        suffix = '-' + suffix
    fact_index = FACT_BASE + suffix
    para_index = PARA_BASE + suffix
    facts = fetch_facts(args.program, fact_index, args.fact_id, args.limit)
    if not facts:
        print('No facts found')
        return
    enriched = enrich_facts(facts, para_index)
    if args.pretty:
        print(json.dumps(enriched, indent=2)[:20000])
    else:
        for e in enriched:
            print(json.dumps(e))

if __name__ == '__main__':
    main()
