#!/usr/bin/env python3
"""qa_facts_probe.py

Quality probe for facts index: coverage, role distribution, posting types, gating condition presence,
embedding status, and paragraph join sanity.

Usage:
    python qa_facts_probe.py --index-suffix v3 --program ORDERS --sample 15

Outputs a concise JSON summary plus optional samples.
"""
import os, json, argparse, statistics, random, requests
from collections import Counter

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

def search(index, body):
    ep = (os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    r = session.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Search error {r.status_code}: {r.text[:300]}")
    return r.json().get('value',[]), r.json().get('@odata.count')


def fetch_all_facts(index, program: str|None, cap=4000):
    flt = None
    if program:
        flt = f"program_id eq '{program}'"
    body = {"search":"*", "top": cap, "filter": flt, "count": True}
    vals, total = search(index, body)
    return vals, total


def fetch_paragraph_map(index, para_ids):
    out={}
    for i in range(0, len(para_ids), 32):
        subset = para_ids[i:i+32]
        flt = ' or '.join([f"para_id eq '{p}'" for p in subset])
        vals,_ = search(index, {"search":"*", "filter": flt, "top": len(subset)})
        for v in vals:
            out[v['para_id']] = v
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index-suffix', default='')
    ap.add_argument('--program')
    ap.add_argument('--sample', type=int, default=15)
    ap.add_argument('--show-samples', action='store_true')
    args = ap.parse_args()
    load_settings()
    suffix = args.index_suffix
    if suffix and not suffix.startswith('-'):
        suffix='-'+suffix
    fact_index = FACT_BASE + suffix
    para_index = PARA_BASE + suffix

    facts, total = fetch_all_facts(fact_index, args.program)
    if not facts:
        print(json.dumps({"error":"no_facts"}))
        return
    roles = Counter([f.get('action_role') or 'NONE' for f in facts])
    postings = Counter([f.get('posting_type') or 'NONE' for f in facts])
    gating_present = sum(1 for f in facts if f.get('gating_cond'))
    with_vectors = sum(1 for f in facts if f.get('has_vector'))

    sample = random.sample(facts, min(args.sample, len(facts)))
    all_para_ids = []
    for f in sample:
        for sid in f.get('sources',[]) or []:
            if sid not in all_para_ids:
                all_para_ids.append(sid)
    para_map = fetch_paragraph_map(para_index, all_para_ids) if all_para_ids else {}

    enriched_samples = []
    for f in sample:
        enriched_samples.append({
            "fact_id": f['fact_id'],
            "fact_text": f.get('fact_text'),
            "action_role": f.get('action_role'),
            "posting_type": f.get('posting_type'),
            "gating_cond": f.get('gating_cond'),
            "has_vector": f.get('has_vector'),
            "sources": [
                {"para_id": sid, "name": para_map.get(sid,{}).get('name'), "text": para_map.get(sid,{}).get('text')} for sid in f.get('sources',[]) or []
            ]
        })

    summary = {
        "total_facts_retrieved": len(facts),
        "reported_total": total,
        "role_distribution": roles,
        "posting_type_distribution": postings,
        "gating_condition_count": gating_present,
        "gating_condition_ratio": gating_present/len(facts),
        "with_vector_count": with_vectors,
        "with_vector_ratio": with_vectors/len(facts),
        "sample_count": len(enriched_samples)
    }
    out = {"summary": summary}
    if args.show_samples:
        out["samples"] = enriched_samples
    print(json.dumps(out, indent=2)[:30000])

if __name__ == '__main__':
    main()
