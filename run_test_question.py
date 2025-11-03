#!/usr/bin/env python3
"""run_test_question.py

Lightweight test question harness over the v3 indexes.
Performs:
 1. Interpret a natural language question (simple heuristics to pick query terms)
 2. Query facts index (keyword OR vector optional later) and paragraph index
 3. Aggregate top facts + source paragraphs
 4. Produce a synthesized answer with cited paragraph names.

Usage:
    python run_test_question.py "How does ORDERS handle reversals?" --program ORDERS --index-suffix v3 --facts 8 --paras 8

Environment:
  SEARCH_ENDPOINT / SEARCH_KEY

NOTE: Vector enrichment only partially present; for now we rely on keyword search.
"""
import os, sys, json, argparse, re, requests
from typing import List, Dict

API_VERSION = os.environ.get('AZURE_SEARCH_API_VERSION','2024-07-01')
FACT_BASE = 'cobol-facts'
PARA_BASE = 'cobol-paragraphs'

session = requests.Session()

KEYWORDS_MAP = [
    (re.compile(r'reversal|reverse', re.I), ['REVERSAL','reverse','reversal','rv']),
    (re.compile(r'payment|post', re.I), ['PAYMENT','POSTING','payment','posting','post']),
    (re.compile(r'interest', re.I), ['INTEREST','interest']),
    (re.compile(r'fee|charges?', re.I), ['FEE','fee','charge','charges']),
]

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
    return r.json().get('value', [])

def derive_terms(question: str):
    q = question.strip()
    terms = []
    for rx, words in KEYWORDS_MAP:
        if rx.search(q):
            terms.extend(words)
    if not terms:
        # fallback: split into words, keep alphanumerics >3 chars
        toks = re.findall(r'[A-Za-z0-9][A-Za-z0-9-]{3,}', q)
        terms.extend(toks[:4])
    # dedupe
    out = []
    for t in terms:
        if t not in out:
            out.append(t)
    return out

def fetch_facts(program: str, fact_index: str, terms: List[str], top: int):
    query = ' OR '.join(terms) if terms else '*'
    flt = f"program_id eq '{program}'"
    body = {"search": query, "filter": flt, "top": top, "queryType":"simple"}
    return search_index(fact_index, body)

def fetch_paragraphs(program: str, para_index: str, terms: List[str], top: int):
    query = ' OR '.join(terms) if terms else '*'
    flt = f"file_id ne ''"  # could refine by file_id set derived from facts later
    body = {"search": query, "filter": flt, "top": top, "queryType":"simple"}
    return search_index(para_index, body)

def synthesize_answer(question: str, facts: List[Dict], paras: List[Dict]):
    lines = []
    if facts:
        # Group simple explanation
        roles = {}
        for f in facts:
            r = f.get('action_role') or 'OTHER'
            roles[r] = roles.get(r,0)+1
        role_summary = ', '.join(f"{k}:{v}" for k,v in sorted(roles.items(), key=lambda x:-x[1]))
        lines.append(f"Facts role distribution in retrieved set: {role_summary}.")
        # List top fact_text entries
        lines.append("Key detected actions:")
        for f in facts[:6]:
            ft = f.get('fact_text') or f.get('action_role')
            lines.append(f" - {ft} (confidence {f.get('fact_confidence')})")
    if paras:
        lines.append(f"Sample paragraphs related: {', '.join(p.get('name') for p in paras[:5] if p.get('name'))}.")
    if not lines:
        lines.append("No evidence found to answer the question.")
    answer = ' '.join(lines)
    return answer

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('question')
    ap.add_argument('--program', required=True)
    ap.add_argument('--index-suffix', default='')
    ap.add_argument('--facts', type=int, default=10)
    ap.add_argument('--paras', type=int, default=8)
    ap.add_argument('--json', action='store_true', help='Output structured JSON')
    args = ap.parse_args()
    load_settings()
    suffix = args.index_suffix
    if suffix and not suffix.startswith('-'):
        suffix='-'+suffix
    fact_index = FACT_BASE + suffix
    para_index = PARA_BASE + suffix
    terms = derive_terms(args.question)
    facts = fetch_facts(args.program, fact_index, terms, args.facts)
    paras = fetch_paragraphs(args.program, para_index, terms, args.paras)
    answer = synthesize_answer(args.question, facts, paras)
    if args.json:
        print(json.dumps({
            'question': args.question,
            'terms': terms,
            'fact_hits': facts,
            'para_hits': paras,
            'answer': answer
        }, indent=2)[:40000])
    else:
        print('QUESTION:', args.question)
        print('DERIVED TERMS:', terms)
        print('ANSWER:', answer)

if __name__ == '__main__':
    main()
