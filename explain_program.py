#!/usr/bin/env python3
"""explain_program.py

Produce a structured, evidence-linked explanation of how a COBOL program works
using fact documents (optionally vector-ranked) from the facts index.

Usage:
    python explain_program.py --program ORDERS --index cobol-facts-v3l
"""
import os, sys, json, argparse, requests, math

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")

def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

def search_facts(ep, key, index, program_id, top=100):
    ep = ep.rstrip('/')
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    # We rely on simple keyword star search + program filter; ranking can later be hybrid.
    flt = f"program_id eq '{program_id}'"
    body = {
        "search": "*",
        "filter": flt,
        "top": top,
        "select": "fact_id,program_id,action_role,posting_type,gating_cond,fact_confidence,sources,fact_text"
    }
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Search error {r.status_code}: {r.text[:400]}")
    return r.json().get('value', [])

def classify_phases(facts):
    phases = {"Initialization": [], "Validation": [], "Main Processing": [], "Posting": [], "Error / Edge Handling": [], "Other": []}
    for f in facts:
        txt = (f.get('fact_text') or '')
        role = (f.get('action_role') or '').lower()
        posting = (f.get('posting_type') or '').lower()
        bucket = None
        if 'initialize' in txt.lower() or role.startswith('init'):
            bucket = 'Initialization'
        elif 'validate' in txt.lower() or 'check ' in txt.lower():
            bucket = 'Validation'
        elif posting:
            bucket = 'Posting'
        elif 'error' in txt.lower() or 'invalid' in txt.lower():
            bucket = 'Error / Edge Handling'
        elif 'process' in txt.lower() or 'loop' in txt.lower():
            bucket = 'Main Processing'
        else:
            bucket = 'Other'
        phases[bucket].append(f)
    return phases

def summarize_phase(name, facts):
    if not facts:
        return None
    lines = []
    # Sort by confidence desc then by gating presence
    facts_sorted = sorted(facts, key=lambda x: (-(x.get('fact_confidence') or 0.0), len(x.get('gating_cond') or '')))
    for f in facts_sorted:
        conf = f.get('fact_confidence')
        gating = f.get('gating_cond') or ''
        snippet = f.get('fact_text') or ''
        snippet = snippet.strip().replace('\n',' ')
        if len(snippet) > 220:
            snippet = snippet[:217] + '...'
        lines.append({
            'fact_id': f.get('fact_id'),
            'confidence': conf,
            'gating': gating[:180],
            'summary': snippet
        })
    return {'phase': name, 'count': len(facts_sorted), 'items': lines[:12]}

def build_narrative(phases_struct):
    order = ['Initialization','Validation','Main Processing','Posting','Error / Edge Handling','Other']
    narrative_sections = []
    for name in order:
        ph = summarize_phase(name, phases_struct[name])
        if ph:
            narrative_sections.append(ph)
    return narrative_sections

def format_output(program_id, narrative):
    out_lines = [f"Program: {program_id}", "Explanation (auto-structured):", ""]
    for section in narrative:
        out_lines.append(f"## {section['phase']} ({section['count']} facts)")
        for item in section['items']:
            meta = []
            if item['confidence'] is not None:
                meta.append(f"conf={item['confidence']:.2f}")
            if item['gating']:
                meta.append(f"gate={item['gating']}")
            meta_str = (' [' + '; '.join(meta) + ']') if meta else ''
            out_lines.append(f"- {item['summary']} (fact {item['fact_id']}){meta_str}")
        out_lines.append("")
    return '\n'.join(out_lines)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--program', required=True)
    ap.add_argument('--index', default='cobol-facts-v3l')
    ap.add_argument('--top', type=int, default=150)
    args = ap.parse_args()
    load_settings()
    ep = os.getenv('SEARCH_ENDPOINT'); key = os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)
    facts = search_facts(ep, key, args.index, args.program, top=args.top)
    if not facts:
        print(f"No facts found for program {args.program}")
        return
    phases = classify_phases(facts)
    narrative = build_narrative(phases)
    print(format_output(args.program, narrative))

if __name__ == '__main__':
    main()
