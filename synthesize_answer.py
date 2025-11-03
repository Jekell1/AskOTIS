#!/usr/bin/env python3
"""synthesize_answer.py

End-to-end question answering over COBOL facts + paragraphs.
Pipeline:
 1. Hybrid fact retrieval (imports and reuses logic from hybrid_fact_search)
 2. Paragraph enrichment (reuses logic from expand_facts)
 3. Lightweight synthesis: cluster facts, organize by gating conditions / action role, and emit
    a narrative answer with inline citations. Citations format: [fact:<id>|para:<name>|lines start-end]
 4. Optional JSON export of the intermediate/final structures.

This script deliberately avoids an LLM generation step for determinism; it creates a
structured narrative from retrieved facts + paragraph excerpts. You can later layer
an LLM on top if desired.

Usage:
    python synthesize_answer.py --program ORDERS --question "How does ORDERS handle reversal payoff gating?" \
     --facts-index cobol-facts-v3l --paragraph-index cobol-paragraphs-v3 --top 18 --max-answer-facts 10

Outputs plain text answer to stdout; optional --output-json writes a JSON with fields:
  {
    "question": str,
    "program": str,
    "facts_used": [...],    # truncated fact records with paragraphs
    "answer_text": str
  }

Environment variables reused: SEARCH_ENDPOINT, SEARCH_KEY, Azure OpenAI embedding creds for question embedding.
"""
import os, sys, json, argparse, math, re, requests, tempfile, textwrap
from typing import List, Dict, Any

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
VECTOR_SEARCH_PROFILE = os.environ.get("FACTS_VECTOR_PROFILE", "facts-profile")

# ---------------- Settings Loader -----------------
def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

# ---------------- Embedding (reuse minimal code) ---------
def embed_question(text: str):
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    model = (os.getenv('AZURE_OPENAI_EMBED_MODEL') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 'text-embedding-3-large')
    if not azure_ep or not azure_key:
        raise SystemExit('Missing Azure embedding credentials for question embedding')
    url = f"{azure_ep.rstrip('/')}/openai/deployments/{model}/embeddings?api-version=2024-02-15-preview"
    r = requests.post(url, headers={'api-key': azure_key, 'Content-Type':'application/json'}, json={'input': [text]}, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Embedding error {r.status_code}: {r.text[:400]}")
    data = r.json().get('data',[])
    if not data:
        raise SystemExit('No embedding returned for question')
    return data[0]['embedding']

# ---------------- Retrieval -----------------
FACT_SELECT = 'fact_id,fact_text,program_id,action_role,posting_type,gating_cond,fact_confidence'

def keyword_search(ep, key, index, query, program, top):
    url = f"{ep.rstrip('/')}/indexes/{index}/docs/search?api-version={API_VERSION}"
    flt = f"program_id eq '{program}'" if program else None
    body = { 'search': query if query.strip() else '*', 'top': top, 'select': FACT_SELECT }
    if flt: body['filter'] = flt
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Keyword search error {r.status_code}: {r.text[:400]}")
    vals = r.json().get('value', [])
    out=[]
    for rank,v in enumerate(vals, start=1):
        out.append({'fact_id': v.get('fact_id'), 'rank': rank, 'source':'keyword', 'raw': v})
    return out

def vector_search(ep, key, index, qvec, program, top):
    url = f"{ep.rstrip('/')}/indexes/{index}/docs/search?api-version={API_VERSION}"
    flt = f"program_id eq '{program}'" if program else None
    body={
        'vectorQueries':[{'kind':'vector','vector': qvec,'k': top,'fields':'fact_vector'}],
        'top': top,
        'select': FACT_SELECT
    }
    if flt: body['filter']=flt
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Vector search error {r.status_code}: {r.text[:400]}")
    vals = r.json().get('value', [])
    out=[]
    for rank,v in enumerate(vals, start=1):
        out.append({'fact_id': v.get('fact_id'), 'rank': rank, 'source':'vector', 'raw': v})
    return out

def rrf_fuse(keyword_results, vector_results, kparam):
    fused={}
    def add(lst):
        for r in lst:
            fid = r['fact_id']
            if not fid: continue
            entry = fused.setdefault(fid, {'fact_id': fid, 'sources':{}, 'raw': r['raw']})
            entry['sources'][r['source']] = r['rank']
    add(keyword_results)
    add(vector_results)
    for fid,e in fused.items():
        score=0.0
        for src,rank in e['sources'].items():
            score += 1.0/(rank + kparam)
        e['score']=score
    return list(fused.values())

# ---------------- Paragraph Enrichment (inline minimal from expand_facts) ---------
PARA_NAME_RE = re.compile(r'paragraph\s+([A-Z0-9-]+)', re.IGNORECASE)
PARA_SELECT = 'para_id,name,kind,start_line,end_line,text'

def para_search(ep, key, para_index, name, top=5):
    url = f"{ep.rstrip('/')}/indexes/{para_index}/docs/search?api-version={API_VERSION}"
    body={'search': name, 'top': top, 'select': PARA_SELECT}
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Paragraph search error {r.status_code}: {r.text[:400]}")
    return r.json().get('value', [])

def build_para_lookup(ep, key, para_index, names):
    out={}
    for nm in sorted(names):
        vals = para_search(ep, key, para_index, nm)
        chosen=None
        for v in vals:
            if v.get('name','').upper()==nm.upper():
                chosen=v; break
        if not chosen and vals: chosen=vals[0]
        if chosen:
            out[nm.upper()]=chosen
    return out

def context_neighbors(ep, key, para_index, para_doc, window):
    if window<=0 or not para_doc: return []
    url = f"{ep.rstrip('/')}/indexes/{para_index}/docs/search?api-version={API_VERSION}"
    # broad grab limited to 200
    body={'search':'*','top':200,'select': PARA_SELECT}
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code>=300:
        return []
    vals=r.json().get('value',[])
    start=para_doc.get('start_line'); end=para_doc.get('end_line')
    if start is None or end is None: return []
    res=[]
    for v in vals:
        s=v.get('start_line'); e=v.get('end_line')
        if s is None or e is None: continue
        if (s < start and start - s <= window) or (s > end and s - end <= window):
            res.append(v)
    return sorted(res, key=lambda x: x.get('start_line') or 0)[:2*window]

# ---------------- Synthesis -----------------

def select_facts(fused_sorted: List[Dict[str,Any]], limit:int)->List[Dict[str,Any]]:
    return fused_sorted[:limit]

def fact_key(f):
    role=(f.get('action_role') or 'ZZZ')
    gate=1 if f.get('gating_cond') else 0
    conf=-(f.get('fact_confidence') or 0.0)
    return (role, gate, conf)

def cluster_facts(facts: List[Dict[str,Any]]):
    buckets={}
    for f in facts:
        role = (f.get('action_role') or 'OTHER').upper()
        buckets.setdefault(role, []).append(f)
    for k in buckets:
        buckets[k]=sorted(buckets[k], key=fact_key)
    return dict(sorted(buckets.items(), key=lambda x: (0 if x[0]=='REVERSAL' else 1, x[0])))

def format_paragraph_excerpt(pdoc: Dict[str,Any], max_lines=14):
    if not pdoc: return ''
    txt=(pdoc.get('text') or '').rstrip()
    lines=txt.splitlines()
    if len(lines)>max_lines:
        lines=lines[:max_lines]+['...']
    joined='\n'.join(lines)
    return joined

def make_citation(fact: Dict[str,Any], para_doc: Dict[str,Any]):
    if not para_doc:
        return f"[fact:{fact.get('fact_id')}]"
    return f"[fact:{fact.get('fact_id')}|para:{para_doc.get('name')}|lines {para_doc.get('start_line')}-{para_doc.get('end_line')}]"

def synthesize_answer(question: str, program: str, facts: List[Dict[str,Any]]):
    clusters = cluster_facts(facts)
    lines=[f"Question: {question}", f"Program: {program}", "", "Answer (evidence-grounded):", ""]
    for role, facts_list in clusters.items():
        lines.append(f"### {role} ({len(facts_list)})")
        for f in facts_list:
            para_doc=None
            attached = f.get('paragraphs') or []
            if attached:
                para_doc = attached[0]
            cit = make_citation(f, para_doc)
            summary = (f.get('fact_text') or '').strip()
            summary = re.sub(r'\s+', ' ', summary)
            if len(summary)>240: summary=summary[:237]+'...'
            gating = f.get('gating_cond')
            if gating:
                summary += f" (gated by: {gating})"
            lines.append(f"- {summary} {cit}")
            if para_doc:
                excerpt = format_paragraph_excerpt(para_doc)
                indented = textwrap.indent(excerpt, '    ')
                lines.append(indented)
        lines.append("")
    return '\n'.join(lines).rstrip()+"\n"

# ---------------- Main -----------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--question', required=True)
    ap.add_argument('--program', required=True)
    ap.add_argument('--facts-index', default='cobol-facts-v3l')
    ap.add_argument('--paragraph-index', default='cobol-paragraphs-v3')
    ap.add_argument('--top', type=int, default=25, help='Initial retrieval depth per modality')
    ap.add_argument('--rrf-k', type=int, default=50)
    ap.add_argument('--max-answer-facts', type=int, default=12, help='Limit number of facts included in narrative')
    ap.add_argument('--context-window', type=int, default=3)
    ap.add_argument('--no-vector', action='store_true')
    ap.add_argument('--no-keyword', action='store_true')
    ap.add_argument('--output-json', help='Optional JSON output file with facts_used + answer_text')
    args = ap.parse_args()
    load_settings()
    ep=os.getenv('SEARCH_ENDPOINT'); key=os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)

    # Retrieval
    keyword_results=[]; vector_results=[]
    if not args.no_keyword:
        keyword_results = keyword_search(ep, key, args.facts_index, args.question, args.program, args.top)
    if not args.no_vector:
        qvec = embed_question(args.question)
        vector_results = vector_search(ep, key, args.facts_index, qvec, args.program, args.top)
    fused = rrf_fuse(keyword_results, vector_results, args.rrf_k)
    fused_sorted = sorted(fused, key=lambda x: -x['score'])

    # Enrich paragraphs
    fact_records=[]
    # extract para names
    para_names=set()
    for e in fused_sorted[:args.max_answer_facts]:
        ft = e['raw'].get('fact_text') or ''
        for m in PARA_NAME_RE.finditer(ft):
            para_names.add(m.group(1).upper())
    para_lookup = build_para_lookup(ep, key, args.paragraph_index, para_names)

    for e in fused_sorted[:args.max_answer_facts]:
        raw=e['raw']
        record={
            'fact_id': e['fact_id'],
            'score': e['score'],
            'rank_sources': e['sources'],
            'program_id': raw.get('program_id'),
            'fact_confidence': raw.get('fact_confidence'),
            'action_role': raw.get('action_role'),
            'posting_type': raw.get('posting_type'),
            'gating_cond': raw.get('gating_cond'),
            'fact_text': raw.get('fact_text'),
        }
        names=[m.group(1).upper() for m in PARA_NAME_RE.finditer(raw.get('fact_text') or '')]
        attached=[]; ctx=[]
        for nm in names:
            pd = para_lookup.get(nm)
            if pd:
                attached.append(pd)
                ctx.extend(context_neighbors(ep, key, args.paragraph_index, pd, args.context_window))
        # dedupe ctx
        seen=set(p.get('para_id') for p in attached)
        final_ctx=[]
        for c in ctx:
            pid=c.get('para_id')
            if pid and pid not in seen:
                seen.add(pid); final_ctx.append(c)
        record['paragraphs']=attached
        record['context_paragraphs']=final_ctx
        fact_records.append(record)

    answer_text = synthesize_answer(args.question, args.program, fact_records)
    print(answer_text)

    if args.output_json:
        out_js={
            'question': args.question,
            'program': args.program,
            'facts_used': fact_records,
            'answer_text': answer_text
        }
        json.dump(out_js, open(args.output_json,'w'), indent=2)

if __name__ == '__main__':
    main()
