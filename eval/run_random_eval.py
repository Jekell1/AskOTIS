"""Randomized evaluation harness for orchestrate_answer.

Generates synthetic questions across several categories and invokes orchestrate() in-process
for speed. Produces JSONL of per-question results plus an aggregate summary JSON.

Categories:
  - purpose (program)
  - flow (program)
  - dependency (program)
  - variable usage (variable)
  - where-used (variable)
  - ui navigation (ui path)

Data sampling strategy:
  - Pull program ids from program_meta
  - Pull variable ids & usage roles from variable_usage (variable_id)
  - Pull path ids from ui_paths

Question templates include placeholders replaced per sample.

Scoring heuristics (pass/fail):
  - purpose: evidence contains program_meta or copybook_meta index
  - flow: evidence contains program_flows OR calls OR program_deps
  - dependency: evidence contains program_deps OR calls
  - variable usage: evidence contains variable_usage OR symbol_refs
  - where-used: evidence contains symbol_refs with line_number present
  - ui: evidence contains ui_paths OR screen_nodes

Aggregate metrics: per-category pass %, overall pass %.

Usage examples:
  python eval/run_random_eval.py --programs 40 --variables 40 --uipaths 20 --out eval/out/random_eval

Outputs:
  <out>.jsonl  (per question detailed records)
  <out>_summary.json (aggregate)

Environment: relies on same credentials as orchestrate.
"""
from __future__ import annotations
import os, json, time, argparse, random, requests, sys, re
from typing import List, Dict, Any

# Ensure project root on path
ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from retrieval.orchestrate_answer import orchestrate, API, INDEX_DEFS, load_settings, resolve

RAND = random.Random()

API_VERSION = API

SAMPLE_LIMIT = 400

# ------------------ Sampling helpers ------------------

def _search_ids(ep,key,index,select,key_field, top=50):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':top,'select':select}
    try:
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
        if r.status_code!=200:
            return []
        return [row.get(key_field) for row in r.json().get('value',[]) if row.get(key_field)]
    except Exception:
        return []

def sample_program_ids(ep,key, n:int)->List[str]:
    ids=_search_ids(ep,key, INDEX_DEFS['program_meta']['index'], 'program_id', 'program_id', top=min(max(n*3,50),SAMPLE_LIMIT))
    RAND.shuffle(ids)
    return ids[:n]

def sample_variable_ids(ep,key,n:int)->List[str]:
    ids=_search_ids(ep,key, INDEX_DEFS['variable_usage']['index'], 'variable_id,usage_role,total_refs', 'variable_id', top=min(max(n*4,80),SAMPLE_LIMIT))
    RAND.shuffle(ids)
    return ids[:n]

def sample_ui_path_ids(ep,key,n:int)->List[str]:
    ids=_search_ids(ep,key, INDEX_DEFS['ui_paths']['index'], 'path_id,hop_count', 'path_id', top=min(max(n*3,50),SAMPLE_LIMIT))
    RAND.shuffle(ids)
    return ids[:n]

# ------------------ Question Generation ------------------

PURPOSE_TEMPLATES = [
    "What is the purpose of program {pid}?",
    "Explain what {pid} does",
    "Why does {pid} exist?"
]
FLOW_TEMPLATES = [
    "Describe the high-level flow of {pid}",
    "Show the execution sequence performed by {pid}",
    "How does the control flow progress inside {pid}?"
]
DEP_TEMPLATES = [
    "What does {pid} call?",
    "List outbound calls made by {pid}",
    "Which programs invoke {pid}?"
]
VAR_TEMPLATES = [
    "Explain usage of variable {vid}",
    "Show which programs write variable {vid}",
    "How is {vid} used?"
]
WHERE_USED_TEMPLATES = [
    "Where is {vid} first written?",
    "Where is {vid} updated?",
    "Where is {vid} referenced?"
]
UI_TEMPLATES = [
    "Describe the navigation path represented by path {path_id}",
    "Explain the screen sequence in path {path_id}",
    "Show UI path details for {path_id}"
]

# ------------------ Scoring ------------------

def score_record(category:str, rec:Dict[str,Any])->bool:
    ev=rec.get('evidence',[])
    indexes={e.get('index') for e in ev}
    if category=='purpose':
        return any('program_meta' in (idx or '') or 'copybook_meta' in (idx or '') for idx in indexes)
    if category=='flow':
        return any('program_flows' in (idx or '') or 'calls' in (idx or '') or 'program_deps' in (idx or '') for idx in indexes)
    if category=='dependency':
        return any('program_deps' in (idx or '') or 'calls' in (idx or '') for idx in indexes)
    if category=='variable':
        return any('variable_usage' in (idx or '') or 'symbol_refs' in (idx or '') for idx in indexes)
    if category=='where_used':
        return any(e.get('line') is not None and 'symbol_refs' in (e.get('index') or '') for e in ev)
    if category=='ui':
        return any('ui_paths' in (idx or '') or 'screen_nodes' in (idx or '') for idx in indexes)
    return False

# ------------------ Main ------------------

def main():
    ap=argparse.ArgumentParser(description='Random evaluation over orchestrate_answer')
    ap.add_argument('--programs',type=int,default=30,help='Number of program samples (each used across multiple categories)')
    ap.add_argument('--variables',type=int,default=30,help='Number of variable samples')
    ap.add_argument('--uipaths',type=int,default=15,help='Number of UI path samples')
    ap.add_argument('--out',required=True,help='Base output path (no extension)')
    ap.add_argument('--k',type=int,default=6)
    ap.add_argument('--vec-k',type=int,default=8)
    ap.add_argument('--kw-k',type=int,default=4)
    ap.add_argument('--limit',type=int,help='Optional hard cap on total questions')
    ap.add_argument('--seed',type=int,default=42)
    ap.add_argument('--no-diagram',action='store_true')
    ap.add_argument('--debug',action='store_true')
    args=ap.parse_args()
    RAND.seed(args.seed)

    load_settings(); ep,key=resolve()

    programs=sample_program_ids(ep,key,args.programs)
    variables=sample_variable_ids(ep,key,args.variables)
    uipaths=sample_ui_path_ids(ep,key,args.uipaths)

    questions=[]
    for pid in programs:
        questions.append(('purpose', random.choice(PURPOSE_TEMPLATES).format(pid=pid)))
        questions.append(('flow', random.choice(FLOW_TEMPLATES).format(pid=pid)))
        questions.append(('dependency', random.choice(DEP_TEMPLATES).format(pid=pid)))
    for vid in variables:
        questions.append(('variable', random.choice(VAR_TEMPLATES).format(vid=vid)))
        questions.append(('where_used', random.choice(WHERE_USED_TEMPLATES).format(vid=vid)))
    for path_id in uipaths:
        questions.append(('ui', random.choice(UI_TEMPLATES).format(path_id=path_id)))

    if args.limit:
        questions=questions[:args.limit]

    out_jsonl=args.out + '.jsonl'
    out_summary=args.out + '_summary.json'
    os.makedirs(os.path.dirname(out_jsonl) or '.', exist_ok=True)

    results=[]
    t_start=time.time()
    with open(out_jsonl,'w',encoding='utf-8') as f:
        for idx,(cat,q) in enumerate(questions, start=1):
            t0=time.time()
            try:
                ans=orchestrate(q,args.k,args.vec_k,args.kw_k,no_diagram=args.no_diagram,debug=args.debug)
                latency=time.time()-t0
                passed=score_record(cat, ans)
                rec={
                    'i':idx,
                    'category':cat,
                    'question':q,
                    'scope_inferred':ans.get('scope'),
                    'passed':passed,
                    'latency_sec':round(latency,4),
                    'evidence_count':len(ans.get('evidence',[])),
                    'indexes':[e.get('index') for e in ans.get('evidence',[])],
                    'where_used_mode':ans.get('meta',{}).get('where_used_mode'),
                }
            except SystemExit as e:
                rec={
                    'i':idx,
                    'category':cat,
                    'question':q,
                    'error':str(e),
                    'passed':False
                }
            results.append(rec)
            f.write(json.dumps(rec)+'\n')
            if args.debug and idx % 10 == 0:
                print(f"Processed {idx}/{len(questions)}")

    duration=time.time()-t_start
    # Aggregate stats
    agg={}
    for r in results:
        cat=r['category']
        agg.setdefault(cat, {'total':0,'pass':0})
        agg[cat]['total']+=1
        if r.get('passed'): agg[cat]['pass']+=1
    summary={'total_questions':len(results),'duration_sec':round(duration,2),'categories':{}}
    total_pass=sum(v['pass'] for v in agg.values())
    summary['overall_pass_pct']=round(100.0 * total_pass / len(results),2) if results else 0.0
    for cat,data in agg.items():
        pct=100.0 * data['pass']/data['total'] if data['total'] else 0.0
        summary['categories'][cat]={'total':data['total'],'pass':data['pass'],'pass_pct':round(pct,2)}

    open(out_summary,'w',encoding='utf-8').write(json.dumps(summary,indent=2))
    print(f"Wrote {out_jsonl} and {out_summary}")
    print(json.dumps(summary,indent=2))

if __name__=='__main__':
    main()
