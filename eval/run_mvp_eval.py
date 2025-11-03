"""MVP evaluation harness.

Runs a fixed suite of COBOL domain questions through the orchestrator and
applies lightweight pass/fail heuristics:

Base requirement (all questions):
  - >= 2 evidence items

Additional conditional requirements:
  - Flow / screen flow questions (contain 'flow', 'user interface screens', 'screen flow'):
      Must include a diagram (non-empty)
  - Where/used questions (contain 'where', 'used', 'instigated', 'how is'):
      Must include at least one evidence item with a line number sourced from symbol refs
      (heuristic: evidence item with line != None and index == 'new_cobol_symbol_refs' OR id looks like ref)

Outputs:
  - JSON per question written to eval/out/<slug>.json
  - Summary table (plain text) with PASS/FAIL + reason(s)

Usage:
  python eval/run_mvp_eval.py            # run full 18-question suite
  python eval/run_mvp_eval.py --subset 3 # quick smoke test of first 3 questions

Exit code: 0 always (does not fail CI) – adjust if you want strict gating.
"""
from __future__ import annotations
import os, sys, json, re, argparse, traceback, time
from dataclasses import dataclass
from typing import List, Dict, Any

# Ensure project root import path for retrieval.orchestrate_answer
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

try:
    from retrieval.orchestrate_answer import orchestrate  # type: ignore
except Exception as e:  # Fallback to subprocess invocation later
    orchestrate = None  # type: ignore

QUESTIONS = [
    "Explain how APIPAY.CBL works",
    "What is the purpose of LONPF2?",
    "Where is the variable BT-BRANCH instigated?",
    "How is BT-BRANCH used?",
    "What external programs does APIPAY call?",
    "What dependencies does LONPF2 have?",
    "Where is TIM360 used?",
    "What does TIM360 do?",
    "Show me a flow of DAILY.CBL",
    "What CopyBooks are used in DAILY.CBL",
    "What programs use SCREEN.CPY",
    "Tell me about the variable DS-BRANCH. Where is it initially defined and what is it used for?",
    "Show me a flow of user interface screens starting from the main program.",
    "What is IRMAIN.CBL used for?",
    "What does the full set of code do?",
    "Show me a flow for LNQUOT.CBL",
    "How is a loan payment received?",
    "Show me a complete user screen flow from the main menu down to the bottom of the tree."
]

@dataclass
class EvalResult:
    question: str
    slug: str
    passed: bool
    reasons: List[str]
    scope: str | None = None
    diagram: bool = False
    evidence_count: int = 0

SLUG_CLEAN_RE = re.compile(r'[^a-z0-9]+')

def slugify(q: str) -> str:
    s = q.lower().strip()
    s = SLUG_CLEAN_RE.sub('-', s).strip('-')
    return s[:70]

def classify_flow(q: str) -> bool:
    return bool(re.search(r'flow|user interface screens|screen flow', q, re.I))

def classify_where_used(q: str) -> bool:
    return bool(re.search(r'\bwhere\b|used|instigated|how is', q, re.I))

def has_symbol_ref_line(evidence: List[Dict[str,Any]]) -> bool:
    for ev in evidence:
        if ev.get('line') is not None:
            idx = ev.get('index','')
            if idx == 'new_cobol_symbol_refs' or ev.get('id','').startswith('REF') or 'symbol' in idx or idx=='new_cobol_copybook_usage':
                return True
    return False

def run_question(q: str, out_dir: str) -> Dict[str, Any]:
    if orchestrate is None:
        # Fallback: invoke subprocess to call orchestrate_answer CLI
        import subprocess, json as _json, tempfile
        tmp = os.path.join(out_dir, '__tmp_orch.json')
        cmd = [sys.executable, os.path.join(ROOT,'retrieval','orchestrate_answer.py'), '--q', q, '--out', tmp]
        try:
            subprocess.run(cmd, check=True, capture_output=True, text=True)
            return _json.load(open(tmp,'r',encoding='utf-8'))
        except Exception as e:
            raise RuntimeError(f"Subprocess orchestrate failure: {e}")
    # Direct call
    return orchestrate(q)

def evaluate(answer: Dict[str,Any], q: str) -> EvalResult:
    slug = slugify(q)
    evidence = answer.get('evidence', [])
    scope = answer.get('scope')
    need_flow = classify_flow(q)
    need_where = classify_where_used(q)
    reasons: List[str] = []

    # Base requirement
    if len(evidence) < 2:
        reasons.append(f"insufficient evidence ({len(evidence)})")

    # Flow requirement
    if need_flow:
        dia = bool(answer.get('diagram'))
        if not dia:
            reasons.append('missing diagram for flow question')
    else:
        dia = bool(answer.get('diagram'))

    # Where/used requirement
    if need_where:
        if not has_symbol_ref_line(evidence):
            reasons.append('no line-level symbol_ref evidence')

    passed = not reasons
    return EvalResult(question=q, slug=slug, passed=passed, reasons=reasons, scope=scope, diagram=dia, evidence_count=len(evidence))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--subset', type=int, help='Only run first N questions (for quick smoke)')
    ap.add_argument('--out-dir', default=os.path.join('eval','out'))
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    selected = QUESTIONS[:args.subset] if args.subset else QUESTIONS
    results: List[EvalResult] = []

    for i,q in enumerate(selected, start=1):
        slug = slugify(q)
        path = os.path.join(args.out_dir, f"{slug}.json")
        print(f"[{i}/{len(selected)}] {q}")
        start = time.time()
        try:
            answer = run_question(q, args.out_dir)
            # Persist answer
            with open(path,'w',encoding='utf-8') as f:
                json.dump(answer, f, indent=2)
            res = evaluate(answer, q)
        except Exception as e:
            traceback.print_exc()
            res = EvalResult(question=q, slug=slug, passed=False, reasons=[f"error: {e}"], scope=None, diagram=False, evidence_count=0)
        end = time.time()
        status = 'PASS' if res.passed else 'FAIL'
        print(f"  -> {status} ({end-start:.1f}s) evidence={res.evidence_count} diagram={'Y' if res.diagram else 'N'}")
        if res.reasons:
            for r in res.reasons:
                print(f"     reason: {r}")
        results.append(res)

    # Summary table
    print("\nSummary:")
    header = f"{'#':<3} {'Slug':<35} {'Scope':<10} {'Ev':<3} {'Diag':<4} Result Reasons"
    print(header)
    print('-'*len(header))
    for idx,res in enumerate(results, start=1):
        reasons = '; '.join(res.reasons) if res.reasons else ''
        print(f"{idx:<3} {res.slug[:35]:<35} {str(res.scope)[:10]:<10} {res.evidence_count:<3} {'Y' if res.diagram else 'N':<4} {'PASS' if res.passed else 'FAIL':<6} {reasons}")

    # Write machine-readable summary
    summary_path = os.path.join(args.out_dir, '_summary.json')
    with open(summary_path,'w',encoding='utf-8') as f:
        json.dump([res.__dict__ for res in results], f, indent=2)
    print(f"\nWrote summary JSON: {summary_path}")

if __name__ == '__main__':
    main()
