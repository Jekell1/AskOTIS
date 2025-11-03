#!/usr/bin/env python3
"""Chatbot test harness.

Takes a routine-style user query, performs retrieval via retrieval_helpers, and prints:
  - Candidate paragraphs
  - Flow edge stats (resolved/unresolved, kinds)
  - Top callers (by frequency in edges)
  - Naive synthesized answer (heuristic, no LLM)

Usage:
  python chatbot_test.py TIM360
  python chatbot_test.py "MAIN-PROGRAM-EXIT"
"""
from __future__ import annotations
import argparse, collections
from retrieval_helpers import retrieve_routine_context


def synthesize(bundle: dict) -> str:
    flow = bundle['flow_edges']
    paras = bundle['paragraphs']
    calls = bundle['calls']
    alias_doc = bundle.get('alias_doc')
    resolved = sum(1 for e in flow if e.get('resolved'))
    kinds = collections.Counter(e.get('kind') for e in flow if e.get('kind'))
    callers = collections.Counter(e.get('caller_para') for e in flow if e.get('caller_para'))

    top_callers = ', '.join(f"{k}({v})" for k,v in callers.most_common(8)) or 'none'
    kinds_str = ', '.join(f"{k}:{v}" for k,v in kinds.most_common()) or 'none'

    alias_bits = []
    if alias_doc:
        alias_bits.append(f"family={alias_doc.get('family_key')}")
        alias_bits.append(f"confidence={alias_doc.get('confidence')}")
        if alias_doc.get('canonical_target'):
            alias_bits.append(f"canonical={alias_doc['canonical_target']}")
    alias_meta = ' | '.join(alias_bits) if alias_bits else 'no-alias'

    para_names = {p.get('name') for p in paras if p.get('name')}
    summary = (
        f"Query '{bundle['query']}' normalized '{bundle['normalized']}'. "
        f"Alias: {alias_meta}. Candidate paragraphs={len(bundle['candidate_paragraphs'])}; "
        f"Paragraph defs retrieved={len(para_names)}; Flow edges={len(flow)} (resolved={resolved}); "
        f"Kinds: {kinds_str}. Top callers: {top_callers}."
    )

    # Simple heuristic guidance statement
    guidance = "This routine appears mostly unresolved in flow edges; additional program-level resolution may be needed." if resolved < 3 else "Routine has some resolved paragraph linkages; you can trace callers above."

    return summary + "\n" + guidance


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('query')
    ap.add_argument('--max-flow', type=int, default=200)
    args = ap.parse_args()
    bundle = retrieve_routine_context(args.query, max_flow=args.max_flow)
    ans = synthesize(bundle)
    print(ans)
    print("\nCandidate Paragraphs:")
    for c in bundle['candidate_paragraphs'][:40]:
        print("  -", c)
    print("\nSample Flow Edges (first 15):")
    for e in bundle['flow_edges'][:15]:
        print(f"  {e.get('caller_para')} -> {e.get('raw_target')} | kind={e.get('kind')} resolved={e.get('resolved')}")

if __name__ == '__main__':
    main()
