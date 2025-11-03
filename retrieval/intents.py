"""Intent classification module for question routing.

Defines high-level intents used by orchestrator:
  PURPOSE | DEPS | FLOW | VARIABLE | COPYBOOK | UI | MIXED

Exports:
  classify_intent(question: str) -> str
  Intent constants for reuse.

Heuristics balance explicit keywords, structural tokens, and tie-breaking precedence.
"""
from __future__ import annotations
import re
from typing import Dict

PURPOSE='PURPOSE'
DEPS='DEPS'
FLOW='FLOW'
VARIABLE='VARIABLE'
COPYBOOK='COPYBOOK'
UI='UI'
MIXED='MIXED'

# Keyword buckets
_DEPS_PAT=re.compile(r'\b(call|calls|invok|dependency|depends|callee|caller)\b', re.I)
_FLOW_PAT=re.compile(r'\b(flow|perform|sequence|step|path|execution order|control flow)\b', re.I)
_VAR_PAT=re.compile(r'\b(variable|field|data item|working-storage|first write|where used|set by)\b', re.I)
_COPYBOOK_PAT=re.compile(r'\bcopybook|copy book|included from|COPY\b', re.I)
_UI_PAT=re.compile(r'\b(screen|menu|panel|page|navigation|ui path|guard|hop count|placeholder)\b', re.I)
_PURPOSE_PAT=re.compile(r'\b(purpose|what does|why |responsib|role of|function of)\b', re.I)

def classify_intent(q: str) -> str:
    ql=q.lower()
    if _PURPOSE_PAT.search(ql):
        return PURPOSE
    hits={
        DEPS: bool(_DEPS_PAT.search(ql)),
        FLOW: bool(_FLOW_PAT.search(ql)),
        VARIABLE: bool(_VAR_PAT.search(ql)),
        COPYBOOK: bool(_COPYBOOK_PAT.search(ql)),
        UI: bool(_UI_PAT.search(ql)),
    }
    active=[k for k,v in hits.items() if v]
    if not active:
        return PURPOSE if q.strip().endswith('?') else MIXED
    if len(active)==1:
        return active[0]
    # precedence: explicit UI overrides others if present with >=1 more
    if UI in active:
        return UI
    # flow vs deps: if both, check for additional verbs count
    if FLOW in active and DEPS in active:
        # count frequency
        flow_ct=len(_FLOW_PAT.findall(ql))
        deps_ct=len(_DEPS_PAT.findall(ql))
        return FLOW if flow_ct>=deps_ct else DEPS
    return MIXED

if __name__=='__main__':
    tests=['Explain how APIPAY works','What does LONPF2 call?','Where is BT-BRANCH first written?','Show UI navigation from MAIN MENU','Copybook purpose of SCREEN.cpy']
    for t in tests:
        print(t,'=>', classify_intent(t))
