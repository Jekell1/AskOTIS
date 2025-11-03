"""Join expansion helpers.

Currently placeholder / scaffolding: stubs show where symbol/xref/call/fact lookups would occur.
Real implementation will query additional indexes via Azure Search just-in-time.
"""
from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Dict, Any

@dataclass
class ExpandedContext:
    base_passages: List[Dict[str, Any]]
    symbols: List[Dict[str, Any]] = field(default_factory=list)
    xrefs: List[Dict[str, Any]] = field(default_factory=list)
    calls: List[Dict[str, Any]] = field(default_factory=list)
    facts: List[Dict[str, Any]] = field(default_factory=list)
    flow_edges: List[Dict[str, Any]] = field(default_factory=list)
    copybooks: List[Dict[str, Any]] = field(default_factory=list)
    notes: List[str] = field(default_factory=list)

# Future: generic Azure Search simple query helper; for now, return empty lists.

def expand(plan_intent: str, passages: List[Any], entities: Dict[str,str]) -> ExpandedContext:
    # Convert passages to dicts (they may be dataclass objects from retriever)
    base = []
    for p in passages:
        if hasattr(p, '__dict__'):
            d = dict(p.__dict__)
        elif isinstance(p, dict):
            d = dict(p)
        else:
            d = {'raw': str(p)}
        base.append(d)
    ctx = ExpandedContext(base_passages=base)

    # Minimal heuristic expansions (stubs)
    if plan_intent == 'DEFINITION_USAGE' and 'symbol' in entities:
        ctx.notes.append('Symbol definition/xrefs lookup not yet implemented.')
    elif plan_intent == 'CALL_GRAPH' and 'program' in entities:
        ctx.notes.append('Call graph expansion pending (calls + flow).')
    elif plan_intent == 'PARAGRAPH_EXPLAIN' and 'paragraph' in entities:
        ctx.notes.append('Paragraph symbol/fact enrichment pending.')
    elif plan_intent == 'TABLE_IMPACT' and 'table' in entities:
        ctx.notes.append('Table impact facts enrichment pending.')
    elif plan_intent == 'COPYBOOK_USAGE' and 'copybook' in entities:
        ctx.notes.append('Copybook usage expansion pending.')
    elif plan_intent == 'DATA_LINEAGE' and 'entity' in entities:
        ctx.notes.append('Data lineage expansion pending.')
    return ctx
