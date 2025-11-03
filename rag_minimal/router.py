"""Lightweight router / planner for classifying developer questions.

Produces a Plan object with:
- intent: canonical intent label
- primary_entities: extracted entity tokens (e.g., symbol name, program id, table name, paragraph)
- filters: dict of high level filters (programId, table, paragraph etc.)
- sources: list of SourceSpec (index name + role + optional filter template)

Initial implementation is heuristic & regex based; cheap enough to run inline.
"""
from __future__ import annotations
import re
from dataclasses import dataclass, field
from typing import List, Dict, Optional

INTENT_DEFINITION_USAGE = "DEFINITION_USAGE"
INTENT_CALL_GRAPH = "CALL_GRAPH"
INTENT_PARAGRAPH_EXPLAIN = "PARAGRAPH_EXPLAIN"
INTENT_TABLE_IMPACT = "TABLE_IMPACT"
INTENT_COPYBOOK_USAGE = "COPYBOOK_USAGE"
INTENT_DATA_LINEAGE = "DATA_LINEAGE"
INTENT_GENERAL_NL = "GENERAL_NL"

INTENTS = [
    INTENT_DEFINITION_USAGE,
    INTENT_CALL_GRAPH,
    INTENT_PARAGRAPH_EXPLAIN,
    INTENT_TABLE_IMPACT,
    INTENT_COPYBOOK_USAGE,
    INTENT_DATA_LINEAGE,
    INTENT_GENERAL_NL,
]

@dataclass
class SourceSpec:
    name: str
    role: str  # primary | symbols | xrefs | calls | facts | flow | copybooks
    filter: Optional[str] = None  # simple OData filter expression if known

@dataclass
class Plan:
    intent: str
    question: str
    primary_entities: Dict[str, str] = field(default_factory=dict)
    filters: Dict[str, str] = field(default_factory=dict)
    sources: List[SourceSpec] = field(default_factory=list)
    notes: List[str] = field(default_factory=list)

_SYMBOL_NAME_RE = re.compile(r"[A-Z0-9][A-Z0-9_-]{1,30}")

PROGRAM_HINT_WORDS = {"program", "module", "routine"}
TABLE_HINT_WORDS = {"table", "file"}
PARAGRAPH_HINT_WORDS = {"paragraph", "section"}
COPYBOOK_HINT_WORD = "copybook"

# Basic heuristics for classification order
_PATTERNS = [
    (INTENT_DEFINITION_USAGE, re.compile(r"\b(what is|definition of)\b", re.I)),
    (INTENT_CALL_GRAPH, re.compile(r"\b(who calls|what calls|call graph|callees?)\b", re.I)),
    (INTENT_PARAGRAPH_EXPLAIN, re.compile(r"\b(explain paragraph|explain section|paragraph)\b", re.I)),
    (INTENT_TABLE_IMPACT, re.compile(r"\b(update|insert|modify)\b.*\btable\b", re.I)),
    (INTENT_COPYBOOK_USAGE, re.compile(r"\bcopy ?book\b", re.I)),
    (INTENT_DATA_LINEAGE, re.compile(r"\b(lineage|touch points|reads|writes)\b", re.I)),
]

def classify(question: str) -> Plan:
    q = question.strip()
    intent = INTENT_GENERAL_NL
    for cand, pat in _PATTERNS:
        if pat.search(q):
            intent = cand
            break
    plan = Plan(intent=intent, question=q)

    tokens = [t for t in re.split(r"[^A-Za-z0-9_-]+", q) if t]
    upper_tokens = [t for t in tokens if t.isupper() and len(t) > 1]

    # Attempt entity extraction:
    if intent == INTENT_DEFINITION_USAGE:
        # Choose likely symbol token (uppercase, appears after 'what is' or 'definition of')
        m = re.search(r"(?:what is|definition of)\s+([A-Z0-9_-]{2,})", q, re.I)
        if m:
            plan.primary_entities['symbol'] = m.group(1).upper()
    elif intent == INTENT_CALL_GRAPH:
        # Look for a program token after 'calls' patterns
        m = re.search(r"calls?\s+([A-Z0-9_-]{2,})", q, re.I)
        if m:
            plan.primary_entities['program'] = m.group(1).upper()
        else:
            # fallback: first long upper token
            if upper_tokens:
                plan.primary_entities['program'] = upper_tokens[0]
    elif intent == INTENT_PARAGRAPH_EXPLAIN:
        # Example: Explain paragraph READ INPUT in ACCT001
        m = re.search(r"paragraph\s+([A-Z0-9_-]{2,})", q, re.I)
        if m:
            plan.primary_entities['paragraph'] = m.group(1).upper()
        m2 = re.search(r"in\s+([A-Z0-9_-]{2,})", q, re.I)
        if m2:
            plan.primary_entities['program'] = m2.group(1).upper()
    elif intent == INTENT_TABLE_IMPACT:
        # Find table token (uppercase after table or direct uppercase if unique)
        m = re.search(r"table\s+([A-Z0-9_-]{2,})", q, re.I)
        if m:
            plan.primary_entities['table'] = m.group(1).upper()
        elif upper_tokens:
            plan.primary_entities['table'] = upper_tokens[0]
    elif intent == INTENT_COPYBOOK_USAGE:
        m = re.search(r"copy ?book\s+([A-Z0-9_-]{2,})", q, re.I)
        if m:
            plan.primary_entities['copybook'] = m.group(1).upper()
        elif upper_tokens:
            plan.primary_entities['copybook'] = upper_tokens[0]
    elif intent == INTENT_DATA_LINEAGE:
        # Reuse table/file extraction
        m = re.search(r"(table|file)\s+([A-Z0-9_-]{2,})", q, re.I)
        if m:
            plan.primary_entities['entity'] = m.group(2).upper()
        elif upper_tokens:
            plan.primary_entities['entity'] = upper_tokens[0]

    # Derive filters based on entities
    if 'program' in plan.primary_entities:
        plan.filters['program_id'] = plan.primary_entities['program']
    if 'table' in plan.primary_entities:
        plan.filters['tableOrFile'] = plan.primary_entities['table']
    if 'entity' in plan.primary_entities:
        plan.filters['tableOrFile'] = plan.primary_entities['entity']

    # Choose sources (initial minimal mapping)
    if intent == INTENT_DEFINITION_USAGE:
        plan.sources = [
            SourceSpec('cobol-symbols','symbols'),
            SourceSpec('cobol-xrefs','xrefs'),
            SourceSpec('cobol-file-chunks-v1','primary'),
        ]
    elif intent == INTENT_CALL_GRAPH:
        plan.sources = [
            SourceSpec('cobol-calls','calls'),
            SourceSpec('cobol-file-chunks-v1','primary'),
            SourceSpec('cobol-flow-edges-v2','flow'),
        ]
    elif intent == INTENT_PARAGRAPH_EXPLAIN:
        plan.sources = [
            SourceSpec('cobol-paragraphs-v3','primary'),
            SourceSpec('cobol-symbols','symbols'),
            SourceSpec('cobol-facts-v3l','facts'),
        ]
    elif intent == INTENT_TABLE_IMPACT:
        plan.sources = [
            SourceSpec('cobol-facts-v3l','facts'),
            SourceSpec('cobol-file-chunks-v1','primary'),
        ]
    elif intent == INTENT_COPYBOOK_USAGE:
        plan.sources = [
            SourceSpec('cobol-copybooks-v2','copybooks'),
            SourceSpec('cobol-xrefs','xrefs'),
            SourceSpec('cobol-file-chunks-v1','primary'),
        ]
    elif intent == INTENT_DATA_LINEAGE:
        plan.sources = [
            SourceSpec('cobol-facts-v3l','facts'),
            SourceSpec('cobol-file-chunks-v1','primary'),
            SourceSpec('cobol-calls','calls'),
        ]
    else:  # GENERAL_NL
        plan.sources = [
            SourceSpec('cobol-file-chunks-v1','primary'),
            SourceSpec('cobol-paragraphs-v3','primary'),
            SourceSpec('cobol-facts-v3l','facts'),
        ]

    return plan

if __name__ == "__main__":
    tests = [
        "What is DATA X and where is it used",
        "Who calls LOANPROC and what does it call",
        "Explain paragraph READ INPUT in ACCT001",
        "Which programs update table CUSTOMER",
        "Show data lineage for table CUSTOMER",
        "Where is copybook XYZ included",
        "How does the loan payment process validate accounts",
    ]
    for t in tests:
        p = classify(t)
        print(t, "->", p.intent, p.primary_entities, [s.name for s in p.sources])
