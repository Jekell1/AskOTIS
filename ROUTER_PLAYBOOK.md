# Router Playbook (Intent -> Retrieval Plan)

This document defines the lightweight planner ("router") that classifies an incoming developer question and selects indexes, filters, and enrichment joins for the COBOL RAG system.

## Core Philosophy
1. **Classify once, keep it cheap.** Intents are mutually exclusive coarse buckets.
2. **Always run hybrid search** (BM25 + vector) on the primary content index(es) chosen for the intent.
3. **Fuse results with Reciprocal Rank Fusion (RRF)**; then optional semantic ranker (future) and light cross-encoder rerank only for difficult identifier queries.
4. **Enrich context only as needed**: join structural lookups (symbols, xrefs, calls, flow edges, facts, copybooks) around matched lines / paragraphs to build answer snippets.
5. **Never hallucinate code**. If a symbol / program not found, return a refusal plus targeted next-step queries.

## Index Roles
| Role | Index (example) | Notes |
|------|-----------------|------|
| Primary content | `cobol-file-chunks-v1`, `cobol-facts-v3l`, `cobol-paragraphs-v3` | Chunks & paragraphs hold text bodies with vectors. Facts hold extracted semantic facts (IO, SQL). |
| Symbols | `cobol-symbols` | Definitions of data names, variables, paragraphs (if represented), types. |
| XRefs | `cobol-xrefs` | Usage sites (name, file, line, program, paragraph). |
| Calls | `cobol-calls` | Inter-program call relationships (callerProgram, calleeProgram). |
| Flow edges | `cobol-flow-edges-v2` | Intra-program paragraph transitions. |
| Copybooks | `cobol-copybooks-v2` | Copybook raw content. |
| Facts | `cobol-facts-v3l` | SQL/file IO/data lineage facts (kind, subtype, tableOrFile, verb). |
| Generated docs (future) | `cobol-program-portraits` | Cached one-page program summaries for fast follow-up. |

## Minimal Field Set (Join Efficiency)
Content-style indexes: `id, content, repoPath, programId, paragraph, lineStart, lineEnd, commit, updatedAt, contentVector`
Symbols: `name, kind, definedInFile, definedAtLine, scope, programId`
XRefs: `name, file, line, programId, paragraph`
Calls: `callerProgram, calleeProgram, file, line, paragraph`
Flow edges: `programId, fromParagraph, toParagraph, file, line`
Facts: `kind, subtype, tableOrFile, verb, file, line, programId, paragraph`

## Intents & Retrieval Recipes
Below are canonical intent mappings.

### Intent: DEFINITION_USAGE ("What is DATA X and where is it used")
Steps:
1. Symbols: filter `name == <X>` (definition). If none -> refusal.
2. XRefs: filter `name == <X>`; order by frequency or group by program.
3. Extract top N usage lines; map each to its containing chunk (file-chunk / paragraph) for context.
4. Hybrid search optional if term is ambiguous to surface surrounding descriptive chunks.
Answer Sections: definition text, usage locations (file:line), top 3-5 contextual snippets, next-step links (callers/callees if relevant).

### Intent: CALL_GRAPH ("Who calls LOANPROC and what does it call")
1. Calls index: `calleeProgram == LOANPROC` (collect callers)
2. Calls index: `callerProgram == LOANPROC` (collect callees)
3. For each caller & callee site, fetch surrounding chunk / paragraph.
4. (Optional) Flow edges inside LOANPROC to hint internal structure ordering.
Answer: two lists + tiny ASCII call tree + representative snippets.

### Intent: PARAGRAPH_EXPLAIN ("Explain paragraph READ INPUT in ACCT001")
1. Paragraphs: filter `programId == ACCT001 AND paragraph == READ INPUT` -> body.
2. Symbols: names referenced in the paragraph range (join by program + line range or stored paragraph key).
3. Facts: any IO / SQL lines within range.
4. Optional hybrid search over file chunks to pull commentary-like contexts.
Answer: short explanation, paragraph text (verbatim lines), table of symbols/definitions, detected IO/SQL.

### Intent: TABLE_IMPACT ("Which programs update table CUSTOMER")
1. Facts: filter `kind == 'SQL' AND verb IN (UPDATE, INSERT) AND tableOrFile == CUSTOMER`.
2. Group by program; for each program gather lines -> map to chunks for context.
3. Optional related reads (SELECT) for completeness.
Answer: programs + counts + exact line refs + top contextual snippets.

### Intent: COPYBOOK_USAGE ("Where is COPYBOOK XYZ included" / "Explain copybook XYZ")
1. Copybooks: fetch copybook content.
2. XRefs (or facts depending on ingestion) for inclusion sites (lines with COPY statement referencing name).
3. Map each inclusion line to a chunk snippet.
Answer: summary of copybook purpose (if derivable), raw content excerpt, inclusion sites.

### Intent: GENERAL_NL (Default fallback for broad questions)
1. Hybrid search across primary content indexes (chunks + paragraphs + maybe facts as text facets).
2. Expand with top matching symbols (exact name matches from query tokens).
3. (Future) Semantic ranker rerank.
Answer: synthesized explanation with citations.

### Intent: DATA_LINEAGE ("Show data/file touch points for FILE X" / table inquiries not strictly updates)
1. Facts: filter `tableOrFile == X` (any verb) to list operations.
2. Join calls / flow edges optionally to show propagation.
3. Map lines to context chunks.
Answer: operations grouped by verb and program + lineage bullet list.

## Classification Heuristics (Cheap Regex / Keyword)
Order matters; first match wins.
- If pattern: `what is|definition of` + token => DEFINITION_USAGE
- If pattern: `who calls|what calls|callees|call graph` => CALL_GRAPH
- If pattern: `explain paragraph|explain section|paragraph` + program id token => PARAGRAPH_EXPLAIN
- If pattern: `update|insert|modify` + `table` => TABLE_IMPACT
- If pattern: `copybook` or `copy book` => COPYBOOK_USAGE
- If pattern: `lineage|touch points|reads|writes` => DATA_LINEAGE
- Else => GENERAL_NL

## Hybrid Search Procedure (Always)
1. Embed question (vector).
2. BM25 query over selected content index(es).
3. Vector KNN over same docs.
4. RRF fuse (k=60) -> top N.
5. (Planned) Semantic ranker call on fused list if available.
6. (Optional) Cross-encoder rerank for ambiguous identifier intents.

## Expansion / Joins
`expand_with_joins` takes fused hits + plan intent; executes only needed lookups:
- Symbols for names present in query or inside fused context.
- XRefs for targeted name (definition usage) or copybook includes.
- Calls for CALL_GRAPH intent.
- Facts for TABLE_IMPACT / DATA_LINEAGE.
- Flow edges for CALL_GRAPH (internal shape) or PARAGRAPH_EXPLAIN.

## Answer Composition Skeleton
- Summary sentence (<= 200 chars).
- Structured JSON body:
  - `snippets`: list of `{tag, file, lineStart, lineEnd, text}`
  - `links`: `{definitions:[], callers:[], callees:[], copybookIncludes:[], relatedFacts:[]}`
  - `graph`: optional ASCII call tree or lineage list.
  - `notice`: refusal or limitation messages.
Always include file + line ranges for every snippet.

## Guardrails
- If primary entity missing (symbol, program, table, copybook), return refusal: "Entity <X> not found in indexes" + suggestion queries.
- Do not fabricate definitions or code lines.
- Limit snippet text length; truncate with ellipsis if > ~600 chars.

## Caching (Future)
Program portraits index built offline with: definition summary, main paragraphs, call graph summary, top data touch points.

## Metrics (Future)
Maintain gold set; track recall@k for each intent and answer exactness precision.

## Pseudocode
```
plan = classify(user_question)
sources = choose_indexes(plan.intent)
results = []
for s in sources:
    hits = hybrid_search(index=s.name, text=user_question, filter=s.filter_from(plan), vector=embed(user_question), k=50)
    results.extend(hits)
fused = reciprocal_rank_fuse(results, top_n=20)
context = expand_with_joins(fused, join_specs_for(plan.intent))
answer = compose_answer(user_question, context)
return answer
```

## Next Implementation Steps
1. `router.py` with `classify()` + dataclasses for Plan, SourceSpec.
2. `fusion.py` with RRF util.
3. `expander.py` for join enrichment.
4. Integrate `--auto-plan` into `ask_question.py`.
5. Add answer composer skeleton returning structured JSON.

---
This playbook guides the next set of incremental additions without overhauling existing minimal RAG flow.
