# COBOL RAG Base Indexes Overview

This document summarizes the current base Azure AI Search indexes that power the COBOL Retrieval-Augmented Generation pipeline. All index names use the `new_` prefix for clarity and version isolation.

## Inventory Snapshot

| Index | Docs | Vector Field | Dim | Key | Purpose |
|-------|------|--------------|-----|-----|---------|
| new-cobol-files | (varies) | `content_vector` (if added later) | (planned) | `file_id` | Raw full-source documents (primary source of truth). |
| new_cobol_data_items | 267,769 | `vector` | 3072 | `item_id` | Hierarchical Data Division items (levels 01–49) for field structure and storage questions. |
| new_cobol_paragraphs | 224,655 | `para_vector` | 3072 | `para_id` | Paragraph-level logic retrieval enabling fine-grained procedural explanations. |
| new_cobol_flow_edges | 368,454 | `edge_vector` | 3072 | `edge_id` | Intra-program flow transitions for path/trace reasoning. |
| new_cobol_calls | 7,520 | `snippet_vector` | 3072 | `call_id` | Inter-program call relationships and call-site context. |
| new_cobol_program_meta | 9,541 | (none) | — | `program_id` | Program-level classification, coverage, aggregation hubs. |
| new_cobol_copybook_meta | 8,213 | `summary_vector` | 3072 | `copybook_id` | Structural summaries of copybooks (.cpy) for schema exploration and grouping. |
| new_cobol_copybook_usage | (pending) | `context_vector` | 3072 | `usage_id` | Each COPY occurrence linking copybooks to programs & context for dependency reasoning. |
| new_cobol_menu_trees | (pending) | (none) | — | (none) | Hierarchical navigation trees for top-level UI/dispatch/menu roots. |
| new_cobol_program_deps | 1,572 | `dependency_blob_vector` | 3072 | `program_id` | One-shot dependency resolution and impact analysis (100% vector coverage). |
| new_cobol_variable_usage | 10,735 | `usage_summary_vector` | 3072 | `variable_id` | Aggregated per-variable read/write & program participation summary. |
| new_cobol_symbol_refs | 63,411 | `excerpt_vector` | 1536 | `ref_id` | Fine-grained symbol reference occurrences (READ / WRITE / PARAM_*). |
| new_cobol_program_flows | (dynamic) | `flow_vector` | 3072 | `program_id` | Per-program paragraph/PERFORM control-flow graph with Mermaid + analytics. |
| new_cobol_screen_nodes | (dynamic) | `summary_vector` | 3072 | `screen_id` | Concrete SCREEN SECTION screen artifacts (fields, actions, transitions). |
| new_cobol_name_aliases | (dynamic) | (none) | — | `alias_id` | Normalized / variant name expansion for recall (programs, copybooks, paragraphs). |

## Added: new_cobol_variable_usage
Key: `variable_id`
Purpose: Provide a collapsed, aggregated view of each variable / data item name across the codebase to support semantic and lexical queries like "where is CUSTOMER-ID heavily written" or "variables related to billing totals".
Primary Fields (base + enrichment):
- `variable_id` (uppercase canonical form)
- `read_count`, `write_count`, `param_in_count`, `param_out_count`, `total_refs`
- `program_readers`, `program_writers`, `program_params_in`, `program_params_out`
- `sample_refs_json` (representative refs)
- `symbol_id_global` (stable global hash for joins across variable usage & symbol refs)
- `usage_summary_vector` (3072 dims) embedding generated from constructed summary
- `has_vector` boolean flag post embedding
- Enrichment fields (from symbol ref sweep via `enrich_variable_usage_refs.py`):
  - `usage_role` (READ_DOMINANT | WRITE_DOMINANT | BALANCED | PARAM_HEAVY)
  - `first_ref_program_id`, `first_ref_line_number`
  - `last_ref_program_id`, `last_ref_line_number`
  - `first_write_location` (program:line if any WRITE occurs)

Creation vs Enrichment:
1. Base index (aggregation) created without vector field; vector field & `has_vector` added by `add_vector_field_variable_usage.py`.
2. Embeddings populated by `backfill_embeddings_variable_usage.py` (supports `--resume-missing`).
3. Reference-derived positional fields populated by `enrich_variable_usage_refs.py` after symbol refs ingestion.

Embedding Backfill:
```
python add_vector_field_variable_usage.py
python backfill_embeddings_variable_usage.py --batch 64
```

Retrieval Patterns:
1. Lexical: `search="CUSTOMER-ID"` to grab summary and top interacting programs.
2. Semantic: Vector search with paraphrased descriptions (e.g., "fields counting total backorders") after embedding backfill.
3. Cross-index join: Use `symbol_id_global` to pivot into `new_cobol_symbol_refs` for precise occurrences and into `new_cobol_data_items` for structural definitions.

Planned Enhancements:
* Add boolean flag `has_vector` and set post-backfill for faster completeness checks.
* Introduce per-program variable expansion (scoped variables) enabling `symbol_id` (scoped) differentiation.
* Derive role weighting (read-dominant vs write-dominant) for ranking relevance when answering "which variables are modified here".

## Added / Enriched: new_cobol_symbol_refs
Key: `ref_id`
Purpose: High-fidelity, line-level reference occurrences for data items / variables enabling precise answer grounding, frequency analytics, and context snippet retrieval.
Primary Fields (latest schema):
- Identity & linkage: `ref_id`, `program_id`, `symbol_name`, `symbol_id`, `symbol_id_global`, `cluster_key`
- Classification: `kind` (legacy), `op` (raw operation token), `normalized_kind` (final normalized category)
- Location: `file_path`, `line_number`, `paragraph_name`, `first_in_program` (bool)
- Context & content: `excerpt`, `context_before`, `context_after`
- Vectorization: `excerpt_vector` (1536 or configured dim), `has_vector`
- Audit: `ingested_at`

Vector Enrichment:
`backfill_symbol_ref_embeddings.py` populates `excerpt_vector` for refs lacking vectors (`has_vector=false`).

Source Extraction:
```
python create_symbol_refs_index.py --force   # optional rebuild
python extract_symbol_refs_from_xrefs.py --batch 1000  # full run (respects $skip < 100k)
```

Join Usage:
1. From a variable usage doc → `symbol_id_global` → all refs for patterns / density.
2. From a data item name → search refs (`symbol_name` equality) to illustrate concrete usage lines.
3. Potential future linking to paragraphs by intersecting line ranges if paragraph span metadata is present.

Historic Note: Earlier version lacked context lines & vector field; now upgraded for semantic clustering and richer grounding.

## Variable Usage Vector Similarity Probe (New Utility Planned)
A helper script (proposed: `probe_variable_usage_vectors.py`) can perform an on-the-fly embedding of a natural language query and run a vector similarity search against `usage_summary_vector` to surface relevant variables. Planned shape:
```
python probe_variable_usage_vectors.py --query "late fee accumulation counters" --top 15
```
Would return: variable_id, read/write counts, top writer programs.

## Updated Cross-Index Join Flow (Variable → Refs → Structure)
```
variable_id ─(variable usage aggregate: readers/writers + vector)─┐
  │                                                            │
  ├─ symbol_id_global → new_cobol_symbol_refs (line occurrences)│
  └─ item_name == variable_id → new_cobol_data_items (PIC/USAGE)│
                                           ▼
           Combined answer: structural definition + usage density + exemplar code lines
```

This triad supports high-quality answers to: *"Show me what TOTAL-BW-COUNT is, where it’s defined, and how it’s used."*

## Embedding & Maintenance Command Reference (New)

Unified cheat sheet for (re)building vector-enabled indexes, regenerating embeddings, and validating integrity.

### Variable Usage (`new_cobol_variable_usage`)
Add / verify vector field + boolean flag:
```
python add_vector_field_variable_usage.py
```
Backfill (fresh full run):
```
python backfill_embeddings_variable_usage.py --batch 128
```
Resume only missing embeddings (uses `has_vector` filter):
```
python backfill_embeddings_variable_usage.py --batch 128 --resume-missing
```
Coverage report:
```
python variable_usage_embedding_coverage.py
```
Vector probe (semantic):
```
python probe_variable_usage_vectors.py --query "late fee accumulation counters" --top 15 --lexical-fallback
```

### Program Dependencies (`new_cobol_program_deps`)
Ensure vector field present:
```
python add_vector_fields_program_deps.py
```
Embedding backfill:
```
python backfill_embeddings_program_deps.py --batch 64
```

### Symbol References (`new_cobol_symbol_refs`)
Embedding backfill (excerpt vectors – incremental top-off):
```
python backfill_symbol_ref_embeddings.py --batch 128 --resume-missing
```

Force re-embed (all refs) – supervised multi-shard run with robustness & post-run verification:
```
python ops/supervise_symbol_ref_backfill.py \
  --hex-shards 16 \
  --force \
  --windowed-scan-chars \
  --track-ids \
  --auto-keyset-after-skip 60000 \
  --no-progress-exit-batches 12 \
  --run-diagnose \
  --diagnose-script diagnose_symbol_ref_gaps.py
```

Manual single-process full force (slower, for smaller corpora or debugging):
```
python backfill_symbol_ref_embeddings.py --force --windowed-scan-chars --track-ids --auto-keyset-after-skip 60000
```

Coverage diagnostic (stand‑alone, authoritative completeness check):
```
python diagnose_symbol_ref_gaps.py
```

Flag glossary (symbol ref embedding):
* `--resume-missing` – Only embed docs where `has_vector=false` (fast incremental top-off).
* `--force` – Re-embed every document (ignores `has_vector`). Use with safeguards below.
* `--windowed-scan-chars` – Partition scan by first hex char of `ref_id` (16 deterministic windows) eliminating deep `$skip` accumulation while smoothing refresh races.
* `--track-ids` – Maintain a distinct in-memory set of seen `ref_id`s; guarantees termination in force mode (no reliance on shrinking batch heuristic).
* `--auto-keyset-after-skip N` – When skip-based paging would exceed N, pivot to keyset pagination (`orderby ref_id asc`) to avoid Azure Search 100k skip ceiling.
* `--no-progress-exit-batches N` – Plateau safeguard: if N consecutive batches embed zero new docs, exit early (supervisor then runs diagnostic to confirm completeness).
* `--hex-shards / --hex-shard-index` – (Supervisor internal) Distributes first-hex windows across worker processes for parallel coverage.
* `--run-diagnose --diagnose-script` – After shards finish, executes diagnostic script for authoritative coverage JSON (verifies 0 missing embeddings).

Operational notes:
* Success criterion: `missing_docs == 0` from `diagnose_symbol_ref_gaps.py` (do not rely solely on shard remaining estimates during force mode).
* Force-mode shard "remaining" counts can appear stuck ~97–98% late; diagnostic resolves true 100% state.
* Deterministic ordering (`orderby ref_id asc`) + windowed scanning yields reproducible traversal & stable keyset handoffs.
* Plateau exits are rare; if triggered, check logs for repeated identical `last_ref_id` values (possible staleness).
* Force cycles are idempotent; re-running safely overwrites vectors.
* Spot fixes: `python backfill_symbol_ref_embeddings.py --ids REF123 REF456`.

Recommended quick flows:
1. Incremental top-off after new ingest: `python backfill_symbol_ref_embeddings.py --batch 128 --resume-missing`
2. Full periodic quality refresh (parallel + verified): use supervised force example above.
3. Manual validation: `python diagnose_symbol_ref_gaps.py`

Enrich variable usage with first/last ref positions:
```
python enrich_variable_usage_refs.py --batch 500
```

Create / recreate index:
```
python create_symbol_refs_index.py --force
```
Extract real refs (full ingest):
```
python extract_symbol_refs_from_xrefs.py --batch 1000
```
Stage limited ingest (e.g. first 5k):
```
python extract_symbol_refs_from_xrefs.py --batch 500 --limit 5000
```

### Integrity / Regression Suite
Symbol ID & embedding regression:
```
python test_symbol_id_regression.py --sample 30
```
Full composite (adds summary JSON):
```
python run_full_regression.py
```
Join probe (random variables):
```
python symbol_join_probe.py --random 3 --show-programs --top-items 5 --top-refs 5
```
Join probe (specific variable):
```
python symbol_join_probe.py --variable TOTAL-BW-COUNT --show-programs --top-items 5 --top-refs 5
```

### Recommended Recovery Flow (If Embeddings Lost)
1. Re-add vector field(s) (idempotent):
  - `python add_vector_field_variable_usage.py`
2. Run resume backfill:
  - `python backfill_embeddings_variable_usage.py --batch 128 --resume-missing`
3. Verify coverage:
  - `python variable_usage_embedding_coverage.py` (expect 100%)
4. Sanity probe:
  - `python probe_variable_usage_vectors.py --query "customer balance totals" --top 10`
5. Regression:
  - `python run_full_regression.py`

### Operational Notes
* `has_vector` is the authoritative completeness flag for variable usage; if any docs remain false after a run, re-run resume mode.
* The resume embedding mode uses shrinking filtered result sets—no skip paging—so it converges even if the index refresh is slightly delayed.
* Overwrites are safe: re-embedding a doc just replaces its vector and keeps `has_vector=True`.
* For large-scale reprocessing, consider temporarily reducing concurrency in other ingestion scripts to avoid throttling (not yet an issue observed).

### Potential Future Additions
| Item | Purpose | Status |
|------|---------|--------|
| vector_quality_probe.py | Sample cosine similarities & cluster inspection | Planned |
| orchestrated_semantic_search.py | Multi-index hybrid retrieval aggregator | Planned |
| nightly_health_report.py | Emit JSON snapshot of counts & coverage | Planned |


## Roles & Retrieval Graph

```
User Query
  ├─ Program Scoping → new_cobol_program_meta
  ├─ Data Structure Questions → new_cobol_data_items (+ copybook_meta for context)
  ├─ Logic / “How does it work?” → new_cobol_paragraphs
  ├─ Execution / Path → new_cobol_flow_edges
  ├─ Cross-program Dependencies → new_cobol_calls
  ├─ Structural Grouping (Copybooks) → new_cobol_copybook_meta
  └─ UI Navigation Trees → new_cobol_menu_trees
```

## Field Highlights

### new_cobol_data_items
Key: `item_id`
Essential fields: `program_id`, `path`, `item_name`, `level`, `pic`, `usage`, `occurs`, `redefines`, `is_group`, `length_bytes`, `has_vector`.
Use Cases: PIC/USAGE explanation, schema comparison, field lineage discussions (paired with call or paragraph context for usage).

### new_cobol_paragraphs
Key: `para_id`
Essential fields: `program_id`, `paragraph_name`, `normalized_name`, line span, body text, `has_vector`.
Use Cases: Procedural reasoning, control flow entry points, summarizing what code blocks do.

### new_cobol_flow_edges
Key: `edge_id`
Essential fields: `program_id`, `from_para`, `to_para`, edge type, `edge_vector`.
Use Cases: Tracing how execution reaches a target paragraph; “show the path between INIT and CLEANUP.”

### new_cobol_calls
Key: `call_id`
Essential fields: `caller_program`, `callee_program`, optional paragraph source, call snippet, `snippet_vector`.
Use Cases: Impact analysis (“What breaks if we change CALC-TAX?”), multi-hop dependency reasoning.

### new_cobol_program_meta
Key: `program_id`
Essential fields: classification flags (data-only / mixed), coverage %, counts (paragraphs, calls, items), enrichment features.
Use Cases: Program selection, ambiguity resolution, ranking/boosting.

### new_cobol_copybook_meta
Key: `copybook_id`
Essential fields: `copybook_name`, counts (level01, occurs, redefines), working-storage vs linkage presence, summary, vector.
Use Cases: High-level structural inventory, picking candidate copybooks for deeper inspection.

### new_cobol_menu_trees
Purpose: One document per top-level UI/dispatch/menu root capturing the hierarchical navigation tree (UI + limited logic hops) discovered from the call graph.

Generated By: `build_full_menu_tree.py`

Command Example:
```
python build_full_menu_tree.py \
  --limit-roots 20 \
  --non-ui-hop-budget 4 \
  --max-depth 20 \
  --prune-dead \
  --push \
  --export-json menu_trees_final.json
```

Document Fields:
- `root_program_id`: Root menu or dispatch program.
- `tree_json`: JSON string of recursive structure: `{ program_id, role, ui, depth, terminal, children:[...] }`.
- `total_nodes`: Count of nodes included in the tree.
- `total_ui_nodes`: Count of UI-classified nodes within tree.
- `ui_ratio`: `total_ui_nodes / total_nodes`.
- `max_depth`: Deepest depth reached (root depth = 0).
- `build_params_json`: Parameters used (non-ui budget, max depth, prune flag).
- `generated_at`: UTC timestamp of generation.
- `doc_type`: `menu_tree`.

Traversal Heuristics:
- UI roots chosen by role (UI/DISPATCH) & name tokens (contains `MENU`).
- Always include UI children; optionally include up to `non-ui-hop-budget` non-UI branches per depth chain.
- Cycle mitigation via (node, depth) visitation.
- `--prune-dead` removes branches that end without a UI descendant.

Usage Patterns:
1. Fetch a root tree to answer: *"Show me the complete menu tree for X"*.
2. Combine with `new_cobol_ui_paths` to highlight high-scoring linear flows inside the broader tree (overlay path sequences onto the tree nodes for scoring annotations).
3. Provide a progressive disclosure UI: render top-level children, expand on demand using the nested structure already precomputed (no recursive round-trips needed).
4. Derive analytics (e.g., UI depth distribution, branching factor) directly from `tree_json` to enrich summaries.

Integration with Answer Flow:
- When a user requests *"complete user screen flow"*, first attempt path overlay (from `new_cobol_ui_paths`) for linear exemplars; then show associated `menu_tree` segment for comprehensive context.

Future Extensions:
- Annotate nodes with outbound call counts or risk metrics from `new_cobol_program_meta`.
- Store a flattened adjacency list alongside the tree (for incremental diffing / partial updates).
- Add guard / condition metadata on edges when call parsing is enhanced.

## Added: new_cobol_program_deps
Key: `program_id`
Purpose: One-shot dependency resolution, reverse impact questions, and semantic dependency similarity.
Vector Support: `dependency_blob_vector` (3072 dims) populated (100% coverage) from `dependency_blob` summary.
Primary Fields:
- `outgoing_programs_json` / `incoming_programs_json`: JSON arrays (strings) of direct call graph neighbors.
- `external_programs_json`: Called names not recognized as internal programs.
- `copybooks_used_json`: Distinct copybooks referenced (from usage aggregation).
- `screens_touched_json`: Outgoing callees classified as UI (via `program_meta.ui_flag` or UI role).
- `dependency_blob`: Concise concatenated summary used for vectorization & lexical fallback.
- Counts & Flags: `outgoing_count`, `incoming_count`, `copybook_count`, `external_count`, `screens_count`, `has_outgoing`, `has_incoming`, `has_vector`.
- Audit: `updated_at`.

Retrieval Patterns:
1. Direct lookup: `search=PROGRAM_ID` to fetch full dependency sets.
2. Reverse dependency (current): lexical scan for target inside `incoming_programs_json`; (future optimization: add token array field).
3. Semantic variant: embed a question ("who relies on lonpf2 component") and vector search on `dependency_blob_vector`.

Build & Embedding:
```
python create_program_deps_index.py --force
python build_program_deps.py
python add_vector_fields_program_deps.py
python backfill_embeddings_program_deps.py --batch 64
```
Enhancements Completed:
- Coverage reporting (prints produced docs and % of meta universe).
- Idempotent vector schema addition + multi-pass embedding backfill to 100% completeness.

Future Enhancements:
- Add normalized token array fields for O(1) reverse dependency filters.
- Multi-hop expansion (second-hop outgoing) with size guard.
- Risk heuristic (e.g., centrality * external_count * screens_count) for change impact ranking.
## Added: new_cobol_program_flows
Key: `program_id`
Purpose: Pre-computed paragraph / PERFORM control-flow graph per program for instant visualization, reasoning over branching, cycle detection, and path summaries.
Primary Fields:
- Graph metrics: `node_count`, `edge_count`, `max_depth`, `has_cycles`, `perform_thru_unexpanded_count`, `risk_score`
- Node & edge payloads: `flow_nodes_json`, `flow_edges_json`
- Entry / Exit analytics: `entry_nodes_json`, `exit_nodes_json`, `high_fanout_nodes_json`
- Path sampling: `path_samples_json` (representative distinct paths)
- Role mapping: `paragraph_roles_json` (optional roles inferred - e.g. INIT, CLEANUP, DRIVER)
- Visualization: `mermaid_flow`, `condensed_mermaid`
- Summarization & semantic: `flow_summary`, `flow_vector`, `has_vector`
- Audit: `updated_at`

Creation:
```
python create_program_flows_index.py --overwrite
python build_program_flows.py --batch 200 --push
```

Embedding Backfill (if not already embedded during build): integrated into builder or via dedicated vector backfill script (future optional).

Retrieval Patterns:
1. Quick overview: fetch doc by `program_id` → show `condensed_mermaid` + `flow_summary`.
2. Detailed path reasoning: expand `path_samples_json` to answer “shortest path from INIT-PARA to COMMIT-PARA”.
3. Risk triage: filter where `has_cycles=true` or `risk_score > threshold`.
4. Semantic: vector search on `flow_vector` with queries like “programs with deep branching loops”.

## Added: new_cobol_screen_nodes
Key: `screen_id`
Purpose: Concrete representation of SCREEN SECTION extracted UI surfaces (fields + actions + inferred transitions) for UI-centric reasoning and screen-to-variable or screen-to-program joins.
Primary Fields:
- Identity & linkage: `screen_id`, `program_id`, `screen_name`
- Metrics: `field_count`, `action_count`, `transition_count`
- Payload: `fields_json`, `actions_json`, `transitions_json`, `raw_span_text`
- Summarization: `summary_text` (basis for vector field `summary_vector` added via `add_vector_field_screen_nodes.py` + backfill)
- Audit: `generated_at`, `has_vector`

Creation & Enrichment:
```
python create_screen_nodes_index.py --overwrite
python build_screen_nodes.py --batch 200 --push
python add_vector_field_screen_nodes.py
python backfill_screen_node_embeddings.py --batch 128 --resume-missing
```

Retrieval Patterns:
1. Lexical search on `screen_name` or field names inside `raw_span_text`.
2. Semantic search (post embedding) for “customer maintenance screen” using `summary_vector`.
3. Join to UI paths / program flows via shared `program_id` for end-to-end navigation answers.

## Added: new_cobol_name_aliases
Key: `alias_id`
Purpose: Expand user queries by mapping variant spellings, separators, case, truncations, and derived forms back to a `canonical_name` across PROGRAM / COPYBOOK / PARAGRAPH kinds.
Primary Fields:
- `alias_id`
- `canonical_name`
- `alias`
- `variant_type` (e.g., CASE_FOLD, STRIP_SUFFIX, NORMALIZED, TOKEN_SPLIT, DERIVED)
- `kind` (PROGRAM | COPYBOOK | PARAGRAPH | UNKNOWN)
- `source_hint`
- `ingested_at`

Creation:
```
python create_name_aliases_index.py --force
python build_name_aliases.py --push --sample 0   # full run
```

Retrieval & Integration:
Used at query-time expansion: map initial tokens to alias variants to widen lexical + semantic recall before multi-index dispatch (integrated into retrieval orchestrator).

Future Enhancements:
- Add vector field for alias clustering (optional; low priority)
- Add frequency weighting (alias popularity) for scoring boosts.

## Rebuild Orchestration
Unified rebuild script ensures deterministic full environment materialization:
```
python rebuild_all_programs.py --force --with-aliases --with-flows --with-screennodes
```
Steps (conceptual):
1. (Re)create core indexes (meta, data items, variable usage, symbol refs, deps, UI paths, screen nodes, flows, aliases)
2. Ingest / aggregate
3. Enrichment (symbol refs → variable usage; flows analytics; UI frequency metrics)
4. Embed vectors (batch backfills honoring `has_vector`)
5. Validation & probes (join probes, coverage reports)

## Updated Cross-Index Join Patterns
| Start | Join Targets | Purpose |
|-------|--------------|---------|
| variable usage (`symbol_id_global`) | symbol refs | Concrete lines & context |
| symbol refs (`program_id`,`paragraph_name`) | program flows | Localize reference within control-flow graph |
| program flows (`program_id`) | screen nodes | Combine logic branching with UI surfaces |
| name aliases (`canonical_name`) | any name-bearing index | Query expansion for recall |
| program deps (`program_id`) | flows + calls | Impact chain visualization with structural flow overlay |

These compounded joins support richer answers like: *“Show the control flow around the first write of CUSTOMER-BAL in a program that displays the Customer Maintenance screen.”*


## Consistency Conventions
* Key field always singular, snake-cased (e.g., `item_id`, `para_id`).
* Vector dimension uniform (3072) to simplify embedding provider interchangeability.
* Presence flag: `has_vector` across vectorized indexes.
* Temporal audit: `ingested_at` (ISO) on per-document basis where ingestion scripts control lifecycle.

### Cross-Index Stable IDs (New)
To enable reliable joins without fragile text matching, the following ID strategy is introduced:

| Concept | Field | Scope | Construction | Notes |
|---------|-------|-------|--------------|-------|
| Program | `program_id` | Global | Uppercased original name | Already primary key in program-centric indexes. |
| Data Item Symbol | `symbol_id` (data items) | Program-scoped | SHA1(UPPER(program_id) + '|' + UPPER(path)) | Added to `new_cobol_data_items`. Path encodes hierarchy. |
| Variable (global) | `symbol_id_global` | Global | SHA1(UPPER(variable_id)) | Added to `new_cobol_variable_usage` & symbol refs. |
| Variable (scoped) | `symbol_id` (variable usage) | Program-scoped (future) | For now mirrors `symbol_id_global` | Placeholder until per-program variable docs exist. |
| Reference Occurrence | `ref_id` | Unique | SHA1(program_id|line|symbol|kind) | In `new_cobol_symbol_refs`. |

Helper functions live in `id_normalization.py`:
`normalize_program_id`, `make_data_item_symbol_id`, `make_global_symbol_id`, `make_scoped_symbol_id`, `make_ref_id`.

Migration / Backfill Utilities:
* `add_symbol_id_to_data_items.py` + `backfill_symbol_id_data_items.py`
* `add_symbol_ids_to_variable_usage.py` + `backfill_symbol_ids_variable_usage.py`
* `create_symbol_refs_index.py` + `build_symbol_refs.py`

Integrity Test:
`test_symbol_id_integrity.py` recomputes hashes for sampled docs and reports mismatches.

Planned Enhancements:
* Explode aggregated variable usage into program-scoped variants to give `symbol_id` distinct meaning.
* Optional alias map for symbols spanning multiple programs with consistent semantics (join via `symbol_id_global`).

## Integration Notes
* `program_id` is the primary join axis across program-centric indexes.
* Copybook usage (future enhancement) will join by `program_id` + `copybook_name` → enrich `new_cobol_copybook_meta` with usage counts.
* Data items + paragraphs correlation can be inferred by overlapping line spans if needed (optional future index for explicit mapping).

## Current Quality Status
* All vector indexes validated with random sample + similarity probe.
* No known schema drifts or field-type mismatches.
* Ingestion scripts support resume & graceful interrupt for large sets.

## Deferred / Planned Enhancements
| Enhancement | Description | Status |
|-------------|-------------|--------|
| Copybook usage index (`new_cobol_copybook_usage`) | One doc per COPY statement occurrence | In progress |
| Synonym / alias map | Normalize variant field and copybook names for recall | Deferred |
| PACKED / COMP-3 precise sizing | Replace heuristic length_bytes with exact calculation | Deferred |
| Data item → paragraph usage link | Cross-reference where fields are referenced in logic | Deferred |
| Group-level semantic summaries | Aggregated explanations for large data groups | Deferred |
| Retrieval orchestrator module | Multi-index scoring & answer assembly pipeline | Deferred |

## Recommended Next Steps (If Needed)
1. Implement copybook usage index to enable dependency questions fully.
2. Add a retrieval orchestration helper consolidating top-k from paragraphs + data items + calls.
3. Introduce evaluation harness with canonical test queries for regression safety.

## Running the Inventory
```
python index_inventory_report.py
```

---
Generated as part of automated base index consolidation.
