# COBOL Retrieval & Analytics Indexes

This document enumerates every Azure AI Search index ("new_*" generation plus legacy) used by the COBOL knowledge/retrieval stack. It is written as if the consumer is another LLM that must:
1. Understand each index's intent and document granularity.
2. Know field semantics (data type, cardinality, retrieval role).
3. Recommend improvements or diagnose gaps (coverage, schema design, enrichment opportunities).

For each index:
- Purpose / Role in Answering Queries
- Document Granularity & Identity
- Field Inventory (with rationale and usage patterns)
- Vector & Semantic Config
- Ingestion / Refresh Flow & Diagnostics
- Enhancements Implemented in Recent Iterations
- Potential Future Improvements (LLM guidance targets)

---

## Global Diagnostics & Quality Gates (Updated)

The retrieval stack now includes standardized diagnostics, vector coverage enforcement, and CI gates to ensure answer quality and index health remain above minimum thresholds.

### Orchestration & Pipelines
| Component | Path | Purpose |
|-----------|------|---------|
| Build Orchestrator | `build_all.py` | Phased index creation, ingestion, backfills. |
| Backfill & Coverage Gate | `ops/run_backfills.py` | Executes embedding backfills (missing-only) and enforces per-index vector coverage threshold (default 90%). |
| Retrieval Orchestrator | `retrieval/orchestrate_answer.py` | Intent-driven hybrid (keyword + vector) retrieval with RRF fusion; guarantees symbol line evidence for variable/where-used; demotes placeholder UI paths; emits `mermaid` for FLOW/UI intents. |
| Evaluation Harness (planned) | `eval/run_random_eval.py` | Random sampling across program / variable / UI path question templates; enforces pass-rate (≥80%). |
| Make Targets | `Makefile` | `indexes`, `ingest`, `embed`, `eval`, `mvp` (pipeline composite). |
| CI Workflow | `.github/workflows/mvp-smoke.yml` | Runs `make mvp` on PR & main pushes; fails if vector coverage < threshold or eval pass rate < 80%. |

### Diagnostic Scripts
| Script | Path | Summary |
|--------|------|---------|
| Symbol Ref Probe | `diagnostics/probe_symbol_refs.py` | Counts refs, distinct programs, first-write ratio, and programs missing references vs flow index. |
| Program Flow Coverage Audit | `diagnostics/audit_program_flow_coverage.py` | Detects .CBL programs lacking flow docs with stub / missing PROGRAM-ID heuristics. |
| Referenced Copybooks Listing | `diagnostics/list_referenced_copybooks.py` | Enumerates referenced copybooks, orphan definitions, frequency spectrum. |
| Consolidated Vector Health | `consolidated_vector_health.py` | Summarizes vector field presence & coverage, classifies gap types (partial, missing_flag, zero_coverage). |
| Vector Coverage Assertion | `assert_vector_coverage.py` & `diagnostics/assert_vector_coverage.py` | CI assertion that each core index meets or exceeds minimum coverage (default 90%) unless allow‑listed. |

### Quality Gates
| Gate | Metric | Threshold | Source |
|------|--------|-----------|--------|
| Vector Coverage | has_vector true / total | ≥ 90% (configurable) | `ops/run_backfills.py`, `assert_vector_coverage.py` |
| Retrieval Evidence | evidence items per answer | ≥ 2 | `retrieval/orchestrate_answer.py` |
| Symbol Proof (Variable / Where-used) | presence of at least one `symbol_refs` doc | Mandatory | `retrieval/orchestrate_answer.py` |
| Flow/UI Diagram | mermaid non-empty (unless disabled) | Required for FLOW / UI | `retrieval/orchestrate_answer.py` |
| Eval Pass Rate | sampled Q/A success | ≥ 80% | `eval/run_random_eval.py` (pending) |

### Retrieval Orchestrator Highlights
* Intent classes: PURPOSE, DEPS, FLOW, VARIABLE, COPYBOOK, UI, MIXED.
* Hybrid retrieval: lexical + hash‑fallback vector embedding (replaced heavy dependency runtime for robustness in CI).
* RRF fusion with post-processing adjustments: symbol injection (VARIABLE), placeholder UI path demotion (UI).
* Output JSON schema: `{intent, question, answer_outline, mermaid, evidence[], meta}`.
* Evidence object always tries to include snippet and index provenance; duplicates avoided; single evidence edge case duplicated to meet ≥2 requirement.

### Makefile Targets (Quick Reference)
```
make indexes   # create indexes (idempotent)
make ingest    # run ingestion & build scripts
make embed     # backfill vectors + enforce coverage >= MIN_VECTOR_COV (default 90)
make eval      # run random evaluation harness (skips gracefully if missing for now)
make mvp       # full pipeline (indexes -> ingest -> embed -> eval)
```

### CI Behavior
1. GitHub Actions job `mvp-smoke` executes `make mvp`.
2. Failure conditions:
  - Any backfill coverage < configured threshold (default 90%).
  - (Once present) evaluation pass rate < 80%.
3. Non-fatal diagnostic output: consolidated vector health table printed for visibility.

### Roadmap Enhancements
* Add `eval/run_random_eval.py` with reproducible seed sampling and per-intent breakdown.
* Extend `consolidated_vector_health.py` to surface vector dimension mismatches & average vector_quality when field present.
* Promote retrieval orchestrator outline templates to a versioned spec for downstream LLM answer generation consistency testing.

---

## 1. `new_cobol_symbol_refs`

### Purpose
Fine‑grained variable/data item reference occurrences enabling precise "where used", line‑level evidence, copy/reference timelines, and symbol‑centric enrichment of higher level answers. Guarantees at least one line‑proof document in responses that require evidentiary anchors.

### Granularity
One document per symbol reference occurrence (per line / paragraph context) extracted from cross‑reference (xref) processing and validated against source excerpts.

### Key Identity Field
`ref_id` (unique). Composition convention: may be synthetically derived (e.g., `<program_id>:<symbol_id>:<line_number>:<seq>`).

### Field Inventory
| Field | Type | Role | Notes / Usage |
|-------|------|------|---------------|
| ref_id | String (key) | Identity | Filter & dedupe anchor |
| program_id | String | Scoping / faceting | Enables grouping refs by program; prevents misclassification as flow docs. |
| symbol_name | String (searchable) | Query matching | Direct keyword & fuzzy expansion target. |
| symbol_id | String | Scoped symbol key | Program‑scoped (may duplicate across programs). |
| symbol_id_global | String | Global symbol key | Stable join across data_items → variable_usage → symbol_refs. |
| kind | String | Raw xref kind | Original classification (READ, WRITE, PARAM_IN, PARAM_OUT). |
| normalized_kind | String | Canonical category | Collapses PARAM_* to READ / WRITE for aggregation. |
| op | String | Operation | Mirrors kind (kept distinct for future granular ops like MOVE/ADD). |
| line_number | Int | Evidence coordinate | Used in answer grounding & first-write detection. |
| file_path | String | Provenance | For display & direct source retrieval. |
| paragraph_name | String (searchable) | Local exec context | Enables paragraph‑scoped filtering. |
| excerpt | String (searchable) | Local textual context | Semantic embedding source text. |
| context_before / context_after | String | Additional snippet | 3–N line window; populated selectively for top refs. |
| excerpt_vector | Vector(Float32, dim=3072) | Semantic retrieval | Dimension aligned with other 3072‑dim vectors; hybrid + RRF fusion. |
| has_vector | Bool | Diagnostics | Coverage checks (vector completeness). |
| first_in_program | Bool | Source ordering semantics | Earliest occurrence per (program_id, symbol). |
| is_first_write | Bool | Data lineage anchor | True only on earliest WRITE / PARAM_OUT occurrence across all refs for the symbol. |
| cluster_key | String | Clustering / grouping | `(program_id|symbol_name)` for grouping heuristics. |
| ingested_at | String (timestamp) | Freshness | Staleness / rebuild gating. |

### Vector / Semantic Config
HNSW profile `ref-vector-profile` on `excerpt_vector`; semantic config prioritizes `excerpt` (title omitted). Hybrid retrieval uses lexical + vector fusion (RRF).

### Enhancements Implemented
- First Write Enrichment: `ingest/enrich_variable_usage_refs.py` marks earliest WRITE / PARAM_OUT per `symbol_id_global` with `is_first_write=true` and updates variable usage `first_write_location`.
- Guaranteed symbol evidence: Orchestrator ensures ≥1 symbol_refs doc for where/used queries (reliability requirement).
- Filter escaping fix: Prevents exact-match failures on symbol filters containing special characters.
- Priority key inference repair: Stops mislabeling as `program_flows` during fusion.
- Selective context expansion: Adds before/after only for top-ranked refs to manage token budget.
- Added `first_in_program`, `is_first_write`, and `cluster_key` for ordering & grouping.
- Vector dimension harmonized to 3072 for consistent weighting across indexes (was 1536 previously).

### Diagnostics
- `diagnostics/probe_symbol_refs.py` counts presence & cross‑checks each local program appears either in flows or symbol refs.
- Rebuild workflow: `create_symbol_refs_index.py` → `extract_symbol_refs_from_xrefs.py` (or `build_symbol_refs.py`).

### Potential Improvements
### Global Symbol Identity Chain (NEW)
Cross-index linkage for variable Q&A now relies on a shared `symbol_id_global`:

| Index | Field | Purpose |
|-------|-------|---------|
| new_cobol_data_items | symbol_id_global | Allows direct definition → usage/reference joins |
| new_cobol_variable_usage | symbol_id_global | Aggregated counts & first write location join key |
| new_cobol_symbol_refs | symbol_id_global | Line-precise evidence & first write flag |

Population Scripts:
- `add_symbol_id_global_to_data_items.py` – schema patch + backfill for data items.
- `build_variable_usage.py` (native) & `ingest/enrich_variable_usage_refs.py` (repair) – ensure usage docs carry global ID.
- `extract_symbol_refs_from_xrefs.py` / `build_symbol_refs.py` – initial refs; enrichment script recomputes missing IDs if any.

### First Write Determination Logic
1. Scan refs grouped by `symbol_id_global`.
2. Track earliest line where `op ∈ {WRITE, PARAM_OUT}`.
3. Mark that ref: `is_first_write=True`.
4. Update variable usage: `first_write_location = PROGRAM:LINE` (idempotent merge).
5. Leave unset if symbol has no write‑like operation (purely read-only / input param).

### Supporting / Maintenance Scripts (NEW)
| Script | Role |
|--------|------|
| `ingest/enrich_variable_usage_refs.py` | Computes earliest writes & updates refs + usage docs. |
| `search/backfill/add_vector_field_symbol_refs.py` | Idempotent schema patch (adds `is_first_write`, warns on vector dim mismatches). |
| `backfill_symbol_ref_embeddings.py` / `search/backfill/backfill_symbol_ref_embeddings.py` | Embedding population & force re-embed. Advanced flags: `--force`, `--ids`, `--windowed-scan-chars`, `--track-ids`, `--auto-keyset-after-skip N`, `--no-progress-exit-batches N` (plateau safeguard). |
| `add_symbol_id_global_to_data_items.py` | Introduces & backfills global symbol ID to data items. |

### Query Patterns Enabled
- "Where is BT-BRANCH instigated?" → Filter `symbol_id_global` then pick `is_first_write=true` ref + show `file_path:line_number`.
- "How is BT-BRANCH used?" → Join variable usage counts (read/write, param in/out) + sample refs + definition (data_items) via global ID.
- "Show first write and subsequent writes" → Sort refs by `line_number`, highlight `is_first_write`.

- Derive write/read classification density per symbol to feed data‑flow summarization.
- Add `paragraph_offset` (position index inside paragraph) to sharpen precision snippet extraction.
- Introduce `evidence_rank` precomputed weighting (e.g., definitions > writes > reads).
- Provide aggregated per‑program summary doc (materialized view) to reduce N lookups.

---

## 2. `new_cobol_copybook_usage`

### Purpose
Enumerate each COPY statement usage (with or without REPLACING) to support copybook dependency analysis, coverage diagnostics, orphan detection, and semantic retrieval of surrounding inclusion context.

### Granularity
One document per occurrence of a COPY directive in a source program.

### Identity
`usage_id` (synthetic). Often composed from `<program_id>:<line_start>:<normalized_copybook_name>:<seq>`.

### Field Inventory
| Field | Type | Role | Notes |
|-------|------|------|-------|
| usage_id | String (key) | Identity | Unique per occurrence. |
| program_id | String | Grouping / filter | Upstream for frequency analysis. |
| program_name | String | Display / search | Human friendly variant. |
| copybook_name | String (searchable) | Raw reference | Retains extension for quality gating. |
| normalized_copybook_name | String | Join key | Extension stripped; aids cross‑file joins. |
| copybook_name_plain | String | Filtering | Another normalization cut (path removed). |
| section | String | Location anchor | (IDENTIFICATION, DATA, PROCEDURE, SCREEN, etc.). |
| paragraph_name | String (searchable) | Local exec context | Enables scoping queries (e.g., only copies inside paragraph X). |
| line_start / line_end | Int | Span | Multi‑line copy / replacing coverage. |
| inclusion_order | Int | Relative sequence in program | Helps reconstruct logical layering. |
| file_path | String | Provenance | Source file of host program. |
| line_number | Int | Redundant anchor (some ingestion scripts supply). |
| raw_copy_line | String (searchable) | Exact COPY line for lexical queries. |
| context_snippet | String (searchable) | Local text window feeding vector. |
| expansion_present | Bool | Indicates copybook resolved / found. |
| has_replacing_clause | Bool | Flags semantic variation risk. |
| program_classification | String | Upstream classification (e.g., TRANSACTION, BATCH, UI). |
| ingested_at | DateTimeOffset | Freshness. |
| has_vector | Bool | Vector coverage. |
| context_vector | Vector(3072) | Semantic retrieval. |

### Enhancements
- Reframed coverage metric: now counts only *referenced* copybooks (vs raw existence of .CPY files) to eliminate false missing interpretations.
- Added `has_replacing_clause` to track transformation risk.
- Added frequency enumeration script for referenced vs orphan detection (`diagnostics/list_referenced_copybooks.py`).

### Diagnostics & Rebuild
Created via `create_copybook_usage_index.py` then ingestion by a usage extraction script (not displayed here—assumed existing). Diagnostics produce counts: distinct referenced copybooks, orphan definitions, usage frequency distribution.

### Potential Improvements
- Add structural digest (hash of expanded region) to detect drift when copybook content changes.
- Add `referenced_symbols_json` (list of symbol_ids appearing within expansion window if pre-processed) for faster dependency queries.
- Aggregated per‑copybook summary doc (in a separate index) for coverage dashboards.

---

## 3. `new_cobol_program_flows`

### Purpose
Provide condensed PERFORM / paragraph control‑flow graphs and summarized execution topology for each program. Powers higher‑level “flow” or “what does this program do” reasoning and diagram rendering (Mermaid).

### Granularity
One document per COBOL program.

### Identity
`program_id` (key) matching naming used in other indexes.

### Field Inventory
| Field | Type | Role | Notes |
|-------|------|------|-------|
| program_id | String (key) | Identity & cross-index join | Searchable for direct lookups. |
| node_count | Int | Graph metric | Distinct paragraphs. |
| edge_count | Int | Graph complexity | Counts PERFORM edges. |
| max_depth | Int | Longest path heuristic | Risk / complexity signal. |
| has_cycles | Bool | Cycle detection | Flags potential loop constructs or paragraph recursion. |
| flow_nodes_json | String (JSON) | Node list | Paragraphs in source order. |
| flow_edges_json | String (JSON) | Edge list | Array of {src,dst}. |
| entry_nodes_json | String (JSON) | Entry points | Supports partial flow queries. |
| exit_nodes_json | String (JSON) | Exit paragraphs | For termination analysis. |
| high_fanout_nodes_json | String (JSON) | Hubs | Aids hotspot summarization. |
| path_samples_json | String (JSON) | Example representative paths | For narrative generation. |
| mermaid_flow | String (searchable) | Full diagram DSL | Direct rendering. |
| condensed_mermaid | String (searchable) | Reduced diagram | For large programs. |
| flow_summary | String (searchable) | Human summary | Semantic basis for high-level answers. |
| flow_vector | Vector(3072) | Embedding of summary | Vector queries (“program similar to X”). |
| has_vector | Bool | Coverage metric | Identify unembedded docs. |
| perform_thru_unexpanded_count | Int | Counts PERFORM THRU not expanded | Quality risk indicator. |
| paragraph_roles_json | String (JSON) | Role tagging (INIT, IO, COMPUTE, EXIT, ERROR) | Program structure classification. |
| risk_score | Double | Composite risk | Derived (e.g., f(node_count, cycles, THRU count, fanout). |
| updated_at | String | Freshness. |

### Enhancements
- Added `condensed_mermaid` for token‑efficient rendering.
- Added paragraph role tagging & high fanout list for architectural summarization.
- Introduced `risk_score` and `perform_thru_unexpanded_count` to surface refactor targets.
- Added entry/exit nodes and path samples to strengthen narrative synthesis.

### Diagnostics
- `diagnostics/audit_program_flow_coverage.py` enumerates missing programs (ensures each .CBL has flow doc or reason). Reasons: missing PROGRAM-ID, stub, parse failure.
- Coverage gating integrated into evaluation harness (flow questions require at least one diagram present).

### Potential Improvements
- Add `branch_factor_distribution_json` for deeper complexity analytics.
- Add inline frequency of PERFORM targets executed (if runtime profiling ever integrated).
- Materialize `dominators_json` to enable structural decomposition suggestions.

---

## 4. `cobol-index` (Legacy / Line Index)

### Purpose
Line‑level lexical + vector search across raw COBOL source. Serves as broad fallback and symbol_name oriented lexical expansion.

### Granularity
One document per source line (with optional symbol annotations).

### Fields (from `create_search_index.py`)
| Field | Type | Role |
|-------|------|------|
| id | String (key) | Unique line identity |
| repo_path | String (searchable, filterable) | File path for scoping |
| line | Int | Line number |
| code | String (searchable) | Main lexical body |
| symbol_name | String (searchable) | Quick symbol queries |
| symbol_kind | String (filterable/facetable) | Symbol taxonomy |
| calls | Collection(String) | Direct CALL names for dependency pivots |
| embeddings | Vector(1536) | Semantic retrieval (OpenAI embedding) |

### Notes / Enhancements
- Retained as a legacy broad-spectrum fallback; newer targeted indexes (symbol_refs, copybook_usage) provide higher precision for structured queries.
- Could be augmented with a per‑paragraph aggregating view to reduce noise.

### Potential Improvements
- Add `paragraph_name` and `program_id` (if not already implicit) for easier joins.
- Decompose large files into paragraph chunk documents to reduce scoring bias from long files.

---

## 5. (Referenced but Not Shown) `new_cobol_program_deps`, `new_cobol_calls`, `variable_usage`, `ui_paths`, `screen_nodes`, `menu_trees`, `new_cobol_name_aliases`

These indices are referenced in orchestration logic (`retrieval/orchestrate_answer.py`) but their schema files weren’t part of the snippet set. Below are inferred roles (marking as INFERRED for transparency):

| Index | INFERRED Purpose | Expected Granularity | Key Fields (Inferred) | Potential Use |
|-------|------------------|----------------------|-----------------------|---------------|
| new_cobol_program_deps | Static inter-program dependency graph (CALL / COPY / data linkage) | One per program or edge doc | program_id, depends_on_program_id, dep_type, weight | Impact analysis, transitive upstream/downstream answers |
| new_cobol_calls | Individual CALL occurrences (richer than simple line list) | One per CALL site | call_id, program_id, target_program, paragraph_name, line_number, context | Where-called details, dynamic call map enrichment |
| variable_usage | Aggregated variable lifecycle per program (not per occurrence) | One per (program_id, variable) | variable_name, first_line, last_line, total_refs, write_count, read_count | Summarized usage/time span answers, hotspot detection |
| ui_paths | UI navigation path segments | One per path | path_id, from_screen, to_screen, action, frequency | UI flow queries (“how to reach screen X”) |
| screen_nodes | Atomic screen definitions | One per screen | screen_id, title, fields_json, program_id | Tie UI flow to data capture fields |
| menu_trees | Hierarchical menu graph | One per root tree | tree_id, root_label, mermaid_menu, node_count | High-level navigation answer generation |
| new_cobol_name_aliases | Symbol/Program canonicalization | One per cluster | alias_cluster_id, canonical_name, aliases_json, alias_type | Query expansion & de-duplication |

### Suggested Future Enhancements (Cross-Index)
- Standardize `program_id` indexing policy (`searchable:False` for purely filter fields to reduce index size unless full-text on ID is needed). Currently `program_flows.program_id` is searchable; others could align.
- Introduce a universal `updated_at` + `source_hash` for all indexes to orchestrate incremental sync and stale detection.
- Add `origin_index` field to every evidence doc to simplify fusion provenance reasoning.
- Derive composite synthetic documents (e.g., Program Profile) merging flows + top symbols + copybooks + dependency degree for single-document retrieval in high-level questions.

---

## 6. Orchestration / Fusion Enhancements (Index Interaction Layer)
While not indexes themselves, several retrieval behaviors materially change how index documents are used:

| Enhancement | Description | Impact |
|-------------|-------------|--------|
| Guaranteed symbol_refs presence | Forces at least one line-level doc for where/used | Evaluation reliability & evidentiary grounding |
| Alias expansion (name aliases index) | Expands query tokens across alias clusters | Recall for variant program/symbol names |
| RRF fusion across heterogeneous indexes | Reciprocal Rank Fusion blending lexical + vector + structured | Balances high-precision (symbol_refs) with high-recall (cobol-index) |
| Scope inference (flow/dependency/ui/mixed) | Dynamically selects candidate indexes set | Reduces noise, optimizes token bandwidth |
| Key precedence repair | Ensures documents retain correct `index` provenance | Prevents mislabeling flows as refs (or vice versa) |
| Symbol ref enrichment (context windows) | Adds before/after only for top selected refs | Improves summarization without bloating token count |
| Copybook coverage redefinition | Switch from raw file existence to referenced frequency | Accurate coverage diagnostics |
| Flow risk scoring & paragraph roles | Surfaces refactor priority & structured summarization | Higher-level reasoning quality |

### Embedding Backfill Parity (NEW)
All embedding-capable core indexes now support uniform re-embedding controls:

| Index | Vector Field | Backfill Script | Flags |
|-------|--------------|-----------------|-------|
| new_cobol_program_meta | program_summary_vector | backfill_embeddings_program_meta.py | --force, --ids PROGRAM_ID... |
| new_cobol_copybook_meta | summary_vector | backfill_embeddings_copybook_meta.py | --force, --ids COPYBOOK_ID... |
| new_cobol_program_flows | flow_vector | backfill_flow_embeddings.py | --force, --ids PROGRAM_ID... |
| new_cobol_symbol_refs | excerpt_vector | backfill_symbol_ref_embeddings.py | --force, --ids REF_ID..., `--windowed-scan-chars`, `--track-ids` |

Operational Guidelines:
1. Use targeted subset re-embeds after improving summary generation quality (e.g., fix flow_summary synthesis then: `python backfill_flow_embeddings.py --ids DAILY`).
2. Use `--force` for global embedding model upgrades or dimensional changes (ensure index schema already updated if dimension changes).
3. Combine `--force --ids` to re-embed only specific docs but overriding skip semantics even if `has_vector` is already true.
4. Schedule nightly lightweight coverage audit (extend `assert_vector_coverage.py`) to alert on any `has_vector=false` regressions across these four indexes.

Future Considerations:
- Introduce exponential backoff + retry logging for transient embedding failures (current scripts raise fast-fail).
- Emit a JSONL change log of re-embedded ids (facilitates downstream cache invalidation for vector caches or RRF tuning artifacts).
- Centralize shared argument parsing & fetch/upload helpers to reduce duplication across scripts.


---

## 7. Diagnostics Summary Mapping

| Script | Indexes Touched | Diagnostic Purpose |
|--------|-----------------|--------------------|
| rebuild_missing_indexes.py | symbol_refs, copybook_usage | Ensures foundational references exist |
| probe_symbol_refs.py | symbol_refs, program_flows | Coverage & ref counts sanity check |
| list_referenced_copybooks.py | copybook_usage | Frequency distribution & orphan detection |
| audit_program_flow_coverage.py | program_flows | Missing flow doc reasons classification |
| consolidated_vector_health.py | symbol_refs, copybook_usage, program_flows | Vector coverage completeness |
| assert_vector_coverage.py | Multiple | Gating on required embedding presence |

---

## 8. Cross-Index Design Principles (Current vs Target)

| Principle | Current Status | Improvement Opportunity |
|-----------|---------------|-------------------------|
| Granularity Fit | Mixed (line, occurrence, program) | Add aggregated synthetic layers (Program Profile, Copybook Summary) |
| Provenance Uniformity | Partially implicit (`index` outside doc) | Add `origin_index` field for internal LLM reasoning |
| Temporal Freshness | Some `ingested_at` fields | Normalize timestamp type & add `source_hash` |
| Vector Strategy | Varied dims (1536, 3072) | Document rationale & consider projection for alignment |
| Risk / Complexity Signals | Only in program_flows | Add per-symbol volatility metrics (churn) |
| Normalization of IDs | Inconsistent composition | Introduce canonical ID spec doc |

---

## 9. Recommended Next Steps (High Leverage)
1. Generate composite `program_profiles` index merging flows + top 20 symbols + copybook list + dependency degree + risk_score for single-shot retrieval.
2. Add `read_count`, `write_count` to aggregated `variable_usage` (or compute and persist) enabling data flow emphasis in answers.
3. Implement vector fallback for missing embeddings (batch reprocessor) with retry & instrumentation index.
4. Introduce `evidence_score` normalization across indexes to stabilize fusion without overfitting to doc length.
5. Add taxonomy for transaction vs batch vs UI vs support programs, materialized into multiple indexes for scope inference accuracy improvements.

---

## 10. Glossary
| Term | Meaning |
|------|---------|
| Occurrence Doc | A document representing a single concrete textual event (symbol ref, copy usage). |
| Profile Doc | Aggregated multi-source view (future concept). |
| Flow Hub | Paragraph with fanout above percentile threshold. |
| Orphan Copybook | Copybook defined but never referenced (no usage docs). |
| Risk Score | Heuristic: f(node_count, edge_count, max_depth, cycles, THRU count, hub density). |

---

If a schema file for a referenced index is later added, append a concrete section replacing the INFERRED placeholders above.

---

## NEW: `new_cobol_program_deps` (Materialized Dependencies)

### Purpose
Single-document snapshot of a program's static outbound calls, inbound callers (reverse mapping), external (non-local) targets, and copybook usages. Optimized for direct dependency questions without assembling multiple index joins at query time.

### Granularity
One document per program (`program_id`).

### Fields
| Field | Type | Description |
|-------|------|-------------|
| program_id | String (key) | Normalized program name (uppercase). |
| calls_out_json | String(JSON) | Sorted unique list of outbound static CALL targets present locally or external. |
| calls_in_json | String(JSON) | Sorted unique list of local programs calling this program. |
| external_programs_json | String(JSON) | Outbound calls lacking a local source file. |
| copybooks_used_json | String(JSON) | Sorted unique copybook names referenced (raw normalization). |
| dependency_blob | String | Narrative summary combining counts & lists (lexical retrieval seed). |
| dependency_blob_vector | Vector(3072) | Embedding for semantic dependency queries. |
| outgoing_count | Int | Size of calls_out set. |
| incoming_count | Int | Size of calls_in set. |
| external_count | Int | Size of external_programs set. |
| copybook_count | Int | Size of copybooks_used set. |
| has_vector | Bool | Embedding coverage. |
| updated_at | String | ISO8601 UTC timestamp of materialization. |

### Ingestion (Current Implementation – Catalog‑Driven, 2025‑09‑28)
The ingestion path was refactored from a filesystem‑only regex scan to a catalog + index driven materialization (`ingest/build_program_deps.py`).

Current Steps:
1. Discover canonical program universe via `program_catalog.discover_program_ids()` (merging program_meta, calls, copybook_usage, filesystem fallback).
2. (For metrics) Discover copybook universe via `discover_copybook_names()`.
3. Pull all call edges from `new_cobol_calls` (`caller_program`, `callee_program`) and build outgoing / incoming adjacency maps (normalized).
4. Pull copybook inclusion rows from `new_cobol_copybook_usage` and aggregate unique copybooks per program.
5. Derive `external_programs` as outbound callees not in the discovered program set.
6. Compose concise narrative `dependency_blob` (≤ ~500 chars) summarizing counts & top names for stable embeddings.
7. Upsert one doc per discovered program (even if it has zero calls/copybooks) — guarantees coverage and produces `calls_out_json="[]"` for leaf/no‑call programs.
8. Optional inline embedding when `--embed-inline` flag is provided (populates `dependency_blob_vector`, sets `has_vector=true`). Otherwise a backfill script handles embeddings.

CLI Flags (new):
- `--all` (required unless using `--only`)
- `--only <P1 P2 ...>` subset rebuild
- `--resume-from <PROGRAM>` lexicographic continuation (chunked reruns)
- `--batch <N>` upload batch size (default 500)
- `--embed-inline` perform embeddings during ingestion
- `--force-embed` (placeholder for future re-embed override)
- `--dry-run` preview without upserts
- `--limit <N>` debug cap

Legacy (Initial) Implementation (Now Deprecated):
- Walked `cobol_src` filesystem only; regex extracted CALL/COPY lines directly.
- Missed programs only discoverable via indexes (e.g., callee present in calls index but no local file).
- External classification relied solely on presence/absence of local `.CBL` files.
- Required explicit `--push` flag (removed; new script upserts unless `--dry-run`).

Coverage Metric:
- Script prints coverage percent of discovered universe (`docs_generated / total_discovered * 100`). Acceptance target: ≥95% when run with `--all`.

Operational Notes:
- If `new_cobol_calls` or `new_cobol_copybook_usage` are sparsely populated, docs still ingest (lists empty) — this can look like “no dependencies” but ingestion did occur.
- Missing `AZURE_SEARCH_ENDPOINT` / `AZURE_SEARCH_KEY` falls back to filesystem discovery only; coverage metric may drop.
- Inline embedding requires `AZURE_OPENAI_ENDPOINT` + key or equivalent OPENAI_* env vars.

### Strengths
- Constant-time retrieval for standard dependency questions.
- Avoids multi-index fusion overhead for simple queries.
- Provides narrative field for LLM summarization plus structured JSON lists for deterministic extraction.

### Limitations / Future Enhancements
- Static regex parsing (no conditional / dynamic call resolution).
- No paragraph-level call context (could add `call_sites_json`).
- No weighting (frequency of calls) currently captured.
- Copybook list untyped (could tag screen vs data vs copy-replacing). 

### Potential Improvements
- Integrate with `new_cobol_calls` (if available) to enrich with line numbers & paragraph names.
- Add `transitive_out_count` (2-hop unique callee count) for impact estimation.
- Add `degree_score` or composite risk for dependency sprawl analytics.
- Add `first_seen_at` / `last_seen_at` if historical snapshots retained for drift detection.

### Embedding Backfill
- Scripts: `add_vector_fields_program_deps.py` (idempotent) and `backfill_embeddings_program_deps.py` produce embeddings for `dependency_blob` and set `has_vector`.

# COBOL Search Index Architecture

This document describes all Azure Cognitive Search indexes created for the COBOL application analysis and RAG / navigation features. It captures purpose, field inventory, key design choices, sample documents, and improvement recommendations for each index.

> Source for schemas & counts: `index_schemas.json` (generated from live service) on 2025-09-24.

## Index Overview

| Index | Purpose | Approx Docs | Key Field (Key) | Notes |
|-------|---------|-------------|-----------------|-------|
| `new_cobol_program_meta` | Per-program aggregated metadata (graph metrics, role classification, reachability, structural stats) | 1,572 | `program_id` | Central hub feeding UI detection & path generation. |
| `new_cobol_calls` | Individual call edges (caller -> callee) for graph construction | (dynamic) | (edge implicit / composite) | Used to build adjacency and frequency heuristics. |
| `new_cobol_ui_paths` | Heuristically enumerated UI navigation/path sequences | 664 | `path_id` | 100% root coverage; placeholders + full vectorization (path_vector) achieved. |
| `new_cobol_menu_trees` | Canonical tree snapshots per potential root (program) | 1,572 | `root_program_id` | Full breadth; one tree per program capturing children fan-out. |
| `new_cobol_flow_edges` | (Placeholder / reserved) flow edges or enriched relationships | TBD | `edge_id` (planned) | Not fully utilized yet; potential vector/semantic enrichment target. |
| `new_cobol_paragraphs` | Paragraph-level retrieval with vectors for semantic code navigation | TBD | `para_id` | Supports fine-grained code explanation & paragraph semantic search. |
| `new_cobol_data_items` | Data division items (hierarchical) with semantic vectors | TBD | `item_id` | Enables structural data exploration & PIC/usage lookups. |
| `new_cobol_copybook_meta` | Summary metadata per copybook file (structure stats + embedding) | TBD | `copybook_id` | For locating & characterizing copybooks rapidly. |
| `new_cobol_copybook_usage` | One doc per COPY statement occurrence (context + vector) | TBD | `usage_id` | Dependency & impact analysis; locating expansion sites. |
| `new_cobol_variable_usage` | Aggregated variable usage/read/write statistics | TBD | `variable_id` | Hotspot detection & unused / write-only variable analysis. |
| `new_cobol_system_overview` | Macro snapshot of system metrics & distributions | Low (1..N snapshots) | `overview_id` | Supports executive/system-level reporting & trend diffs. |

---
## 1. `new_cobol_program_meta`
**Purpose:** Canonical per-program record aggregating structural & behavioral attributes required for navigation analysis, UI detection, risk surfacing, and answer composition.

**Document Count:** 1,572

**Primary Key:** `program_id`

**Field Groups:**
- Identity / Linking: `program_id`
- Call Graph Metrics: `outgoing_count`, `incoming_count`, `unique_callees[]`, `unique_callers[]`, `has_cycles`, `call_depth_score`
- Summaries & Narrative: `program_summary`, `sample_call_lines`, `flow_graph_json`
- UI & Screen Participation: `ui_flag`, `ui_path_participant`, `input_screen_paths_json`, `program_role`
- Reach / Centrality: `reach_out_size`, `reach_in_size`, `centrality_score`
- Risk & External Interaction: `risk_flag`, `dynamic_call_ratio`, `external_callee_count`, `external_callees[]`
- Code Coverage / Structure: `total_lines`, `covered_lines`, `coverage_pct`, `paragraph_count`, `avg_paragraph_length`, `median_paragraph_length`, `max_paragraph_length`, `gap_count`, `largest_gap_length`
- Classification / Tagging: `classification` (string), `program_tags`, `risk_tags`, `ui_tokens`
- Derived Complexity & Graph Stats (if present): `degree_score`, `betweenness_rank`, `fan_in_rank`, `fan_out_rank`
- Temporal / Generation: `generated_at` (if added), `version_hash`

> NOTE: Above list includes observed + intended fields; some may not yet exist in the live schema (only those present in `index_schemas.json` are guaranteed). Unused planned fields are candidates for formal addition if product value proven.

**Sample Document (truncated):**
```json
{
  "program_id": "GLMENU",
  "outgoing_count": 4,
  "incoming_count": 0,
  "program_summary": "Program GLMENU; calls 4 others; not called by others; depth=2; role=UI",
  "ui_flag": true,
  "reach_out_size": 6,
  "reach_in_size": 0,
  "centrality_score": 0.1176,
  "risk_flag": false
}
```

**Design Notes:**
- Heavy use of filterable + sortable numeric fields enables ad-hoc ranking queries (e.g., highest centrality, largest fan-out).
- `program_summary` is free-text searchable to support semantic root hints.
- Graph JSON (`flow_graph_json`) intentionally not searchable to reduce index size; could be promoted to searchable if text-mining is needed.

**Potential Improvements:**
1. Add vector embedding field for semantic program queries (e.g., `program_summary_vector`).
2. Introduce normalized risk scoring numeric field (0–1) aggregate of multiple risk flags.
3. Add `module_cluster_id` to group related programs for multi-hop summarization.
4. Maintain a `last_updated_utc` for synchronization integrity.
5. Evaluate splitting rarely-used bulky fields (graph JSON) into a secondary index or blob storage.

---
## 2. `new_cobol_calls`
**Purpose:** Edge-level representation powering adjacency, path enumeration, frequency weighting, reverse traversal.

**Expected Fields (based on ingestion scripts):**
- `caller_program` (String, filterable/searchable)
- `callee_program` (String, filterable/searchable)
- Optional frequency or occurrence: `call_count` (if aggregated)
- Contextual metadata (potential future): `call_site_lines`, `conditional_flag`, `dynamic_flag`

**Notes:** Live schema JSON was not enumerated in `index_schemas.json` excerpt above (script truncated or permissions). Document with explicit retrieval later if needed.

**Sample (illustrative):**
```json
{
  "caller_program": "GLMENU",
  "callee_program": "CHKSEC"
}
```

**Design Considerations:**
- Might benefit from composite key surrogate: `edge_id = sha256(caller+">"+callee)` for idempotent upserts.
- Add `edge_weight` for frequency or static call complexity weighting.
- Provide reverse index optimization: maintain `callee_program` as facetable/filterable for inbound exploration.

**Improvement Ideas:**
1. Store execution classification (STATIC, DYNAMIC) for better risk scoring.
2. Persist paragraph or section anchor for context reconstruction.
3. Provide derived topological ordering / layer for path heuristics.

---
## 3. `new_cobol_ui_paths`
**Purpose:** Curated heuristic navigation flows from UI/menu root programs to downstream UI / leaf endpoints for explaining end‑to‑end user journeys.

**Document Count:** 664 (post expansion + placeholder synthesis; stable after pruning; 100% vector coverage)

**Key Field:** `path_id` (SHA-1 hash of sequence; legacy fields still present for compatibility)

**Core Fields (current schema union):**
- Identity: `path_id`, `start_program_id`, `end_program_id`, (legacy) `root_program_id`, `leaf_program_id`
- Sequences: `path_json` (canonical ordered array), `program_sequence_json`, `screen_ids_json`, `screen_sequence_json`
- Metrics: `hop_count` (edges), `length` (nodes), `ui_program_count`, `score`, `frequency_score`, `frequency_score_norm`
- Branch / Guards: `guard_summary`, `guards_json`, `branching_events_json`
- Frequency Details: `edge_freqs_json`, `avg_edge_freq`, `min_edge_freq`
- Classification / Generation: `is_placeholder` (bool), `path_type` (enum-ish: PLACEHOLDER, LOGIN_FLOW, MENU_FLOW, MAINT_FLOW, GENERIC_FLOW), `generation_pass` (int)
- Vector: `path_vector`, `has_vector`
- Temporal / Misc: `updated_at`, `generated_at`, `loop_collapsed`, `notes`

**Recent Enhancements (multi-pass tasks):**
1. Added `hop_count`, `guard_summary`, `frequency_score`, `frequency_score_norm`, and `updated_at` population.
2. Implemented multi-attempt expansion (depth/branch widening + high-degree seed augmentation) with `generation_pass` tagging.
3. Added automatic synthetic placeholder paths for uncovered menu roots (single-node) to guarantee base coverage; placeholders marked `is_placeholder=true` and `path_type=PLACEHOLDER`.
4. Added source-level guard harvesting (IF / EVALUATE lines referencing UI verbs) beyond summary text heuristics.
5. Introduced `path_type` classification (LOGIN_FLOW, MENU_FLOW, MAINT_FLOW, GENERIC_FLOW, PLACEHOLDER).
6. Added normalized frequency `frequency_score_norm` (0–1) for downstream ranking calibration.
7. Orchestrator demotes placeholder paths post-fusion and provides UI fallback outline when scope inferred as flow but UI markers detected.
8. Scope inference logic updated to give UI precedence unless significantly outweighed by flow terms.

**Coverage:** 54/54 menu roots represented. 664 total path docs (original + synthesized). All have `path_vector` populated following two-pass pagination fix; placeholders flagged with `(no outgoing UI calls detected)` and down-weighted at retrieval.

**Sample Path (truncated):**
```json
{
  "path_id": "3f61...",
  "start_program_id": "SPMENU",
  "end_program_id": "LOGUID",
  "path_json": "[\"SPMENU\",\"CHKSEC\",\"LOGUID\"]",
  "hop_count": 2,
  "guard_summary": "IF USER-ROLE DISPLAY; EVALUATE AUTH-LEVEL ...",
  "frequency_score": 1.2744
}
```

**Design Notes:**
- Guard extraction is heuristic; currently limited to line slicing (<=160 chars) and capped at 10 markers per path sequence.
- Frequency score mixes edge multiplicity (avg/min) and path depth; may overweight shallow high-frequency paths—future normalization by global maximum could help.
- Placeholder paths ensure coverage metrics but should be excluded (or down-weighted) in journey explanations; they have `hop_count=0` and no `frequency_score` beyond 0.0.

**Improvement Opportunities / Next Iterations:**
1. Deeper path extension for roots still lacking hop_count>=3 representative paths (current depth distribution skewed shallow).
2. Multi-screen alignment: populate ordered `screen_ids_json` for each program occurrence (currently first-screen per program heuristic).
3. Rich guard semantic extraction (parse conditions into structured predicates for answer generation).
4. Refine `path_type` taxonomy (e.g., SECURITY_FLOW, PRINT_FLOW) using additional pattern sets.
5. Embedding refresh incorporating guards + normalized frequency to improve semantic clustering.
6. Historical diff tracking: materialize prior generation pass snapshots to analyze coverage improvements over time.

---
## 4. `new_cobol_screen_nodes`
**Purpose:** Inventory of SCREEN SECTION derived UI artifacts (fields, actions, transitions) enabling field-level discovery, action analysis, and cross-program screen similarity.

**Document Count:** (dynamic; built on demand) – includes one doc per parsed screen block; multi-screen programs yield multiple `screen_id`s.

**Key Field:** `screen_id` (`PROGRAM::SCRN` synthetic identifier)

**Schema Fields:**
- Identity / Join: `screen_id`, `program_id`, `screen_name`
- Structural JSON: `fields_json` (level, name, pic), `actions_json`, `transitions_json`
- Raw / Summaries: `raw_span_text`, `summary_text`
- Metrics: `field_count`, `action_count`, `transition_count`
- Vector: `summary_vector` (3072 dims, HNSW `vprofile`), `has_vector`
- Temporal: `generated_at`

**Extraction Heuristics:**
1. Locate `SCREEN SECTION` slices up to `PROCEDURE DIVISION`.
2. Split blocks at new 01-level declarations—each becomes a screen candidate.
3. Field detection via level-number + name + optional PIC capture.
4. Actions aggregated from verbs (`ACCEPT`, `DISPLAY`, `ENTER`, `PROMPT`, CRUD verbs) and PF key tokens (PF1..PF24).
5. Transitions derived from inline `CALL 'PROG'` statements within the screen slice.

**Sample (truncated):**
```json
{
  "screen_id": "LONPF2::SCR1",
  "program_id": "LONPF2",
  "field_count": 37,
  "action_count": 5,
  "transition_count": 2,
  "screen_name": "LOAN-MAIN-SCREEN"
}
```

**Usage Patterns:**
- Join with `new_cobol_ui_paths` to enrich path explanations with screen structure.
- Vector search over `summary_vector` for similar screens (e.g., locate all rate maintenance screens).
- Filter by `action_count` to locate highly interactive screens, or by `transition_count` to analyze branching points.

**Limitations & Next Steps:**
1. Parsing is shallow; nested redefinitions / COPY injected fields may be missed—extend with copybook inlining stage.
2. Lacks field semantic typing (date, currency); can integrate with existing variable/type inference modules.
3. No direct link to variable usage or validation paragraphs—add `related_paragraphs_json` field in future.
4. Add `is_generate_source` flag if some screens produced by generator patterns (naming heuristics).
5. Screen to path alignment currently indirect (via program + sequence index); could store `path_ids_json` backrefs.

**Embedding Backfill:** Executed via `backfill_screen_node_embeddings.py` selecting docs where `has_vector=false`; embeddings stored in `summary_vector`.

---

---
## 4. `new_cobol_menu_trees`
**Purpose:** One hierarchical expansion doc per program capturing its immediate (and limited-depth) call-tree, enabling quick subtree inspection without recomputing graph traversals.

**Document Count:** 1,572 (one per program)

**Key Field:** `root_program_id`

**Core Fields (inferred from generation tool):**
- `root_program_id`
- `tree_json` (serialized hierarchical structure: each node has `program_id`, `role`, `ui`, `depth`, `children[]`, `terminal` flag)
- Potential extras: `generated_at`, `max_depth_observed`, `child_count`

**Sample (truncated):**
```json
{
  "root_program_id": "GLMENU",
  "tree_json": "{\"program_id\":\"GLMENU\",\"role\":\"UI\",\"ui\":true,\"depth\":0,\"children\":[{\"program_id\":\"FORM-PROGX\",...}] }"
}
```

**Design Notes:**
- Storing tree as one large string trades off query-ability for ingestion simplicity. Consider splitting nodes into a separate `menu_tree_nodes` index if fine-grained search/filter over interior nodes is required.
- Does not encode edge frequency or conditional branching yet.

**Improvement Options:**
1. Add `node_count`, `ui_node_count`, `leaf_count` metrics for quick filtering.
2. Extract and store a flattened `descendants[]` list (up to a cap) for containment searches.
3. Provide path compression (e.g., collapse linear chains into summarized segments to reduce payload size).
4. Add vector embedding of concatenated leaf IDs for similarity between menus.

---
## 5. `new_cobol_flow_edges` (Reserved / Incomplete)
**Purpose (Intended):** Serve as a normalized edge index distinct from raw call edges—possibly including inferred flow types (UI_TRANSITION, SECURITY_CHECK, DATA_LOOKUP), conditional flags, or enriched semantic labels.

**Current State:** Schema retrieval attempted; details either absent or index not yet populated with final structure.

**Recommended Path Forward:**
1. Formalize schema with: `edge_id`, `source_program`, `target_program`, `edge_type`, `frequency`, `security_sensitive`, `ui_transition`, `semantic_label[]`.
2. Populate from merged heuristics (calls + UI path derivations + menu expansions).
3. Provide vector field for semantic relationship classification.

---
## Cross-Index Relationships
| Source | References | Purpose |
|--------|-----------|---------|
| program_meta | calls / ui_paths / menu_trees | Acts as enrichment anchor for display & reasoning |
| calls | (program_meta) | Graph adjacency feeding path & tree generation |
| ui_paths | program_meta, calls | Precomputed navigation journeys for answer composition |
| menu_trees | program_meta | Rapid subtree visualization / fallback navigation synthesis |
| flow_edges (future) | program_meta, calls, ui_paths | Enriched edge semantics & analytics |
| paragraphs | program_meta | Link paragraph lineage to program meta & support granular retrieval |
| data_items | program_meta, copybook_meta | Data lineage & structure search linked to programs/copybooks |
| copybook_meta | copybook_usage, data_items | Summarize copybook purpose & structure |
| copybook_usage | copybook_meta, program_meta | Track inclusion points and expansion context |
| variable_usage | program_meta | Variable hotspot & dependency insights |
| system_overview | program_meta, variable_usage, ui_paths | Aggregated health & coverage metrics |

---
## Query Patterns Enabled
- Root Selection: sort `program_meta` by `outgoing_count` or `centrality_score`.
- UI Coverage Audit: filter `program_meta` by `ui_flag == true`, join presence in `ui_paths`.
- Security Funnel Identification: high `incoming_count` + program names or roles (e.g., CHKSEC).
- Path Explanation: fetch `ui_paths` by `root_program_id`, merge with `program_meta` details.
- Menu Drilldown: fetch `menu_trees` by `root_program_id` and parse `tree_json`.
- Paragraph Semantic Jump: vector search `new_cobol_paragraphs` by natural language snippet description.
- Data Field Exploration: filter `new_cobol_data_items` by `item_name` or search `path` for hierarchical segments.
- Copybook Impact: query `new_cobol_copybook_usage` by `copybook_name` to enumerate all inclusion points.
- Variable Hotspot: sort `new_cobol_variable_usage` by `write_count` or filter where `read_count == 0`.
- System Snapshot Trend: compare two docs in `new_cobol_system_overview` (e.g., `overview_id` = latest vs prior).

---
## Identified Gaps
1. Missing vector search capability (no vectorEmbedding / profiles yet) blocks semantic clustering / retrieval augmentation.
2. Frequency & guard metadata dropped from `ui_paths` due to schema mismatch (should be restored explicitly).
3. No consolidated coverage index summarizing UI representation metrics; consider `cobol_ui_coverage`.
4. Large monolithic `tree_json` field prevents selective node-level filtering.
5. Lack of provenance fields (`source_generation_pass`, `ingestor_version`) weakens reproducibility.
6. Paragraphs & data items indexes lack explicit linkage fields to variable_usage (e.g., shared stable symbol id).
7. Copybook usage index omits semantic configuration; adding will improve hybrid ranking.
8. System overview lacks embedded trend deltas (diff vs previous snapshot) for quick change detection.

---
## Recommended Enhancements (Roadmap)
| Priority | Area | Action |
|----------|------|--------|
| High | UI Paths | Add frequency fields & re-score; backfill missing UI screens with synthetic minimal paths |
| High | Program Meta | Introduce vector field for `program_summary` and maybe `flow_graph_text` |
| Medium | Menu Trees | Add node/leaf counts + optional flattened descendants array |
| Medium | Coverage | Create `cobol_ui_coverage` index for tracking UI coverage ratios over time |
| Medium | Flow Edges | Implement enriched `new_cobol_flow_edges` with semantic types & conditional markers |
| Low | Storage Optimization | Externalize large JSON (e.g., `flow_graph_json`) to blob w/ pointer field |
| Low | Similarity | Add embeddings for sequences (`ui_paths`) and menu leaf sets for clustering |
| Low | Copybook Usage | Add semantic config & summarize context_snippet for improved retrieval |
| Low | Variable Usage | Consider vectorization of variable usage context clusters |
| Low | System Overview | Add delta fields (`program_count_delta`, etc.) & trend classification |

---
## 6. `new_cobol_paragraphs`
**Purpose:** Fine-grained paragraph-level retrieval enabling precise code explanation, semantic similarity, and focused context injection into RAG answers.

**Key Field:** `para_id`

**Core Fields:** `program_id`, `file_id`, `paragraph_name`, `section_name`, `kind`, `line_start`, `line_end`, `length_lines`, `source_excerpt`, `hash`, `has_vector`, `para_vector` (3072-dim), `ingested_at`.

**Design Notes:**
- Vector field (`para_vector`) enables natural language to paragraph semantic mapping.
- `hash` supports idempotent upserts and change detection.
- `kind` & `section_name` allow filtering (e.g., SECTION headers vs working paragraphs).

**Improvements:**
1. Add `program_role` (copy from program_meta) for role-aware filtering.
2. Store `normalized_source_excerpt` with stripped line numbers for cleaner embeddings.
3. Provide aggregated paragraph metrics (sum length, average) via program_meta backfill.

---
## 7. `new_cobol_data_items`
**Purpose:** Searchable inventory of COBOL data division items with hierarchical paths and semantic vectors to answer structural data questions.

**Key Field:** `item_id`

**Core Fields:** `program_id`, `file_id`, `file_path`, `level`, `item_name`, `pic`, `occurs`, `redefines`, `usage`, `full_clause`, `parent_item`, `path`, `line_start`, `line_end`, `length_bytes`, `is_group`, `has_vector`, `ingested_at`, `vector` (3072-dim).

**Design Notes:**
- `path` encodes hierarchy enabling prefix searches (e.g., CUSTOMER.ADDRESS.*).
- Vector field supports semantic lookup ("customer postal code length").
- Distinguishes group vs elementary for summarization.

**Improvements:**
1. Add stable `symbol_id` shared with variable_usage for cross-index joins.
2. Add `occurs_count_int` numeric (currently string form in `occurs`).
3. Introduce `effective_length_bytes` after PIC/USAGE evaluation.

---
## 8. `new_cobol_copybook_meta`
**Purpose:** Summarize each copybook to accelerate dependency reasoning and semantic discovery of data/layout patterns.

**Key Field:** `copybook_id`

**Core Fields:** `copybook_name`, `file_path`, `program_like`, `lines_total`, `lines_non_comment`, `data_division_present`, `working_storage_items`, `linkage_items`, `redefines_count`, `occurs_count`, `level01_count`, `approximate_item_count`, `summary`, `has_vector`, `ingested_at`, `summary_vector`.

**Design Notes:**
- Embedding on `summary` supports conceptual queries (e.g., "invoice header layout").
- Structural counts provide quick complexity heuristics.

**Improvements:**
1. Add `dominant_usage_context` (e.g., INTERFACE, STORAGE, LINKAGE).
2. Add `referenced_by_programs` (truncated list) for hot copybooks.
3. Track `summary_version` to ensure embedding refresh on schema script change.

---
## 9. `new_cobol_copybook_usage`
**Purpose:** Each COPY statement occurrence with contextual snippet & optional embedding for impact analysis and expansion reasoning.

**Key Field:** `usage_id`

**Core Fields:** `program_id`, `program_name`, `copybook_name`, `normalized_copybook_name`, `section`, `paragraph_name`, `line_start`, `line_end`, `inclusion_order`, `raw_copy_line`, `context_snippet`, `expansion_present`, `has_replacing_clause`, `program_classification`, `ingested_at`, `has_vector`, `context_vector` (3072-dim).

**Design Notes:**
- Vector enables semantic retrieval ("where is the security copybook included").
- `has_replacing_clause` flags potentially divergent expansions.

**Improvements:**
1. Add semantic config for better hybrid scoring.
2. Store resolved include hash to match identical expansions across programs.
3. Add `copybook_id` foreign key (link into copybook_meta) for direct join.

---
## 10. `new_cobol_variable_usage`
**Purpose:** Aggregated variable usage metrics to identify hotspots, dead reads, and write-only fields across the codebase.

**Key Field:** `variable_id`

**Core Fields:** `read_count`, `write_count`, `param_in_count`, `param_out_count`, `total_refs`, `program_readers[]`, `program_writers[]`, `program_params_in[]`, `program_params_out[]`, `sample_refs_json`, `usage_role`, `first_write_location`, `ingested_at`.

**Design Notes:**
- Collections are filterable allowing queries like variables referenced by specific program sets.
- `usage_role` can classify patterns (e.g., CONFIG, COUNTER, TEMP) for answer shaping.

**Improvements:**
1. Add `volatility_score` (ratio of writes to total refs).
2. Add optional embedding summarizing usage contexts (future vector expansion).
3. Provide `last_seen_location` & change tracking for evolving variables.

---
## 11. `new_cobol_system_overview`
**Purpose:** Macro-level system health & structure snapshot; foundation for trend analysis and executive reporting.

**Key Field:** `overview_id` (e.g., `latest`, timestamp, release tag)

**Core Fields:** `generated_at`, `program_count`, `ui_program_count`, `risk_program_count`, `avg_coverage_pct`, `coverage_bands_json`, `role_distribution_json`, `top_central_programs_json`, `top_fan_out_programs_json`, `top_fan_in_programs_json`, `external_programs_json`, `notes`.

**Design Notes:**
- JSON string fields hold arrays/buckets; denormalization keeps index simple for now.
- Serves as anchor for derived KPIs (coverage progress, UI path coverage delta).

**Improvements:**
1. Add deltas vs previous snapshot (`*_delta`).
2. Add `ui_path_coverage_pct`, `menu_tree_completeness_pct` metrics inline.
3. Consider splitting large top lists into separate facet-friendly index if size grows.

---
### Consolidated Notes on the Ten Actively Utilized Indexes
Flow edges remain planned; other ten deliver operational value today. Prioritize vector enrichment & linkage keys to unlock multi-index semantic joins.


---
## Suggested Governance / Review Checklist
1. Schema Drift Detection: Schedule a daily diff of live index schema vs repo specification.
2. Field Cardinality Monitoring: Track explosive growth fields (e.g., arrays) to avoid query performance degradation.
3. Size & Latency Budget: Record average doc size per index; ensure under target for query SLA.
4. Security Sensitivity Tagging: Add explicit `sensitive` boolean to risky programs (auth, financial calc) for filtered retrieval.
5. Versioning: Maintain a `schema_version` field or maintain this document as part of a release process.

---
## Quick Reference Summary
- Core Graph Hub: `new_cobol_program_meta`
- Raw Call Edges: `new_cobol_calls`
- Navigation Journeys: `new_cobol_ui_paths`
- Structural Trees: `new_cobol_menu_trees`
- Future Enriched Edges: `new_cobol_flow_edges`

---
## Next Steps (If Approved)
1. Draft and apply schema update for `new_cobol_ui_paths` (add frequency fields + guards).
2. Add embeddings: create vector profile (e.g., `summary_vector`, dimension 1536) for program meta.
3. Implement coverage reporting index to track improvements toward ≥90% UI path coverage.
4. Design & populate `new_cobol_flow_edges` with semantic labeling pipeline.

---
*End of INDEXES Architecture Document.*

---
## Appendix: Embedding Coverage Status (Post Backfill Completion)

All vector-capable core indexes now at 100% `has_vector=true` coverage:

| Index | Vector Field(s) | Dimension(s) | Docs | Vectorized | Coverage |
|-------|------------------|--------------|------|------------|----------|
| new_cobol_symbol_refs | excerpt_vector | 3072 | 63,405 | 63,405 | 100% |
| new_cobol_copybook_usage | context_vector | 3072 | 112,534 | 112,534 | 100% |
| new_cobol_copybook_meta | summary_vector | 3072 | 7,819 | 7,819 | 100% |
| new_cobol_variable_usage | usage_summary_vector | 3072 (logical) | 10,735 | 10,735 | 100% |
| new_cobol_screen_nodes | summary_vector | 3072 | 1,609 | 1,609 | 100% |
| new_cobol_ui_paths | path_vector | 1,536 | 664 | 664 | 100% |

### Key Operational Improvements
1. Centralized Secret Loading: Introduced `secrets_loader.py` with multi-source env hydration and alias normalization, eliminating redundant per-script loaders.
2. Pagination Fix (UI Paths): Adjusted skip advancement to prevent skipping unvectorized documents when server-side filtering used.
3. Missing-Only Mode: Added server-side `(has_vector eq false) or (has_vector eq null)` filter support to backfill scripts for efficient mop-up passes.
4. Fallback Summaries: Implemented deterministic fallback text for empty `summary` (copybook_meta) and empty variable usage summaries, enabling full coverage.
5. Two-Pass UI Path Embedding: Initial batch (512 docs) followed by corrected pagination pass (remaining 152) -> full vectorization.
6. Idempotent Re-Entrancy: Scripts tolerate repeated execution without duplicate uploads (merge semantics) and can force re-embed via flags.

### Re-Embed Guidelines
Use targeted subset re-embeds after text generation improvements:
```
python search/backfill/backfill_embeddings_copybook_meta.py --ids CUSTADDR --auto-key
python backfill_variable_usage_vectors.py --all --limit 200 --preview
```
For global model upgrade (dimension unchanged): run each backfill with `--force` sequentially (smallest index first) to surface issues early.

### Future Hardening Ideas
- Add `summary_hash` / `usage_summary_hash` to enable true incremental embedding.
- Introduce exponential backoff + structured retry logs (currently exponential in some scripts only).
- Consolidate shared fetch/upload patterns into a mini SDK module (reduces pagination divergence risk).
- Emit JSONL audit of re-embedded doc IDs to support downstream cache invalidation.

---

## Purpose Retrieval Hardening (Program Meta Coverage Repairs)

Recent additions to ensure purpose queries like "What is the purpose of LONPF2?" reliably surface the correct `new_cobol_program_meta` document:

- Prefix Boosting: During purpose scope reranking, candidate docs whose `program_id` shares the first 5 chars with the primary query token receive a soft boost (+1.5) to stabilize near-miss ranking (e.g., TIM36 vs TIM360).
- Direct ID Injection: If the exact `program_id` (or O/0 normalization variants) is absent from the top-k fuse results, an on-demand filtered lookup (`program_id eq 'X' or program_id eq 'XO'...`) injects the matching doc(s) ahead of reranked results before final trim.
- O/0 Variant Handling: Both zero-to-letter and letter-to-zero substitutions attempted to mitigate transcription ambiguity in user prompts.
- Targeted Meta Repair Script: `inject_specific_program_meta.py` can insert or repair missing program meta documents (e.g., LONPF2, TIM360) with deterministic summaries and optional immediate embedding generation.

### Injection / Repair Script
`inject_specific_program_meta.py`:
- Inputs: `--ids P1 P2 ...`
- Options: `--force-replace` (overwrite existing), `--no-embed` (skip embedding even if credentials), automatically batches embedding calls.
- Summary Heuristic: Minimal deterministic string: `Program <ID>; calls <N>; called by <M>` to remain stable and safely embeddable.
- Embeddings: Uses Azure OpenAI deployment specified via `AZURE_OPENAI_EMBEDDING_DEPLOYMENT` (falls back to `OPENAI_EMBEDDING_MODEL`). Retries with exponential backoff.

### Operational Flow for a Missing Program
1. Detect absence via `search/diagnostics/probe_meta_purpose_entities.py --ids <ID>` (returns found=false).
2. Run repair: `python inject_specific_program_meta.py --ids <ID>`.
3. Re-run purpose query; orchestrator prefix + direct ID injection logic ensures presence in top results.
4. (Optional) Later run full `build_program_meta.py` to enrich richer graph-derived fields; embeddings remain valid (no override needed unless summary changes).

### Future Improvements
- Add alias normalization layer (cluster-based) before direct injection to catch broader variant sets.
- Materialize composite program profile index merging flows + dependencies + summary for single-document purpose answers.
- Track `purpose_quality_score` (heuristic on summary length + field coverage) to prioritize re-enrichment.

### Unified Vector Re-Embedding Controls (All Purpose-Relevant Indexes)
Purpose answers may involve not only program meta but also copybook meta, flows, and symbol references (for evidentiary grounding). All four embedding-bearing indexes now expose consistent backfill flags:

| Index | Summary Field | Vector Field | Script | Example Subset Refresh | Full Force Refresh |
|-------|---------------|--------------|--------|------------------------|--------------------|
| new_cobol_program_meta | program_summary | program_summary_vector | backfill_embeddings_program_meta.py | `python backfill_embeddings_program_meta.py --ids TIM360 LONPF2` | `python backfill_embeddings_program_meta.py --force` |
| new_cobol_copybook_meta | summary | summary_vector | backfill_embeddings_copybook_meta.py | `python backfill_embeddings_copybook_meta.py --ids CUSTADDR COPYHIST` | `python backfill_embeddings_copybook_meta.py --force` |
| new_cobol_program_flows | flow_summary | flow_vector | backfill_flow_embeddings.py | `python backfill_flow_embeddings.py --ids DAILY LNQUOT` | `python backfill_flow_embeddings.py --force` |
| new_cobol_symbol_refs | excerpt | excerpt_vector | backfill_symbol_ref_embeddings.py | `python backfill_symbol_ref_embeddings.py --ids REF123 REF987` | `python backfill_symbol_ref_embeddings.py --force --windowed-scan-chars --track-ids` |

Operational Guidance:
1. After modifying summary generation logic (e.g., flow risk scoring changes), run a targeted subset re-embed for impacted program_ids before a full force.
2. Use `--force --ids` to surgically re-embed even if `has_vector` is already true (e.g., to correct previously truncated excerpts or summaries).
3. Run a nightly lightweight coverage audit (extend `assert_vector_coverage.py`) to confirm `has_vector=true` across all four indexes; alert if any drift appears.
4. Pre-flight schema updates (dimension change) before a force re-embed; dimension mismatch will cause silent ingestion failures or 400 errors.
5. Stagger large force operations (flows then meta then symbol_refs) to avoid embedding rate throttling; record durations to establish baseline SLAs.

Recommended Maintenance Cadence:
- Daily: Coverage audit (no re-embed unless gaps found).
- Weekly: Differential subset re-embed of docs whose source-derived summaries changed (detected via hash) – future enhancement.
- Monthly / Model Upgrade: Full `--force` cycle across all four indexes (sequence: smallest → largest to surface issues early).

Future Hardening Ideas:
- Add `summary_hash` / `excerpt_hash` fields to enable cheap change detection and incremental embedding pipelines.
- Emit a JSONL change log (program_id/ref_id + timestamp) per backfill run for downstream cache invalidation.
- Centralize shared embedding backfill utility module to reduce duplicated pagination and upload code.
- Introduce retry + partial failure reporting (current scripts fail fast on HTTP error).

---

## Retrieval Orchestrator & Evaluation Updates (2025-09-30)

This section captures the most recent retrieval layer enhancements and supporting evaluation tooling added after full embedding coverage was achieved.

### New Orchestrator Capabilities (`retrieval/orchestrate_answer.py`)
| Feature | Description | Purpose / Impact |
|---------|-------------|------------------|
| Batch Mode (`--jsonl-input` / `--jsonl-output`) | Processes many questions from a plain text or JSONL file; writes one JSON object per line. | Enables large scale offline evaluation & regression tracking. |
| Dry Run (`--dry-run`) | Skips all Azure Search calls; returns candidate index list + outline scaffold. | Fast CI smoke test without credentials; validates scope inference logic. |
| Debug Logging (`--debug`) | Captures per-index vector/keyword success/failure + counts in `meta.debug_logs`. | Aids diagnosis of partial index outages / schema mismatches. |
| No Diagram (`--no-diagram`) | Suppresses Mermaid/graph synthesis. | Lowers latency & token usage in bulk eval runs. |
| Embedding Cache | 256‑entry in‑memory FIFO keyed by (dimension,text head). | Reduces duplicate embedding calls across similar batched questions. |
| Dry-Run Safe Exit | Returns structured payload even when credentials absent. | Shields CI and local tooling from failure noise. |
| Improved Error Resilience | Per‑index vector search exceptions downgraded to empty list with optional debug note. | Prevents single index failure from collapsing entire fusion. |

### Retrieval Logic Adjustments
1. Placeholder UI Path Demotion retained but now happens after fusion with stable ordering (ensures placeholders never crowd top evidence when real paths exist).
2. Symbol Reference Enrichment refined to deduplicate on `ref_id` and cap enrichment injections (≤10) for controlled token budget.
3. Purpose Query Reranking enhanced with: prefix similarity boost, direct ID injection (including O/0 substitutions), and fallback keyword injection when primary token missing.
4. Where-Used Guarantee: If no line-level symbol evidence survives fusion, a targeted symbol_refs keyword fallback explicitly injects at least one line-anchored doc.
5. UI Fallback Heuristic: Ensures at least one `ui_paths` doc is introduced for UI scope queries or UI-marked mixed questions (MENU token / capitalized first token heuristic).

### New Evaluation Harness (`eval/run_random_eval.py`)
| Aspect | Details |
|--------|---------|
| Categories | purpose, flow, dependency, variable, where_used, ui |
| Sampling | Random program_ids, variable_ids, ui path_ids via lightweight `*` searches (capped) |
| Templates | Multiple phrasings per category for lexical variety |
| Scoring | Heuristic pass/fail based on presence of required index evidence (e.g., symbol_refs with line for where_used) |
| Outputs | `<base>.jsonl` (per-question), `<base>_summary.json` (aggregate percentages) |
| Metrics | latency per question, evidence count, overall & per-category pass % |
| Flags | `--limit`, `--seed`, `--no-diagram`, `--debug` |

Usage Example:
```
python eval/run_random_eval.py --programs 40 --variables 40 --uipaths 20 --out eval/out/random_eval --no-diagram
```

### Evaluation Pass Criteria (Current Heuristics)
| Category | Pass Condition |
|----------|----------------|
| purpose | Evidence contains program_meta OR copybook_meta |
| flow | Evidence contains program_flows OR calls OR program_deps |
| dependency | Evidence contains program_deps OR calls |
| variable | Evidence contains variable_usage OR symbol_refs |
| where_used | At least one symbol_refs evidence with a line/line_number |
| ui | Evidence contains ui_paths OR screen_nodes |

### Operational Guidance
1. Run a dry-run orchestrator batch in CI to validate scope + outline shape before credentialed eval.
2. Schedule at least a daily randomized evaluation; track pass % trends (alert if drop >5% absolute vs baseline).
3. Store JSONL outputs with date suffix for longitudinal regressions (optionally load into a small metrics dashboard).
4. Investigate sudden symbol_refs absence first (common cause of where-used dip) using debug logs.

### Planned Near-Term Enhancements
| Priority | Item | Rationale |
|----------|------|-----------|
| High | Add retry / exponential backoff wrapper (429/500/502/503) | Stabilize batch eval under transient Azure throttling |
| Medium | Structured evidence scoring (`evidence_score`) | Normalize cross-index weighting for RRF calibration |
| Medium | HTML report generator for evaluation runs | Easier human QA & sharing |
| Low | LRU embedding cache replacement + metrics | Provide hit rate observability |
| Low | JSON schema validation for orchestrator output | Early detection of shape regressions |

### Maintenance Checklist (Retrieval Layer)
| Task | Cadence | Tooling |
|------|---------|---------|
| Dry-run orchestrator smoke | Each commit touching retrieval | `--dry-run --debug` |
| Randomized evaluation | Daily | `eval/run_random_eval.py` |
| Coverage audit (vectors) | Daily (cron) | Existing backfill + coverage scripts |
| RRF calibration review | Monthly or after major index add | Manual + eval diffs |
| Purpose rerank regression check | After changes to prefix / injection logic | Targeted purpose question set |

### Known Limitations (Current State)
1. No adaptive per-scope k tuning (fixed `--vec-k` / `--kw-k` may over-fetch for sparse indexes).
2. Fusion does not yet weight evidence freshness or risk_score (future multi-factor scoring input).
3. Evaluation scoring is heuristic only (precision/recall of evidence relevance not yet measured).
4. Embedding cache is in-process only (no cross-run persistence / eviction metrics).

---

