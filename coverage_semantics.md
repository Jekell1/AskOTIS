# Coverage & Embedding Semantics

This document standardizes how we define and measure *coverage* and the `has_vector` marker across indexes.

## 1. Coverage Dimensions
We track two independent dimensions:
1. Source/entity coverage: proportion of expected entities (programs, paragraphs, edges, etc.) that have a document in the index.
2. Embedding coverage: proportion of index documents whose *primary semantic vector field* is populated (non-empty float array of expected dimension).

## 2. Primary Vector Field
Each vector-capable index designates exactly one primary semantic vector field for coverage accounting:
- code-chunks / new_code_chunks: `text_vector`
- new_cobol_paragraphs: `para_vector`
- new_cobol_flow_edges_v2: `edge_vector`
- new_cobol_symbol_refs: `excerpt_vector`
- new_cobol_program_meta: `summary_vector`
- new_cobol_program_deps: `dependency_blob_vector`
- new_cobol_program_flows: `flow_vector`
- new_cobol_screen_nodes: `summary_vector`
- new_cobol_copybook_meta: `summary_vector`
- new_cobol_copybook_usage: `context_vector`
- new_cobol_data_items: `vector`
- new_cobol_ui_paths: `path_vector`
- new_cobol_calls: `snippet_vector`
- new-cobol-files: `contentVector` (may be dimension-adjusted or re-embedded; currently sparse)
- (Planned) new_cobol_variable_usage: `usage_summary_vector` (to be added)

Indexes without a primary vector field (menu_trees, name_aliases, program_copybook_edges, program_inventory, variable_usage before augmentation) are excluded from embedding coverage metrics unless we explicitly add a vector.

## 3. `has_vector` Semantics
`has_vector` MUST mean: "This document's primary vector field is present and non-empty (length == expected dimension)."

Rules:
- Set `has_vector=true` ONLY when the primary vector float array is populated.
- Do **not** pre-populate `has_vector` just because the index has a vector field schema.
- If a document loses its vector (rare), future maintenance scripts should set `has_vector=false`.
- For multi-vector experimental cases (none currently), choose one canonical semantic field; auxiliary vectors do not affect `has_vector`.

## 4. Migration Plan
1. Audit actual vector field population (sampling if index > 500k docs) to derive real coverage.
2. For each index, recompute `has_vector` by scanning documents and merging updates where the flag mismatches actual presence.
3. Persist a JSON snapshot: `embedding_coverage_baseline.json` (fields: index, total_docs, vector_docs, pct, ts, method = full|sampled|inferred).
4. Replace existing inflated markers (if any) BEFORE launching large embedding backfill waves to ensure accurate progress metrics.

## 5. Recompute Strategy
- Small/medium indexes (<= 300k docs): full scan in pages (e.g. 500) selecting primary key + `has_vector` + vector field.
- Large indexes (> 300k docs): two-phase
  a. Sample N pages (configurable) to estimate error rate; if mismatch rate > 0.5%, queue full recompute with throttling.
  b. Full recompute uses streaming pagination and conditional merges only for mismatches to minimize write ops.

## 6. Edge Cases & Validation
- Missing vector field schema: abort (config drift) and log.
- Non-retrievable vector field: warn; optional fallback (cannot recompute accurately) => skip index.
- Zero-length vector arrays (should not occur) treated as absent.
- Dimension mismatch: treat as absent (forces re-embed) and record anomaly count.

## 7. Reporting
`audit_vector_coverage.py` remains the public view: it counts docs where `has_vector` is true. Post-migration this equals actual primary vector coverage.
A future script `consolidated_index_status.py` will merge:
- Source coverage metric per index
- Embedding coverage (from `has_vector`)
- Vector field dimension & estimated storage footprint

## 8. Actions Completed / Pending
- (Done) Formalize semantics (this doc).
- (In Progress) Recompute for `new-cobol-files` to establish honest baseline before embedding wave.
- (Planned) Dimension strategy + large index recompute (symbol refs already 100%).

## 9. Decision Log
- We explicitly **do not** add `has_vector` to metadata-only indexes until a retrieval use case requires embeddings (keeps storage predictable).
- We will not overwrite `has_vector` to true for sparse indexes just to reach 100%; progress must reflect real embedding presence.

## 10. Next Steps
1. Run `recompute_has_vector.py` for `new-cobol-files` (expect current ~5%).
2. Sample 25 docs where `has_vector=false` to verify absence of vectors.
3. If acceptable, schedule embedding backfill for that index in Wave 3 (or earlier if needed for search quality).
4. Extend recompute to `new_cobol_calls` (currently ~48%).
