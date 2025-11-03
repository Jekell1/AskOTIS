# Program Meta & Variable Usage Enhancements (Sept 2025)

This document records the new enrichment fields added to `new_cobol_program_meta` and the creation of the
`new_cobol_variable_usage` index that aggregates cross‑program variable usage statistics.

## 1. Program Meta Schema Additions

New fields (added in `create_call_indexes.py`):

| Field | Type | Purpose |
|-------|------|---------|
| `program_summary` | `Edm.String` | Concise deterministic textual summary (no LLM) describing call fan‑out/in, depth, role, cycle hint. |
| `program_role` | `Edm.String` | Heuristic classification (UI, DATA_ACCESS, API, BATCH, UTILITY, DISPATCH, LEAF, UNKNOWN). |
| `flow_graph_json` | `Edm.String` | Compact JSON preview of adjacency (top <=25 incoming / outgoing + degree counts). |
| `ui_flag` | `Edm.Boolean` | Boolean convenience flag for UI related programs (derived from name heuristics). |
| `input_screen_paths_json` | `Edm.String` | Placeholder (currently `[]`) for future traced UI/input screen navigation paths. |
| `reach_out_size` | `Edm.Int32` | Count of distinct programs reachable downstream (outgoing transitive). |
| `reach_in_size` | `Edm.Int32` | Count of distinct programs that can reach this program (incoming transitive). |
| `centrality_score` | `Edm.Double` | Normalized centrality approximation (degree + reach blend). |
| `risk_flag` | `Edm.Boolean` | Heuristic risk indicator (e.g., high fan-in & fan-out, external calls). |
| `dynamic_call_ratio` | `Edm.Double` | Fraction of calls that are dynamic/indirect (if detectable). |
| `external_callee_count` | `Edm.Int32` | Number of unique external (non‑indexed) callees referenced. |
| `external_callees` | `Collection(Edm.String)` | List of representative external callees. |
| `ui_path_participant` | `Edm.Boolean` | True if program participates in a UI interaction path (heuristic). |

Populated via updated `build_program_meta.py` (no extra API calls required). Coverage merge scripts continue to work unchanged.

## 2. Variable Usage Aggregation Index

Index name: `new_cobol_variable_usage`

One document per canonical variable (uppercase simple name) with counts and participating programs.

Fields:
* `variable_id` (key)
* `read_count`, `write_count`, `param_in_count`, `param_out_count`, `total_refs`
* `program_readers`, `program_writers`, `program_params_in`, `program_params_out`
* `sample_refs_json` (small JSON array with up to 12 sample reference objects)
* `usage_role` (DERIVED: one of READ_ONLY, WRITE_ONLY, READ_WRITE, PARAM_ONLY)
* `first_write_location` (string; first observed write site reference)
* `ingested_at`

Scripts:
* `create_variable_usage_index.py` – defines schema (no vectors yet)
* `build_variable_usage.py` – pages through `cobol-xrefs` and aggregates stats + role classification

## 3. System Overview Index

Index name: `new_cobol_system_overview`
Purpose: provide macro / portfolio level metrics enabling high‑level dashboards and health summaries.

Schema (see `create_system_overview_index.py`):
| Field | Type | Description |
|-------|------|-------------|
| `overview_id` | `Edm.String` (key) | Identifier (`latest` or timestamp snapshot). |
| `generated_at` | `Edm.String` | ISO UTC generation time. |
| `program_count` | `Edm.Int32` | Total number of program meta docs. |
| `ui_program_count` | `Edm.Int32` | Count of UI/UI-path programs. |
| `risk_program_count` | `Edm.Int32` | Count with `risk_flag` true. |
| `avg_coverage_pct` | `Edm.Double` | Synthetic/real coverage percent average. |
| `coverage_bands_json` | `Edm.String` | JSON mapping coverage buckets to counts. |
| `role_distribution_json` | `Edm.String` | JSON map of program_role -> count. |
| `top_central_programs_json` | `Edm.String` | JSON array top central programs. |
| `top_fan_out_programs_json` | `Edm.String` | JSON array highest downstream reach. |
| `top_fan_in_programs_json` | `Edm.String` | JSON array highest upstream reach. |
| `external_programs_json` | `Edm.String` | JSON array of distinct external callees. |
| `notes` | `Edm.String` | Optional commentary. |

Built via `build_system_overview.py` which:
1. Fetches all `new_cobol_program_meta` docs (paged) 
2. Computes distributions & top lists
3. Emits one doc with `overview_id='latest'` plus optional timestamp snapshot (`--snapshot`)
4. `--push` merges into index

Example build:
```powershell
python create_system_overview_index.py --overwrite
python build_system_overview.py --push --snapshot
```

Fetch the latest overview:
```json
{"search":"*","filter":"overview_id eq 'latest'","select":"program_count,ui_program_count,risk_program_count,avg_coverage_pct"}
```

## 4. Rebuild / Refresh Procedure

If you need to recreate these structures from scratch:

```powershell
# 1. Recreate program meta index with new fields (will delete if --overwrite specified)
python create_call_indexes.py --overwrite

# 2. Rebuild base program meta aggregation (call graph + heuristics)
python build_program_meta.py

# 3. (Optional) Merge coverage enrichment once coverage JSON is available
python merge_program_coverage.py --coverage JSONL/coverage_full.json

# 4. Create/update variable usage index (first time or after schema change)
python create_variable_usage_index.py --overwrite

# 5. Build variable usage aggregation
python build_variable_usage.py

# 6. Create/update system overview index
python create_system_overview_index.py --overwrite

# 7. Build overview (latest + timestamp snapshot)
python build_system_overview.py --push --snapshot

# 8. Run health check to confirm new fields detected
python index_health_check.py --prefix new_cobol_ --sample 2 --report-json index_health_after_full_update.json
```

## 5. Query Examples

Find candidate UI programs:
```json
{"search":"*","filter":"ui_flag eq true","select":"program_id,program_role,program_summary","top":30}
```

Top variables by write frequency (REST example body, sent to `new_cobol_variable_usage`):
```json
{"search":"*","orderby":"write_count desc","select":"variable_id,write_count,read_count,total_refs","top":20}
```

Variables only written (potential data sinks):
```json
{"search":"*","filter":"read_count eq 0 and write_count gt 0","select":"variable_id,write_count"}
```

Variables only read (potential externally supplied / configuration values):
```json
{"search":"*","filter":"write_count eq 0 and read_count gt 0","select":"variable_id,read_count"}
```

Programs writing a specific variable:
```json
{"search":"ACCT-BAL","queryType":"full","select":"program_writers,program_readers,sample_refs_json"}
```

Top central programs (from system overview doc):
```json
{"search":"*","filter":"overview_id eq 'latest'","select":"top_central_programs_json"}
```

## 6. Future Extensions
* Embed variable semantic meaning (add vector field + embedding pipeline)
* Populate `input_screen_paths_json` with actual traced UI navigation sequences
* Add `role_confidence` numeric field for downstream ranking
* Derive `is_chokepoint` boolean (high in_degree & out_degree) for dependency risk analysis
* Replace synthetic coverage metric with real test / code coverage integration
* Add temporal trend documents (weekly snapshots) for drift & risk monitoring

## 7. UI Paths (User Screen Flow) Index

Index: `new_cobol_ui_paths`
Purpose: Store enumerated heuristic navigation paths from high-level DISPATCH/UI roots to leaf UI programs for answering questions like "Show me a complete user screen flow from the main menu." Initial version is heuristic — no event/guard extraction yet.

Created by: `create_ui_paths_index.py`
Populated by: `build_ui_paths.py`

Schema fields:
| Field | Purpose |
|-------|---------|
| `path_id` | Stable hash of program sequence (first 40 hex chars). |
| `root_program_id` | Starting program (menu/dispatch/ui). |
| `leaf_program_id` | Final program in the enumerated sequence. |
| `program_sequence_json` | JSON array of ordered program IDs. |
| `screen_sequence_json` | Placeholder (currently identical to program sequence). |
| `length` | Number of nodes in the sequence. |
| `ui_program_count` | Count of UI-tagged programs in the sequence. |
| `branching_events_json` | Reserved for future menu choice / PF key annotations. |
| `guards_json` | Reserved for future condition/predicate capture. |
| `loop_collapsed` | True if a cycle was detected and truncated. |
| `score` | Heuristic path score (ui density * log length + depth factor). |
| `generated_at` | Timestamp of ingestion. |
| `notes` | Freeform annotation. |

Generation heuristics (v1):
1. Root candidates: `program_role in (DISPATCH, UI)` OR `ui_flag` OR `ui_path_participant`.
2. Roots ranked by `(reach_out_size, centrality_score)` descending.
3. Depth-first bounded traversal: `--max-depth` (default 10), branching limited (`--branching-limit` default 12), max paths per root (`--max-paths-per-root` default 200).
4. A child is followed only if it passes UI relevance: `ui_flag` OR `program_role == 'UI'` OR name contains one of `MENU, SCR, MAP, PANEL`.
5. Score = `(ui_program_count/length) * log(length+1) + 0.1 * (length/max_depth)`.
6. Loop detection: if a program reappears in the same path, mark `loop_collapsed` and stop expansion.

Example build/run:
```powershell
python create_ui_paths_index.py --overwrite
python build_ui_paths.py --push --limit-roots 5 --max-depth 12
```

Sample query: Top UI paths from a root program:
```json
{"search":"ROOTPROG1","searchFields":"root_program_id","orderby":"score desc","top":5,
 "select":"path_id,program_sequence_json,score,length,loop_collapsed"}
```

Limitations / Next steps:
* No differentiation between user-driven branch vs system call
* No explicit screen IDs yet (needs screen map extraction)
* No guard conditions or PF key mapping
* Potential explosion if call graph becomes very broad (heuristics cap branching)

Planned enhancements:
* Integrate screen artifact parsing to populate `screen_sequence_json`
* Add event extraction (`branching_events_json` populated from DISPLAY/ACCEPT patterns)
* Add path compression & uniqueness ranking (e.g., Jaccard similarity pruning)
* Optionally embed textual path representation for semantic lookups

## 8. Health Check Integration (Guidance)
To verify population of UI paths after generation:
1. Run a count check:
```powershell
python index_health_check.py --indexes new_cobol_ui_paths --sample 2 --report-json ui_paths_health.json
```
2. Ensure `doc_count` > 0 and spot-check a `program_sequence_json` contains plausible UI-named programs.
3. (Optional) Add a simple rule: if `doc_count == 0` but `new_cobol_program_meta` has > X UI candidates, flag a warning.

---
Document maintained alongside code changes – update this file if schema evolves.

