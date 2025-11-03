# COBOL Call Graph Workflow

This directory now contains a lightweight pipeline for extracting, indexing, aggregating and querying COBOL program call relationships using Azure AI Search.

## Indexes

1. `new_cobol_calls`  (edge-level call occurrences + snippet vectors)
2. `new_cobol_program_meta` (per-program aggregate metadata)

Both indexes are created by `create_call_indexes.py`.

### Calls Index Fields (key ones)
- `call_id` (stable hash of file path + called program + line + column + occurrence)
- `caller_program`, `callee_program`
- `line`, `col`, `occurrence` (disambiguate multiple calls on same line)
- `call_type` (`static` or `dynamic`) + `is_dynamic`
- `snippet` (trimmed source line) + `snippet_vector` (3072-dim)
- `has_vector` boolean
- `call_hash` (for duplicate detection)
- `ingested_at` timestamp

### Program Meta Index Fields
- `program_id` (uppercased program name)
- `outgoing_count`, `incoming_count`
- `unique_callees`, `unique_callers`
- `has_cycles` (simple DFS detection)
- `call_depth_score` (bounded max depth from program following call edges)
- `sample_call_lines` (up to 10 representative line numbers)
- `ingested_at` (currently `None` placeholder)

## Environment / Credentials
Scripts auto‑load values from `local.settings.json` (`Values` section) for any of:
`SEARCH_ENDPOINT`, `SEARCH_KEY`, `AZURE_SEARCH_ENDPOINT`, `AZURE_SEARCH_KEY`.
You can still override with `--endpoint` / `--key` CLI flags.

## Workflow Steps

1. **Create / Recreate Indexes**
   ```pwsh
   python create_call_indexes.py --overwrite
   ```
   Use `--overwrite` when schema changes (e.g. newly added `col`/`occurrence`).

2. **Ingest Call Edges**
   ```pwsh
   $env:COBOL_SOURCE_ROOT="cobol_src"  # or your path
   python ingest_cobol_calls.py --stream-batch-size 2000 --embed-batch-size 64
   ```
   Flags:
   - `--dry-run` preview embedding + sample docs
   - `--limit N` restrict for debugging
   - `--recreate-index` one-off drop+create based on current server schema

3. **Aggregate Program Metadata**
   After edges are ingested:
   ```pwsh
   python build_program_meta.py
   ```
   This fetches all edges via search pagination and uploads program docs.

4. **Query the Graph**
   ```pwsh
   # Who calls a program
   python call_graph_queries.py who-calls APIPAY

   # Callees of a program
   python call_graph_queries.py callees APIPAY

   # Forward call chain (depth 5)
   python call_graph_queries.py chain APIPAY --depth 5

   # Reverse call chain (who eventually leads to)
   python call_graph_queries.py rchain APIPAY --depth 4

   # Find calls at a specific line
   python call_graph_queries.py search-line APIPAY:1200
   ```

## Dynamic vs Static Detection
A call line is considered `static` if it contains a quoted literal after `CALL`, otherwise `dynamic`. This heuristic can be refined later (e.g. resolving identifiers defined via COPY or data names).

## Embeddings
`snippet_vector` uses the same 3072-dimension embedding model configuration as the main COBOL file index. Adjust `VECTOR_DIM` in `create_call_indexes.py` if you change embedding providers.

## Extensibility Ideas
- Add paragraph context / caller paragraph ID when paragraph extraction is wired in.
- Enhance dynamic call resolution with data division tracing.
- Add cycle path enumeration (currently only boolean flag).
- Persist ingestion timestamps for program meta (set `ingested_at`).
- Add graph export to DOT / JSON for visualization.

## Troubleshooting
- If ingestion raises vector dimension mismatch: confirm the embedding deployment dimension matches `VECTOR_DIM`.
- If index field errors occur after schema edits, re-run creation with `--overwrite` then re‑ingest.
- Large corpora: consider using an Indexer or exporting call edges to blob for bulk load if search pagination becomes slow.

## License
Internal use only (adjust as needed).
