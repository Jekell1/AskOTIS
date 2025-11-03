# ✅ Azure OpenAI Integration Complete

## What Was Changed

The COBOL RAG chatbot now automatically loads Azure OpenAI configuration from your `local.settings.json` file. No more manual API key entry required!

## Configuration Loaded

From your `local.settings.json`:
- ✅ **AZURE_OPENAI_ENDPOINT**: `https://wrldopenai.openai.azure.com`
- ✅ **AZURE_OPENAI_KEY**: Configured (hidden for security)
- ✅ **AZURE_OPENAI_DEPLOYMENT**: `gpt-4.1`
- ✅ **SEARCH_ENDPOINT**: `https://az-use1-ai-search.search.windows.net`
- ✅ **SEARCH_KEY**: Configured
- ✅ **SEARCH_INDEX**: `operations-1749143776823`

## Key Changes Made

### 1. Configuration Loading
```python
def load_local_settings():
    """Load configuration from local.settings.json"""
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})
```

### 2. Azure OpenAI Client
- Switched from OpenAI to Azure OpenAI
- Uses your enterprise Azure OpenAI deployment (`gpt-4.1`)
- Automatic configuration from local.settings.json

### 3. Improved UI
- Sidebar now shows Azure OpenAI status automatically
- No more API key input forms
- Clear indication of LLM capabilities
- Configuration details in expandable section

### 4. Search Index Update
- Now uses the correct index: `operations-1749143776823`
- Matches your actual Azure Search configuration

## Current Status

🎉 **Your RAG chatbot is now fully configured and ready!**

**Access at**: http://localhost:8503

### What You'll See:
- ✅ **"Azure OpenAI configured from local.settings.json"**
- 🤖 **Azure OpenAI** (instead of pattern matching)
- 📋 **Configuration Details** showing your endpoint and deployment

## Benefits

1. **Enterprise Integration**: Uses your Azure OpenAI deployment
2. **No Manual Setup**: Automatic configuration loading
3. **Security**: No API keys entered in the UI
4. **Cost Efficient**: Uses your existing Azure credits/subscription
5. **Enhanced Performance**: GPT-4.1 deployment for better analysis

## Test the Intelligence

Try asking questions like:
- *"What does this COBOL program do?"*
- *"Explain the business logic in this code"*
- *"How does the data flow work?"*
- *"What programs are called by this code?"*

The system will now use your Azure OpenAI deployment to provide intelligent, context-aware analysis of your COBOL code instead of just keyword matching.

🚀 **Your enterprise COBOL RAG assistant is ready!**

---

## Flow Edges Hybrid Retrieval (Keyword + Vector)

The `cobol-flow-edges-v2` index now supports optional hybrid retrieval that merges keyword scoring (BM25) with vector similarity for better recall on semantic control‑flow questions.

### Enable Hybrid Mode
Set an environment variable before launching the chatbot or when using `retrieval_helpers.py`:

```pwsh
$env:FLOW_EDGES_VECTOR_HYBRID = "1"
python retrieval_helpers.py TIM360 --max-flow 150
```

Or force hybrid for a single CLI invocation:
```pwsh
python retrieval_helpers.py TIM360 --hybrid
```

### Bundle Fields Added
When hybrid is active the retrieval bundle includes:
- `flow_edges` (keyword set)
- `flow_edges_vector` (vector-only hits)
- `flow_edges_merged` (final merged / re-ranked list)
- `hybrid_used` (bool)
- `hybrid_scores` (score map)

### Tuning (Environment Variables)
```pwsh
$env:FLOW_EDGES_VECTOR_K = "60"              # neighbors in vector search
$env:FLOW_EDGES_VECTOR_WEIGHT = "1.2"        # weight multiplier for vector scores
$env:FLOW_EDGES_KEYWORD_WEIGHT = "0.9"       # weight multiplier for keyword scores
$env:FLOW_EDGES_VECTOR_REQUIRE_FLAG = "false" # include docs even if not yet embedded
$env:FLOW_EDGES_VECTOR_FIELD = "edge_vector"  # vector field name
$env:FLOW_EDGES_TEXT_FIELD = "edge_text"      # text field used for semantic embedding source
```

### Scoring Merge Logic
1. Collect `@search.score` from both result sets (fallback to positional decay if missing).
2. Apply weights: `keyword_score * FLOW_EDGES_KEYWORD_WEIGHT`, `vector_score * FLOW_EDGES_VECTOR_WEIGHT`.
3. Combine by taking the max per edge id.
4. Preserve / fill in `edge_text` from vector doc if missing in keyword hit.
5. Sort descending for the merged list.

### Requirements
- Index patched with `edge_vector` and `has_vector` fields.
- Vector embeddings populated (higher coverage => better hybrid quality).

### Progress Monitoring
Use:
```pwsh
python show_flow_vector_progress.py
python verify_flow_edges_vectors.py --sample 20
```

### Troubleshooting
- If `flow_edges_vector` is empty: ensure embeddings are running & vector field configured; verify index has a `vectorSearch` section.
- If SSL issues during embedding: temporary `--insecure` on `embed_flow_edges.py` while fixing corporate CA trust; do not leave it enabled long-term.

### Extending Hybrid
After validating benefit on flow edges you can mirror the same pattern for file or copybook indexes (patch retrieval code + add env gating variables).

---

## Fact + Paragraph Answer Synthesis

You now have an end-to-end deterministic QA pipeline over extracted posting facts and paragraph text.

### Script: `synthesize_answer.py`
Pipeline steps:
1. Hybrid retrieval over facts index (`cobol-facts-v3l` by default) using keyword + vector (RRF fusion).
2. Paragraph enrichment: resolves referenced COBOL paragraph names in each fact and pulls the paragraph text plus nearby context paragraphs (approx line-distance heuristic).
3. Structured narrative synthesis: clusters facts by `action_role` and emits an evidence-grounded answer with inline citations.

### Quick Start
```pwsh
# Example: explain reversal / payoff gating in APIPAY
python synthesize_answer.py `
    --program APIPAY `
    --question "How does APIPAY handle reversal payoff gating?" `
    --facts-index cobol-facts-v3l `
    --paragraph-index cobol-paragraphs-v3 `
    --top 25 `
    --max-answer-facts 12 `
    --output-json apipay_reversal_answer.json
```

### Output
1. Stdout: human-readable narrative with sections (one per action role) and citations like:
```
- Program APIPAY paragraph SKIP-PAYOFF-CALC performs reversal gl ... [fact:a1393270|para:SKIP-PAYOFF-CALC|lines 1246-1296]
        (paragraph excerpt ...)
```
2. Optional JSON (`--output-json`):
```jsonc
{
    "question": "…",
    "program": "APIPAY",
    "facts_used": [ { "fact_id": "…", "paragraphs": [...], "context_paragraphs": [...] } ],
    "answer_text": "…full narrative…"
}
```

### Key Arguments
| Argument | Purpose |
|----------|---------|
| `--question` | Natural language question |
| `--program` | Program id filter (improves precision) |
| `--facts-index` | Facts index (vector-enabled) |
| `--paragraph-index` | Paragraph index containing text field |
| `--top` | Depth per modality before fusion |
| `--rrf-k` | RRF damping constant (default 50) |
| `--max-answer-facts` | Cap facts included in narrative |
| `--context-window` | Approx neighbor line-distance for context paras |
| `--no-vector` / `--no-keyword` | Ablation switches for debugging |

### Environment Requirements
`SEARCH_ENDPOINT`, `SEARCH_KEY`, and Azure OpenAI embedding deployment env vars (model: `text-embedding-3-large` or configured override).

### Extending
To layer an LLM summarizer, feed `answer_text` + `facts_used` JSON into a GPT call with instructions to compress while preserving citations.

---

## Full File Index (Program-Level Fulltext)

To support deep variable provenance and multi‑paragraph context questions, a file-level index has been added.

### Index: `cobol-files-v1`
Fields (simplified):
- `file_id` (key)
- `program_name`
- `path`
- `length_lines`
- `file_hash`
- `ingest_timestamp`
- `has_vectors` (bool flag)
- `full_text` (searchable)
- `full_text_vector` (optional vector embedding of entire file)

### Create / Verify Index
```pwsh
python create_cobol_file_index.py --name cobol-files-v1
```
(Will no-op if already exists with same schema.)

### Ingest COBOL Source Files
```pwsh
# Dry run (no upload)
python ingest_cobol_files.py --roots SRC COBOL --index cobol-files-v1 --dry-run

# Actual ingest
python ingest_cobol_files.py --roots SRC COBOL --index cobol-files-v1

# Ingest + embed (large files may be truncated for embedding depending on model token limits)
python ingest_cobol_files.py --roots SRC COBOL --index cobol-files-v1 --embed --embedding-model text-embedding-3-large
```
Environment variables used (falls back to `local.settings.json` values if present):
- `AZURE_SEARCH_ENDPOINT`
- `AZURE_SEARCH_API_KEY`
- `AZURE_OPENAI_ENDPOINT`
- `AZURE_OPENAI_KEY`
- `AZURE_OPENAI_EMBEDDING_DEPLOYMENT` (or pass `--embedding-model`)

### Document Identity & De-Duping
A stable `file_id` is derived from the normalized program name; if multiple paths map to same program the last processed wins (logged). A content hash (`sha256`) is stored to detect drift.

### Embedding Strategy
Whole‑file embedding is optional; for very large programs consider future enhancement to chunk + aggregate. Current implementation embeds raw `full_text` (truncate if necessary) and sets `has_vectors=true` when successful.

### Usage in Retrieval
You can extend hybrid retrieval to include file-level semantic hits (e.g., fetch top N file docs then drill into paragraphs / facts for answer synthesis). This adds resilience when specific fact extraction misses latent business rules present only in narrative comments.

---

## Paragraph Coverage Audit

Script: `audit_paragraph_coverage.py`

Purpose: Verify which filesystem programs have paragraphs ingested (and with text) and optionally cross‑check facts coverage.

### Run
```pwsh
python audit_paragraph_coverage.py --paragraph-index cobol-paragraphs-v3 --facts-index cobol-facts-v3l --roots SRC COBOL --json-out paragraph_coverage.json
```

### Output Summary Fields
- `total_files`
- `filesystem_programs`
- `programs_in_paragraph_index`
- `programs_with_text`
- `missing_programs` (need ingestion)
- `present_no_text_programs` (schema missing text field at ingestion time)
- `facts_without_paragraphs`
- `paragraphs_without_facts`

Use this before large question batches to ensure adequate source coverage.

---
