# Minimal Hybrid RAG Design

## Goals
Provide a lean pipeline to:
1. Embed a user question.
2. Perform hybrid retrieval (vector KNN + BM25) with optional filters (e.g., REGIONID == 'OK').
3. Return top N passages plus structured KPI / tenure metrics fields.
4. Build a grounded system prompt that:
   - Cites all passages with source handles.
   - Forbids fabrication of numbers (must quote or mark unknown).
   - Applies business rules (e.g., AutoPay is supporting unless delinquency weak).
5. Generate an answer with an LLM (Azure OpenAI or OpenAI fallback).

## Assumptions (Adjust if different)
- Azure AI Search index name: `business-passages` (placeholder).
- Key text field: `content`.
- Vector field: `content_vector` (dimension 1536, HNSW). Already populated.
- Filterable fields used here: `region_id` (Edm.String), maybe `program_id`, `category`.
- Structured numeric / KPI fields (example): `kpi_delinquency_rate`, `kpi_autopay_pct`, `kpi_avg_tenure_months`.
- Each document has an `id`, optional `source_path`.

## Environment Variables
```
AZURE_SEARCH_ENDPOINT=https://<yournamespace>.search.windows.net
AZURE_SEARCH_KEY=<adminOrQueryKey>
AZURE_OPENAI_ENDPOINT=https://<your-aoai>.openai.azure.com
AZURE_OPENAI_KEY=<key>
AZURE_OPENAI_EMBED_DEPLOYMENT=text-embedding-3-large
AZURE_OPENAI_CHAT_DEPLOYMENT=gpt-4o-mini
# Optional OpenAI fallback
OPENAI_API_KEY=<key>
OPENAI_EMBED_MODEL=text-embedding-3-large
OPENAI_CHAT_MODEL=gpt-4o-mini
```

## Retrieval Strategy
1. Embed query (float[1536]).
2. Vector search: top K_v (e.g., 15) on `content_vector`.
3. BM25 search: top K_t (e.g., 15) using `search_text` or `content` with `search=` parameter.
4. Merge / deduplicate by document `id` with simple RRF or score normalization.
5. Apply region filter by specifying `filter=region_id eq 'OK'` in each search (so both sub-results respect it).
6. After merge, keep top N_final (e.g., 12) passages.
7. Extract KPI fields if present; compile into a compact structured snippet (YAML or JSON section embedded in prompt).

## Ranking Fusion (RRF)
RRF score = sum(1 / (k + rank_i)), choose k=60 for stability.

## Prompt Construction
System prompt sections:
- Role / Behavior
- Business Rules
- KPIs (tabular / JSON)
- Retrieved Passages with citation tags `[P1]`, `[P2]`, ...
- Instructions about numeric grounding & citation style.

Business Rule Example:
```
Business Rules:
- Treat AutoPay enrollment as supporting evidence of payment stability unless the delinquency rate in KPIs is categorized as Weak (>= 5%).
- If any KPI needed for an answer is missing, explicitly state it was not found.
- Never invent numeric values; if absent, answer with: "Value not present in retrieved data".
```

## Output Format
Model answer should end with a Sources line listing citation tags in order of first use.

Example:
```
Answer body...

Sources: P2, P5, P1
```

## CLI Flow (ask_question.py)
- Parse args: `--question`, `--region OK`, `--top 12`.
- Load env & clients.
- Embed question.
- Run hybrid retrieval.
- Build prompt.
- Call chat completion (stream optional).
- Print answer + JSON block of sources.

## Files
- `embed.py` – embedding utility with Azure/OpenAI fallback.
- `retriever.py` – hybrid retrieval logic.
- `prompt_builder.py` – builds system prompt.
- `ask_question.py` – CLI orchestrator.
- `DESIGN.md` – this document.

## Testing
Smoke test: `python ask_question.py --question "What is delinquency trend?" --region OK`
Expect: prints answer and list of sources; no exceptions.

## Extensibility
Future improvements: caching embeddings, reciprocal rank fusion weighting, semantic filtering, summarization of long passages.
