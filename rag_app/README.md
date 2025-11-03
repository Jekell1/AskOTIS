# COBOL RAG Chatbot

A minimal but structured Retrieval-Augmented Generation (RAG) chatbot over your COBOL analysis indexes.

## Features
- FastAPI backend with `/ask` endpoint.
- Hybrid retrieval across program meta, paragraphs, and (optionally) data items.
- Context collapsing with length-aware reranking.
- Structured prompt with professional answer formatting & citations table.
- Frontend single-page chat UI (ChatGPT-style) with sources table.
- Retry logic for LLM calls and basic guardrail instructions.

## Architecture
```
User -> FastAPI (/ask) -> HybridRetriever -> Azure Cognitive Search
                                 |-> collapse_context -> AnswerLLM (OpenAI/Azure OpenAI)
                                 -> JSON response (answer + cited chunks)
```

## Environment Variables
| Variable | Purpose |
|----------|---------|
| `AZURE_SEARCH_ENDPOINT` | Your Azure Cognitive Search endpoint (https://xxx.search.windows.net) |
| `AZURE_SEARCH_KEY` | Admin/query key (omit if using managed identity) |
| `AZURE_SEARCH_API_VERSION` | API version (default 2025-08-01-preview) |
| `AZURE_OPENAI_ENDPOINT` | (Optional) Azure OpenAI endpoint |
| `AZURE_OPENAI_DEPLOYMENT` | (Optional) Deployed model name (e.g., gpt-4o-mini) |
| `OPENAI_API_KEY` | Fallback non-Azure OpenAI key |
| `USE_MANAGED_IDENTITY` | `true` to use MSI via DefaultAzureCredential |

## Install
```bash
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\activate
pip install -r rag_app/requirements.txt
```

## Run
```bash
uvicorn rag_app.app:app --reload --port 8000
```
Then open http://localhost:8000

## Ask Endpoint Example
```bash
curl -X POST http://localhost:8000/ask -H 'Content-Type: application/json' -d '{"question":"Describe GLMENU navigation."}'
```

## New Advanced Features
- Vector + keyword hybrid with Reciprocal Rank Fusion (RRF)
- Optional streaming responses (`/ask/stream`) via SSE
- Basic PII redaction & sensitive query classification
- In-memory LRU answer cache
- Per-IP rate limiting (queries per minute)
- Context deduplication & basic compression
- Evaluation harness (`eval_harness.py`) for latency & size metrics
- Metrics endpoint `/metrics`

## Additional Environment Variables
| Variable | Default | Description |
|----------|---------|-------------|
| `ENABLE_VECTOR` | true | Enable vector retrieval + fusion |
| `ENABLE_SEMANTIC` | false | (Reserved) toggle semantic rank usage |
| `AZURE_OPENAI_EMBEDDING_DEPLOYMENT` | (none) | Embedding deployment for Azure OpenAI |
| `OPENAI_EMBEDDING_MODEL` | text-embedding-3-small | Public OpenAI embedding model fallback |
| `RRF_K` | 60 | RRF denominator constant |
| `CACHE_MAX_ENTRIES` | 200 | LRU cache size |
| `RATE_LIMIT_QPM` | 120 | Queries per minute per IP |
| `ENABLE_STREAMING` | true | Enable streaming endpoint & UI toggle |
| `PII_REDACT` | true | Redact simple PII patterns |

## Streaming Usage
Open browser UI and ensure "Streaming" checkbox is enabled, or:
```bash
curl -N "http://localhost:8000/ask/stream?question=Outline%20GLMENU%20navigation"
```

## Metrics
```bash
curl http://localhost:8000/metrics
```

## Evaluation Harness
Prepare a file `eval_questions.json`:
```json
[
  "Describe GLMENU navigation.",
  "What are top risk programs?",
  "Explain data structure for customer address." 
]
```
Run:
```bash
python -m rag_app.eval_harness --questions eval_questions.json --endpoint http://localhost:8000/ask
```

## Next Enhancements (Suggested)
1. Add vector similarity calls using `search_client.search` with `vector_queries` once vector profile retrieval examples finalized.
2. Implement semantic ranker toggle and fusion (Reciprocal Rank Fusion) between indexes.
3. Add chunk deduplication + adaptive compression (summaries when context budget exceeded).
4. Add streaming responses using Server Sent Events (SSE) or WebSockets.
5. Integrate guardrails (PII redaction + query classification) pre-LLM.
6. Add evaluation harness (faithfulness, groundedness scoring) for regression testing.

## Security Notes
- Prefer Managed Identity in Azure hosting scenarios and remove key usage.
- Avoid logging full excerpts with sensitive content.
- Consider Key Vault for secret management.

## License
Internal / Example scaffold.
