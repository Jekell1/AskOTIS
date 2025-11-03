# Minimal Hybrid RAG Usage

## 1. Prerequisites
- Python 3.10+
- An Azure AI Search index populated with text + vector field `content_vector`.
- Azure OpenAI (preferred) or OpenAI API credentials.

## 2. Environment Variables
```
AZURE_SEARCH_ENDPOINT=https://<search>.search.windows.net
AZURE_SEARCH_KEY=<key>
AZURE_OPENAI_ENDPOINT=https://<aoai>.openai.azure.com
AZURE_OPENAI_KEY=<key>
AZURE_OPENAI_EMBED_DEPLOYMENT=text-embedding-3-large
AZURE_OPENAI_CHAT_DEPLOYMENT=gpt-4o-mini
# Optional fallback
OPENAI_API_KEY=<key>
OPENAI_EMBED_MODEL=text-embedding-3-large
OPENAI_CHAT_MODEL=gpt-4o-mini
```

## 3. Install Dependencies
```
pip install -r requirements_rag.txt
```

## 4. Ask a Question
```
python rag_minimal/ask_question.py --question "What is delinquency trend?" --region OK --index business-passages
```

## 5. Output
- Printed answer citing passages like [P2].
- JSON block with sources (ids, citation tags, scores) after a separator.

## 6. Extending
- Add more KPI fields: update `retriever.py` select list and `prompt_builder.py`.
- Change fusion: adjust RRF k constant or implement weighted sum.
- Add caching: wrap `embedder.embed` with an LRU keyed by text hash.

## 7. Troubleshooting
- 401 / 403: verify keys & endpoint region.
- Empty result set: ensure index name matches and region filter values exist.
- Missing KPIs: confirm fields are defined as retrievable in index schema.
