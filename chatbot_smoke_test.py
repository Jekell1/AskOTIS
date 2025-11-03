#!/usr/bin/env python3
"""Minimal COBOL RAG smoke test.

Purpose:
  - Embed a user query using Azure OpenAI embedding deployment (same one used for index vectors)
  - Perform a vector search (and optional text fallback) against primary index (code-chunks)
  - Construct a compact prompt with top N snippets
  - Call chat completion (Azure OpenAI) and print answer + cited chunk_ids

Environment requirements (local.settings.json Values or env vars):
  AZURE_SEARCH_ENDPOINT
  AZURE_SEARCH_KEY
  AZURE_OPENAI_ENDPOINT (or OPENAI_ENDPOINT)
  AZURE_OPENAI_KEY (or OPENAI_API_KEY)
  EMBEDDING_DEPLOYMENT (e.g. text-embedding-3-large)
  CHAT_DEPLOYMENT (e.g. gpt-4o-mini)

Usage:
  python chatbot_smoke_test.py --query "How is ACUMEM program called?" --k 6
"""
import os, sys, json, argparse, time, math
import requests

API_VERSION_SEARCH = "2024-07-01"
API_VERSION_OPENAI = "2024-08-01-preview"

DEFAULT_INDEX = "code-chunks"
VECTOR_FIELD = "text_vector"
TEXT_FIELD = "text"
KEY_FIELD = "chunk_id"


def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    cfg = {
        'search_endpoint': first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT'),
        'search_key': first('AZURE_SEARCH_KEY','SEARCH_KEY'),
        'aoai_endpoint': first('AZURE_OPENAI_ENDPOINT','OPENAI_ENDPOINT'),
        'aoai_key': first('AZURE_OPENAI_KEY','OPENAI_API_KEY'),
        'embedding_deployment': first('EMBEDDING_DEPLOYMENT') or 'text-embedding-3-large',
        'chat_deployment': first('CHAT_DEPLOYMENT') or 'gpt-4o-mini'
    }
    missing = [k for k,v in cfg.items() if not v]
    if missing:
        print(f"Missing required configuration values: {missing}", file=sys.stderr)
        sys.exit(1)
    # normalize endpoints (strip trailing slash)
    cfg['search_endpoint'] = cfg['search_endpoint'].rstrip('/')
    cfg['aoai_endpoint'] = cfg['aoai_endpoint'].rstrip('/')
    return cfg


def embed(cfg, text: str):
    url = f"{cfg['aoai_endpoint']}/openai/deployments/{cfg['embedding_deployment']}/embeddings?api-version={API_VERSION_OPENAI}"
    body = {"input": text}
    r = requests.post(url, headers={'api-key':cfg['aoai_key'],'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding failed {r.status_code}: {r.text[:160]}")
    data = r.json()
    return data['data'][0]['embedding']


def vector_search(cfg, index: str, embedding, k: int):
    url = f"{cfg['search_endpoint']}/indexes/{index}/docs/search?api-version={API_VERSION_SEARCH}"
    body = {
        "vectorQueries": [
            {
                "kind": "vector",
                "vector": embedding,
                "fields": VECTOR_FIELD,
                "k": k
            }
        ],
        # small semantic / keyword fallback could be added, for now just star search to allow scoringProfile combos later
        "search": "*",
        "select": f"{KEY_FIELD},{TEXT_FIELD},path,program_id,scope,name,start_line,end_line",
        "top": k
    }
    r = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Search failed {r.status_code}: {r.text[:160]}")
    return r.json().get('value', [])


def build_prompt(query: str, docs):
    parts = [
        "You are a COBOL code expert. Answer the user question ONLY using the provided source snippets.",
        "Cite chunk_id values in parentheses when referencing code (e.g., (chunk:FILE123:PARA:10-20)).",
        "If insufficient info, say you need more context instead of hallucinating.",
        "\nUser Question: " + query,
        "\nSources:" ]
    for i,d in enumerate(docs,1):
        chunk_id = d.get(KEY_FIELD,'?')
        text = d.get(TEXT_FIELD,'')
        snippet = text[:600].replace('\n',' ')  # trim for prompt
        parts.append(f"[{i}] chunk_id={chunk_id} : {snippet}")
    parts.append("\nProvide answer now:")
    return "\n".join(parts)


def chat(cfg, prompt: str):
    url = f"{cfg['aoai_endpoint']}/openai/deployments/{cfg['chat_deployment']}/chat/completions?api-version={API_VERSION_OPENAI}"
    body = {
        "model": cfg['chat_deployment'],
        "messages": [
            {"role":"system","content":"You are a concise, accurate COBOL code assistant."},
            {"role":"user","content": prompt}
        ],
        "temperature": 0.2,
        "max_tokens": 900
    }
    r = requests.post(url, headers={'api-key':cfg['aoai_key'],'Content-Type':'application/json'}, json=body, timeout=120)
    if r.status_code != 200:
        raise RuntimeError(f"Chat failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    return data['choices'][0]['message']['content']


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--query', required=True, help='User question about COBOL code')
    ap.add_argument('--k', type=int, default=6, help='Top vector results')
    ap.add_argument('--index', default=DEFAULT_INDEX)
    args = ap.parse_args()
    cfg = load_settings()

    print(f"Query: {args.query}")
    embedding = embed(cfg, args.query)
    print(f"Embedding length={len(embedding)}")

    docs = vector_search(cfg, args.index, embedding, args.k)
    print(f"Retrieved {len(docs)} docs")
    if not docs:
        print("No docs retrieved; aborting.")
        return

    prompt = build_prompt(args.query, docs)
    print("--- Prompt (truncated) ---")
    print(prompt[:1200])
    print("--- End Prompt Preview ---")

    answer = chat(cfg, prompt)
    print("\n=== Chatbot Answer ===\n")
    print(answer)
    print("\n=== Source Chunk IDs ===")
    for d in docs:
        print(d.get(KEY_FIELD))

if __name__ == '__main__':
    main()
