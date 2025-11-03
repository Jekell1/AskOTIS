"""Ingest a single sample COBOL file document into the new-cobol-files index.

Steps:
 1. Autoload env (Search + optional Azure OpenAI embedding)
 2. Prepare a representative payload (fields align with schema)
 3. Generate embedding vector (1536 floats):
      - If Azure OpenAI env vars present -> call embeddings endpoint
      - Else fallback to deterministic pseudo-vector so pipeline can be tested
 4. POST to /docs/index
 5. Optionally query back (future script) using vectorQueries

Usage:
  python ingest_sample_cobol_file.py --program SAMPLE1
"""
from __future__ import annotations
import os, sys, json, argparse, hashlib, struct, requests, time
from typing import List
import env_autoload

INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
VECTOR_DIMS = 3072

# -------------- Embedding Helpers --------------

def have_azure_openai() -> bool:
    return all(os.getenv(k) for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'])


def embed_text_azure_openai(text: str) -> List[float]:
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_OPENAI_KEY')
    dep = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    url = f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    body = {"input": text}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding request failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    vec = data['data'][0]['embedding']
    if len(vec) != VECTOR_DIMS:
        raise ValueError(f"Embedding dimension mismatch: expected {VECTOR_DIMS} got {len(vec)}")
    return vec


def embed_text_fallback(text: str) -> List[float]:
    # Deterministic pseudo-random based on SHA256, repeated to fill VECTOR_DIMS
    seed = hashlib.sha256(text.encode('utf-8')).digest()
    floats: List[float] = []
    while len(floats) < VECTOR_DIMS:
        for i in range(0, len(seed), 4):
            if len(floats) >= VECTOR_DIMS:
                break
            chunk = seed[i:i+4]
            if len(chunk) < 4:
                chunk = chunk.ljust(4,b'\0')
            val = struct.unpack('!I', chunk)[0]
            floats.append(((val % 1000) / 1000.0) - 0.5)  # center roughly around 0
        seed = hashlib.sha256(seed).digest()
    return floats


def generate_vector(text: str) -> List[float]:
    if have_azure_openai():
        try:
            return embed_text_azure_openai(text)
        except Exception as e:
            print(f"WARNING: Azure OpenAI embedding failed, falling back to pseudo-vector: {e}")
    return embed_text_fallback(text)

# -------------- Ingestion --------------

def build_document(program: str) -> dict:
    content = f"IDENTIFICATION DIVISION. PROGRAM-ID. {program}.\n* Sample COBOL body for {program}.\nPROCEDURE DIVISION.\n    DISPLAY 'HELLO FROM {program}'.\n    GOBACK."  # minimal
    summary = f"Program {program} sample summary for ingestion pipeline validation."
    paragraphs = ["MAIN-PARA"]
    defines = ["WS-VAR1","WS-VAR2"]
    refs = ["WS-VAR2"]
    copybooks = ["COPYA"]
    has_sql = False
    has_cics = False
    has_screens = False
    vector = generate_vector(summary + '\n' + content)
    doc = {
        "@search.action": "mergeOrUpload",
        "id": f"{program}-file-1",
        "programId": program,
        "kind": "program",
        "path": f"/src/{program}.cbl",
        "language": "COBOL",
        "sloc": 12,
        "hasSQL": has_sql,
        "hasCICS": has_cics,
        "hasScreens": has_screens,
        "paragraphs": paragraphs,
        "definesNames": defines,
        "referencesNames": refs,
        "copybooksUsed": copybooks,
        "summary": summary,
        "content": content,
        "contentVector": vector,
        "commit": "sample-ingest",
        "updatedAt": time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
    }
    return doc


def ingest(doc: dict):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    payload = {"value": [doc]}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, data=json.dumps(payload).encode('utf-8'))
    if r.status_code >= 300:
        raise RuntimeError(f"Ingest failed {r.status_code}: {r.text[:400]}")
    return r.json()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--program', required=True, help='Program ID to simulate')
    args = ap.parse_args()
    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    doc = build_document(args.program)
    print(f"Vector length: {len(doc['contentVector'])}")
    res = ingest(doc)
    print('Ingest response:', json.dumps(res, indent=2)[:800])
    print('Done.')

if __name__ == '__main__':
    main()
