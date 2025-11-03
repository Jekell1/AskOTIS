from __future__ import annotations
from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import JSONResponse, HTMLResponse, StreamingResponse
from pydantic import BaseModel
from .config import get_settings
from .retrieval import HybridRetriever, collapse_context
from .llm import AnswerLLM, redact_pii
import os, json, time, hashlib, asyncio, re
from collections import deque
from functools import lru_cache

app = FastAPI(title="COBOL RAG Chatbot", version="0.1.0")

CACHE: dict[str, dict] = {}
CACHE_ORDER: deque[str] = deque()
RATE_BUCKET: dict[str, list[float]] = {}

SEARCH_KEY = os.getenv("AZURE_SEARCH_KEY") or os.getenv("SEARCH_KEY")
retriever = HybridRetriever(search_key=SEARCH_KEY)
llm = AnswerLLM()

settings = get_settings()

# Simple LRU cache
def cache_set(key: str, value: dict):
    if key in CACHE:
        return
    CACHE[key] = value
    CACHE_ORDER.append(key)
    if len(CACHE_ORDER) > settings.cache_max_entries:
        old = CACHE_ORDER.popleft()
        CACHE.pop(old, None)

def cache_get(key: str):
    return CACHE.get(key)

# Token bucket per IP (queries per minute)
def rate_limit(ip: str):
    window = 60.0
    now = time.time()
    arr = RATE_BUCKET.setdefault(ip, [])
    # purge
    while arr and now - arr[0] > window:
        arr.pop(0)
    if len(arr) >= settings.rate_limit_qpm:
        return False
    arr.append(now)
    return True

# Basic context compression (remove duplicate content, short merging)
async def retrieve_pipeline(question: str, top: int):
    if settings.enable_vector:
        chunks = await retriever.hybrid_vector_search(question, top=top)
    else:
        chunks = retriever.search(question, top=top)
    # De-duplicate by chunk text
    seen_txt = set(); dedup = []
    for c in chunks:
        txt = c.get('_chunk','')
        key = txt.strip()[:160]
        if key in seen_txt:
            continue
        seen_txt.add(key)
        dedup.append(c)
    return dedup

class AskRequest(BaseModel):
    question: str
    top: int | None = 8

@app.get("/")
async def root():
    return HTMLResponse(open(os.path.join(os.path.dirname(__file__), 'frontend.html'), 'r', encoding='utf-8').read())

@app.post("/ask")
async def ask(req: AskRequest, request: Request):
    ip = request.client.host if request.client else 'local'
    if not rate_limit(ip):
        raise HTTPException(429, detail="Rate limit exceeded")
    q = req.question.strip()
    if not q:
        raise HTTPException(400, detail="Empty question")
    cache_key = hashlib.sha256(q.encode()).hexdigest()[:32]
    cached = cache_get(cache_key)
    if cached:
        return JSONResponse(cached)
    raw_chunks = await retrieve_pipeline(q, top=req.top or settings.max_chunks)
    context = collapse_context(raw_chunks, settings.max_context_chars)
    answer = llm.answer(q, context)
    cache_set(cache_key, answer)
    return JSONResponse(answer)

@app.get("/ask/stream")
async def ask_stream(question: str, request: Request):
    ip = request.client.host if request.client else 'local'
    if not rate_limit(ip):
        raise HTTPException(429, detail="Rate limit exceeded")
    q = question.strip()
    if not q:
        raise HTTPException(400, detail="Empty question")
    async def event_gen():
        yield "data: {\"stage\":\"retrieval\"}\n\n"
        chunks = await retrieve_pipeline(q, top=settings.max_chunks)
        context = collapse_context(chunks, settings.max_context_chars)
        yield "data: {\"stage\":\"context\",\"chunks\":%s}\n\n" % json.dumps([c['id'] for c in context])
        answer = llm.answer(q, context)
        # Split answer for streaming effect
        for part in re.split(r'(\n\n)', answer['answer']):
            if await request.is_disconnected():
                break
            yield "data: {\"stage\":\"answer\",\"content\":%s}\n\n" % json.dumps(part)
            await asyncio.sleep(0.05)
        yield "data: {\"stage\":\"done\"}\n\n"
    return StreamingResponse(event_gen(), media_type='text/event-stream')

@app.get("/health")
async def health():
    return {"status": "ok"}

@app.get("/metrics")
async def metrics():
    return {"cache_entries": len(CACHE_ORDER), "rate_buckets": len(RATE_BUCKET)}
