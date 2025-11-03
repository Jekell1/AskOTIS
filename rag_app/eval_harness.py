"""Simple evaluation harness for the RAG system.

Usage:
  python -m rag_app.eval_harness --questions eval_questions.json --endpoint http://localhost:8000/ask

Produces a JSONL with per-question latency and basic length metrics.
"""
from __future__ import annotations
import argparse, json, time, httpx, statistics, pathlib

def run(questions, endpoint: str, out: str):
    rows = []
    with httpx.Client(timeout=30) as client:
        for q in questions:
            start = time.time()
            r = client.post(endpoint, json={"question": q})
            latency = time.time() - start
            if r.status_code != 200:
                rows.append({"question": q, "error": r.status_code, "latency": latency})
                continue
            js = r.json()
            answer_len = len(js.get('answer',''))
            ctx_chars = sum(len(c.get('content','')) for c in js.get('chunks',[]))
            rows.append({
                "question": q,
                "latency_sec": latency,
                "answer_len": answer_len,
                "context_chars": ctx_chars,
                "chunk_count": len(js.get('chunks',[]))
            })
    with open(out,'w',encoding='utf-8') as f:
        for r in rows:
            f.write(json.dumps(r)+"\n")
    # summary
    lats = [r['latency_sec'] for r in rows if 'latency_sec' in r]
    print(f"Completed {len(rows)} questions. p50={statistics.median(lats):.2f}s max={max(lats):.2f}s")

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('--questions', required=True, help='JSON array file of questions')
    ap.add_argument('--endpoint', default='http://localhost:8000/ask')
    ap.add_argument('--out', default='eval_results.jsonl')
    args = ap.parse_args()
    questions = json.loads(open(args.questions,'r',encoding='utf-8').read())
    run(questions, args.endpoint, args.out)
