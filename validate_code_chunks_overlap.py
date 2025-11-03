"""Validate overlapped code chunk recall and continuity.

Given one or more natural language queries (typically multi-step 'how' /
procedural questions), this script fetches top code chunk hits from the
new_code_chunks index and inspects their adjacent overlapped windows to
verify logical continuity.

Outputs per query:
  - Top hits with basic metadata
  - Neighbor (previous/next) window presence & line span relationship
  - Observed stride (delta of start_line) vs recorded stride field
  - Overlap size (lines) for forward neighbor
  - A short merged excerpt snippet (first and last few lines) if --show-text

Assumptions:
  Index new_code_chunks has fields: chunk_id (key), text, program_id, path,
  window_index (Int32), start_line (Int32), end_line (Int32), window_size (Int32),
  stride (Int32), tokens_estimate (Int32).

Usage examples (PowerShell):
  python .\validate_code_chunks_overlap.py --query "how interest is calculated" --top 5 --neighbors 1 --show-text
  python .\validate_code_chunks_overlap.py --query-file queries.txt --top 6 --neighbors 2

Environment discovery: reads SEARCH_ENDPOINT / SEARCH_KEY from env or from
local.settings.json (Values.SEARCH_ENDPOINT, Values.SEARCH_KEY) mirroring
other ingestion scripts.
"""
from __future__ import annotations
import os, json, argparse, math
from dataclasses import dataclass
from typing import List, Optional, Dict, Any

from azure.core.credentials import AzureKeyCredential
from azure.search.documents import SearchClient

API_VERSION = "2024-07-01"
INDEX_NAME = "new_code_chunks"


def load_settings():
    try:
        with open('local.settings.json','r', encoding='utf-8') as f:
            data = json.load(f)
        vals = data.get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


@dataclass
class Chunk:
    chunk_id: str
    program_id: str
    path: Optional[str]
    window_index: Optional[int]
    start_line: Optional[int]
    end_line: Optional[int]
    window_size: Optional[int]
    stride: Optional[int]
    text: Optional[str]
    score: float

    @property
    def line_span(self) -> str:
        if self.start_line is None or self.end_line is None:
            return "?"
        return f"{self.start_line}-{self.end_line}"


def make_client(endpoint: str, key: str) -> SearchClient:
    return SearchClient(endpoint=endpoint, index_name=INDEX_NAME, credential=AzureKeyCredential(key), api_version=API_VERSION)


def search_chunks(client: SearchClient, query: str, top: int) -> List[Chunk]:
    results: List[Chunk] = []
    try:
        resp = client.search(query, top=top, query_type="simple", include_total_count=False)
        for r in resp:
            score = float(getattr(r, '@search.score', 0.0) or 0.0)
            results.append(Chunk(
                chunk_id=r.get('chunk_id'),
                program_id=r.get('program_id') or r.get('file_id') or '',
                path=r.get('path'),
                window_index=r.get('window_index'),
                start_line=r.get('start_line'),
                end_line=r.get('end_line'),
                window_size=r.get('window_size'),
                stride=r.get('stride'),
                text=r.get('text'),
                score=score
            ))
    except Exception as e:
        print(f"[WARN] Search failed for query '{query}': {e}")
    return results


def fetch_neighbor(client: SearchClient, program_id: str, window_index: int) -> Optional[Chunk]:
    # Use wildcard search with OData filter on window_index & program_id
    # Escape single quotes in program_id if any
    safe_pid = program_id.replace("'", "''")
    filt = f"program_id eq '{safe_pid}' and window_index eq {window_index}"
    try:
        resp = client.search("*", top=1, query_type="simple", filter=filt, include_total_count=False)
        for r in resp:
            score = float(getattr(r, '@search.score', 0.0) or 0.0)
            return Chunk(
                chunk_id=r.get('chunk_id'),
                program_id=r.get('program_id') or r.get('file_id') or '',
                path=r.get('path'),
                window_index=r.get('window_index'),
                start_line=r.get('start_line'),
                end_line=r.get('end_line'),
                window_size=r.get('window_size'),
                stride=r.get('stride'),
                text=r.get('text'),
                score=score
            )
    except Exception:
        return None
    return None


def summarize_overlap(base: Chunk, nxt: Optional[Chunk]) -> Dict[str, Any]:
    if not base or not nxt:
        return {"has_next": bool(nxt), "observed_stride": None, "overlap_lines": None, "continuous": False}
    observed_stride = None
    overlap = None
    continuous = False
    if base.start_line is not None and nxt.start_line is not None:
        observed_stride = nxt.start_line - base.start_line
    if base.end_line is not None and nxt.start_line is not None:
        overlap = base.end_line - nxt.start_line + 1  # positive means overlapping
        continuous = overlap is not None and overlap >= 0
    return {
        "has_next": True,
        "observed_stride": observed_stride,
        "recorded_stride": base.stride,
        "overlap_lines": overlap,
        "continuous": continuous
    }


def format_excerpt(text: Optional[str], max_chars: int = 240) -> str:
    if not text:
        return ''
    t = text.strip().replace('\r','')
    if len(t) <= max_chars:
        return t
    head = t[: max_chars // 2].rsplit('\n', 1)[0]
    tail = t[-max_chars // 2 :].split('\n', 1)[-1]
    return head + "\n...\n" + tail


def main():
    p = argparse.ArgumentParser(description="Validate overlap recall characteristics for new_code_chunks index.")
    p.add_argument('--query', action='append', help='Query string (repeatable). If omitted, --query-file required.')
    p.add_argument('--query-file', type=str, help='Path to file containing one query per line.')
    p.add_argument('--top', type=int, default=6, help='Top code chunks to inspect per query.')
    p.add_argument('--neighbors', type=int, default=1, help='How many forward neighbor windows to inspect (currently only uses +1).')
    p.add_argument('--show-text', action='store_true', help='Show abbreviated chunk text excerpts.')
    p.add_argument('--json', action='store_true', help='Emit machine-readable JSON summary instead of human format.')
    args = p.parse_args()

    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT / SEARCH_KEY (set env or local.settings.json).')
    client = make_client(endpoint, key)

    queries: List[str] = []
    if args.query:
        queries.extend(args.query)
    if args.query_file:
        try:
            with open(args.query_file,'r', encoding='utf-8') as f:
                for line in f:
                    line=line.strip()
                    if line:
                        queries.append(line)
        except FileNotFoundError:
            raise SystemExit(f"Query file not found: {args.query_file}")
    if not queries:
        queries = ["how is process flow determined", "how are totals calculated"]  # reasonable defaults

    all_summary = []
    for q in queries:
        hits = search_chunks(client, q, args.top)
        q_summary = {
            'query': q,
            'hit_count': len(hits),
            'hits': []
        }
        for h in hits:
            forward = fetch_neighbor(client, h.program_id, (h.window_index or 0) + 1) if h.window_index is not None else None
            overlap_info = summarize_overlap(h, forward)
            entry = {
                'chunk_id': h.chunk_id,
                'program_id': h.program_id,
                'window_index': h.window_index,
                'lines': h.line_span,
                'score': round(h.score,4),
                **overlap_info
            }
            if args.show_text:
                entry['excerpt'] = format_excerpt(h.text, 300)
                if forward and forward.text:
                    entry['next_excerpt'] = format_excerpt(forward.text, 180)
            q_summary['hits'].append(entry)
        all_summary.append(q_summary)

    if args.json:
        print(json.dumps({'queries': all_summary}, indent=2))
        return

    # Human readable output
    for qd in all_summary:
        print('='*90)
        print(f"Query: {qd['query']}")
        print(f"Hits: {qd['hit_count']}")
        for h in qd['hits']:
            cont_flag = 'YES' if h.get('continuous') else 'NO'
            print(f"  - idx {h.get('window_index')} lines {h.get('lines')} score={h.get('score')} stride(obs/rec)={h.get('observed_stride')}/{h.get('recorded_stride')} overlap={h.get('overlap_lines')} cont={cont_flag}")
            if args.show_text:
                if 'excerpt' in h:
                    print("    excerpt:\n" + "\n".join('      '+ln for ln in h['excerpt'].splitlines()[:12]))
                if 'next_excerpt' in h:
                    print("    next:+1 excerpt:\n" + "\n".join('      '+ln for ln in h['next_excerpt'].splitlines()[:8]))
        # Aggregate metrics
        overlaps = [h.get('overlap_lines') for h in qd['hits'] if h.get('overlap_lines') is not None]
        if overlaps:
            avg_overlap = sum(overlaps)/len(overlaps)
            print(f"  Avg overlap lines (forward window): {avg_overlap:.1f}")
    print('='*90)
    print('Done.')


if __name__ == '__main__':
    main()
