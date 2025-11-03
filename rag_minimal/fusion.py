"""Reciprocal Rank Fusion utility.

Given multiple ranked lists of documents (each list: sequence of dicts containing at least an 'id'),
compute fused scores: score = sum(1.0 / (k + rank)) per doc across lists.

Default k=60 per literature; configurable.
"""
from __future__ import annotations
from collections import defaultdict
from typing import List, Dict, Any

def reciprocal_rank_fuse(lists: List[List[Dict[str, Any]]], *, k: float = 60.0, top_n: int | None = None) -> List[Dict[str, Any]]:
    fusion: Dict[str, Dict[str, Any]] = {}
    for lst in lists:
        for rank, doc in enumerate(lst, 1):
            doc_id = doc.get('id') or doc.get('key') or doc.get('fact_id') or doc.get('chunk_id') or doc.get('file_id')
            if not doc_id:
                continue
            entry = fusion.setdefault(doc_id, {'doc': doc, 'score': 0.0, 'sources': 0})
            entry['score'] += 1.0 / (k + rank)
            entry['sources'] += 1
    fused = sorted(fusion.values(), key=lambda x: x['score'], reverse=True)
    if top_n:
        fused = fused[:top_n]
    # Return list of docs with fused score injected
    out: List[Dict[str, Any]] = []
    for item in fused:
        d = dict(item['doc'])
        d['fused_score'] = item['score']
        d['fusion_sources'] = item['sources']
        out.append(d)
    return out

if __name__ == '__main__':
    a = [{'id':'A'},{'id':'B'},{'id':'C'}]
    b = [{'id':'B'},{'id':'A'},{'id':'D'}]
    fused = reciprocal_rank_fuse([a,b])
    for f in fused:
        print(f)
