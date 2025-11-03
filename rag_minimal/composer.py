"""Answer composer skeleton.

Takes expanded context and user question; emits structured JSON-able dict with:
- summary (placeholder; final summary produced by LLM later if desired)
- snippets (file, lineStart, lineEnd, text, tag)
- links (empty placeholders)
- notes (from expansion)

Later iterations: integrate LLM summarization *after* assembling grounded snippets.
"""
from __future__ import annotations
from typing import Dict, Any, List

def compose_answer(question: str, expanded_ctx: Any) -> Dict[str, Any]:
    snippets: List[Dict[str, Any]] = []
    for p in expanded_ctx.base_passages[:10]:  # limit for now
        snippets.append({
            'tag': p.get('citation_tag') or p.get('id'),
            'file': p.get('source_path'),
            'lineStart': p.get('lineStart') or p.get('line_start'),
            'lineEnd': p.get('lineEnd') or p.get('line_end'),
            'score': p.get('score') or p.get('fused_score'),
            'text': (p.get('content') or '')[:800]
        })
    answer = {
        'summary': '(summary pending) Question: ' + question[:200],
        'snippets': snippets,
        'links': {
            'definitions': [], 'callers': [], 'callees': [], 'copybookIncludes': [], 'relatedFacts': []
        },
        'graph': None,
        'notes': getattr(expanded_ctx, 'notes', [])
    }
    return answer
