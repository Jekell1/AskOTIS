from __future__ import annotations
import json, math, hashlib
from typing import List, Dict, Any
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from azure.identity import DefaultAzureCredential
from azure.search.documents.models import VectorizedQuery
from .config import get_settings
import os

class HybridRetriever:
    def __init__(self, search_key: str | None = None):
        s = get_settings()
        credential = (
            DefaultAzureCredential(exclude_interactive_browser_credential=True)
            if s.use_managed_identity and not search_key
            else AzureKeyCredential(search_key or "")
        )
        self.program_meta = SearchClient(
            endpoint=s.azure_search_endpoint,
            index_name=s.azure_search_index_program_meta,
            credential=credential,
            api_version=s.azure_search_api_version
        )
        self.paragraphs = SearchClient(
            endpoint=s.azure_search_endpoint,
            index_name=s.azure_search_index_paragraphs,
            credential=credential,
            api_version=s.azure_search_api_version
        )
        # Code chunks (overlapped windows) optional; ignore if not configured
        try:
            self.code_chunks = SearchClient(
                endpoint=s.azure_search_endpoint,
                index_name=getattr(s, 'azure_search_index_code_chunks', 'new_code_chunks'),
                credential=credential,
                api_version=s.azure_search_api_version
            )
        except Exception:
            self.code_chunks = None
        # Data items optional; ignore if not present
        try:
            self.data_items = SearchClient(
                endpoint=s.azure_search_endpoint,
                index_name=s.azure_search_index_data_items,
                credential=credential,
                api_version=s.azure_search_api_version
            )
        except Exception:
            self.data_items = None

    def search(self, query: str, top: int = 8) -> List[Dict[str, Any]]:
        """Hybrid retrieval: combine BM25 + semantic (if enabled) + simple rerank.
        Returns unified chunk list with scores and provenance.
        """
        results: List[Dict[str, Any]] = []
        procedural = self._is_procedural_query(query)

        # Program meta broad semantic / keyword search
        try:
            for r in self.program_meta.search(query, top=4, query_type="simple", include_total_count=False):
                score = float(getattr(r, '@search.score', 0.0) or 0.0)
                doc = r.copy()
                doc['_index'] = 'program_meta'
                doc['_score'] = score * 0.8  # weight
                doc['_chunk'] = doc.get('program_summary') or ''
                results.append(doc)
        except Exception:
            pass

        # Paragraph level (more content rich)
        try:
            for r in self.paragraphs.search(query, top=top, query_type="simple", include_total_count=False):
                score = float(getattr(r, '@search.score', 0.0) or 0.0)
                doc = r.copy()
                doc['_index'] = 'paragraphs'
                doc['_score'] = score
                excerpt = doc.get('source_excerpt') or ''
                doc['_chunk'] = excerpt
                results.append(doc)
        except Exception:
            pass

        # Overlapped code chunks for procedural / multi-step queries
        if procedural and self.code_chunks:
            try:
                for r in self.code_chunks.search(query, top=min(top, 6), query_type="simple", include_total_count=False):
                    score = float(getattr(r, '@search.score', 0.0) or 0.0)
                    d = r.copy()
                    d['_index'] = 'new_code_chunks'
                    d['_score'] = score * 0.95  # slightly discount vs paragraphs for baseline
                    d['_chunk'] = d.get('text','')
                    results.append(d)
            except Exception:
                pass

        # Optional data items for data-structure oriented queries
        if self.data_items:
            try:
                for r in self.data_items.search(query, top=4, query_type="simple", include_total_count=False):
                    score = float(getattr(r, '@search.score', 0.0) or 0.0)
                    d = r.copy()
                    d['_index'] = 'data_items'
                    d['_score'] = score * 0.6
                    d['_chunk'] = d.get('full_clause') or d.get('path') or ''
                    results.append(d)
            except Exception:
                pass

        # Simple rerank: length-normalized score to favor denser explanatory content
        for r in results:
            length = max(20, len(r.get('_chunk','')))
            r['_adj_score'] = r['_score'] * math.log10(length)
            r['_id'] = hashlib.sha256((r.get('_index','')+str(r.get('program_id'))+r.get('_chunk',''))[:512].encode()).hexdigest()[:16]

        results.sort(key=lambda x: x['_adj_score'], reverse=True)
        return results[:top]

    async def hybrid_vector_search(self, query: str, top: int = 8):
        s = get_settings()
        if not s.enable_vector:
            return self.search(query, top)
        vec = (await embed_texts([query]))[0]
        results = []
        procedural = self._is_procedural_query(query)
        # Program meta vector (if field exists)
        try:
            from azure.search.documents.models import VectorizedQuery
            pm_vq = VectorizedQuery(vector=vec, k=min(6, top), fields=["program_summary_vector"])
            resp = self.program_meta.search(search_text=query, vector_queries=[pm_vq], top=min(6, top), query_type="simple")
            for r in resp:
                sc=float(getattr(r,'@search.score',0.0) or 0.0); d=r.copy(); d['_index']='program_meta_vec'; d['_score']=sc; d['_chunk']=d.get('program_summary',''); results.append(d)
        except Exception:
            pass
        # Paragraph vectors as before
        try:
            from azure.search.documents.models import VectorizedQuery
            vq = VectorizedQuery(vector=vec, k=top, fields=["para_vector"])  # field name per paragraphs index
            resp = self.paragraphs.search(search_text=query, vector_queries=[vq], top=top, query_type="simple")
            for r in resp:
                sc=float(getattr(r,'@search.score',0.0) or 0.0); d=r.copy(); d['_index']='paragraphs_vec'; d['_score']=sc; d['_chunk']=d.get('source_excerpt',''); results.append(d)
        except Exception:
            pass
        # Code chunks vector search (if procedural and available)
        if procedural and self.code_chunks:
            try:
                from azure.search.documents.models import VectorizedQuery
                cq = VectorizedQuery(vector=vec, k=min(8, top+2), fields=["text_vector"])
                resp = self.code_chunks.search(search_text=query, vector_queries=[cq], top=min(8, top+2), query_type="simple")
                for r in resp:
                    sc=float(getattr(r,'@search.score',0.0) or 0.0); d=r.copy(); d['_index']='new_code_chunks_vec'; d['_score']=sc; d['_chunk']=d.get('text',''); results.append(d)
            except Exception:
                pass
        # UI path sequence vectors (if index configured via separate SearchClient? reuse program_meta client not available). Skipped here unless added similarly.
        # Fallback keyword mix
        base = self.search(query, top=top)
        results.extend(base)
        # RRF fusion
        seen = {}
        fused = []
        for rank, item in enumerate(results, start=1):
            key = (item.get('_index'), item.get('program_id'), item.get('_chunk')[:120])
            if key not in seen:
                seen[key] = item
                item['_rrf'] = 1.0 / (s.rrf_k + rank)
            else:
                seen[key]['_rrf'] += 1.0 / (s.rrf_k + rank)
        for v in seen.values():
            v['_adj_score'] = v.get('_adj_score', v.get('_score', 0.0)) + v['_rrf']
            fused.append(v)
        fused.sort(key=lambda x: x['_adj_score'], reverse=True)
        import hashlib
        for r in fused:
            r['_id'] = hashlib.sha256((r.get('_index','')+str(r.get('program_id'))+r.get('_chunk',''))[:512].encode()).hexdigest()[:16]
        return fused[:top]

    # ---------------------------------
    # Heuristic procedural query detect
    # ---------------------------------
    def _is_procedural_query(self, q: str) -> bool:
        ql = q.lower().strip()
        if len(ql) == 0:
            return False
        # Trigger words indicating process / flow intent
        process_terms = [
            'how ', 'process ', 'calculate', 'derive', 'flow', 'sequence', 'steps', 'logic', 'performed', 'determine', 'compute', 'allocation'
        ]
        score = 0
        for t in process_terms:
            if t in ql:
                score += 1
        # Long technical question w/ verbs often needs procedure context
        if len(ql.split()) >= 10:
            score += 1
        return score >= 1


def collapse_context(chunks: List[Dict[str, Any]], max_chars: int) -> List[Dict[str, Any]]:
    out = []
    total = 0
    for c in chunks:
        content = c.get('_chunk','')
        if not content:
            continue
        if total + len(content) > max_chars:
            break
        out.append({
            'id': c['_id'],
            'index': c.get('_index'),
            'program_id': c.get('program_id'),
            'score': round(c.get('_adj_score',0.0),4),
            'content': content[:1500]
        })
        total += len(content)
    return out

# Simple embedding helper (extensible):
async def embed_texts(texts: list[str]) -> list[list[float]]:
    s = get_settings()
    if not s.embedding_model:
        return [[0.0]] * len(texts)
    try:
        import openai
        if s.azure_openai_endpoint and s.embedding_model:
            openai.azure_endpoint = s.azure_openai_endpoint
            openai.api_type = "azure"
            openai.api_key = s.openai_api_key
            resp = openai.embeddings.create(model=s.embedding_model, input=texts)
        else:
            openai.api_key = s.openai_api_key
            resp = openai.embeddings.create(model=s.embedding_model or "text-embedding-3-small", input=texts)
        return [d.embedding for d in resp.data]
    except Exception:
        return [[0.0]] * len(texts)
