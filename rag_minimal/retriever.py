import os, logging, math, json, requests
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass
from azure.core.credentials import AzureKeyCredential
from azure.search.documents import SearchClient
try:
    from azure.search.documents.models import VectorQuery  # newer SDKs
except Exception:  # pragma: no cover
    VectorQuery = None  # type: ignore

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')

@dataclass
class Passage:
    id: str
    content: str
    score: float
    program_id: Optional[str]
    source_path: Optional[str]
    citation_tag: str = ''

class HybridRetriever:
    def __init__(self, index_name: str, *, endpoint: Optional[str] = None, key: Optional[str] = None,
                 vector_field: str = 'fact_vector', text_field: str = 'fact_text', local_settings: str = 'local.settings.json',
                 id_candidates: Optional[List[str]] = None):
        # Prefer explicit parameters, then AZURE_* then fallback SEARCH_* env vars
        endpoint = endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
        key = key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        if not (endpoint and key):
            # Attempt autoload from local.settings.json once
            p = Path(local_settings)
            if p.exists():
                try:
                    data = json.loads(p.read_text(encoding='utf-8'))
                    vals = data.get('Values', {}) or {}
                    if not endpoint:
                        endpoint = vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
                    if not key:
                        key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
                    if endpoint and key:
                        logger.info(f"HybridRetriever autoloaded credentials from {local_settings}")
                except Exception as ex:
                    logger.debug(f"HybridRetriever failed autoload: {ex}")
        if not (endpoint and key):
            raise RuntimeError('Search endpoint/key not configured (env vars or local.settings.json).')
        self.index_name = index_name
        self.endpoint = endpoint.rstrip('/')
        self.vector_field = vector_field
        self.text_field = text_field
        self._api_key = key
        self.client = SearchClient(endpoint=endpoint, index_name=index_name, credential=AzureKeyCredential(key))
        self.id_candidates = id_candidates or ['id','fact_id','chunk_id','file_id','doc_id']

    def search(self, query_text: str, query_vector: List[float], *, top_k_vector: int = 15, top_k_text: int = 15,
               top_n: int = 12, program: Optional[str] = None) -> List[Passage]:
        filter_expr = None
        if program:
            filter_expr = f"program_id eq '{program}'"
        # Vector search via REST to avoid SDK incompatibilities.
        v_results = self._rest_vector_search(query_vector, top_k_vector, filter_expr)
        # BM25 search (omit select to allow dynamic id field resolution)
        t_results = list(self.client.search(
            search_text=query_text,
            filter=filter_expr,
            top=top_k_text,
        ))
        # RRF Fusion
        fusion: Dict[str, Dict[str, Any]] = {}
        k = 60.0
        def _pick_id(d: Dict[str, Any]):
            for cand in self.id_candidates:
                if cand in d:
                    return d[cand]
            # fallback: first key that looks id-like
            for k in d.keys():
                if k.endswith('_id') or k == 'key':
                    return d[k]
            return 'UNKNOWN'

        for rank, doc in enumerate(v_results, 1):
            fid = _pick_id(doc)
            entry = fusion.setdefault(fid, {'doc': doc, 'score': 0.0, 'vec_rank': rank, 'txt_rank': None})
            entry['score'] += 1.0 / (k + rank)
        for rank, doc in enumerate(t_results, 1):
            fid = _pick_id(doc)
            entry = fusion.setdefault(fid, {'doc': doc, 'score': 0.0, 'vec_rank': None, 'txt_rank': rank})
            entry['score'] += 1.0 / (k + rank)
        merged = sorted(fusion.values(), key=lambda x: x['score'], reverse=True)[:top_n]
        passages: List[Passage] = []
        for idx, item in enumerate(merged, 1):
            doc = item['doc']
            doc_id = _pick_id(doc)
            passages.append(Passage(
                id=doc_id,
                content=doc.get(self.text_field, ''),
                score=item['score'],
                program_id=doc.get('program_id'),
                source_path=doc.get('source_path'),
                citation_tag=f"P{idx}"
            ))
        return passages

    def search_debug(self, query_text: str, query_vector: List[float], *, top_k_vector: int = 15, top_k_text: int = 15,
                     top_n: int = 12, program: Optional[str] = None) -> Tuple[List[Passage], Dict[str, Any]]:
        """Debug variant of search returning fused passages and raw intermediate detail.

        Returns (passages, debug_info) where debug_info contains raw vector results, text results,
        fusion map with ranks/scores, and simple program distribution stats.
        """
        filter_expr = None
        if program:
            filter_expr = f"program_id eq '{program}'"
        v_results = self._rest_vector_search(query_vector, top_k_vector, filter_expr)
        t_results = list(self.client.search(search_text=query_text, filter=filter_expr, top=top_k_text))
        fusion: Dict[str, Dict[str, Any]] = {}
        k = 60.0

        def _pick_id(d: Dict[str, Any]):
            for cand in self.id_candidates:
                if cand in d:
                    return d[cand]
            for key in d.keys():
                if key.endswith('_id') or key == 'key':
                    return d[key]
            return 'UNKNOWN'

        for rank, doc in enumerate(v_results, 1):
            fid = _pick_id(doc)
            entry = fusion.setdefault(fid, {'doc': doc, 'score': 0.0, 'vec_rank': rank, 'txt_rank': None})
            entry['score'] += 1.0 / (k + rank)
        for rank, doc in enumerate(t_results, 1):
            fid = _pick_id(doc)
            entry = fusion.setdefault(fid, {'doc': doc, 'score': 0.0, 'vec_rank': None, 'txt_rank': rank})
            entry['score'] += 1.0 / (k + rank)
        merged = sorted(fusion.values(), key=lambda x: x['score'], reverse=True)[:top_n]
        passages: List[Passage] = []
        program_counts: Dict[str, int] = {}
        for idx, item in enumerate(merged, 1):
            doc = item['doc']
            doc_id = _pick_id(doc)
            prog = doc.get('program_id') or 'UNKNOWN'
            program_counts[prog] = program_counts.get(prog, 0) + 1
            passages.append(Passage(
                id=doc_id,
                content=doc.get(self.text_field, ''),
                score=item['score'],
                program_id=doc.get('program_id'),
                source_path=doc.get('source_path'),
                citation_tag=f"P{idx}"
            ))
        debug_info = {
            'vector_raw': v_results,
            'text_raw': [dict(r) for r in t_results],
            'fusion': [
                {
                    'id': _pick_id(item['doc']),
                    'vec_rank': item['vec_rank'],
                    'txt_rank': item['txt_rank'],
                    'fused_score': item['score'],
                    'program_id': item['doc'].get('program_id')
                } for item in merged
            ],
            'program_distribution': program_counts,
            'params': {
                'top_k_vector': top_k_vector,
                'top_k_text': top_k_text,
                'top_n': top_n,
                'program_filter': program
            }
        }
        return passages, debug_info

    def _rest_vector_search(self, query_vector, k: int, filter_expr: Optional[str]):
        # Use a broadly supported preview api-version for vector queries
        api_version = '2024-05-01-preview'
        url = f"{self.endpoint}/indexes/{self.index_name}/docs/search?api-version={api_version}"
        headers = {"Content-Type": "application/json", "api-key": self._api_key}
        body = {
            "vectorQueries": [
                {"kind": "vector", "vector": query_vector, "k": k, "fields": self.vector_field}
            ],
            "search": "*",
            # omit select for dynamic id/text field discovery
        }
        if filter_expr:
            body['filter'] = filter_expr
        resp = requests.post(url, json=body, headers=headers, timeout=30)
        if resp.status_code != 200:
            logger.warning(f"Vector REST search failed {resp.status_code}: {resp.text[:180]}")
            return []
        data = resp.json()
        return data.get('value', [])
