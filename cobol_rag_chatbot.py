"""
COBOL RAG Chatbot with Memory
============================
A comprehensive RAG system using LangChain that integrates with your COBOL Search API
and provides conversational memory for multi-turn interactions.
"""

import os
import json
from pathlib import Path
import requests
import streamlit as st
from typing import List, Dict, Any, Optional
from datetime import datetime
from dataclasses import dataclass, field
import re
import asyncio
import logging
from pydantic import PrivateAttr
from functools import lru_cache

# Compatibility handling for LangChain 0.2+ where many integrations moved to langchain_community
try:  # vectorstores location changed across versions
    from langchain.vectorstores import Chroma  # type: ignore
except Exception:  # pragma: no cover
    from langchain_community.vectorstores import Chroma  # type: ignore

# HuggingFace sentence-transformers may be disallowed in some environments.
ENABLE_HF = os.environ.get("NO_HF_EMBEDDINGS", "").lower() not in {"1","true","yes"}
if ENABLE_HF:
    try:
        from langchain.embeddings import SentenceTransformerEmbeddings  # type: ignore
    except Exception:  # pragma: no cover
        try:
            from langchain_community.embeddings import SentenceTransformerEmbeddings  # type: ignore
        except Exception:
            SentenceTransformerEmbeddings = None  # type: ignore
else:
    SentenceTransformerEmbeddings = None  # type: ignore

from langchain.memory import ConversationBufferWindowMemory
from langchain.prompts import PromptTemplate
from langchain.chains.base import Chain
from langchain.callbacks.manager import CallbackManagerForChainRun

def _load_local_settings_fallback():
    """Load local.settings.json and map legacy + embedding keys without overwriting explicit env vars.

    Mappings include:
      SEARCH_* -> AZURE_SEARCH_*
      AZURE_OPENAI_* (deployment, endpoint, key, embed deployment)
      Optional tuning: COBOL_CHAT_MEMORY, COBOL_MAX_SEARCH_RESULTS, COBOL_VECTOR_DB_PATH
    """
    try:
        path = Path("local.settings.json")
        if not path.exists():
            return
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
        values = data.get("Values", {})
        # Core search legacy mapping
        search_mapping = {
            "SEARCH_ENDPOINT": "AZURE_SEARCH_ENDPOINT",
            "SEARCH_INDEX": "AZURE_SEARCH_INDEX",
            "SEARCH_KEY": "AZURE_SEARCH_KEY",
        }
        for legacy, modern in search_mapping.items():
            if modern not in os.environ and legacy in values:
                os.environ[modern] = values[legacy]
        # Azure OpenAI embedding related direct pass-through if missing.
        ao_keys = [
            "AZURE_OPENAI_ENDPOINT",
            "AZURE_OPENAI_KEY",
            "AZURE_OPENAI_EMBED_DEPLOYMENT",
            "AZURE_OPENAI_EMBED_DEPLOYMENT_NAME",  # alt naming
            "AZURE_OPENAI_API_VERSION",
        ]
        for k in ao_keys:
            if k not in os.environ and k in values:
                os.environ[k] = values[k]
        # Optional config tuning keys
        opt_keys = {
            "COBOL_CHAT_MEMORY": "COBOL_CHAT_MEMORY",
            "COBOL_MAX_SEARCH_RESULTS": "COBOL_MAX_SEARCH_RESULTS",
            "COBOL_VECTOR_DB_PATH": "COBOL_VECTOR_DB_PATH",
            "COBOL_EMBED_MODEL": "COBOL_EMBED_MODEL",
        }
        for src, dest in opt_keys.items():
            if dest not in os.environ and src in values:
                os.environ[dest] = values[src]
    except Exception:
        pass

_load_local_settings_fallback()

@dataclass
class COBOLRagConfig:
    """Configuration for COBOL RAG system (after fallback resolution)"""
    # Search
    azure_search_endpoint: str = os.environ.get("AZURE_SEARCH_ENDPOINT", "https://az-use1-ai-search.search.windows.net")
    azure_search_key: str = os.environ.get("AZURE_SEARCH_KEY", "")
    azure_search_index: str = os.environ.get("AZURE_SEARCH_INDEX", "")
    azure_search_indexes_raw: str = os.environ.get("AZURE_SEARCH_INDEXES", "")  # comma separated optional list
    # Embeddings primary (HuggingFace sentence-transformers)
    embedding_model: str = os.environ.get("COBOL_EMBED_MODEL", "all-MiniLM-L6-v2")
    # Azure OpenAI fallback
    azure_openai_endpoint: str = os.environ.get("AZURE_OPENAI_ENDPOINT", "")
    azure_openai_key: str = os.environ.get("AZURE_OPENAI_KEY", "")
    azure_openai_embed_deployment: str = os.environ.get("AZURE_OPENAI_EMBED_DEPLOYMENT", os.environ.get("AZURE_OPENAI_EMBED_DEPLOYMENT_NAME", ""))
    azure_openai_api_version: str = os.environ.get("AZURE_OPENAI_API_VERSION", "2024-08-01-preview")
    # Vector DB path / memory / retrieval
    vector_db_path: str = os.environ.get("COBOL_VECTOR_DB_PATH", "./cobol_vector_db")
    memory_window_size: int = int(os.environ.get("COBOL_CHAT_MEMORY", "10"))
    max_search_results: int = int(os.environ.get("COBOL_MAX_SEARCH_RESULTS", "20"))
    # Misc
    openai_api_key: str = os.environ.get("OPENAI_API_KEY", "")
    # Hybrid / augmentation
    code_vector_field: str = os.environ.get("COBOL_VECTOR_FIELD", "")  # e.g. code_vector
    vector_k: int = int(os.environ.get("COBOL_VECTOR_K", "25"))
    program_deps_index: str = os.environ.get("COBOL_PROGRAM_DEPS_INDEX", "new_cobol_program_deps")
    flow_edges_index: str = os.environ.get("COBOL_FLOW_EDGES_INDEX", "new_cobol_flow_edges_v2")
    deps_max_append: int = int(os.environ.get("COBOL_DEPS_MAX_APPEND", "5"))
    flow_max_depth: int = int(os.environ.get("COBOL_FLOW_MAX_DEPTH", "4"))
    flow_max_branch: int = int(os.environ.get("COBOL_FLOW_MAX_BRANCH", "25"))
    enable_flow_path: bool = os.environ.get("COBOL_ENABLE_FLOW_PATH", "1") not in {"0","false","no"}
    enable_deps_aug: bool = os.environ.get("COBOL_ENABLE_DEPS_AUG", "1") not in {"0","false","no"}
    enable_vector: bool = os.environ.get("COBOL_ENABLE_VECTOR", "1") not in {"0","false","no"}

    def azure_embeddings_enabled(self) -> bool:
        return bool(self.azure_openai_endpoint and self.azure_openai_key and self.azure_openai_embed_deployment)

    def get_index_list(self) -> List[str]:
        if self.azure_search_indexes_raw:
            lst = [x.strip() for x in self.azure_search_indexes_raw.split(',') if x.strip()]
            if lst:
                return lst
        if self.azure_search_index:
            return [self.azure_search_index]
        # Allow fallback to legacy SEARCH_INDEX env if present
        legacy = os.environ.get("SEARCH_INDEX")
        return [legacy] if legacy else []

class COBOLSearchTool:
    """Custom tool for searching COBOL code using Azure AI Search (supports dynamic index switching)."""
    def __init__(self, config: COBOLRagConfig, index_name: Optional[str] = None):
        self.config = config
        if not config.azure_search_key:
            raise RuntimeError("AZURE_SEARCH_KEY environment variable required for search.")
        if not config.azure_search_endpoint:
            raise RuntimeError("AZURE_SEARCH_ENDPOINT not configured.")
        # Determine initial index
        resolved = index_name or os.environ.get("AZURE_SEARCH_INDEX") or config.azure_search_index or os.environ.get("SEARCH_INDEX")
        if not resolved:
            raise RuntimeError("No search index specified (AZURE_SEARCH_INDEX / SEARCH_INDEX / AZURE_SEARCH_INDEXES).")
        self.index_name = resolved
        self.headers = {
            'Content-Type': 'application/json',
            'api-key': config.azure_search_key
        }

    def set_index(self, index_name: str):
        self.index_name = index_name

    def search_cobol(self, query: str, filter_type: Optional[str] = None, max_results: int = 20, index_override: Optional[str] = None) -> List[Dict[str, Any]]:
        active_index = index_override or self.index_name
        api_url = f"{self.config.azure_search_endpoint}/indexes/{active_index}/docs/search"
        search_body = {
            "search": query,
            "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
            "top": max_results,
            "count": True
        }
        if filter_type:
            filter_map = {
                "programs": "symbol_kind eq 'program'",
                "variables": "symbol_kind eq 'variable'",
                "procedures": "symbol_kind eq 'procedure'"
            }
            if filter_type in filter_map:
                search_body["filter"] = filter_map[filter_type]
        try:
            resp = requests.post(
                api_url,
                headers=self.headers,
                params={"api-version": "2024-07-01"},
                json=search_body,
                timeout=20
            )
            if resp.status_code == 200:
                data = resp.json()
                formatted = []
                for r in data.get('value', []):
                    formatted.append({
                        'content': f"File: {r.get('repo_path','Unknown')}\n"\
                                   f"Line: {r.get('line','N/A')}\n"\
                                   f"Code: {r.get('code','')}\n"\
                                   f"Symbol: {r.get('symbol_name','N/A')}\n"\
                                   f"Type: {r.get('symbol_kind','N/A')}",
                        'metadata': {
                            'source': r.get('repo_path','Unknown'),
                            'line': r.get('line',0),
                            'symbol_type': r.get('symbol_kind','unknown'),
                            'symbol_name': r.get('symbol_name',''),
                            'calls': r.get('calls', [])
                        }
                    })
                return formatted
            else:
                st.error(f"Search HTTP {resp.status_code}: {resp.text[:200]}")
        except Exception as e:
            st.error(f"Search error: {e}")
        return []

    def vector_search_cobol(self, query: str, max_results: int = 20, index_override: Optional[str] = None, query_embedding: Optional[List[float]] = None) -> List[Dict[str,Any]]:
        if not self.config.code_vector_field or not self.config.enable_vector:
            return []
        active_index = index_override or self.index_name
        api_url = f"{self.config.azure_search_endpoint}/indexes/{active_index}/docs/search"
        body = {
            "search": "",
            "top": max_results,
            "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
            "vectorQueries": []
        }
        if query_embedding is not None:
            body["vectorQueries"].append({
                "kind": "vector",
                "value": query_embedding,
                "fields": self.config.code_vector_field,
                "k": max_results
            })
        else:
            # Fallback to text kind if integrated vectorization is configured server-side
            body["vectorQueries"].append({
                "kind": "text",
                "text": query,
                "fields": self.config.code_vector_field,
                "k": max_results
            })
        try:
            resp = requests.post(api_url, headers=self.headers, params={"api-version":"2024-07-01"}, json=body, timeout=25)
            if resp.status_code != 200:
                return []
            data = resp.json()
            out=[]
            for r in data.get('value', []):
                out.append({
                    'content': f"File: {r.get('repo_path','Unknown')}\nLine: {r.get('line','N/A')}\nCode: {r.get('code','')}\nSymbol: {r.get('symbol_name','N/A')}\nType: {r.get('symbol_kind','N/A')}",
                    'metadata': {
                        'source': r.get('repo_path','Unknown'),
                        'line': r.get('line',0),
                        'symbol_type': r.get('symbol_kind','unknown'),
                        'symbol_name': r.get('symbol_name',''),
                        'calls': r.get('calls', []),
                        'retrieval_mode': 'vector'
                    }
                })
            return out
        except Exception:
            return []

class COBOLRAGChain(Chain):
    """Custom RAG chain for COBOL code search and response generation."""
    # Declare dynamic attributes so Pydantic doesn't block assignment
    _config = PrivateAttr()
    _search_tool = PrivateAttr()
    _embeddings = PrivateAttr(default=None)
    _vector_store = PrivateAttr(default=None)
    _memory = PrivateAttr()
    _prompt_template = PrivateAttr()

    def __init__(self, config: COBOLRagConfig, **kwargs):
        super().__init__(**kwargs)
        self._config = config
        self._search_tool = COBOLSearchTool(config)
        # Embedding initialization with layered fallback
        self._embeddings = None
        embed_errors: List[str] = []
        # 1. Try sentence-transformers local/HF (only if enabled and class resolved)
        if ENABLE_HF and SentenceTransformerEmbeddings is not None:
            try:
                self._embeddings = SentenceTransformerEmbeddings(model_name=config.embedding_model)
            except Exception as e:
                embed_errors.append(f"SentenceTransformerEmbeddings failed: {e}")
        elif not ENABLE_HF:
            embed_errors.append("HuggingFace embeddings explicitly disabled via NO_HF_EMBEDDINGS flag.")
        # 2. Azure OpenAI embeddings fallback
        if self._embeddings is None and config.azure_embeddings_enabled():
            try:
                # Lazy import to avoid mandatory dependency
                from langchain_openai import AzureOpenAIEmbeddings  # type: ignore
                self._embeddings = AzureOpenAIEmbeddings(
                    azure_endpoint=config.azure_openai_endpoint,
                    api_key=config.azure_openai_key,
                    deployment=config.azure_openai_embed_deployment,
                    openai_api_version=config.azure_openai_api_version,
                )
            except Exception as e:
                embed_errors.append(f"AzureOpenAIEmbeddings fallback failed: {e}")
        # 3. Stub fallback (no vector semantic similarity; just returns zero vectors). Only if all failed.
        if self._embeddings is None:
            class _StubEmbeddings:
                def embed_documents(self, texts):
                    return [[0.0]*384 for _ in texts]
                def embed_query(self, text):
                    return [0.0]*384
            self._embeddings = _StubEmbeddings()
            st.warning("All embedding providers failed; using stub embeddings (no true vector similarity)." )
            for err in embed_errors:
                st.info(err)
        # Attempt vector store build (skip if stub to avoid wasted disk writes)
        if self._embeddings and self._embeddings.__class__.__name__ != '_StubEmbeddings':
            try:
                self._vector_store = Chroma(persist_directory=config.vector_db_path, embedding_function=self._embeddings)
            except Exception as e:
                st.warning(f"Chroma init failed ({e}); continuing without vector store.")
                self._vector_store = None
        else:
            self._vector_store = None
        self._memory = ConversationBufferWindowMemory(
            k=config.memory_window_size,
            memory_key="chat_history",
            return_messages=True,
            output_key="answer"
        )
        self._prompt_template = PromptTemplate(
            input_variables=["context","question","chat_history"],
            template=(
                "You are a COBOL code expert assistant.\n\n"
                "Chat History:\n{chat_history}\n\nContext from COBOL codebase:\n{context}\n\n"
                "Question: {question}\n\n"
                "Instructions:\n"
                "1. Use context accurately.\n2. Cite file path + line numbers.\n3. Suggest refined searches if context thin.\n4. Keep answers concise then offer follow-up help.\n\nAnswer:" )
        )

    @property
    def input_keys(self) -> List[str]:
        return ["question"]

    @property
    def output_keys(self) -> List[str]:
        return ["answer"]

    def _call(self, inputs: Dict[str, Any], run_manager: Optional[CallbackManagerForChainRun]=None) -> Dict[str, Any]:
        q = inputs["question"]
        # Support dynamic index selection (session state may hold active index)
        active_index = None
        try:
            # Streamlit session state accessible only in main thread
            if 'active_index' in st.session_state:
                active_index = st.session_state['active_index']
                # update tool if changed
                if active_index and active_index != self._search_tool.index_name:
                    self._search_tool.set_index(active_index)
        except Exception:
            pass
        # --- Hybrid retrieval (lexical + optional vector) ---
        lexical_results = self._search_tool.search_cobol(q, max_results=self._config.max_search_results, index_override=active_index)
        vector_results: List[Dict[str,Any]] = []
        if self._config.enable_vector and self._config.code_vector_field:
            try:
                if hasattr(self._embeddings, 'embed_query') and self._embeddings.__class__.__name__ != '_StubEmbeddings':
                    q_emb = self._embeddings.embed_query(q)
                else:
                    q_emb = None
                vector_results = self._search_tool.vector_search_cobol(q, max_results=min(self._config.vector_k, self._config.max_search_results), index_override=active_index, query_embedding=q_emb)
            except Exception:
                vector_results = []
        # Merge & dedupe by (source,line,code snippet)
        merged = []
        seen = set()
        def _key(d):
            m=d['metadata']; return (m.get('source'), m.get('line'), m.get('symbol_name'))
        for col in (lexical_results, vector_results):
            for r in col:
                k=_key(r)
                if k in seen: continue
                seen.add(k); merged.append(r)
        results = merged
        # --- Dependency enrichment ---
        deps_section = ""
        if self._config.enable_deps_aug and results:
            prog_names = []
            for r in results:
                if r['metadata'].get('symbol_type') == 'program':
                    name = r['metadata'].get('symbol_name')
                    if name and name not in prog_names:
                        prog_names.append(name)
                if len(prog_names) >= self._config.deps_max_append:
                    break
            if prog_names:
                deps_section = self._fetch_program_deps_snippets(prog_names)
        # --- Flow path tracing (query intent) ---
        flow_section = ""
        if self._config.enable_flow_path:
            flow_section = self._maybe_trace_flow(q)
        context_parts = [f"Result {i+1}:\n{r['content']}\n" for i,r in enumerate(results[:10])]
        context = "\n".join(context_parts) if context_parts else "<no matches>"
        if deps_section:
            context += f"\n\n[Dependencies]\n{deps_section}"
        if flow_section:
            context += f"\n\n[FlowPath]\n{flow_section}"
        # history
        hist_msgs = getattr(self._memory, 'chat_memory', None)
        hist_text = ""
        if hist_msgs and getattr(hist_msgs, 'messages', None):
            for m in hist_msgs.messages[-6:]:
                role = 'Human' if getattr(m,'type','')=='human' else 'Assistant'
                hist_text += f"{role}: {m.content}\n"
        prompt = self._prompt_template.format(context=context, question=q, chat_history=hist_text)
        answer = self._generate_response(prompt, results)
        self._memory.save_context({"question": q},{"answer": answer})
        return {"answer": answer}

    def _generate_response(self, prompt: str, results: List[Dict[str,Any]]) -> str:
        if not results:
            return ("No direct matches found. Consider narrowing with a program/paragraph name or a variable fragment. "
                    "Examples: 'PROGRAM-ID ACUMEM', 'PERFORM THRU', 'WORKING-STORAGE DATE'.")
        out=[f"Found {len(results)} relevant code sections (showing top 5 grouped by file):"]
        files={}
        for r in results[:5]:
            files.setdefault(r['metadata']['source'], []).append(r)
        for fp, recs in files.items():
            out.append(f"\n📁 {fp}")
            for r in recs[:3]:
                line=r['metadata']['line']
                sym=r['metadata']['symbol_name'] or '<anon>'
                kind=r['metadata']['symbol_type']
                code_line=""
                for ln in r['content'].split('\n'):
                    if ln.startswith('Code: '):
                        code_line=ln.split('Code: ',1)[1].strip()
                        break
                out.append(f"  • Line {line}: {sym} ({kind})")
                if code_line:
                    out.append(f"    ```cobol\n    {code_line}\n    ```")
        out.append("\nYou can ask about callers, called programs, data items, or paragraph flow.")
        return "\n".join(out)

    # --- Augmentation helpers ---
    def _fetch_program_deps_snippets(self, program_ids: List[str]) -> str:
        if not program_ids: return ""
        ep = self._config.azure_search_endpoint.rstrip('/')
        key = self._config.azure_search_key
        headers={'api-key':key,'Content-Type':'application/json'}
        segs=[]
        for pid in program_ids:
            body={"search":"*","top":1,"filter":f"program_id eq '{pid}'","select":"program_id,dependency_blob"}
            try:
                r=requests.post(f"{ep}/indexes/{self._config.program_deps_index}/docs/search?api-version=2024-07-01", headers=headers, json=body, timeout=8)
                if r.status_code!=200: continue
                vals=r.json().get('value',[])
                if not vals: continue
                blob=(vals[0].get('dependency_blob') or '').strip()
                if blob:
                    clean = blob[:300].replace('\n',' ')
                    tail = '...' if len(blob)>300 else ''
                    segs.append(f"{pid}: {clean}{tail}")
            except Exception:
                continue
        return "\n".join(segs[:self._config.deps_max_append])

    def _maybe_trace_flow(self, query: str) -> str:
        # Heuristic triggers
        trigger = any(k in query.lower() for k in ["flow","path","reach","route"])
        # pattern: from X to Y or X -> Y
        m = re.search(r"(?:from\s+)?([A-Za-z0-9_]+)\s*(?:->|to)\s*([A-Za-z0-9_]+)", query, re.IGNORECASE)
        if not (trigger or m):
            return ""
        if not m:
            return ""  # require explicit pair for now
        src=m.group(1).upper(); dst=m.group(2).upper()
        return self._bfs_program_flow(src,dst)

    def _bfs_program_flow(self, src: str, dst: str) -> str:
        ep=self._config.azure_search_endpoint.rstrip('/')
        key=self._config.azure_search_key
        headers={'api-key':key,'Content-Type':'application/json'}
        max_depth=self._config.flow_max_depth
        max_branch=self._config.flow_max_branch
        index=self._config.flow_edges_index
        if not (ep and key and index):
            return "(flow index not configured)"
        from collections import deque
        q=deque(); q.append((src,[src]))
        visited={src}
        def fetch_outgoing(p):
            body={"search":"*","top":max_branch,"filter":f"from_program eq '{p}'","select":"from_program,to_program,edge_text"}
            try:
                r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version=2024-07-01", headers=headers, json=body, timeout=8)
                if r.status_code!=200: return []
                return r.json().get('value',[])
            except Exception:
                return []
        found_path=None
        while q:
            node,path=q.popleft()
            if len(path)-1 >= max_depth: continue
            for e in fetch_outgoing(node):
                tgt=(e.get('to_program') or e.get('callee_program') or e.get('target_program') or e.get('called_program') or '').upper()
                if not tgt: continue
                if tgt==dst:
                    found_path=path+[tgt]; q.clear(); break
                if tgt not in visited:
                    visited.add(tgt); q.append((tgt,path+[tgt]))
        if not found_path:
            return f"No path {src}->{dst} within depth {max_depth}."
        return " -> ".join(found_path)

class COBOLChatbotApp:
    """Streamlit app for COBOL RAG Chatbot"""
    def __init__(self):
        self.config = COBOLRagConfig()
        self.rag_chain: Optional[COBOLRAGChain] = None
        self._index_validated: bool = False

    @st.cache_data(ttl=30, show_spinner=False)
    def _cached_index_stats(_self, endpoint: str, key: str):  # type: ignore
        """Fetch index list + stats + vector field detection (cached for 30s).

        Returns list of dicts: name, documentCount, storageSize, vectorFields(list[str]), vectorFieldCount
        """
        base = endpoint.strip().rstrip('/')
        headers = {'api-key': key}
        out = []
        try:
            lst = requests.get(f"{base}/indexes?api-version=2024-07-01", headers=headers, timeout=15)
            if lst.status_code != 200:
                return [{'name':'<error>', 'error': f"List indexes {lst.status_code}"}]
            for item in lst.json().get('value', []):
                name = item.get('name')
                stats = requests.get(f"{base}/indexes/{name}/stats?api-version=2024-07-01", headers=headers, timeout=10)
                doc_count = storage_size = None
                if stats.status_code == 200:
                    js = stats.json()
                    doc_count = js.get('documentCount')
                    storage_size = js.get('storageSize')
                # definition to detect vector fields
                defn = requests.get(f"{base}/indexes/{name}?api-version=2024-07-01", headers=headers, timeout=10)
                vector_fields = []
                if defn.status_code == 200:
                    d = defn.json()
                    for f in d.get('fields', []):
                        if 'vectorSearchDimensions' in f or 'vectorSearchProfile' in f or f.get('name','').endswith('_vector'):
                            vector_fields.append(f.get('name'))
                out.append({
                    'name': name,
                    'documentCount': doc_count,
                    'storageSize': storage_size,
                    'vectorFields': vector_fields,
                    'vectorFieldCount': len(vector_fields)
                })
        except Exception as e:
            return [{'name':'<error>', 'error': str(e)}]
        return out

    def _render_index_stats(self):
        if not (self.config.azure_search_endpoint and self.config.azure_search_key):
            st.info("Index stats unavailable (missing endpoint/key).")
            return
        with st.expander("Index Stats (cached 30s)", expanded=False):
            if st.button("Refresh Index Stats", key="refresh_index_stats"):
                try:
                    self._cached_index_stats.clear()  # type: ignore
                except Exception:
                    pass
            data = self._cached_index_stats(self.config.azure_search_endpoint, self.config.azure_search_key)
            active = st.session_state.get('active_index') if 'active_index' in st.session_state else None
            lines = []
            for row in data:
                if 'error' in row:
                    lines.append(f"ERROR: {row['error']}")
                    continue
                marker = '⭐' if row['name'] == active else ''
                vf = ",".join(row['vectorFields']) if row['vectorFields'] else '-'
                lines.append(f"{marker}{row['name']}: docs={row['documentCount']} size={row['storageSize']} vectors={row['vectorFieldCount']} [{vf}]")
            st.code("\n".join(lines) or "<no indexes>")

    def _validate_index(self) -> bool:
        """Issue a lightweight search (top=0) to confirm index existence before enabling chat."""
        if self._index_validated:
            return True
        endpoint = self.config.azure_search_endpoint.strip().rstrip('/')
        # Prefer actively selected index if present
        index = st.session_state.get('active_index') if 'active_index' in st.session_state else None
        if not index:
            index = os.environ.get("AZURE_SEARCH_INDEX") or self.config.azure_search_index or os.environ.get("SEARCH_INDEX")
        key = self.config.azure_search_key
        if not (endpoint and index and key):
            st.error("Incomplete search configuration (endpoint/index/key missing).")
            return False
        url = f"{endpoint}/indexes/{index}/docs/search?api-version=2024-07-01"
        try:
            resp = requests.post(url, headers={"Content-Type": "application/json", "api-key": key}, json={"search":"*","top":0}, timeout=8)
            if resp.status_code == 200:
                self._index_validated = True
                return True
            if resp.status_code == 404:
                st.error(f"Azure AI Search index '{index}' not found. Set AZURE_SEARCH_INDEX / SEARCH_INDEX to an existing index or create it.")
                return False
            if resp.status_code in (401,403):
                st.error("Search key unauthorized (401/403). Use a valid query or admin key.")
                return False
            st.warning(f"Index validation unexpected status {resp.status_code}: {resp.text[:140]}")
            return False
        except Exception as e:
            st.error(f"Index validation error: {e}")
            return False
    def setup_rag_chain(self):
        if self.rag_chain is None:
            if not self._validate_index():
                return
            self.rag_chain = COBOLRAGChain(self.config)
    def run(self):
        st.set_page_config(page_title="COBOL RAG Chatbot", page_icon="🔍", layout="wide")
        st.title("🔍 COBOL Code Assistant")
        st.subheader("RAG-Powered Chatbot with Conversational Memory")
        with st.sidebar:
            st.markdown("### 📊 Runtime Info")
            st.write(f"Search Endpoint: {self.config.azure_search_endpoint}")
            st.write(f"Vector DB Path: {self.config.vector_db_path}")
            st.write(f"Embeddings: {self.config.embedding_model}")
            # Dynamic index selection
            index_list = self.config.get_index_list()
            if index_list:
                default_idx = 0
                if 'active_index' not in st.session_state or st.session_state['active_index'] not in index_list:
                    st.session_state['active_index'] = index_list[0]
                active_choice = st.selectbox("Active Index", index_list, index=index_list.index(st.session_state['active_index']))
                if active_choice != st.session_state['active_index']:
                    st.session_state['active_index'] = active_choice
                    # Force re-validation next run
                    self._index_validated = False
                    st.experimental_rerun()
            else:
                st.warning("No index specified (set AZURE_SEARCH_INDEX or AZURE_SEARCH_INDEXES).")
            # On-demand stats
            self._render_index_stats()
            st.markdown("### 💡 Example Queries")
            examples=[
                "Find all CUSTOMER variables",
                "Show me PROGRAM-ID statements",
                "What programs call ACUMEM?",
                "Search for IDENTIFICATION DIVISION",
                "Find variables with DATE in the name"
            ]
            for ex in examples:
                if st.button(ex, key=f"ex_{ex[:18]}"):
                    st.session_state.example_query=ex
        self.setup_rag_chain()
        if 'messages' not in st.session_state:
            st.session_state.messages=[{"role":"assistant","content":"Hello! Ask me about COBOL programs, paragraphs, variables, or call flow."}]
        # display history
        for m in st.session_state.messages:
            with st.chat_message(m['role']):
                st.markdown(m['content'])
        if 'example_query' in st.session_state:
            query=st.session_state.example_query
            del st.session_state.example_query
        else:
            query=st.chat_input("Ask about the COBOL codebase...")
        if query:
            st.session_state.messages.append({"role":"user","content":query})
            with st.chat_message("user"):
                st.markdown(query)
            with st.chat_message("assistant"):
                with st.spinner("Searching..."):
                    try:
                        resp=self.rag_chain({"question":query})
                        ans=resp['answer']
                        st.markdown(ans)
                        st.session_state.messages.append({"role":"assistant","content":ans})
                    except Exception as e:
                        msg=f"Error: {e}"
                        st.error(msg)
                        st.session_state.messages.append({"role":"assistant","content":msg})

if __name__ == "__main__":
    app = COBOLChatbotApp()
    app.run()
