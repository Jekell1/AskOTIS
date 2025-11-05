"""Hybrid Retrieval - Semantic + Lexical search across indexes."""

import json
import logging
import re
import requests
from typing import List, Dict, Any
from openai import AzureOpenAI

# Set up logging
logger = logging.getLogger(__name__)


class HybridRetriever:
    """Hybrid retrieval combining semantic vector search and keyword search."""
    
    def __init__(self, config):
        """Initialize retriever with configuration."""
        self.config = config
        
        # Initialize Azure OpenAI for embeddings
        self.openai_client = AzureOpenAI(
            api_key=self.config.openai_key,
            api_version=self.config.openai_api_version,  # >>> FIX: Use config for consistency
            azure_endpoint=self.config.openai_endpoint
        )
        
        # Search API headers
        self.search_headers = {
            'Content-Type': 'application/json',
            'api-key': config.search_key
        }
    
    def weighted_rrf(self, runs: Dict[str, List[Dict]], weights: Dict[str, float] = None, k: int = 60) -> List[Dict]:
        """Weighted Reciprocal Rank Fusion for combining multiple result sets.
        
        Args:
            runs: Dict of result lists by name (e.g., {"lex": [...], "vec": [...]})
            weights: Dict of weights by run name (e.g., {"lex": 0.65, "vec": 0.35})
            k: RRF constant (default 60)
            
        Returns:
            Fused and sorted list of documents
        """
        if weights is None:
            weights = {}
        
        scores = {}
        keep = {}
        
        for name, items in runs.items():
            w = float(weights.get(name, 1.0))
            for rank1, it in enumerate(items, start=1):
                # >>> FIX: robust doc_id extraction
                doc_id = (
                    it.get('id') or 
                    it.get('key') or 
                    it.get('chunk_id') or
                    it.get('para_id') or 
                    it.get('paragraph_id') or 
                    it.get('program_id') or
                    it.get('screen_id') or
                    it.get('usage_id')
                )
                
                # Fallback for paragraphs
                if not doc_id and it.get('paragraph_name'):
                    doc_id = f"{it.get('program_id','')}|{it.get('paragraph_name','')}|{it.get('line_start','?')}"
                
                # Last resort: hash of text
                if not doc_id:
                    txt = (it.get('text') or it.get('summary_text') or it.get('source_excerpt') or '')[:128]
                    doc_id = f"anon::{hash(txt)}"
                
                # RRF scoring
                scores[doc_id] = scores.get(doc_id, 0.0) + w * (1.0 / (k + rank1))
                keep.setdefault(doc_id, it)
        
        # Sort by score descending
        return [keep[i] for i, _ in sorted(scores.items(), key=lambda kv: kv[1], reverse=True)]
    
    def _cross_encoder_rerank(self, query: str, documents: List[Dict], top_k: int = 20) -> List[Dict]:
        """
        >>> FIX: Cross-encoder reranking stub (for future deployment).
        
        If config.use_cross_encoder is True, reranks documents using a cross-encoder model
        and combines with original fusion scores (0.7 × rerank + 0.3 × fused).
        
        If False (default), returns documents unchanged with original scores.
        
        Args:
            query: User query
            documents: Fused documents with @search.score
            top_k: Number of top documents to rerank
            
        Returns:
            Reranked documents (or unchanged if cross-encoder disabled)
        """
        if not self.config.use_cross_encoder:
            # Cross-encoder disabled - return original scores
            logger.info("   🔄 Cross-encoder disabled (config.use_cross_encoder=False), using fused scores")
            return documents
        
        logger.info(f"   🔄 Cross-encoder enabled - reranking top {top_k} documents")
        
        # TODO: Implement actual cross-encoder reranking when needed
        # Placeholder logic:
        # 1. Take top_k documents
        # 2. Call cross-encoder model with (query, doc_text) pairs
        # 3. Get relevance scores from cross-encoder
        # 4. Combine: final_score = 0.7 × cross_encoder_score + 0.3 × original_fused_score
        # 5. Re-sort by final_score
        
        # For now, log that this is a stub and return unchanged
        logger.warning("   ⚠️ Cross-encoder reranking requested but not yet implemented - returning fused scores")
        return documents
    
    def _mine_corpus_tokens(self, candidates: List[Dict], max_tokens: int = 24) -> Dict[str, List[str]]:
        """Mine hyphen-codes, ALLCAPS tokens, and numbers from corpus candidates.
        
        >>> FIX: corpus-driven token mining (generic, no hardcoding)
        
        Args:
            candidates: List of candidate documents (from probing)
            max_tokens: Maximum tokens to return
            
        Returns:
            Dict with keys: hyphen, caps, nums
        """
        import re
        from collections import Counter
        
        _HYPHEN = re.compile(r'\b[A-Z0-9]+(?:-[A-Z0-9]+){1,}\b')  # CTY-SCAROLINA-18-42, INS-GAP-03
        _ALLCAPS = re.compile(r'\b[A-Z]{3,}\b')                   # SCRATE, COMPUTE, APR
        _NUM = re.compile(r'\b\d{1,4}\b')
        
        scores = Counter()
        ranked_fields = []
        
        for r, c in enumerate(candidates, start=1):
            pn = c.get('paragraph_name') or ''
            tx = c.get('text') or c.get('source_excerpt') or ''
            ranked_fields.append((pn, r))
            ranked_fields.append((tx, r))
        
        for s, rank in ranked_fields:
            if not s:
                continue
            inv = 1.0 / (10 + rank)
            for t in _HYPHEN.findall(s):
                scores[t] += 2.0 * inv
            for t in _ALLCAPS.findall(s):
                scores[t] += 1.0 * inv
            for t in _NUM.findall(s):
                scores[f'#{t}'] += 0.5 * inv
        
        # Deduplicate while preserving order
        def dd(seq):
            seen = set()
            out = []
            for x in seq:
                if x not in seen:
                    out.append(x)
                    seen.add(x)
            return out
        
        hy = dd([t for t, _ in scores.most_common() if '-' in t])[:12]
        cp = dd([t for t, _ in scores.most_common() if '-' not in t and t.isupper() and not t.startswith('#')])[:12]
        nm = dd([t[1:] for t, _ in scores.most_common() if t.startswith('#')])[:8]
        
        return {"hyphen": hy, "caps": cp, "nums": nm}
    
    def _probe_paragraphs(self, search_client_config: tuple, embedder, query_text: str, top: int = 120) -> List[Dict]:
        """Probe paragraphs index to gather corpus candidates for token mining.
        
        Args:
            search_client_config: (endpoint, headers, index_name) tuple
            embedder: Embedding function
            query_text: User query
            top: Number of candidates
            
        Returns:
            List of candidate paragraph docs
        """
        endpoint, headers, index_name = search_client_config
        url = f"{endpoint}/indexes/{index_name}/docs/search?api-version={self.config.search_api_version}"
        
        ranked = []
        r = 1
        
        # Vector probe
        try:
            qv = embedder(query_text)
            if qv:
                vec_body = {
                    "search": None,
                    "vectorQueries": [{
                        "kind": "vector",
                        "vector": qv,
                        "k": top,
                        "fields": "para_vector"
                    }],
                    "select": "paragraph_name,paragraph_name_terms,text,source_excerpt,program_id",
                    "top": top
                }
                vec_response = requests.post(url, headers=headers, json=vec_body, timeout=30)
                vec_response.raise_for_status()
                for d in vec_response.json().get('value', []):
                    ranked.append({
                        "paragraph_name": d.get("paragraph_name"),
                        "text": d.get("text") or d.get("source_excerpt"),
                        "program_id": d.get("program_id"),
                        "rank": r
                    })
                    r += 1
        except Exception as e:
            logger.warning(f"⚠️ Vector probe failed: {e}")
        
        # Optional small lexical probe
        try:
            lex_body = {
                "search": query_text,
                "queryType": "simple",
                "searchMode": "any",
                "select": "paragraph_name,paragraph_name_terms,text,source_excerpt,program_id",
                "top": min(40, top // 3)
            }
            lex_response = requests.post(url, headers=headers, json=lex_body, timeout=30)
            lex_response.raise_for_status()
            for d in lex_response.json().get('value', []):
                ranked.append({
                    "paragraph_name": d.get("paragraph_name"),
                    "text": d.get("text") or d.get("source_excerpt"),
                    "program_id": d.get("program_id"),
                    "rank": r
                })
                r += 1
        except Exception:
            pass
        
        return ranked
    
    def _dynamic_paragraph_expansions(self, query: str, initial_results: List[Dict]) -> tuple:
        """Generate query expansions by mining the corpus dynamically.
        
        >>> TIER A FIX 6: Dual probe strategy for more robust token mining (pattern-free)
        
        Args:
            query: User query
            initial_results: Initial search results (for program_id context)
            
        Returns:
            (expansions, mined) tuple
        """
        import re
        
        # Probe paragraphs index
        paragraphs_index = self.config.get_index_name('paragraphs')
        search_config = (self.config.search_endpoint, self.search_headers, paragraphs_index)
        
        # Probe 1: Full query (original)
        cand1 = self._probe_paragraphs(search_config, lambda q: self._generate_embedding(q, use_small_model=False), query, top=100)
        
        # Probe 2: Stripped query (keep alphanumerics ≥4 chars + all digits) - breadth-first, pattern-free
        # This helps capture rare labels even when user query is verbose
        stripped = ' '.join([w for w in re.findall(r'\w+', query) if (len(w) >= 4 or w.isdigit())])
        cand2 = []
        if stripped and stripped != query:
            cand2 = self._probe_paragraphs(search_config, lambda q: self._generate_embedding(q, use_small_model=False), stripped, top=80)
        
        # Merge and deduplicate candidates
        all_cand = cand1 + cand2
        seen_para = set()
        unique_cand = []
        for c in all_cand:
            pn = c.get('paragraph_name')
            if pn and pn not in seen_para:
                seen_para.add(pn)
                unique_cand.append(c)
        
        # Mine tokens from combined corpus (cap to 24 total)
        mined = self._mine_corpus_tokens(unique_cand, max_tokens=24)
        
        # Extract numbers from query for combination
        _NUM = re.compile(r'\b\d{1,4}\b')
        nums_in_q = _NUM.findall(query or "")
        
        expansions = set()
        
        # Add top hyphen tokens with quotes (exact match)
        for t in mined["hyphen"][:10]:
            expansions.add(f'"{t}"')
        
        # Add ALLCAPS tokens, combine with query numbers
        for c in mined["caps"][:10]:
            expansions.add(c)
            for n in nums_in_q[:3]:
                expansions.add(f'"{c}-{n}"')
        
        logger.info(f"🔍 [dynamic_expansions] mined tokens: hyphen={mined['hyphen'][:5]}, caps={mined['caps'][:5]}, nums={mined['nums'][:3]}")
        
        return sorted(expansions)[:24], mined
    
    def retrieve(self, query: str, indexes: List[str], max_results: int = 5, 
                 index_weights: Dict[str, float] = None, question_type: str = None) -> List[Dict[str, Any]]:
        """Retrieve relevant documents using hybrid search.
        
        Args:
            query: Search query
            indexes: List of index types to search (e.g., ['code', 'programs'])
            max_results: Maximum results per index  (default 5, increased by caller to 1000)
            index_weights: Optional dict of boost factors per index (e.g., {'screen_nodes': 3.0})
            question_type: Optional question type from router (for special handling)
        
        Returns:
            List of documents with metadata
        """
        import time
        start_retrieve = time.time()
        
        logger.info(f"📥 RETRIEVE CALLED: question_type={question_type}, indexes={indexes}")
        
        # Special handling for implementation/calculation/formula questions (increase depth)
        if question_type == 'implementation':
            # Implementation questions need MORE results to find the right copybooks/formulas
            if max_results < 30:
                max_results = 30  # Minimum 30 results for implementation questions (120 total across 4 indexes)
                logger.info(f"🔧 IMPLEMENTATION QUERY - increasing max_results to {max_results}")
        
        # Special handling for transaction-specific copybook questions (deterministic)
        if question_type == 'transaction_copybooks':
            logger.info("🎯 DETERMINISTIC MODE: Transaction copybook query detected")
            deterministic_results = self._handle_transaction_copybooks(query, max_results)
            
            # If deterministic handler returns empty, fall back to semantic search
            if not deterministic_results:
                logger.warning("⚠️ Deterministic handler returned empty - falling back to semantic search")
                # Continue with normal semantic search below
            else:
                logger.info(f"✅ Deterministic handler returned {len(deterministic_results)} results")
                return deterministic_results
        
        # 🔹 Special handling for copybook usage queries (maximize recall)
        # Pattern: "what programs use LPBW1IN.CPY" - needs ALL matches, not top-K semantic
        query_lower = query.lower()
        is_copybook_usage_query = ('.cpy' in query_lower and 
                                    ('use' in query_lower or 'used' in query_lower or 'reference' in query_lower))
        
        if is_copybook_usage_query:
            logger.info("📄 COPYBOOK USAGE QUERY DETECTED - maximizing recall")
        
        # 🔹 Special handling for menu queries - search MORE screens to find the right menu
        # Menu queries often need to retrieve 30-50 screens to find parent menus vs submenus
        search_top = max_results
        if question_type == 'menu' and 'screen_nodes' in indexes:
            search_top = max(50, max_results * 3)  # Search 3x more screens for menu queries
            logger.info(f"🎯 MENU QUERY - expanding search from {max_results} to {search_top} to find correct menu level")
        
        all_results = []
        filtered_relationship_results = []  # Separate bucket for filtered structural data
        
        # Default weights to 1.0 if not provided
        if index_weights is None:
            index_weights = {}
        
        # Determine which embedding dimensions we need based on indexes
        needs_large_model = any(idx not in ('screen_nodes', 'screens') for idx in indexes)
        needs_small_model = 'screen_nodes' in indexes or 'screens' in indexes
        
        # Generate query embeddings
        query_vector = None  # 3072-dim for most indexes
        query_vector_1536 = None  # 1536-dim for screen_nodes
        
        if needs_large_model:
            start_embed = time.time()
            query_vector = self._generate_embedding(query, use_small_model=False)
            time_embed = time.time() - start_embed
            logger.info(f"⏱️ TIMING: Large embedding generation (3072-dim) took {time_embed:.3f}s")
            if not query_vector:
                logger.error("Failed to generate large model query embedding")
        
        if needs_small_model:
            start_embed_1536 = time.time()
            query_vector_1536 = self._generate_embedding(query, use_small_model=True)
            time_embed_1536 = time.time() - start_embed_1536
            logger.info(f"⏱️ TIMING: Small embedding generation (1536-dim) took {time_embed_1536:.3f}s")
            if not query_vector_1536:
                logger.warning("Failed to generate small model embedding for screen_nodes")
        
        # Check if we have at least one valid embedding
        if not query_vector and not query_vector_1536:
            logger.error("Failed to generate any query embeddings, returning empty results")
            return []
        
        # Search each index
        for index_type in indexes:
            start_index_search = time.time()
            
            index_name = self.config.get_index_name(index_type)
            weight = index_weights.get(index_type, 1.0)  # Get weight, default 1.0
            
            # Use 1536-dim vector for screen_nodes and screens, standard 3072-dim for others
            vector_to_use = query_vector_1536 if index_type in ('screen_nodes', 'screens') else query_vector
            
            # Skip if vector is empty (screens/screen_nodes with failed 1536-dim embedding)
            if not vector_to_use:
                logger.warning(f"Skipping {index_type} - no valid embedding vector")
                continue
            
            results = self._search_index(
                index_name=index_name,
                query=query,
                query_vector=vector_to_use,
                top=search_top if index_type == 'screen_nodes' else max_results,  # Use expanded top for screen_nodes in menu queries
                is_copybook_usage_query=is_copybook_usage_query
            )
            
            time_index_search = time.time() - start_index_search
            logger.info(f"⏱️ TIMING: Search {index_type} took {time_index_search:.3f}s ({len(results)} results)")
            
            # Check if these are filtered relationship results (program_name extracted, filter applied)
            program_name = self._extract_program_name(query)
            is_filtered_relationship = (
                (index_name == 'new_cobol_copybook_usage' and program_name) or
                (index_name == 'new_cobol_calls' and program_name)
                # Add other relationship indexes here as needed
            )
            
            # Add index type and apply weight to score
            for result in results:
                result['_index_type'] = index_type
                result['_is_filtered_relationship'] = is_filtered_relationship
                # Apply weight to search score for ranking
                if '@search.score' in result:
                    original_score = result['@search.score']
                    result['@search.score'] = original_score * weight
                    # Log score boosting for first few results
                    if results.index(result) < 3:
                        logger.info(f"   Score boost for {index_type}: {original_score:.4f} * {weight} = {result['@search.score']:.4f}")
                result['_weight_applied'] = weight
            
            # Separate filtered relationships from semantic search results
            if is_filtered_relationship:
                logger.info(f"   ✅ Preserving {len(results)} filtered relationship results from {index_type}")
                filtered_relationship_results.extend(results)
            else:
                all_results.extend(results)
        
        # Deduplicate and rank by relevance
        # For queries requesting "all" items, return everything (up to max_results)
        # For targeted queries, keep half for focused results
        query_lower = query.lower()
        # Use very specific detection for comprehensive TOP-LEVEL entity lists only
        # Don't trigger for "all fields in X" - those need focused semantic results
        wants_all = (
            ('list all' in query_lower and ('program' in query_lower or 'copybook' in query_lower or 'file' in query_lower)) or
            ('show all' in query_lower and ('program' in query_lower or 'copybook' in query_lower or 'file' in query_lower)) or
            ('every program' in query_lower) or
            ('every copybook' in query_lower) or
            ('all programs' in query_lower and 'in' not in query_lower) or
            ('all copybooks' in query_lower and 'in' not in query_lower) or
            ('all files' in query_lower and 'in' not in query_lower)
        )
        
        # 🔹 For copybook usage queries, treat like "wants_all" (return everything)
        if is_copybook_usage_query:
            wants_all = True
            logger.info(f"📄 Copybook usage query - returning all results (no cap)")
        
        if wants_all:
            # Return all unique results, up to max_results limit
            final_count = max_results
        else:
            # Dynamic cap based on max_results (which caller sets based on question type)
            # >>> TUNE: Increased dynamic cap from 200 to 500 to support higher retrieval targets
            # For specific queries (max_results=120): return up to 240 docs
            # For general queries (max_results=150): return up to 300 docs  
            # For complex queries (max_results=300): return up to 500 docs
            raw_half = len(all_results) // 2
            dynamic_cap = min(max_results * 2, self.config.max_fused_docs)  # 2x the per-index limit, max 500
            final_count = min(dynamic_cap, max(10, raw_half))  # At least 10, up to dynamic cap
        
        logger.info(f"📄 Retrieved {len(all_results)} semantic results + {len(filtered_relationship_results)} filtered relationship results")
        
        # � MULTI-HOP RETRIEVAL for implementation queries: Follow COPY statements and variable references
        if question_type == 'implementation' and all_results:
            logger.info("🔗 MULTI-HOP: Starting multi-hop relationship traversal")
            multi_hop_results = self._multi_hop_implementation_search(all_results, query)
            if multi_hop_results:
                logger.info(f"✅ Multi-hop found {len(multi_hop_results)} additional chunks")
                all_results.extend(multi_hop_results)
        
        # �🔹 ENTITY EXTRACTION for implementation queries: Find mentioned copybooks/programs and fetch them
        if question_type == 'implementation' and all_results:
            logger.info("🔍 ENTITY EXTRACTION: Looking for copybook/program references in results")
            additional_results = self._extract_and_fetch_entities(all_results, query)
            if additional_results:
                logger.info(f"✅ Found {len(additional_results)} additional chunks via entity extraction")
                all_results.extend(additional_results)
        
        # 🔹 EXPLICIT MENU FETCHING: If menu query doesn't have screens with target menu name, fetch explicitly
        # This handles cases where hybrid search doesn't rank submenu screens highly enough
        if 'screen_nodes' in indexes and question_type == 'menu':
            logger.info(f"🎯 EXPLICIT MENU FETCHING: Starting check for question_type={question_type}")
            query_lower = query.lower()
            
            # Classify query to know if we want parent or submenu
            query_type = self._classify_menu_query(query)
            
            # Check if we have screens matching the menu type in results
            # >>> FIX: Only use hardcoded IDs if config flag enabled (default OFF for corpus-driven)
            menu_patterns = {
                'collection': (r'COLLECTION', ['04C134294DA40F2A18EBA019EBA43F4F814D48FA_1', '69B4EBD83CBF78D027749E34BE5CD143ECDF01CC_1'] if self.config.use_hardcoded_menu_ids else []),
                'daily processing': (r'DAILY\s+PROCESSING\s+MENU', []),
                'report': (r'REPORTS?\s+MENU', []),
                'inquir': (r'INQUIR', []),
                'batch': (r'BATCH', []),
            }
            
            target_pattern = None
            known_ids = []
            for keyword, (pattern, ids) in menu_patterns.items():
                if keyword in query_lower:
                    target_pattern = pattern
                    known_ids = ids
                    break
            
            if target_pattern:
                import re
                logger.info(f"🔍 Checking for menu pattern: {target_pattern}, known IDs: {known_ids}")
                
                # Check if our KNOWN menu screens (the ones we want) are in results
                has_known_menu = known_ids and any(
                    r.get('screen_id') in known_ids
                    for r in all_results[:30]  # Check top 30
                )
                
                logger.info(f"📊 Known menu in top 30: {has_known_menu}")
                
                # ALWAYS explicitly fetch for Collection menus since filtering is unreliable
                # Even if they're in top 30, they may be filtered out or ranked poorly
                should_fetch = not has_known_menu or 'collection' in query_lower
                
                if should_fetch and known_ids:
                    logger.info(f"🎯 Fetching menu screens explicitly for '{target_pattern}'")
                    try:
                        index_name = self.config.get_index_name('screen_nodes')
                        url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version={self.config.search_api_version}"
                        
                        # Build filter for known IDs
                        filter_parts = [f"screen_id eq '{sid}'" for sid in known_ids]
                        filter_str = " or ".join(filter_parts)
                        
                        body = {
                            "search": "*",
                            "filter": filter_str,
                            "top": len(known_ids)
                        }
                        
                        response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                        response.raise_for_status()
                        explicit_docs = response.json().get('value', [])
                        
                        if explicit_docs:
                            # Count options and classify screens
                            for doc in explicit_docs:
                                summary = doc.get('summary_text', '')
                                option_matches = re.findall(r'\b(\d{1,2})\.\s+[A-Z]', summary)
                                doc['_option_count'] = len(option_matches)
                                
                                # Check for FILE MAINTENANCE indicators
                                doc['_is_file_maint'] = bool(re.search(r'FILE.*MAINT', summary, re.IGNORECASE))
                            
                            # Pick screen based on query type
                            if query_type == 'submenu':
                                # For submenu queries, prefer screens with MORE options and FILE MAINT
                                explicit_docs.sort(key=lambda x: (-x.get('_is_file_maint', False), -x.get('_option_count', 0)))
                                logger.info(f"🗂️ SUBMENU query: picking screen with MORE options")
                            else:
                                # For parent queries, prefer screens with FEWER options
                                explicit_docs.sort(key=lambda x: x.get('_option_count', 99))
                                logger.info(f"📋 PARENT query: picking screen with FEWER options")
                            
                            best_menu = explicit_docs[0]
                            logger.info(f"✅ Explicitly fetched: {best_menu.get('screen_id', 'N/A')[:40]} (options={best_menu.get('_option_count')})")
                            
                            best_menu['_index_type'] = 'screen_nodes'
                            best_menu['_is_filtered_relationship'] = False
                            best_menu['@search.score'] = 100.0
                            best_menu['_weight_applied'] = 30.0
                            best_menu['_explicit_fetch'] = True
                            all_results.insert(0, best_menu)
                            logger.info(f"✅ Explicitly fetched menu {best_menu['screen_id']} with {best_menu['_option_count']} options")
                        else:
                            logger.warning(f"❌ Known menu IDs not found: {known_ids}")
                    except Exception as e:
                        logger.error(f"Failed to fetch explicit menu: {e}")
        
        # ✨ NEW: Classify and rerank menu screens for better parent vs submenu distinction
        if 'screen_nodes' in indexes and question_type == 'menu':
            # Classify query intent (parent vs submenu)
            query_type = self._classify_menu_query(query)
            
            # Apply post-search reranking based on structural patterns
            all_results = self._rerank_menu_screens(all_results, query_type, query)
            logger.info(f"🎯 After reranking for {query_type}: {len(all_results)} results")
            
            # Then apply traditional filtering to remove non-menu screens
            all_results = self._filter_menu_screens(all_results, query)
            logger.info(f"📋 After menu filtering: {len(all_results)} results")
            
            # >>> FIX: lexical sentinel for MASTER MENU promotion
            promoted = False
            for d in all_results[:5]:
                fields = ' '.join([
                    d.get('title_normalized', ''),
                    d.get('summary_text', ''),
                    d.get('options_bag', '')
                ]).upper()
                
                # Canonical master menu indicators
                canonical = [
                    'MASTER MENU', 'DAILY PROCESSING', 'REPORTS', 'INQUIRIES',
                    'COLLECTION PROCESSING', 'BATCH PROCESSING', 'END OF DAY',
                    'END OF MONTH', 'END OF YEAR', 'SPECIAL PROCEDURES', 'OPTIONAL MODULES'
                ]
                hits = sum(1 for k in canonical if k in fields)
                
                if 'MASTER MENU' in fields or hits >= 3:
                    d['@search.score'] = d.get('@search.score', 0.0) + 10.0
                    promoted = True
                    logger.info(f"📌 Promoted MASTER MENU screen: {d.get('screen_id', 'unknown')}")
                    break
            
            if promoted:
                all_results = sorted(all_results, key=lambda x: x.get('@search.score', 0), reverse=True)
        
        # Rank semantic search results normally
        semantic_results = self._rank_results(all_results, final_count, question_type=question_type)
        
        # Combine: filtered relationships come FIRST (complete, unranked by semantic score)
        # Then add semantic results to provide additional context
        if filtered_relationship_results:
            logger.info(f"🔗 Preserving ALL {len(filtered_relationship_results)} filtered relationship results")
            # Sort relationships by metadata (line number if available) instead of semantic score
            sorted_relationships = sorted(
                filtered_relationship_results,
                key=lambda x: (x.get('program_id', ''), x.get('line_number', 0))
            )
            
            # If we have filtered relationships, they take priority
            # Combine them with semantic results, but cap total to reasonable limit
            remaining_budget = max(final_count - len(sorted_relationships), 0)
            logger.info(f"📊 Returning {len(sorted_relationships)} relationships + {remaining_budget} semantic results")
            
            final_results = sorted_relationships + semantic_results[:remaining_budget]
        else:
            # No filtered relationships, just use semantic results
            final_results = semantic_results
        
        # Auto-expand submenus if this is a menu query asking for levels
        if 'screen_nodes' in indexes and any(term in query.lower() for term in ['menu', 'level', 'submenu']):
            final_results = self._expand_submenus(final_results, query)
        
        return final_results
    
    
    def _classify_menu_query(self, query: str) -> str:
        """Classify menu query to determine if user wants parent menu or submenu.
        
        Args:
            query: User's menu query
            
        Returns:
            'submenu' - User wants detailed submenu (file maintenance, setup, configuration)
            'parent' - User wants parent/main menu (processing, main menu)
            'generic' - Unclear, use neutral approach
        """
        q = query.lower()
        
        # Submenu indicators - user wants detailed/file-level operations
        submenu_keywords = ['file', 'maintenance', 'setup', 'configuration', 
                           'extraction', 'action code', 'activity code']
        if any(kw in q for kw in submenu_keywords):
            logger.info(f"📂 Classified as SUBMENU query (found: {[k for k in submenu_keywords if k in q]})")
            return 'submenu'
        
        # Parent indicators - user wants top-level menu
        parent_keywords = ['processing', 'main', 'master', 'menu options', 'what\'s in']
        if any(kw in q for kw in parent_keywords):
            logger.info(f"📋 Classified as PARENT query (found: {[k for k in parent_keywords if k in q]})")
            return 'parent'
        
        logger.info("🔍 Classified as GENERIC menu query (no clear parent/submenu indicators)")
        return 'generic'
    
    
    def _rerank_menu_screens(self, results: List[Dict[str, Any]], query_type: str, query: str) -> List[Dict[str, Any]]:
        """Rerank menu screens based on structural patterns and query intent.
        
        This addresses the root cause: hybrid search returns wrong screens because:
        - Vector embeddings are weak for structured menu text
        - Keyword search fragments across unrelated mentions
        - Sorting by "fewer options" assumes parent menus have fewer options (often wrong!)
        
        Solution: Post-search reranking based on:
        - Query type (parent vs submenu)
        - Option count (more options for submenu, fewer for parent)
        - Content match (presence of specific terms)
        
        Args:
            results: List of screen documents from hybrid search
            query_type: 'parent', 'submenu', or 'generic'
            query: Original query for content matching
            
        Returns:
            Reranked results with adjusted scores
        """
        if not results:
            return results
        
        import re
        query_lower = query.lower()
        
        def calculate_rerank_score(doc: Dict[str, Any]) -> float:
            """Calculate reranking score for a screen document."""
            base_score = doc.get('@search.score', 0.0)
            summary = doc.get('summary_text', '').lower()
            
            # Count menu options
            option_matches = re.findall(r'\b(\d{1,2})\.\s+[A-Z]', doc.get('summary_text', ''))
            option_count = len(option_matches)
            
            # Start with base hybrid search score
            score = base_score
            
            # 🔹 Structural heuristics based on query type
            if query_type == 'parent':
                # Parent menus typically have FEWER options
                if option_count > 0:
                    score += (1.0 / option_count) * 10.0  # Boost screens with fewer options
                logger.debug(f"  Parent boost: +{(1.0 / max(option_count, 1)) * 10.0:.3f} (options={option_count})")
                
            elif query_type == 'submenu':
                # Submenus typically have MORE options (detail screens)
                score += (option_count / 20.0) * 10.0  # Boost screens with more options
                logger.debug(f"  Submenu boost: +{(option_count / 20.0) * 10.0:.3f} (options={option_count})")
            
            # 🔹 Content-based boosting
            # Submenu indicators
            if 'file' in summary and 'maint' in summary:
                if query_type == 'submenu':
                    score += 15.0
                    logger.debug(f"  Content boost: +15.0 (FILE MAINT match for submenu)")
                else:
                    score -= 5.0  # Penalize if looking for parent
                    
            # Parent menu indicators
            if 'work screen' in summary or 'queue listing' in summary:
                if query_type == 'parent':
                    score += 15.0
                    logger.debug(f"  Content boost: +15.0 (WORK SCREEN/QUEUE match for parent)")
                elif query_type == 'submenu':
                    score -= 5.0  # Penalize if looking for submenu
            
            # Check for specific query terms in summary
            specific_terms = []
            if 'collection' in query_lower:
                specific_terms.append('collection')
            if 'daily' in query_lower:
                specific_terms.append('daily')
            if 'report' in query_lower:
                specific_terms.append('report')
                
            term_matches = sum(1 for term in specific_terms if term in summary)
            if term_matches > 0:
                score += term_matches * 5.0
                logger.debug(f"  Term match boost: +{term_matches * 5.0:.3f}")
            
            return score
        
        # Calculate rerank scores
        for doc in results:
            doc['_rerank_score'] = calculate_rerank_score(doc)
        
        # Sort by rerank score
        reranked = sorted(results, key=lambda x: x.get('_rerank_score', 0.0), reverse=True)
        
        logger.info(f"🎯 Reranked {len(reranked)} screens for {query_type} query")
        if reranked:
            logger.info(f"  Top result: {reranked[0].get('screen_id', 'N/A')[:40]} (rerank_score={reranked[0].get('_rerank_score', 0):.2f})")
        
        return reranked
    
    
    def _filter_menu_screens(self, results: List[Dict[str, Any]], query: str) -> List[Dict[str, Any]]:
        """Filter menu screen results to prioritize actual menu content over navigation screens.
        
        When searching for menus, many results contain "F7 - MASTER MENU" which is just
        a navigation link back to the menu from submenus. This filters to prioritize
        screens that have actual menu options (numbered items like "1. DAILY PROCESSING").
        
        Args:
            results: List of screen_nodes results
            query: Original query
            
        Returns:
            Filtered and reordered results with actual menus first
        """
        import re
        
        primary_menus = []  # Screens that are likely the actual requested menu
        secondary_menus = []  # Other menu screens with options
        navigation_screens = []  # Screens with only F-key navigation
        
        query_lower = query.lower()
        is_main_menu_query = 'main' in query_lower and 'menu' in query_lower
        
        # Extract specific menu type from query for targeted matching
        menu_type_keywords = {
            'daily processing': r'DAILY\s+PROCESSING\s+MENU',
            'report': r'REPORTS?\s+MENU',
            'inquir': r'INQUIR(Y|IES)\s+MENU',
            'collection': r'COLLECTION\s+(PROCESSING\s+)?MENU',
            'batch': r'BATCH\s+(PROCESSING\s+)?MENU',
            'optional': r'OPTIONAL\s+MODULES',
            'addon': r'ADD.?ON|INSURANCE|DEALER'
        }
        
        target_menu_pattern = None
        for keyword, pattern in menu_type_keywords.items():
            if keyword in query_lower:
                target_menu_pattern = pattern
                break
        
        for result in results:
            summary = result.get('summary_text', '')
            summary_upper = summary.upper()
            
            # Extract all numbered menu options (1. through 99.)
            option_matches = re.findall(r'\b(\d{1,2})\.\s+[A-Z]', summary)
            numbered_options = len(option_matches)
            
            # Check if menu starts at option 1 (strong indicator of top-level menu)
            starts_at_one = option_matches and option_matches[0] == '1'
            
            # Check for F-key navigation patterns (just for nav, not content)
            has_f_key_only = bool(re.search(r'F\d+\s*-\s*(MASTER\s+)?MENU', summary, re.IGNORECASE)) and numbered_options < 2
            
            # Check if this mentions "MASTER MENU" or similar as content (not just F-key)
            has_master_menu_content = bool(re.search(r'M\s*A\s*S\s*T\s*E\s*R\s+(M|ME|MEN|MENU)', summary, re.IGNORECASE))
            
            # Check if this matches the specific menu type being queried
            matches_target_menu = target_menu_pattern and bool(re.search(target_menu_pattern, summary_upper, re.IGNORECASE))
            
            # Classify the screen - PRIORITIZE MENUS THAT START AT OPTION 1
            if starts_at_one and matches_target_menu:
                # Starts at 1 AND matches query = TOP PRIORITY (even if few options)
                primary_menus.append(result)
                logger.debug(f"  ✓✓✓✓ TOP-LEVEL menu: {result.get('screen_id', '')[:20]}... (starts at 1 + matches query + {numbered_options} options)")
            elif starts_at_one and numbered_options >= 2:
                # Starts at 1 with multiple options = likely top-level menu
                primary_menus.append(result)
                logger.debug(f"  ✓✓✓ TOP-LEVEL menu: {result.get('screen_id', '')[:20]}... (starts at 1 + {numbered_options} options)")
            elif matches_target_menu and numbered_options >= 2:
                # Query for specific menu + screen has that menu name + has options = GOOD match (might be submenu)
                primary_menus.append(result)
                logger.debug(f"  ✓✓ TARGET menu: {result.get('screen_id', '')[:20]}... (matches query + {numbered_options} options)")
            elif is_main_menu_query and has_master_menu_content and numbered_options >= 2:
                # Main menu query + explicit "MASTER MENU" text + multiple options = PRIMARY match
                primary_menus.append(result)
                logger.debug(f"  ✓✓ PRIMARY menu: {result.get('screen_id', '')[:20]}... (MASTER MENU + {numbered_options} options)")
            elif numbered_options >= 6:
                # >>> FIX: Remove duplicate append - was causing double entries
                # Has many numbered options - likely a real menu screen (raised threshold to avoid submenus)
                primary_menus.append(result)
                logger.debug(f"  ✓ Primary menu: {result.get('screen_id', '')[:20]}... ({numbered_options} options)")
            elif numbered_options >= 2:
                # Has some options - secondary priority
                secondary_menus.append(result)
                logger.debug(f"  ~ Secondary menu: {result.get('screen_id', '')[:20]}... ({numbered_options} options)")
            elif has_f_key_only:
                # Only has F-key navigation - likely just a submenu screen linking back
                navigation_screens.append(result)
                logger.debug(f"  ✗ Navigation screen: {result.get('screen_id', '')[:20]}... (F-key only)")
            else:
                # Unclear - put in secondary to be safe
                secondary_menus.append(result)
        
        logger.info(f"📋 Menu filtering: {len(primary_menus)} primary, {len(secondary_menus)} secondary, {len(navigation_screens)} navigation screens")
        
        # IMPORTANT: Sort primary menus to prioritize parent menus over detail screens
        # When query matches (e.g., "collection processing"), FEWER options = parent menu, MORE options = detail submenu
        # Store option count for sorting
        for menu in primary_menus:
            summary = menu.get('summary_text', '')
            option_matches = re.findall(r'\b(\d{1,2})\.\s+[A-Z]', summary)
            menu['_option_count'] = len(option_matches)
            menu['_starts_at_one'] = option_matches and option_matches[0] == '1'
        
        # Sort: menus starting at 1 first, then by FEWER options (parent menus), then by search score
        primary_menus.sort(key=lambda x: (
            not x.get('_starts_at_one', False),  # Menus starting at 1 come first
            x.get('_option_count', 99),           # Then sort by fewer options (parent menus)
            -x.get('@search.score', 0)             # Then by search relevance
        ))
        
        logger.info(f"📊 Sorted primary menus by: starts_at_1 > fewer_options > score")
        
        # Return primary menus first (most relevant), then secondary, then navigation
        return primary_menus + secondary_menus + navigation_screens

    def _expand_submenus(self, results: List[Dict[str, Any]], query: str) -> List[Dict[str, Any]]:
        """Automatically fetch linked submenus when user asks for multiple levels.
        
        Args:
            results: Initial search results containing menu screens
            query: Original user query
            
        Returns:
            Expanded results including submenu screens
        """
        import re
        
        # Check if query mentions levels or requests comprehensive menu structure
        query_lower = query.lower()
        wants_levels = any(term in query_lower for term in ['2 level', 'two level', 'all level', 'submenu', 'sub-menu', 'sub menu'])
        
        if not wants_levels:
            return results
        
        logger.info(f"🔗 Query requests menu levels - expanding submenus...")
        
        expanded_results = list(results)  # Start with original results
        submenu_ids = set()
        
        # Extract F-key and numeric options that might be submenus
        for doc in results:
            if doc.get('_index_type') != 'screen_nodes':
                continue
            
            # Check fields that might contain menu option text
            raw_text = doc.get('raw_span_text', '')
            summary = doc.get('summary_text', '')
            actions_json = doc.get('actions_json', '')
            
            # Look for F-key menu references (F6, F7, F8, etc.) followed by "MENU"
            fkey_pattern = r'(F\d+)\s*-?\s*([A-Z\s]+MENU)'
            for match in re.finditer(fkey_pattern, raw_text + ' ' + summary, re.IGNORECASE):
                menu_name = match.group(2).strip()
                logger.info(f"   Found submenu reference: {match.group(1)} - {menu_name}")
                submenu_ids.add(menu_name)
        
        # Search for identified submenus
        if submenu_ids:
            logger.info(f"   Fetching {len(submenu_ids)} linked submenus...")
            for menu_name in list(submenu_ids)[:5]:  # Limit to 5 submenus to avoid explosion
                try:
                    submenu_results = self._fetch_menu_by_name(menu_name)
                    if submenu_results:
                        logger.info(f"   ✓ Found submenu: {menu_name} ({len(submenu_results)} screens)")
                        # Mark as submenu for formatting
                        for sub_doc in submenu_results:
                            sub_doc['_is_submenu'] = True
                            sub_doc['_submenu_name'] = menu_name
                        expanded_results.extend(submenu_results)
                except Exception as e:
                    logger.warning(f"   Failed to fetch submenu {menu_name}: {e}")
        
        logger.info(f"📄 Expanded from {len(results)} to {len(expanded_results)} documents (including submenus)")
        return expanded_results
    
    def _fetch_menu_by_name(self, menu_name: str) -> List[Dict[str, Any]]:
        """Fetch a specific menu screen by name.
        
        Args:
            menu_name: Name of the menu (e.g., "COLLECTION MENU")
            
        Returns:
            List of matching screen documents
        """
        index_name = self.config.get_index_name('screen_nodes')
        
        # Simple keyword search for the menu name
        search_url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version={self.config.search_api_version}"
        
        payload = {
            "search": menu_name,
            "top": 2,  # Usually only 1-2 screens per menu
            "searchMode": "all"
        }
        
        try:
            response = requests.post(search_url, headers=self.search_headers, json=payload, timeout=10)
            response.raise_for_status()
            data = response.json()
            return data.get('value', [])
        except Exception as e:
            logger.warning(f"Failed to fetch menu {menu_name}: {e}")
            return []
    
    def _generate_embedding(self, text: str, use_small_model: bool = False) -> List[float]:
        """Generate embedding for text using the appropriate model.
        
        Args:
            text: Text to embed
            use_small_model: If True, use small model (1536-dim) for screen_nodes.
                           If False, use large model (3072-dim) for other indexes.
            
        Returns:
            List of floats representing the embedding, or None if generation fails
        """
        try:
            # Select the appropriate embedding deployment
            if use_small_model:
                model = self.config.embed_deployment_small
                expected_dim = 1536
                logger.info(f"🧩 Using small embedding model ({model}) for screen_nodes (1536-dim)")
            else:
                model = self.config.embed_deployment_large
                expected_dim = 3072
                logger.info(f"🧩 Using large embedding model ({model}) for indexes (3072-dim)")
            
            params = {
                "input": text,
                "model": model
            }
            
            response = self.openai_client.embeddings.create(**params)
            embedding = response.data[0].embedding
            
            if embedding is None:
                logger.warning(f"Embedding generation returned None for text: {text[:50]}...")
                return []
            
            # Verify dimensions match expectations
            actual_dim = len(embedding)
            if actual_dim != expected_dim:
                logger.warning(f"⚠️ Embedding dimension mismatch: expected {expected_dim}, got {actual_dim}")
                
            return embedding
        except Exception as e:
            logger.error(f"Failed to generate embedding: {e}")
            return []
    
    def _is_program_id(self, name: str) -> bool:
        """Return True if name is a valid COBOL program identifier.
        
        Valid program IDs:
        - Short names (<= 8 chars, alphanumeric, no extension) like LONPF2, APIPAY
        - Names with .CBL extension like LONPF2.CBL
        
        Invalid program IDs:
        - Copybook names (.CPY) like LPBW1IN.CPY
        - Screen/menu identifiers (-MENU, -SCREEN, -PATH)
        - Empty or None values
        
        Args:
            name: Potential program name to validate
            
        Returns:
            True if this is a valid program identifier for filtering
        """
        if not name:
            return False
        
        n = name.upper()
        
        # Reject copybooks
        if n.endswith('.CPY'):
            return False
        
        # Reject screen/menu/path identifiers
        if n.endswith('-MENU') or n.endswith('-SCREEN') or n.endswith('-PATH'):
            return False
        
        # Accept .CBL files
        if n.endswith('.CBL'):
            return True
        
        # Accept short alphanumeric names (standard COBOL program ID format)
        # COBOL program names are max 8 characters (COBOL-74/85 standard)
        return len(n) <= 8 and n.replace('-', '').isalnum()
    
    def _extract_program_name(self, query: str) -> str:
        """Extract program name from query if present.
        
        Returns None for copybook names (.CPY files) and invalid identifiers
        to prevent them from being treated as program names for filtering.
        """
        import re
        # Look for program names (uppercase words, possibly with slashes, hyphens, or extensions)
        # Skip common question words
        skip_words = {'WHAT', 'DOES', 'CALL', 'USE', 'REFERENCE', 'PROGRAM', 'PROGRAMS', 'COPYBOOK', 'COPYBOOKS'}
        
        patterns = [
            # Pattern includes optional hyphens to match LOAN-MENU, MAIN-SCREEN, etc.
            r'\b([A-Z][A-Z0-9-]{3,}(?:/[A-Z0-9]+)?(?:\.CBL|\.CPY)?)\b',  # At least 4 chars: APIPAY, LOAN-MENU, LP/LONPF2, etc.
        ]
        for pattern in patterns:
            matches = re.findall(pattern, query)
            for match in matches:
                if match.upper() not in skip_words:
                    # Validate it's a true program identifier, not a copybook or screen
                    if self._is_program_id(match):
                        return match.upper()
                    else:
                        logger.info(f"   ⚠️ Skipping non-program identifier: {match}")
                        continue
        return None
    
    def _enhance_query_for_business_logic(self, query: str, index_name: str) -> str:
        """Enhance query to boost business logic terms over data definitions.
        
        When users ask about modifications/actions, they want to know business constraints
        (e.g., MAX=3 means only 3 fields modifiable), not all screen field definitions.
        
        For menu queries, boosts specific menu name terms to improve keyword search accuracy.
        
        This method boosts constraint and logic keywords to surface business rules first.
        """
        import re
        query_lower = query.lower()
        
        # 🔹 MENU QUERY ENHANCEMENT: Add exact menu name terms to improve keyword search
        # This ensures hybrid search finds the right menu even if semantic similarity is low
        if index_name == 'new_cobol_screen_nodes' and 'menu' in query_lower:
            # Extract menu name from query and add exact terms from screen content
            menu_boost_terms = []
            
            # Main/Master/Login menu
            if any(term in query_lower for term in ['main menu', 'master menu', 'login', 'lpmenu']):
                menu_boost_terms.extend(['MASTER MENU', 'LPMENU', 'DAILY PROCESSING', 'REPORTS', 
                                        'INQUIRIES', 'COLLECTION PROCESSING', 'BATCH PROCESSING',
                                        'END OF DAY', 'END OF MONTH', 'END OF YEAR'])
            
            # Daily Processing menu
            elif 'daily processing' in query_lower:
                menu_boost_terms.extend(['DAILY PROCESSING MENU', 'INQUIRIES AND ENTRY', 
                                        'PAYMENT INQUIRY', 'CONTRACT INQUIRY'])
            
            # Reports menu
            elif 'report' in query_lower and 'menu' in query_lower:
                menu_boost_terms.extend(['REPORTS MENU', 'GENERAL REPORTS', 'PORTFOLIO REPORTS',
                                        'FINANCIAL REPORTS', 'DAILY REPORTS'])
            
            # Inquiries menu
            elif 'inquir' in query_lower:
                menu_boost_terms.extend(['INQUIRIES MENU', 'INQUIRY', 'PAYMENT INQUIRY',
                                        'CONTRACT INQUIRY', 'CUSTOMER INQUIRY'])
            
            # Collection FILE MAINTENANCE submenu (more specific - check first!)
            elif 'collection' in query_lower and 'file' in query_lower and 'maintenance' in query_lower:
                menu_boost_terms.extend(['EXTRACTION', 'ACTION CODE FILE', 'ACTIVITY CODE FILE',
                                        'RESULT CODE FILE', 'COLLECTOR QUEUE FILE', 
                                        'COLLECTOR POOL FILE', 'ACTION CODE 2 FILE'])
            
            # Collection Processing/Maintenance menu (parent level)
            elif 'collection' in query_lower:
                menu_boost_terms.extend(['COLLECTION WORK SCREEN', 'COLLECTOR QUEUE LISTING',
                                        'COLLECTOR FILE MAINTENANCE', 'COLLECTOR POOL MAINTENANCE',
                                        'GROUP MAINTENANCE', 'TIME ZONE FILE'])
            
            # Batch Processing menu
            elif 'batch' in query_lower:
                menu_boost_terms.extend(['BATCH PROCESSING', 'BATCH MENU', 'BATCH JOBS',
                                        'BATCH RUN'])
            
            # Optional Modules menu
            elif 'optional' in query_lower or 'module' in query_lower:
                menu_boost_terms.extend(['OPTIONAL MODULES', 'ADDON', 'INSURANCE',
                                        'DEALER TRACKING'])
            
            # Add menu-specific boost terms to query
            if menu_boost_terms:
                enhanced_query = f"{query} {' '.join(menu_boost_terms)}"
                logger.info(f"🎯 Enhanced menu query with {len(menu_boost_terms)} boost terms")
                return enhanced_query
        
        # Detect if query is asking about user actions/modifications
        action_keywords = [
            'modify', 'modifi', 'change', 'update', 'edit', 
            'user can', 'user options', 'options', 'choices',
            'what fields', 'which fields', 'enter', 'input'
        ]
        is_action_query = any(keyword in query_lower for keyword in action_keywords)
        
        if not is_action_query:
            return query
        
        # For code chunks and paragraphs, boost constraint/logic terms
        # Add business logic keywords to the query to find constraints first
        if index_name in ['new_code_chunks', 'new_cobol_paragraphs', 'code-chunks']:
            # Extract program name from original query if present
            program_name = None
            import re
            prog_match = re.search(r'\b([A-Z][A-Z0-9]{3,})\b', query)
            if prog_match:
                program_name = prog_match.group(1)
            
            # Build enhanced query with business logic keywords
            logic_keywords = [
                'MAX', 'VALUE', 'PIC 99',
                'ELEMENT SPECIFICATIONS',
                'ENTER NEW', 'ENTRY-MODULE', 'ENTER-ELE',
                'SPEC-TABLE', 'OCCURS', 'CHFG',
                'IF ELE', 'field number'
            ]
            
            # Combine: original query + program name + logic keywords
            if program_name:
                enhanced_query = f"{query} {program_name} {' '.join(logic_keywords)}"
            else:
                enhanced_query = f"{query} {' '.join(logic_keywords)}"
            
            return enhanced_query
        
        return query
    
    def _search_index(self, index_name: str, query: str, query_vector: List[float], top: int, 
                     is_copybook_usage_query: bool = False) -> List[Dict]:
        """Search a single index using hybrid search.
        
        Args:
            index_name: Name of the Azure Search index
            query: Search query text
            query_vector: Embedding vector for semantic search
            top: Maximum number of results to return
            is_copybook_usage_query: Whether this is a copybook usage query (needs special handling)
        """
        # Use the same API version as other scripts in this workspace
        url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version={self.config.search_api_version}"
        
        # DEBUGGING: Log search request details
        logger.info(f"🔍 SEARCH REQUEST for index: {index_name}")
        logger.info(f"   Query: {query}")
        logger.info(f"   Top: {top}")
        logger.info(f"   URL: {url}")
        vector_dim = len(query_vector) if query_vector is not None else 'None'
        logger.info(f"   Vector dimensions: {vector_dim}")
        
        # 🔹 COPYBOOK USAGE DETECTION: Force keyword search for copybook queries
        # Extract copybook name (e.g., "LPBW1IN" from "LPBW1IN.CPY")
        import re
        copybook_match = re.search(r'\b([A-Z0-9]+)\.CPY\b', query.upper())
        copybook_basename = copybook_match.group(1) if copybook_match else None
        
        if is_copybook_usage_query and index_name == 'new_cobol_copybook_usage' and copybook_basename:
            logger.info(f"📄 Copybook query detected – switching to keyword mode, top=1000, copybook={copybook_basename}")
            # Build search for full copybook name with .CPY extension
            # The index contains full paths like "LIBLP/LPBW1IN.CPY", so search for the filename with extension
            copybook_search_term = f"{copybook_basename}.CPY"
            body = {
                "search": copybook_search_term,  # Search for "LPBW1IN.CPY"
                "top": 1000,  # Get ALL matches
                "select": "*"
            }
            # NO vectorQueries - pure keyword search for deterministic results
            
            try:
                response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                response.raise_for_status()
                data = response.json()
                results = data.get('value', [])
                logger.info(f"📊 Copybook usage raw results: {len(results)} documents")
                return results
            except Exception as e:
                logger.error(f"Error searching {index_name}: {e}")
                return []
        
        # 🔹 BOOST RECALL for copybook_usage index in general (5x more results)
        if index_name == 'new_cobol_copybook_usage':
            top = min(top * 5, 1000)  # 5x boost, cap at 1000
            logger.info(f"   ⚡ Boosting copybook_usage recall: top={top}")
        
        # For "list all" queries, use wildcard search to bypass vector k limits
        query_lower = query.lower()
        # Use very specific phrases for comprehensive listing of TOP-LEVEL entities only
        # Don't trigger for "all fields in X" or "all methods in Y" - those need semantic search
        is_list_all = (
            ('list all' in query_lower and ('program' in query_lower or 'copybook' in query_lower or 'file' in query_lower)) or
            ('show all' in query_lower and ('program' in query_lower or 'copybook' in query_lower or 'file' in query_lower)) or
            ('every program' in query_lower) or
            ('every copybook' in query_lower) or
            ('all programs' in query_lower and 'in' not in query_lower) or  # Avoid "all programs in X"
            ('all copybooks' in query_lower and 'in' not in query_lower) or
            ('all files' in query_lower and 'in' not in query_lower)
        )
        
        # Enhance query for business logic if applicable (unless it's a list-all query)
        if is_list_all:
            enhanced_query = "*"  # Wildcard to get all documents
        else:
            enhanced_query = self._enhance_query_for_business_logic(query, index_name)
        
        # Determine vector field based on index
        vector_field = self._get_vector_field(index_name)
        
        # Build search body - only include vectorQueries if we have a vector field
        body = {
            "search": enhanced_query,  # Use enhanced query with boosting (or * for list-all)
            "top": top,
            "select": "*"
        }
        
        # Special handling for 'calls' index - add filter if program name detected
        program_name = self._extract_program_name(query)
        if index_name == 'new_cobol_calls' and program_name:
            # Validate it's a real program ID before applying filter
            if self._is_program_id(program_name):
                # Add filter to find calls from this program
                body["filter"] = f"caller_program eq '{program_name}'"
                # Increase top to get more results when filtering
                body["top"] = min(top * 2, 1000)
                logger.info(f"   ✅ Applying deterministic filter for calls from: {program_name}")
            else:
                logger.info(f"   ⚙️ Skipping deterministic filter for non-program name: {program_name}")
                program_name = None  # Clear to use semantic search
        
        # Special handling for 'copybook_usage' index - add filter if program name detected
        # Only apply filter if the detected name is a valid program identifier
        if index_name == 'new_cobol_copybook_usage' and program_name:
            # Verify it's a valid program ID (not a copybook or screen)
            if self._is_program_id(program_name):
                # Add filter to find copybooks used by this program
                body["filter"] = f"program_id eq '{program_name}'"
                # Increase top significantly to get all copybooks (programs can use 100+ copybooks)
                body["top"] = min(500, 1000)  # Most programs won't use more than 500 copybooks
                # For copybook usage, we want ALL matches, not semantic similarity
                # So skip vector search and use keyword search instead
                body["search"] = f"*"  # Match all documents (filter does the selection)
                logger.info(f"   ✅ Applying deterministic filter for copybooks used by: {program_name}")
            else:
                # It's a copybook name or invalid - don't filter, use full semantic search
                logger.info(f"   ⚙️ Skipping deterministic filter for non-program name: {program_name}")
                program_name = None  # Clear to skip vector skip logic below
        
        # Special handling for 'code' indexes - add filter if program name detected
        # This ensures we get ALL code chunks from the specific program, then do semantic search within them
        if index_name in ['code', 'code_new', 'new_cobol_code'] and program_name:
            # Validate it's a real program ID before applying filter
            if self._is_program_id(program_name):
                # Add filter to limit to this program's code
                body["filter"] = f"program_id eq '{program_name}'"
                # Increase top to get more code chunks from this program
                body["top"] = min(top * 5, 1000)  # Get more chunks when filtering to specific program
                logger.info(f"   ✅ Applying deterministic filter for code from: {program_name}")
            else:
                logger.info(f"   ⚙️ Skipping deterministic filter for non-program name: {program_name}")
                program_name = None  # Clear to use semantic search
        
        # Only add vector search if field exists AND it's not a list-all query
        # AND not a filtered copybook_usage query (those use keyword search with filter)
        # (vector search has k limit of 100-1000 depending on tier)
        skip_vector_for_copybook_usage = (index_name == 'new_cobol_copybook_usage' and program_name)
        if vector_field and not is_list_all and not skip_vector_for_copybook_usage:
            body["vectorQueries"] = [
                {
                    "vector": query_vector,
                    "k": top,
                    "fields": vector_field,
                    "kind": "vector"
                }
            ]
        
        try:
            # DEBUGGING: Log the full search body
            logger.info(f"   Search body: {json.dumps(body, indent=2)}")
            
            response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
            response.raise_for_status()
            
            results = response.json().get('value', [])
            
            # DEBUGGING: Log response details
            logger.info(f"   Results count: {len(results)}")
            
            # DEBUG: For screen_nodes, log all fields in first result
            if index_name == 'new_cobol_screen_nodes' and results:
                logger.info(f"   SCREEN_NODES first result fields: {list(results[0].keys())}")
                if 'enhanced_summary' in results[0]:
                    logger.info(f"   enhanced_summary found, first 150 chars: {results[0]['enhanced_summary'][:150]}")
                else:
                    logger.info(f"   enhanced_summary NOT IN RESULT!")
            
            # Log top 5 results with key fields
            logger.info(f"   Top 5 results for {index_name}:")
            for i, result in enumerate(results[:5]):
                logger.info(f"     {i+1}. ID: {result.get('id', result.get('screen_id', result.get('chunk_id', 'Unknown')))}")
                logger.info(f"        Score: {result.get('@search.score', 'None')}")
                
                # Check for summary_text specifically
                summary = result.get('summary_text', '')
                if summary:
                    logger.info(f"        Summary: {summary[:100]}...")
                else:
                    logger.info(f"        Summary: MISSING or EMPTY!")
                
                # Check other possible text fields
                text_fields = ['text', 'content', 'body', 'raw_span_text']
                for field in text_fields:
                    if field in result and result[field]:
                        logger.info(f"        {field}: {str(result[field])[:50]}...")
                        break
                logger.info("")
            
            return results
        
        except Exception as e:
            logger.warning(f"Warning: Search failed for index {index_name}: {e}")
            return []
    
    def _get_vector_field(self, index_name: str) -> str:
        """Get the vector field name for an index.
        
        These field names match the actual schema in Azure Search.
        Verified against live indexes on 2025-10-16.
        Expanded to include all 18 indexes.
        """
        vector_fields = {
            # Code chunks
            'code-chunks': 'text_vector',
            'new_code_chunks': 'text_vector',
            
            # Core metadata
            'new-cobol-files': 'contentVector',  # Has 2 vector fields, using primary
            'new_cobol_program_meta': 'summary_vector',
            'new_cobol_paragraphs': 'para_vector',
            'new_cobol_data_items': 'vector',
            'new_cobol_copybook_meta': 'summary_vector',
            
            # Relationships & usage
            'new_cobol_calls': 'snippet_vector',
            'new_cobol_variable_usage': 'usage_summary_vector',
            'new_cobol_copybook_usage': 'context_vector',
            'new_cobol_symbol_refs': 'excerpt_vector',
            'new_cobol_name_aliases': 'alias_vector',
            
            # Flow & navigation
            'new_cobol_program_flows': 'flow_vector',
            'new_cobol_flow_edges_v2': 'edge_vector',
            'new_cobol_ui_paths': 'path_vector',
            'new_cobol_menu_trees': 'content_vector',
            'new_cobol_screens': 'content_vector',       # NEW: Unified screens with AI analysis (1536-dim)
            'new_cobol_screen_nodes': 'summary_vector',  # Legacy screen nodes (1536-dim)
            
            # Dependencies
            'new_cobol_program_deps': 'dependency_blob_vector',
            'enhanced_cobol_dependencies': ''  # No vector field yet
        }
        return vector_fields.get(index_name, '')
    
    def _rank_results(self, results: List[Dict], max_results: int, question_type: str = None) -> List[Dict]:
        """Rank and deduplicate results by relevance score.
        
        Args:
            results: List of search results
            max_results: Maximum results to return
            question_type: Question type for special scoring adjustments
        """
        # Remove duplicates based on document ID
        seen = set()
        unique_results = []
        
        # Track copybook_usage stats for diagnostics
        copybook_usage_raw_count = sum(1 for r in results if r.get('_index_type') == 'copybook_usage')
        
        for result in results:
            # 🔹 SPECIAL DEDUPLICATION for copybook_usage: Keep only ONE entry per program
            # (not per line - we want unique programs, not unique references)
            if result.get('_index_type') == 'copybook_usage':
                program_id = result.get('program_id', '')
                copybook_name = result.get('copybook_name', '')
                # Key by program + copybook to get unique program list
                key = f"copybook_usage:{program_id}:{copybook_name}"
            else:
                # Standard deduplication for other indexes
                # Create unique key (index + document ID)
                # >>> FIX: robust paragraph doc_id (para_id is primary for paragraphs)
                doc_id = (
                    result.get('chunk_id') or 
                    result.get('para_id') or          # primary for paragraphs
                    result.get('paragraph_id') or     # legacy/alt
                    result.get('program_id') or 
                    result.get('call_id') or 
                    result.get('call_hash') or
                    result.get('edge_id') or
                    result.get('path_id') or
                    result.get('tree_id') or
                    result.get('screen_id') or
                    result.get('usage_id') or
                    result.get('symbol_id')
                )
                
                # >>> FIX: fallback for paragraph docs if still missing
                if not doc_id and result.get('_index_type') == 'paragraphs':
                    doc_id = f"{result.get('program_id','')}|{result.get('paragraph_name','')}|{result.get('line_start','?')}"
                
                if not doc_id:
                    # If still no ID, use a hash of key fields
                    id_parts = [
                        str(result.get('caller_program', '')),
                        str(result.get('callee_program', '')),
                        str(result.get('line', '')),
                        str(result.get('file_path', ''))
                    ]
                    doc_id = ':'.join(id_parts) if any(id_parts) else None
                
                if not doc_id:
                    continue
                
                key = f"{result['_index_type']}:{doc_id}"
            
            if key not in seen:
                seen.add(key)
                unique_results.append(result)
        
        # 🔹 DIAGNOSTIC LOGGING for copybook_usage
        copybook_usage_unique_count = sum(1 for r in unique_results if r.get('_index_type') == 'copybook_usage')
        if copybook_usage_raw_count > 0:
            logger.info(f"📊 Copybook_usage raw={copybook_usage_raw_count} unique={copybook_usage_unique_count}")
        
        # >>> FIX: paragraph precision nudge for implementation queries
        if question_type == 'implementation':
            for result in unique_results:
                if result.get('_index_type') == 'paragraphs':
                    txt = (result.get('text') or result.get('source_excerpt') or '').upper()
                    # Boost paragraphs with calculation keywords
                    if any(tok in txt for tok in [' COMPUTE ', ' LATE-CHARGE', ' SERVICE-CHG', ' SCRATE ', ' FORMULA']):
                        current_score = result.get('@search.score', 0)
                        result['@search.score'] = current_score * 1.08
        
        # >>> TIER A FIX 5: Generic structure-based nudge for multi-segment labels (pattern-free)
        for result in unique_results:
            if result.get('_index_type') == 'paragraphs':
                pn = result.get('paragraph_name') or ''
                # Generic boost for labels that look like structured IDs (≥2 hyphens)
                if pn.count('-') >= 2:
                    current_score = result.get('@search.score', 0)
                    result['@search.score'] = current_score * 1.06
        
        # Sort by search score (higher is better)
        sorted_results = sorted(
            unique_results,
            key=lambda x: x.get('@search.score', 0),
            reverse=True
        )
        
        # Log distribution of top results by index type
        if sorted_results:
            top_20 = sorted_results[:20]
            index_counts = {}
            for r in top_20:
                idx_type = r.get('_index_type', 'unknown')
                index_counts[idx_type] = index_counts.get(idx_type, 0) + 1
            logger.info(f"📊 Top 20 results distribution: {index_counts}")
        
        # Normalize content field for all results before returning
        # Different indexes use different field names (summary_text, text, content, etc.)
        for result in sorted_results[:max_results]:
            if 'content' not in result or not result['content']:
                # Add content field based on index type
                index_type = result.get('_index_type', '')
                
                if index_type in ('screen_nodes', 'screens'):
                    # Screen nodes use summary_text as primary content
                    result['content'] = result.get('summary_text') or result.get('enhanced_summary') or result.get('text') or ''
                elif index_type == 'code':
                    # Code chunks use 'text' field
                    result['content'] = result.get('text') or result.get('code') or ''
                elif index_type in ('programs', 'program_meta'):
                    # Program metadata uses summary fields
                    result['content'] = result.get('summary_text') or result.get('summary') or result.get('description') or ''
                else:
                    # Generic fallback for other indexes
                    result['content'] = (result.get('text') or result.get('summary_text') or 
                                       result.get('summary') or result.get('description') or 
                                       result.get('content') or '')
        
        # >>> FIX: observability - log paragraphs kept
        kept_para = sum(1 for r in sorted_results[:max_results] if r.get('_index_type') == 'paragraphs')
        if kept_para > 0:
            logger.info(f"📊 [rank] paragraphs kept={kept_para} of {max_results} total results")
        
        return sorted_results[:max_results]
    
    def _handle_transaction_copybooks(self, query: str, max_results: int) -> List[Dict[str, Any]]:
        """Deterministic handler for transaction-specific copybook questions.
        
        Pattern: "copybooks used in PROGRAM when handling TRANSACTION"
        Strategy: Get all program code, find transaction entry points, return nearby copybooks deterministically
        """
        import re
        
        # Extract program name and transaction code
        program_name = self._extract_program_name(query)
        
        # Extract transaction code (2-letter codes like RP, CP, EP)
        tx_match = re.search(r'\b([A-Z]{2})\b.*(?:transaction|processing|handling)', query, re.IGNORECASE)
        if not tx_match:
            # Try alternate pattern
            tx_match = re.search(r'(?:handling|processing|for)\s+([A-Z]{2})\b', query, re.IGNORECASE)
        
        transaction_code = tx_match.group(1).upper() if tx_match else None
        
        logger.info(f"🎯 Extracted: program={program_name}, transaction={transaction_code}")
        
        if not program_name:
            logger.warning("No program name detected, falling back to semantic search")
            return []
        
        # Validate it's a real program, not a copybook
        if not self._is_program_id(program_name):
            logger.warning(f"⚙️ Detected non-program identifier '{program_name}', falling back to semantic search")
            return []
        
        # Strategy: Get ALL code chunks for this program, find transaction entry points,
        # then get copybook_usage records from those specific line ranges
        
        # Step 1: Get all code chunks for the program (sorted by line number)
        code_index = self.config.get_index_name('code_new')
        code_url = f"{self.config.search_endpoint}/indexes/{code_index}/docs/search?api-version={self.config.search_api_version}"
        
        # Note: new_code_chunks uses 'name' field for program filename (e.g., 'LONPF2.CBL')
        # Construct the expected filename
        program_filename = f"{program_name}.CBL"
        
        code_body = {
            "search": "*",
            "filter": f"name eq '{program_filename}'",
            "top": 1000,  # Get all chunks
            "orderby": "start_line asc"
        }
        
        logger.info(f"   ✅ Applying deterministic filter for transaction copybooks in: {program_name}")
        
        try:
            code_response = requests.post(code_url, headers=self.search_headers, json=code_body, timeout=30)
            code_response.raise_for_status()
            code_chunks = code_response.json().get('value', [])
            logger.info(f"📄 Retrieved {len(code_chunks)} code chunks for {program_name}")
        except Exception as e:
            logger.error(f"Failed to get code chunks: {e}")
            return []
        
        # Step 2: Find chunks containing transaction entry points
        tx_entry_chunks = []
        tx_pattern = re.compile(rf'IF\s+.*TRCD.*=.*["\'{transaction_code}["\']', re.IGNORECASE) if transaction_code else None
        
        for chunk in code_chunks:
            content = chunk.get('text', chunk.get('content', ''))  # Try 'text' first, fallback to 'content'
            if tx_pattern and tx_pattern.search(content):
                tx_entry_chunks.append(chunk)
                logger.info(f"✅ Found TX entry in chunk at lines {chunk.get('start_line')}-{chunk.get('end_line')}")
        
        if not tx_entry_chunks:
            logger.warning(f"No {transaction_code} entry points found in code chunks")
            
            # If no code chunks exist at all, we can't do deterministic retrieval
            if not code_chunks:
                logger.error(f"No code chunks found for {program_name} - cannot do deterministic retrieval")
                logger.info(f"Falling back to semantic search for better filtering")
                return []  # Return empty to trigger semantic search fallback
            
            # If code exists but no transaction pattern matched, return all copybooks as last resort
            logger.warning(f"Code exists but no {transaction_code} pattern matched - returning all copybooks")
            return self._get_all_program_copybooks(program_name, max_results)
        
        # Step 3: Get copybooks used in a window around transaction entry points
        # Use ±100 lines as context window
        tx_line_ranges = []
        for chunk in tx_entry_chunks:
            start = max(1, chunk.get('start_line', 0) - 100)
            end = chunk.get('end_line', 0) + 100
            tx_line_ranges.append((start, end))
        
        # Step 4: Get copybook_usage records in those line ranges
        copybook_index = self.config.get_index_name('copybook_usage')
        copybook_url = f"{self.config.search_endpoint}/indexes/{copybook_index}/docs/search?api-version={self.config.search_api_version}"
        
        # Build filter for line ranges
        range_filters = [f"(line_number ge {start} and line_number le {end})" for start, end in tx_line_ranges]
        filter_str = f"program_id eq '{program_name}' and ({' or '.join(range_filters)})"
        
        copybook_body = {
            "search": "*",
            "filter": filter_str,
            "select": "*",
            "top": 500
        }
        
        try:
            copybook_response = requests.post(copybook_url, headers=self.search_headers, json=copybook_body, timeout=30)
            copybook_response.raise_for_status()
            copybook_docs = copybook_response.json().get('value', [])
            
            # Check if we got any results
            if not copybook_docs:
                logger.warning(f"⚠️ No copybooks found in ±100 line windows around {transaction_code} entry points")
                logger.info(f"💡 Using HYBRID approach: Semantic search on {transaction_code} code sections")
                
                # Build a combined text from the RP transaction code chunks
                tx_code_text = "\n\n".join([
                    chunk.get('text', chunk.get('content', ''))
                    for chunk in tx_entry_chunks
                ])
                
                # Do semantic search on copybook_meta using this code
                return self._semantic_search_copybooks_for_code(
                    tx_code_text,
                    program_name,
                    transaction_code,
                    max_results
                )
            
            # Dedupe by copybook name
            seen_copybooks = set()
            unique_copybooks = []
            for doc in copybook_docs:
                cb_name = doc.get('copybook_name', '')
                if cb_name and cb_name not in seen_copybooks:
                    seen_copybooks.add(cb_name)
                    doc['_index_type'] = 'copybook_usage'
                    doc['@search.score'] = 1.0  # Fixed score (deterministic)
                    unique_copybooks.append(doc)
            
            logger.info(f"🎯 DETERMINISTIC RESULT: {len(unique_copybooks)} unique copybooks in {transaction_code} logic")
            return unique_copybooks[:max_results]
            
        except Exception as e:
            logger.error(f"Failed to get copybook usage: {e}")
            return []
    
    def _semantic_search_copybooks_for_code(
        self,
        code_text: str,
        program_name: str,
        transaction_code: str,
        max_results: int
    ) -> List[Dict[str, Any]]:
        """
        Semantic search to find relevant copybooks based on transaction code content.
        This is used when deterministic line-range lookup fails (COBOL copybooks are
        in DATA DIVISION but transaction logic is in PROCEDURE DIVISION).
        """
        logger.info(f"🔍 Semantic copybook search for {program_name} {transaction_code} code")
        
        # Generate embedding for the transaction code (use large model - copybook_meta is 3072-dim)
        try:
            embedding = self._generate_embedding(code_text[:8000], use_small_model=False)  # Limit to 8K chars
        except Exception as e:
            logger.error(f"Failed to generate embedding: {e}")
            return []
        
        # Search copybook_meta with vector similarity
        copybook_meta_index = self.config.get_index_name('copybook_meta')
        url = f"{self.config.search_endpoint}/indexes/{copybook_meta_index}/docs/search?api-version={self.config.search_api_version}"
        
        # >>> FIX: Standardize to vectorQueries format (not legacy single vector)
        body = {
            "search": None,
            "vectorQueries": [{
                "kind": "vector",
                "vector": embedding,
                "fields": "summary_vector",  # >>> FIX: Match config mapping (was description_vector)
                "k": max_results * 2  # Get more candidates
            }],
            # Note: No program_id filter - copybook_meta is global, not program-specific
            # Copybooks are shared across programs, rely on semantic similarity
            "top": max_results * 2
        }
        
        try:
            response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
            response.raise_for_status()
            results = response.json().get('value', [])
            
            # Add metadata
            for doc in results:
                doc['_index_type'] = 'copybook_meta'
                doc['_search_method'] = 'semantic_code_based'
            
            logger.info(f"🎯 SEMANTIC RESULT: {len(results)} relevant copybooks for {transaction_code} code")
            return results[:max_results]
            
        except Exception as e:
            logger.error(f"Semantic copybook search failed: {e}")
            return []
    
    
    def _extract_and_fetch_entities(self, initial_results: List[Dict[str, Any]], query: str) -> List[Dict[str, Any]]:
        """Extract copybook/program names from initial results and fetch additional chunks.
        
        For implementation questions, the initial search might mention key copybooks
        (e.g., LPLCHG) but not retrieve all their chunks. This extracts those names
        and does targeted follow-up searches.
        
        Args:
            initial_results: Chunks from initial hybrid search
            query: Original user query (for context)
            
        Returns:
            Additional chunks found via entity extraction
        """
        import re
        from collections import Counter
        
        # Extract all COBOL identifiers (uppercase names with hyphens, 4+ chars)
        identifier_pattern = re.compile(r'\b[A-Z][A-Z0-9-]{3,}\b')
        
        all_identifiers = []
        for doc in initial_results[:50]:  # Only scan top 50 to avoid noise
            content = doc.get('text', doc.get('content', doc.get('description', '')))
            # Look for common patterns: "COPYBOOK: XXX", "COPY XXX", standalone caps names
            identifiers = identifier_pattern.findall(content)
            all_identifiers.extend(identifiers)
        
        # Count frequency
        entity_counts = Counter(all_identifiers)
        
        # Filter to likely copybook/program names (common patterns)
        copybook_entities = []
        for entity, count in entity_counts.most_common(20):
            # Skip common COBOL keywords/noise
            skip_patterns = [
                'WORKING-STORAGE', 'PROCEDURE', 'DIVISION', 'SECTION',
                'DISPLAY', 'ACCEPT', 'COMPUTE', 'PERFORM', 'MOVE',
                'VALUE', 'PICTURE', 'REDEFINES', 'OCCURS', 'INDEXED'
            ]
            if any(skip in entity for skip in skip_patterns):
                continue
            
            # Keep if it looks like a copybook (ends with common suffixes or appears frequently)
            if count >= 2 or any(entity.endswith(suffix) for suffix in ['CPY', 'CHG', 'CAS', 'CAP', 'SP', 'PF', 'MENU']):
                copybook_entities.append(entity)
        
        logger.info(f"🔍 Extracted {len(copybook_entities)} candidate entities: {copybook_entities[:10]}")
        
        if not copybook_entities:
            return []
        
        # Fetch additional chunks for top 5 entities
        additional_chunks = []
        code_index = self.config.get_index_name('code')
        url = f"{self.config.search_endpoint}/indexes/{code_index}/docs/search?api-version={self.config.search_api_version}"
        
        for entity in copybook_entities[:5]:
            # Simple keyword search for this entity
            body = {
                "search": entity,
                "searchMode": "all",
                "queryType": "simple",
                "select": "*",
                "top": 5
            }
            
            try:
                response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                response.raise_for_status()
                results = response.json().get('value', [])
                
                for doc in results:
                    doc['_index_type'] = 'code'
                    doc['_search_method'] = 'entity_extraction'
                    doc['_extracted_entity'] = entity
                
                additional_chunks.extend(results)
                logger.info(f"   ✅ {entity}: found {len(results)} chunks")
                
            except Exception as e:
                logger.warning(f"   ⚠️ {entity}: search failed - {e}")
        
        # Deduplicate against initial results
        initial_ids = {doc.get('chunk_id', doc.get('id', '')) for doc in initial_results}
        unique_additional = [
            doc for doc in additional_chunks
            if doc.get('chunk_id', doc.get('id', '')) not in initial_ids
        ]
        
        logger.info(f"✅ Entity extraction: {len(unique_additional)} unique new chunks (from {len(additional_chunks)} total)")
        return unique_additional
    
    def _extract_copy_statements(self, chunks: List[Dict[str, Any]]) -> List[str]:
        """Extract copybook names from COPY statements in chunks.
        
        Parses COBOL COPY statements like:
        - COPY LPLCHG
        - COPY LPLCHG.
        - COPY 'LPLCHG'
        
        Args:
            chunks: List of code chunks
            
        Returns:
            List of unique copybook names
        """
        import re
        from collections import Counter
        
        # Pattern matches: COPY copybook-name (with optional quotes/period)
        copy_pattern = re.compile(r'\bCOPY\s+["\']?([A-Z][A-Z0-9-]+)["\']?\.?', re.IGNORECASE)
        
        all_copybooks = []
        for chunk in chunks:
            content = chunk.get('text', chunk.get('content', ''))
            matches = copy_pattern.findall(content)
            all_copybooks.extend(matches)
        
        # Get frequency and return top copybooks
        copybook_counts = Counter(all_copybooks)
        
        # Filter out common false positives
        skip_patterns = ['REPLACING', 'SUPPRESS', 'EJECT']
        copybooks = [
            cb for cb, count in copybook_counts.most_common(10)
            if cb not in skip_patterns
        ]
        
        return copybooks
    
    def _extract_paragraph_references(self, chunks: List[Dict[str, Any]]) -> List[str]:
        """Extract paragraph references from PERFORM and GO TO statements.
        
        Parses COBOL control flow statements like:
        - PERFORM CTY-SCAROLINA-18-42
        - GO TO CTY-SCAROLINA-18-42
        - PERFORM THRU CTY-SCAROLINA-18-42-EXIT
        
        Args:
            chunks: List of code chunks
            
        Returns:
            List of unique paragraph names
        """
        import re
        from collections import Counter
        
        # Pattern matches: PERFORM/GO TO paragraph-name
        perform_pattern = re.compile(r'\b(?:PERFORM|GO\s+TO)\s+([A-Z][A-Z0-9-]+)', re.IGNORECASE)
        
        all_paragraphs = []
        for chunk in chunks:
            content = chunk.get('text', chunk.get('content', chunk.get('source_excerpt', '')))
            matches = perform_pattern.findall(content)
            all_paragraphs.extend(matches)
        
        # Get frequency and return top paragraphs
        paragraph_counts = Counter(all_paragraphs)
        
        # Filter out common keywords that aren't paragraphs
        skip_patterns = ['VARYING', 'UNTIL', 'TIMES', 'THRU', 'THROUGH']
        paragraphs = [
            para for para, count in paragraph_counts.most_common(15)
            if para not in skip_patterns and len(para) > 3
        ]
        
        return paragraphs
    
    def _extract_query_terms_for_paragraphs(self, query: str) -> List[str]:
        """>>> DEPRECATED: Unused (superseded by _dynamic_paragraph_expansions with dual-probe corpus mining).
        
        Extract terms from query that might be paragraph/section names.
        
        For state-specific queries like "South Carolina late fee", generates
        search terms like "SCAROLINA", "CTY-SCAROLINA", etc.
        
        This hardcoded approach has been replaced by corpus-driven token mining
        in Round 5 multi-hop search, which discovers relevant terms generically
        without state-specific mappings.
        
        Args:
            query: Original user question
            
        Returns:
            List of search terms for paragraph lookup
        """
        terms = []
        query_lower = query.lower()
        
        # State mappings
        state_mappings = {
            'south carolina': ['SCAROLINA', 'CTY-SCAROLINA', 'CSA-SCAROLINA', 'SC-'],
            'north carolina': ['NCAROLINA', 'CTY-NCAROLINA', 'NC-'],
            'georgia': ['GEORGIA', 'CTY-GEORGIA', 'GA-'],
            'florida': ['FLORIDA', 'CTY-FLORIDA', 'FL-'],
            'alabama': ['ALABAMA', 'CTY-ALABAMA', 'AL-'],
            'tennessee': ['TENNESSEE', 'CTY-TENNESSEE', 'TN-']
        }
        
        # Check for state mentions
        for state, patterns in state_mappings.items():
            if state in query_lower:
                terms.extend(patterns)
                logger.info(f"   🗺️  Detected state: {state} → {patterns}")
        
        # Check for formula/fee type keywords
        if 'late fee' in query_lower or 'late charge' in query_lower:
            terms.extend(['LATE-CHARGE', 'LATE-FEE', 'LC-', 'LCHG'])
        
        if 'service charge' in query_lower:
            terms.extend(['SERVICE-CHARGE', 'SERV-CHG', 'SC-'])
        
        # Extract formula numbers if mentioned
        import re
        formula_nums = re.findall(r'formula\s+(\d+)', query_lower)
        for num in formula_nums:
            terms.append(f'FORMULA-{num}')
            terms.append(f'FRMLA-{num}')
        
        return list(set(terms))  # Remove duplicates
    
    def _extract_cobol_variables(self, chunks: List[Dict[str, Any]]) -> List[str]:
        """Extract COBOL variable names from chunks.
        
        Looks for uppercase identifiers with hyphens (standard COBOL naming).
        Filters out keywords and common noise.
        
        Args:
            chunks: List of code chunks
            
        Returns:
            List of unique variable names
        """
        import re
        from collections import Counter
        
        # Pattern for COBOL identifiers: starts with letter, contains letters/numbers/hyphens
        var_pattern = re.compile(r'\b([A-Z][A-Z0-9-]{2,})\b')
        
        # COBOL keywords to skip
        keywords = {
            'WORKING-STORAGE', 'PROCEDURE', 'DIVISION', 'SECTION', 'IDENTIFICATION',
            'ENVIRONMENT', 'DATA', 'FILE-CONTROL', 'SELECT', 'ASSIGN',
            'DISPLAY', 'ACCEPT', 'COMPUTE', 'PERFORM', 'MOVE', 'ADD', 'SUBTRACT',
            'MULTIPLY', 'DIVIDE', 'IF', 'ELSE', 'END-IF', 'EVALUATE', 'WHEN',
            'PICTURE', 'VALUE', 'REDEFINES', 'OCCURS', 'INDEXED', 'DEPENDING',
            'COPY', 'REPLACING', 'CALL', 'USING', 'RETURNING', 'EXIT', 'STOP',
            'OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE', 'DELETE', 'START',
            'AND', 'OR', 'NOT', 'THE', 'FOR', 'FROM', 'INTO', 'BY', 'TO', 'OF',
            'ON', 'AT', 'IN', 'WITH', 'THEN', 'ELSE-IF', 'IS', 'ARE', 'WAS', 'WERE'
        }
        
        all_vars = []
        for chunk in chunks[:30]:  # Sample first 30 chunks
            content = chunk.get('text', chunk.get('content', ''))
            matches = var_pattern.findall(content)
            all_vars.extend(matches)
        
        # Count frequency
        var_counts = Counter(all_vars)
        
        # Filter keywords and return top variables
        variables = [
            var for var, count in var_counts.most_common(50)
            if var not in keywords and count >= 2
        ]
        
        return variables[:15]  # Top 15 variables
    
    def _multi_hop_implementation_search(
        self, 
        initial_results: List[Dict[str, Any]], 
        query: str
    ) -> List[Dict[str, Any]]:
        """Perform multi-hop search for implementation questions.
        
        Follows relationships in the codebase:
        1. Extract COPY statements → search copybook_usage
        2. Extract variables → search data_items (definitions)
        3. Extract variables → search variables (usage patterns)
        
        Args:
            initial_results: Chunks from initial search
            query: Original query (for context)
            
        Returns:
            Additional chunks found via multi-hop exploration
        """
        additional_chunks = []
        
        logger.info("🔗 MULTI-HOP: Starting relationship traversal")
        
        # Round 1: Follow COPY statements
        copybooks = self._extract_copy_statements(initial_results)
        logger.info(f"📚 ROUND 1: Found {len(copybooks)} copybooks: {copybooks[:5]}")
        
        if copybooks:
            copybook_usage_index = self.config.get_index_name('copybook_usage')
            url = f"{self.config.search_endpoint}/indexes/{copybook_usage_index}/docs/search?api-version={self.config.search_api_version}"
            
            for cb in copybooks[:5]:  # Top 5 copybooks
                body = {
                    "search": cb,
                    "searchMode": "all",
                    "queryType": "simple",
                    "select": "*",
                    "top": 10
                }
                
                try:
                    response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                    response.raise_for_status()
                    results = response.json().get('value', [])
                    
                    for doc in results:
                        doc['_index_type'] = 'copybook_usage'
                        doc['_search_method'] = 'multi_hop_copy'
                        doc['_extracted_copybook'] = cb
                    
                    additional_chunks.extend(results)
                    logger.info(f"   ✅ {cb}: {len(results)} usage records")
                    
                except Exception as e:
                    logger.warning(f"   ⚠️ {cb}: search failed - {e}")
        
        # Round 2: Find variable definitions
        variables = self._extract_cobol_variables(initial_results)
        logger.info(f"🔤 ROUND 2: Found {len(variables)} variables: {variables[:10]}")
        
        if variables:
            data_items_index = self.config.get_index_name('data_items')
            url = f"{self.config.search_endpoint}/indexes/{data_items_index}/docs/search?api-version={self.config.search_api_version}"
            
            for var in variables[:10]:  # Top 10 variables
                body = {
                    "search": var,
                    "searchMode": "all",
                    "queryType": "simple",
                    "select": "*",
                    "top": 5
                }
                
                try:
                    response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                    response.raise_for_status()
                    results = response.json().get('value', [])
                    
                    for doc in results:
                        doc['_index_type'] = 'data_items'
                        doc['_search_method'] = 'multi_hop_definition'
                        doc['_extracted_variable'] = var
                    
                    additional_chunks.extend(results)
                    if results:
                        logger.info(f"   ✅ {var}: {len(results)} definitions")
                    
                except Exception as e:
                    logger.warning(f"   ⚠️ {var}: definition search failed - {e}")
        
        # Round 3: Find variable usage patterns
        if variables:
            var_usage_index = self.config.get_index_name('variables')
            url = f"{self.config.search_endpoint}/indexes/{var_usage_index}/docs/search?api-version={self.config.search_api_version}"
            
            for var in variables[:5]:  # Top 5 variables for usage
                body = {
                    "search": var,
                    "searchMode": "all",
                    "queryType": "simple",
                    "select": "*",
                    "top": 5
                }
                
                try:
                    response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                    response.raise_for_status()
                    results = response.json().get('value', [])
                    
                    for doc in results:
                        doc['_index_type'] = 'variables'
                        doc['_search_method'] = 'multi_hop_usage'
                        doc['_extracted_variable'] = var
                    
                    additional_chunks.extend(results)
                    if results:
                        logger.info(f"   ✅ {var}: {len(results)} usage patterns")
                    
                except Exception as e:
                    logger.warning(f"   ⚠️ {var}: usage search failed - {e}")
        
        # Round 4: Find referenced paragraphs (e.g., PERFORM, GO TO)
        paragraphs = self._extract_paragraph_references(initial_results + additional_chunks)
        logger.info(f"📍 ROUND 4: Found {len(paragraphs)} paragraph references: {paragraphs[:10]}")
        
        if paragraphs:
            paragraphs_index = self.config.get_index_name('paragraphs')
            url = f"{self.config.search_endpoint}/indexes/{paragraphs_index}/docs/search?api-version={self.config.search_api_version}"
            
            for para in paragraphs[:10]:  # Top 10 paragraphs
                body = {
                    "search": para,
                    "searchMode": "all",
                    "queryType": "simple",
                    "select": "*",
                    "top": 3
                }
                
                try:
                    response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
                    response.raise_for_status()
                    results = response.json().get('value', [])
                    
                    for doc in results:
                        doc['_index_type'] = 'paragraphs'
                        doc['_search_method'] = 'multi_hop_paragraph'
                        doc['_extracted_paragraph'] = para
                    
                    additional_chunks.extend(results)
                    if results:
                        logger.info(f"   ✅ {para}: {len(results)} paragraph implementations")
                    
                except Exception as e:
                    logger.warning(f"   ⚠️ {para}: paragraph search failed - {e}")
        
        # >>> FIX: Round 5 - Enhanced paragraph search with dynamic expansions and weighted 3-leg RRF
        logger.info(f"🎯 ROUND 5: Enhanced paragraph search (implementation focus)")
        
        # Collect likely program_ids from earlier hops (for soft boosting, not filtering)
        likely_programs = {d.get('program_id') for d in (initial_results + additional_chunks) if d.get('program_id')}
        if likely_programs:
            logger.info(f"   🔍 Likely programs from earlier hops: {list(likely_programs)[:5]}")
        
        # Generate dynamic expansions by mining the corpus
        try:
            expansions, mined = self._dynamic_paragraph_expansions(query, initial_results)
            logger.info(f"   📈 Generated {len(expansions)} expansion terms from corpus")
            logger.info(f"   🔬 Mined tokens - hyphen: {mined.get('hyphen', [])[:8]}, caps: {mined.get('caps', [])[:5]}")
        except Exception as e:
            logger.warning(f"   ⚠️ Dynamic expansion failed: {e}, falling back to basic search")
            expansions, mined = [], {"hyphen": [], "caps": [], "nums": []}
        
        paragraphs_index = self.config.get_index_name('paragraphs')
        url = f"{self.config.search_endpoint}/indexes/{paragraphs_index}/docs/search?api-version={self.config.search_api_version}"
        
        # Build expanded lexical query
        expanded_lex = f'({query})'
        if expansions:
            expanded_lex += ' OR ' + ' OR '.join(expansions)
        
        # >>> TIER A FIX 1: Remove hard filter - will apply soft boost instead (fail-open design)
        # NO filter_clause applied to searches - retrieve broadly first, then boost
        
        # >>> TIER A FIX 4: Lexical search with paragraph_name field boost and increased recall
        # >>> TIER B: Added paragraph_name_terms for tokenized label matching
        # >>> TUNE: Increased lex top from 300 to 300 (already optimized)
        lex_results = []
        try:
            lex_body = {
                "search": expanded_lex,
                "queryType": "full",
                "searchMode": "any",
                "searchFields": "paragraph_name^6,paragraph_name_terms^5,text",  # >>> TUNE: Boosted terms field, added text
                "select": "*",
                "top": 300  # >>> TUNE: Per-leg recall target
            }
            
            lex_response = requests.post(url, headers=self.search_headers, json=lex_body, timeout=30)
            lex_response.raise_for_status()
            lex_results = lex_response.json().get('value', [])
            logger.info(f"   ✅ Lexical: {len(lex_results)} paragraphs (top 300 recall)")
        except Exception as e:
            logger.warning(f"   ⚠️ Lexical search failed: {e}")
        
        # >>> TIER A FIX 3: Vector search with expanded query and increased recall
        # >>> TUNE: Increased vec k from 150 to 150 (already optimized)
        vec_results = []
        try:
            expand_for_vec = query + " " + " ".join(mined.get("hyphen", [])[:8])
            qv = self._generate_embedding(expand_for_vec, use_small_model=False)
            
            if qv:
                vec_body = {
                    "search": None,
                    "vectorQueries": [{
                        "kind": "vector",
                        "vector": qv,
                        "k": 150,  # >>> TUNE: Per-leg vec recall target
                        "fields": "para_vector"
                    }],
                    "select": "*",
                    "top": 150  # >>> TUNE: Matches k value
                }
                
                vec_response = requests.post(url, headers=self.search_headers, json=vec_body, timeout=30)
                vec_response.raise_for_status()
                vec_results = vec_response.json().get('value', [])
                logger.info(f"   ✅ Vector: {len(vec_results)} paragraphs (top 150 recall)")
        except Exception as e:
            logger.warning(f"   ⚠️ Vector search failed: {e}")
        
        # >>> TIER A FIX 2: Add dedicated ID leg for hyphenated labels (generic, pattern-free)
        # >>> TUNE: Increased ID top from 120 to 120 (already optimized)
        id_results = []
        try:
            hyphen_tokens = mined.get("hyphen", [])[:8]
            if hyphen_tokens:
                # Build quoted ID query for exact label matching (generic for any hyphen-style labels)
                id_terms = [f'"{token}"' for token in hyphen_tokens]
                id_query = " OR ".join(id_terms)
                
                id_body = {
                    "search": id_query,
                    "queryType": "full",
                    "searchMode": "any",
                    "searchFields": "paragraph_name^6,paragraph_name_terms^5",  # Include terms field for state codes
                    "select": "*",
                    "top": 120  # >>> TUNE: Per-leg ID recall target
                }
                
                id_response = requests.post(url, headers=self.search_headers, json=id_body, timeout=30)
                id_response.raise_for_status()
                id_results = id_response.json().get('value', [])
                logger.info(f"   ✅ ID leg: {len(id_results)} paragraphs matched hyphenated labels")
        except Exception as e:
            logger.warning(f"   ⚠️ ID leg search failed: {e}")
        
        # >>> FIX: Add 4th policy/prose leg for implementation questions
        # >>> TUNE: Increased policy top from 100 to 150 for better recall
        # >>> FIX: policy leg — OR query for recall
        policy_results = []
        try:
            # Build generic policy query: policy tokens + numbers from query
            import re
            query_numbers = re.findall(r'\d+', query)  # Extract all numbers
            terms = []
            
            # Add policy tokens from config
            for token in self.config.policy_tokens[:10]:  # Top 10 policy terms
                terms.append(token)
            
            # Add numbers for specificity (e.g., "90", "15", "1996")
            for num in query_numbers[:5]:  # Top 5 numbers
                terms.append(num)
            
            if terms:
                policy_query = " OR ".join(terms)  # >>> FIX: OR for high recall (was AND-like)
                
                policy_body = {
                    "search": policy_query,
                    "queryType": "full",
                    "searchMode": "any",
                    "searchFields": "source_excerpt,text",  # >>> FIX: Schema-safe fields only
                    "select": "*",
                    "top": 150  # >>> TUNE: Per-leg policy recall target (was 100)
                }
                
                policy_response = requests.post(url, headers=self.search_headers, json=policy_body, timeout=30)
                policy_response.raise_for_status()
                candidates = policy_response.json().get('value', [])
                
                # >>> FIX: post-filter for precision (must have number + policy token)
                tokset = set(t.upper() for t in self.config.policy_tokens)
                def _has_policy_signals(d):
                    t = (d.get("source_excerpt") or d.get("text") or "").upper()
                    return any(k in t for k in tokset) and re.search(r"\d", t)
                
                policy_results = [d for d in candidates if _has_policy_signals(d)]
                logger.info(f"   ✅ Policy leg: {len(candidates)} candidates → {len(policy_results)} after post-filter (number+token)")
        except Exception as e:
            logger.warning(f"   ⚠️ Policy leg search failed: {e}")
        
        # >>> FIX: Fuse with 4-leg weighted RRF (lex:0.4, vec:0.25, id:0.20, policy:0.15)
        if lex_results or vec_results or id_results or policy_results:
            # >>> TUNE: Log per-leg counts before fusion
            logger.info(f"   📊 Per-leg counts: lex={len(lex_results)}, vec={len(vec_results)}, id={len(id_results)}, policy={len(policy_results)}")
            
            round5_fused = self.weighted_rrf(
                {"lex": lex_results, "vec": vec_results, "id": id_results, "policy": policy_results},
                weights={"lex": 0.4, "vec": 0.25, "id": 0.20, "policy": 0.15},
                k=self.config.rrf_k
            )
            
            # >>> TUNE: Apply fused cap and tail cutoff
            initial_fused_count = len(round5_fused)
            
            # Step 1: Cap at max_fused_docs
            if len(round5_fused) > self.config.max_fused_docs:
                round5_fused = round5_fused[:self.config.max_fused_docs]
                logger.info(f"   ✂️ Capped fused results: {initial_fused_count} → {len(round5_fused)} (max={self.config.max_fused_docs})")
            
            # Step 2: Apply tail cutoff (drop docs below tail_cutoff * top_score)
            if round5_fused:
                top_score = round5_fused[0].get('@search.score', 1.0)
                cutoff_threshold = top_score * self.config.tail_cutoff
                before_cutoff = len(round5_fused)
                round5_fused = [doc for doc in round5_fused if doc.get('@search.score', 0.0) >= cutoff_threshold]
                if len(round5_fused) < before_cutoff:
                    logger.info(f"   ✂️ Tail cutoff: {before_cutoff} → {len(round5_fused)} (threshold={cutoff_threshold:.4f}, top={top_score:.4f})")
            
            # >>> FIX: label alignment boost for ID-like labels that match mined hyphen tokens
            align_boost = 1.08
            hyphens = set((mined.get("hyphen") or []))  # mined from dual-probe
            align_count = 0
            for doc in round5_fused:
                pn = (doc.get("paragraph_name") or "").upper()
                if any(h in pn for h in hyphens):
                    doc["@search.score"] = doc.get("@search.score", 0.0) * align_boost
                    align_count += 1
            
            if align_count > 0:
                logger.info(f"   🎯 Label alignment boost: {align_count}/{len(round5_fused)} docs matched mined hyphen tokens (×{align_boost})")
            
            # >>> FIX: Apply structure-based ranking nudges (generic, pattern-free)
            # Nudge 1: Labels with ≥2 hyphens (id-like structure) × 1.06
            # Nudge 2: Contains COMPUTE + numbers (calculation logic) × 1.05
            structure_nudge_count = {"hyphen": 0, "compute": 0}
            for doc in round5_fused:
                para_name = doc.get('paragraph_name', '')
                source_excerpt = doc.get('source_excerpt', '')
                original_score = doc.get('@search.score', 0.0)
                nudge_applied = False
                
                # Hyphen nudge (≥2 hyphens in label)
                if para_name.count('-') >= 2:
                    doc['@search.score'] = original_score * 1.06
                    structure_nudge_count["hyphen"] += 1
                    nudge_applied = True
                
                # COMPUTE nudge (contains COMPUTE + digits)
                if 'COMPUTE' in source_excerpt.upper() and re.search(r'\d', source_excerpt):
                    current_score = doc.get('@search.score', original_score)
                    doc['@search.score'] = current_score * 1.05
                    structure_nudge_count["compute"] += 1
                    nudge_applied = True
                
                if nudge_applied:
                    doc['_structure_nudges_applied'] = True
            
            logger.info(f"   📊 Structure nudges: {structure_nudge_count['hyphen']} hyphen-rich labels, {structure_nudge_count['compute']} compute logic")
            
            # >>> TIER A FIX 1: Apply soft boost for likely programs (fail-open)
            # >>> TUNE: Confirm this is a soft bonus, NOT a filter
            LIKELY_PROGRAM_BONUS = 1.05
            boosted_count = 0
            if likely_programs:
                for doc in round5_fused:
                    if doc.get('program_id') in likely_programs:
                        doc['@search.score'] = doc.get('@search.score', 0.0) * LIKELY_PROGRAM_BONUS
                        boosted_count += 1
                logger.info(f"   🎯 Soft boost (NO FILTER): {boosted_count}/{len(round5_fused)} docs matched likely programs")
            
            # >>> TUNE: Keep more from Round 5 (was top 20, now use max_fused_docs/10)
            round5_keep = min(len(round5_fused), max(100, self.config.max_fused_docs // 5))  # Keep at least 100, up to 20% of max
            
            # Tag and add to additional_chunks
            for doc in round5_fused[:round5_keep]:
                doc['_index_type'] = 'paragraphs'
                doc['_search_method'] = 'multi_hop_round5_4leg_rrf'  # >>> FIX: Was 3leg, now includes policy leg
                additional_chunks.append(doc)
            
            # >>> TUNE: Enhanced logging for tuning verification
            logger.info(f"   ✅ Round 5 final: fused={initial_fused_count}, after_cap={len(round5_fused)}, kept={round5_keep}")
            logger.info(f"   📊 Round 5 pipeline: legs→fused→cap→cutoff→nudges→boost→kept = {len(lex_results)}+{len(vec_results)}+{len(id_results)}+{len(policy_results)}→{initial_fused_count}→{len(round5_fused)}→{round5_keep}")
        
        logger.info(f"🔗 MULTI-HOP: Retrieved {len(additional_chunks)} additional chunks across all rounds")
        return additional_chunks
    
    def _get_all_program_copybooks(self, program_name: str, max_results: int) -> List[Dict[str, Any]]:
        """Fallback: Get all copybooks for a program.
        
        Only applies deterministic filter if program_name is a valid program ID.
        """
        # Validate it's a real program before applying filter
        if not self._is_program_id(program_name):
            logger.warning(f"⚙️ Invalid program ID '{program_name}' in _get_all_program_copybooks, returning empty")
            return []
        
        copybook_index = self.config.get_index_name('copybook_usage')
        url = f"{self.config.search_endpoint}/indexes/{copybook_index}/docs/search?api-version={self.config.search_api_version}"
        
        body = {
            "search": "*",
            "filter": f"program_id eq '{program_name}'",
            "select": "*",
            "top": 500
        }
        
        logger.info(f"   ✅ Applying deterministic filter for all copybooks in: {program_name}")
        
        try:
            response = requests.post(url, headers=self.search_headers, json=body, timeout=30)
            response.raise_for_status()
            docs = response.json().get('value', [])
            
            # Dedupe
            seen = set()
            unique = []
            for doc in docs:
                cb = doc.get('copybook_name', '')
                if cb and cb not in seen:
                    seen.add(cb)
                    doc['_index_type'] = 'copybook_usage'
                    doc['@search.score'] = 1.0
                    unique.append(doc)
            
            return unique[:max_results]
        except Exception as e:
            logger.error(f"Failed fallback copybooks: {e}")
            return []
