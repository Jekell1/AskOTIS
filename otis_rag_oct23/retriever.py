"""Hybrid Retrieval - Semantic + Lexical search across indexes."""

import json
import logging
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
            api_key=config.openai_key,
            api_version="2024-08-01-preview",
            azure_endpoint=config.openai_endpoint
        )
        
        # Search API headers
        self.search_headers = {
            'Content-Type': 'application/json',
            'api-key': config.search_key
        }
    
    def retrieve(self, query: str, indexes: List[str], max_results: int = 5, 
                 index_weights: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """Retrieve relevant documents using hybrid search.
        
        Args:
            query: Search query
            indexes: List of index types to search (e.g., ['code', 'programs'])
            max_results: Maximum results per index  (default 5, increased by caller to 1000)
            index_weights: Optional dict of boost factors per index (e.g., {'screen_nodes': 3.0})
        
        Returns:
            List of documents with metadata
        """
        import time
        start_retrieve = time.time()
        
        all_results = []
        
        # Default weights to 1.0 if not provided
        if index_weights is None:
            index_weights = {}
        
        # Generate query embedding (default 3072 dimensions)
        start_embed = time.time()
        query_vector = self._generate_embedding(query)
        time_embed = time.time() - start_embed
        logger.info(f"⏱️ TIMING: Embedding generation (3072-dim) took {time_embed:.3f}s")
        
        if not query_vector:
            logger.error("Failed to generate query embedding, returning empty results")
            return []
        
        # Also generate 1536-dim embedding if screen_nodes is in the list
        query_vector_1536 = None
        if 'screen_nodes' in indexes:
            start_embed_1536 = time.time()
            query_vector_1536 = self._generate_embedding(query, dimensions=1536)
            time_embed_1536 = time.time() - start_embed_1536
            logger.info(f"⏱️ TIMING: Embedding generation (1536-dim) took {time_embed_1536:.3f}s")
            if not query_vector_1536:
                logger.warning("Failed to generate 1536-dim embedding for screen_nodes")
                query_vector_1536 = None  # Will skip screen_nodes search
        
        # Search each index
        for index_type in indexes:
            start_index_search = time.time()
            
            index_name = self.config.get_index_name(index_type)
            weight = index_weights.get(index_type, 1.0)  # Get weight, default 1.0
            
            # Use 1536-dim vector for screen_nodes, standard for others
            vector_to_use = query_vector_1536 if index_type == 'screen_nodes' else query_vector
            
            # Skip if vector is empty (screen_nodes with failed 1536-dim embedding)
            if not vector_to_use:
                logger.warning(f"Skipping {index_type} - no valid embedding vector")
                continue
            
            results = self._search_index(
                index_name=index_name,
                query=query,
                query_vector=vector_to_use,
                top=max_results  # Use the passed max_results (1000 from config)
            )
            
            time_index_search = time.time() - start_index_search
            logger.info(f"⏱️ TIMING: Search {index_type} took {time_index_search:.3f}s ({len(results)} results)")
            
            # Add index type and apply weight to score
            for result in results:
                result['_index_type'] = index_type
                # Apply weight to search score for ranking
                if '@search.score' in result:
                    original_score = result['@search.score']
                    result['@search.score'] = original_score * weight
                    # Log score boosting for first few results
                    if results.index(result) < 3:
                        logger.info(f"   Score boost for {index_type}: {original_score:.4f} * {weight} = {result['@search.score']:.4f}")
                result['_weight_applied'] = weight
            
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
        
        if wants_all:
            # Return all unique results, up to max_results limit
            final_count = max_results
        else:
            # Dynamic cap based on max_results (which caller sets based on question type)
            # For specific queries (max_results=20): return up to 40 docs
            # For general queries (max_results=50): return up to 100 docs  
            # For complex queries (max_results=100): return up to 200 docs
            raw_half = len(all_results) // 2
            dynamic_cap = min(max_results * 2, 200)  # 2x the per-index limit, max 200
            final_count = min(dynamic_cap, max(10, raw_half))  # At least 10, up to dynamic cap
        
        logger.info(f"📄 Returning {final_count} documents (from {len(all_results)} retrieved)")
        
        final_results = self._rank_results(all_results, final_count)
        
        # Auto-expand submenus if this is a menu query asking for levels
        if 'screen_nodes' in indexes and any(term in query.lower() for term in ['menu', 'level', 'submenu']):
            final_results = self._expand_submenus(final_results, query)
        
        return final_results
    
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
        search_url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version=2024-07-01"
        
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
    
    def _generate_embedding(self, text: str, dimensions: int = None) -> List[float]:
        """Generate embedding for text.
        
        Args:
            text: Text to embed
            dimensions: Optional dimension override (for screen_nodes which uses 1536)
            
        Returns:
            List of floats representing the embedding, or None if generation fails
        """
        try:
            params = {
                "input": text,
                "model": self.config.embed_deployment
            }
            
            # Add dimensions parameter if specified (for text-embedding-3-large)
            if dimensions:
                params["dimensions"] = dimensions
                
            response = self.openai_client.embeddings.create(**params)
            embedding = response.data[0].embedding
            
            if embedding is None:
                logger.warning(f"Embedding generation returned None for text: {text[:50]}...")
                return []
                
            return embedding
        except Exception as e:
            logger.error(f"Failed to generate embedding: {e}")
            return []
    
    def _extract_program_name(self, query: str) -> str:
        """Extract program name from query if present."""
        import re
        # Look for program names (uppercase words, possibly with slashes or extensions)
        # Skip common question words
        skip_words = {'WHAT', 'DOES', 'CALL', 'USE', 'REFERENCE', 'PROGRAM', 'PROGRAMS', 'COPYBOOK', 'COPYBOOKS'}
        
        patterns = [
            r'\b([A-Z][A-Z0-9]{3,}(?:/[A-Z0-9]+)?(?:\.CBL|\.CPY)?)\b',  # At least 4 chars: APIPAY, LP/LONPF2, etc.
        ]
        for pattern in patterns:
            matches = re.findall(pattern, query)
            for match in matches:
                if match.upper() not in skip_words:
                    return match.upper()
        return None
    
    def _enhance_query_for_business_logic(self, query: str, index_name: str) -> str:
        """Enhance query to boost business logic terms over data definitions.
        
        When users ask about modifications/actions, they want to know business constraints
        (e.g., MAX=3 means only 3 fields modifiable), not all screen field definitions.
        
        This method boosts constraint and logic keywords to surface business rules first.
        """
        # Detect if query is asking about user actions/modifications
        action_keywords = [
            'modify', 'modifi', 'change', 'update', 'edit', 
            'user can', 'user options', 'options', 'choices',
            'what fields', 'which fields', 'enter', 'input'
        ]
        is_action_query = any(keyword in query.lower() for keyword in action_keywords)
        
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
    
    def _search_index(self, index_name: str, query: str, query_vector: List[float], top: int) -> List[Dict]:
        """Search a single index using hybrid search."""
        # Use the same API version as other scripts in this workspace
        url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
        
        # DEBUGGING: Log search request details
        logger.info(f"🔍 SEARCH REQUEST for index: {index_name}")
        logger.info(f"   Query: {query}")
        logger.info(f"   Top: {top}")
        logger.info(f"   URL: {url}")
        vector_dim = len(query_vector) if query_vector is not None else 'None'
        logger.info(f"   Vector dimensions: {vector_dim}")
        
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
            # Add filter to find calls from this program
            body["filter"] = f"caller_program eq '{program_name}'"
            # Increase top to get more results when filtering
            body["top"] = min(top * 2, 1000)
        
        # Only add vector search if field exists AND it's not a list-all query
        # (vector search has k limit of 100-1000 depending on tier)
        if vector_field and not is_list_all:
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
            'new_cobol_screen_nodes': 'summary_vector',  # Now has embeddings (1536-dim)
            
            # Dependencies
            'new_cobol_program_deps': 'dependency_blob_vector',
            'enhanced_cobol_dependencies': ''  # No vector field yet
        }
        return vector_fields.get(index_name, '')
    
    def _rank_results(self, results: List[Dict], max_results: int) -> List[Dict]:
        """Rank and deduplicate results by relevance score."""
        # Remove duplicates based on document ID
        seen = set()
        unique_results = []
        
        for result in results:
            # Create unique key (index + document ID)
            # Support different ID fields across different indexes
            doc_id = (
                result.get('chunk_id') or 
                result.get('program_id') or 
                result.get('paragraph_id') or
                result.get('call_id') or 
                result.get('call_hash') or
                result.get('edge_id') or
                result.get('path_id') or
                result.get('tree_id') or
                result.get('screen_id') or
                result.get('usage_id') or
                result.get('symbol_id')
            )
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
        
        return sorted_results[:max_results]
