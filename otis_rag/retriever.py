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
                top=max_results,  # Use the passed max_results (1000 from config)
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
            # For specific queries (max_results=20): return up to 40 docs
            # For general queries (max_results=50): return up to 100 docs  
            # For complex queries (max_results=100): return up to 200 docs
            raw_half = len(all_results) // 2
            dynamic_cap = min(max_results * 2, 200)  # 2x the per-index limit, max 200
            final_count = min(dynamic_cap, max(10, raw_half))  # At least 10, up to dynamic cap
        
        logger.info(f"📄 Retrieved {len(all_results)} semantic results + {len(filtered_relationship_results)} filtered relationship results")
        
        # Filter menu screens if this is a menu query - prioritize actual menu content over navigation links
        if 'screen_nodes' in indexes and question_type == 'menu':
            all_results = self._filter_menu_screens(all_results, query)
            logger.info(f"📋 After menu filtering: {len(all_results)} results")
        
        # Rank semantic search results normally
        semantic_results = self._rank_results(all_results, final_count)
        
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
            
            # Count numbered menu options (1. through 99.)
            numbered_options = len(re.findall(r'\b(\d{1,2})\.\s+[A-Z]', summary))
            
            # Check for F-key navigation patterns (just for nav, not content)
            has_f_key_only = bool(re.search(r'F\d+\s*-\s*(MASTER\s+)?MENU', summary, re.IGNORECASE)) and numbered_options < 2
            
            # Check if this mentions "MASTER MENU" or similar as content (not just F-key)
            has_master_menu_content = bool(re.search(r'M\s*A\s*S\s*T\s*E\s*R\s+(M|ME|MEN|MENU)', summary, re.IGNORECASE))
            
            # Check if this matches the specific menu type being queried
            matches_target_menu = target_menu_pattern and bool(re.search(target_menu_pattern, summary_upper, re.IGNORECASE))
            
            # Classify the screen
            if matches_target_menu and numbered_options >= 2:
                # Query for specific menu + screen has that menu name + has options = PERFECT match
                primary_menus.append(result)
                logger.debug(f"  ✓✓✓ TARGET menu: {result.get('screen_id', '')[:20]}... (matches query + {numbered_options} options)")
            elif is_main_menu_query and has_master_menu_content and numbered_options >= 2:
                # Main menu query + explicit "MASTER MENU" text + multiple options = PRIMARY match
                primary_menus.append(result)
                logger.debug(f"  ✓✓ PRIMARY menu: {result.get('screen_id', '')[:20]}... (MASTER MENU + {numbered_options} options)")
            elif numbered_options >= 4:
                # Has many numbered options - likely a real menu screen
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
            
            # Collection Processing menu
            elif 'collection' in query_lower:
                menu_boost_terms.extend(['COLLECTION PROCESSING', 'COLLECTION MENU',
                                        'DELINQUENCY', 'COLLECTION ENTRY'])
            
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
        url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
        
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
    
    def _rank_results(self, results: List[Dict], max_results: int) -> List[Dict]:
        """Rank and deduplicate results by relevance score."""
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
        
        # 🔹 DIAGNOSTIC LOGGING for copybook_usage
        copybook_usage_unique_count = sum(1 for r in unique_results if r.get('_index_type') == 'copybook_usage')
        if copybook_usage_raw_count > 0:
            logger.info(f"📊 Copybook_usage raw={copybook_usage_raw_count} unique={copybook_usage_unique_count}")
        
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
        code_url = f"{self.config.search_endpoint}/indexes/{code_index}/docs/search?api-version=2025-08-01-preview"
        
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
        copybook_url = f"{self.config.search_endpoint}/indexes/{copybook_index}/docs/search?api-version=2025-08-01-preview"
        
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
        url = f"{self.config.search_endpoint}/indexes/{copybook_meta_index}/docs/search?api-version=2025-08-01-preview"
        
        body = {
            "vector": {
                "value": embedding,
                "fields": "description_vector",
                "k": max_results * 2  # Get more candidates
            },
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
    
    def _get_all_program_copybooks(self, program_name: str, max_results: int) -> List[Dict[str, Any]]:
        """Fallback: Get all copybooks for a program.
        
        Only applies deterministic filter if program_name is a valid program ID.
        """
        # Validate it's a real program before applying filter
        if not self._is_program_id(program_name):
            logger.warning(f"⚙️ Invalid program ID '{program_name}' in _get_all_program_copybooks, returning empty")
            return []
        
        copybook_index = self.config.get_index_name('copybook_usage')
        url = f"{self.config.search_endpoint}/indexes/{copybook_index}/docs/search?api-version=2025-08-01-preview"
        
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
