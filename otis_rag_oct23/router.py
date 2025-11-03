"""Query Router - Determines if question is about OTIS application or general COBOL."""

import re
import logging
from typing import Dict, Literal, Optional

# Set up logging
logger = logging.getLogger(__name__)


class QueryRouter:
    """Routes queries to appropriate handler based on intent."""
    
    # OTIS-specific keywords (application name variations)
    OTIS_KEYWORDS = {
        'otis', 'otos',  # Common variations
        'otis system', 'otos system',
        'otis application', 'otos application'
    }
    
    # Menu/UI intent keywords for boosting screen_nodes
    MENU_UI_KEYWORDS = [
        'menu', 'screen', 'choice', 'option', 'label', 'prompt', 
        'ui', 'display', 'user interface', 'menu option', 'menu choice',
        'screen text', 'user sees', 'shown to user', 'appears on screen'
    ]
    
    # Question type patterns (expanded for new indexes + Phase 2)
    QUESTION_PATTERNS = {
        'menu': [  # NEW: Explicit menu/screen text questions (check FIRST)
            r'\bmenu\b.*\b(choice|option|selection|text|display|show)\w*\b',
            r'\b(choice|option|selection)\w*\b.*\b(menu|screen)\b',
            r'\bwhat\b.*\b(choice|option|selection)\w*\b.*\b(display|show|see)\w*\b',
            r'\b(display|show)\w*\b.*\b(menu|screen)\b',
            r'\buser\b.*\bsee\w*\b.*\b(menu|screen)\b',
        ],
        'explain_program': [
            r'\bwhat does\b.*\bprogram\b',
            r'\bexplain\b.*\bprogram\b',
            r'\bhow does\b.*\bprogram\b.*\bwork\b',
            r'\bprogram\b.*\bdo\b',
            r'\bdependenc\w+\b.*\bprogram\b',  # dependencies
        ],
        'find_code': [
            r'\bfind\b.*\bcode\b',
            r'\bwhere is\b',
            r'\bshow me\b.*\bcode\b',
            r'\bsearch for\b',
            r'\bfile\b.*\bcontain\w+\b',
        ],
        'explain_data': [
            r'\bwhat is\b.*\bfield\b',
            r'\bwhat is\b.*\bvariable\b',
            r'\bdata structure\b',
            r'\brecord\b.*\blayout\b',
            r'\bcopybook\b',
            r'\bdata item\b',
            r'\bsymbol\b',
            r'\balias\w*\b',  # aliases
        ],
        'trace_flow': [
            r'\btrace\b',
            r'\bflow\b',
            r'\bcall\b.*\bchain\b',
            r'\bcalls?\b',  # "call" or "calls"
            r'\breference\w*\b',  # references, referenced
            r'\bdependen\w+\b',  # dependency, dependencies, depends
            r'\bexecution\b.*\bpath\b',
            r'\bmenu\b',
            r'\bscreen\b',
            r'\bnavigation\b',
            r'\bui\b.*\bpath\b',
            r'\buser interface\b',
            r'\buse\w*\b.*\bcopybook\b',  # uses copybook
            r'\binclude\w*\b',  # includes
        ],
        'transaction': [  # NEW: Phase 2 transaction questions
            r'\btransaction\b.*\bcode\b',
            r'\bwhat happens when\b.*\benter\w*\b',
            r'\buser enters\b',
            r'\btx\b.*\bcode\b',
            r'\bworkflow\b',
            r'\bmenu\b.*\bcode\b',
        ],
        'complexity': [  # NEW: Phase 2 complexity questions
            r'\bcomplex\b',
            r'\bcomplexity\b',
            r'\bdifficult\b.*\bmaintain\b',
            r'\bhard to\b.*\bunderstand\b',
            r'\bperformance\b.*\brisk\w*\b',  # performance risk/risks
            r'\brisk\w*\b.*\bperformance\b',  # risks in performance
            r'\bwhich programs\b.*\bcomplex\b',
            r'\bmost complex\b',
            r'\bnesting\b.*\bdepth\b',
            r'\bdeep\w*\b.*\bnest\w+\b',
            r'\bhigh\b.*\bcomplexity\b',
            r'\bfile\b.*\bi/?o\b',  # file I/O
            r'\bcyclomatic\b',
            r'\blines of code\b',
            r'\bloc\b',
        ],
        'general': []  # Catch-all
    }
    
    def route(self, query: str) -> Dict[str, any]:
        """Route query and determine intent.
        
        Returns:
            {
                'is_otis': bool,           # Is this about OTIS application?
                'question_type': str,      # Type of question
                'search_indexes': list,    # Which indexes to search
                'index_weights': dict,     # Boost factors per index
                'clean_query': str         # Query with OTIS references normalized
            }
        """
        query_lower = query.lower()
        
        # 1. Determine if OTIS-related
        is_otis = self._is_otis_question(query_lower)
        
        # 2. Classify question type
        question_type = self._classify_question(query_lower)
        
        # 3. Detect menu/UI intent for boosting
        has_menu_intent = self._detect_menu_intent(query_lower)
        
        # 4. Determine which indexes to search (pass query for mixed-intent detection)
        search_indexes = self._select_indexes(question_type, is_otis, query_lower)
        
        # 5. Calculate index weights based on intent
        index_weights = self._calculate_index_weights(search_indexes, has_menu_intent, question_type)
        
        # 6. Clean query (remove OTIS references if found)
        clean_query = self._clean_query(query, is_otis)
        
        return {
            'is_otis': is_otis,
            'question_type': question_type,
            'search_indexes': search_indexes,
            'index_weights': index_weights,
            'has_menu_intent': has_menu_intent,
            'clean_query': clean_query,
            'original_query': query
        }
    
    def _detect_menu_intent(self, query_lower: str) -> bool:
        """Detect if query is asking about menu/UI/screen text.
        
        Returns True if query contains menu/UI keywords.
        """
        return any(keyword in query_lower for keyword in self.MENU_UI_KEYWORDS)
    
    def _calculate_index_weights(self, indexes: list[str], has_menu_intent: bool, 
                                 question_type: str) -> Dict[str, float]:
        """Calculate boost weights for each index based on detected intent.
        
        Args:
            indexes: List of index names to search
            has_menu_intent: Whether query has menu/UI intent
            question_type: Type of question (complexity, code, etc.)
            
        Returns:
            Dict mapping index name to boost factor (1.0 = normal, >1.0 = boosted, <1.0 = deprioritized)
        """
        weights = {idx: 1.0 for idx in indexes}
        
        # DEBUGGING: Log menu intent detection
        logger.info(f"🎯 Menu intent detected: {has_menu_intent}")
        logger.info(f"📋 Indexes to search: {indexes}")
        
        # Boost screen_nodes for menu/UI queries
        if has_menu_intent and 'screen_nodes' in weights:
            weights['screen_nodes'] = 20.0  # VERY strong boost to compete with transactions
            # Heavily deprioritize non-screen indexes for menu queries
            if 'code' in weights:
                weights['code'] = 0.5
            if 'code_new' in weights:
                weights['code_new'] = 0.5
            if 'copybooks' in weights:
                weights['copybooks'] = 0.5
            # Transactions have very high base scores - suppress heavily
            if 'transactions' in weights:
                weights['transactions'] = 0.05  # Reduce ~40x to bring in line with screen_nodes
            if 'flows' in weights:
                weights['flows'] = 0.7
            if 'flow_edges' in weights:
                weights['flow_edges'] = 0.7
            logger.info(f"⚡ Applied menu boosting - screen_nodes: 20.0x, transactions: 0.05x")
        
        # Boost complexity index for complexity questions
        elif question_type == 'complexity' and 'complexity' in weights:
            weights['complexity'] = 2.0
            weights['programs'] = 1.5
        
        # Boost transaction index for transaction questions
        elif question_type == 'transaction' and 'transactions' in weights:
            weights['transactions'] = 2.0
            weights['ui_paths'] = 1.5
            weights['menu_trees'] = 1.5
        
        logger.info(f"⚖️  Final weights: {weights}")
        
        return weights
    
    def _is_otis_question(self, query_lower: str) -> bool:
        """Check if question mentions OTIS/OTOS."""
        # Check for exact keyword matches
        for keyword in self.OTIS_KEYWORDS:
            if keyword in query_lower:
                return True
        
        # Check for context clues
        if 'this application' in query_lower or 'this system' in query_lower:
            return True
        
        return False
    
    def _classify_question(self, query_lower: str) -> str:
        """Classify the type of question being asked.
        
        Returns the most specific match. Priority order:
        1. menu (specific UI/screen questions - HIGHEST PRIORITY)
        2. transaction (specific business questions)
        3. complexity (specific technical questions)  
        4. explain_program, find_code, explain_data, trace_flow
        5. general (fallback)
        """
        # Check for menu questions FIRST (most specific)
        if 'menu' in self.QUESTION_PATTERNS:
            patterns = self.QUESTION_PATTERNS['menu']
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return 'menu'
        
        # Check for Phase 2 specific patterns (high priority)
        for q_type in ['transaction', 'complexity']:
            patterns = self.QUESTION_PATTERNS[q_type]
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return q_type
        
        # Then check other patterns
        for q_type in ['explain_program', 'find_code', 'explain_data', 'trace_flow']:
            patterns = self.QUESTION_PATTERNS[q_type]
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return q_type
        
        return 'general'
    
    def _has_complexity_intent(self, query_lower: str) -> bool:
        """Check if query mentions complexity (for mixed-intent questions)."""
        complexity_keywords = ['complex', 'complexity', 'nesting', 'performance', 
                              'risk', 'difficult', 'cyclomatic', 'loc', 'lines of code']
        return any(kw in query_lower for kw in complexity_keywords)
    
    def _has_transaction_intent(self, query_lower: str) -> bool:
        """Check if query mentions transactions (for mixed-intent questions)."""
        transaction_keywords = ['transaction', 'tx', 'code', 'workflow', 'enters']
        return any(kw in query_lower for kw in transaction_keywords)
    
    def _select_indexes(self, question_type: str, is_otis: bool, query_lower: str = '') -> list[str]:
        """Select which indexes to search based on question type.
        
        Expanded to intelligently use all 20 available indexes (18 original + 2 Phase 2).
        Handles mixed-intent questions by checking for secondary keywords.
        """
        # Always include primary code chunks
        indexes = ['code', 'code_new']
        
        # Add indexes based on primary question type
        if question_type == 'explain_program':
            # Program structure and flow
            indexes.extend(['programs', 'flows', 'flow_edges', 'paragraphs', 
                          'calls', 'program_deps', 'files', 'complexity'])  # Added complexity
        
        elif question_type == 'find_code':
            # Code search with metadata
            indexes.extend(['programs', 'paragraphs', 'files'])
        
        elif question_type == 'explain_data':
            # Data structures and usage
            # Note: symbol_refs uses 1536d embeddings (incompatible with our 3072d model)
            indexes.extend(['data_items', 'variables', 'copybooks', 
                          'copybook_usage', 'name_aliases'])
        
        elif question_type == 'trace_flow':
            # Flow and navigation - INCLUDES data_items for screen field definitions
            indexes.extend(['flows', 'flow_edges', 'calls', 'ui_paths', 
                          'menu_trees', 'screen_nodes', 'program_deps', 
                          'data_items', 'copybooks', 'transactions'])  # Added data_items, copybooks for screen text
        
        elif question_type == 'transaction':  # NEW: Phase 2 transaction questions
            # Transaction codes and workflows
            indexes.extend(['transactions', 'ui_paths', 'menu_trees', 'screen_nodes', 
                          'programs', 'flows', 'flow_edges'])
        
        elif question_type == 'complexity':  # NEW: Phase 2 complexity questions
            # Program complexity and performance
            indexes.extend(['complexity', 'programs', 'flows', 'paragraphs', 
                          'calls', 'program_deps'])
        
        else:  # general
            # Broad search across key indexes - include screen_nodes for menu/UI questions
            indexes.extend(['programs', 'paragraphs', 'data_items', 
                          'flows', 'files', 'calls', 'program_deps', 'screen_nodes'])
        
        # Handle mixed-intent questions: add secondary indexes if keywords present
        if query_lower:
            # Add complexity index if complexity keywords detected
            if self._has_complexity_intent(query_lower) and 'complexity' not in indexes:
                indexes.append('complexity')
            
            # Add transaction index if transaction keywords detected
            if self._has_transaction_intent(query_lower) and 'transactions' not in indexes:
                indexes.append('transactions')
        
        return indexes
    
    def _clean_query(self, query: str, is_otis: bool) -> str:
        """Clean query by removing OTIS references if present."""
        if not is_otis:
            return query
        
        # Remove OTIS/OTOS references
        clean = query
        for keyword in self.OTIS_KEYWORDS:
            # Case-insensitive replacement
            clean = re.sub(rf'\b{keyword}\b', '', clean, flags=re.IGNORECASE)
        
        # Clean up extra whitespace
        clean = re.sub(r'\s+', ' ', clean).strip()
        
        return clean


# Convenience function
def route_query(query: str) -> Dict[str, any]:
    """Route a query. Convenience function."""
    router = QueryRouter()
    return router.route(query)
