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
    
    # Routing profiles - focused index groups per domain
    ROUTE_PROFILES = {
        "program": ["code", "code_new", "programs", "calls", "program_deps"],
        "data": ["data_items", "variables", "copybooks", "copybook_usage", "name_aliases"],
        "ui": ["screen_nodes", "ui_paths", "menu_trees"],
        "transaction": ["transactions", "flows", "flow_edges", "programs"],
        "flow": ["flows", "flow_edges", "calls", "program_deps", "paragraphs"],
        "complexity": ["complexity", "programs", "flows", "paragraphs", "calls", "program_deps"],
        "files": ["files", "code", "code_new", "programs"]
    }
    
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
            r'\bcopybook\w*\b',  # copybook or copybooks
            r'\bdata item\b',
            r'\bsymbol\b',
            r'\balias\w*\b',  # aliases
        ],
        'transaction_copybooks': [  # NEW: Transaction-specific copybook questions (needs deterministic handling)
            r'\bcopybook\w*\b.*\b(transaction|handling|processing)\b',
            r'\b(when|for|during)\b.*\bhandling\b.*\bcopybook\w*\b',
            r'\bcopybook\w*\b.*\bused\b.*\b(for|when|during)\b.*\b(transaction|processing)\b',
            r'\b(list|show|what)\b.*\bcopybook\w*\b.*\b(for|when)\b.*\b[A-Z]{2}\b',  # "copybooks for RP"
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
            r'\buse\w*\b.*\bcopybook\w*\b',  # uses copybook or copybooks
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
        
        # 🔹 PRIORITY ROUTING: Detect copybook usage queries early (BEFORE general classification)
        # Pattern: "what programs use LPBW1IN.CPY" or "which programs use .cpy files"
        if '.cpy' in query_lower and ('use' in query_lower or 'used' in query_lower or 'reference' in query_lower):
            logger.info("📄 COPYBOOK USAGE QUERY DETECTED - applying specialized routing")
            return {
                'is_otis': True,
                'question_type': 'copybook_usage',
                'search_indexes': ['copybook_usage', 'copybooks'],
                'index_weights': {'copybook_usage': 3.0, 'copybooks': 1.0},
                'has_menu_intent': False,
                'clean_query': query,
                'original_query': query,
                'max_results': 1000  # Override to get all matches
            }
        
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
        
        # DEBUGGING: Log intent detection
        logger.info(f"🎯 Menu intent: {has_menu_intent}, Question type: {question_type}")
        logger.info(f"📋 Indexes to search: {indexes}")
        
        # Boost screen_nodes and UI indexes for menu/UI queries
        if has_menu_intent or question_type == 'menu':
            if 'screen_nodes' in weights:
                weights['screen_nodes'] = 20.0  # VERY strong boost (restored from Oct 23)
            if 'ui_paths' in weights:
                weights['ui_paths'] = 3.0
            if 'menu_trees' in weights:
                weights['menu_trees'] = 3.0
            # Suppress non-UI indexes
            if 'code' in weights:
                weights['code'] = 0.5
            if 'code_new' in weights:
                weights['code_new'] = 0.5
            if 'transactions' in weights:
                weights['transactions'] = 0.05  # Heavily suppress (restored from Oct 23)
            logger.info(f"⚡ Applied menu boosting - screen_nodes: 20.0x, ui_paths/menu_trees: 3.0x")
        
        # Boost complexity index for complexity questions
        elif question_type == 'complexity':
            if 'complexity' in weights:
                weights['complexity'] = 2.5
            if 'programs' in weights:
                weights['programs'] = 1.5
        
        # Boost copybook_usage for data/copybook questions
        elif question_type in ('explain_data', 'transaction_copybooks'):
            if 'copybook_usage' in weights:
                weights['copybook_usage'] = 10.0  # Very strong boost for relationships
            if 'copybooks' in weights:
                weights['copybooks'] = 2.0  # Moderate boost for metadata
            # Deprioritize code indexes
            if 'code' in weights:
                weights['code'] = 0.3
            if 'code_new' in weights:
                weights['code_new'] = 0.3
            logger.info(f"⚡ Applied copybook boosting - copybook_usage: 10.0x, code: 0.3x")
        
        # Boost transaction and UI indexes for transaction questions
        elif question_type == 'transaction':
            if 'transactions' in weights:
                weights['transactions'] = 3.0  # Strong boost
            if 'ui_paths' in weights:
                weights['ui_paths'] = 2.0
            if 'menu_trees' in weights:
                weights['menu_trees'] = 2.0
            if 'screen_nodes' in weights:
                weights['screen_nodes'] = 2.0
            logger.info(f"⚡ Applied transaction boosting - transactions: 3.0x, UI: 2.0x")
        
        # Boost flow indexes for trace_flow questions
        elif question_type == 'trace_flow':
            if 'flows' in weights:
                weights['flows'] = 2.0
            if 'flow_edges' in weights:
                weights['flow_edges'] = 2.0
            if 'calls' in weights:
                weights['calls'] = 1.5
        
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
        2. transaction_copybooks (deterministic transaction copybook queries)
        3. transaction (specific business questions)
        4. complexity (specific technical questions)  
        5. explain_program, find_code, explain_data, trace_flow
        6. general (fallback)
        """
        # Check for menu questions FIRST (most specific)
        if 'menu' in self.QUESTION_PATTERNS:
            patterns = self.QUESTION_PATTERNS['menu']
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return 'menu'
        
        # Check for transaction_copybooks (deterministic handling required)
        if 'transaction_copybooks' in self.QUESTION_PATTERNS:
            patterns = self.QUESTION_PATTERNS['transaction_copybooks']
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return 'transaction_copybooks'
        
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
        """Select which indexes to search based on question type using routing profiles.
        
        Uses focused index groups per domain to reduce noise and latency.
        Handles mixed-intent questions by checking for secondary keywords.
        """
        # Select indexes based on primary question type using routing profiles
        if question_type == 'explain_program':
            # Program structure and flow
            indexes = list(self.ROUTE_PROFILES["program"]) + list(self.ROUTE_PROFILES["flow"])
        
        elif question_type == 'find_code':
            # Code search with metadata
            indexes = list(self.ROUTE_PROFILES["files"])
        
        elif question_type == 'explain_data':
            # Data structures and usage
            indexes = list(self.ROUTE_PROFILES["data"])
        
        elif question_type == 'trace_flow':
            # Flow and navigation - includes UI paths and data for screen field definitions
            indexes = list(self.ROUTE_PROFILES["flow"]) + \
                     list(self.ROUTE_PROFILES["ui"]) + \
                     ["data_items", "copybooks"]
        
        elif question_type == 'menu':
            # Menu and screen text
            indexes = list(self.ROUTE_PROFILES["ui"])
        
        elif question_type == 'transaction':
            # Transaction codes and workflows
            indexes = list(self.ROUTE_PROFILES["transaction"]) + \
                     list(self.ROUTE_PROFILES["ui"])
        
        elif question_type == 'transaction_copybooks':
            # Transaction-specific copybook questions (deterministic handling)
            indexes = list(self.ROUTE_PROFILES["data"]) + ["transactions"]
        
        elif question_type == 'complexity':
            # Program complexity and performance
            indexes = list(self.ROUTE_PROFILES["complexity"])
        
        else:  # general
            # Broad search across program and data domains
            indexes = list(self.ROUTE_PROFILES["program"]) + \
                     list(self.ROUTE_PROFILES["data"]) + \
                     ["files"]
        
        # Remove duplicates while preserving order
        seen = set()
        indexes = [x for x in indexes if not (x in seen or seen.add(x))]
        
        # Handle mixed-intent questions: add secondary indexes if keywords present
        if query_lower:
            # Add complexity profile if complexity keywords detected
            if self._has_complexity_intent(query_lower) and 'complexity' not in indexes:
                for idx in self.ROUTE_PROFILES["complexity"]:
                    if idx not in indexes:
                        indexes.append(idx)
            
            # Add transaction profile if transaction keywords detected
            if self._has_transaction_intent(query_lower) and 'transactions' not in indexes:
                for idx in self.ROUTE_PROFILES["transaction"]:
                    if idx not in indexes:
                        indexes.append(idx)
        
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
