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
    
    # Routing profiles - focused index groups per domain (updated after index cleanup)
    ROUTE_PROFILES = {
        "program": ["code", "programs", "flows"],
        "data": ["data_items", "variables", "copybooks", "copybook_usage"],
        "ui": ["screen_nodes", "ui_paths", "help_fields"],  # screen_nodes for definitions, help_fields for documentation
        "transaction": ["flows", "flow_edges", "programs"],  # removed transactions (deleted)
        "flow": ["flows", "flow_edges", "paragraphs"],  # removed calls and program_deps (deleted)
        "complexity": ["programs", "flows", "paragraphs", "flow_edges"],  # removed complexity (deleted)
        "files": ["files", "code", "programs"]
    }
    
    # Question type patterns (expanded for new indexes + Phase 2)
    QUESTION_PATTERNS = {
        'implementation': [  # NEW: Implementation/calculation/formula questions (CHECK FIRST - needs code details)
            r'\bhow (is|are|does)\b.*\b(calculated|computed|built|processed|created|generated|determined)\b',
            r'\bwhat (is|are) the (formula|calculation|computation|logic|process|method|algorithm)\b',
            r'\b(calculation|computation|formula|logic|process|method)\b.*\bfor\b',
            r'\bhow\b.*\b(calculate|compute|build|process|create|determine)\b',
            r'\b(explain|describe)\b.*\b(calculation|computation|formula|process|method)\b',
            r'\bwhat.*formula.*use\w*\b',
            r'\b(show|tell) me.*how.*calculated\b',
            r'\bhow.*work.*internally\b',
            r'\bimplementation.*detail\w*\b',
            r'\bcode.*calculate\w*\b',
        ],
        'screen_usage': [  # Screen/field usage questions (check FIRST - prioritizes help_fields)
            # Field-specific questions
            r'\bhow do i (use|fill out|fill in|complete)\b.*\b(screen|field)\b',
            r'\bwhat do i (enter|type|input|put)\b.*\b(field|screen)\b',
            r'\bwhat (goes|information|data|value\w*) (in|into|on)\b.*\b(screen|field)\b',
            r'\bwhat value\w* can i enter\b',  # "what values can I enter"
            r'\bfield instructions?\b',
            r'\bhelp (text|for)\b.*\b(screen|field)\b',
            r'\bwhat (does|is)\b.*\bfield\b.*\b(do|for|mean|used)\b',
            r'\bexplain\b.*\bfield\b',
            r'\bvalid values?\b.*\bfield\b',
            r'\bwhat can i enter\b',
            r'\bwhat am i supposed to\b',
            r'\bwhat do the fields\b.*\bdo\b',
            # Function key questions - NEW
            r'\bfunction key\w*\b.*\b(available|screen|use)\b',
            r'\bwhat (function )?key\w*\b.*\b(available|screen)\b',
            r'\bwhat does (f\d+|function key)\b',  # "what does F7" or "what does function key"
            r'\b(f\d+|function key\w*)\b.*\bdo\b',  # "F7 do" or "function keys do"
            r'\bkeyboard shortcut\w*\b',
            r'\bpress\b.*\b(key|f\d+)\b',  # "press F7" or "press key"
        ],
        'menu': [  # Explicit menu/screen text questions
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
        # Pattern: "what programs use LPBW1IN.CPY" or "which programs use copybook X" or "programs that reference AGEALP"
        copybook_usage_patterns = [
            ('.cpy' in query_lower and any(kw in query_lower for kw in ['use', 'used', 'reference', 'include'])),
            ('copybook' in query_lower and any(kw in query_lower for kw in ['use', 'used', 'reference', 'include', 'call'])),
            (any(kw in query_lower for kw in ['what programs', 'which programs', 'programs that']) and 
             any(kw in query_lower for kw in ['use', 'used', 'reference']) and
             any(kw in query_lower for kw in ['copybook', 'file', '.cpy']))
        ]
        
        if any(copybook_usage_patterns):
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
        
        # 🔹 PRIORITY ROUTING: Detect "files/copybooks used in menu" queries
        # Pattern: "what files are used in menu X" or "what copybooks does menu option Y use"
        menu_dependency_patterns = [
            r'\bfile\w*\b.*\buse\w*\b.*\bmenu\b',  # "files used in menu"
            r'\bmenu\b.*\buse\w*\b.*\bfile\w*\b',  # "menu uses files"
            r'\bcopybook\w*\b.*\buse\w*\b.*\bmenu\b',  # "copybooks used in menu"
            r'\bmenu\b.*\b(option|item|choice)\b.*\b(file\w*|copybook\w*)\b',  # "menu option files"
            r'\b(file\w*|copybook\w*)\b.*\bmenu\b.*\b(option|item)\b',  # "files in menu option"
            r'\bwhat\b.*\b(file\w*|copybook\w*)\b.*\bfor\b.*\bmenu\b',  # "what files for menu"
        ]
        if any(re.search(pattern, query_lower) for pattern in menu_dependency_patterns):
            logger.info("📋🔗 MENU DEPENDENCY QUERY DETECTED - routing to screens + program_deps + ui_paths")
            return {
                'is_otis': True,
                'question_type': 'menu_dependencies',
                'search_indexes': ['screens', 'ui_paths', 'program_deps', 'copybooks'],
                'index_weights': {
                    'screens': 10.0,      # Strong boost to find the menu screen
                    'ui_paths': 5.0,      # Find navigation to identify programs
                    'program_deps': 8.0,  # Get dependencies of those programs
                    'copybooks': 2.0      # Get copybook metadata
                },
                'has_menu_intent': True,
                'clean_query': query,
                'original_query': query,
                'max_results': 200  # INCREASED: Need many program_deps docs for recursive traversal
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
        
        # 7. Expand query for state-specific implementation questions
        if question_type == 'implementation':
            # Detect state-specific queries and add relevant keywords
            state_expansions = self._expand_state_keywords(query_lower)
            if state_expansions:
                clean_query = clean_query + " " + state_expansions
                logger.info(f"🗺️  State-specific query detected - adding keywords: {state_expansions}")
        
        # 8. Expand query for main menu queries to improve retrieval
        if question_type == 'menu' and 'main' in query.lower() and 'menu' in query.lower():
            # For main menu queries, add explicit search terms that appear in the LPMENU screen
            clean_query = clean_query + " MASTER MENU DAILY PROCESSING REPORTS"
            logger.info("📋 Main menu query detected - adding search terms: MASTER MENU, DAILY PROCESSING, REPORTS")
        
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
        
        # NEW: Boost code chunks for implementation/calculation questions (HIGHEST PRIORITY)
        if question_type == 'implementation':
            if 'code' in weights:
                weights['code'] = 10.0  # Very strong boost for actual source code
            if 'copybooks' in weights:
                weights['copybooks'] = 5.0  # Strong boost for copybook definitions (formulas often here)
            if 'paragraphs' in weights:
                weights['paragraphs'] = 3.0  # Moderate boost for detailed code sections
            if 'copybook_usage' in weights:
                weights['copybook_usage'] = 1.5  # Light boost for relationships
            # Suppress high-level indexes for implementation questions
            if 'programs' in weights:
                weights['programs'] = 0.5  # Deprioritize summaries
            if 'flows' in weights:
                weights['flows'] = 0.3  # Deprioritize flow diagrams
            logger.info(f"⚡ Applied implementation boosting - code: 10.0x, copybooks: 5.0x, paragraphs: 3.0x")
        
        # NEW: Boost help_fields for screen usage questions (HIGHEST PRIORITY)
        if question_type == 'screen_usage':
            if 'help_fields' in weights:
                weights['help_fields'] = 8.0  # Very strong boost for field instructions
            if 'screen_nodes' in weights:
                weights['screen_nodes'] = 2.0  # Moderate boost for structure
            if 'ui_paths' in weights:
                weights['ui_paths'] = 1.5  # Light boost for navigation context
            # Suppress flow/code indexes for usage questions
            if 'flows' in weights:
                weights['flows'] = 0.3
            if 'flow_edges' in weights:
                weights['flow_edges'] = 0.3
            if 'code' in weights:
                weights['code'] = 0.2
            logger.info(f"⚡ Applied screen usage boosting - help_fields: 8.0x, screen_nodes: 2.0x")
        
        # Boost screens index for menu/UI queries
        elif has_menu_intent or question_type == 'menu':
            if 'screen_nodes' in weights:
                weights['screen_nodes'] = 30.0  # VERY strong boost - contains actual menu option text
            if 'screens' in weights:
                weights['screens'] = 25.0  # Very strong boost for unified screens index
            if 'ui_paths' in weights:
                weights['ui_paths'] = 5.0
            if 'menu_trees' in weights:
                weights['menu_trees'] = 5.0
            if 'help_fields' in weights:
                weights['help_fields'] = 2.0  # Moderate boost for menu help
            # Suppress non-UI indexes
            if 'code' in weights:
                weights['code'] = 0.5
            logger.info(f"⚡ Applied menu boosting - screen_nodes: 30.0x, screens: 25.0x, ui_paths/menu_trees: 5.0x")
        
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
            logger.info(f"⚡ Applied copybook boosting - copybook_usage: 10.0x, code: 0.3x")
        
        # Boost flow and UI indexes for transaction questions
        elif question_type == 'transaction':
            if 'flows' in weights:
                weights['flows'] = 2.5  # Strong boost for transaction flows
            if 'ui_paths' in weights:
                weights['ui_paths'] = 2.0
            if 'menu_trees' in weights:
                weights['menu_trees'] = 2.0
            if 'screens' in weights:
                weights['screens'] = 2.5  # Boost screens index for transaction queries
            logger.info(f"⚡ Applied transaction boosting - flows: 2.5x, UI: 2.0-2.5x")
        
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
        1. implementation (calculation/formula/process questions - HIGHEST for code detail)
        2. screen_usage (field/screen usage questions - HIGHEST PRIORITY for help_fields)
        3. menu (specific UI/screen questions)
        4. transaction_copybooks (deterministic transaction copybook queries)
        5. transaction (specific business questions)
        6. complexity (specific technical questions)  
        7. explain_program, find_code, explain_data, trace_flow
        8. general (fallback)
        """
        # Check for implementation questions FIRST (most specific for code details)
        if 'implementation' in self.QUESTION_PATTERNS:
            patterns = self.QUESTION_PATTERNS['implementation']
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return 'implementation'
        
        # Check for screen usage questions SECOND (most specific for help_fields)
        if 'screen_usage' in self.QUESTION_PATTERNS:
            patterns = self.QUESTION_PATTERNS['screen_usage']
            for pattern in patterns:
                if re.search(pattern, query_lower):
                    return 'screen_usage'
        
        # Check for menu questions (specific UI/screen structure)
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
        if question_type == 'implementation':
            # NEW: Implementation/calculation/formula questions - prioritize code and copybooks
            indexes = ['code', 'copybooks', 'paragraphs', 'copybook_usage']
            logger.info("🔧 IMPLEMENTATION QUERY - searching code, copybooks, paragraphs")
        
        elif question_type == 'screen_usage':
            # NEW: Screen and field usage questions - prioritize help_fields
            indexes = list(self.ROUTE_PROFILES["ui"])  # screen_nodes, ui_paths, help_fields
        
        elif question_type == 'explain_program':
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
            indexes = list(self.ROUTE_PROFILES["data"]) + list(self.ROUTE_PROFILES["flow"])
        
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
    
    def _expand_state_keywords(self, query_lower: str) -> str:
        """Expand query with state-specific keywords for better retrieval.
        
        Args:
            query_lower: Lowercased query string
            
        Returns:
            Additional keywords to append to query, or empty string
        """
        # State name to code mapping
        state_mappings = {
            'south carolina': 'SC',
            'north carolina': 'NC',
            'georgia': 'GA',
            'florida': 'FL',
            'texas': 'TX',
            'california': 'CA',
            'kansas': 'KS',
            'washington': 'WA',
            'ohio': 'OH',
        }
        
        expansions = []
        
        # Check for state mentions
        for state_name, state_code in state_mappings.items():
            if state_name in query_lower or f' {state_code.lower()} ' in query_lower:
                # Add state-specific copybook/parameter patterns
                expansions.extend([
                    f'SP-{state_code}FRMLA',  # e.g., SP-SCFRMLA
                    f'{state_code}FRMLA',     # e.g., SCFRMLA
                    f'ORGST = {state_code}',  # Origination state
                    f'{state_code} SMALL LOANS',  # Common description pattern
                ])
                break  # Only expand for first matched state
        
        return ' '.join(expansions) if expansions else ''
    
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
