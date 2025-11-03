"""Main RAG Orchestrator - Coordinates all components."""

import logging
from typing import Dict, Any
from .config import Config
from .router import QueryRouter
from .retriever import HybridRetriever
from .memory import ConversationMemory
from .generator import ResponseGenerator

logger = logging.getLogger(__name__)


class OTISRAG:
    """Main RAG system orchestrating all components.
    
    Simple, elegant interface:
        rag = OTISRAG()
        answer = rag.ask("What does program GB01SE do?")
    """
    
    def __init__(self, config_file: str = "local.settings.json", enable_persistence: bool = False, session_id: str = None):
        """Initialize RAG system.
        
        Args:
            config_file: Path to configuration file
            enable_persistence: Enable conversation persistence to Azure Table Storage
            session_id: Session ID for loading existing conversation history
        """
        # Initialize components
        self.config = Config(config_file)
        self.router = QueryRouter()
        self.retriever = HybridRetriever(self.config)
        
        # Initialize memory with optional persistence
        backend = "table" if enable_persistence else "memory"
        self.memory = ConversationMemory(
            max_turns=self.config.max_conversation_turns,
            backend=backend,
            connection_string=self.config.storage_connection_string if hasattr(self.config, 'storage_connection_string') else None
        )
        
        # Load existing session if provided
        if session_id and enable_persistence:
            self.memory.load_session(session_id)
        
        self.generator = ResponseGenerator(self.config)
        self.last_timing = {}  # Store timing from last query
        self.session_id = session_id  # Store for subsequent operations
    
    def ask(self, query: str, verbose: bool = False) -> str:
        """Ask a question and get an answer.
        
        Args:
            query: User's question
            verbose: If True, print routing and retrieval info
        
        Returns:
            Generated answer
        """
        result = self.ask_with_timing(query, verbose)
        return result['answer']
    
    def ask_with_timing(self, query: str, verbose: bool = False) -> dict:
        """Ask a question and get an answer with timing breakdown.
        
        Args:
            query: User's question
            verbose: If True, print routing and retrieval info
        
        Returns:
            Dict with 'answer' and 'timing' keys
        """
        import time
        start_total = time.time()
        
        # 1. Route the query
        start_route = time.time()
        routing = self.router.route(query)
        time_route = time.time() - start_route
        logger.info(f"⏱️ TIMING: Routing took {time_route:.3f}s")
        print(f"⏱️ TIMING: Routing took {time_route:.3f}s")
        
        if verbose:
            print(f"Route: is_otis={routing['is_otis']}, type={routing['question_type']}")
            if routing.get('has_menu_intent'):
                print(f"Menu/UI intent detected - boosting screen_nodes")
            print(f"Searching indexes: {routing['search_indexes']}")
        
        # 2. Retrieve relevant context with index weights
        # Dynamically adjust retrieval limits based on question type
        question_type = routing.get('question_type', 'general')
        
        # 🔹 Check if router provided max_results override (e.g., for copybook queries)
        if 'max_results' in routing:
            max_results_for_query = routing['max_results']
            logger.info(f"📄 Using router-specified max_results={max_results_for_query}")
            print(f"📄 Using router-specified max_results={max_results_for_query}")
        # Determine max_results based on question complexity
        elif question_type in ('menu', 'list', 'simple'):
            # Specific questions need fewer documents
            max_results_for_query = 20  # Top 20 per index
        elif question_type == 'trace_flow':
            # Flow tracing needs comprehensive context
            max_results_for_query = 100  # Top 100 per index
        elif question_type == 'transaction':
            # Business transactions need moderate context
            max_results_for_query = 50  # Top 50 per index
        elif 'all' in routing['clean_query'].lower() or 'list' in routing['clean_query'].lower():
            # Explicit list requests need many results
            max_results_for_query = 200  # Top 200 per index
        else:
            # General questions - balanced approach
            max_results_for_query = 50  # Default: Top 50 per index
        
        logger.info(f"📊 Dynamic retrieval: question_type={question_type}, max_results={max_results_for_query}")
        print(f"📊 Dynamic retrieval: question_type={question_type}, max_results={max_results_for_query}")
        
        start_retrieve = time.time()
        context_docs = self.retriever.retrieve(
            query=routing['clean_query'],
            indexes=routing['search_indexes'],
            max_results=max_results_for_query,
            index_weights=routing.get('index_weights', {}),  # Pass weights from router
            question_type=routing.get('question_type')  # Pass question_type for deterministic handling
        )
        time_retrieve = time.time() - start_retrieve
        logger.info(f"⏱️ TIMING: Retrieval took {time_retrieve:.3f}s")
        print(f"⏱️ TIMING: Retrieval took {time_retrieve:.3f}s")
        
        if verbose:
            print(f"Retrieved {len(context_docs)} documents")
        
        # 3. Get conversation context
        start_memory = time.time()
        conv_context = self.memory.get_context(last_n=3)
        time_memory = time.time() - start_memory
        logger.info(f"⏱️ TIMING: Memory context took {time_memory:.3f}s")
        print(f"⏱️ TIMING: Memory context took {time_memory:.3f}s")
        
        # 4. Generate response
        start_generate = time.time()
        response = self.generator.generate(
            query=query,
            context_docs=context_docs,
            conversation_context=conv_context,
            is_otis=routing['is_otis'],
            question_type=routing['question_type']
        )
        time_generate = time.time() - start_generate
        logger.info(f"⏱️ TIMING: Generation took {time_generate:.3f}s")
        print(f"⏱️ TIMING: Generation took {time_generate:.3f}s")
        
        # 5. Store in memory (with session_id if available)
        self.memory.add_turn(
            user_query=query,
            assistant_response=response,
            metadata={
                'routing': routing,
                'num_docs': len(context_docs)
            },
            session_id=self.session_id
        )
        
        time_total = time.time() - start_total
        timing_summary = f"Total={time_total:.3f}s (Route={time_route:.3f}s, Retrieve={time_retrieve:.3f}s, Memory={time_memory:.3f}s, Generate={time_generate:.3f}s)"
        logger.info(f"⏱️ TIMING SUMMARY: {timing_summary}")
        print(f"⏱️ TIMING SUMMARY: {timing_summary}")
        
        # Store timing for retrieval
        timing_dict = {
            'total_seconds': round(time_total, 3),
            'route_seconds': round(time_route, 3),
            'retrieve_seconds': round(time_retrieve, 3),
            'memory_seconds': round(time_memory, 3),
            'generate_seconds': round(time_generate, 3),
            'question_type': routing.get('question_type', 'unknown'),
            'num_documents': len(context_docs)
        }
        self.last_timing = timing_dict
        
        return {
            'answer': response,
            'timing': timing_dict
        }
    
    def clear_memory(self):
        """Clear conversation history."""
        self.memory.clear()
    
    def get_stats(self) -> Dict[str, Any]:
        """Get system statistics."""
        return {
            'total_queries': len(self.memory.history),
            'conversation_turns': len(self.memory),
            'max_turns': self.config.max_conversation_turns,
            'indexes_available': len(self.config.indexes),
            'last_query': self.memory.get_last_query()
        }
    
    def __repr__(self):
        return f"<OTISRAG turns={len(self.memory)} indexes={len(self.config.indexes)}>"
