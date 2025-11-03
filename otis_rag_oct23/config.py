"""Configuration for OTIS RAG System."""

import os
import json
import logging
from pathlib import Path
from typing import Dict, Any

# Set up logging
logger = logging.getLogger(__name__)


class Config:
    """Central configuration for OTIS RAG system."""
    
    def __init__(self, settings_file: str = "local.settings.json"):
        """Initialize configuration from local.settings.json or environment variables"""
        self.settings = self._load_settings(settings_file)
        
        # Azure Search Configuration
        self.search_endpoint = self.settings.get("SEARCH_ENDPOINT") or self.settings.get("AZURE_SEARCH_ENDPOINT")
        self.search_key = self.settings.get("SEARCH_KEY") or self.settings.get("AZURE_SEARCH_KEY")
        
        # Azure OpenAI Configuration
        self.openai_endpoint = self.settings.get("AZURE_OPENAI_ENDPOINT")
        self.openai_key = self.settings.get("AZURE_OPENAI_KEY") or self.settings.get("OPENAI_API_KEY")
        self.chat_deployment = self.settings.get("AZURE_OPENAI_DEPLOYMENT", "gpt-4.1")
        self.embed_deployment = self.settings.get("AZURE_OPENAI_EMBED_DEPLOYMENT", "text-embedding-3-large")
        
        # Indexes to search (expanded to all 18 available indexes)
        self.indexes = {
            # Code chunks (2 indexes)
            'code': 'code-chunks',              # Legacy code chunks (167K docs, 3.8GB)
            'code_new': 'new_code_chunks',      # New code chunks (84K docs, 3.7GB)
            
            # Core metadata (5 indexes)
            'files': 'new-cobol-files',         # File metadata (10K docs)
            'programs': 'new_cobol_program_meta',  # Program metadata (9.7K docs)
            'paragraphs': 'new_cobol_paragraphs',  # Paragraph details (225K docs)
            'data_items': 'new_cobol_data_items',  # Data definitions (536K docs)
            'copybooks': 'new_cobol_copybook_meta', # Copybook metadata (7.8K docs)
            
            # Relationships & usage (5 indexes)
            'calls': 'new_cobol_calls',         # Program calls (16K docs)
            'variables': 'new_cobol_variable_usage',  # Variable usage (107K docs)
            'copybook_usage': 'new_cobol_copybook_usage',  # Copybook usage (115K docs)
            'symbol_refs': 'new_cobol_symbol_refs',  # Symbol references (1.9M docs)
            'name_aliases': 'new_cobol_name_aliases',  # Name aliases (56K docs)
            
            # Flow & navigation (5 indexes)
            'flows': 'new_cobol_program_flows',    # Program flows (9.7K docs)
            'flow_edges': 'new_cobol_flow_edges_v2',  # Flow edges (385K docs)
            'ui_paths': 'new_cobol_ui_paths',      # UI navigation paths (2K docs)
            'menu_trees': 'new_cobol_menu_trees',  # Menu structures (9.7K docs)
            'screen_nodes': 'new_cobol_screen_nodes',  # Screen nodes (1.6K docs)
            
            # Dependencies (2 indexes)
            'program_deps': 'new_cobol_program_deps',  # Program dependencies (9.7K docs)
            'enhanced_deps': 'enhanced_cobol_dependencies',  # Enhanced dependencies with all external references
            
            # Phase 2: Quick Wins (2 indexes)
            'transactions': 'transaction_taxonomy',  # Transaction codes and workflows (51 docs)
            'complexity': 'program_complexity'  # Program complexity metrics (1,736 docs)
        }

        # RAG Parameters
        self.max_results_per_index = 50  # Top 50 most relevant per index (was 10000 - too slow)
        self.max_context_length = 32000  # Increased for comprehensive context
        self.temperature = 0.1  # Low for factual responses        # Conversation Memory
        self.max_conversation_turns = 30  # Keep last 30 exchanges
        
        self._validate()
    
    def _load_settings(self, settings_file: str) -> Dict[str, Any]:
        """Load settings from JSON file or environment variables."""
        path = Path(settings_file)
        
        # Try to load from file first (for local development)
        if path.exists():
            with open(path, 'r') as f:
                data = json.load(f)
                # Handle nested structure like {"Values": {...}}
                if "Values" in data:
                    return data["Values"]
                return data
        
        # Fall back to environment variables (for Azure Functions)
        return dict(os.environ)

    def _validate(self):
        """Validate required configuration."""
        missing = []
        
        if not self.search_endpoint:
            missing.append("SEARCH_ENDPOINT")
        if not self.search_key:
            missing.append("SEARCH_KEY")
        if not self.openai_endpoint:
            missing.append("AZURE_OPENAI_ENDPOINT")
        if not self.openai_key:
            missing.append("AZURE_OPENAI_KEY")
        
        if missing:
            raise ValueError(f"Missing required configuration: {', '.join(missing)}")
    
    def get_index_name(self, index_type: str) -> str:
        """Get index name by type."""
        index_name = self.indexes.get(index_type, self.indexes['code'])
        logger.info(f"🗃️  Index mapping: {index_type} -> {index_name}")
        return index_name
    
    def __repr__(self):
        return f"<Config search={self.search_endpoint} openai={self.openai_endpoint}>"
