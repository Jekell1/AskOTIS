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
        self.chat_deployment = self.settings.get("AZURE_OPENAI_DEPLOYMENT", "gpt-4o")
        
        # Embedding model configuration - support both large (3072-dim) and small (1536-dim)
        self.embed_deployment_large = self.settings.get("AZURE_OPENAI_EMBED_DEPLOYMENT_LARGE", "text-embedding-3-large")
        self.embed_deployment_small = self.settings.get("AZURE_OPENAI_EMBED_DEPLOYMENT_SMALL", "text-embedding-3-small")
        
        # Legacy embed_deployment for backward compatibility (defaults to large model)
        self.embed_deployment = self.settings.get("AZURE_OPENAI_EMBED_DEPLOYMENT", self.embed_deployment_large)
        
        # Azure Storage Configuration (for conversation persistence)
        self.storage_connection_string = self.settings.get("AZURE_STORAGE_CONNECTION_STRING") or self.settings.get("AzureWebJobsStorage")
        
        # Indexes to search - 13 active indexes (9 redundant indexes removed for optimization)
        self.indexes = {
            # Code chunks (1 index)
            'code': 'new_code_chunks',          # Primary code chunks (152K docs, 100% coverage)
            
            # Core metadata (5 indexes)
            'files': 'new-cobol-files',         # File metadata with summaries (10K docs)
            'programs': 'new_cobol_program_meta',  # Program metadata (9.7K docs, 100% embedded)
            'paragraphs': 'new_cobol_paragraphs',  # Paragraph details (225K docs)
            'data_items': 'new_cobol_data_items',  # Data definitions (146K docs)
            'copybooks': 'new_cobol_copybook_meta', # Copybook metadata with fields (7.8K docs)
            
            # Relationships & usage (2 indexes)
            'variables': 'new_cobol_variable_usage',  # Variable usage (107K docs)
            'copybook_usage': 'new_cobol_copybook_usage',  # Copybook usage with context (115K docs)
            
            # Flow & navigation (3 indexes)
            'flows': 'new_cobol_program_flows',    # Program flows (9.7K docs)
            'flow_edges': 'new_cobol_flow_edges_v2',  # Flow edges: CALL, PERFORM, GOTO (385K docs)
            'ui_paths': 'new_cobol_ui_paths',      # UI navigation paths (2K docs)
            
            # Screen & UI (2 indexes)
            'screen_nodes': 'new_cobol_screen_nodes',  # Screen definitions (2.3K docs)
            'help_fields': 'help_fields',         # Field-level help documentation (26K docs)
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
