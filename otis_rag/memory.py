"""Conversation Memory - Short-term memory for context continuity."""

import os
import logging
from typing import List, Dict, Optional
from datetime import datetime

logger = logging.getLogger(__name__)


class ConversationMemory:
    """Manages short-term conversation history for context with optional persistence."""
    
    def __init__(
        self, 
        max_turns: int = 10,
        backend: str = "memory",
        connection_string: Optional[str] = None,
        table_name: str = "OTISChatHistory"
    ):
        """Initialize conversation memory.
        
        Args:
            max_turns: Maximum number of conversation turns to remember
            backend: Storage backend ("memory", "table", "redis")
            connection_string: Azure Storage or Redis connection string
            table_name: Table name for Azure Table Storage backend
        """
        self.max_turns = max_turns
        self.backend = backend
        self.connection_string = connection_string or os.getenv("AZURE_STORAGE_CONNECTION_STRING")
        self.table_name = table_name
        self.history: List[Dict[str, any]] = []
        
        # Initialize backend if not using memory-only
        if self.backend == "table" and self.connection_string:
            self._init_table_storage()
        elif self.backend == "table" and not self.connection_string:
            logger.warning("Table backend requested but no connection string provided, falling back to memory")
            self.backend = "memory"
        elif self.backend == "table" and not self.connection_string:
            logger.warning("Table backend requested but no connection string provided, falling back to memory")
            self.backend = "memory"
    
    def _init_table_storage(self):
        """Initialize Azure Table Storage client and ensure table exists."""
        try:
            from azure.data.tables import TableServiceClient
            service_client = TableServiceClient.from_connection_string(self.connection_string)
            
            # Create table if it doesn't exist
            try:
                service_client.create_table_if_not_exists(self.table_name)
                logger.info(f"Table Storage initialized: {self.table_name}")
            except Exception as e:
                logger.error(f"Failed to create table: {e}")
                self.backend = "memory"
        except ImportError:
            logger.warning("azure-data-tables not installed, falling back to memory backend")
            self.backend = "memory"
        except Exception as e:
            logger.error(f"Failed to initialize Table Storage: {e}")
            self.backend = "memory"
    
    def _save_to_table(self, session_id: str):
        """Save conversation history to Azure Table Storage.
        
        Args:
            session_id: Unique session identifier
        """
        try:
            from azure.data.tables import TableServiceClient
            
            service_client = TableServiceClient.from_connection_string(self.connection_string)
            table_client = service_client.get_table_client(self.table_name)
            
            # Save only the most recent max_turns
            recent_history = self.history[-self.max_turns:]
            
            for i, turn in enumerate(recent_history):
                entity = {
                    "PartitionKey": session_id,
                    "RowKey": f"{i:04d}",  # Zero-padded for proper sorting
                    "timestamp": turn.get('timestamp', datetime.now().isoformat()),
                    "user": turn['user'],
                    "assistant": turn['assistant'],
                    "metadata_json": str(turn.get('metadata', {}))  # Store as string
                }
                
                table_client.upsert_entity(entity)
            
            logger.info(f"Saved {len(recent_history)} turns to Table Storage for session {session_id}")
            
        except Exception as e:
            logger.error(f"Failed to save to Table Storage: {e}")
    
    def _load_from_table(self, session_id: str):
        """Load conversation history from Azure Table Storage.
        
        Args:
            session_id: Unique session identifier
        """
        try:
            from azure.data.tables import TableServiceClient
            import ast
            
            service_client = TableServiceClient.from_connection_string(self.connection_string)
            table_client = service_client.get_table_client(self.table_name)
            
            # Query entities for this session, ordered by RowKey
            query_filter = f"PartitionKey eq '{session_id}'"
            entities = list(table_client.query_entities(query_filter))
            
            # Sort by RowKey to maintain order
            entities.sort(key=lambda x: x['RowKey'])
            
            # Reconstruct history
            self.history = []
            for entity in entities:
                turn = {
                    'timestamp': entity.get('timestamp', ''),
                    'user': entity.get('user', ''),
                    'assistant': entity.get('assistant', ''),
                    'metadata': {}
                }
                
                # Try to parse metadata
                metadata_json = entity.get('metadata_json', '{}')
                try:
                    turn['metadata'] = ast.literal_eval(metadata_json)
                except Exception:
                    turn['metadata'] = {}
                
                self.history.append(turn)
            
            logger.info(f"Loaded {len(self.history)} turns from Table Storage for session {session_id}")
            
        except Exception as e:
            logger.error(f"Failed to load from Table Storage: {e}")
            self.history = []
    
    def load_session(self, session_id: str):
        """Load conversation history for a session.
        
        Args:
            session_id: Unique session identifier
        """
        if self.backend == "table":
            self._load_from_table(session_id)
        else:
            logger.debug(f"Backend is {self.backend}, no persistent session to load")
    
    def add_turn(self, user_query: str, assistant_response: str, metadata: Dict = None, session_id: Optional[str] = None):
        """Add a conversation turn to memory.
        
        Args:
            user_query: User's question
            assistant_response: Assistant's response
            metadata: Optional metadata (retrieved docs, query type, etc.)
            session_id: Session ID for persistent backends
        """
        turn = {
            'timestamp': datetime.now().isoformat(),
            'user': user_query,
            'assistant': assistant_response,
            'metadata': metadata or {}
        }
        
        self.history.append(turn)
        
        # Keep only last N turns in memory
        if len(self.history) > self.max_turns:
            self.history = self.history[-self.max_turns:]
        
        # Persist to backend if configured
        if self.backend == "table" and session_id:
            self._save_to_table(session_id)
    
    def get_context(self, last_n: int = 3) -> str:
        """Get recent conversation context as formatted string.
        
        Args:
            last_n: Number of recent turns to include
        
        Returns:
            Formatted conversation history
        """
        if not self.history:
            return ""
        
        recent = self.history[-last_n:]
        
        context_parts = ["## Recent Conversation Context:"]
        for i, turn in enumerate(recent, 1):
            context_parts.append(f"\n**Turn {i}:**")
            context_parts.append(f"User: {turn['user']}")
            context_parts.append(f"Assistant: {turn['assistant'][:200]}...")  # Truncate long responses
        
        return "\n".join(context_parts)
    
    def get_last_query(self) -> str:
        """Get the last user query."""
        if not self.history:
            return ""
        return self.history[-1]['user']
    
    def get_last_metadata(self) -> Dict:
        """Get metadata from last turn."""
        if not self.history:
            return {}
        return self.history[-1].get('metadata', {})
    
    def clear(self):
        """Clear conversation history."""
        self.history = []
    
    def __len__(self):
        return len(self.history)
    
    def __repr__(self):
        return f"<ConversationMemory turns={len(self.history)} max={self.max_turns}>"
