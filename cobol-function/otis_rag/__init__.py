"""OTIS RAG - Simple, Elegant RAG for COBOL Source Code Analysis.

Example:
    >>> from otis_rag import OTISRAG
    >>> rag = OTISRAG()
    >>> answer = rag.ask("What does program GB01SE do?")
"""

__version__ = "1.0.0"

from .rag import OTISRAG
from .config import Config
from .router import QueryRouter
from .retriever import HybridRetriever
from .memory import ConversationMemory
from .generator import ResponseGenerator

__all__ = [
    'OTISRAG',
    'Config',
    'QueryRouter',
    'HybridRetriever',
    'ConversationMemory',
    'ResponseGenerator'
]
