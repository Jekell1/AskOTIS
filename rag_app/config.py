import os
from pydantic import BaseModel
from functools import lru_cache

class Settings(BaseModel):
    azure_search_endpoint: str
    azure_search_index_program_meta: str = "new_cobol_program_meta"
    azure_search_index_paragraphs: str = "new_cobol_paragraphs"
    azure_search_index_data_items: str = "new_cobol_data_items"
    azure_search_api_version: str = os.getenv("AZURE_SEARCH_API_VERSION", "2025-08-01-preview")
    azure_openai_endpoint: str | None = None
    azure_openai_deployment: str | None = None
    openai_api_key: str | None = None
    embedding_model: str | None = os.getenv("AZURE_OPENAI_EMBEDDING_DEPLOYMENT") or os.getenv("OPENAI_EMBEDDING_MODEL")
    use_managed_identity: bool = False
    enable_vector: bool = os.getenv("ENABLE_VECTOR", "true").lower() == "true"
    enable_semantic: bool = os.getenv("ENABLE_SEMANTIC", "false").lower() == "true"
    rrf_k: int = int(os.getenv("RRF_K", "60"))
    max_context_chars: int = int(os.getenv("MAX_CONTEXT_CHARS", "18000"))
    max_chunks: int = int(os.getenv("MAX_CHUNKS", "12"))
    temperature: float = float(os.getenv("LLM_TEMPERATURE", "0.2"))
    enable_streaming: bool = os.getenv("ENABLE_STREAMING", "true").lower() == "true"
    cache_max_entries: int = int(os.getenv("CACHE_MAX_ENTRIES", "200"))
    rate_limit_qpm: int = int(os.getenv("RATE_LIMIT_QPM", "120"))
    pii_redact: bool = os.getenv("PII_REDACT", "true").lower() == "true"

    class Config:
        arbitrary_types_allowed = True

@lru_cache
def get_settings() -> Settings:
    return Settings(
        azure_search_endpoint=os.getenv("AZURE_SEARCH_ENDPOINT") or os.getenv("SEARCH_ENDPOINT", ""),
        azure_openai_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT"),
        azure_openai_deployment=os.getenv("AZURE_OPENAI_DEPLOYMENT"),
        openai_api_key=os.getenv("OPENAI_API_KEY") or os.getenv("AZURE_OPENAI_KEY"),
        use_managed_identity=os.getenv("USE_MANAGED_IDENTITY", "false").lower() == "true"
    )
