"""Helpers for inspecting Azure AI Search index schemas.

Provides:
  get_index_field_dimension(endpoint, key, index_name, field_name, api_version="2024-07-01") -> int | None

Used by ingestion scripts to dynamically adapt embedding vector size to the field's declared dimension.
"""
from __future__ import annotations
import requests
from typing import Optional

DEFAULT_API_VERSION = "2024-07-01"

def get_index_field_dimension(endpoint: str, key: str, index_name: str, field_name: str, api_version: str = DEFAULT_API_VERSION) -> Optional[int]:
    """Return the declared vector dimension for a field in an index, or None if not found.

    We look for either 'dimensions' (current schema) or legacy 'dimension'.
    Any non-2xx response returns None (caller may fallback to default embedding dim).
    """
    if not endpoint or not key:
        return None
    url = f"{endpoint.rstrip('/')}/indexes/{index_name}?api-version={api_version}"
    headers = {"api-key": key}
    try:
        r = requests.get(url, headers=headers, timeout=30)
        if r.status_code >= 300:
            return None
        data = r.json()
        for f in data.get("fields", []):
            if f.get("name") == field_name:
                # Azure Search currently uses 'dimensions' but be defensive
                return f.get("dimensions") or f.get("dimension")
    except Exception:
        return None
    return None

__all__ = ["get_index_field_dimension"]
