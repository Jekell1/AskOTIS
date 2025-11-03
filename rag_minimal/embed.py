import os, time, logging, math
from typing import List, Optional

import requests

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')

# Simple retry decorator for embedding calls
def _retry_embed(func):
    def wrapper(*args, **kwargs):
        delay = 1.0
        for attempt in range(6):
            try:
                return func(*args, **kwargs)
            except Exception as e:  # broad for brevity in minimal sample
                if attempt == 5:
                    raise
                logger.warning(f"Embed attempt {attempt+1} failed: {e}. Retrying in {delay:.1f}s")
                time.sleep(delay)
                delay = min(delay * 1.8, 30)
    return wrapper

class Embedder:
    """Minimal embedding utility supporting Azure OpenAI first, OpenAI public API fallback.

    Environment variables:
      Azure: AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT
      OpenAI: OPENAI_API_KEY, OPENAI_EMBED_MODEL
    """
    def __init__(self):
        self.azure_endpoint = os.getenv('AZURE_OPENAI_ENDPOINT')
        self.azure_key = os.getenv('AZURE_OPENAI_KEY')
        self.azure_deployment = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
        self.openai_key = os.getenv('OPENAI_API_KEY')
        self.openai_model = os.getenv('OPENAI_EMBED_MODEL', 'text-embedding-3-large')

        if not (self.azure_endpoint and self.azure_key and self.azure_deployment) and not self.openai_key:
            raise RuntimeError("No embedding provider configured. Set Azure or OpenAI env vars.")

    @_retry_embed
    def embed(self, texts: List[str]) -> List[List[float]]:
        # Trim whitespace & enforce non-empty
        clean = [t.strip() if t and t.strip() else "?" for t in texts]
        if self.azure_endpoint and self.azure_key and self.azure_deployment:
            return self._azure_embed(clean)
        return self._openai_embed(clean)

    def _azure_embed(self, texts: List[str]) -> List[List[float]]:
        url = f"{self.azure_endpoint}/openai/deployments/{self.azure_deployment}/embeddings?api-version=2024-02-15-preview"
        headers = {"api-key": self.azure_key, "Content-Type": "application/json"}
        resp = requests.post(url, json={"input": texts}, headers=headers, timeout=60)
        if resp.status_code != 200:
            raise RuntimeError(f"Azure embed error {resp.status_code}: {resp.text[:200]}")
        data = resp.json()
        return [d['embedding'] for d in data['data']]

    def _openai_embed(self, texts: List[str]) -> List[List[float]]:
        url = "https://api.openai.com/v1/embeddings"
        headers = {"Authorization": f"Bearer {self.openai_key}", "Content-Type": "application/json"}
        resp = requests.post(url, json={"input": texts, "model": self.openai_model}, headers=headers, timeout=60)
        if resp.status_code != 200:
            raise RuntimeError(f"OpenAI embed error {resp.status_code}: {resp.text[:200]}")
        data = resp.json()
        return [d['embedding'] for d in data['data']]
