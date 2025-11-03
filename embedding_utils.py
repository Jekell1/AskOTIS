"""Embedding utilities shared by ingestion scripts.

Providers (explicit selection via EMBEDDING_PROVIDER):
    - openai : Public OpenAI API (requires OPENAI_API_KEY, optional OPENAI_EMBED_MODEL)
    - azure  : Azure OpenAI Embeddings (requires AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY,
                         and AZURE_OPENAI_EMBED_DEPLOYMENT or AZURE_OPENAI_EMBED_MODEL)

Selection Logic:
    1. If EMBEDDING_PROVIDER is set to 'openai' or 'azure', honor it.
    2. Otherwise attempt OpenAI first; on failure (auth/config) fall back to Azure if its
         env vars are present.
    3. If neither succeeds, raise RuntimeError (no stub fallback – strict correctness).

Why Reintroduced Azure:
    The supplied credential is an Azure OpenAI key (not a public sk- key). To unblock
    ingestion while still preferring OpenAI when available, Azure support is restored.

Environment Variables:
    OpenAI:
        OPENAI_API_KEY (required)
        OPENAI_EMBED_MODEL (default: text-embedding-3-large)
    Azure OpenAI:
        AZURE_OPENAI_ENDPOINT
        AZURE_OPENAI_KEY
        AZURE_OPENAI_EMBED_DEPLOYMENT (preferred) OR AZURE_OPENAI_EMBED_MODEL
        AZURE_OPENAI_API_VERSION (optional; default 2024-12-01-preview)

Dimension Handling:
    batch_embed() pads/truncates to EMBED_DIM (default 1536 unless overridden by
    COBOL_EMBED_DIM or target_dim argument) so index dimensionality remains consistent.
"""
import os, json, time, math
from pathlib import Path
from typing import List, Iterable

# Attempt early load of local.settings.json so env vars (especially AZURE_OPENAI_*) are available
def _early_load_settings():
    try:
        if os.path.isfile('local.settings.json'):
            vals = json.load(open('local.settings.json','r')).get('Values', {})
            for k,v in vals.items():
                os.environ.setdefault(k, v)
    except Exception:
        pass
_early_load_settings()

EMBED_DIM = int(os.environ.get("COBOL_EMBED_DIM", "1536"))  # enforced final dimension (can be overridden per-call)

class _ProviderBase:
    def embed(self, texts: List[str]) -> List[List[float]]:
        raise NotImplementedError

class _OpenAIProvider(_ProviderBase):
    def __init__(self):
        # Native OpenAI embeddings (non-Azure) using openai package
        import openai  # type: ignore
        key = os.environ.get("OPENAI_API_KEY")
        model = (
            os.environ.get("OPENAI_EMBED_MODEL") or
            os.environ.get("OPENAI_EMBEDDING_MODEL") or
            # sensible default modern text-embedding-3-large
            "text-embedding-3-large"
        )
        if not key:
            raise RuntimeError("OPENAI_API_KEY missing for OpenAI provider")
        # Basic format sanity check to catch obvious copy/paste or truncation before network call
        if not key.startswith("sk-") and not key.startswith("3j"):
            # Some orgs rotate keys with non sk- prefix (rare); allow override with OPENAI_ALLOW_NON_SK
            if os.environ.get("OPENAI_ALLOW_NON_SK", "").lower() not in {"1","true","yes"}:
                raise RuntimeError("OPENAI_API_KEY does not appear to be a standard key (missing sk- prefix). Set OPENAI_ALLOW_NON_SK=1 to bypass if intentional.")
        self._client = openai.OpenAI(api_key=key) if hasattr(openai, 'OpenAI') else openai
        self._model = model
    def embed(self, texts: List[str]) -> List[List[float]]:
        # Batch request per OpenAI API; if new SDK, use .embeddings.create
        out: List[List[float]] = []
        for t in texts:
            try:
                resp = self._client.embeddings.create(model=self._model, input=t)
                emb = resp.data[0].embedding  # type: ignore
            except Exception as e:
                msg = str(e)
                # Detect invalid key and fallback to Azure if available
                if ("Incorrect API key provided" in msg or "invalid_api_key" in msg) and _azure_env_present():
                    try:
                        # late import to avoid circular
                        global _PROVIDER
                        az = _AzureOpenAIProvider()
                        _ERRORS.append("OpenAI key invalid; switched to Azure provider dynamically")
                        _PROVIDER = az
                        # delegate remaining (including current text) to azure provider
                        remaining = [t] + [x for x in texts if x not in out]  # simplistic; re-embed current
                        return _PROVIDER.embed(remaining)
                    except Exception as e2:
                        raise RuntimeError(f"OpenAI key invalid and Azure fallback failed: {e2}") from e
                raise RuntimeError(f"OpenAI embed failure: {e}")
            out.append(list(emb))
        return out

class _AzureOpenAIProvider(_ProviderBase):
    def __init__(self):
        endpoint = os.environ.get("AZURE_OPENAI_ENDPOINT")
        key = os.environ.get("AZURE_OPENAI_KEY")
        deployment = (
            os.environ.get("AZURE_OPENAI_EMBED_DEPLOYMENT") or
            os.environ.get("AZURE_OPENAI_EMBED_MODEL")
        )
        api_version = os.environ.get("AZURE_OPENAI_API_VERSION", "2024-12-01-preview")
        missing = [n for n,v in {
            "AZURE_OPENAI_ENDPOINT": endpoint,
            "AZURE_OPENAI_KEY": key,
            "AZURE_OPENAI_EMBED_DEPLOYMENT": deployment
        }.items() if not v]
        if missing:
            raise RuntimeError(f"Azure OpenAI configuration incomplete: {', '.join(missing)}")
        self._deployment = deployment
        self._api_version = api_version
        self._use_sdk = False
        # Try azure.openai SDK first
        try:  # pragma: no cover
            from azure.core.credentials import AzureKeyCredential  # type: ignore
            from azure.openai import AzureOpenAI  # type: ignore
            self._client = AzureOpenAI(
                api_version=api_version,
                endpoint=endpoint,
                credential=AzureKeyCredential(key)
            )
            self._use_sdk = True
        except Exception as e:  # fallback to langchain if available
            try:
                from langchain_openai import AzureOpenAIEmbeddings  # type: ignore
                self._client = AzureOpenAIEmbeddings(
                    azure_endpoint=endpoint,
                    api_key=key,
                    deployment=deployment,
                    openai_api_version=api_version
                )
            except Exception as e2:
                raise RuntimeError(f"Azure OpenAI provider init failed via both azure.openai and langchain: {e} | {e2}")

    def embed(self, texts: List[str]) -> List[List[float]]:
        if self._use_sdk:
            # azure.openai supports list input; request in a single call for the chunk
            try:
                resp = self._client.embeddings.create(model=self._deployment, input=list(texts))  # type: ignore
                return [list(d.embedding) for d in resp.data]  # type: ignore
            except Exception as e:
                raise RuntimeError(f"Azure OpenAI embed failure: {e}")
        # langchain fallback
        try:
            return self._client.embed_documents(list(texts))  # type: ignore
        except Exception as e:
            raise RuntimeError(f"Azure OpenAI (langchain) embed failure: {e}")

_ERRORS: List[str] = []
_PROVIDER: _ProviderBase

def _azure_env_present() -> bool:
    return all(os.environ.get(k) for k in ["AZURE_OPENAI_ENDPOINT","AZURE_OPENAI_KEY"]) and (
        os.environ.get("AZURE_OPENAI_EMBED_DEPLOYMENT") or os.environ.get("AZURE_OPENAI_EMBED_MODEL")
    )

def _init_provider():
    global _PROVIDER
    selection = os.environ.get("EMBEDDING_PROVIDER", "").lower()
    tried: List[str] = []

    def attempt(label: str, ctor):
        tried.append(label)
        try:
            return ctor()
        except Exception as e:
            _ERRORS.append(f"{label} init failed: {e}")
            return None

    if selection == "openai":
        prov = attempt("OpenAI", _OpenAIProvider)
        if prov is None: raise RuntimeError("Requested OpenAI provider failed; see errors: " + "; ".join(_ERRORS))
        _PROVIDER = prov; return
    if selection == "azure":
        prov = attempt("Azure", _AzureOpenAIProvider)
        if prov is None: raise RuntimeError("Requested Azure provider failed; see errors: " + "; ".join(_ERRORS))
        _PROVIDER = prov; return

    # Auto mode: try OpenAI then Azure
    prov = attempt("OpenAI", _OpenAIProvider)
    if prov is None and _azure_env_present():
        prov = attempt("Azure", _AzureOpenAIProvider)
    if prov is None:
        raise RuntimeError("No embedding provider could be initialized. Errors: " + "; ".join(_ERRORS))
    _PROVIDER = prov

_init_provider()

def embed_one(text: str, target_dim: int | None = None) -> List[float]:
    """Embed a single string (wrapper over batch_embed)."""
    return batch_embed([text], target_dim=target_dim)[0]

__all__ = ['batch_embed','embed_one','provider_info']

def batch_embed(
    texts: Iterable[str],
    batch_size: int = 64,
    pad_or_truncate: bool = True,
    target_dim: int | None = None
) -> List[List[float]]:
    """Embed a list of texts.

    Parameters:
      texts: iterable of strings
      batch_size: provider call batch size
      pad_or_truncate: if True adjust length to target_dim (or EMBED_DIM)
      target_dim: optional override of global EMBED_DIM (e.g. discovered from index field schema)

    Returns list of vectors sized target_dim (or provider native length if pad_or_truncate False).
    """
    texts = list(texts)
    dim = target_dim or EMBED_DIM
    out: List[List[float]] = []
    for i in range(0, len(texts), batch_size):
        chunk = texts[i:i+batch_size]
        vecs = _PROVIDER.embed(chunk)
        for v in vecs:
            if pad_or_truncate:
                if len(v) > dim:
                    v = v[:dim]
                elif len(v) < dim:
                    v = v + [0.0]*(dim-len(v))
            out.append(v)
        time.sleep(0.05)  # mild throttle
    return out

def provider_info() -> str:
    return _PROVIDER.__class__.__name__ + (" | notes: " + "; ".join(_ERRORS) if _ERRORS else "")

if __name__ == '__main__':
    sample = ["TEST one", "TEST two"]
    print("Provider:", provider_info())
    emb = batch_embed(sample)
    print("Default dim:", len(emb[0]) if emb else 0)
    emb2 = batch_embed(sample, target_dim=3072)
    print("Override dim (3072):", len(emb2[0]) if emb2 else 0)
