"""Minimal Azure OpenAI embedding smoke test.

Loads credentials/endpoint from environment (populated via local.settings.json
at runtime by embedding_utils early loader, or set manually).

Env vars required:
  AZURE_OPENAI_ENDPOINT
  AZURE_OPENAI_KEY
  AZURE_OPENAI_EMBED_DEPLOYMENT  (or AZURE_OPENAI_EMBED_MODEL if using older naming)
Optional:
  AZURE_OPENAI_API_VERSION (default 2024-12-01-preview)

Usage (PowerShell):
  $env:AZURE_OPENAI_ENDPOINT="https://<your-endpoint>.openai.azure.com"
  $env:AZURE_OPENAI_KEY="<key>"
  $env:AZURE_OPENAI_EMBED_DEPLOYMENT="text-embedding-3-large"
  python test-embed.py
"""

import os, sys
from typing import List

try:
    from azure.core.credentials import AzureKeyCredential  # type: ignore
except Exception:
    print("ERROR: azure-core not installed. Install with: pip install azure-core", file=sys.stderr)
    raise

try:
    # The unified OpenAI SDK (>=1.0) supplies AzureOpenAI class
    from openai import AzureOpenAI  # type: ignore
except Exception:
    print("ERROR: openai SDK (>=1.0) not installed. Install with: pip install openai --upgrade", file=sys.stderr)
    raise

endpoint = os.environ.get("AZURE_OPENAI_ENDPOINT")
key = os.environ.get("AZURE_OPENAI_KEY")
deployment = (
    os.environ.get("AZURE_OPENAI_EMBED_DEPLOYMENT") or
    os.environ.get("AZURE_OPENAI_EMBED_MODEL") or
    "text-embedding-3-large"
)
api_version = os.environ.get("AZURE_OPENAI_API_VERSION", "2024-12-01-preview")

missing = [name for name,val in {
    "AZURE_OPENAI_ENDPOINT": endpoint,
    "AZURE_OPENAI_KEY": key,
    "AZURE_OPENAI_EMBED_DEPLOYMENT": deployment
}.items() if not val]

if missing:
    # Attempt to load from local.settings.json (Functions style) if present
    ls_path = os.path.join(os.getcwd(), 'local.settings.json')
    if os.path.exists(ls_path):
        try:
            import json
            with open(ls_path,'r',encoding='utf-8') as f:
                data = json.load(f)
            values = data.get('Values', {}) if isinstance(data, dict) else {}
            endpoint = endpoint or values.get('AZURE_OPENAI_ENDPOINT')
            key = key or values.get('AZURE_OPENAI_KEY')
            deployment = deployment or values.get('AZURE_OPENAI_EMBED_DEPLOYMENT') or values.get('AZURE_OPENAI_EMBED_MODEL')
            missing = [name for name,val in {
                "AZURE_OPENAI_ENDPOINT": endpoint,
                "AZURE_OPENAI_KEY": key,
                "AZURE_OPENAI_EMBED_DEPLOYMENT": deployment
            }.items() if not val]
        except Exception as e:
            print(f"Warning: failed loading local.settings.json: {e}", file=sys.stderr)

if missing:
    print("Missing required environment variables after fallback: " + ", ".join(missing), file=sys.stderr)
    sys.exit(1)

client = AzureOpenAI(
    api_version=api_version,
    endpoint=endpoint,
    credential=AzureKeyCredential(key)
)

texts: List[str] = ["first phrase","second phrase","third phrase"]
try:
    response = client.embeddings.create(input=texts, model=deployment)
except Exception as e:
    print(f"Embedding request failed: {e}", file=sys.stderr)
    raise

for item in response.data:
    emb = item.embedding  # type: ignore
    length = len(emb)
    preview = f"[{emb[0]:.4f}, {emb[1]:.4f}, ..., {emb[-2]:.4f}, {emb[-1]:.4f}]"
    print(f"data[{item.index}] dim={length} sample={preview}")

usage = getattr(response, 'usage', None)
if usage:
    print("usage:", usage)
else:
    print("(No usage field returned)")