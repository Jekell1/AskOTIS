"""Shared environment variable autoload for Azure Search / OpenAI credentials.

Order of resolution (first non-empty wins for each canonical var):
  Existing process env
  local.settings.json (Functions style: {"Values": { ... }})

Aliases unified:
  SEARCH_ENDPOINT -> AZURE_SEARCH_ENDPOINT
  SEARCH_KEY -> AZURE_SEARCH_KEY

Optional Azure OpenAI embedding support (if you configure):
  AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT

Usage:
  import env_autoload; env_autoload.ensure()
"""
from __future__ import annotations
import os, json
from typing import Iterable

LOCAL_SETTINGS_FILE = 'local.settings.json'
VALUES_KEY = 'Values'
SEARCH_KEYS = [
    'AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY',
    'SEARCH_ENDPOINT','SEARCH_KEY'
]
AOAI_KEYS = [
    'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'
]

ALL_KEYS = set(SEARCH_KEYS + AOAI_KEYS)

_loaded = False


def _load_local_settings(path: str) -> dict:
    try:
        with open(path,'r',encoding='utf-8') as f:
            data = json.load(f)
        return data.get(VALUES_KEY, {}) or {}
    except Exception:
        return {}


def _alias_promote(env: dict):
    # Promote SEARCH_* to AZURE_SEARCH_* if canonical missing
    if 'AZURE_SEARCH_ENDPOINT' not in env and 'SEARCH_ENDPOINT' in env:
        env['AZURE_SEARCH_ENDPOINT'] = env['SEARCH_ENDPOINT']
    if 'AZURE_SEARCH_KEY' not in env and 'SEARCH_KEY' in env:
        env['AZURE_SEARCH_KEY'] = env['SEARCH_KEY']


def ensure(required: Iterable[str] | None = None) -> None:
    global _loaded
    if _loaded and not required:
        return
    needed = set(required) if required else ALL_KEYS
    missing = [k for k in needed if not os.getenv(k)]
    if not missing:
        _alias_promote(os.environ)
        _loaded = True
        return
    # Attempt load
    vals = _load_local_settings(LOCAL_SETTINGS_FILE)
    injected = 0
    for k,v in vals.items():
        if k in ALL_KEYS and k not in os.environ and isinstance(v,str) and v.strip():
            os.environ[k] = v
            injected += 1
    _alias_promote(os.environ)
    if injected:
        print(f"env_autoload: injected {injected} value(s) from {LOCAL_SETTINGS_FILE}")
    _loaded = True


def summarize(mask: bool = True) -> dict:
    ensure()
    out = {}
    for k in sorted(ALL_KEYS):
        val = os.getenv(k)
        if val is None:
            out[k] = None
        else:
            out[k] = (val[:4] + '...' + val[-4:] if (mask and len(val) > 12) else val if not mask else '***')
    return out

if __name__ == '__main__':
    ensure()
    print('Credential presence (masked):')
    for k,v in summarize().items():
        print(f"  {k}: {v}")
