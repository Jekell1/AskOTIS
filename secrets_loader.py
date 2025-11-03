"""Central secrets loader for multi-source configuration.

Order of load (non-destructive; existing env vars win):
  1. local.settings.json
  2. local.settings.shared.json
  3. azure_search.json
  4. azure_openai.json

Alias normalization:
  SEARCH_ENDPOINT <-> AZURE_SEARCH_ENDPOINT
  SEARCH_KEY <-> AZURE_SEARCH_KEY
  AZURE_OPENAI_EMBED_DEPLOYMENT from any of: AZURE_OPENAI_EMBEDDING_DEPLOYMENT, AZURE_OPENAI_EMBED_MODEL
  OPENAI_EMBED_MODEL <-> OPENAI_EMBEDDING_MODEL

Set SHOW_SECRET_SOURCES=1 to print which files contributed (never prints values).
"""
from __future__ import annotations
import os, json, pathlib
from typing import Dict, Any

_FILES = [
    'local.settings.json',
    'local.settings.shared.json',
    'azure_search.json',
    'azure_openai.json'
]

def _read(path:str)->Dict[str,str]:
    p=pathlib.Path(path)
    if not p.is_file(): return {}
    try:
        data=json.loads(p.read_text(encoding='utf-8'))
    except Exception:
        return {}
    if isinstance(data,dict) and 'Values' in data and isinstance(data['Values'],dict):
        return {k:str(v) for k,v in data['Values'].items() if isinstance(v,(str,int,float))}
    return {k:str(v) for k,v in data.items() if isinstance(v,(str,int,float))}

def _alias():
    def cp(a,b):
        if a in os.environ and b not in os.environ:
            os.environ[b]=os.environ[a]
    cp('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT')
    cp('SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT')
    cp('AZURE_SEARCH_KEY','SEARCH_KEY')
    cp('SEARCH_KEY','AZURE_SEARCH_KEY')
    if 'AZURE_OPENAI_EMBED_DEPLOYMENT' not in os.environ:
        for c in ('AZURE_OPENAI_EMBEDDING_DEPLOYMENT','AZURE_OPENAI_EMBED_MODEL'):
            if c in os.environ:
                os.environ['AZURE_OPENAI_EMBED_DEPLOYMENT']=os.environ[c]; break
    if 'OPENAI_EMBED_MODEL' not in os.environ and 'OPENAI_EMBEDDING_MODEL' in os.environ:
        os.environ['OPENAI_EMBED_MODEL']=os.environ['OPENAI_EMBEDDING_MODEL']
    if 'OPENAI_EMBEDDING_MODEL' not in os.environ and 'OPENAI_EMBED_MODEL' in os.environ:
        os.environ['OPENAI_EMBEDDING_MODEL']=os.environ['OPENAI_EMBED_MODEL']

def load_secrets():
    loaded=[]
    for f in _FILES:
        vals=_read(f)
        if not vals: continue
        applied=False
        for k,v in vals.items():
            if k not in os.environ and v:
                os.environ[k]=v; applied=True
        if applied: loaded.append(f)
    _alias()
    if os.getenv('SHOW_SECRET_SOURCES'):
        print('[secrets_loader] loaded from:', ', '.join(loaded) if loaded else 'none')

__all__=['load_secrets']
