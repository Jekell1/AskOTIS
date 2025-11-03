"""Quick check of index document counts."""
from __future__ import annotations
import os, json, requests

API = '2025-08-01-preview'

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_settings()
ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

indexes = ['new_cobol_paragraphs', 'new_cobol_ui_paths', 'new_cobol_screen_nodes', 'new_cobol_copybook_usage', 'new_cobol_flow_edges_v2']

for idx in indexes:
    r = requests.get(f'{ep}/indexes/{idx}/docs/$count?api-version={API}', headers={'api-key': key}, timeout=30)
    print(f'{idx}: {r.text} documents')
