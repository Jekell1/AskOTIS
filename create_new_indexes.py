"""Create or update the *New suffixed Azure AI Search indexes.

Reads schema definitions from ``schemas_canonical_new.ALL_NEW_SCHEMAS``
and issues PUT calls. Safe to re-run (idempotent) - will overwrite existing
index definitions (non-destructive for docs if schema-compatible but may
fail if incompatible; in that case delete manually or version again).

Credential Loading Order:
    1. Existing environment variables (AZURE_SEARCH_ENDPOINT / KEY or SEARCH_ aliases)
    2. Auto-load from ``local.settings.json`` (Functions-style) if present and needed

If after autoload the required values are still missing, the script exits
with a clear message instead of attempting invalid URLs.
"""
from __future__ import annotations
import os, sys, json, time, argparse
import requests
from typing import Dict, Any
from schemas_canonical_new import ALL_NEW_SCHEMAS

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')  # upgraded to latest preview supporting vectors
LOCAL_SETTINGS_FILE = 'local.settings.json'
_AUTOLOAD_KEYS = {
    'AZURE_SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_ENDPOINT', 'SEARCH_KEY'
}

def _autoload_local_settings(path: str = LOCAL_SETTINGS_FILE) -> int:
    """Load credentials from local.settings.json (Functions style) if present.

    Returns number of env vars injected.
    """
    p = os.path.abspath(path)
    if not os.path.exists(p):
        return 0
    try:
        data = json.loads(open(p, 'r', encoding='utf-8').read())
        values = data.get('Values', {}) or {}
    except Exception:
        return 0
    changed = 0
    for k, v in values.items():
        if k in _AUTOLOAD_KEYS and k not in os.environ and isinstance(v, str) and v.strip():
            os.environ[k] = v
            changed += 1
    # Alias mapping SEARCH_* -> AZURE_SEARCH_* if only former present
    if 'AZURE_SEARCH_ENDPOINT' not in os.environ and 'SEARCH_ENDPOINT' in os.environ:
        os.environ['AZURE_SEARCH_ENDPOINT'] = os.environ['SEARCH_ENDPOINT']; changed += 1
    if 'AZURE_SEARCH_KEY' not in os.environ and 'SEARCH_KEY' in os.environ:
        os.environ['AZURE_SEARCH_KEY'] = os.environ['SEARCH_KEY']; changed += 1
    if changed:
        print(f"Autoloaded {changed} credential(s) from {path}")
    return changed

# Attempt autoload only if something missing
if not (os.getenv('AZURE_SEARCH_ENDPOINT') and os.getenv('AZURE_SEARCH_KEY')):
    _autoload_local_settings()

ENDPOINT = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
KEY = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

if not ENDPOINT or not KEY:
    print('ERROR: Missing AZURE_SEARCH_ENDPOINT/KEY after autoload. Create local.settings.json with {"Values": {"AZURE_SEARCH_ENDPOINT": "https://<service>.search.windows.net", "AZURE_SEARCH_KEY": "<admin-or-query-key>"}} or export env vars.', file=sys.stderr)
    sys.exit(1)

HEADERS = {"Content-Type": "application/json", "api-key": KEY}


def put_index(schema: Dict[str, Any]) -> tuple[int, str]:
    name = schema['name']
    url = f"{ENDPOINT}/indexes/{name}?api-version={API_VERSION}"
    # For create/update semantics, Azure Search uses PUT of full definition
    body = json.dumps(schema, ensure_ascii=False)
    try:
        r = requests.put(url, headers=HEADERS, data=body.encode('utf-8'), timeout=60)
        return r.status_code, r.text[:400]
    except Exception as ex:
        return -1, str(ex)


def create_or_update_all(target: str | None = None):
    start = time.time()
    results = []
    schemas = ALL_NEW_SCHEMAS
    if target:
        target = target.strip().lower()
        schemas = [s for s in ALL_NEW_SCHEMAS if s['name'].lower() == target]
        if not schemas:
            print(f"No schema matches name '{target}'. Available: {[s['name'] for s in ALL_NEW_SCHEMAS]}")
            return
    for schema in schemas:
        code, txt = put_index(schema)
        results.append((schema['name'], code, txt))
    print('Create/Update Results:')
    for name, code, txt in results:
        status = 'OK' if code in (200,201) else f'ERR({code})'
        print(f" - {name}: {status}")
        if code not in (200,201):
            print(f"   Response: {txt}")
    print(f"Done in {time.time()-start:.2f}s")

if __name__ == '__main__':
    ap = argparse.ArgumentParser(description='Create or update new-* Azure AI Search indexes')
    ap.add_argument('--index', help='Optional single index name (e.g. new-cobol-files)')
    args = ap.parse_args()
    create_or_update_all(args.index)
