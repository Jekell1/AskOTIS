import os, requests, json, argparse, sys, pathlib
from secrets_loader import load_secrets

def load_from_config(path: str):
    p = pathlib.Path(path)
    if not p.is_file():
        return None, None
    try:
        data = json.loads(p.read_text())
    except Exception as e:
        print(f"Failed to parse config file {path}: {e}")
        return None, None
    ep = data.get('AZURE_SEARCH_ENDPOINT') or data.get('SEARCH_ENDPOINT')
    key = data.get('AZURE_SEARCH_KEY') or data.get('SEARCH_KEY')
    return ep, key

parser = argparse.ArgumentParser(description='Sample a few UI path docs to see available start_program_ids.')
parser.add_argument('--endpoint', help='Azure AI Search endpoint (e.g. https://your-svc.search.windows.net)')
parser.add_argument('--key', help='Admin or query API key for the search service')
parser.add_argument('--config', help='Optional JSON config file containing endpoint/key')
parser.add_argument('--top', type=int, default=25, help='Number of docs to sample (default 25)')
parser.add_argument('--index', default='new_cobol_ui_paths', help='Index name (default new_cobol_ui_paths)')
args = parser.parse_args()

load_secrets()
ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

# If still missing, try config file (explicit) then common defaults
if (not ep or not key) and args.config:
    cfg_ep, cfg_key = load_from_config(args.config)
    ep = ep or cfg_ep
    key = key or cfg_key
if not ep or not key:
    # Try a few conventional config filenames
    for candidate in ['azure_search.json', 'azure_settings.json', 'config.search.json']:
        if ep and key: break
        cfg_ep, cfg_key = load_from_config(candidate)
        ep = ep or cfg_ep
        key = key or cfg_key

if not ep or not key:
    print('Missing endpoint/key after multi-source load (local.settings.* / azure_search.json / env). Provide --endpoint/--key.')
    sys.exit(1)

api_version = '2025-08-01-preview'
url = f"{ep.rstrip('/')}/indexes/{args.index}/docs/search?api-version={api_version}"
body = {
    'search': '*',
    'top': args.top,
    'select': 'path_id,start_program_id,end_program_id,hop_count,guard_summary'
}

try:
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body, timeout=30)
except Exception as e:
    print('Request failed:', e)
    sys.exit(2)

print('Status:', r.status_code)
if r.status_code == 200:
    try:
        data = r.json().get('value', [])
    except Exception as e:
        print('Failed to parse JSON:', e)
        print(r.text[:500])
        sys.exit(3)
    roots = sorted({d.get('start_program_id') for d in data if d.get('start_program_id')})
    print(f'Sampled {len(data)} docs. Unique start_program_ids (sample set):', roots)
    print('-'*80)
    for d in data[:5]:
        print(json.dumps(d, indent=2)[:600])
        print('-'*80)
else:
    print(r.text[:1000])
    if r.status_code == 404:
        print('Index not found. Confirm the index name and that it exists.')
