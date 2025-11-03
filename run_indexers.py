import os, json, time, sys, requests
from typing import List
API_VERSION = "2024-07-01"
INDEXERS = [
    "idx-chunks","idx-symbols","idx-xrefs","idx-calls","idx-files",
    "idx-paragraphs","idx-facts","idx-flow-edges","idx-copybooks"
]

def load_settings():
    path = os.path.join(os.getcwd(), "local.settings.json")
    vals = {}
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            try:
                data = json.load(f)
                vals.update(data.get('Values', {}))
            except Exception:
                pass
    return vals

def get_endpoint_and_key():
    vals = load_settings()
    endpoint = (os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT") or
                vals.get("AZURE_SEARCH_ENDPOINT") or vals.get("SEARCH_ENDPOINT"))
    key = (os.environ.get("AZURE_SEARCH_KEY") or os.environ.get("SEARCH_KEY") or
           vals.get("AZURE_SEARCH_KEY") or vals.get("SEARCH_KEY"))
    if not endpoint or not key:
        print("Missing endpoint or key.")
        sys.exit(1)
    return endpoint.rstrip('/'), key

def run_indexer(endpoint: str, key: str, name: str):
    url = f"{endpoint}/indexers/{name}/run?api-version={API_VERSION}"
    r = requests.post(url, headers={"api-key": key})
    if r.status_code in (202,204):
        print(f"Triggered {name}")
    else:
        print(f"Failed {name}: {r.status_code} {r.text[:200]}")

def get_status(endpoint: str, key: str, name: str):
    url = f"{endpoint}/indexers/{name}/status?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code==200:
        data = r.json()
        last = data.get('lastResult') or {}
        status = last.get('status')
        err = last.get('errorMessage')
        return status, err
    return None, f"HTTP {r.status_code}"

def main():
    endpoint, key = get_endpoint_and_key()
    # If specific indexer names passed on CLI, use only those.
    targets = sys.argv[1:] or INDEXERS
    print(f"Using {endpoint}")
    print("Target indexers:", ", ".join(targets))
    for name in targets:
        run_indexer(endpoint, key, name)
    print("Sleeping 30s...")
    time.sleep(30)
    print("Status summary:")
    for name in targets:
        status, err = get_status(endpoint, key, name)
        print(f" {name}: {status or 'unknown'}{' | '+err if err else ''}")

if __name__ == "__main__":
    main()
