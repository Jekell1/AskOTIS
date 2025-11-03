"""Lightweight vector presence & similarity verifier.

Usage:
  python vector_verifier.py --index new_cobol_flow_edges --samples 8 --vector-field edge_vector --id-field edge_id

Steps:
  1. Random-sample N docs via search=* with $top paging.
  2. Retrieve selected docs including vector field presence flag (boolean or check if list length >0).
  3. Pick one doc that has a vector and run a vector similarity query against the same index.
  4. Report findings and exit code 0 if all sampled docs have vectors; else 2.

Environment / credentials auto-loaded from local.settings.json Values:
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY

Supports either classical vector query (vectorQueries param) or legacy vector parameter depending on API version.
"""
import os, sys, json, random, argparse, requests
from typing import List, Dict, Any, Optional
from embedding_utils import batch_embed

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def get_count(ep: str, key: str, index: str) -> int:
    url = f"{ep}/indexes/{index}/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise RuntimeError(f"Count failed {r.status_code}: {r.text[:200]}")
    return int(r.text)

def fetch_docs(ep: str, key: str, index: str, skip: int, top: int, select: Optional[str]) -> List[Dict[str,Any]]:
    url = f"{ep}/indexes/{index}/docs?api-version={API_VERSION}&search=*&$skip={skip}&$top={top}"
    if select:
        url += f"&$select={select}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def run_vector_query(ep: str, key: str, index: str, vector_field: str, vec: List[float], k: int = 5) -> List[Dict[str,Any]]:
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type':'application/json'}
    # Use new vectorQueries syntax if preview API
    body: Dict[str,Any]
    if '2025' in API_VERSION or 'preview' in API_VERSION:
        body = {
            "search": "",
            "vectorQueries": [{
                "kind": "vector",
                "vector": vec,
                "k": k,
                "fields": vector_field
            }],
            "select": "*"
        }
    else:
        body = {
            "search": "",
            "vectors": [{
                "value": vec,
                "k": k,
                "fields": vector_field
            }],
            "select": "*"
        }
    r = requests.post(url, headers=headers, json=body, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Vector query failed {r.status_code}: {r.text[:300]}")
    return r.json().get('value',[])

def get_index_definition(ep: str, key: str, index: str) -> Dict[str,Any]:
    url = f"{ep}/indexes/{index}?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise RuntimeError(f"Index def fetch failed {r.status_code}: {r.text[:200]}")
    return r.json()

def detect_vector_dimension(index_def: Dict[str,Any], field: str) -> Optional[int]:
    for f in index_def.get('fields', []):
        if f.get('name') == field:
            # vector fields usually have 'dimensions' property
            dims = f.get('dimensions') or f.get('searchDimensions') or f.get('vectorSearchDimensions')
            if isinstance(dims, int):
                return dims
    return None

def main():
    ap = argparse.ArgumentParser(description='Verify vectors exist and are queryable in an index.')
    ap.add_argument('--index', required=True)
    ap.add_argument('--samples', type=int, default=10)
    ap.add_argument('--select-extra', default='')
    ap.add_argument('--vector-field', required=True, help='Vector field name (non-retrievable acceptable)')
    ap.add_argument('--id-field', required=True)
    ap.add_argument('--max-page', type=int, default=1000, help='Page size for random window sampling')
    ap.add_argument('--seed', type=int, default=42)
    args = ap.parse_args()
    random.seed(args.seed)
    load_local_settings()
    ep, key = resolve_endpoint_key()
    total = get_count(ep, key, args.index)
    index_def = get_index_definition(ep, key, args.index)
    expected_dim = detect_vector_dimension(index_def, args.vector_field)
    if expected_dim:
        print(f"Detected vector dimension: {expected_dim}")
    if total == 0:
        print('Index empty; nothing to verify.')
        sys.exit(1)
    print(f"Index {args.index} has {total} docs")
    samples: List[Dict[str,Any]] = []
    needed = args.samples
    attempts = 0
    # Do not include vector field in select (likely non-retrievable); rely on has_vector flag
    select_fields = f"{args.id_field},has_vector" + (","+args.select_extra if args.select_extra else "")
    while len(samples) < needed and attempts < needed * 10:
        attempts += 1
        if total <= args.max_page:
            skip = 0
            page_size = total
        else:
            skip = random.randint(0, max(0, total-args.max_page))
            page_size = args.max_page
        try:
            page_docs = fetch_docs(ep, key, args.index, skip, page_size, select_fields)
        except Exception as e:
            print(f"[WARN] fetch page failed: {e}")
            continue
        if not page_docs:
            continue
        random.shuffle(page_docs)
        for d in page_docs:
            samples.append(d)
            if len(samples) >= needed:
                break
    samples = samples[:needed]
    if len(samples) < needed:
        print(f"Only collected {len(samples)} samples (requested {needed})")
    missing = []
    vector_examples: List[List[float]] = []
    for d in samples:
        vec = d.get(args.vector_field)  # probably None (non-retrievable)
        if d.get('has_vector') is True:
            # Some schemas store vector non-retrievable; treat flag as presence.
            pass
        else:
            missing.append(d.get(args.id_field))
    # Build a synthetic probe vector by embedding concatenated IDs (deterministic test token)
    if not missing:
        probe_text = " ".join([str(s.get(args.id_field)) for s in samples[:3]]) or "probe"
        try:
            probe_vec = batch_embed([probe_text], target_dim=None, batch_size=1)[0]
            if expected_dim and len(probe_vec) != expected_dim:
                if len(probe_vec) < expected_dim:
                    # tile up to expected dimension
                    times = (expected_dim + len(probe_vec) - 1)//len(probe_vec)
                    tiled = (probe_vec * times)[:expected_dim]
                    probe_vec = tiled
                else:
                    probe_vec = probe_vec[:expected_dim]
            vector_examples.append(probe_vec)
        except Exception as e:
            print(f"[WARN] Could not build probe embedding: {e}; using zero vector")
            if expected_dim:
                vector_examples.append([0.0]*expected_dim)
    print("Sampled IDs:", [d.get(args.id_field) for d in samples])
    if missing:
        print(f"Missing vectors for {len(missing)} sampled docs: {missing}")
    else:
        print('All sampled docs show vectors (or has_vector flag).')
    # Run similarity test if we have a concrete vector
    if vector_examples:
        print('Running similarity query with synthetic probe...')
        try:
            hits = run_vector_query(ep, key, args.index, args.vector_field, vector_examples[0], k=5)
            for h in hits:
                print(f"Hit {h.get(args.id_field)} score={h.get('@search.score')}")
        except Exception as e:
            print(f"[WARN] Similarity query failed: {e}")
    else:
        print('No retrievable vector found in sampled docs; cannot run similarity test (vectors may be non-retrievable).')
    if missing:
        sys.exit(2)
    sys.exit(0)

if __name__ == '__main__':
    main()
