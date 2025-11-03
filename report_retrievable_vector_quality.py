#!/usr/bin/env python3
"""Report quality metrics for retrievable vector field (default: contentVector_r).

Metrics:
  - Sample count (non-null vectors)
  - Dimensions (validated against expected --dims)
  - Norm stats: mean, min, max, stdev
  - % zero vectors (all components == 0)
  - Pairwise cosine stats (mean, min, max, sample size) across up to N sampled pairs
  - Degeneracy flags (all identical, zero norm prevalence, mismatched dims)

Sampling:
  - Pages through index with $skip/$top until --limit collected.
  - Skips docs where field missing or null.

Env credentials (auto-load from local.settings.json Values if unset):
  SEARCH_ENDPOINT / SEARCH_KEY or AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY

Usage examples:
  python report_retrievable_vector_quality.py --index new-cobol-files --limit 500
  python report_retrievable_vector_quality.py --index new-cobol-files --json --dims 3072 --limit 300

Exit codes:
  0 success
  2 credential issue
  3 no vectors collected
"""
from __future__ import annotations
import os, json, math, argparse, statistics, random, sys
from typing import List, Dict
import requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
DEFAULT_FIELD = 'contentVector_r'

# ---------------- Credential helpers ----------------

def load_local_settings_values():
    path='local.settings.json'
    if not os.path.exists(path): return {}
    try:
        with open(path,'r',encoding='utf-8') as f:
            return json.load(f).get('Values',{}) or {}
    except Exception:
        return {}

def resolve_creds():
    vals = load_local_settings_values()
    ep = (os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or
          vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT'))
    key = (os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or
           vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY'))
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

# ---------------- Data fetch ----------------

def fetch_page(ep: str, key: str, index: str, skip: int, top: int, select: str):
    url = f"{ep}/indexes/{index}/docs"
    params = {
        'api-version': API_VERSION,
        '$skip': str(skip),
        '$top': str(top),
        '$select': select,
    }
    r = requests.get(url, headers={'api-key':key}, params=params, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:180]}")
    return r.json().get('value',[])

# ---------------- Math helpers ----------------

def vector_norm(v: List[float]) -> float:
    return math.sqrt(sum(x*x for x in v))

def cosine(a: List[float], b: List[float]) -> float:
    num = sum(x*y for x,y in zip(a,b))
    na = vector_norm(a) or 1.0
    nb = vector_norm(b) or 1.0
    return num/(na*nb)

# ---------------- Main ----------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--field', default=DEFAULT_FIELD)
    ap.add_argument('--dims', type=int, default=3072)
    ap.add_argument('--limit', type=int, default=500, help='Max number of vectors to sample')
    ap.add_argument('--page-size', type=int, default=200)
    ap.add_argument('--pairs', type=int, default=60, help='Max pairwise cosine samples')
    ap.add_argument('--json', action='store_true')
    args = ap.parse_args()

    ep, key = resolve_creds()
    select = f"id,{args.field}"
    collected: List[List[float]] = []
    skip = 0
    while len(collected) < args.limit:
        page = fetch_page(ep,key,args.index,skip,args.page_size,select)
        if not page: break
        skip += len(page)
        for d in page:
            if len(collected) >= args.limit: break
            vec = d.get(args.field)
            if not vec: continue
            if not isinstance(vec,list) or not vec: continue
            collected.append(vec)
    if not collected:
        print('No vectors collected (field missing or empty).', file=sys.stderr)
        sys.exit(3)

    dims = {len(v) for v in collected}
    all_dims_match = (len(dims)==1 and list(dims)[0]==args.dims)
    norms = [vector_norm(v) for v in collected]
    zero_vectors = sum(1 for v,n in zip(collected,norms) if n==0.0)
    identical = len({tuple(v[:16]) for v in collected}) == 1  # quick prefix check

    # Pairwise cosine sampling
    cos_samples: List[float] = []
    ids = list(range(len(collected)))
    random.shuffle(ids)
    for i in range(0,len(ids)-1,2):
        if len(cos_samples) >= args.pairs: break
        a = collected[ids[i]]
        b = collected[ids[i+1]]
        cos_samples.append(cosine(a,b))

    report = {
        'index': args.index,
        'field': args.field,
        'sample_count': len(collected),
        'expected_dims': args.dims,
        'observed_dims_unique': sorted(list(dims))[:10],
        'all_dims_match_expected': all_dims_match,
        'norm': {
            'mean': statistics.mean(norms),
            'min': min(norms),
            'max': max(norms),
            'stdev': statistics.pstdev(norms) if len(norms)>1 else 0.0,
        },
        'zero_vectors': {
            'count': zero_vectors,
            'percent': (zero_vectors/len(collected))*100.0,
        },
        'cosine_pairs': {
            'samples': len(cos_samples),
            'mean': statistics.mean(cos_samples) if cos_samples else None,
            'min': min(cos_samples) if cos_samples else None,
            'max': max(cos_samples) if cos_samples else None,
        },
        'flags': {
            'all_identical_prefix16': identical,
            'dims_mismatch': not all_dims_match,
            'all_zero': zero_vectors == len(collected),
        }
    }

    if args.json:
        print(json.dumps(report, indent=2))
    else:
        print(f"Index: {report['index']}  Field: {report['field']}")
        print(f"Samples: {report['sample_count']}  Dims(expected/observed_unique): {report['expected_dims']} / {report['observed_dims_unique']}")
        norm = report['norm']
        print(f"Norm mean {norm['mean']:.4f} min {norm['min']:.4f} max {norm['max']:.4f} stdev {norm['stdev']:.4f}")
        zv = report['zero_vectors']
        print(f"Zero vectors: {zv['count']} ({zv['percent']:.2f}%)")
        cp = report['cosine_pairs']
        if cp['samples']:
            print(f"Cosine pairs: samples {cp['samples']} mean {cp['mean']:.4f} min {cp['min']:.4f} max {cp['max']:.4f}")
        flags = report['flags']
        active = [k for k,v in flags.items() if v]
        print("Flags: " + (', '.join(active) if active else 'none'))

if __name__=='__main__':
    main()
