"""Post-ingestion verification for vector-enabled indexes.

Checks:
 - Each target index exists
 - Document count > 0
 - At least one document reports has_vector = true (sample facet search)
 - Optionally list missing indexes or zero-vector situations

Usage:
  python post_ingestion_verify.py --indexes cobol-symbols,cobol-paragraphs,code-chunks
"""
import os, json, argparse, requests

API_VERSION = '2024-07-01'

def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def get_index_stats(endpoint: str, key: str, name: str):
    h={'api-key': key}
    base = endpoint.rstrip('/')
    r = requests.get(f"{base}/indexes/{name}/stats?api-version={API_VERSION}", headers=h, timeout=10)
    if r.status_code == 404:
        return {'name': name, 'exists': False}
    if r.status_code != 200:
        return {'name': name, 'exists': True, 'error': f'stats {r.status_code}'}
    js = r.json()
    return {'name': name, 'exists': True, 'documentCount': js.get('documentCount'), 'storageSize': js.get('storageSize')}

def facet_has_vector(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}/docs/search?api-version={API_VERSION}"
    body = {"search":"*","top":0,"count":True,"facets":["has_vector,count:2"]}
    h={'api-key': key, 'Content-Type': 'application/json'}
    try:
        r = requests.post(url, headers=h, json=body, timeout=12)
        if r.status_code != 200:
            return {'error': f'facet status {r.status_code}'}
        js = r.json()
        buckets = js.get('@search.facets',{}).get('has_vector',[])
        has_true = next((b.get('count') for b in buckets if b.get('value') is True), 0)
        has_false = next((b.get('count') for b in buckets if b.get('value') is False), 0)
        return {'vector_true': has_true, 'vector_false': has_false}
    except Exception as e:
        return {'error': str(e)}

def main():
    parser = argparse.ArgumentParser(description='Verify post-ingestion vector readiness.')
    parser.add_argument('--indexes', default='cobol-symbols,cobol-paragraphs,cobol-xrefs,cobol-calls,code-chunks')
    args = parser.parse_args()
    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    targets = [x.strip() for x in args.indexes.split(',') if x.strip()]
    rows=[]
    failures=0
    for name in targets:
        stat = get_index_stats(endpoint, key, name)
        if not stat.get('exists'):
            rows.append({**stat, 'status':'MISSING'})
            failures += 1
            continue
        if 'error' in stat:
            rows.append({**stat, 'status':'ERROR'})
            failures += 1
            continue
        facet = facet_has_vector(endpoint, key, name)
        vector_true = facet.get('vector_true',0) if 'error' not in facet else 0
        doc_count = stat.get('documentCount',0)
        status='OK'
        if doc_count == 0:
            status='EMPTY'
            failures +=1
        elif vector_true == 0:
            status='NO_VECTORS'
            failures +=1
        rows.append({**stat, **facet, 'status': status})
    # Output summary
    print('Index | Docs | Vectors(>0) | Status')
    print('-'*70)
    for r in rows:
        print(f"{r['name']} | {r.get('documentCount','-')} | {r.get('vector_true','-')} | {r['status']}")
    print('-'*70)
    if failures:
        print(f"FAILURES: {failures} index(es) need attention.")
    else:
        print('All indexes populated with vectors. ✅')
    return 1 if failures else 0

if __name__ == '__main__':
    raise SystemExit(main())
