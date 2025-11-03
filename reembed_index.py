import os, json, requests, sys, time
from embedding_utils import batch_embed, provider_info

# Usage: python reembed_index.py --index cobol-files --field snippet --vector-field snippet_vector --has-flag has_vector --batch 1000 --dry-run
import argparse

API='2024-07-01'

# Load settings
try:
    vals=json.load(open('local.settings.json')).get('Values',{})
    for k,v in vals.items(): os.environ.setdefault(k,v)
except Exception:
    pass

EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY:
    print('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    sys.exit(1)

headers={'api-key':KEY,'Content-Type':'application/json'}

def fetch_batch(index, select, filter_expr, top, skip):
    url=f"{EP.rstrip('/')}/indexes/{index}/docs/search?api-version={API}"
    body={'search':'*','select':select,'top':top,'skip':skip}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(url, headers=headers, json=body)
    if r.status_code!=200:
        raise RuntimeError(f"Search fetch error {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def push(index, docs):
    if not docs: return
    url=f"{EP.rstrip('/')}/indexes/{index}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:200]}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--text-field', default='snippet', help='Source text field to embed')
    ap.add_argument('--vector-field', default='snippet_vector')
    ap.add_argument('--id-field', default='call_id', help='Document key field')
    ap.add_argument('--has-flag', default='has_vector', help='Boolean field to mark embedding presence')
    ap.add_argument('--filter', default='has_vector eq false')
    ap.add_argument('--batch', type=int, default=1000)
    ap.add_argument('--embed-batch', type=int, default=64)
    ap.add_argument('--limit', type=int, default=None)
    ap.add_argument('--dry-run', action='store_true')
    args=ap.parse_args()

    index=args.index
    # Determine total needing embedding
    count_url=f"{EP.rstrip('/')}/indexes/{index}/docs/search?api-version={API}"
    count_resp=requests.post(count_url, headers=headers, json={'search':'*','filter':args.filter,'count':True,'top':0})
    if count_resp.status_code!=200:
        print('Count query failed:', count_resp.text[:200])
        return
    to_process=count_resp.json().get('@odata.count',0)
    print(f"Provider: {provider_info()} | Needing embeddings: {to_process}")
    processed=0
    skip=0
    docs_per_page= min(args.batch, 1000)
    all_docs=[]
    while processed < to_process:
        remaining = to_process - processed
        want = min(docs_per_page, remaining)
        select_clause = f"{args.id_field},{args.text_field}"
        page = fetch_batch(index, select_clause, args.filter, want, skip)
        if not page:
            break
        skip += len(page)
        texts = [d.get(args.text_field) or '' for d in page]
        vecs = batch_embed(texts, batch_size=args.embed_batch)
        for d, v in zip(page, vecs):
            d[args.vector_field] = v
            d[args.has_flag] = True
        if args.dry_run:
            print('Dry-run first doc update sample:', {args.id_field: page[0].get(args.id_field)})
            break
        for i in range(0, len(page), 500):
            push(index, page[i:i+500])
        processed += len(page)
        print(f"Embedded {processed}/{to_process}")
        if args.limit and processed >= args.limit:
            break
    print('Done re-embedding.')

if __name__=='__main__':
    main()
