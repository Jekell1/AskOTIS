import os, json, sys, requests
from embedding_utils import batch_embed, provider_info

API='2024-07-01'
INDEX='cobol-copybooks'
ID_FIELD='copybook_id'
VECTOR_FIELD='copybook_vector'
FLAG_FIELD='has_vector'
EMBED_BATCH=64
PAGE=1000
FORCE='--force' in sys.argv

# Load settings
try:
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    for k,v in vals.items():
        os.environ.setdefault(k,v)
except Exception:
    pass

EP=os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
KEY=os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
if not EP or not KEY:
    raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
headers={'api-key':KEY,'Content-Type':'application/json'}


def stream_docs():
    select=f"{ID_FIELD},copybook_name,parent_path,replacing_clause,{FLAG_FIELD}" if FLAG_FIELD else f"{ID_FIELD},copybook_name,parent_path,replacing_clause"
    last_id=None
    use_order=True
    while True:
        filter_expr=None
        if last_id:
            safe_last = last_id.replace("'","''")
            filter_expr=f"{ID_FIELD} gt '{safe_last}'"
        body={'search':'*','select':select,'top':PAGE}
        if use_order:
            body['orderby']=f'{ID_FIELD} asc'
        if filter_expr:
            body['filter']=filter_expr
        r=requests.post(f"{EP.rstrip('/')}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code==400 and use_order and 'orderby' in r.text.lower():
            # fallback: server doesn't accept orderby, switch mode
            use_order=False
            print('OrderBy not accepted; falling back to legacy skip paging (may hit 100k limit).')
            # fallback to skip loop
            yield from legacy_skip_mode(select)
            return
        if r.status_code!=200:
            raise RuntimeError(f"Fetch error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch:
            break
        for doc in batch:
            yield doc
        last_id=batch[-1].get(ID_FIELD)
        if len(batch)<PAGE:
            break

def legacy_skip_mode(select):
    skip=0
    while True:
        body={'search':'*','select':select,'top':PAGE,'skip':skip}
        r=requests.post(f"{EP.rstrip('/')}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code!=200:
            raise RuntimeError(f"Legacy skip fetch error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch:
            break
        for doc in batch:
            yield doc
        skip+=len(batch)
        if skip>=100000:
            print('Reached skip 100000 limit; stopping early.')
            break
        if len(batch)<PAGE:
            break

def upload(docs):
    if not docs: return
    url=f"{EP.rstrip('/')}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

def build_text(d):
    parts=[d.get('copybook_name',''), d.get('parent_path','')]
    rc=d.get('replacing_clause')
    if rc:
        parts.append(rc)
    return ' | '.join(p for p in parts if p)

def main():
    print('Embedding provider:', provider_info())
    to_process=[]
    total=0
    for doc in stream_docs():
        total+=1
        if FORCE or not doc.get(FLAG_FIELD):
            to_process.append(doc)
    print('Docs scanned:', total)
    if not FORCE:
        print(f"Docs needing embedding: {len(to_process)} (skipped {total-len(to_process)} already embedded)")
    else:
        print('Force mode: re-embedding all documents.')
    processed=0
    batch_docs=[]; batch_texts=[]
    def flush():
        nonlocal processed, batch_docs, batch_texts
        if not batch_docs: return
        vecs=batch_embed(batch_texts, batch_size=EMBED_BATCH)
        for d,v in zip(batch_docs,vecs):
            d[VECTOR_FIELD]=v
            d[FLAG_FIELD]=True
        for i in range(0,len(batch_docs),500):
            upload(batch_docs[i:i+500])
        processed+=len(batch_docs)
        print(f"Embedded {processed}/{len(to_process)}")
        batch_docs.clear(); batch_texts.clear()
    for d in to_process:
        batch_docs.append(d)
        batch_texts.append(build_text(d))
        if len(batch_docs)>=PAGE:
            flush()
    flush()
    print('Completed copybook embeddings.')

if __name__=='__main__':
    main()
