import os, json, requests, sys
from embedding_utils import batch_embed, provider_info

API='2024-07-01'
INDEX='cobol-files'
ID_FIELD='file_id'
VECTOR_FIELD='file_vector'
FLAG_FIELD='has_vector'
BATCH=800
EMBED_BATCH=64

# Added incremental mode: only embed docs missing FLAG_FIELD unless --force provided.
FORCE='--force' in sys.argv

vals=json.load(open('local.settings.json')).get('Values',{})
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY: raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
headers={'api-key':KEY,'Content-Type':'application/json'}

def fetch_all(select_extra=""):
    out=[]; skip=0; page=1000
    select=f"{ID_FIELD},name,path,program_id"
    if select_extra:
        select += ','+select_extra
    while True:
        body={'search':'*','select':select,'top':page,'skip':skip}
        r=requests.post(f"{EP.rstrip('/')}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code!=200:
            raise RuntimeError(f"Fetch error {r.status_code}: {r.text[:200]}\nBody: {body}")
        batch=r.json().get('value',[])
        if not batch: break
        out.extend(batch)
        skip+=len(batch)
        if len(batch)<page: break
    return out

def upload(docs):
    if not docs: return
    url=f"{EP.rstrip('/')}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

def main():
    print('Embedding provider:', provider_info())
    docs=fetch_all(select_extra=FLAG_FIELD)
    print('Docs fetched:', len(docs))
    if not FORCE:
        before=len(docs)
        docs=[d for d in docs if not d.get(FLAG_FIELD)]
        print(f"Docs needing embedding: {len(docs)} (skipped {before-len(docs)} already embedded) -- use --force to re-embed all")
    else:
        print('Force mode: re-embedding all documents.')
    # Build composite text (file name + path + program id)
    texts=[f"{d.get('name','')} | {d.get('path','')} | {d.get('program_id','')}" for d in docs]
    vectors=batch_embed(texts, batch_size=EMBED_BATCH)
    for d,v in zip(docs,vectors):
        d[VECTOR_FIELD]=v
        d[FLAG_FIELD]=True
    # Chunked upload
    uploaded=0
    for i in range(0,len(docs),500):
        upload(docs[i:i+500])
        uploaded+=len(docs[i:i+500])
        print(f"Uploaded {uploaded}/{len(docs)}")
    print('Completed file embeddings.')

if __name__=='__main__':
    main()
