import os, json, requests, sys
from embedding_utils import batch_embed, provider_info

API='2024-07-01'
INDEX='cobol-routine-aliases'
TEXT_FIELD='alias'
VECTOR_FIELD='alias_vector'
ID_FIELD='alias'
BATCH=512
EMBED_BATCH=64
EXPORT_FILE='routine_aliases_export.jsonl'

# Load settings
vals=json.load(open('local.settings.json')).get('Values',{})
for k,v in vals.items(): os.environ.setdefault(k,v)
EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY:
    raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')

headers={'api-key':KEY,'Content-Type':'application/json'}

def load_export_file():
    docs=[]
    with open(EXPORT_FILE,'r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            obj=json.loads(line)
            # Remove search score metadata if present
            obj.pop('@search.score', None)
            docs.append(obj)
    return docs

def chunk(seq, size):
    for i in range(0, len(seq), size):
        yield seq[i:i+size]

def upload(docs):
    if not docs: return
    url=f"{EP.rstrip('/')}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

def main():
    print('Embedding provider:', provider_info())
    docs=load_export_file()
    print(f'Total aliases loaded from export: {len(docs)}')
    texts=[d.get(TEXT_FIELD,'') for d in docs]
    vectors=batch_embed(texts, batch_size=EMBED_BATCH)
    for d,v in zip(docs, vectors):
        d[VECTOR_FIELD]=v
        d['has_vector']=True
    # Upload in chunks of 500
    uploaded=0
    for c in chunk(docs, 500):
        upload(c)
        uploaded+=len(c)
        print(f'Uploaded {uploaded}/{len(docs)}')
    print('Completed alias vector ingestion.')

if __name__=='__main__':
    main()
