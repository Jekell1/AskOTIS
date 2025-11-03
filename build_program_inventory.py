"""Construct and upload program inventory metrics into new_cobol_program_inventory.

Sources: new_cobol_copybook_usage index.

Metrics:
  - copybook_count (distinct copybook_name_plain per program)
  - paragraph_count (distinct paragraph_name per program)
  - usage_rows (total rows referencing the program)
  - metrics_json (future extensibility placeholder)

Run preview:
  python build_program_inventory.py
Commit:
  python build_program_inventory.py --commit
"""
from __future__ import annotations
import os, json, argparse, requests, time
from collections import defaultdict

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
USAGE_INDEX='new_cobol_copybook_usage'
INV_INDEX='new_cobol_program_inventory'
PAGE=3000
BATCH=1000

SELECT='program_id,copybook_name_plain,paragraph_name'

CANON_RE=None

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search(ep,key,index,body):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        raise SystemExit(r.text[:400])
    return r.json()

def iter_usage(ep,key):
    skip=0
    while True:
        body={'search':'*','top':PAGE,'skip':skip,'select':SELECT}
        data=search(ep,key,USAGE_INDEX,body)
        rows=data.get('value',[])
        if not rows:
            break
        for r in rows:
            yield r
        skip+=len(rows)
        if len(rows)<PAGE:
            break

def build_inventory(ep,key):
    copybooks=defaultdict(set)
    paragraphs=defaultdict(set)
    usage_rows=defaultdict(int)
    for row in iter_usage(ep,key):
        p=row.get('program_id')
        if not p:
            continue
        usage_rows[p]+=1
        c=row.get('copybook_name_plain')
        if c:
            copybooks[p].add(c)
        par=row.get('paragraph_name')
        if par:
            paragraphs[p].add(par)
    ts=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime())
    docs=[]
    for prog in sorted(usage_rows.keys()):
        docs.append({
            'program_id': prog,
            'copybook_count': len(copybooks[prog]),
            'paragraph_count': len(paragraphs[prog]),
            'usage_rows': usage_rows[prog],
            'last_ingested_at': ts,
            'metrics_json': json.dumps({'copybooks': len(copybooks[prog]), 'paragraphs': len(paragraphs[prog])})
        })
    return docs

def upload(ep,key,docs):
    if not docs:
        print('No docs to upload.')
        return
    url=f"{ep}/indexes/{INV_INDEX}/docs/index?api-version={API}"
    for i in range(0,len(docs),BATCH):
        chunk=docs[i:i+BATCH]
        payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in chunk]}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=120)
        if r.status_code not in (200,201):
            print('[ERROR] upload failed',r.status_code,r.text[:300])
        else:
            print(f"[UPLOAD] +{len(chunk)} / {len(docs)}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--commit',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    docs=build_inventory(ep,key)
    print(f"Prepared {len(docs)} program inventory docs")
    sample=docs[:3]
    print(json.dumps({'sample':sample},indent=2))
    if not args.commit:
        print('Run with --commit to upload.')
        return
    upload(ep,key,docs)
    print('Done.')

if __name__=='__main__':
    main()
