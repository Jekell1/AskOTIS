"""Build program->copybook dependency edge docs into new_cobol_program_copybook_edges.

Run:
  python ingest/build_program_copybook_edges.py --commit
"""
from __future__ import annotations
import os,json,requests,hashlib,time,argparse
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
USAGE_INDEX='new_cobol_copybook_usage'
EDGE_INDEX='new_cobol_program_copybook_edges'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search(ep,key,index,body):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200: raise SystemExit(r.text[:400])
    return r.json()

def iter_usage(ep,key):
    skip=0; page=3000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'program_id,copybook_name_plain,line_number'}
        data=search(ep,key,USAGE_INDEX,body); rows=data.get('value',[])
        if not rows: break
        for r in rows: yield r
        skip+=len(rows)
        if len(rows)<page: break

def build_edges(ep,key):
    m={}  # (program,copybook)-> {count,first_line}
    for r in iter_usage(ep,key):
        p=r.get('program_id'); c=r.get('copybook_name_plain'); ln=r.get('line_number')
        if not p or not c: continue
        k=(p,c)
        slot=m.get(k)
        if not slot:
            m[k]={'count':0,'first':ln if isinstance(ln,int) else None}
        slot=m[k]
        slot['count']+=1
        if ln and (slot['first'] is None or ln < slot['first']):
            slot['first']=ln
    ts=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    docs=[]
    for (p,c),info in m.items():
        edge_id=hashlib.sha1(f"{p}|{c}".encode('utf-8')).hexdigest()[:40]
        docs.append({'edge_id':edge_id,'program_id':p,'copybook_name_plain':c,'first_line':info['first'] or 0,'occurrence_count':info['count'],'ingested_at':ts})
    return docs

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{EDGE_INDEX}/docs/index?api-version={API}"
    headers={'api-key':key,'Content-Type':'application/json'}
    for i in range(0,len(docs),1000):
        chunk=docs[i:i+1000]
        payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in chunk]}
        r=requests.post(url,headers=headers,json=payload,timeout=120)
        if r.status_code not in (200,201):
            print('[ERROR] upload failed',r.status_code,r.text[:300])
        else:
            print(f"[UPLOAD] +{len(chunk)} / {len(docs)}")

def main():
    ap=argparse.ArgumentParser(); ap.add_argument('--commit',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve()
    docs=build_edges(ep,key)
    print(f"Prepared edges={len(docs)}")
    if not args.commit:
        print(json.dumps({'sample':docs[:5]},indent=2)); print('Run with --commit to upload.'); return
    upload(ep,key,docs); print('Done.')

if __name__=='__main__':
    main()
