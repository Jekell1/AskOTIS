"""Quick probe of new_cobol_flow_edges_v2 coverage and sample edges."""
import os, json, argparse, requests, random
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def count_docs(ep,key, filt=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'count':True,'top':0}
    if filt: body['filter']=filt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(r.text)
    return r.json().get('@odata.count',0)

def sample_docs(ep,key, top=5):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'count':False,'top':top,'select':'edge_id,program_id,caller_para,target_para,resolved_target_para,edge_kind,edge_subkind,line'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(r.text)
    return [x for x in r.json().get('value',[])]

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--filter')
    ap.add_argument('--top',type=int,default=8)
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    total=count_docs(ep,key)
    print(f'Total edges: {total}')
    if args.filter:
        cf=count_docs(ep,key,args.filter)
        print(f'Filtered ({args.filter}) count: {cf}')
    for rec in sample_docs(ep,key,args.top):
        print(json.dumps(rec,indent=2))

if __name__=='__main__':
    main()
