import os,requests,json,sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def fetch(ep,key,pids):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    # Build an or search (Azure Search OR; exact filtering via post-search filter may require eq checks)
    terms=[p for p in pids]
    body={'search':' OR '.join(terms),'top':len(terms)*5,'select':'program_id,risk_score,edge_count,node_count,max_depth,has_cycles'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Error',r.status_code,r.text[:200]); return []
    return r.json().get('value',[])

def main():
    if len(sys.argv)<2:
        print('Usage: python diagnostics/show_flow_docs.py P1,P2,..'); return
    load_settings(); ep,key=resolve(); pids=[p.strip().upper() for p in sys.argv[1].split(',') if p.strip()]
    docs=fetch(ep,key,pids)
    by={d['program_id']:d for d in docs}
    out=[]
    for p in pids:
        out.append(by.get(p,{'program_id':p,'missing':True}))
    print(json.dumps(out,indent=2))

if __name__=='__main__':
    main()
