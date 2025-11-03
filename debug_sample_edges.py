import os, json, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
IDX=os.getenv('EDGE_INDEX','new_cobol_flow_edges')
vals={}
if os.path.exists('local.settings.json'):
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception:
        pass
for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
    if k in vals and k not in os.environ:
        os.environ[k]=vals[k]
EP=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
KEY=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
if not EP or not KEY:
    print('Missing endpoint/key'); sys.exit(1)
EP=EP.rstrip('/')
headers={'api-key':KEY,'Content-Type':'application/json'}

def q(body):
    url=f"{EP}/indexes/{IDX}/docs/search?api-version={API}"
    r=requests.post(url,headers=headers,json=body,timeout=60)
    return r

# 1. Count
r=q({'search':'*','top':0,'count':True})
print('COUNT status', r.status_code, 'count', r.json().get('@odata.count') if r.status_code==200 else r.text[:200])
# 2. Sample docs
r=q({'search':'*','top':5,'select':'edge_id,file_id,program_id,caller_para,target_para,resolved_target_para'})
print('SAMPLE status', r.status_code)
if r.status_code==200:
    for d in r.json().get('value',[]):
        print(d)
# 3. Specific search terms
for term in ['LONPF2','REBATE']:
    r=q({'search':term,'top':10,'select':'edge_id,file_id,program_id,caller_para,target_para,resolved_target_para'})
    print(f'TERM {term} status', r.status_code)
    if r.status_code==200:
        vals=r.json().get('value',[])
        print(' hits', len(vals))
        for d in vals[:3]:
            print('  ', d)
# 4. Filter queries
for pid in ['LONPF2','REBATE']:
    r=q({'search':'*','top':5,'filter':f"program_id eq '{pid}'",'select':'edge_id,file_id,program_id,caller_para,target_para'})
    print(f'FILTER program_id {pid} status', r.status_code)
    if r.status_code==200:
        print('  count', len(r.json().get('value',[])))

