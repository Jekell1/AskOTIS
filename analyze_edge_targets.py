"""Analyze distribution of target resolution in new_cobol_flow_edges.
Counts:
  - total docs
  - empty target_para
  - empty resolved_target_para
  - both empty (non-edges)
  - with any target
Outputs top sample of docs missing targets.
"""
import os, json, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges'
vals={}
if os.path.exists('local.settings.json'):
    try: vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception: pass
for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
    if k in vals and k not in os.environ: os.environ[k]=vals[k]
EP=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
KEY=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
if not EP or not KEY:
    print('Missing endpoint/key'); sys.exit(1)
EP=EP.rstrip('/')
headers={'api-key':KEY,'Content-Type':'application/json'}

def search(body):
    url=f"{EP}/indexes/{INDEX}/docs/search?api-version={API}"
    r=requests.post(url,headers=headers,json=body,timeout=60)
    if r.status_code!=200:
        print('FAIL', r.status_code, r.text[:200]); sys.exit(2)
    return r.json()

# total count
js=search({'search':'*','top':0,'count':True})
TOTAL=js.get('@odata.count',0)
# missing target_para
miss_target=search({'search':'*','top':0,'count':True,'filter':"target_para eq ''"}).get('@odata.count',0)
# missing resolved_target_para
miss_resolved=search({'search':'*','top':0,'count':True,'filter':"resolved_target_para eq ''"}).get('@odata.count',0)
# both empty (treat as non-edge)
both_empty=search({'search':'*','top':0,'count':True,'filter':"target_para eq '' and resolved_target_para eq ''"}).get('@odata.count',0)
print('Total docs:', TOTAL)
print('Empty target_para:', miss_target)
print('Empty resolved_target_para:', miss_resolved)
print('Both empty (non-edges):', both_empty)
if both_empty:
    samp=search({'search':'*','top':5,'filter':"target_para eq '' and resolved_target_para eq ''",'select':'edge_id,file_id,program_id,caller_para,target_para,resolved_target_para'})
    for r in samp.get('value',[]):
        print(' SAMPLE', r)
