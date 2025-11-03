import os, json, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
vals={}
if os.path.exists('local.settings.json'):
    try: vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception: pass
for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
    if k in vals and k not in os.environ: os.environ[k]=vals[k]
EP=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT'); KEY=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
if not EP or not KEY: print('Missing keys'); sys.exit(1)
EP=EP.rstrip('/')
headers={'api-key':KEY,'Content-Type':'application/json'}

def search(idx, body):
    url=f"{EP}/indexes/{idx}/docs/search?api-version={API}"
    r=requests.post(url,headers=headers,json=body,timeout=60)
    if r.status_code!=200:
        print('FAIL', idx, r.status_code, r.text[:200])
        return []
    return r.json().get('value',[])

progs=['LONPF2','REBATE']
print('Paragraph samples:')
for p in progs:
    rows=search('new_cobol_paragraphs',{'search':p,'filter':f"program_id eq '{p}'",'top':10,'select':'program_id,paragraph_name'})
    print(p, 'paras', len(rows))
    for r in rows[:3]:
        print('  ', r)
print('\nEdge samples:')
for p in progs:
    rows=search('new_cobol_flow_edges',{'search':p,'filter':f"program_id eq '{p}'",'top':10,'select':'program_id,file_id,caller_para,target_para,resolved_target_para'})
    print(p, 'edges', len(rows))
    for r in rows[:3]:
        print('  ', r)
print('\nRaw lexical edge search (no filter):')
for p in progs:
    rows=search('new_cobol_flow_edges',{'search':p,'top':10,'select':'program_id,file_id,caller_para,target_para,resolved_target_para'})
    print(p, 'lex hits', len(rows))
    for r in rows[:3]:
        print('  ', r)
