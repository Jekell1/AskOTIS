import os, json, requests, sys

TARGET_INDEXES = [
    'cobol-copybooks','cobol-facts','cobol-files','cobol-flow-edges-v2','cobol-routine-aliases'
]

# load settings
try:
    vals = json.load(open('local.settings.json')).get('Values', {})
    for k,v in vals.items():
        os.environ.setdefault(k,v)
except Exception:
    pass

EP=os.environ.get('SEARCH_ENDPOINT'); KEY=os.environ.get('SEARCH_KEY')
if not EP or not KEY:
    print('Missing search endpoint/key'); sys.exit(1)

API='2024-07-01'
headers={'api-key':KEY,'Content-Type':'application/json'}


def count_index(idx:str)->dict:
    # use filter has_vector eq false if field exists; attempt count with two queries
    url=f"{EP.rstrip('/')}/indexes/{idx}/docs/search?api-version={API}"
    # total
    r_tot=requests.post(url, headers=headers, json={'search':'*','count':True,'top':0})
    total = r_tot.json().get('@odata.count') if r_tot.status_code==200 else None
    # missing
    r_miss=requests.post(url, headers=headers, json={'search':'*','filter':'has_vector eq false','count':True,'top':0})
    if r_miss.status_code==400:
        # field not present
        return {'index':idx,'total':total,'missing':None,'note':'has_vector field not in schema'}
    missing = r_miss.json().get('@odata.count') if r_miss.status_code==200 else None
    return {'index':idx,'total':total,'missing':missing,'note':None}

results=[count_index(i) for i in TARGET_INDEXES]
print('Index | total | missing(has_vector=false) | note')
for r in results:
    print(f"{r['index']:22} | {str(r['total']).rjust(6)} | {str(r['missing']).rjust(8)} | {r['note'] or ''}")
