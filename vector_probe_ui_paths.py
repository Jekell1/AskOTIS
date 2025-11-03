"""Vector probe for UI paths (path_vector).

Usage:
  python vector_probe_ui_paths.py --q "menu to loan disbursement" --k 10
"""
import os, json, argparse, requests
from embedding_utils import batch_embed
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_ui_paths'
VECTOR_FIELD='path_vector'


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('missing endpoint/key')
    return ep.rstrip('/'), key


def vector_search(ep,key,vec,k,select='path_id,root_program_id,leaf_program_id,length,score,ui_program_count,program_sequence_json,has_vector'):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'vectorQueries':[{'kind':'vector','vector':vec,'k':k,'fields':VECTOR_FIELD}], 'select':select}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('vector search failed',r.status_code,r.text[:300]); return []
    return r.json().get('value',[])

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--q',required=True)
    ap.add_argument('--k',type=int,default=5)
    ap.add_argument('--show-seq',action='store_true')
    args=ap.parse_args()
    load(); ep,key=resolve(); vec=batch_embed([args.q])[0]
    hits=vector_search(ep,key,vec,args.k)
    for i,h in enumerate(hits):
        seq=json.loads(h.get('program_sequence_json') or '[]')
        seq_disp=' -> '.join(seq)
        print(f"{i+1:2d}. {h.get('path_id')} root={h.get('root_program_id')} leaf={h.get('leaf_program_id')} len={h.get('length')} ui={h.get('ui_program_count')} score={h.get('score')} has_vec={h.get('has_vector')} ")
        if args.show_seq:
            print('    ',seq_disp[:250])
