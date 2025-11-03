"""Expand a UI path with detailed screen node summaries.

Usage:
  python expand_path_with_screens.py --path-id <id> [--show-screens 5]
  or semantic path lookup:
  python vector_probe_ui_paths.py --q "open account creation" --k 3 --show-seq
  then pass a path_id here.
"""
import os, json, argparse, requests
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PATH_INDEX='new_cobol_ui_paths'
SCREEN_INDEX='new_cobol_screen_nodes'


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
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


def fetch_path(ep,key,path_id):
    url=f"{ep}/indexes/{PATH_INDEX}/docs/search?api-version={API}"
    body={'search':path_id,'filter':f"path_id eq '{path_id}'",'top':1,'select':'path_id,program_sequence_json,screen_ids_json,guards_json,edge_freqs_json,avg_edge_freq,min_edge_freq'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise SystemExit('fetch path failed')
    val=r.json().get('value',[])
    return val[0] if val else None


def fetch_screens(ep,key,screen_ids):
    if not screen_ids: return []
    # batch filter using or clauses (chunk if large)
    out=[]
    headers={'api-key':key,'Content-Type':'application/json'}
    url=f"{ep}/indexes/{SCREEN_INDEX}/docs/search?api-version={API}"
    chunk=40
    for i in range(0,len(screen_ids),chunk):
        part=screen_ids[i:i+chunk]
        filt=' or '.join([f"screen_id eq '{sid}'" for sid in part])
        body={'search':'*','filter':filt,'top':len(part),'select':'screen_id,program_id,screen_name,field_count,action_count,transition_count,summary_text'}
        r=requests.post(url,headers=headers,json=body)
        if r.status_code!=200:
            print('screen fetch failed',r.status_code,r.text[:200]); continue
        out.extend(r.json().get('value',[]))
    return out

if __name__=='__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--path-id',required=True)
    ap.add_argument('--show-screens',type=int,default=5)
    args=ap.parse_args()
    load(); ep,key=resolve()
    path=fetch_path(ep,key,args.path_id)
    if not path:
        print('Path not found'); raise SystemExit(1)
    seq=json.loads(path.get('program_sequence_json') or '[]')
    screen_ids=json.loads(path.get('screen_ids_json') or '[]')
    print('PATH',path.get('path_id'),'len',len(seq),'avg_edge_freq',path.get('avg_edge_freq'),'min_edge_freq',path.get('min_edge_freq'))
    print('SEQ:',' -> '.join(seq))
    print('GUARDS:', json.loads(path.get('guards_json') or '[]'))
    print('EDGE_FREQS:', json.loads(path.get('edge_freqs_json') or '[]')[:30])
    if not screen_ids:
        print('No linked screen ids.'); raise SystemExit(0)
    screens=fetch_screens(ep,key,screen_ids)
    print(f'SCREENS ({len(screens)} linked, showing up to {args.show_screens}):')
    for s in screens[:args.show_screens]:
        print(f" - {s.get('screen_id')} prog={s.get('program_id')} fields={s.get('field_count')} actions={s.get('action_count')} trans={s.get('transition_count')} name={s.get('screen_name')}")
        print('    ', (s.get('summary_text') or '')[:180])
