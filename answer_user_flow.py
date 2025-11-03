import os, json, sys, argparse, requests, math
from typing import List, Dict, Any
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_META_INDEX='new_cobol_program_meta'
UI_PATHS_INDEX='new_cobol_ui_paths'

# --- helpers ---

def load_local_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key');sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,index,filter_expr=None,select=None,top=50):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'search':'*','top':top,'count':True}
    if filter_expr:
        payload['filter']=filter_expr
    if select:
        payload['select']=select
    r=requests.post(url,headers=headers,json=payload)
    if r.status_code!=200:
        raise RuntimeError(f"Search {index} {r.status_code}: {r.text[:200]}")
    return r.json()

def fetch_ui_paths(ep,key,root,limit, min_length):
    # Filter by root and min length (Azure OData filter on numeric length)
    # length ge <min_length> AND root_program_id eq '<root>'
    filt=f"root_program_id eq '{root}' and length ge {min_length}" if min_length>1 else f"root_program_id eq '{root}'"
    data=search(ep,key,UI_PATHS_INDEX,filt,top=limit)
    docs=data.get('value',[])
    # sort by score desc then length desc
    docs.sort(key=lambda d:(d.get('score',0), d.get('length',0)), reverse=True)
    return docs

def fetch_program_meta(ep,key,ids:List[str]):
    if not ids: return {}
    # simple repeated lookups (could batch w/ search.in but keep simple)
    result={}
    for pid in ids[:512]:
        filt=f"program_id eq '{pid}'"
        data=search(ep,key,PROGRAM_META_INDEX,filt,top=1)
        if data.get('value'):
            result[pid]=data['value'][0]
    return result

def classify(p:Dict[str,Any]):
    if not p: return 'unknown'
    role=(p.get('program_role') or '').upper()
    name=(p.get('program_id') or '').upper()
    tokens=['MENU','SCR','SCRN','PAGE','PANEL','FORM','SCREEN','INQ','MAINT','LIST']
    if role in ('UI','SCREEN','DISPATCH') or any(t in name for t in tokens):
        return 'ui'
    return 'logic'

def build_branch_tree(paths:List[Dict[str,Any]]):
    # Convert list of sequences into a nested tree {id:{children:..., terminal:bool}}
    root_nodes={}
    tree={'_root':{'children':{},'count':0}}
    for p in paths:
        seq=json.loads(p.get('program_sequence_json','[]'))
        node=tree['_root']
        for idx,prog in enumerate(seq):
            node['count']=node.get('count',0)+1
            children=node.setdefault('children',{})
            if prog not in children:
                children[prog]={'children':{},'count':0,'terminal':False}
            node=children[prog]
            if idx==len(seq)-1:
                node['terminal']=True
    return tree['_root']

def render_tree(tree, meta_map, indent=''):
    lines=[]
    children=tree.get('children',{})
    for prog, data in children.items():
        meta=meta_map.get(prog,{})
        cls=classify(meta)
        role=meta.get('program_role')
        summ=(meta.get('program_summary') or '')[:70]
        tag='[UI]' if cls=='ui' else '[LOG]'
        leaf=' (leaf)' if data.get('terminal') else ''
        lines.append(f"{indent}{tag} {prog}{leaf} - role={role} {summ}")
        if data.get('children'):
            lines.extend(render_tree(data, meta_map, indent+'  '))
    return lines

def main():
    ap=argparse.ArgumentParser(description='Answer: complete user screen flow')
    ap.add_argument('--root','-r',required=True,help='Root menu program id (e.g. PGMENU)')
    ap.add_argument('--min-length',type=int,default=3)
    ap.add_argument('--limit-paths',type=int,default=200)
    ap.add_argument('--top-paths',type=int,default=5,help='Show top N linear paths')
    ap.add_argument('--tree',action='store_true',help='Render merged branch tree')
    args=ap.parse_args()
    load_local_settings()
    ep,key=resolve()
    paths=fetch_ui_paths(ep,key,args.root.upper(),args.limit_paths,args.min_length)
    if not paths:
        print(f'No precomputed paths found for root {args.root}')
        sys.exit(0)
    # collect meta
    all_ids=set()
    for p in paths[:args.top_paths]:
        for prog in json.loads(p.get('program_sequence_json','[]')):
            all_ids.add(prog)
    meta_map=fetch_program_meta(ep,key,sorted(all_ids))
    print(f"Top {min(args.top_paths,len(paths))} paths for root {args.root.upper()} (filtered length >= {args.min_length}):\n")
    for i,p in enumerate(paths[:args.top_paths],1):
        seq=json.loads(p.get('program_sequence_json','[]'))
        stages=[]
        for prog in seq:
            cls=classify(meta_map.get(prog,{}))
            stages.append(f"{prog}({'UI' if cls=='ui' else 'LOG'})")
        print(f"[{i}] score={p.get('score')} len={p.get('length')} ui={p.get('ui_program_count')} avgFreq={p.get('edge_avg_call_frequency')} -> ")
        print('    ' + ' -> '.join(stages))
    if args.tree:
        tree=build_branch_tree(paths[:args.limit_paths])
        print('\nMerged Branch Tree:')
        lines=render_tree(tree, meta_map)
        for line in lines:
            print(line)

if __name__=='__main__':
    main()
