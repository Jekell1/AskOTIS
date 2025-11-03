"""Build a unified full menu tree per root menu program.

Outputs one document per root (likely MENU / DISPATCH UI hubs) with fields:
  root_program_id: <root>
  tree_json: JSON string representing hierarchical structure:
      {
        "program_id": "ROOT",
        "role": "UI",
        "children": [ { "program_id": ..., "role": ..., "children": [...] } ],
        "depth": 0,
        "ui": true,
        "terminal": false
      }
  total_nodes: count of unique nodes included
  total_ui_nodes: count of nodes classified UI
  max_depth: deepest depth discovered
  generated_at: timestamp
  build_params_json: parameters used

Discovery rules:
  - Start from candidate roots (UI / DISPATCH / *MENU* tokens) limited by --limit-roots
  - DFS with cycle avoidance
  - Non-UI expansion allowed but can be limited by --non-ui-hop-budget (consecutive budget)
  - Merges repeated subtrees (shared nodes) by reference; final materialization uses set to prevent infinite loops
  - Optionally only include branches that lead to at least one UI descendant (prune-dead)

Index (optional) target: new_cobol_menu_trees
"""
import os, json, sys, argparse, datetime, math, requests, collections, re
from typing import Dict, Any, List, Set, Tuple

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_META_INDEX='new_cobol_program_meta'
CALL_INDEX='new_cobol_calls'
MENU_TREE_INDEX='new_cobol_menu_trees'

UI_TOKENS=['MENU','SCR','SCRN','PAGE','PANEL','FORM','SCREEN','INQ','MAINT','LIST']

# ---------------- env helpers ----------------

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

# ---------------- search utilities ----------------

def search_all(ep,key,index,select=None):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'search':'*','top':1000,'queryType':'simple'}
    if select: payload['select']=select
    out=[]; skip=0
    while True:
        payload['skip']=skip
        r=requests.post(url,headers=headers,json=payload)
        if r.status_code!=200:
            print('ERROR search',index,r.status_code,r.text[:200])
            break
        data=r.json()
        batch=data.get('value',[])
        out.extend(batch)
        if len(batch)<payload['top']: break
        skip+=payload['top']
    return out

# ---------------- heuristics ----------------

def is_ui(p: Dict[str,Any]) -> bool:
    if not p: return False
    if p.get('ui_flag') or p.get('ui_path_participant'): return True
    role=(p.get('program_role') or '').upper()
    if role in ('UI','SCREEN','DISPATCH'): return True
    name=(p.get('program_id') or '').upper()
    if any(tok in name for tok in UI_TOKENS): return True
    summary=(p.get('program_summary') or '').upper()
    if any(tok in summary for tok in UI_TOKENS): return True
    return False

# ---------------- graph build ----------------

def build_outgoing(calls: List[Dict[str,Any]]) -> Dict[str,Set[str]]:
    out=collections.defaultdict(set)
    for c in calls:
        src=(c.get('caller_program') or c.get('caller_program_id') or c.get('caller') or '').upper()
        dst=(c.get('callee_program') or c.get('callee_program_id') or c.get('callee') or '').upper()
        if src and dst and src!=dst:
            out[src].add(dst)
    return out

# ---------------- root selection ----------------

def select_roots(programs: List[Dict[str,Any]], limit:int) -> List[str]:
    # Score roots by out-degree (approx) and UI classification; prefer MENU names
    scored=[]
    for p in programs:
        pid=p.get('program_id');
        if not pid: continue
        ui=is_ui(p)
        name=pid.upper()
        if ui and ('MENU' in name or p.get('program_role') in ('DISPATCH','UI')):
            score=(p.get('outgoing_count') or 0, p.get('centrality_score') or 0.0)
            scored.append((score,pid))
    scored.sort(reverse=True)
    return [pid for _,pid in scored[:limit]]

def enhanced_select_roots(programs, mode:str, limit:int, prefix:str|None, pattern:str|None) -> List[str]:
    prog_ids=[p.get('program_id') for p in programs if p.get('program_id')]
    prog_map={p.get('program_id'):p for p in programs if p.get('program_id')}
    def passes(p):
        pid=p.get('program_id')
        if not pid: return False
        if prefix and not pid.upper().startswith(prefix.upper()):
            return False
        if pattern and not re.search(pattern, pid, re.IGNORECASE):
            return False
        return True
    if mode=='all':
        # include every program that passes filters
        roots=[pid for pid in prog_ids if passes({'program_id':pid})]
        return roots[:limit] if limit>0 else roots
    # fallback to previous logic for menu roots
    root_candidates=[]
    for p in programs:
        pid=p.get('program_id')
        if not pid: continue
        if not passes(p):
            continue
        if is_ui(p) and ('MENU' in pid.upper() or (p.get('program_role') in ('DISPATCH','UI'))):
            score=(p.get('outgoing_count') or 0, p.get('centrality_score') or 0.0)
            root_candidates.append((score,pid))
    root_candidates.sort(reverse=True)
    return [pid for _,pid in root_candidates[:limit]]

# ---------------- tree construction ----------------

def build_tree_for_root(root: str, prog_map: Dict[str,Dict[str,Any]], outgoing: Dict[str,Set[str]], non_ui_budget:int, max_depth:int, prune_dead:bool) -> Dict[str,Any]:
    visited=set()  # prevent infinite cycles

    def dfs(node_id:str, depth:int, remaining_budget:int) -> Dict[str,Any]:
        p=prog_map.get(node_id, {})
        node_ui=is_ui(p)
        ref={'program_id':node_id,'role':p.get('program_role'),'ui':node_ui,'depth':depth,'children':[],'terminal':False}
        if depth>=max_depth:
            ref['terminal']=True
            return ref
        children=list(outgoing.get(node_id,[]))
        if not children:
            ref['terminal']=True
            return ref
        # Partition children
        ui_children=[c for c in children if is_ui(prog_map.get(c,{}))]
        non_ui_children=[c for c in children if c not in ui_children]
        expand=[]
        # Always add UI children
        expand.extend(ui_children)
        # Add controlled non-UI children
        if non_ui_children and remaining_budget>0:
            # heuristic: allow up to remaining_budget non-ui
            expand.extend(non_ui_children[:remaining_budget])
        sub_any=False
        for ch in expand:
            key=(ch, depth+1)
            # avoid re-expanding same (node,depth) pair to limit bloat
            if key in visited: continue
            visited.add(key)
            next_budget=remaining_budget - (0 if is_ui(prog_map.get(ch,{})) else 1)
            if next_budget<0: continue
            child_ref=dfs(ch, depth+1, next_budget)
            # prune if requested and subtree has no UI descendant
            if prune_dead and not child_ref['ui'] and not child_ref.get('children') and not child_ref['terminal']:
                continue
            ref['children'].append(child_ref)
            sub_any=True
        if not sub_any:
            ref['terminal']=True
        return ref

    return dfs(root,0,non_ui_budget)

# ---------------- metrics & upload ----------------

def collect_metrics(tree: Dict[str,Any]) -> Tuple[int,int,int]:
    max_depth=0
    total=0
    ui_total=0
    stack=[tree]
    while stack:
        n=stack.pop()
        total+=1
        if n.get('ui'): ui_total+=1
        d=n.get('depth',0)
        if d>max_depth: max_depth=d
        for ch in n.get('children',[]): stack.append(ch)
    return total, ui_total, max_depth

def upload(ep,key,docs):
    if not docs:
        print('No docs to upload');return
    url=f"{ep}/indexes/{MENU_TREE_INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers=headers,json=payload)
    if r.status_code!=200:
        print('Upload error',r.status_code,r.text[:300])
    else:
        print('Uploaded',len(docs),'menu tree docs')

# ---------------- index management ----------------

MENU_TREE_SCHEMA = {
  "name": MENU_TREE_INDEX,
  "fields": [
    {"name":"root_program_id","type":"Edm.String","key":True,"filterable":True,"sortable":True,"facetable":False,"searchable":False},
    {"name":"tree_json","type":"Edm.String","searchable":False},
    {"name":"total_nodes","type":"Edm.Int32","filterable":True,"sortable":True},
    {"name":"total_ui_nodes","type":"Edm.Int32","filterable":True,"sortable":True},
    {"name":"ui_ratio","type":"Edm.Double","filterable":True,"sortable":True},
    {"name":"max_depth","type":"Edm.Int32","filterable":True,"sortable":True},
    {"name":"build_params_json","type":"Edm.String","searchable":False},
    {"name":"generated_at","type":"Edm.String","filterable":True,"sortable":True},
    {"name":"doc_type","type":"Edm.String","filterable":True,"sortable":True}
  ]
}

def ensure_index(ep,key):
    url=f"{ep}/indexes/{MENU_TREE_INDEX}?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    r=requests.get(url,headers=headers)
    if r.status_code==200:
        return True
    if r.status_code==404:
        print('Creating index',MENU_TREE_INDEX)
        c=requests.put(f"{ep}/indexes/{MENU_TREE_INDEX}?api-version={API_VERSION}",headers=headers,json=MENU_TREE_SCHEMA)
        if c.status_code!=201:
            print('Index creation failed',c.status_code,c.text[:300])
            return False
        return True
    print('Unexpected index get status',r.status_code,r.text[:200])
    return False

# ---------------- main ----------------

def main():
    ap=argparse.ArgumentParser(description='Build full menu trees')
    ap.add_argument('--limit-roots',type=int,default=20)
    ap.add_argument('--non-ui-hop-budget',type=int,default=3)
    ap.add_argument('--max-depth',type=int,default=20)
    ap.add_argument('--prune-dead',action='store_true',help='Prune branches with no UI descendant')
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--export-json',help='Write docs to JSON file')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--root-mode',choices=['menu','all'],default='menu')
    ap.add_argument('--root-prefix',help='Restrict roots to program IDs starting with this prefix')
    ap.add_argument('--root-pattern',help='Regex to filter root IDs (case-insensitive)')
    args=ap.parse_args()
    load_local_settings()
    ep,key=resolve()
    programs=search_all(ep,key,PROGRAM_META_INDEX)
    calls=search_all(ep,key,CALL_INDEX,select='caller_program,callee_program')
    prog_map={p.get('program_id'):p for p in programs if p.get('program_id')}
    outgoing=build_outgoing(calls)
    roots=enhanced_select_roots(programs,args.root_mode,args.limit_roots,args.root_prefix,args.root_pattern)
    docs=[]
    ts=datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    for r in roots:
        tree=build_tree_for_root(r,prog_map,outgoing,args.non_ui_hop_budget,args.max_depth,args.prune_dead)
        total,ui_total,max_depth=collect_metrics(tree)
        doc={
            'root_program_id': r,
            'tree_json': json.dumps(tree),
            'total_nodes': total,
            'total_ui_nodes': ui_total,
            'max_depth': max_depth,
            'ui_ratio': round(ui_total/total,4) if total else 0,
            'generated_at': ts,
            'build_params_json': json.dumps({
                'non_ui_hop_budget': args.non_ui_hop_budget,
                'max_depth': args.max_depth,
                'prune_dead': args.prune_dead
            }),
            'doc_type': 'menu_tree'
        }
        docs.append(doc)
    diag={
        'roots_considered': len(roots),
        'root_mode': args.root_mode,
        'filters': {'prefix': args.root_prefix, 'pattern': args.root_pattern},
        'sample_root': roots[:5],
        'doc_count': len(docs)
    }
    print(json.dumps(diag,indent=2))
    if args.export_json:
        with open(args.export_json,'w',encoding='utf-8') as f:
            json.dump(docs,f,ensure_ascii=False,indent=2)
        print('Exported',len(docs),'docs to',args.export_json)
    if args.dry_run:
        # show first tree truncated
        if docs:
            print('\nExample tree snippet:')
            tree=json.loads(docs[0]['tree_json'])
            def walk(n,depth=0):
                if depth>4: return
                print('  '*depth + f"- {n['program_id']} (ui={n['ui']}, children={len(n.get('children',[]))})")
                for ch in n.get('children',[])[:5]:
                    walk(ch,depth+1)
            walk(tree)
        return
    if args.push:
        if ensure_index(ep,key):
            upload(ep,key,docs)
        else:
            print('Skipping upload due to index creation failure')

if __name__=='__main__':
    main()
