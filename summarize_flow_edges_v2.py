"""Produce summary stats for new_cobol_flow_edges_v2: counts, distribution by kind/subkind, top caller fanout."""
import os, json, argparse, requests, collections
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


def page(ep,key, skip):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':1000,'skip':skip,'select':'program_id,caller_para,target_para,edge_kind,edge_subkind'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(r.text[:200])
    js=r.json(); return js.get('value',[])


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--limit',type=int,help='Max docs to scan (multiple of 1000)')
    ap.add_argument('--top-callers',type=int,default=15)
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    kind=collections.Counter(); subkind=collections.Counter(); caller_fanout=collections.defaultdict(set); program_counts=collections.Counter()
    scanned=0; skip=0
    while True:
        rows=page(ep,key,skip)
        if not rows: break
        for r in rows:
            k=(r.get('edge_kind') or 'UNK').lower(); sk=(r.get('edge_subkind') or '').lower() or 'none'
            kind[k]+=1; subkind[sk]+=1
            caller=r.get('caller_para'); tgt=r.get('target_para') or r.get('resolved_target_para')
            if caller and tgt:
                caller_fanout[caller].add(tgt)
            pid=r.get('program_id') or 'UNK'
            program_counts[pid]+=1
        scanned+=len(rows)
        skip+=len(rows)
        if args.limit and scanned>=args.limit: break
        if len(rows)<1000: break
    print(f"Scanned edges: {scanned}")
    print("By kind:")
    for k,c in kind.most_common():
        print(f"  {k}: {c}")
    print("By subkind:")
    for k,c in subkind.most_common():
        print(f"  {k}: {c}")
    fanout_sorted=sorted(caller_fanout.items(), key=lambda kv: len(kv[1]), reverse=True)[:args.top_callers]
    print("Top caller fanout (distinct targets):")
    for caller, tgts in fanout_sorted:
        print(f"  {caller}: {len(tgts)}")
    print(f"Programs with edges: {len(program_counts)} (avg edges/program ~ {round(scanned/max(1,len(program_counts)),2)})")

if __name__=='__main__':
    main()
