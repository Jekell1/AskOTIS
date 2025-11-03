"""Validate that paragraph names referenced in symbol refs appear in program flow graph Mermaid.

Approach (heuristic):
  1. Fetch all symbol refs for a given --program-id (or sample refs if --limit provided)
  2. Collect distinct non-empty paragraph_name values.
  3. Fetch the program's flow doc from new_cobol_program_flows.
  4. For each paragraph name, check substring presence in mermaid_flow (or condensed)
     (Future improvement: parse Mermaid graph formally.)
  5. Report missing paragraphs and coverage percentage.

Usage:
  python validate_paragraph_flow_consistency.py --program-id CLAIMUPD
  python validate_paragraph_flow_consistency.py --program-id CLAIMUPD --limit 200
"""
from __future__ import annotations
import os, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
REF_INDEX='new_cobol_symbol_refs'
FLOW_INDEX='new_cobol_program_flows'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch_refs(ep,key,program_id,limit):
    url=f"{ep}/indexes/{REF_INDEX}/docs/search?api-version={API_VERSION}"
    filt=f"program_id eq '{program_id}'"
    page=1000; skip=0; out=[]
    while True:
        body={'search':'*','filter':filt,'top':page,'skip':skip,'select':'paragraph_name'}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200: raise SystemExit(f"Fetch refs failed {r.status_code}: {r.text[:200]}")
        rows=r.json().get('value',[])
        if not rows: break
        skip+=len(rows)
        out.extend(rows)
        if limit and len(out)>=limit: break
        if len(rows)<page: break
        if skip>=100000: break
    return out

def fetch_flow(ep,key,program_id):
    url=f"{ep}/indexes/{FLOW_INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','filter':f"program_id eq '{program_id}'",'top':1,'select':'program_id,mermaid_flow,condensed_mermaid'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(f"Fetch flow failed {r.status_code}: {r.text[:200]}")
    vals=r.json().get('value',[])
    return vals[0] if vals else {}

def main():
    ap=argparse.ArgumentParser(description='Paragraph to flow consistency validator')
    ap.add_argument('--program-id',required=True)
    ap.add_argument('--limit',type=int,default=0,help='Limit number of refs to sample (0=all)')
    args=ap.parse_args(); load(); ep,key=resolve()
    pid=args.program_id.upper()
    refs=fetch_refs(ep,key,pid,args.limit)
    paragraphs=sorted({(r.get('paragraph_name') or '').upper() for r in refs if r.get('paragraph_name')})
    flow=fetch_flow(ep,key,pid)
    mermaid=(flow.get('mermaid_flow') or '') + '\n' + (flow.get('condensed_mermaid') or '')
    missing=[]; present=[]
    for para in paragraphs:
        if para and para in mermaid:
            present.append(para)
        else:
            missing.append(para)
    coverage=(len(present)/len(paragraphs)*100.0) if paragraphs else 0.0
    report={
        'program_id':pid,
        'paragraphs_detected':len(paragraphs),
        'paragraphs_present_in_flow':len(present),
        'paragraphs_missing_in_flow':len(missing),
        'coverage_pct':round(coverage,2),
        'missing_list':missing[:200]
    }
    print(json.dumps(report,indent=2))

if __name__=='__main__':
    main()
