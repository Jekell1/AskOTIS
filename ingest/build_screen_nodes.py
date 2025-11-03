"""Extract SCREEN SECTION artifacts into `new_cobol_screen_nodes`.

Heuristic parser (lightweight):
  - Locate COBOL files (*.CBL / *.cbl) under source roots (default ./cobol_src or ./src if present).
  - Identify lines between 'SCREEN SECTION' and next 'PROCEDURE DIVISION' or EOF.
  - Split into screen blocks if multiple 01-level groups or FORM markers encountered.
  - Extract field candidates (level-number + name, PIC clause) -> fields_json.
  - Extract action lines (ACCEPT, DISPLAY, ADD, DELETE, ENTER, PF keys) -> actions_json (dedup).
  - Extract transitions (CALL 'PROG') -> transitions_json.
  - Compose summary_text with program/screen, field count, sample actions & transitions.

Success criterion support: For UI-heavy programs (role UI/DISPATCH or name contains MENU/SCREEN),
fields_json & actions_json should be non-empty if SCREEN SECTION present.

Usage:
  python ingest/build_screen_nodes.py --push
"""
from __future__ import annotations
import os, sys, re, json, argparse, datetime, hashlib, requests
from typing import List, Dict, Any

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_screen_nodes'

FIELD_RE=re.compile(r'^\s*(\d{2})\s+([A-Z0-9][A-Z0-9-]*)')
PIC_RE=re.compile(r'PIC\s+([A-Z9XV\(\)0-9]+)',re.IGNORECASE)
CALL_RE=re.compile(r'CALL\s+["\']([A-Z0-9_#$-]+)["\']',re.IGNORECASE)
ACTION_RE=re.compile(r'\b(ACCEPT|DISPLAY|ENTER|PROMPT|VALIDATE|ADD|DELETE|UPDATE)\b',re.IGNORECASE)
PF_RE=re.compile(r'PF\d{1,2}',re.IGNORECASE)

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: print('[FATAL] Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def find_source_roots()->List[str]:
    candidates=['cobol_src','src','source']
    out=[]
    for c in candidates:
        if os.path.isdir(c): out.append(c)
    return out or ['.']

def list_cobol_files()->List[str]:
    roots=find_source_roots(); files=[]
    for r in roots:
        for root,_,fns in os.walk(r):
            for fn in fns:
                if fn.lower().endswith(('.cbl','.cob')):
                    files.append(os.path.join(root,fn))
    return files

def split_screen_sections(text:str)->List[str]:
    up=text.upper()
    if 'SCREEN SECTION' not in up:
        return []
    # naive slice from SCREEN SECTION to PROCEDURE DIVISION or EOF
    start=up.find('SCREEN SECTION')
    proc_idx=up.find('PROCEDURE DIVISION', start)
    chunk=text[start: proc_idx if proc_idx!=-1 else len(text)]
    return [chunk]

def extract_blocks(section:str)->List[str]:
    # split by 01  or 77 top-level declarations heuristically
    lines=section.splitlines()
    blocks=[]; cur=[]
    for ln in lines:
        if re.match(r'^\s*0?1\s',ln) and cur:
            blocks.append('\n'.join(cur)); cur=[ln]
        else:
            cur.append(ln)
    if cur: blocks.append('\n'.join(cur))
    return blocks

def parse_block(block:str, program_id:str, idx:int)->Dict[str,Any]:
    fields=[]; actions=set(); transitions=set()
    for ln in block.splitlines():
        m=FIELD_RE.match(ln.upper())
        if m:
            lvl,name=m.groups();
            pic=PIC_RE.search(ln.upper())
            fields.append({'level':lvl,'name':name,'pic':pic.group(1) if pic else None})
        for act in ACTION_RE.findall(ln):
            actions.add(act.upper())
        for pf in PF_RE.findall(ln):
            actions.add(pf.upper())
        for c in CALL_RE.findall(ln.upper()):
            transitions.add(c.upper())
    screen_name=None
    if fields:
        screen_name=fields[0]['name'] if fields[0]['level'] in ('01','1') else f"SCREEN{idx+1}"
    # Normalize screen ID for Azure Search key safety
    screen_id=f"{program_id}_SCR{idx+1}"
    raw_span_text=block[:64000]
    summary_parts=[f"Program {program_id} screen {screen_id}"]
    if screen_name: summary_parts.append(f"name {screen_name}")
    if fields: summary_parts.append(f"fields {len(fields)}")
    if actions: summary_parts.append(f"actions {len(actions)}")
    if transitions: summary_parts.append(f"transitions {len(transitions)} -> {', '.join(sorted(list(transitions))[:5])}")
    summary_text='; '.join(summary_parts)[:4000]
    doc={
        'screen_id':screen_id,
        'program_id':program_id,
        'screen_name':screen_name or '',
        'fields_json':json.dumps(fields)[:64000],
        'actions_json':json.dumps(sorted(list(actions)))[:64000],
        'transitions_json':json.dumps(sorted(list(transitions)))[:64000],
        'raw_span_text':raw_span_text,
        'summary_text':summary_text,
        'field_count':len(fields),
        'action_count':len(actions),
        'transition_count':len(transitions),
        'has_vector':False
    }
    return doc

def build_docs()->List[Dict[str,Any]]:
    docs=[]
    for fp in list_cobol_files():
        try:
            with open(fp,'r',encoding='utf-8',errors='ignore') as f: txt=f.read()
        except Exception: continue
        program_id=os.path.splitext(os.path.basename(fp))[0].upper()
        sections=split_screen_sections(txt)
        for sec in sections:
            blocks=extract_blocks(sec)
            for i,blk in enumerate(blocks):
                doc=parse_block(blk,program_id,i)
                docs.append(doc)
    return docs

def upload(ep,key,docs:List[Dict[str,Any]]):
    if not docs: print('No screen docs'); return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    ts=datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    for d in docs: d['generated_at']=ts
    actions=[{'@search.action':'mergeOrUpload', **d} for d in docs]
    for i in range(0,len(actions),500):
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':actions[i:i+500]})
        if r.status_code!=200:
            print('[ERROR] upload chunk',r.status_code,r.text[:200]); break
    print('Uploaded',len(docs),'screen node docs')

def main():
    ap=argparse.ArgumentParser(description='Build screen nodes index docs')
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--export-json')
    args=ap.parse_args(); load_local(); ep,key=resolve()
    docs=build_docs()
    print(f"Extracted {len(docs)} screen node docs")
    if args.export_json:
        with open(args.export_json,'w',encoding='utf-8') as f: json.dump(docs,f,indent=2)
    if args.push:
        upload(ep,key,docs)

if __name__=='__main__':
    main()
