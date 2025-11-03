"""Harvest and populate context_before/context_after for symbol reference docs.

Rationale:
  The initial extraction populated excerpt only. For richer grounding we
  capture a configurable window of surrounding lines (before and after)
  from the original COBOL source file. This script incrementally updates
  only documents where context_before or context_after is still empty.

Strategy:
  1. Scan symbol refs in pages where (context_before eq '' or context_after eq '').
     (Azure Search filter can't test length directly; we treat empty string or null.)
  2. For each doc, open its file_path (cached) and extract window of N lines
     before and after the line_number (exclusive of the focal line) and join
     them with newlines. We do NOT include the focal line itself (already in excerpt).
  3. Upload updates via merge batches.

Usage:
  python harvest_symbol_ref_context.py --window 3
  python harvest_symbol_ref_context.py --window 5 --limit 50000 --batch 400

Edge Cases:
  * Missing file_path or unreadable file -> skip context (leave empty)
  * Line number out of bounds -> skip gracefully
  * Very large files: only read once (cache by absolute path)

"""
from __future__ import annotations
import os, json, argparse, time, requests, io
from typing import Dict, List

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key environment variables.')
    return ep.rstrip('/'), key

def fetch_page(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    # We treat empty or null context fields as needing harvesting.
    # Azure Search null comparison: field eq null.
    filt="(context_before eq '') or (context_before eq null) or (context_after eq '') or (context_after eq null)"
    body={
        'search':'*',
        'filter':filt,
        'top':top,
        'skip':skip,
        'select':'ref_id,file_path,line_number,context_before,context_after'
    }
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Fetch failed {r.status_code}: {r.text[:300]}")
    return r.json().get('value',[])

def upload(ep,key,docs:List[Dict]):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    for d in docs: d['@search.action']='merge'
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs},timeout=60)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")

_FILE_CACHE: Dict[str, List[str]]={}

def get_lines(path:str)->List[str]:
    if not path:
        return []
    if path in _FILE_CACHE:
        return _FILE_CACHE[path]
    try:
        txt=open(path,'r',encoding='utf-8',errors='ignore').read()
        # Normalize CRLF just in case
        lines=txt.splitlines()
        _FILE_CACHE[path]=lines
        return lines
    except Exception:
        _FILE_CACHE[path]=[]
        return []

def harvest_context(lines:List[str], line_number:int, window:int):
    if not lines or line_number<=0:
        return '',''
    n=len(lines)
    idx=line_number-1
    if idx<0 or idx>=n:
        return '',''
    start_before=max(0, idx-window)
    before=lines[start_before:idx]
    end_after=min(n, idx+1+window)
    after=lines[idx+1:end_after]
    # Trim whitespace and join
    before_txt='\n'.join(l.rstrip() for l in before).strip()
    after_txt='\n'.join(l.rstrip() for l in after).strip()
    # Safety truncate
    if len(before_txt)>800: before_txt=before_txt[:800]
    if len(after_txt)>800: after_txt=after_txt[:800]
    return before_txt, after_txt

def main():
    ap=argparse.ArgumentParser(description='Populate context_before/context_after for symbol refs')
    ap.add_argument('--window',type=int,default=3,help='Number of surrounding lines before and after (excluding focal line).')
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--limit',type=int,default=0,help='Max docs to scan (0 = all)')
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    skip=0; scanned=0; updated=0; start=time.time(); pending=[]
    while True:
        if args.limit and scanned>=args.limit: break
        page=fetch_page(ep,key,skip,args.batch)
        if not page: break
        skip+=len(page)
        for doc in page:
            if args.limit and scanned>=args.limit: break
            scanned+=1
            ref_id=doc.get('ref_id')
            fp=doc.get('file_path') or ''
            ln=doc.get('line_number') or 0
            cb=(doc.get('context_before') or '')
            ca=(doc.get('context_after') or '')
            if cb and ca:
                continue  # already populated
            lines=get_lines(fp)
            before_txt, after_txt=harvest_context(lines, ln, args.window)
            if before_txt or after_txt:
                if args.dry_run:
                    updated+=1
                else:
                    pending.append({'ref_id':ref_id,'context_before':before_txt,'context_after':after_txt})
            if len(pending)>=args.batch:
                upload(ep,key,pending); updated+=len(pending); pending.clear()
        if len(page)<args.batch:
            break
        if scanned % (args.batch*10)==0:
            print(f"Progress scanned={scanned} updated={updated}")
    if pending and not args.dry_run:
        upload(ep,key,pending); updated+=len(pending); pending.clear()
    dur=time.time()-start
    print(f"Done scanned={scanned} updated={updated} dry_run={args.dry_run} in {dur:.1f}s window={args.window}")

if __name__=='__main__':
    main()
