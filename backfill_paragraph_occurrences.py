"""Backfill canonical_occurrences / alias_occurrences for paragraph aliases that currently have zero.

Strategy:
 1. Scan source tree for paragraph labels (programs + copybooks optional) and build occurrence map.
 2. Query alias index for PARAGRAPH rows where canonical_occurrences == 0.
 3. For each affected canonical, update all its alias rows with the computed occurrence (if >0).

Usage:
  python backfill_paragraph_occurrences.py --source-root cobol_src --include-copybooks --strict-copybook-verbs --commit
"""
from __future__ import annotations
import os, json, argparse, pathlib, re, time, requests, hashlib, sys
from collections import Counter, defaultdict

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_name_aliases'
PARA_LABEL_RE=re.compile(r'^[0-9 ]{0,6}[ \t]*([A-Z0-9][A-Z0-9-]{1,63})\.$')
COBOL_VERB_RE=re.compile(r'\b(PERFORM|MOVE|IF|EVALUATE|DISPLAY|CALL|OPEN|READ|WRITE|CLOSE|ADD|SUBTRACT|COMPUTE|ACCEPT|EXEC)\b',re.IGNORECASE)
RESERVED_LABELS={'IDENTIFICATION','ENVIRONMENT','CONFIGURATION','DATA','PROCEDURE','WORKING-STORAGE','LOCAL-STORAGE','LINKAGE','FILE','INPUT-OUTPUT','SPECIAL-NAMES','AUTHOR','DATE','SECURITY','INSTALLATION','PROGRAM-ID','FUNCTION-ID','END','DECLARATIVES','END-DECLARATIVES','COPY','EXEC','GLOBAL-STORAGE'}

BATCH=800

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def scan(root:str, include_copybooks:bool, strict_copybook_verbs:bool):
    counts=Counter()
    for fp in pathlib.Path(root).rglob('*'):
        ext=fp.suffix.lower()
        if ext in {'.cbl','.cob'} or (include_copybooks and ext=='.cpy'):
            try:
                txt=fp.read_text(encoding='utf-8',errors='ignore')
            except Exception:
                continue
            lines=txt.splitlines()
            for i,line in enumerate(lines):
                m=PARA_LABEL_RE.match(line.rstrip())
                if not m: continue
                raw=m.group(1).upper()
                if raw in RESERVED_LABELS: continue
                if raw.isdigit():
                    continue
                if ext=='.cpy' and strict_copybook_verbs:
                    window=' '.join(lines[i+1:i+7])
                    if not COBOL_VERB_RE.search(window):
                        continue
                counts[raw]+=1
    return counts

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print('[ERROR] search failed',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def fetch_zero_paragraph_aliases(ep,key):
    # We fetch all paragraph aliases, then filter those whose canonical_occurrences == 0.
    skip=0; page=3000
    by_canon=defaultdict(list)
    while True:
        body={'search':'*','top':page,'skip':skip,'filter':"kind eq 'PARAGRAPH'",'select':'alias_id,canonical_name,alias,canonical_occurrences,alias_occurrences'}
        data=search(ep,key,body)
        rows=data.get('value',[])
        if not rows: break
        for r in rows:
            by_canon[r['canonical_name']].append(r)
        skip+=len(rows)
        if len(rows)<page: break
    zero={canon:rows for canon,rows in by_canon.items() if all((r.get('canonical_occurrences') or 0)==0 for r in rows)}
    return zero

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    for i in range(0,len(docs),BATCH):
        chunk=docs[i:i+BATCH]
        payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in chunk]}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=120)
        if r.status_code not in (200,201):
            print('[ERROR] upload failed',r.status_code,r.text[:250])
        else:
            print(f"[UPLOAD] +{len(chunk)} / {len(docs)}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--source-root',default='cobol_src')
    ap.add_argument('--include-copybooks',action='store_true')
    ap.add_argument('--strict-copybook-verbs',action='store_true')
    ap.add_argument('--commit',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    print('[SCAN] building occurrence map...')
    counts=scan(args.source_root,args.include_copybooks,args.strict_copybook_verbs)
    print(f"[SCAN] paragraphs discovered with occurrences={len(counts)}")
    zero=fetch_zero_paragraph_aliases(ep,key)
    print(f"[INDEX] paragraph canonicals with zero occurrences currently: {len(zero)}")
    update_docs=[]; ts=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime())
    for canon, rows in zero.items():
        occ=counts.get(canon,0)
        if occ==0: continue
        for r in rows:
            # Update both canonical_occurrences and alias_occurrences if alias equals canonical
            alias=r['alias']
            update_docs.append({
                'alias_id': r['alias_id'],
                'canonical_occurrences': occ,
                'alias_occurrences': occ if alias==canon else 0,
                'ingested_at': ts
            })
    print(f"[PLAN] docs to update = {len(update_docs)}")
    if not args.commit:
        sample=update_docs[:5]
        print(json.dumps({'sample':sample},indent=2))
        print('Run with --commit to apply updates.')
        return
    upload(ep,key,update_docs)
    print('Done backfill.')

if __name__=='__main__':
    main()
