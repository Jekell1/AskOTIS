"""Extract paragraph labels directly from COBOL sources and add any missing PARAGRAPH aliases.

Why: Some environments produce zero paragraph_name values in new_cobol_copybook_usage (heuristic missed). This script rescans
raw source files to ensure paragraph aliases exist for retrieval and canonicalization.

Detection improvements in this version:
    * Filters out common reserved DIVISION / SECTION / STORAGE / meta labels
    * Counts occurrences per paragraph label across all sources
    * Provides verbose statistics (--stats / --debug-sample) to validate detection quality
    * More tolerant regex for sequence numbers (columns 1-6) and area A indentation

Heuristic (simplified):
    A paragraph label is a token (A-Z0-9- starting with letter/number, at least 2 chars) followed by a period on its own logical line
    and not one of the reserved words (IDENTIFICATION, ENVIRONMENT, WORKING-STORAGE, etc.).

Run dry-run:
    python extract_paragraphs_and_update_aliases.py --source-root cobol_src --stats
Commit:
    python extract_paragraphs_and_update_aliases.py --source-root cobol_src --commit --stats
Debug a handful of examples:
    python extract_paragraphs_and_update_aliases.py --source-root cobol_src --debug-sample 10
"""
from __future__ import annotations
import os, json, argparse, pathlib, re, time, hashlib, requests, itertools
from collections import defaultdict, Counter

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
ALIAS_INDEX='new_cobol_name_aliases'
CANON_RE=re.compile(r'[^A-Za-z0-9]')
PARA_LABEL_RE=re.compile(r'^[0-9 ]{0,6}[ \t]*([A-Z0-9][A-Z0-9-]{1,63})\.$')
COBOL_VERB_RE=re.compile(r'\b(PERFORM|MOVE|IF|EVALUATE|DISPLAY|CALL|OPEN|READ|WRITE|CLOSE|ADD|SUBTRACT|COMPUTE|ACCEPT|EXEC)\b',re.IGNORECASE)

# Common non-paragraph labels to exclude (case-insensitive, without trailing period)
RESERVED_LABELS={
    'IDENTIFICATION','ENVIRONMENT','CONFIGURATION','DATA','PROCEDURE','WORKING-STORAGE','LOCAL-STORAGE',
    'LINKAGE','FILE','INPUT-OUTPUT','SPECIAL-NAMES','AUTHOR','DATE','SECURITY','INSTALLATION',
    'PROGRAM-ID','FUNCTION-ID','END','DECLARATIVES','END-DECLARATIVES','COPY','EXEC','GLOBAL-STORAGE'
}

VAR_FUNCS=[lambda s:s, str.upper, str.lower, lambda s:s.replace('-', ''), lambda s:s.replace('_',''), lambda s:s.replace('-','').replace('_',''), lambda s:re.sub(r'[^A-Za-z0-9]','',s)]
VAR_NAMES=['raw','upper','lower','no_hyphen','no_underscore','collapsed','alnum']

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def canonical(s:str)->str:
    return CANON_RE.sub('',s).upper()

def fetch_existing_paragraph_canon(ep,key):
    existing=set(); skip=0; page=3000
    while True:
        body={'search':'*','top':page,'skip':skip,'filter':"kind eq 'PARAGRAPH'",'select':'canonical_name'}
        r=requests.post(f"{ep}/indexes/{ALIAS_INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200: raise SystemExit(r.text[:300])
        data=r.json(); rows=data.get('value',[])
        if not rows: break
        for row in rows:
            existing.add(row['canonical_name'])
        skip+=len(rows)
        if len(rows)<page: break
    return existing

def scan_paragraphs(root:str, stats: bool=False, debug_sample:int=0, include_copybooks: bool=False, copybook_sample:int=0, strict_copybook_verbs: bool=False):
    """Return (labels_set, occurrence_counter, debug_examples).

    occurrence_counter counts raw label (upper) appearances across files.
    debug_examples: list of tuples (file, line_number, label, line_text) limited by debug_sample.
    """
    labels=set(); occurrences=Counter(); examples=[]
    files_scanned=0; lines_scanned=0; skipped_reserved=0
    root_path=pathlib.Path(root)
    candidates=0
    cpy_seen=0
    for fp in root_path.rglob('*'):
        ext=fp.suffix.lower()
        if ext in {'.cbl','.cob'}:
            pass
        elif include_copybooks and ext=='.cpy':
            cpy_seen+=1
            if copybook_sample and cpy_seen>copybook_sample:
                continue
        else:
            continue
        files_scanned+=1
        try:
            txt=fp.read_text(encoding='utf-8',errors='ignore')
        except Exception:
            continue
        lines=txt.splitlines()
        for idx,line in enumerate(lines, start=1):
            lines_scanned+=1
            s=line.rstrip()
            m=PARA_LABEL_RE.match(s)
            if not m:
                continue
            raw=m.group(1).upper()
            candidates+=1
            if raw in RESERVED_LABELS:
                skipped_reserved+=1
                continue
            # Ignore numeric-only labels (likely sequence numbers) unless they contain a letter
            if raw.isdigit():
                skipped_reserved+=1
                continue
            # If copybook heuristics enabled, optionally require a following verb within next few lines to reduce false positives
            if ext=='.cpy' and strict_copybook_verbs:
                window=' '.join(lines[idx:idx+6])
                if not COBOL_VERB_RE.search(window):
                    skipped_reserved+=1
                    continue
            labels.add(raw)
            occurrences[raw]+=1
            if debug_sample and len(examples)<debug_sample:
                examples.append((str(fp), idx, raw, s.strip()))
    if stats:
        print(json.dumps({
            'scan_stats':{
                'files_scanned':files_scanned,
                'lines_scanned':lines_scanned,
                'candidate_labels':candidates,
                'reserved_skipped':skipped_reserved,
                'unique_paragraphs':len(labels),
                'copybooks_processed': cpy_seen if include_copybooks else 0
            }
        }, indent=2))
    return labels, occurrences, examples

def build_docs(new_labels, occurrences):
    ts=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime())
    docs=[]
    for name in sorted(new_labels):
        canon=canonical(name)
        occ=occurrences.get(name,1)
        for fn,vn in zip(VAR_FUNCS,VAR_NAMES):
            alias=fn(name)
            if not alias:
                continue
            aid=hashlib.sha1(f"{canon}|{alias}|PARAGRAPH".encode('utf-8')).hexdigest()[:40]
            docs.append({
                'alias_id':aid,
                'canonical_name':canon,
                'alias':alias,
                'variant_type':vn,
                'kind':'PARAGRAPH',
                'source_hint':'rescan_source',
                'canonical_occurrences':occ,
                'alias_occurrences':occ if alias in {name,canon} else 0,
                'ingested_at':ts
            })
        # Ensure explicit canonical variant tagged 'canonical'
        aid=hashlib.sha1(f"{canon}|{canon}|PARAGRAPH".encode('utf-8')).hexdigest()[:40]
        docs.append({
            'alias_id':aid,
            'canonical_name':canon,
            'alias':canon,
            'variant_type':'canonical',
            'kind':'PARAGRAPH',
            'source_hint':'rescan_source',
            'canonical_occurrences':occ,
            'alias_occurrences':occ,
            'ingested_at':ts
        })
    return docs

def upload(ep,key,docs):
    if not docs:
        print('Nothing to upload.')
        return
    url=f"{ep}/indexes/{ALIAS_INDEX}/docs/index?api-version={API}"
    for i in range(0,len(docs),1000):
        chunk=docs[i:i+1000]
        payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in chunk]}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=120)
        if r.status_code not in (200,201):
            print('[ERROR]',r.status_code,r.text[:200])
        else:
            print(f"[UPLOAD] +{len(chunk)} / {len(docs)}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--source-root',default=os.getenv('COBOL_SOURCE_ROOT','cobol_src'))
    ap.add_argument('--commit',action='store_true')
    ap.add_argument('--stats',action='store_true',help='Print scan statistics JSON')
    ap.add_argument('--debug-sample',type=int,default=0,help='Show N sample detections')
    ap.add_argument('--include-copybooks',action='store_true',help='Also scan .cpy files for paragraph labels (heuristic)')
    ap.add_argument('--copybook-sample',type=int,default=0,help='If >0, limit number of .cpy files processed (for quick trial)')
    ap.add_argument('--strict-copybook-verbs',action='store_true',help='Require a COBOL verb within a short lookahead window in copybooks to accept a paragraph label')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    existing=fetch_existing_paragraph_canon(ep,key)
    found, occurrences, examples=scan_paragraphs(
        args.source_root,
        stats=args.stats,
        debug_sample=args.debug_sample,
        include_copybooks=args.include_copybooks,
        copybook_sample=args.copybook_sample,
        strict_copybook_verbs=args.strict_copybook_verbs
    )
    missing=[p for p in found if canonical(p) not in existing]
    print(f"Found {len(found)} paragraph labels; {len(existing)} already in alias index; missing={len(missing)}")
    docs=build_docs(missing, occurrences)
    print(f"Prepared {len(docs)} docs for upload (paragraph aliases)")
    sample=docs[:5]
    if sample:
        print(json.dumps({'sample':sample},indent=2))
    if args.debug_sample and examples:
        print('\nDebug examples:')
        for fp, ln, lbl, text in examples:
            print(f"  {lbl}  {fp}:{ln}  :: {text}")
    if not args.commit:
        print('Run with --commit to upload new paragraph aliases.')
        return
    upload(ep,key,docs)
    print('Done.')

if __name__=='__main__':
    main()
