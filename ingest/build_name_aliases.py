"""Build and upload canonical name alias variants into new_cobol_name_aliases.

For each discovered name (PROGRAM / COPYBOOK / PARAGRAPH) we generate variants:
  raw, UPPER, lower, no_hyphen, no_underscore, collapsed (remove hyphen + underscore),
  alnum (strip non [A-Z0-9]), plus any deduplicated forms.

We derive names from existing indexes:
  * new_cobol_copybook_usage (program_id, copybook_name_plain, paragraph_name)

Run:
  python ingest/build_name_aliases.py --commit
Preview (no upload):
  python ingest/build_name_aliases.py
"""
from __future__ import annotations
import os, json, requests, re, hashlib, argparse, time
from collections import defaultdict

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
ALIAS_INDEX = 'new_cobol_name_aliases'
USAGE_INDEX = 'new_cobol_copybook_usage'
BATCH_UPLOAD = 1000

SELECT_USAGE = 'program_id,copybook_name_plain,paragraph_name'

VARIANT_FUNCS = []

def variant(func):
    VARIANT_FUNCS.append(func)
    return func

@variant
def v_raw(s: str):
    return s

@variant
def v_upper(s: str):
    return s.upper()

@variant
def v_lower(s: str):
    return s.lower()

@variant
def v_no_hyphen(s: str):
    return s.replace('-', '')

@variant
def v_no_underscore(s: str):
    return s.replace('_', '')

@variant
def v_collapsed(s: str):
    return s.replace('-', '').replace('_','')

@variant
def v_alnum(s: str):
    return re.sub(r'[^A-Za-z0-9]', '', s)

# Canonicalization function used for alias grouping
CANON_RE = re.compile(r'[^A-Za-z0-9]')

def canonical(s: str) -> str:
    return CANON_RE.sub('', s).upper()

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
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search(ep,key,index,body):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        raise SystemExit(r.text[:500])
    return r.json()

def iter_usage(ep,key):
    skip=0; page=3000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':SELECT_USAGE}
        data=search(ep,key,USAGE_INDEX,body)
        rows=data.get('value',[])
        if not rows:
            break
        for r in rows:
            yield r
        skip+=len(rows)
        if len(rows) < page:
            break

def gather(ep,key):
    """Aggregate distinct PROGRAM / COPYBOOK / PARAGRAPH names plus occurrence counts.

    program counts: number of usage rows referencing program_id (proxy for activity)
    copybook counts: number of usage rows referencing copybook_name_plain
    paragraph counts: number of usage rows referencing paragraph_name (new)
    """
    programs=set(); copybooks=set(); paragraphs=set()
    prog_counts={}; cpy_counts={}; para_counts={}
    for row in iter_usage(ep,key):
        p=row.get('program_id'); c=row.get('copybook_name_plain'); para=row.get('paragraph_name')
        if p:
            programs.add(p)
            prog_counts[p]=prog_counts.get(p,0)+1
        if c:
            copybooks.add(c)
            cpy_counts[c]=cpy_counts.get(c,0)+1
        if para:
            paragraphs.add(para)
            para_counts[para]=para_counts.get(para,0)+1
    return programs, copybooks, paragraphs, prog_counts, cpy_counts, para_counts

def build_alias_docs(names, kind: str, source_hint: str, base_counts: dict):
    docs=[]; seen=set();
    ts=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    for name in sorted(names):
        canon=canonical(name)
        variants=defaultdict(set)
        for fn in VARIANT_FUNCS:
            try:
                v=fn(name)
            except Exception:
                continue
            if not v:
                continue
            vtype=fn.__name__[2:]  # remove v_
            variants[vtype].add(v)
        # Always include canonical itself as alias
        variants['canonical'].add(canon)
        for vtype,vals in variants.items():
            for alias in vals:
                aid=hashlib.sha1(f"{canon}|{alias}|{kind}".encode('utf-8')).hexdigest()[:40]
                key=(aid)
                if key in seen:
                    continue
                seen.add(key)
                base_count=base_counts.get(name,0)
                # alias occurrences for raw/canonical forms; others use 0 initially
                alias_count = base_count if alias == name or alias == canon else 0
                docs.append({
                    'alias_id': aid,
                    'canonical_name': canon,
                    'alias': alias,
                    'variant_type': vtype,
                    'kind': kind,
                    'source_hint': source_hint,
                    'canonical_occurrences': base_count,
                    'alias_occurrences': alias_count,
                    'ingested_at': ts
                })
    return docs

def upload(ep,key,docs):
    if not docs:
        return
    url=f"{ep}/indexes/{ALIAS_INDEX}/docs/index?api-version={API}"
    headers={'api-key':key,'Content-Type':'application/json'}
    for i in range(0,len(docs),BATCH_UPLOAD):
        chunk=docs[i:i+BATCH_UPLOAD]
        payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in chunk]}
        r=requests.post(url,headers=headers,json=payload,timeout=120)
        if r.status_code not in (200,201):
            print('[ERROR] upload failed',r.status_code,r.text[:300]);
        else:
            print(f"[UPLOAD] +{len(chunk)} / {len(docs)}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--commit',action='store_true',help='Upload to index (otherwise preview counts)')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    programs, copybooks, paragraphs, prog_counts, cpy_counts, para_counts = gather(ep,key)
    print(f"Collected programs={len(programs)} copybooks={len(copybooks)} paragraphs={len(paragraphs)}")
    prog_docs=build_alias_docs(programs,'PROGRAM','copybook_usage',prog_counts)
    cpy_docs=build_alias_docs(copybooks,'COPYBOOK','copybook_usage',cpy_counts)
    # Provide paragraph occurrence counts (previously zero)
    para_docs=build_alias_docs(paragraphs,'PARAGRAPH','copybook_usage',para_counts)
    all_docs=prog_docs+cpy_docs+para_docs
    print(f"Prepared alias docs total={len(all_docs)} (program={len(prog_docs)} copybook={len(cpy_docs)} paragraph={len(para_docs)})")
    if not args.commit:
        sample=all_docs[:5]
        print(json.dumps({'sample':sample},indent=2))
        print('Run with --commit to upload.')
        return
    upload(ep,key,all_docs)
    print('Done.')

if __name__=='__main__':
    main()
