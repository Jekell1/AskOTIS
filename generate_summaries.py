#!/usr/bin/env python3
"""Scaffold: generate semantic summaries for COBOL routines / families.

This is a placeholder pipeline that:
  1. Reads alias docs (cobol-routine-aliases) or local JSONL file.
  2. For each alias family canonical target (or top candidate), aggregates:
       - Flow edge counts
       - Distinct callers / callees (from flow edges + calls index)
  3. Produces a heuristic summary text (no LLM call yet) to be replaced by model generation later.
  4. Writes JSONL and optionally uploads to cobol-summaries.

Future (LLM integration):
  - Add OpenAI / Azure OpenAI call with controlled prompt referencing aggregated evidence.
  - Add chunking & rate limiting.

Usage:
  python generate_summaries.py --limit 200 --out JSONL/summaries.jsonl --upload

Environment:
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY

Schema (cobol-summaries):
  summary_id, entity_kind (routine|family), name, family_key, program_id,
  source_ids[], summary_text, evidence_json, version, created_ts
"""
from __future__ import annotations
import os, json, time, argparse, hashlib
import requests
from typing import Dict, List, Tuple

API_VERSION = "2024-07-01"
ALIAS_INDEX = "cobol-routine-aliases"
FLOW_INDEX = "cobol-flow-edges-v2"
CALL_INDEX = "cobol-calls"
TARGET_INDEX = "cobol-summaries"
VERSION = "v1"


def load_cfg():
    vals={}
    if os.path.exists('local.settings.json'):
        try: vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception: pass
    ep=(os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or '').rstrip('/')
    key=(os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY'))
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key')
    return ep,key

def search(index: str, body: Dict, ep: str, key: str):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=45)
    if r.status_code!=200:
        return {"value":[],"error":r.text}
    return r.json()

def fetch_alias_families(ep,key,limit=None):
    # fetch alias docs via search * (paged simplistic) (NOTE: for large sets use indexer or continuation tokens; simplified here)
    body={"search":"*","top":1000}
    res=search(ALIAS_INDEX, body, ep, key)
    allv=res.get('value',[])
    # No continuation implemented (simplification)
    fams={}
    for v in allv:
        fk=v.get('family_key')
        if not fk: continue
        fams.setdefault(fk,[]).append(v)
    if limit:
        # limit number of families processed
        fam_items=list(fams.items())[:limit]
        fams=dict(fam_items)
    return fams

def summarize_family(fk: str, docs: List[Dict], ep: str, key: str):
    # pick canonical doc (highest confidence then shortest name)
    canon = sorted(docs, key=lambda d: (-d.get('confidence',0), len(d.get('canonical_target') or d.get('alias') or '')))[0]
    name = canon.get('canonical_target') or canon.get('alias') or fk
    # gather flow edges for candidate names
    candidates = set([name] + [c for d in docs for c in (d.get('candidate_targets') or [])])
    flow_body={"search": ' OR '.join(candidates), "top": 200}
    if fk:
        flow_body['filter']=f"family_key eq '{fk}'"
    flow_res=search(FLOW_INDEX, flow_body, ep, key)
    flow_vals=flow_res.get('value',[])
    callers={e.get('caller_para') for e in flow_vals if e.get('caller_para')}
    targets={e.get('resolved_target_para') for e in flow_vals if e.get('resolved_target_para')}
    edge_kinds={e.get('kind') for e in flow_vals}
    resolved=sum(1 for e in flow_vals if e.get('resolved'))

    # naive textual summary stub
    summary_text=(f"Routine '{name}' (family {fk}) appears in {len(flow_vals)} flow edges; "
                  f"resolved targets={resolved}; callers={len(callers)}; distinct target paragraphs={len(targets)}; "
                  f"edge kinds={','.join(sorted([k for k in edge_kinds if k]))}.")

    evidence={
        'flow_edge_count': len(flow_vals),
        'resolved_edge_count': resolved,
        'callers': list(sorted(callers))[:25],
        'targets': list(sorted([t for t in targets if t]))[:25],
        'edge_kinds': sorted([k for k in edge_kinds if k]),
    }
    sid=hashlib.sha1(f"{fk}:{name}".encode()).hexdigest()[:20]
    doc={
        'summary_id': sid,
        'entity_kind': 'family',
        'name': name,
        'family_key': fk,
        'program_id': None,
        'source_ids': list(candidates)[:50],
        'summary_text': summary_text,
        'evidence_json': json.dumps(evidence, ensure_ascii=False),
        'version': VERSION,
        'created_ts': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
    }
    return doc

def upload(docs, ep, key):
    url=f"{ep}/indexes/{TARGET_INDEX}/docs/index?api-version={API_VERSION}"
    for i in range(0,len(docs),500):
        chunk=docs[i:i+500]
        for d in chunk: d['@search.action']='mergeOrUpload'
        r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'value':chunk}, timeout=90)
        if r.status_code!=200:
            print('Upload error', r.status_code, r.text[:200])
        else:
            body=r.json(); fails=[v for v in body.get('value',[]) if v.get('status') is False]
            if fails: print(f"Batch {(i//500)+1} failures={len(fails)}")


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--limit-families', type=int, default=100, help='Limit number of families processed (dev)')
    ap.add_argument('--out', default='JSONL/summaries.jsonl')
    ap.add_argument('--upload', action='store_true')
    args=ap.parse_args()
    ep,key=load_cfg()
    fams=fetch_alias_families(ep,key,limit=args.limit_families)
    docs=[]
    for fk, adocs in fams.items():
        try:
            docs.append(summarize_family(fk, adocs, ep, key))
        except Exception as e:
            print('⚠️ summary failed for family', fk, e)
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out,'w',encoding='utf-8') as f:
        for d in docs:
            f.write(json.dumps(d, ensure_ascii=False)+"\n")
    print(f"Wrote {len(docs)} summary docs -> {args.out}")
    if args.upload:
        upload(docs, ep, key)

if __name__=='__main__':
    main()
