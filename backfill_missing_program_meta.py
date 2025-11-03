"""Backfill placeholder documents into new_cobol_program_meta for programs present in paragraph index but absent in meta.

Fields populated:
  program_id (key)
  program_summary: placeholder marker
  outgoing_count, incoming_count: 0
  ui_flag: false
  program_role: '' (empty)
  coverage_pct: 0.0
  paragraph_count: null (not yet known) – omitted
  created_at: timestamp

Requires previously generated files:
  meta_program_ids.json
  paragraph_program_ids.json

Usage:
  python backfill_missing_program_meta.py [--dry-run] [--batch 500]
"""
from __future__ import annotations
import os, json, argparse, sys, time, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def load_set(path):
    return set(json.load(open(path,'r',encoding='utf-8'))) if os.path.exists(path) else set()


def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:160]}")


def main():
    ap=argparse.ArgumentParser(description='Backfill placeholder program_meta docs')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--paragraph-file',default='paragraph_program_ids.json')
    ap.add_argument('--meta-file',default='meta_program_ids.json')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    paras=load_set(args.paragraph_file)
    meta=load_set(args.meta_file)
    missing=sorted(paras - meta)
    print('Missing programs to backfill:', len(missing))
    if not missing:
        return
    ts=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    prepared=[]
    for pid in missing:
        prepared.append({
            'program_id': pid,
            'program_summary': '(placeholder – awaiting enrichment)',
            'outgoing_count': 0,
            'incoming_count': 0,
            'ui_flag': False,
            'coverage_pct': 0.0
        })
    if args.dry_run:
        print('Sample placeholder:', prepared[0] if prepared else 'NONE')
        print('Total prepared:', len(prepared))
        return
    for i in range(0,len(prepared),args.batch):
        chunk=prepared[i:i+args.batch]
        upload(ep,key,chunk)
        print(f'Uploaded {i+len(chunk)}/{len(prepared)}')
    print('Backfill complete.')

if __name__=='__main__':
    main()
