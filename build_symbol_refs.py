"""Build symbol reference occurrences into new_cobol_symbol_refs.

Input expectation: a JSONL or NDJSON file with objects containing at minimum:
  program_id, symbol_name, kind (READ|WRITE|PARAM_IN|PARAM_OUT), line_number, file_path, excerpt

Example usage:
  python build_symbol_refs.py --input refs_sample.jsonl --batch 500

If --dry-run is set, only counts classification & would-be uploads.
"""
from __future__ import annotations
import os, sys, json, argparse, time, requests
from id_normalization import make_global_symbol_id, make_scoped_symbol_id, make_ref_id, normalize_program_id

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def iter_input(path: str):
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                yield json.loads(line)
            except Exception:
                continue

def upload(ep,key,batch):
    if not batch: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':batch})
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:200]); sys.exit(1)

def main():
    ap=argparse.ArgumentParser(description='Build symbol refs index from JSONL input')
    ap.add_argument('--input', required=True)
    ap.add_argument('--batch', type=int, default=500)
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--max', type=int, default=0, help='Process at most N lines')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    processed=0; uploaded=0; buf=[]; start=time.time(); first_seen=set()
    for obj in iter_input(args.input):
        if args.max and processed>=args.max: break
        processed+=1
        pid=normalize_program_id(obj.get('program_id',''))
        sname=(obj.get('symbol_name') or '').strip()
        kind=(obj.get('kind') or 'READ').upper()
        line=int(obj.get('line_number') or 0)
        if not pid or not sname:
            continue
        gid=make_global_symbol_id(sname)
        sid=make_scoped_symbol_id(pid,sname)
        ref_id=make_ref_id(pid,line,sname,kind)
        first_key=(pid,sname)
        batch_doc={
            '@search.action':'mergeOrUpload',
            'ref_id':ref_id,
            'program_id':pid,
            'symbol_name':sname,
            'symbol_id':sid,
            'symbol_id_global':gid,
            'kind':kind,
            'op': kind,
            'normalized_kind': 'READ' if kind in ('READ','PARAM_IN') else ('WRITE' if kind in ('WRITE','PARAM_OUT') else kind),
            'line_number':line,
            'file_path':obj.get('file_path') or '',
            'paragraph_name': obj.get('paragraph_name') or '',
            'excerpt':obj.get('excerpt') or '',
            'context_before': obj.get('context_before') or '',
            'context_after': obj.get('context_after') or '',
            'has_vector': False,
            'first_in_program': first_key not in first_seen,
            'cluster_key': f"{pid}|{sname}",
            'ingested_at': obj.get('ingested_at') or ''
        }
        first_seen.add(first_key)
        buf.append(batch_doc)
        if len(buf)>=args.batch:
            if not args.dry_run:
                upload(ep,key,buf); uploaded+=len(buf)
            buf.clear()
    if buf and not args.dry_run:
        upload(ep,key,buf); uploaded+=len(buf)
    dur=time.time()-start
    print(f"Processed {processed} uploaded {uploaded} in {dur:.1f}s dry_run={args.dry_run}")

if __name__=='__main__':
    main()
