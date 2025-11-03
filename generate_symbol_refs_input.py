"""Generate JSONL input for symbol references from data items + variable usage sample.

This is a placeholder synthetic generator until real parser-based refs extraction is integrated.
It will:
  * Sample N variable usage docs
  * For each, synthesize one READ and one WRITE ref (line numbers fake) per first two reader/writer programs

Usage:
  python generate_symbol_refs_input.py --output refs_synth.jsonl --sample 200
"""
from __future__ import annotations
import os, sys, json, argparse, random, requests, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
VAR_INDEX='new_cobol_variable_usage'

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

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{VAR_INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Search failed', r.status_code, r.text[:200]); sys.exit(1)
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--output',required=True)
    ap.add_argument('--sample',type=int,default=200)
    ap.add_argument('--ensure-nonempty',action='store_true',help='If no refs generated, emit fallback synthetic entries for a few placeholder variables')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    vals=search(ep,key,{'search':'*','top':args.sample,'select':'variable_id,program_readers,program_writers'})
    if not vals:
        print('[WARN] Variable usage search returned 0 docs.')
    out=open(args.output,'w',encoding='utf-8')
    now=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    total=0
    for v in vals:
        vid=v.get('variable_id');
        if not vid: continue
        readers=v.get('program_readers') or []
        writers=v.get('program_writers') or []
        if not readers and not writers:
            # skip entirely empty usage groups
            continue
        for prog in readers[:2]:
            if not prog: continue
            obj={'program_id':prog,'symbol_name':vid,'kind':'READ','line_number':random.randint(10,500),'file_path':f"{prog}.CBL",'excerpt':f"READ {vid} ...",'ingested_at':now}
            out.write(json.dumps(obj, ensure_ascii=False)+'\n'); total+=1
        for prog in writers[:2]:
            if not prog: continue
            obj={'program_id':prog,'symbol_name':vid,'kind':'WRITE','line_number':random.randint(10,500),'file_path':f"{prog}.CBL",'excerpt':f"WRITE {vid} ...",'ingested_at':now}
            out.write(json.dumps(obj, ensure_ascii=False)+'\n'); total+=1
    if total==0 and args.ensure_nonempty:
        # fallback creation
        placeholders=[('SYNTH-VAR-1','PROG001'),('SYNTH-VAR-2','PROG002'),('SYNTH-VAR-3','PROG003')]
        for vid,prog in placeholders:
            obj={'program_id':prog,'symbol_name':vid,'kind':'READ','line_number':100,'file_path':f"{prog}.CBL",'excerpt':f"READ {vid} ...",'ingested_at':now}
            out.write(json.dumps(obj, ensure_ascii=False)+'\n'); total+=1
            obj={'program_id':prog,'symbol_name':vid,'kind':'WRITE','line_number':140,'file_path':f"{prog}.CBL",'excerpt':f"WRITE {vid} ...",'ingested_at':now}
            out.write(json.dumps(obj, ensure_ascii=False)+'\n'); total+=1
        print('[INFO] Fallback synthetic refs emitted.')
    out.close()
    print(f"Wrote {total} synthetic refs to {args.output}")

if __name__=='__main__':
    main()
