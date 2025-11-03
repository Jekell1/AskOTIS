"""Build / enrich symbol reference occurrences and variable usage first-write metadata.

Implements requested behavior:
    * Walk COBOL sources under --src-root collecting heuristic symbol occurrences in DATA and PROCEDURE divisions.
    * Emit/merge docs into new_cobol_symbol_refs with: ref_id, symbol_id_global, program_id, file_path,
        line_number, paragraph_name, op (READ/WRITE/PARAM_IN/PARAM_OUT), is_first_write (later set),
        excerpt, has_vector (False). (excerpt_vector populated by separate embedding backfill.)
    * Compute earliest WRITE/PARAM_OUT per symbol_id_global and mark is_first_write=true.
    * Aggregate per-symbol counts and update new_cobol_variable_usage with first_write_location_* fields
        and read/write/param counts + total_refs + has_first_write.
    * Idempotent: deterministic ref_id (program|line|symbol|op) with mergeOrUpload.
    * Supports batching, retries, and diagnostics (--check-symbol TOKEN).

NOTE: Heuristic parsing (not a full COBOL parser). For higher accuracy integrate formal parser.
"""
from __future__ import annotations
import os, json, argparse, requests, sys, time, pathlib, re, random
from collections import defaultdict, Counter
import sys, pathlib
# Ensure repository root on path for id_normalization import when executed from subdir
repo_root=pathlib.Path(__file__).resolve().parent.parent
if str(repo_root) not in sys.path:
    sys.path.insert(0,str(repo_root))
from id_normalization import (
    make_global_symbol_id,
    make_ref_id,
    normalize_program_id
)

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
REF_INDEX='new_cobol_symbol_refs'
USAGE_INDEX='new_cobol_variable_usage'
WRITE_OPS={'WRITE','PARAM_OUT'}

PARA_LABEL_RE=re.compile(r'^(?P<label>[A-Z0-9][A-Z0-9-]{1,63})\.$')
COMMENT_LINE_RE=re.compile(r'^(\*|\*>|//)')
TOKEN_RE=re.compile(r'[A-Z][A-Z0-9-]{1,63}')
ASSIGN_EQ_RE=re.compile(r'(^|\s)(?P<sym>[A-Z][A-Z0-9-]{1,63})\s*=\s*')
MOVE_STMT_RE=re.compile(r'\bMOVE\s+.+?\s+TO\s+(?P<sym>[A-Z][A-Z0-9-]{1,63})\b')
COMPUTE_STMT_RE=re.compile(r'\bCOMPUTE\s+(?P<sym>[A-Z][A-Z0-9-]{1,63})\b')
ARITH_STMTS=('ADD','SUBTRACT','MULTIPLY','DIVIDE')
EXCERPT_MAX=320
MAX_RETRIES=3

def backoff_sleep(i):
    time.sleep(min(4, 0.25 * (2 ** i) + random.random()*0.1))


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_refs(ep,key, limit=None):
    """Retain a minimal fetch helper for skip-source-parse mode.

    NOTE: Schema does not store op/kind fields; thus we cannot recompute earliest writes
    without parsing sources. In skip mode we only enumerate symbols (no earliest write updates).
    """
    headers={'api-key':key,'Content-Type':'application/json'}
    skip=0; total=0; page=1000
    while True:
        if limit and total>=limit: break
        body={'search':'*','top':page,'skip':skip,'select':'ref_id,program_id,symbol_name,line_number,paragraph_name,is_first_write'}
        r=requests.post(f"{ep}/indexes/{REF_INDEX}/docs/search?api-version={API_VERSION}",headers=headers,json=body)
        if r.status_code!=200:
            raise SystemExit(f"Ref fetch failed {r.status_code}: {r.text[:300]}")
        batch=r.json().get('value',[])
        if not batch: break
        for b in batch: yield b
        total+=len(batch)
        if len(batch)<page: break
        skip+=page


def fetch_variable_usage(ep,key, symbol_ids):
    if not symbol_ids: return {}
    headers={'api-key':key,'Content-Type':'application/json'}
    out={}
    ids=list(symbol_ids); CHUNK=32
    for i in range(0,len(ids),CHUNK):
        chunk=ids[i:i+CHUNK]
        or_list=' or '.join([f"symbol_id_global eq '{sid}'" for sid in chunk])
        body={'search':'*','top':len(chunk),'filter':f"({or_list})",'select':'symbol_id_global,first_write_location_path,first_write_location_line,first_write_program,first_write_paragraph,read_count,write_count,param_in_count,param_out_count,total_refs,has_first_write'}
        r=requests.post(f"{ep}/indexes/{USAGE_INDEX}/docs/search?api-version={API_VERSION}",headers=headers,json=body)
        if r.status_code!=200:
            raise SystemExit(f"Usage fetch failed {r.status_code}: {r.text[:240]}")
        for v in r.json().get('value',[]):
            out[v['symbol_id_global']]=v
    return out


def upload(ep,key,index, docs):
    if not docs: return 0
    headers={'api-key':key,'Content-Type':'application/json'}
    url=f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload={'value':docs}
    for attempt in range(MAX_RETRIES):
        r=requests.post(url,headers=headers,json=payload,timeout=60)
        if r.status_code in (200,201):
            return len(docs)
        if r.status_code>=500 and attempt<MAX_RETRIES-1:
            print(f"[WARN] upload retry {attempt+1} status={r.status_code}")
            backoff_sleep(attempt)
            continue
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")
    return 0

def discover_cobol_files(root: str):
    p=pathlib.Path(root)
    if not p.exists():
        return []
    exts={'.cbl','.cob'}
    return [fp for fp in p.rglob('*') if fp.suffix.lower() in exts]

def parse_file(fp: pathlib.Path):
    try:
        text=fp.read_text(encoding='utf-8',errors='ignore')
    except Exception:
        return []
    lines=text.splitlines()
    program_id=normalize_program_id(fp.stem)
    in_proc=False
    paragraph=''
    occ=[]  # (program_id,line,paragraph,symbol,op,excerpt)
    for i,raw in enumerate(lines, start=1):
        stripped=raw.strip()
        if not stripped: continue
        up=stripped.upper()
        if COMMENT_LINE_RE.match(up):
            continue
        if up.startswith('PROCEDURE DIVISION'):
            in_proc=True
        if PARA_LABEL_RE.match(up):
            paragraph=PARA_LABEL_RE.match(up).group('label').upper()
            continue
        # Data division heuristics (treat declarations as WRITE)
        if not in_proc:
            parts=up.split()
            if parts and parts[0].isdigit() and len(parts)>1:
                sym=parts[1]
                if len(sym)>1 and sym.isalnum():
                    occ.append((program_id,i,paragraph,sym,'WRITE',stripped[:EXCERPT_MAX]))
            continue
        # Procedure division
        assigns=ASSIGN_EQ_RE.findall(up)
        move_targets=[m.group('sym') for m in MOVE_STMT_RE.finditer(up)]
        compute_targets=[m.group('sym') for m in COMPUTE_STMT_RE.finditer(up)]
        write_syms=set(assigns+move_targets+compute_targets)
        for kw in ARITH_STMTS:
            if up.startswith(kw+' '):
                if ' TO ' in up:
                    tgt=up.split(' TO ')[-1].split()[0]
                    write_syms.add(tgt)
        tokens=TOKEN_RE.findall(up)
        for t in tokens:
            if len(t)<=1: continue
            op='WRITE' if t in write_syms else 'READ'
            occ.append((program_id,i,paragraph,t,op,stripped[:EXCERPT_MAX]))
    return occ

def build_ref_doc(program_id,line,paragraph,symbol,op,excerpt,file_path):
    ref_id=make_ref_id(program_id,line,symbol,op)
    return {
        '@search.action':'mergeOrUpload',
        'ref_id':ref_id,
        'program_id':program_id,
        'symbol_name':symbol,
        'paragraph_name':paragraph or '',
        'line_number':line,
        'file_path':str(file_path),
        'excerpt_text':excerpt,
        'has_vector': False,
        'is_first_write': False,
        'updated_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    }


def main():
    ap=argparse.ArgumentParser(description='Parse COBOL sources and enrich symbol refs & variable usage')
    ap.add_argument('--src-root',default='cobol_src')
    ap.add_argument('--batch',type=int,default=1000)
    ap.add_argument('--limit-files',type=int,default=0)
    ap.add_argument('--limit-refs',type=int,default=0)
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--skip-source-parse',action='store_true')
    ap.add_argument('--check-symbol',help='Print summary for a symbol (raw name) after enrichment')
    ap.add_argument('--include-copybooks',action='store_true',help='Also parse .CPY copybooks (adds refs for copybook-only declarations)')
    ap.add_argument('--copybook-prefix',default='CPY::',help='Program id prefix to namespace copybook-derived refs (avoid collision with real program ids)')
    ap.add_argument('--copybook-max-lines',type=int,default=0,help='Skip copybooks exceeding this many lines (0 = no limit)')
    ap.add_argument('--stream-upload',action='store_true',help='Upload reference docs incrementally during parse (reduces memory)')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    parse_start=time.time()
    PROGRESS_FILE_INTERVAL=200  # files
    PROGRESS_REF_INTERVAL=250000  # refs parsed
    last_ref_progress=0

    new_docs=[]  # accumulated only when not streaming
    total_uploaded_refs=0
    earliest={}  # gid -> (priority,line,pid,para,ref_id) where priority: 0 program, 1 copybook
    counts=defaultdict(lambda: Counter())

    def consider_write(gid,line,pid,para,ref_id,is_program):
        # Lower priority value wins (program=0 preferred over copybook=1); tie-break by lower line number
        priority = 0 if is_program else 1
        prev = earliest.get(gid)
        if prev is None or (priority < prev[0]) or (priority == prev[0] and line < prev[1]):
            earliest[gid]=(priority,line,pid,para,ref_id)

    def discover_program_files(root):
        p=pathlib.Path(root)
        exts={'.cbl','.cob'}
        files=[fp for fp in p.rglob('*') if fp.suffix.lower() in exts]
        return files

    def discover_copybook_files(root, max_lines=0):
        p=pathlib.Path(root)
        files=[]
        for fp in p.rglob('*'):
            if fp.suffix.lower()=='.cpy':
                if max_lines:
                    try:
                        # Quick line count short-circuit
                        with fp.open('r',encoding='utf-8',errors='ignore') as fh:
                            for i,_ in enumerate(fh, start=1):
                                if max_lines and i>max_lines:
                                    break
                        if max_lines and i>max_lines:
                            continue  # skip oversized
                    except Exception:
                        continue
                files.append(fp)
        return files
    if not args.skip_source_parse:
        prog_files=discover_program_files(args.src_root)
        if args.limit_files:
            prog_files=prog_files[:args.limit_files]
        copy_files=[]
        if args.include_copybooks:
            copy_files=discover_copybook_files(args.src_root, args.copybook_max_lines)
        if args.limit_files:
            copy_files=copy_files[:max(0,args.limit_files-len(prog_files))]
        for fi,fp in enumerate(prog_files, start=1):
            occs=parse_file(fp)
            for (pid,line,para,sym,op,excerpt) in occs:
                d=build_ref_doc(pid,line,para,sym,op,excerpt,fp)
                if args.stream_upload:
                    new_docs.append(d)
                    if len(new_docs)>=args.batch:
                        total_uploaded_refs+=upload(ep,key,REF_INDEX,new_docs); new_docs.clear()
                else:
                    new_docs.append(d)
                gid=make_global_symbol_id(sym)
                # counts
                counts[gid][op]+=1
                counts[gid]['TOTAL']+=1
                if op in WRITE_OPS:
                    consider_write(gid,line,pid,para,d['ref_id'],True)
                current_ref_count = total_uploaded_refs + len(new_docs) if args.stream_upload else len(new_docs)
                if args.limit_refs and current_ref_count>=args.limit_refs:
                    break
            if args.limit_refs and (total_uploaded_refs + len(new_docs) if args.stream_upload else len(new_docs))>=args.limit_refs:
                break
            if fi % PROGRESS_FILE_INTERVAL==0:
                elapsed=time.time()-parse_start
                ref_count = total_uploaded_refs + len(new_docs) if args.stream_upload else len(new_docs)
                rate = ref_count/elapsed if elapsed>0 else 0
                print(f"[PARSE] files={fi}/{len(prog_files)} refs_seen={ref_count} elapsed={elapsed:.1f}s rate={rate:.1f} refs/s",flush=True)
            ref_count = total_uploaded_refs + len(new_docs) if args.stream_upload else len(new_docs)
            if ref_count - last_ref_progress >= PROGRESS_REF_INTERVAL:
                elapsed=time.time()-parse_start
                rate = ref_count/elapsed if elapsed>0 else 0
                est_total = None
                if args.limit_refs:
                    remaining=args.limit_refs-ref_count
                    est_total=args.limit_refs
                # Simple ETA if limit provided
                eta = (remaining/rate) if rate>0 and args.limit_refs else None
                if est_total:
                    pct=ref_count/est_total*100
                    print(f"[PARSE] refs={ref_count}/{est_total} ({pct:.2f}%) rate={rate:.1f}/s ETA={(eta/60):.1f}m",flush=True)
                else:
                    print(f"[PARSE] refs={ref_count} rate={rate:.1f}/s",flush=True)
                last_ref_progress=ref_count
        if args.stream_upload and new_docs:
            total_uploaded_refs+=upload(ep,key,REF_INDEX,new_docs); new_docs.clear()
        # Copybooks: treat each as its own pseudo-program id with prefix; still parse heuristically.
        if args.include_copybooks and (not args.limit_refs or len(new_docs)<args.limit_refs):
            for fp in copy_files:
                pseudo_pid=args.copybook_prefix + normalize_program_id(fp.stem)
                occs=parse_file(fp)
                for (_pid,line,para,sym,op,excerpt) in occs:
                    # Replace pid with pseudo id
                    d=build_ref_doc(pseudo_pid,line,para,sym,op,excerpt,fp)
                    new_docs.append(d)
                    gid=make_global_symbol_id(sym)
                    counts[gid][op]+=1
                    counts[gid]['TOTAL']+=1
                    if op in WRITE_OPS:
                        consider_write(gid,line,pseudo_pid,para,d['ref_id'],False)
                    if args.limit_refs and len(new_docs)>=args.limit_refs:
                        break
                if args.limit_refs and len(new_docs)>=args.limit_refs:
                    break
        final_ref_total = total_uploaded_refs if args.stream_upload else len(new_docs)
        print(f"Parsed programs={len(prog_files)} copybooks={(len(copy_files) if args.include_copybooks else 0)} produced {final_ref_total} refs (stream={args.stream_upload})")
        if args.push and not args.stream_upload and new_docs:
            buf=[]; uploaded=0
            upload_start=time.time(); last_upload_log=time.time(); UPLOAD_LOG_INTERVAL=30  # seconds
            for d in new_docs:
                buf.append(d)
                if len(buf)>=args.batch:
                    uploaded+=upload(ep,key,REF_INDEX,buf); buf.clear()
                    if time.time()-last_upload_log>=UPLOAD_LOG_INTERVAL:
                        elapsed=time.time()-upload_start
                        rate=uploaded/elapsed if elapsed>0 else 0
                        print(f"[UPLOAD] uploaded={uploaded}/{len(new_docs)} ({uploaded/len(new_docs)*100:.2f}%) rate={rate:.1f} docs/s",flush=True)
                        last_upload_log=time.time()
            if buf:
                uploaded+=upload(ep,key,REF_INDEX,buf)
            print(f"Uploaded {uploaded} new/updated refs")
        if args.push and args.stream_upload:
            print(f"[UPLOAD] streamed total_uploaded_refs={total_uploaded_refs}")
    else:
        print('[INFO] Skipping source parsing phase.')

    # If we skipped source parse, attempt a minimal scan (no earliest writes without op fields)
    if args.skip_source_parse:
        scanned=0
        for ref in fetch_refs(ep,key):
            scanned+=1
            name=(ref.get('symbol_name') or '').strip()
            gid=make_global_symbol_id(name)
            counts[gid]['READ']+=1
            counts[gid]['TOTAL']+=1
        print(f"[SKIP-PARSE] Scanned existing refs={scanned}; earliest writes not recomputed (op field absent).")
    else:
        print(f"Parsed refs={len(new_docs)} symbols_with_writes={len(earliest)}")

    ref_updates=[{'@search.action':'merge','ref_id':ref_id,'is_first_write':True} for (_,_,_,_,ref_id) in earliest.values() if ref_id]
    usage_updates=[]
    for sidg,c in counts.items():
        fw=earliest.get(sidg)
        if fw:
            _prio,line,pid,para,ref_id=fw
            usage_updates.append({
                '@search.action':'mergeOrUpload',
                'symbol_id_global':sidg,
                'first_write_location': f"{pid}:{line}",
                'first_write_location_path':'',
                'first_write_location_line':line,
                'first_write_program':pid,
                'first_write_paragraph':para,
                'read_count':c.get('READ',0)+c.get('PARAM_IN',0),
                'write_count':c.get('WRITE',0)+c.get('PARAM_OUT',0),
                'param_in_count':c.get('PARAM_IN',0),
                'param_out_count':c.get('PARAM_OUT',0),
                'total_refs':c.get('TOTAL',0),
                'has_first_write': True
            })
        else:
            usage_updates.append({
                '@search.action':'mergeOrUpload',
                'symbol_id_global':sidg,
                'first_write_location': None,
                'read_count':c.get('READ',0)+c.get('PARAM_IN',0),
                'write_count':c.get('WRITE',0)+c.get('PARAM_OUT',0),
                'param_in_count':c.get('PARAM_IN',0),
                'param_out_count':c.get('PARAM_OUT',0),
                'total_refs':c.get('TOTAL',0),
                'has_first_write': False
            })
    print(f"Prepared ref_updates={len(ref_updates)} usage_updates={len(usage_updates)}")

    if args.push:
        for coll,index in ((ref_updates,REF_INDEX),(usage_updates,USAGE_INDEX)):
            if not coll: continue
            buf=[]; done=0
            for d in coll:
                buf.append(d)
                if len(buf)>=args.batch:
                    done+=upload(ep,key,index,buf); buf.clear()
            if buf:
                done+=upload(ep,key,index,buf)
            print(f"Uploaded {done} updates to {index}")
    else:
        print('[DRY-RUN] Example ref_updates:', ref_updates[:2])
        print('[DRY-RUN] Example usage_updates:', usage_updates[:2])

    if args.check_symbol:
        probe=args.check_symbol.upper()
        sidg=make_global_symbol_id(probe)
        c=counts.get(sidg)
        if not c:
            print(f"[CHECK] No occurrences for {probe}")
        else:
            fw=earliest.get(sidg)
            if fw:
                line,pid,para,_=fw
                print(f"[CHECK] {probe} first_write program={pid} line={line} paragraph={para} counts={dict(c)}")
            else:
                print(f"[CHECK] {probe} counts={dict(c)} (no write detected)")

if __name__=='__main__':
    main()
