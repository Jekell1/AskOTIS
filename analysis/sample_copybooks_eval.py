"""Sample and evaluate copybooks to decide whether to include them in symbol ref ingestion.

Metrics per sampled copybook:
  - total_lines / non_blank / comment_lines
  - data_declaration_lines (level numbers 01-49,66,77,88)
  - distinct_declared_symbols count
  - presence of PROCEDURE DIVISION or PERFORM statements (code indicators)
  - nested COPY statements

Aggregate metrics:
  - averages and ratios across sample
  - top symbols (up to 50) checked for existence already in new_cobol_symbol_refs index
  - duplication rate (fraction of sampled symbols already in symbol ref index)

Usage:
  python analysis/sample_copybooks_eval.py --root cobol_src --sample 20
"""
from __future__ import annotations
import os, json, re, random, argparse, pathlib, requests, collections, time

LEVEL_RE = re.compile(r"^\s*(\d{2})\s+([A-Z0-9][A-Z0-9-]{1,62})\b")
COMMENT_RE = re.compile(r"^\s*(\*|//|\*>)")
COPY_RE = re.compile(r"^\s*COPY\s+", re.IGNORECASE)
PROCEDURE_MARKERS = ("PROCEDURE DIVISION", "END-EXEC")
PERFORM_RE = re.compile(r"\bPERFORM\b")

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
SYMBOL_INDEX='new_cobol_symbol_refs'

ALLOWED_LEVELS=set([f"{i:02d}" for i in range(1,50)])|{'66','77','88'}

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def endpoint():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        return None, None
    return ep.rstrip('/'), key

def list_copybooks(root: str):
    p=pathlib.Path(root)
    return [fp for fp in p.rglob('*') if fp.suffix.lower()=='.cpy']


def analyze_copybook(path: pathlib.Path):
    try:
        text=path.read_text(encoding='utf-8',errors='ignore')
    except Exception:
        return None
    lines=text.splitlines()
    total=len(lines)
    non_blank=sum(1 for l in lines if l.strip())
    comments=sum(1 for l in lines if COMMENT_RE.match(l))
    data_lines=[]
    symbols=[]
    has_procedure=False
    has_perform=False
    nested_copy=0
    for l in lines:
        u=l.upper()
        if any(m in u for m in PROCEDURE_MARKERS):
            has_procedure=True
        if PERFORM_RE.search(u):
            has_perform=True
        if COPY_RE.match(l):
            nested_copy+=1
        m=LEVEL_RE.match(l)
        if m:
            lvl, sym = m.group(1), m.group(2).upper()
            if lvl in ALLOWED_LEVELS:
                data_lines.append(l)
                symbols.append(sym)
    distinct_symbols=set(symbols)
    return {
        'file': str(path),
        'total_lines': total,
        'non_blank': non_blank,
        'comment_lines': comments,
        'data_decl_lines': len(data_lines),
        'distinct_symbols': len(distinct_symbols),
        'has_procedure_marker': has_procedure,
        'has_perform': has_perform,
        'nested_copy_count': nested_copy,
        'sample_symbols': list(sorted(distinct_symbols))[:15]
    }


def query_symbol_presence(symbols, limit=50):
    ep,key=endpoint()
    if not ep or not key:
        return {s: None for s in symbols}
    out={}
    headers={'api-key':key,'Content-Type':'application/json'}
    checked=0
    for s in symbols:
        if checked>=limit: break
        body={'search':'*','top':1,'filter':f"symbol_name eq '{s}'"}
        r=requests.post(f"{ep}/indexes/{SYMBOL_INDEX}/docs/search?api-version={API_VERSION}",headers=headers,json=body,timeout=30)
        present=False
        if r.status_code==200:
            val=r.json().get('value',[])
            if val:
                present=True
        out[s]=present
        checked+=1
    return out


def main():
    ap=argparse.ArgumentParser(description='Evaluate copybooks sampling for inclusion decision')
    ap.add_argument('--root',default='cobol_src')
    ap.add_argument('--sample',type=int,default=20)
    ap.add_argument('--seed',type=int,default=42)
    args=ap.parse_args(); load_settings()
    random.seed(args.seed)
    copybooks=list_copybooks(args.root)
    if not copybooks:
        print(json.dumps({'error':'no_copybooks_found'},indent=2)); return
    sample=copybooks if len(copybooks)<=args.sample else random.sample(copybooks,args.sample)
    reports=[]
    for fp in sample:
        rep=analyze_copybook(fp)
        if rep:
            reports.append(rep)
    agg=collections.Counter()
    for r in reports:
        agg['total_lines']+=r['total_lines']
        agg['data_decl_lines']+=r['data_decl_lines']
        agg['distinct_symbols']+=r['distinct_symbols']
        if r['has_procedure_marker']: agg['with_procedure']+=1
        if r['has_perform']: agg['with_perform']+=1
        if r['nested_copy_count']>0: agg['with_nested_copy']+=1
    n=len(reports)
    avg_total=agg['total_lines']/n if n else 0
    avg_data=agg['data_decl_lines']/n if n else 0
    data_ratio=(agg['data_decl_lines']/agg['total_lines']) if agg['total_lines'] else 0
    all_syms=set()
    for r in reports:
        all_syms.update(r['sample_symbols'])
    presence=query_symbol_presence(sorted(all_syms),limit=50)
    present_rate=sum(1 for v in presence.values() if v)/max(1,len(presence))

    decision_hint={
        'avg_lines_per_copybook': round(avg_total,2),
        'avg_data_lines_per_copybook': round(avg_data,2),
        'data_line_ratio': round(data_ratio,3),
        'procedure_marker_copybook_fraction': round(agg['with_procedure']/n,3) if n else 0,
        'perform_statement_copybook_fraction': round(agg['with_perform']/n,3) if n else 0,
        'nested_copy_fraction': round(agg['with_nested_copy']/n,3) if n else 0,
        'symbol_duplication_rate_in_index': round(present_rate,3)
    }
    print(json.dumps({
        'sample_size': n,
        'total_copybooks_found': len(copybooks),
        'reports': reports,
        'duplication_presence_sample': presence,
        'decision_hint': decision_hint
    },indent=2))

if __name__=='__main__':
    main()
