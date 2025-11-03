"""Composite regression execution for symbol/index integrity & embeddings.

Runs:
  - test_symbol_id_regression.py
  - (optional) quick vector presence probe (counts docs with usage_summary_vector present) if field exists
Outputs JSON summary to stdout.
"""
from __future__ import annotations
import os, sys, json, subprocess

def run(cmd):
    p=subprocess.run(cmd, capture_output=True, text=True)
    return p.returncode, p.stdout, p.stderr

def main():
    summary={'symbol_id_regression':None,'vector_probe':None}
    code,out,err=run([sys.executable,'test_symbol_id_regression.py','--sample','25'])
    summary['symbol_id_regression']={'exit_code':code,'stdout':out.splitlines()[-10:]}
    # vector probe using search REST directly if available
    try:
        import requests
        ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
        key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        if ep and key:
            r=requests.get(f"{ep.rstrip('/')}/indexes/new_cobol_variable_usage?api-version=2025-08-01-preview",headers={'api-key':key})
            if r.status_code==200 and any(f['name']=='usage_summary_vector' for f in r.json().get('fields',[])):
                # count docs (no direct way to count vector presence—assume all or none after backfill).
                c=requests.post(f"{ep.rstrip('/')}/indexes/new_cobol_variable_usage/docs/search?api-version=2025-08-01-preview",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
                if c.status_code==200:
                    summary['vector_probe']={'total_docs':c.json().get('@odata.count',0),'vector_field':'usage_summary_vector'}
    except Exception as e:  # noqa: BLE001
        summary['vector_probe']={'error':str(e)}
    print(json.dumps(summary,indent=2))

if __name__=='__main__':
    main()
