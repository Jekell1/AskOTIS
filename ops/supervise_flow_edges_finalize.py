"""Supervisor for completing vector coverage on new_cobol_flow_edges_v2.

Workflow:
 1. Run finalize_flow_edges_v2_vectors.py to embed / assign vectors for all remaining edges.
 2. Run diagnose_flow_edge_gaps_v2.py to produce authoritative coverage JSON.
 3. Enforce target coverage threshold (default 99%).
 4. Emit human + JSON summary (optional file write).

Example:
  python ops/supervise_flow_edges_finalize.py \
     --batch 512 --page 800 --sentinel-sparse \
     --target-coverage 99.0 --json-out flow_edges_finalize_summary.json

Safe to re-run; finalize script is idempotent (already-vectorized docs skipped).
"""
from __future__ import annotations
import argparse, json, os, sys, subprocess, shlex, time

DEFAULT_FINALIZE='finalize_flow_edges_v2_vectors.py'
DEFAULT_DIAGNOSE='diagnose_flow_edge_gaps_v2.py'
INDEX='new_cobol_flow_edges_v2'

def load_local_settings():
    try:
        data=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in data.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def run_cmd(cmd:list[str]) -> int:
    print('[exec]', ' '.join(shlex.quote(c) for c in cmd))
    start=time.time()
    proc=subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    for line in proc.stdout:  # stream output
        sys.stdout.write(line)
    proc.wait()
    print(f'[exit] code={proc.returncode} elapsed={time.time()-start:.1f}s')
    return proc.returncode

def main():
    ap=argparse.ArgumentParser(description='Supervise full vector coverage for flow edges v2')
    ap.add_argument('--batch', type=int, default=512, help='Finalize script batch cap')
    ap.add_argument('--page', type=int, default=800, help='Finalize script page size')
    ap.add_argument('--sentinel-sparse', action='store_true', help='Use sparse sentinel for empty edge_text')
    ap.add_argument('--max-pages', type=int, default=0, help='Limit finalize pages (0 = all)')
    ap.add_argument('--finalize-script', default=DEFAULT_FINALIZE)
    ap.add_argument('--diagnose-script', default=DEFAULT_DIAGNOSE)
    ap.add_argument('--target-coverage', type=float, default=99.0, help='Required percent coverage (has_vector true)')
    ap.add_argument('--json-out', help='Optional path to write JSON summary')
    ap.add_argument('--dry-run', action='store_true', help='Preview commands only')
    ap.add_argument('--max-passes', type=int, default=3, help='Maximum finalize+diagnose passes if coverage target not yet achieved')
    ap.add_argument('--coverage-probe-only', action='store_true', help='Skip finalize; just run diagnose + summary (assumes env loaded)')
    args=ap.parse_args()

    load_local_settings()

    finalize_cmd=['python', args.finalize_script, '--batch', str(args.batch), '--page', str(args.page), '--full-page']
    if args.max_pages:
        finalize_cmd += ['--max-pages', str(args.max_pages)]
    if args.sentinel_sparse:
        finalize_cmd.append('--sentinel-sparse')

    diag_json='_flow_edges_diag_tmp.json'
    diagnose_cmd=['python', args.diagnose_script, '--max-missing-scan', '0', '--json-out', diag_json]

    if args.dry_run:
        print('[dry-run] Finalize command:', ' '.join(finalize_cmd))
        print('[dry-run] Diagnose command:', ' '.join(diagnose_cmd))
        return

    passes=0
    coverage=0.0
    missing=None
    total=0
    diag={}
    while passes < (1 if args.coverage_probe_only else args.max_passes):
        if not args.coverage_probe_only:
            print(f"=== Pass {passes+1}: Finalize embedding coverage ===")
            code=run_cmd(finalize_cmd)
            if code!=0:
                print('[error] Finalize script failed; aborting.')
                sys.exit(code)
        print(f"\n=== Pass {passes+1}: Diagnose coverage ===")
        code=run_cmd(diagnose_cmd)
        if code!=0 or not os.path.isfile(diag_json):
            print('[error] Diagnose script failed; aborting.')
            sys.exit(code or 1)
        with open(diag_json,'r',encoding='utf-8') as f:
            diag=json.load(f)
        total=diag.get('total_docs') or 0
        missing=diag.get('missing_docs') or 0
        coverage = ((total - missing)/total*100.0) if total else 0.0
        print(f"[pass {passes+1}] coverage={coverage:.2f}% missing={missing}")
        if (coverage >= args.target_coverage and missing==0) or coverage==100.0:
            break
        passes+=1
        if args.coverage_probe_only:
            break
        if passes < args.max_passes:
            print('[info] Target not yet met; initiating another finalize pass...')
    passed = ((coverage >= args.target_coverage and missing==0) or coverage==100.0)
    summary={
        'index': INDEX,
        'total_docs': total,
        'missing_docs': missing,
        'coverage_pct': round(coverage,6),
        'target_coverage': args.target_coverage,
        'passed': passed,
        'finalize_batch': args.batch,
        'finalize_page': args.page,
        'sentinel_sparse': args.sentinel_sparse,
        'passes_used': passes+1,
        'timestamp': time.time()
    }
    banner = f"FLOW EDGES COVERAGE: {summary['coverage_pct']:.2f}% (missing={missing}) -> {'PASS' if passed else 'FAIL'}"
    print('\n=== Summary ===')
    print(banner)
    print(json.dumps(summary,indent=2))
    if args.json_out:
        with open(args.json_out,'w',encoding='utf-8') as outf:
            json.dump(summary,outf,indent=2)
        print(f"[info] Wrote summary JSON -> {args.json_out}")
    if not passed:
        sys.exit(2)

if __name__=='__main__':
    main()
