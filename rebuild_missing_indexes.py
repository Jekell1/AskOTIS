"""Rebuild missing critical indexes: new_cobol_symbol_refs and new_cobol_copybook_usage.

Steps:
 1. Recreate symbol refs index
 2. Extract symbol refs from cobol-xrefs (full pass)
 3. Recreate copybook usage index
 4. Ingest copybook usage occurrences from cobol sources (cobol_src by default)
 5. (Optional) Backfill copybook vectors (skipped by default for speed)

Usage:
  python rebuild_missing_indexes.py --source-root cobol_src --no-symbol-limit --no-copy-limit

You can pass --symbol-limit or --copy-limit for faster smoke runs.
"""
from __future__ import annotations
import os, sys, subprocess, argparse, time

SYMBOL_CREATE = [sys.executable, 'create_symbol_refs_index.py', '--force']
SYMBOL_EXTRACT = [sys.executable, 'extract_symbol_refs_from_xrefs.py']
COPY_CREATE = [sys.executable, 'create_copybook_usage_index.py']
COPY_INGEST = [sys.executable, 'ingest_copybook_usage.py', '--roots', 'cobol_src']


def run(cmd, desc):
    print(f"\n[STEP] {desc}: {' '.join(cmd)}")
    start=time.time()
    r=subprocess.run(cmd, text=True)
    if r.returncode!=0:
        print(f"[ERROR] Step failed rc={r.returncode}")
        sys.exit(r.returncode)
    print(f"[OK] {desc} in {time.time()-start:.1f}s")


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--symbol-limit', type=int, help='Limit xref records processed')
    ap.add_argument('--copy-limit', type=int, help='Limit number of COBOL files scanned for COPY usage')
    ap.add_argument('--source-root', default='cobol_src', help='Root with COBOL sources')
    ap.add_argument('--include-copybooks', action='store_true', help='Include copybooks when resolving paragraph context in symbol refs extraction')
    ap.add_argument('--no-copy-embed', action='store_true', help='Skip embedding for copybook usage ingestion')
    args=ap.parse_args()

    # Prepare commands with limits if provided
    extract_cmd = SYMBOL_EXTRACT[:]
    extract_cmd += ['--recreate-first']
    if args.symbol_limit:
        extract_cmd += ['--limit', str(args.symbol_limit)]
    if args.include_copybooks:
        extract_cmd += ['--include-copybooks']
    extract_cmd += ['--source-root', args.source_root]

    ingest_cmd = COPY_INGEST[:]
    ingest_cmd[ingest_cmd.index('--roots')+1] = args.source_root
    if args.copy_limit:
        ingest_cmd += ['--limit', str(args.copy_limit)]
    if args.no_copy_embed:
        ingest_cmd += ['--no-embed']

    # Run steps
    run(SYMBOL_CREATE, 'Recreate symbol refs index')
    run(extract_cmd, 'Extract symbol refs')
    run(COPY_CREATE, 'Recreate copybook usage index')
    run(ingest_cmd, 'Ingest copybook usage')

    print('\n[SUMMARY] Rebuild complete. Consider running diagnostics/probe_symbol_refs.py again.')

if __name__=='__main__':
    main()
