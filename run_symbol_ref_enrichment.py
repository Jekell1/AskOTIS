"""Orchestrate end-to-end symbol reference enrichment.

Steps (configurable):
  1. Harvest context (context_before/context_after) if requested
  2. Backfill embeddings for excerpts lacking vectors (optional)
  3. Produce coverage report

This is a light wrapper around existing scripts to simplify iterative runs.

Usage:
  python run_symbol_ref_enrichment.py --harvest --embed --coverage
  python run_symbol_ref_enrichment.py --harvest --window 5 --embed-batch 128

Requires the other scripts to be in the same workspace:
  harvest_symbol_ref_context.py
  backfill_symbol_ref_embeddings.py
  symbol_refs_embedding_coverage.py
"""
from __future__ import annotations
import os, argparse, subprocess, sys

def run(cmd:list[str]):
    print('\n>>>',' '.join(cmd))
    r=subprocess.run(cmd,stdout=sys.stdout,stderr=sys.stderr)
    if r.returncode!=0:
        print(f"Command failed: {' '.join(cmd)} exit={r.returncode}")
        sys.exit(r.returncode)

def main():
    ap=argparse.ArgumentParser(description='Symbol ref enrichment orchestrator')
    ap.add_argument('--harvest',action='store_true',help='Run context harvesting step')
    ap.add_argument('--embed',action='store_true',help='Run embedding backfill for missing vectors')
    ap.add_argument('--coverage',action='store_true',help='Print coverage report at end')
    ap.add_argument('--window',type=int,default=3,help='Window lines for harvesting (passed to harvest script)')
    ap.add_argument('--harvest-batch',type=int,default=500)
    ap.add_argument('--embed-batch',type=int,default=256)
    ap.add_argument('--embed-limit',type=int,default=0)
    ap.add_argument('--harvest-limit',type=int,default=0)
    args=ap.parse_args()
    if args.harvest:
        run(['python','harvest_symbol_ref_context.py','--window',str(args.window),'--batch',str(args.harvest_batch)]+(['--limit',str(args.harvest_limit)] if args.harvest_limit else []))
    if args.embed:
        run(['python','backfill_symbol_ref_embeddings.py','--batch',str(args.embed_batch)] + (['--limit',str(args.embed_limit)] if args.embed_limit else []))
    if args.coverage:
        run(['python','symbol_refs_embedding_coverage.py'])
    if not (args.harvest or args.embed or args.coverage):
        ap.print_help()

if __name__=='__main__':
    main()
