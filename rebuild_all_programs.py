"""Full rebuild orchestrator for all COBOL program artifacts.

Purpose:
  One-stop script to (optionally) recreate core indexes and repopulate them from source:
    1. Code chunks
    2. Program dependency summaries
    3. Program flow graphs
    4. UI paths
    5. Screen nodes
    6. Variable usage aggregates
    7. Symbol reference occurrences (via xref extraction)
    8. Context + embeddings for symbol refs
    9. Embeddings for deps, UI paths, screen nodes, variable usage (if scripts exist)

High-Level Steps (configurable via flags):
  (A) Recreate indexes (destructive) if --recreate-indexes provided.
  (B) Ingest / build each layer.
  (C) Run enrichment & coverage validation.

Assumptions:
  * Required create_*.py, build_*.py, ingest_*.py, backfill_* scripts exist in current directory.
  * Environment variables for Search + Embeddings are configured (or local.settings.json present).
  * COBOL sources under COBOL_SOURCE_ROOT (default cobol_src).

Usage examples:
  python rebuild_all_programs.py --all
  python rebuild_all_programs.py --recreate-indexes --layers chunks,deps,flows,ui,screen,vars,symbols --embed --validate
  python rebuild_all_programs.py --layers symbols --harvest-context --embed-symbol-refs

Exit code is non-zero on first failing step unless --continue-on-error is set.
"""
from __future__ import annotations
import argparse, subprocess, sys, shlex, time

DEFAULT_LAYER_ORDER = [
    'chunks', 'deps', 'flows', 'ui', 'screen', 'vars', 'symbols'
]

CREATE_SCRIPTS = {
    'chunks': 'recreate_code_chunks_index.py',  # this script usually handles recreate
    'deps': 'create_program_deps_index.py',
    'flows': 'create_program_flows_index.py',
    'ui': 'create_ui_paths_index.py',
    'screen': 'create_screen_nodes_index.py',
    'vars': 'create_variable_usage_index.py',
    'symbols': 'create_symbol_refs_index.py'
}

BUILD_SCRIPTS = {
    'chunks': 'ingest_code_chunks.py',
    'deps': 'build_program_deps.py',
    'flows': 'build_program_flows.py',
    'ui': 'build_ui_paths.py',
    'screen': 'build_screen_nodes.py',
    'vars': 'build_variable_usage.py',
    'symbols': 'extract_symbol_refs_from_xrefs.py'
}

EMBED_SCRIPTS = [
    # program deps
    'backfill_embeddings_program_deps.py',
    # ui paths
    'backfill_ui_path_embeddings.py',
    # screen nodes
    'backfill_screen_node_embeddings.py',
    # variable usage (if vector field script has been run separately)
    'backfill_embeddings_variable_usage.py',
]

SYMBOL_REF_ENRICH_PIPELINE = [
    'harvest_symbol_ref_context.py',
    'backfill_symbol_ref_embeddings.py'
]

VALIDATION_SCRIPTS = [
    'symbol_refs_embedding_coverage.py',
    'validate_symbol_refs_vs_usage.py --sample 60000',
    'run_full_regression.py'
]

def run(cmd: str, continue_on_error: bool=False, verbose: bool=True):
    if verbose:
        print(f"\n>>> {cmd}")
    t0=time.time()
    proc=subprocess.run(shlex.split(cmd), stdout=sys.stdout, stderr=sys.stderr)
    dur=time.time()-t0
    if verbose:
        print(f"[DONE] {cmd} ({dur:.1f}s exit={proc.returncode})")
    if proc.returncode!=0 and not continue_on_error:
        print(f"[FATAL] Step failed: {cmd}")
        sys.exit(proc.returncode)
    return proc.returncode

def main():
    ap=argparse.ArgumentParser(description='Full rebuild orchestrator for COBOL program indexes')
    ap.add_argument('--layers', help='Comma list subset of layers (chunks,deps,flows,ui,screen,vars,symbols)')
    ap.add_argument('--all', action='store_true', help='Shorthand for all layers in default order')
    ap.add_argument('--recreate-indexes', action='store_true', help='Recreate all chosen indexes before building')
    ap.add_argument('--embed', action='store_true', help='Run general embedding backfills (deps, ui paths, screen nodes, variable usage)')
    ap.add_argument('--embed-symbol-refs', action='store_true', help='Run symbol ref embedding backfill')
    ap.add_argument('--harvest-context', action='store_true', help='Harvest context_before/after for symbol refs')
    ap.add_argument('--validate', action='store_true', help='Run validation scripts at end')
    ap.add_argument('--continue-on-error', action='store_true')
    ap.add_argument('--dry-run', action='store_true', help='Print plan but do not execute')
    ap.add_argument('--limit', type=int, help='Forwarded to build scripts supporting --limit / --top')
    ap.add_argument('--symbol-limit', type=int, help='Limit for symbol extraction step')
    ap.add_argument('--verbose', action='store_true')
    args=ap.parse_args()

    # Determine layers
    if args.all or not args.layers:
        layers = DEFAULT_LAYER_ORDER[:] if args.all or not args.layers else []
    else:
        layers=[x.strip() for x in args.layers.split(',') if x.strip()]
    unknown=[l for l in layers if l not in DEFAULT_LAYER_ORDER]
    if unknown:
        print(f"Unknown layers: {unknown}")
        sys.exit(1)

    plan=[]
    if args.recreate_indexes:
        for l in layers:
            cs=CREATE_SCRIPTS.get(l)
            if not cs: continue
            # some create scripts need --force or --overwrite
            force_flag='--force' if 'symbol_refs' in cs else '--overwrite'
            if cs=='recreate_code_chunks_index.py':
                # this script name implies recreate itself
                plan.append(f"python {cs}")
            else:
                plan.append(f"python {cs} {force_flag}")
    # Build / ingest
    for l in layers:
        bs=BUILD_SCRIPTS.get(l)
        if not bs: continue
        limit_arg=''
        if args.limit and l!='symbols':
            if '--limit' in open(bs,'r',encoding='utf-8',errors='ignore').read():
                limit_arg=f" --limit {args.limit}"
            elif '--top' in open(bs,'r',encoding='utf-8',errors='ignore').read():
                limit_arg=f" --top {args.limit}"
        if l=='symbols' and args.symbol_limit:
            if '--limit' in open(bs,'r',encoding='utf-8',errors='ignore').read():
                limit_arg=f" --limit {args.symbol_limit}"
            elif '--max' in open(bs,'r',encoding='utf-8',errors='ignore').read():
                limit_arg=f" --max {args.symbol_limit}"
        plan.append(f"python {bs}{limit_arg}")
    # Symbol ref enrichment pipeline
    if args.harvest_context or args.embed_symbol_refs:
        if args.harvest_context:
            plan.append("python harvest_symbol_ref_context.py")
        if args.embed_symbol_refs:
            plan.append("python backfill_symbol_ref_embeddings.py")
    # General embeddings
    if args.embed:
        for s in EMBED_SCRIPTS:
            plan.append(f"python {s}")
    # Validation
    if args.validate:
        for s in VALIDATION_SCRIPTS:
            plan.append(f"python {s}")

    if args.dry_run:
        print('Rebuild PLAN:')
        for p in plan:
            print('  ', p)
        return

    print('Executing rebuild plan:')
    for p in plan:
        run(p, continue_on_error=args.continue_on_error, verbose=args.verbose or True)
    print('\nRebuild complete.')

if __name__=='__main__':
    main()
