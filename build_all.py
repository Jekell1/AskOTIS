"""Orchestrate creation and ingestion of all major COBOL search indexes.

Usage:
  python build_all.py --indexes --ingest --backfill --dry-run

Flags:
  --indexes   Create (or recreate) core indexes if they do not exist.
  --ingest    Run ingestion/build scripts to populate indexes.
  --backfill  Run embedding / vector backfill scripts.
  --dry-run   Print the planned commands without executing.

Environment: Relies on local.settings.json for endpoint/key or env vars.
"""
from __future__ import annotations
import argparse, subprocess, sys, shutil, os

# Ordered script groups
INDEX_CREATORS = [
  'create_program_flows_index.py',
  'create_program_deps_index.py',
  'create_name_aliases_index.py',
  'create_paragraphs_v3_index.py',
  'create_flow_edges_index_new.py',
  'create_facts_v3_index.py',
  'create_cobol_file_index.py',
  'create_cobol_file_chunk_index.py',
]

INGEST_SCRIPTS = [
  'ingest/build_aliases.py',
  'build_program_inventory.py',
  'build_program_meta.py',
  'build_symbol_refs.py',
  'build_variable_usage.py',
  'ingest/build_copybook_meta.py',
  'ingest/build_copybook_usage.py',
  'build_program_flows.py',
  'ingest/build_program_deps.py',
  'ingest/build_screen_nodes.py',
  'ingest/build_ui_paths.py',
]

BACKFILL_SCRIPTS = [
  'backfill_embeddings.py',
  'backfill_embeddings_program_deps.py',
  'backfill_flow_embeddings.py',
  'backfill_embeddings_program_meta.py',
  'backfill_symbol_ref_embeddings.py',
  'backfill_symbol_ref_embeddings_ids.py',
  'backfill_screen_node_embeddings.py',
  'backfill_embeddings_ui_paths.py',
  'backfill_ui_path_embeddings.py',
  'backfill_variable_usage_vectors.py',
]

PYTHON = shutil.which('python') or shutil.which('python3') or 'python'


def run_script(script: str, extra_args: list[str]|None=None, dry=False):
    extra_args = extra_args or []
    cmd = [PYTHON, script] + extra_args
    if dry:
        print('[DRY]', ' '.join(cmd))
        return 0
    print('[RUN]', ' '.join(cmd))
    proc = subprocess.run(cmd, stdout=sys.stdout, stderr=sys.stderr)
    return proc.returncode


def phase(label: str):
    print('\n====', label, '====')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--indexes', action='store_true', help='Run index creation scripts')
    ap.add_argument('--ingest', action='store_true', help='Run build/ingest scripts')
    ap.add_argument('--backfill', action='store_true', help='Run embedding/vector backfill scripts')
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    if not any([args.indexes, args.ingest, args.backfill]):
        print('Specify at least one phase: --indexes --ingest --backfill')
        return 1

    failures = 0

    if args.indexes:
        phase('INDEX CREATION')
        for script in INDEX_CREATORS:
            if not os.path.exists(script):
                print('[SKIP] missing', script)
                continue
            rc = run_script(script, dry=args.dry_run)
            if rc!=0:
                print('[ERROR] index creator failed', script)
                failures += 1

    if args.ingest:
        phase('INGESTION BUILD')
        for script in INGEST_SCRIPTS:
            if not os.path.exists(script):
                print('[SKIP] missing', script)
                continue
            # push/ingest flag heuristics
            extra=['--push'] if 'ingest/' in script or script.startswith('build_') else []
            rc = run_script(script, extra, dry=args.dry_run)
            if rc!=0:
                print('[ERROR] ingest script failed', script)
                failures += 1

    if args.backfill:
        phase('BACKFILL')
        for script in BACKFILL_SCRIPTS:
            if not os.path.exists(script):
                print('[SKIP] missing', script)
                continue
            rc = run_script(script, dry=args.dry_run)
            if rc!=0:
                print('[ERROR] backfill script failed', script)
                failures += 1

    if failures:
        print(f'Completed with {failures} failures')
        return 2
    print('All selected phases completed successfully')
    return 0

if __name__ == '__main__':
    raise SystemExit(main())
