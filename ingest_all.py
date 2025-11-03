"""Orchestrate ingestion for all vector-enabled COBOL indexes.

Runs in sequence (symbols, paragraphs, calls, xrefs, chunks) with shared flags.

Usage:
  python ingest_all.py --root C:\path\to\cobol --dry-run --limit 100
"""
import os, subprocess, sys, argparse, shutil

SCRIPTS = [
    ('cobol-symbols', 'ingest_cobol_symbols.py'),
    ('cobol-paragraphs', 'ingest_cobol_paragraphs.py'),
    ('cobol-calls', 'ingest_cobol_calls.py'),
    ('cobol-xrefs', 'ingest_cobol_xrefs.py'),
    ('code-chunks', 'ingest_code_chunks.py'),
]

def run_script(py: str, script: str, args: list[str]):
    cmd = [py, script] + args
    print(f"\n=== Running: {' '.join(cmd)}")
    res = subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode != 0:
        print(f"ERROR ({script}) code={res.returncode}\nSTDERR:\n{res.stderr[:4000]}")
    else:
        print(res.stdout[:4000])
    return res.returncode

def main():
    parser = argparse.ArgumentParser(description='Run all ingestion scripts sequentially.')
    parser.add_argument('--root', default=None, help='COBOL source root override (sets COBOL_SOURCE_ROOT env)')
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--limit', type=int, default=None)
    parser.add_argument('--window', type=int, default=25, help='Chunk window size (for code-chunks)')
    parser.add_argument('--skip', action='append', default=[], help='Index names to skip (repeatable)')
    args = parser.parse_args()

    if args.root:
        os.environ['COBOL_SOURCE_ROOT'] = args.root
    root = os.environ.get('COBOL_SOURCE_ROOT')
    if not root or not os.path.isdir(root):
        print(f"Source root '{root}' not found. Exiting.")
        return 2

    py_exec = sys.executable
    common_flags = []
    if args.dry_run:
        common_flags.append('--dry-run')
    if args.limit:
        common_flags += ['--limit', str(args.limit)]

    exit_code = 0
    for idx_name, script in SCRIPTS:
        if idx_name in args.skip:
            print(f"Skipping {idx_name} per --skip")
            continue
        script_flags = list(common_flags)
        if idx_name == 'code-chunks' and args.window:
            script_flags += ['--window', str(args.window)]
        rc = run_script(py_exec, script, script_flags)
        if rc != 0:
            exit_code = rc
    print("\nAll ingestion scripts complete.")
    return exit_code

if __name__ == '__main__':
    raise SystemExit(main())
