"""Scan workspace for actionable TODO/FIXME items.

Excludes:
  * .venv/ (third-party libs)
  * node_modules/ (if present)
  * build / dist folders

Matches comment-style markers (case-insensitive):
  - TODO
  - FIXME
  - HACK

Ignores occurrences where the token appears inside a variable name (e.g. 'todo=[]').
Outputs JSON summary to stdout:
{
  "total": int,
  "by_file": { "path": count, ... },
  "items": [ {"file": path, "line": n, "text": "..."}, ... ]
}

Usage:
  python list_todos.py [--root .] [--limit 500] [--show]

--show prints each TODO line in a readable form after JSON block.
"""
from __future__ import annotations
import os, re, json, argparse, sys

TOKEN_RE = re.compile(r'\b(TODO|FIXME|HACK)\b', re.IGNORECASE)
COMMENT_LINE_RE = re.compile(r'^[^\n]*?(#|//|/\*|<!--).*$')  # heuristic for comment-containing lines
EXCLUDE_DIRS = {'.git', '.venv', 'node_modules', 'dist', 'build', '.pytest_cache', '.mypy_cache'}
TEXT_EXTS = {'.py', '.md', '.txt', '.yaml', '.yml', '.json', '.cfg', '.ini', '.sh', '.ps1'}


def is_text_candidate(path: str) -> bool:
    _, ext = os.path.splitext(path)
    if ext.lower() in TEXT_EXTS:
        return True
    # Allow COBOL sources too
    if ext.upper() in ('.CBL', '.COB', '.CPY'):
        return True
    return False


def scan(root: str, limit: int):
    items = []
    by_file = {}
    for dirpath, dirnames, filenames in os.walk(root):
        # prune excluded dirs in-place
        dirnames[:] = [d for d in dirnames if d not in EXCLUDE_DIRS]
        for f in filenames:
            p = os.path.join(dirpath, f)
            if not is_text_candidate(p):
                continue
            try:
                with open(p, 'r', encoding='utf-8', errors='ignore') as fh:
                    for lineno, line in enumerate(fh, 1):
                        if len(items) >= limit:
                            break
                        if 'todo' not in line.lower() and 'fixme' not in line.lower() and 'hack' not in line.lower():
                            continue
                        if not TOKEN_RE.search(line):
                            continue
                        # skip plain variable assignments like todo=[] (no comment marker)
                        if 'todo' in line.lower() and '=' in line and '#' not in line and 'todo' in line.lower().strip().split()[0]:
                            # Likely a variable, not a comment
                            continue
                        # Prefer lines that look like comments or contain # / // / /* / <!--
                        if not COMMENT_LINE_RE.search(line):
                            continue
                        rel = os.path.relpath(p, root)
                        items.append({'file': rel.replace('\\','/'), 'line': lineno, 'text': line.strip()})
                        by_file[rel] = by_file.get(rel, 0) + 1
            except Exception:
                continue
    return items, by_file


def main():
    ap = argparse.ArgumentParser(description='List actionable TODO/FIXME/HACK markers.')
    ap.add_argument('--root', default='.', help='Root folder to scan')
    ap.add_argument('--limit', type=int, default=500, help='Max items to collect')
    ap.add_argument('--show', action='store_true', help='Show each TODO line below JSON summary')
    args = ap.parse_args()

    items, by_file = scan(args.root, args.limit)
    summary = {
        'total': len(items),
        'by_file': dict(sorted(by_file.items(), key=lambda x: (-x[1], x[0]))),
        'items': items
    }
    print(json.dumps(summary, indent=2))
    if args.show:
        print('\n--- TODO LIST ---')
        for it in items:
            print(f"{it['file']}:{it['line']}: {it['text']}")

if __name__ == '__main__':
    main()
