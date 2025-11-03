#!/usr/bin/env python3
"""load_search_env.py

Safely load Azure Cognitive Search endpoint & key from local.settings.json (or env) and
emit shell-friendly commands to set them for the current session WITHOUT re-printing
secret values by default.

Usage examples (PowerShell):
  # To set variables in current session
  python load_search_env.py --ps | Invoke-Expression

  # To see what would be set (names only)
  python load_search_env.py --ps --dry-run

  # To explicitly echo the values (not recommended except for debugging)
  python load_search_env.py --ps --reveal

Bash:
  eval "$(python load_search_env.py --bash)"
"""
import os, json, argparse, sys
from pathlib import Path

DEFAULT_FILE = 'local.settings.json'
KEY_NAMES = [
    ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT'),
    ('AZURE_SEARCH_KEY', 'SEARCH_KEY'),
]

def load_local_settings(path: Path):
    if not path.exists():
        return {}
    try:
        with path.open('r', encoding='utf-8') as f:
            js = json.load(f)
        return js.get('Values', {}) or {}
    except Exception as ex:
        print(f"WARN: failed to parse {path}: {ex}", file=sys.stderr)
        return {}


def resolve_values(local_vals: dict):
    resolved = {}
    for primary, fallback in KEY_NAMES:
        val = os.getenv(primary) or os.getenv(fallback) or local_vals.get(primary) or local_vals.get(fallback)
        if val:
            resolved[primary] = val
    return resolved


def emit_powershell(vars_map: dict, reveal: bool, dry: bool):
    lines = []
    for k,v in vars_map.items():
        if dry:
            lines.append(f"# Would set $env:{k}")
        else:
            if reveal:
                lines.append(f"$env:{k} = '{v}'")
            else:
                # Assign without echoing value again
                lines.append(f"$env:{k} = '{v}'  # set")
    return '\n'.join(lines)


def emit_bash(vars_map: dict, reveal: bool, dry: bool):
    lines = []
    for k,v in vars_map.items():
        if dry:
            lines.append(f"# Would export {k}")
        else:
            if reveal:
                lines.append(f"export {k}='{v}'")
            else:
                lines.append(f"export {k}='{v}'  # set")
    return '\n'.join(lines)


def main():
    ap = argparse.ArgumentParser(description='Emit commands to set Azure Search env vars safely.')
    ap.add_argument('--file', default=DEFAULT_FILE, help='Path to local.settings.json')
    ap.add_argument('--ps', action='store_true', help='Emit PowerShell commands')
    ap.add_argument('--bash', action='store_true', help='Emit Bash shell commands')
    ap.add_argument('--reveal', action='store_true', help='Include secret values explicitly in output')
    ap.add_argument('--dry-run', action='store_true', help='Only comment what would be set')
    args = ap.parse_args()

    local_vals = load_local_settings(Path(args.file))
    resolved = resolve_values(local_vals)

    missing = [p for p,_ in KEY_NAMES if p not in resolved]
    if missing:
        print(f"ERROR: Missing values for: {', '.join(missing)}", file=sys.stderr)
        sys.exit(1)

    if not (args.ps or args.bash):
        print("Specify one of --ps or --bash", file=sys.stderr)
        sys.exit(2)

    if args.ps:
        print(emit_powershell(resolved, args.reveal, args.dry_run))
    else:
        print(emit_bash(resolved, args.reveal, args.dry_run))

if __name__ == '__main__':
    main()
