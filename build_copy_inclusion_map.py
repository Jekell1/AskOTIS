"""Build a map of COBOL program source files to included copybooks.

Output: JSONL/copy_inclusions.jsonl where each line is:
{
  "program_file": str,          # base filename of the program (without dirs)
  "path": str,                  # absolute path to program source
  "copybook": str,              # copybook name referenced (as written)
  "normalized_copybook": str,   # normalized upper-case name without extension
  "line": int,                  # line number of COPY statement
  "source_snippet": str         # original trimmed line
}

We scan for lines containing COPY <name> [REPLACING ...]. We tolerate sequence numbers.
We ignore comment lines beginning with * or *> after removing sequence area.

Later, we will join this with copybook_labels to map which labels are in scope for each program.
"""

from __future__ import annotations

import re
import json
from pathlib import Path
from typing import Iterable

PROGRAM_EXTS = [".cbl", ".cob", ".cobol"]
OUTPUT_DIR = Path("JSONL")
OUTPUT_PATH = OUTPUT_DIR / "copy_inclusions.jsonl"

COPY_RE = re.compile(r"\bCOPY\s+([A-Z0-9_\-]+)(?:\s+REPLACING|\.|\s|$)", re.IGNORECASE)

def iter_program_files(root: Path) -> Iterable[Path]:
    for ext in PROGRAM_EXTS:
        yield from root.rglob(f"*{ext}")

def is_comment(line: str) -> bool:
    stripped = line.strip()
    if not stripped:
        return True
    # remove potential sequence number (first 6 digits) before checking
    seq_stripped = stripped.lstrip('0123456789')
    return seq_stripped.startswith('*')

def scan_file(path: Path):
    try:
        lines = path.read_text(encoding='utf-8', errors='ignore').splitlines()
    except Exception as e:  # noqa: BLE001
        print(f"WARN: unable to read {path}: {e}")
        return []
    results = []
    for idx, raw in enumerate(lines, start=1):
        if is_comment(raw):
            continue
        m = COPY_RE.search(raw)
        if not m:
            continue
        name = m.group(1)
        norm = name.upper().removesuffix('.CPY')
        results.append({
            "program_file": path.name,
            "path": str(path),
            "copybook": name,
            "normalized_copybook": norm,
            "line": idx,
            "source_snippet": raw.strip(),
        })
    return results

def main():
    root = Path('.')
    all_rows = []
    for prog in iter_program_files(root):
        rows = scan_file(prog)
        if rows:
            all_rows.extend(rows)
            print(f"{prog} -> {len(rows)} COPY refs")
    if not all_rows:
        print("No COPY statements found.")
        return
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    with OUTPUT_PATH.open('w', encoding='utf-8') as f:
        for r in all_rows:
            f.write(json.dumps(r) + '\n')
    print(f"Wrote {len(all_rows)} records to {OUTPUT_PATH}")

if __name__ == '__main__':
    main()
