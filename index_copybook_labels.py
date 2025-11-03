"""Index copybook labels (record level data names) from .cpy files.

Outputs JSONL/copybook_labels.jsonl with records:
{
  "copybook_file": str,              # filename without path
  "path": str,                      # absolute path to copybook
  "label": str,                     # data name or section marker extracted
  "level": str,                     # COBOL level number (e.g., '01','05')
  "occurs": Optional[int],          # OCCURS value if present
  "redefines": Optional[str],       # target name if REDEFINES present
  "line": int,                      # line number within copybook
  "source_snippet": str             # original line trimmed
}

Heuristics:
 - Treat level numbers 01/77/66 as top-level labels (primary anchors) and
   also include any other levels as secondary; callers can filter.
 - Ignore comment lines starting with *> or * in area A (col 7) after stripping.
 - Collapse multiple spaces; preserve original label case (uppercased output).

Rationale:
 Copybook data names often become part of PERFORM / paragraph or variable
 naming conventions. Indexing them allows unresolved edges whose raw_target
 matches a copybook-defined name (or concatenations with action verbs) to be
 disambiguated.
"""

from __future__ import annotations

import os
import re
import json
from pathlib import Path

COPYBOOK_EXT = ".cpy"
OUTPUT_DIR = Path("JSONL")
OUTPUT_PATH = OUTPUT_DIR / "copybook_labels.jsonl"

LEVEL_RE = re.compile(r"^(?P<seq>\d{0,6})?\s*(?P<level>\d{2})\s+(?P<name>[A-Z0-9-]+)(?P<rest>.*)$", re.IGNORECASE)
OCCURS_RE = re.compile(r"OCCURS\s+(\d+)\s+TIMES", re.IGNORECASE)
REDEFINES_RE = re.compile(r"REDEFINES\s+([A-Z0-9-]+)", re.IGNORECASE)

def find_copybooks(root: Path) -> list[Path]:
    files = []
    for p in root.rglob(f"*{COPYBOOK_EXT}"):
        # skip very large or binary-like files (heuristic)
        try:
            if p.stat().st_size > 2_000_000:  # 2MB safety
                continue
        except OSError:
            continue
        files.append(p)
    return files

def parse_copybook(path: Path) -> list[dict]:
    results: list[dict] = []
    try:
        text = path.read_text(encoding="utf-8", errors="ignore").splitlines()
    except Exception as e:  # noqa: BLE001
        print(f"WARN: unable to read {path}: {e}")
        return results

    for idx, raw in enumerate(text, start=1):
        line = raw.rstrip("\n\r")
        stripped = line.strip()
        if not stripped:
            continue
        # Skip comment lines (*> inline comment or * in area A after optional sequence)
        area_a = stripped.lstrip("0123456789")  # remove possible sequence numbers first
        if area_a.startswith("*>") or (area_a.startswith("*") and not area_a.startswith("**")):
            continue
        m = LEVEL_RE.match(line)
        if not m:
            continue
        level = m.group("level")
        name = m.group("name").upper()
        rest = m.group("rest") or ""
        occurs = None
        redefines = None
        mo = OCCURS_RE.search(rest)
        if mo:
            try:
                occurs = int(mo.group(1))
            except ValueError:
                occurs = None
        mr = REDEFINES_RE.search(rest)
        if mr:
            redefines = mr.group(1).upper()
        results.append({
            "copybook_file": path.name,
            "path": str(path),
            "label": name,
            "level": level,
            "occurs": occurs,
            "redefines": redefines,
            "line": idx,
            "source_snippet": stripped,
        })
    return results

def main():
    root = Path('.')
    copybooks = find_copybooks(root)
    print(f"Discovered {len(copybooks)} copybook files")
    all_records: list[dict] = []
    for cb in copybooks:
        recs = parse_copybook(cb)
        all_records.extend(recs)
        print(f"Indexed {len(recs):4d} labels from {cb}")

    if not all_records:
        print("No copybook labels found; nothing to write.")
        return

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    with OUTPUT_PATH.open('w', encoding='utf-8') as f:
        for r in all_records:
            f.write(json.dumps(r) + "\n")
    print(f"Wrote {len(all_records)} records to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
