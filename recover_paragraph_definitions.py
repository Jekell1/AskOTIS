"""Recover genuine paragraph definitions from COBOL source files.

Outputs JSONL records written to JSONL/paragraph_definitions.jsonl:
  {
    "paragraph": <UPPER_NAME>,
    "file_path": <full path>,
    "file_id": <basename without extension upper>,
    "start_line": <int>,
    "end_line": <int>,          # inclusive
    "label_line_text": <original line>,
    "body_line_count": <int>,
    "section": <optional SECTION name>,
    "comments_above": [<str>, ...],   # contiguous comment lines immediately above (up to 5)
    "hash": <stable hash of body>,
    "source": "recovered-v1"
  }

Heuristics:
  - Paragraph label pattern: starts at Area A (cols 8-11 traditionally) but we handle free form: a token ending with a period followed by whitespace and not a recognized reserved word list (e.g., PROCEDURE, SECTION) OR a label line with no leading column numbers and followed by code lines.
  - We'll approximate: A line whose first non-space token ends with a period and is ALL CAPS / letters / digits / hyphens and length <= 56 and not in SECTION-related keywords.
  - We'll treat lines ending with 'SECTION.' as SECTION markers; paragraphs belong to the most recent section until next.
  - End of paragraph = next paragraph label, next section label, or EOF.
  - Comment lines: those beginning with *> or * in column 7 (classic) or lines starting with '      *'. We'll collect up to 5 contiguous comment lines immediately preceding a paragraph label (skipping blank lines as barrier).

Usage:
  python recover_paragraph_definitions.py --root cobol_src --out JSONL/paragraph_definitions.jsonl

Environment variables:
  COBOL_ROOT (default cobol_src)

Limitations: This is heuristic; complex source formatting may require refinement.
"""
from __future__ import annotations
import os, re, json, argparse, hashlib
from pathlib import Path
from typing import List, Optional

COBOL_EXTS = {'.cbl','.cob','.cobol'}  # exclude copybooks .cpy (paragraphs usually in programs)
LABEL_RE = re.compile(r"^\s*([A-Za-z0-9][A-Za-z0-9-]{0,55})\.")
SECTION_RE = re.compile(r"^\s*([A-Za-z0-9][A-Za-z0-9-]{0,55})\s+SECTION\.", re.IGNORECASE)
RESERVED = { 'PROCEDURE','DIVISION','SECTION','ENVIRONMENT','IDENTIFICATION','DATA','WORKING-STORAGE','LINKAGE','FILE','FD','SD','SELECT','COPY' }

COMMENT_PATTERNS = [
    re.compile(r"^\s*\*>"),  # modern comment
    re.compile(r"^.{0,6}\*.*")  # classic asterisk in indicator area
]

def is_comment(line: str) -> bool:
    if not line.strip():
        return False
    for pat in COMMENT_PATTERNS:
        if pat.match(line):
            return True
    return False

def compute_hash(lines: List[str]) -> str:
    h = hashlib.sha1()
    for ln in lines:
        h.update(ln.rstrip('\n').encode('utf-8'))
        h.update(b"\n")
    return h.hexdigest()

def iter_source_files(root: str):
    for base, dirs, files in os.walk(root):
        for fn in files:
            ext = os.path.splitext(fn)[1].lower()
            if ext in COBOL_EXTS:
                yield os.path.join(base, fn)

def collect_paragraphs(path: str):
    recs = []
    try:
        with open(path,'r',encoding='utf-8',errors='ignore') as f:
            lines = f.readlines()
    except Exception:
        return recs

    current_section: Optional[str] = None
    labels = []  # (name, line_index, is_section)
    for idx, raw in enumerate(lines):
        line = raw.rstrip('\n')
        # Section detection
        sm = SECTION_RE.match(line)
        if sm:
            sec = sm.group(1).upper()
            current_section = sec
            labels.append((sec, idx, True))
            continue
        m = LABEL_RE.match(line)
        if m:
            name = m.group(1).upper()
            if name in RESERVED:
                continue
            # filter out SECTION lines already handled
            if line.upper().strip().endswith(' SECTION.'):
                continue
            labels.append((name, idx, False))

    # Build paragraph records
    for i, (name, start_idx, is_section) in enumerate(labels):
        if is_section:
            continue  # skip adding section itself as paragraph
        # Determine end
        end_idx = len(lines)-1
        for j in range(i+1, len(labels)):
            _, nxt_line_idx_isect = labels[j][0], labels[j][2]
            nxt_name, nxt_idx, nxt_is_section = labels[j]
            if nxt_is_section or not nxt_is_section:
                end_idx = nxt_idx - 1
                break
        body_lines = lines[start_idx+1:end_idx+1]
        # Gather preceding comments
        comments = []
        scan_idx = start_idx - 1
        while scan_idx >=0 and len(comments) < 5:
            prev_line = lines[scan_idx].rstrip('\n')
            if not prev_line.strip():
                break  # blank stops collection
            if is_comment(prev_line):
                comments.append(prev_line.strip())
                scan_idx -= 1
                continue
            else:
                break
        comments.reverse()
        recs.append({
            'paragraph': name,
            'file_path': path,
            'file_id': Path(path).stem.upper(),
            'start_line': start_idx+1,
            'end_line': end_idx+1,
            'label_line_text': lines[start_idx].rstrip('\n'),
            'body_line_count': len(body_lines),
            'section': current_section,
            'comments_above': comments,
            'hash': compute_hash(body_lines),
            'source': 'recovered-v1'
        })
    return recs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', default=os.environ.get('COBOL_ROOT','cobol_src'))
    ap.add_argument('--out', default='JSONL/paragraph_definitions.jsonl')
    args = ap.parse_args()
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    total=0
    files=0
    with open(args.out,'w',encoding='utf-8') as fout:
        for path in iter_source_files(args.root):
            files+=1
            recs = collect_paragraphs(path)
            for r in recs:
                fout.write(json.dumps(r)+'\n')
            total += len(recs)
    print(f"Recovered {total} paragraph definitions from {files} files -> {args.out}")

if __name__=='__main__':
    main()
