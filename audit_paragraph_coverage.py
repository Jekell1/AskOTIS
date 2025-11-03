#!/usr/bin/env python3
"""Audit paragraph coverage vs filesystem COBOL programs.

Objectives:
 1. Enumerate all COBOL source files under provided roots (default: SRC/, SOURCE/, COBOL/, .).
 2. Derive canonical program name for each (strip extension, upper, remove non A-Z0-9- underscore to dash normalization like existing tools if needed).
 3. Query paragraph index (default: cobol-paragraphs-v3) for distinct program names present (program_id or program_name field heuristics).
 4. Produce classification:
      - missing: present on filesystem, absent in index.
      - present_no_text (if paragraph docs missing 'text' field or it's empty for that program).
      - present_with_text.
 5. Optional cross-check of facts index (if --facts-index provided) to spot programs with facts but no paragraphs or vice versa.
 6. Emit JSON summary and textual report.

Usage:
  python audit_paragraph_coverage.py --paragraph-index cobol-paragraphs-v3 --facts-index cobol-facts-v3l --roots SRC COBOL > coverage_report.json

Notes:
  - Uses simple REST calls like other scripts; relies on AZURE_SEARCH_ENDPOINT and AZURE_SEARCH_API_KEY env vars.
  - Limits: if program count large, we page using search=*, top=1000 and @search.nextPageParameters; continue until done.
"""
from __future__ import annotations
import os, re, json, argparse, sys, time
import requests
from typing import List, Dict, Set, Tuple

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-05-01-preview")

DEFAULT_ROOTS = ["SRC", "SOURCE", "COBOL", "."]
PROGRAM_RE = re.compile(r"[^A-Z0-9_\-]")

SEARCH_ENDPOINT = os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT")
SEARCH_KEY = os.environ.get("AZURE_SEARCH_API_KEY") or os.environ.get("SEARCH_KEY")

# Heuristic field candidates that may carry program identity in paragraph index
PROGRAM_FIELD_CANDIDATES = ["program_id", "program", "program_name"]
TEXT_FIELD_CANDIDATES = ["text", "paragraph_text", "para_text"]

SESSION = requests.Session()

def norm_program(name: str) -> str:
    n = name.upper()
    n = PROGRAM_RE.sub('-', n)
    return n

def find_cobol_files(roots: List[str]) -> List[str]:
    exts = {'.CBL', '.COB'}
    files = []
    for root in roots:
        if not os.path.isdir(root):
            continue
        for dirpath, _, filenames in os.walk(root):
            for fn in filenames:
                ext = os.path.splitext(fn)[1].upper()
                if ext in exts:
                    files.append(os.path.join(dirpath, fn))
    return files

def derive_program_name(path: str) -> str:
    base = os.path.basename(path)
    name = os.path.splitext(base)[0]
    return norm_program(name)

def search_all_docs(index: str, select: str = None) -> List[Dict]:
    if not SEARCH_ENDPOINT or not SEARCH_KEY:
        raise SystemExit("Missing AZURE_SEARCH_ENDPOINT or AZURE_SEARCH_API_KEY env vars")
    url = f"{SEARCH_ENDPOINT}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers = {"Content-Type": "application/json", "api-key": SEARCH_KEY}
    body = {"search": "*", "top": 1000}
    if select:
        body["select"] = select
    out = []
    while True:
        r = SESSION.post(url, headers=headers, json=body, timeout=30)
        if r.status_code != 200:
            raise SystemExit(f"Search failed {r.status_code} {r.text[:500]}")
        data = r.json()
        out.extend(data.get('value', []))
        nextp = data.get('@search.nextPageParameters')
        if not nextp:
            break
        body = nextp
    return out

def extract_program_field(doc: Dict) -> str | None:
    for f in PROGRAM_FIELD_CANDIDATES:
        if f in doc and doc[f]:
            return str(doc[f])
    # Sometimes paragraph id encodes program e.g. PROG-para123
    pid = doc.get('para_id') or doc.get('id')
    if pid and isinstance(pid, str) and '-' in pid:
        return pid.split('-')[0]
    return None

def has_text_content(doc: Dict) -> bool:
    for f in TEXT_FIELD_CANDIDATES:
        if f in doc and isinstance(doc[f], str) and doc[f].strip():
            return True
    return False

def collect_paragraph_programs(paragraph_index: str) -> Tuple[Set[str], Set[str]]:
    # Some fields may not exist; selecting * is safer given moderate index size.
    docs = search_all_docs(paragraph_index, select=None)
    programs_with_any = set()
    programs_with_text = set()
    for d in docs:
        p = extract_program_field(d)
        if not p: continue
        p_norm = norm_program(p)
        programs_with_any.add(p_norm)
        if has_text_content(d):
            programs_with_text.add(p_norm)
    return programs_with_any, programs_with_text

def collect_fact_programs(facts_index: str) -> Set[str]:
    # Avoid explicit select to prevent 400 errors when some fields absent.
    docs = search_all_docs(facts_index, select=None)
    progs = set()
    for d in docs:
        for f in PROGRAM_FIELD_CANDIDATES:
            if f in d and d[f]:
                progs.add(norm_program(str(d[f])))
    return progs

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--paragraph-index', default='cobol-paragraphs-v3')
    ap.add_argument('--facts-index')
    ap.add_argument('--roots', nargs='*', default=DEFAULT_ROOTS)
    ap.add_argument('--json-out')
    args = ap.parse_args()

    files = find_cobol_files(args.roots)
    filesystem_programs = {derive_program_name(p): p for p in files}

    para_programs_any, para_programs_text = collect_paragraph_programs(args.paragraph_index)
    fact_programs = collect_fact_programs(args.facts_index) if args.facts_index else set()

    missing = sorted([p for p in filesystem_programs if p not in para_programs_any])
    present_no_text = sorted([p for p in para_programs_any if p not in para_programs_text])
    present_with_text = sorted([p for p in para_programs_text])

    inconsistencies = {
        'facts_without_paragraphs': sorted(list(fact_programs - para_programs_any)),
        'paragraphs_without_facts': sorted(list(para_programs_any - fact_programs)) if fact_programs else []
    }

    summary = {
        'total_files': len(files),
        'filesystem_programs': len(filesystem_programs),
        'programs_in_paragraph_index': len(para_programs_any),
        'programs_with_text': len(para_programs_text),
        'missing_programs': missing,
        'present_no_text_programs': present_no_text,
        'present_with_text_programs': present_with_text,
        'inconsistencies': inconsistencies,
    }

    # Textual report
    print("=== Paragraph Coverage Audit ===")
    print(f"Filesystem COBOL files: {summary['total_files']} unique_programs={summary['filesystem_programs']}")
    print(f"Paragraph index programs: {summary['programs_in_paragraph_index']} with_text={summary['programs_with_text']}")
    print(f"Missing programs (no paragraphs ingested): {len(missing)}")
    if missing:
        print('  ' + ' '.join(missing[:40]) + (' ...' if len(missing) > 40 else ''))
    print(f"Programs lacking paragraph text: {len(present_no_text)}")
    if present_no_text:
        print('  ' + ' '.join(present_no_text[:40]) + (' ...' if len(present_no_text) > 40 else ''))
    if args.facts_index:
        fwop = inconsistencies['facts_without_paragraphs']
        pwf = inconsistencies['paragraphs_without_facts']
        print(f"Facts without paragraphs: {len(fwop)}")
        if fwop:
            print('  ' + ' '.join(fwop[:40]) + (' ...' if len(fwop) > 40 else ''))
        print(f"Paragraphs without facts: {len(pwf)}")
        if pwf:
            print('  ' + ' '.join(pwf[:40]) + (' ...' if len(pwf) > 40 else ''))

    if args.json_out:
        with open(args.json_out,'w',encoding='utf-8') as f:
            json.dump(summary,f,indent=2)
        print(f"Wrote {args.json_out}")

if __name__ == '__main__':
    main()
