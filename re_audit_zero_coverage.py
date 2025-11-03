"""Re-audit zero coverage programs using relaxed paragraph extraction.

Inputs:
  --coverage-json coverage_full.json (original audit)
  --roots . (file search roots)
Outputs:
  Writes re_audit_zero_coverage.json with improved coverage metrics and deltas.

Process:
  1. Load coverage_full.json, filter programs with paragraph_count == 0 OR coverage_pct == 0
  2. Re-run extraction using extract_paragraphs_relaxed
  3. Compute coverage metrics (same methodology as program_coverage_audit)
  4. Produce per-program delta (new vs old) for paragraph_count, coverage_pct, covered_lines
"""
from __future__ import annotations
import os, json, argparse, sys
from typing import List, Dict, Any

from ingest_cobol_paragraphs import extract_paragraphs_relaxed, normalize_program_id, COBOL_EXTENSIONS


def walk_files(roots: List[str]) -> Dict[str, str]:
    mapping: Dict[str, str] = {}
    for root in roots:
        for dirpath, _, files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1].lower()
                if ext in COBOL_EXTENSIONS:
                    path = os.path.join(dirpath, f)
                    pid = normalize_program_id(path)
                    mapping[pid.upper()] = path
    return mapping


def read_lines(path: str) -> List[str]:
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            return f.readlines()
    except Exception:
        return []


def main():
    ap = argparse.ArgumentParser(description='Re-audit zero/low coverage programs with relaxed extraction.')
    ap.add_argument('--coverage-json', required=True)
    ap.add_argument('--roots', nargs='+', default=['.'])
    ap.add_argument('--output', default='re_audit_zero_coverage.json')
    ap.add_argument('--min-gap-lines', type=int, default=5)
    args = ap.parse_args()

    data = json.load(open(args.coverage_json,'r',encoding='utf-8'))
    orig_programs = {p['program_id'].upper(): p for p in data.get('programs', [])}
    zero = [p for p in orig_programs.values() if p.get('paragraph_count',0) == 0 or p.get('coverage_pct',0.0) == 0.0]
    print(f"Found {len(zero)} zero/empty coverage programs to re-audit")

    file_map = walk_files(args.roots)
    results: List[Dict[str, Any]] = []

    for idx, rec in enumerate(zero, start=1):
        pid = rec['program_id'].upper()
        path = file_map.get(pid)
        if not path:
            continue
        lines = read_lines(path)
        total_lines = len(lines)
        paras = extract_paragraphs_relaxed(path)
        covered_bitmap = [False]*total_lines
        for p in paras:
            s = max(1, int(p['line_start']))
            e = min(total_lines, int(p['line_end']))
            for i in range(s,e+1):
                covered_bitmap[i-1] = True
        covered_lines = sum(1 for c in covered_bitmap if c)
        coverage_pct = (covered_lines/total_lines*100.0) if total_lines else 0.0
        delta = {
            'program_id': pid,
            'file_path': path.replace('\\','/'),
            'orig_paragraph_count': rec.get('paragraph_count'),
            'new_paragraph_count': len(paras),
            'orig_coverage_pct': rec.get('coverage_pct'),
            'new_coverage_pct': round(coverage_pct,2),
            'orig_covered_lines': rec.get('covered_lines'),
            'new_covered_lines': covered_lines,
        }
        results.append(delta)
        if idx % 50 == 0:
            print(f"Re-audited {idx} programs...")

    out = {'count': len(results), 'programs': results}
    with open(args.output,'w',encoding='utf-8') as f:
        json.dump(out,f,indent=2)
    print(f"Wrote re-audit results to {args.output}")

if __name__ == '__main__':
    main()
