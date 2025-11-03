"""Program coverage audit for COBOL paragraph extraction.

For each COBOL source file this script:
  * Extracts paragraphs/sections using existing ingestion heuristics
  * Counts total physical lines in the file (UTF-8 decode with replacement)
  * Builds a coverage bitmap of lines spanned by any extracted paragraph
  * Computes per-program metrics:
        program_id, file_path, total_lines, covered_lines, coverage_pct,
        paragraph_count, avg_paragraph_length, median_paragraph_length,
        max_paragraph_length
  * Detects uncovered gaps (contiguous uncovered line ranges) whose length
    is >= --min-gap-lines. Each gap reported with start_line, end_line, length.

Usage examples:
  python program_coverage_audit.py --roots src --limit-programs 20
  python program_coverage_audit.py --roots . --min-gap-lines 10 --output-json coverage.json

Exit code is 0 unless an unexpected exception occurs.
"""
from __future__ import annotations
import os, sys, argparse, json, statistics
from typing import List, Dict, Any, Tuple

# Reuse extraction logic from ingestion script (guarded by if __name__ == '__main__')
try:
    from ingest_cobol_paragraphs import extract_paragraphs, normalize_program_id, COBOL_EXTENSIONS
except ImportError as e:
    print(f"[FATAL] Could not import extraction utilities: {e}", file=sys.stderr)
    sys.exit(1)

def walk_files(roots: List[str]) -> List[str]:
    out: List[str] = []
    for root in roots:
        for dirpath, _, files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1].lower()
                if ext in COBOL_EXTENSIONS:
                    out.append(os.path.join(dirpath, f))
    return sorted(out)

def read_lines(path: str) -> List[str]:
    try:
        with open(path, 'r', encoding='utf-8', errors='replace') as f:
            return f.readlines()
    except Exception as e:
        print(f"[WARN] Failed to read {path}: {e}", file=sys.stderr)
        return []

def build_coverage_bitmap(total_lines: int, paragraphs: List[Dict[str, Any]]) -> List[bool]:
    covered = [False] * total_lines
    for p in paragraphs:
        # line_start / line_end are 1-based inclusive
        s = max(1, int(p['line_start']))
        e = min(total_lines, int(p['line_end']))
        for i in range(s, e + 1):
            covered[i - 1] = True
    return covered

def find_gaps(covered: List[bool], min_gap: int) -> List[Dict[str, int]]:
    gaps: List[Dict[str, int]] = []
    start = None
    for idx, c in enumerate(covered, start=1):
        if not c and start is None:
            start = idx
        elif c and start is not None:
            length = idx - start
            if length >= min_gap:
                gaps.append({'start_line': start, 'end_line': idx - 1, 'length': length})
            start = None
    # tail gap
    if start is not None:
        length = len(covered) + 1 - start
        if length >= min_gap:
            gaps.append({'start_line': start, 'end_line': len(covered), 'length': length})
    # sort largest first
    gaps.sort(key=lambda g: g['length'], reverse=True)
    return gaps

def compute_stats(lengths: List[int]) -> Tuple[float, float, int]:
    if not lengths:
        return 0.0, 0.0, 0
    avg_len = sum(lengths) / len(lengths)
    try:
        median_len = statistics.median(lengths)
    except statistics.StatisticsError:
        median_len = 0.0
    max_len = max(lengths)
    return avg_len, median_len, max_len

def audit_program(path: str, min_gap_lines: int) -> Dict[str, Any]:
    lines = read_lines(path)
    total_lines = len(lines)
    paragraphs = extract_paragraphs(path)
    covered_bitmap = build_coverage_bitmap(total_lines, paragraphs) if total_lines else []
    covered_lines = sum(1 for c in covered_bitmap if c)
    coverage_pct = (covered_lines / total_lines * 100.0) if total_lines else 0.0
    lengths = [int(p['length_lines']) for p in paragraphs]
    avg_len, median_len, max_len = compute_stats(lengths)
    gaps = find_gaps(covered_bitmap, min_gap_lines) if total_lines else []
    program_id = normalize_program_id(path)
    return {
        'program_id': program_id,
        'file_path': path.replace('\\','/'),
        'total_lines': total_lines,
        'paragraph_count': len(paragraphs),
        'covered_lines': covered_lines,
        'coverage_pct': round(coverage_pct, 2),
        'avg_paragraph_length': round(avg_len, 2),
        'median_paragraph_length': median_len,
        'max_paragraph_length': max_len,
        'gap_count': len(gaps),
        'largest_gaps': gaps[:10],  # top 10 largest gaps
    }

def aggregate_summary(results: List[Dict[str, Any]]) -> Dict[str, Any]:
    if not results:
        return {}
    total_programs = len(results)
    total_lines = sum(r['total_lines'] for r in results)
    total_covered = sum(r['covered_lines'] for r in results)
    overall_cov_pct = (total_covered / total_lines * 100.0) if total_lines else 0.0
    cov_values = [r['coverage_pct'] for r in results]
    low_coverage = [r for r in results if r['coverage_pct'] < 70.0]
    return {
        'programs': total_programs,
        'total_lines': total_lines,
        'total_covered_lines': total_covered,
        'overall_coverage_pct': round(overall_cov_pct, 2),
        'median_program_coverage_pct': statistics.median(cov_values) if cov_values else 0.0,
        'programs_below_70pct': len(low_coverage),
    }

def main():
    ap = argparse.ArgumentParser(description='Audit COBOL paragraph extraction coverage by program.')
    ap.add_argument('--roots', nargs='+', default=['.'], help='Root directories to scan for COBOL files.')
    ap.add_argument('--limit-programs', type=int, help='Limit number of programs processed (after sorting by path).')
    ap.add_argument('--min-gap-lines', type=int, default=5, help='Minimum uncovered line span to report as a gap.')
    ap.add_argument('--sort-by', choices=['coverage', 'lines', 'paragraphs'], default='coverage', help='Ordering for printed table.')
    ap.add_argument('--output-json', help='Optional path to write full JSON results (list + aggregate).')
    ap.add_argument('--top', type=int, default=30, help='How many rows to print in the summary table.')
    args = ap.parse_args()

    files = walk_files(args.roots)
    if args.limit_programs:
        files = files[:args.limit_programs]
    print(f"Discovered {len(files)} COBOL source files for coverage audit.")

    results: List[Dict[str, Any]] = []
    for idx, path in enumerate(files, start=1):
        rec = audit_program(path, args.min_gap_lines)
        results.append(rec)
        if idx % 25 == 0:
            print(f"Processed {idx} programs...")

    # Sorting
    if args.sort_by == 'coverage':
        results.sort(key=lambda r: r['coverage_pct'])  # ascending coverage (worst first)
    elif args.sort_by == 'lines':
        results.sort(key=lambda r: r['total_lines'], reverse=True)
    else:  # paragraphs
        results.sort(key=lambda r: r['paragraph_count'], reverse=True)

    aggregate = aggregate_summary(results)

    # Print table header
    print("\nPROGRAM COVERAGE SUMMARY (worst coverage first)" if args.sort_by=='coverage' else "\nPROGRAM COVERAGE SUMMARY")
    print(f"{'Program':40} {'Lines':>8} {'Paras':>6} {'Covered':>8} {'Cov%':>6} {'Gaps':>4} {'MaxGap':>6}")
    for rec in results[:args.top]:
        max_gap_len = rec['largest_gaps'][0]['length'] if rec['largest_gaps'] else 0
        print(f"{rec['program_id'][:40]:40} {rec['total_lines']:8d} {rec['paragraph_count']:6d} {rec['covered_lines']:8d} {rec['coverage_pct']:6.2f} {rec['gap_count']:4d} {max_gap_len:6d}")

    print("\nAGGREGATE:")
    for k,v in aggregate.items():
        print(f"  {k}: {v}")

    if args.output_json:
        out_obj = {'aggregate': aggregate, 'programs': results}
        try:
            with open(args.output_json, 'w', encoding='utf-8') as f:
                json.dump(out_obj, f, indent=2)
            print(f"Wrote JSON results to {args.output_json}")
        except Exception as e:
            print(f"[WARN] Failed to write JSON output: {e}", file=sys.stderr)

if __name__ == '__main__':
    main()
