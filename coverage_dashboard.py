"""Generate coverage & classification dashboard summaries.

Inputs:
  --coverage coverage_full.json
  --classifications classifications.jsonl (from classify_cobol_programs.py)

Outputs:
  Prints bucket statistics and writes optional JSON summary if --output specified.

Buckets (coverage_pct):
  0
  (0,10]
  (10,50]
  (50,70]
  (70,90]
  (90,100]

Also counts classification frequency.
"""
from __future__ import annotations
import json, argparse, statistics, sys
from typing import Dict, Any, List

COVERAGE_BUCKETS = [
    ('zero', lambda c: c == 0),
    ('0_10', lambda c: 0 < c <= 10),
    ('10_50', lambda c: 10 < c <= 50),
    ('50_70', lambda c: 50 < c <= 70),
    ('70_90', lambda c: 70 < c <= 90),
    ('90_100', lambda c: 90 < c <= 100),
]


def load_coverage(path: str) -> List[Dict[str, Any]]:
    data = json.load(open(path,'r',encoding='utf-8'))
    return data.get('programs', [])


def load_classifications(path: str) -> Dict[str,str]:
    mapping: Dict[str,str] = {}
    if not path:
        return mapping
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
            except Exception:
                continue
            pid = rec.get('program_id')
            cl = rec.get('classification')
            if pid and cl:
                mapping[pid.upper()] = cl
    return mapping


def summarize(coverage: List[Dict[str, Any]], classifications: Dict[str,str]) -> Dict[str, Any]:
    total = len(coverage)
    bucket_counts = {name:0 for name,_ in COVERAGE_BUCKETS}
    class_counts: Dict[str,int] = {}
    cov_values: List[float] = []
    for rec in coverage:
        c = rec.get('coverage_pct',0.0) or 0.0
        cov_values.append(c)
        for name, predicate in COVERAGE_BUCKETS:
            if predicate(c):
                bucket_counts[name] += 1
                break
        pid = rec.get('program_id','').upper()
        cl = classifications.get(pid)
        if cl:
            class_counts[cl] = class_counts.get(cl,0) + 1
    median_cov = statistics.median(cov_values) if cov_values else 0.0
    pct = lambda n: round((n/total*100.0),2) if total else 0.0
    bucket_pct = {k: {'count': v, 'pct': pct(v)} for k,v in bucket_counts.items()}
    class_pct = {k: {'count': v, 'pct': pct(v)} for k,v in class_counts.items()}
    return {
        'programs': total,
        'median_program_coverage_pct': round(median_cov,2),
        'coverage_buckets': bucket_pct,
        'classifications': class_pct
    }


def main():
    ap = argparse.ArgumentParser(description='Coverage & classification dashboard.')
    ap.add_argument('--coverage', required=True)
    ap.add_argument('--classifications', required=True)
    ap.add_argument('--output')
    args = ap.parse_args()
    coverage = load_coverage(args.coverage)
    classes = load_classifications(args.classifications)
    summary = summarize(coverage, classes)
    print("COVERAGE DASHBOARD")
    print(f"Programs: {summary['programs']}")
    print(f"Median Coverage %: {summary['median_program_coverage_pct']}")
    print("Coverage Buckets:")
    for b, stats in summary['coverage_buckets'].items():
        print(f"  {b:7} : {stats['count']:6} ({stats['pct']:5.2f}%)")
    print("Classifications:")
    for c, stats in summary['classifications'].items():
        print(f"  {c:12} : {stats['count']:6} ({stats['pct']:5.2f}%)")
    if args.output:
        with open(args.output,'w',encoding='utf-8') as f:
            json.dump(summary,f,indent=2)
        print(f"Wrote summary JSON to {args.output}")

if __name__ == '__main__':
    main()
