"""Classify COBOL programs by content heuristics and (optionally) coverage metrics.

Produces JSON lines with: program_id, file_path, classification, reasons
Classification values:
  DATA_ONLY    - High proportion of PIC/01 level records, little procedure logic
  NO_PARAS     - Has zero extracted paragraphs (pass --coverage-json) and > 0 lines
  LOW_COVERAGE - coverage_pct < threshold and paragraphs present
  NORMAL       - everything else

Usage:
  python classify_cobol_programs.py --roots . --coverage-json coverage_full.json --output classifications.jsonl
"""
from __future__ import annotations
import os, re, json, argparse, sys
from typing import List, Dict, Any

try:
    from ingest_cobol_paragraphs import normalize_program_id, COBOL_EXTENSIONS
except ImportError as e:
    print(f"[FATAL] Missing dependencies: {e}", file=sys.stderr)
    sys.exit(1)

RE_DATA = re.compile(r'\b(PIC|OCCURS|COMP-?3|REDEFINES|GROUP|FILLER|SYNC|USAGE|DISPLAY)\b', re.IGNORECASE)
RE_PROCEDURE_TOKENS = re.compile(r'\b(PERFORM|MOVE|CALL|IF|EVALUATE|ADD|SUBTRACT|COMPUTE|READ|WRITE|OPEN|CLOSE|STOP|EXEC\s+SQL|ACCEPT|DISPLAY|INITIALIZE)\b', re.IGNORECASE)
RE_PROCEDURE_DIV = re.compile(r'^\s*PROCEDURE\s+DIVISION', re.IGNORECASE|re.MULTILINE)
RE_DATA_DIV = re.compile(r'^\s*DATA\s+DIVISION', re.IGNORECASE|re.MULTILINE)

def detect_data_only_context(content: str, data_hits: int, proc_hits: int, total_lines: int, para_count: int | None) -> Dict[str, Any]:
    """Return dict with decision flags for data-only upgrade.

    Criteria (any strong combination):
      * No PROCEDURE DIVISION but has DATA DIVISION
      * Extension .CPY and data density high
      * Zero paragraphs AND (data_hits >= 8 and proc_hits == 0)
      * Data/proc ratio > 10 with proc_hits <= 1
    """
    has_proc_div = bool(RE_PROCEDURE_DIV.search(content))
    has_data_div = bool(RE_DATA_DIV.search(content))
    data_ratio = (data_hits / max(proc_hits,1)) if proc_hits else float('inf')
    reasons: List[str] = []
    upgrade = False
    if not has_proc_div and has_data_div and data_hits >= 5:
        upgrade = True; reasons.append('no PROCEDURE DIVISION, has DATA DIVISION')
    if data_ratio > 10 and proc_hits <= 1 and data_hits >= 6:
        upgrade = True; reasons.append(f'data_ratio={data_ratio:.1f} proc_hits={proc_hits}')
    if para_count == 0 and data_hits >= 8 and proc_hits == 0:
        upgrade = True; reasons.append('zero paras, pure data tokens')
    if total_lines <= 15 and proc_hits == 0 and data_hits >= 3:
        upgrade = True; reasons.append('small file data stub')
    return {'upgrade': upgrade, 'reasons': reasons, 'data_ratio': data_ratio, 'has_proc_div': has_proc_div, 'has_data_div': has_data_div}


def walk_files(roots: List[str]) -> List[str]:
    out: List[str] = []
    for root in roots:
        for dirpath, _, files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1].lower()
                if ext in COBOL_EXTENSIONS:
                    out.append(os.path.join(dirpath, f))
    return sorted(out)

def load_coverage_map(path: str | None) -> Dict[str, Dict[str, Any]]:
    if not path:
        return {}
    try:
        data = json.load(open(path,'r',encoding='utf-8'))
        out = {}
        for p in data.get('programs', []):
            pid = p.get('program_id')
            if pid:
                out[pid.upper()] = p
        return out
    except Exception as e:
        print(f"[WARN] Failed to load coverage JSON: {e}", file=sys.stderr)
        return {}


def classify_file(path: str, coverage: Dict[str, Any] | None, low_cov_threshold: float) -> Dict[str, Any]:
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            content = f.read()
    except Exception as e:
        return {'program_id': normalize_program_id(path), 'file_path': path, 'classification': 'ERROR', 'reasons': [str(e)]}
    program_id = normalize_program_id(path)
    lines = content.splitlines()
    total_lines = len(lines)
    data_hits = len(RE_DATA.findall(content))
    proc_hits = len(RE_PROCEDURE_TOKENS.findall(content))
    reasons = []
    classification = 'NORMAL'
    cov_rec = coverage.get(program_id) if coverage else None
    para_count = cov_rec.get('paragraph_count',0) if cov_rec else None
    coverage_pct = cov_rec.get('coverage_pct',0.0) if cov_rec else None

    if cov_rec and para_count == 0 and total_lines > 0:
        # Start as NO_PARAS then attempt upgrade to DATA_ONLY
        classification = 'NO_PARAS'
        reasons.append('zero paragraphs extracted')
        ctx = detect_data_only_context(content, data_hits, proc_hits, total_lines, para_count or 0)
        if ctx['upgrade']:
            classification = 'DATA_ONLY'
            reasons.extend(ctx['reasons'])
    elif cov_rec and coverage_pct is not None and coverage_pct < low_cov_threshold and para_count and para_count > 0:
        classification = 'LOW_COVERAGE'
        reasons.append(f'coverage {coverage_pct} < {low_cov_threshold}')
    else:
        # broader data-only heuristic for programs with paragraphs or no coverage context
        ctx = detect_data_only_context(content, data_hits, proc_hits, total_lines, para_count or 0)
        if ctx['upgrade']:
            classification = 'DATA_ONLY'
            reasons.extend(ctx['reasons'])
        elif data_hits >= 5 and proc_hits <= 2 and (proc_hits == 0 or data_hits / max(proc_hits,1) > 5):
            classification = 'DATA_ONLY'
            reasons.append(f'data_hits={data_hits} proc_hits={proc_hits}')

    return {
        'program_id': program_id,
        'file_path': path.replace('\\','/'),
        'classification': classification,
        'reasons': reasons,
        'lines': total_lines,
        'data_hits': data_hits,
        'proc_hits': proc_hits,
        'coverage_pct': coverage_pct,
        'paragraph_count': para_count,
        'data_ratio': (data_hits / max(proc_hits,1)) if proc_hits else None
    }


def main():
    ap = argparse.ArgumentParser(description='Classify COBOL programs by content and coverage heuristics.')
    ap.add_argument('--roots', nargs='+', default=['.'])
    ap.add_argument('--coverage-json', help='Optional coverage_full.json to incorporate paragraph counts & coverage.')
    ap.add_argument('--low-cov-threshold', type=float, default=10.0)
    ap.add_argument('--limit', type=int)
    ap.add_argument('--output', required=True, help='Output JSONL file of classifications.')
    args = ap.parse_args()
    coverage_map = load_coverage_map(args.coverage_json)
    files = walk_files(args.roots)
    if args.limit:
        files = files[:args.limit]
    print(f"Classifying {len(files)} files...")
    with open(args.output,'w',encoding='utf-8') as out:
        for idx, path in enumerate(files, start=1):
            rec = classify_file(path, coverage_map, args.low_cov_threshold)
            out.write(json.dumps(rec)+'\n')
            if idx % 200 == 0:
                print(f"Processed {idx} files...")
    print(f"Classification complete -> {args.output}")

if __name__ == '__main__':
    main()
