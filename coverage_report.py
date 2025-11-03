"""Generate a quick coverage report (filesystem based) of program vs copybook sources.

Outputs:
  - total program files (.cbl/.cob/.cobol)
  - total copybook files (.cpy/.copy)
  - ratio copybooks:programs
  - top N (configurable) largest copybooks by line count (to gauge significance)

Future extension (TODO):
  * Query indexes (if desired) to compare counts of origin_kind=program vs origin_kind=copybook
    once those fields are added to schemas.

Usage:
  python coverage_report.py --root cobol_src --top 15
"""
from __future__ import annotations
import argparse, pathlib, json, statistics

PROGRAM_EXTS={'.cbl','.cob','.cobol'}
COPYBOOK_EXTS={'.cpy','.copy'}

def scan(root: str):
    root_path=pathlib.Path(root)
    programs=[]
    copybooks=[]
    for fp in root_path.rglob('*'):
        if not fp.is_file():
            continue
        ext=fp.suffix.lower()
        try:
            line_count=sum(1 for _ in fp.open('r',encoding='utf-8',errors='ignore'))
        except Exception:
            line_count=0
        if ext in PROGRAM_EXTS:
            programs.append((fp,line_count))
        elif ext in COPYBOOK_EXTS:
            copybooks.append((fp,line_count))
    return programs, copybooks

def build_report(programs, copybooks, top:int):
    prog_lines=[lc for _,lc in programs]
    cpy_lines=[lc for _,lc in copybooks]
    report={
        'program_file_count': len(programs),
        'copybook_file_count': len(copybooks),
        'copybook_to_program_ratio': round(len(copybooks)/len(programs),3) if programs else None,
        'program_total_lines': sum(prog_lines),
        'copybook_total_lines': sum(cpy_lines),
        'copybook_line_ratio': round(sum(cpy_lines)/sum(prog_lines),3) if prog_lines and sum(prog_lines)>0 else None,
        'program_median_lines': statistics.median(prog_lines) if prog_lines else None,
        'copybook_median_lines': statistics.median(cpy_lines) if cpy_lines else None,
        'top_copybooks': [
            {
                'path': str(p),
                'lines': lc
            } for p,lc in sorted(copybooks, key=lambda t:t[1], reverse=True)[:top]
        ]
    }
    return report

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--root',default='cobol_src')
    ap.add_argument('--top',type=int,default=10)
    args=ap.parse_args()
    programs, copybooks=scan(args.root)
    rep=build_report(programs, copybooks, args.top)
    print(json.dumps(rep, indent=2))

if __name__=='__main__':
    main()
