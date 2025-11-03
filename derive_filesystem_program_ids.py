"""Enumerate COBOL source roots and collect file stems as candidate program IDs.
Scans COBOL_SOURCE_ROOT or --root.
Writes filesystem_program_ids.json / .txt.
"""
import os, json, argparse, pathlib, sys

EXTS={'.cbl','.cob'}


def main():
    ap=argparse.ArgumentParser(description='Collect file-stem program ids from filesystem')
    ap.add_argument('--root',default=os.getenv('COBOL_SOURCE_ROOT','cobol_src'))
    ap.add_argument('--out-base',default='filesystem_program_ids')
    args=ap.parse_args()
    root=pathlib.Path(args.root)
    if not root.exists():
        print('Root not found', root, file=sys.stderr); sys.exit(1)
    stems=set()
    for p in root.rglob('*'):
        if p.suffix.lower() in EXTS:
            stems.add(p.stem.upper())
    stems_sorted=sorted(stems)
    print('Filesystem program count:', len(stems_sorted))
    json.dump(stems_sorted, open(f"{args.out_base}.json","w"), indent=2)
    with open(f"{args.out_base}.txt","w") as f:
        for s in stems_sorted:
            f.write(s+'\n')
    print('Wrote', f"{args.out_base}.json", 'and', f"{args.out_base}.txt")

if __name__=='__main__':
    main()
