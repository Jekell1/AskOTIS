"""Validate a JSONL file; print counts and first n objects."""
from __future__ import annotations
import json, sys, argparse

def main():
    ap=argparse.ArgumentParser(); ap.add_argument('file'); ap.add_argument('--head',type=int,default=5)
    args=ap.parse_args()
    total=0; shown=0
    with open(args.file,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip() or ''
            if not line: continue
            try:
                obj=json.loads(line)
            except Exception as e:
                print('PARSE_ERROR', e, 'LINE', line[:120])
                continue
            total+=1
            if shown<args.head:
                print(json.dumps(obj,indent=2)); shown+=1
    print('Total valid objects:', total)
if __name__=='__main__':
    main()