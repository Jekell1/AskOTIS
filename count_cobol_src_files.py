hy"""Count total files and COBOL source files plus extension distribution under cobol_src."""
from __future__ import annotations
import os, json, collections

ROOT='cobol_src'

def main():
    all_files=[]
    for dp,_,fs in os.walk(ROOT):
        for f in fs:
            all_files.append(os.path.join(dp,f))
    by_ext=collections.Counter(os.path.splitext(f)[1].lower() or '<noext>' for f in all_files)
    cobol=[f for f in all_files if os.path.splitext(f)[1].lower() in ('.cbl','.cob')]
    out={
        'total_files': len(all_files),
        'cobol_source_files': len(cobol),
        'top_extensions': by_ext.most_common(30)
    }
    print(json.dumps(out,indent=2))

if __name__=='__main__':
    main()
