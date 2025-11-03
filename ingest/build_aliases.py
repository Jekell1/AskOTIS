"""Build repo-wide alias map for PROGRAMS and COPYBOOKS.

Generates data/aliases.json containing structure:
{
  "PROGRAM": { "CANON": ["alias1","alias2", ...], ... },
  "COPYBOOK": { "CANON-CPY": ["aliasX", ...] }
}

Rules:
  * Discover canonical sets via program_catalog.discover_* (indexes preferred, filesystem fallback)
  * Each canonical program keeps its normalized (uppercase, hyphen collapsed) base form
  * Generate variants:
      - lower, mixed (original) not stored (we generate algorithmically) but we still include common textual forms
      - hyphenless (remove '-')
      - underscore form (replace '-' with '_')
      - space form (replace '-' with ' ')
      - dotted form (replace '-' with '.')
      - OCR slips: 1<->I, 0<->O applied to each token position independently (limited expansion)
  * Copybook canonical names receive '-CPY' suffix to avoid collisions with program IDs when overlapping.
  * Deduplicate variants and exclude canonical itself from alias list only if identical.

CLI:
  python ingest/build_aliases.py --src-root cobol_src --out data/aliases.json

This file intentionally does not attempt to merge pre-existing aliases.json; it overwrites.
"""
from __future__ import annotations
import os, json, argparse, itertools, re
from typing import List, Set, Dict
import sys, pathlib
# Ensure repo root on sys.path for direct execution (python ingest/build_aliases.py)
_HERE=pathlib.Path(__file__).resolve().parent
_ROOT=_HERE.parent
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))
from ingest import program_catalog as catalog

OCR_SWAPS = [
    ('0','O'), ('O','0'),
    ('1','I'), ('I','1'),
    ('L','I'), ('I','L'),
    ('5','S'), ('S','5'),
    ('2','Z'), ('Z','2'),
    ('8','B'), ('B','8'),
    ('4','A'), ('A','4')
]
ALNUM_SPLIT_RE = re.compile(r'^([A-Z]+)([0-9]{2,})$')  # e.g. TIM360 -> (TIM, 360)

def gen_ocr_slips(name: str) -> Set[str]:
    outs={name}
    chars=list(name)
    for i,ch in enumerate(chars):
        for a,b in OCR_SWAPS:
            if ch==a:
                mod=chars.copy(); mod[i]=b; outs.add(''.join(mod))
    return outs

def generate_variants(canon: str) -> Set[str]:
    variants:set[str]=set()
    base=canon
    hyphenless=base.replace('-','')
    underscore=base.replace('-','_')
    spaced=base.replace('-',' ')
    dotted=base.replace('-','.')
    variants.update([base.lower(), hyphenless, hyphenless.lower(), underscore, underscore.lower(), spaced, spaced.lower(), dotted, dotted.lower()])
    # If canonical has no hyphen but matches ALPHA + DIGITS, synthesize hyphenated forms (TIM360 -> TIM-360)
    if '-' not in base:
        m=ALNUM_SPLIT_RE.match(base)
        if m:
            part_a, part_b = m.groups()
            hyph=f"{part_a}-{part_b}"
            underscore2=f"{part_a}_{part_b}"
            spaced2=f"{part_a} {part_b}"
            dotted2=f"{part_a}.{part_b}"
            variants.update([
                hyph, hyph.lower(),
                underscore2, underscore2.lower(),
                spaced2, spaced2.lower(),
                dotted2, dotted2.lower()
            ])
    # OCR slips on each of those plus base
    for seed in list(variants)+[base]:
        variants.update(gen_ocr_slips(seed))
    # If this is a copybook canonical (endswith -CPY), also generate variants without the suffix to assist resolution
    if base.endswith('-CPY'):
        root = base[:-4]
        variants.update([root, root.lower(), root.replace('-',''), root.replace('-','').lower()])
        m=ALNUM_SPLIT_RE.match(root)
        if m and '-' not in root:
            pa,pb=m.groups()
            hyph=f"{pa}-{pb}"
            variants.update([hyph, hyph.lower()])
    # Remove empties and canonical uppercase exact from variant set (will be stored separately)
    variants={v for v in variants if v and v.upper()!=canon}
    return variants

def build(src_root: str, include_placeholders: bool=False, placeholder_path: str|None=None, placeholder_min_occurrences: int=5) -> Dict[str,Dict[str,List[str]]]:
    programs=set(catalog.discover_program_ids(src_root=src_root, disable_index=False))  # allow index if present
    copybooks=set(catalog.discover_copybook_names(src_root=src_root, disable_index=False))
    # Fallback: if cobol_src empty, try current directory to avoid producing an empty alias file
    if not programs and src_root != '.':
        alt_programs=catalog.discover_program_ids(src_root='.', disable_index=True)
        if alt_programs:
            programs=set(alt_programs)
    if not copybooks and src_root != '.':
        alt_copybooks=catalog.discover_copybook_names(src_root='.', disable_index=True)
        if alt_copybooks:
            copybooks=set(alt_copybooks)
    # Optionally augment with external placeholder routines (e.g., TIM360) that appear in flow edges but have no source file.
    if include_placeholders and placeholder_path and os.path.exists(placeholder_path):
        try:
            with open(placeholder_path,'r',encoding='utf-8') as ph:
                for line in ph:
                    line=line.strip()
                    if not line:
                        continue
                    try:
                        obj=json.loads(line)
                    except Exception:
                        continue
                    pid=obj.get('program_id')
                    occ=obj.get('occurrences',0)
                    if not pid or not isinstance(pid,str):
                        continue
                    up=pid.upper()
                    if up not in programs and occ >= placeholder_min_occurrences:
                        programs.add(up)
        except Exception as e:
            print(f"[WARN] Failed reading placeholders {placeholder_path}: {e}")
    out={'PROGRAM':{}, 'COPYBOOK':{}}
    for p in programs:
        canon=p  # already normalized by catalog
        vars=sorted(generate_variants(canon))
        out['PROGRAM'][canon]=vars
    for c in copybooks:
        canon=f"{c}-CPY"
        vars=sorted(generate_variants(canon))
        out['COPYBOOK'][canon]=vars
    return out

def main():
    ap=argparse.ArgumentParser(description='Build alias map for all programs and copybooks.')
    ap.add_argument('--src-root', default='cobol_src')
    ap.add_argument('--out', default='data/aliases.json')
    ap.add_argument('--pretty', action='store_true')
    ap.add_argument('--include-placeholders', action='store_true', help='Include unresolved external routine placeholders (e.g., TIM360)')
    ap.add_argument('--placeholder-path', default='JSONL/external_program_placeholders.jsonl')
    ap.add_argument('--placeholder-min-occurrences', type=int, default=5)
    args=ap.parse_args()
    os.makedirs(os.path.dirname(args.out) or '.', exist_ok=True)
    data=build(
        args.src_root,
        include_placeholders=args.include_placeholders,
        placeholder_path=args.placeholder_path,
        placeholder_min_occurrences=args.placeholder_min_occurrences,
    )
    js=json.dumps(data, indent=2 if args.pretty else None)
    with open(args.out,'w',encoding='utf-8') as f:
        f.write(js)
    print(f"[INFO] Wrote {args.out} (PROGRAM={len(data['PROGRAM'])} COPYBOOK={len(data['COPYBOOK'])})")

if __name__=='__main__':
    main()
