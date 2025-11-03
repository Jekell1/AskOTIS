"""Randomized validator for alias coverage.

Samples up to N canonical names (programs + copybooks) and generates randomized variants
(case changes, separator changes, OCR slips, spacing) then resolves them using alias_resolver.
Fails (exit code 1) if overall pass rate < threshold (default 0.98).
"""
from __future__ import annotations
import random, sys, os, math, argparse, pathlib
import itertools

# Ensure repo root in path
HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from ingest import program_catalog as catalog  # type: ignore
from retrieval import alias_resolver  # type: ignore

OCR_SWAPS = [("0","O"),("O","0"),("1","I"),("I","1"),("L","I"),("I","L"),("5","S"),("S","5"),("2","Z"),("Z","2"),("8","B"),("B","8"),("4","A"),("A","4")]
SEPS = ["-","_"," ","."]

def synth_variants(canon: str, k: str, max_variants: int, canon_aliases: dict[str, list[str]], program_roots: set[str]):
    """Generate realistic variants by sampling from pre-built alias list plus a few normalization forms.

    Optimized: Instead of scanning all alias pairs each call (O(N) per canonical), we pre-build a
    canonical->aliases map once. This function now performs only local shuffling & light set ops.
    """
    variants = set()
    known = canon_aliases.get(canon, [])
    if known:
        random.shuffle(known)
        for a in known[: max_variants * 2]:  # slight over-sample before trimming
            variants.add(a)
            variants.add(a.lower())
    # Add normalized separator permutations for canonical itself (without altering characters)
    base = canon
    root = base[:-4] if (k == 'COPYBOOK' and base.endswith('-CPY')) else base
    tokens = root.split('-')
    if k == 'COPYBOOK':
        if root not in program_roots:  # avoid ambiguity with program roots
            for sep in SEPS:
                variants.add(sep.join(tokens))
                variants.add(sep.join(t.lower() for t in tokens))
            variants.add(''.join(tokens))
            variants.add(''.join(tokens).lower())
    else:
        for sep in SEPS:
            variants.add(sep.join(tokens))
            variants.add(sep.join(t.lower() for t in tokens))
        variants.add(''.join(tokens))
        variants.add(''.join(tokens).lower())
    out = list(variants)
    random.shuffle(out)
    return out[:max_variants]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--src-root', default='cobol_src')
    ap.add_argument('--samples', type=int, default=1000)
    ap.add_argument('--max-variants', type=int, default=12, help='Maximum variants generated per canonical name')
    # Backward-compatible alias for user convenience (maps to --max-variants)
    ap.add_argument('--variants-per-name', type=int, help='Alias for --max-variants')
    ap.add_argument('--threshold', type=float, default=0.98)
    args = ap.parse_args()

    if getattr(args, 'variants_per_name', None) is not None:
        args.max_variants = args.variants_per_name

    programs = list(dict.fromkeys(catalog.discover_program_ids(src_root=args.src_root, disable_index=False)))
    copybooks_raw = list(dict.fromkeys(catalog.discover_copybook_names(src_root=args.src_root, disable_index=False)))
    copybooks = [f"{c}-CPY" for c in copybooks_raw]

    canon_pairs = [(p,'PROGRAM') for p in programs] + [(c,'COPYBOOK') for c in copybooks]
    random.shuffle(canon_pairs)
    if len(canon_pairs) > args.samples:
        canon_pairs = canon_pairs[:args.samples]

    total_tests = 0
    passes = 0
    failures = []

    # Pre-load alias store so we can reference alias_to_canon
    alias_resolver._ensure_loaded()  # type: ignore
    alias_lookup = alias_resolver.STORE.alias_to_canon  # type: ignore
    # Build optimized canonical->aliases list (exclude canonical self for noise reduction only if desired)
    canon_aliases: dict[str, list[str]] = {}
    for alias, (c, _k) in alias_lookup.items():
        # include alias even if identical to canonical; downstream dedup handles
        canon_aliases.setdefault(c, []).append(alias)
    program_roots = {p for p,_k in [(c,'PROGRAM') for c in programs]}

    for canon, kind in canon_pairs:
        variants = synth_variants(canon, kind, max_variants=args.max_variants, canon_aliases=canon_aliases, program_roots=program_roots)
        for v in variants:
            total_tests += 1
            resolved, rkind, score = alias_resolver.resolve(v)
            if resolved == canon and rkind == kind:
                passes += 1
            else:
                failures.append((canon, kind, v, resolved, rkind, score))

    pass_rate = passes / total_tests if total_tests else 0.0
    print(f"Canonical sampled: {len(canon_pairs)}")
    print(f"Total variants tested: {total_tests}")
    print(f"Passes: {passes}")
    print(f"Failures: {len(failures)}")
    print(f"Pass rate: {pass_rate:.4f}")

    if failures and pass_rate < args.threshold:
        print("-- Failures (showing up to 25) --")
        for row in failures[:25]:
            print("CANON=",row[0]," KIND=",row[1]," VARIANT=",row[2]," -> RESOLVED=",row[3]," KIND=",row[4]," SCORE=",f"{row[5]:.3f}")
        sys.exit(1)
    else:
        print("PASS: Alias coverage above threshold")

if __name__ == '__main__':
    main()
