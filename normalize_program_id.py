"""Utility to normalize program identifiers for consistent cross-index joins.

Rules:
- Take the base filename (strip directories and extensions) when source path provided.
- Uppercase alpha chars.
- Preserve embedded digits.
- Replace internal spaces with hyphens.
- For copybooks (optional hint), treat similarly.
- Provide a fallback hash (first 12 of sha1) ONLY if empty result.

This ensures programId aligns across files, chunks, symbols, xrefs, facts, etc.
"""
from __future__ import annotations
import os, re, hashlib
from typing import Optional

VALID_RE = re.compile(r'^[A-Z0-9][A-Z0-9-]{0,31}$')  # allow up to 32 chars

EXT_STRIP = {'.cbl', '.cob', '.cpy', '.CPY', '.CBL', '.COB'}

def normalize_program_id(raw: str | None, path_hint: str | None = None) -> str:
    cand = (raw or '').strip()
    if not cand and path_hint:
        cand = os.path.basename(path_hint)
    # remove extension
    lower = cand.lower()
    for ext in EXT_STRIP:
        if lower.endswith(ext.lower()):
            cand = cand[: -len(ext)]
            break
    cand = cand.replace(' ', '-')
    cand = re.sub(r'[^A-Za-z0-9-]', '-', cand)
    cand = re.sub(r'-{2,}', '-', cand).strip('-')
    cand = cand.upper()
    if not cand or len(cand) < 2:
        # fallback to hashed path_hint
        basis = (path_hint or raw or 'UNKNOWN').encode('utf-8')
        cand = hashlib.sha1(basis).hexdigest()[:12].upper()
    # enforce length max
    if len(cand) > 32:
        cand = cand[:32]
    # final validate
    if not VALID_RE.match(cand):
        # last resort sanitized collapse
        cand = re.sub(r'[^A-Z0-9-]', '', cand) or 'UNKNOWN'
    return cand

if __name__ == '__main__':
    tests = [
        ('apipay', None),
        (None, '/repo/pay/APIPAY.cbl'),
        ('lonpf2  ', None),
        ('bad name*!', None),
        ('', '/repo/core/xyz.cob'),
        (None, '/repo/core/verylongprogramnamethatshouldbetruncatedXYZ.cbl'),
    ]
    for raw, path in tests:
        print(raw, path, '=>', normalize_program_id(raw, path))
