"""Utilities for generating name normalization / alias variants.

Rules implemented (extensible):
  * UPPERCASE canonical
  * Hyphen variants: remove hyphens, replace hyphen with underscore, insert hyphens between alpha/digit boundaries
  * Collapse consecutive underscores/hyphens
  * REPLACING pseudo-variants (placeholder hook)

Returned structure: { 'canonical': <CANON>, 'variants': { variant: variant_type } }

Variant types (string enum): ORIGINAL, UPPER, NO_HYPHEN, WITH_UNDERSCORES, INSERTED_HYPHENS, DEDUP_SEP, REPLACING
"""
from __future__ import annotations
import re, hashlib
from typing import Dict

ALNUM_RE=re.compile(r'[A-Za-z0-9]+')

def stable_alias_id(canonical: str, variant: str) -> str:
    h=hashlib.sha256(f"{canonical}|{variant}".encode('utf-8')).hexdigest()[:24]
    return f"ALIAS_{h}"

def split_alpha_digit(name: str):
    # Split boundaries where letter->digit or digit->letter to allow inserted hyphen variants
    if not name:
        return [name]
    parts=[name[0]]
    for ch in name[1:]:
        if (ch.isdigit() and parts[-1][-1].isalpha()) or (ch.isalpha() and parts[-1][-1].isdigit()):
            parts.append(ch)
        else:
            parts[-1]+=ch
    return parts

def generate_variants(name: str) -> Dict[str,str]:
    variants: Dict[str,str]={}
    if not name:
        return variants
    canon=name.upper()
    variants[canon]='ORIGINAL'
    if '-' in canon:
        variants[canon.replace('-','')]='NO_HYPHEN'
        variants[canon.replace('-','_')]='WITH_UNDERSCORES'
    if '_' in canon:
        variants[canon.replace('_','')]='DEDUP_SEP'
        variants[canon.replace('_','-')]='WITH_HYPHENS'
    # Insert hyphens on alpha/digit boundaries if not already present
    parts=split_alpha_digit(canon.replace('-','').replace('_',''))
    if len(parts)>1:
        inserted='-'.join(parts)
        if inserted!=canon and inserted not in variants:
            variants[inserted]='INSERTED_HYPHENS'
    # Deduplicate separators
    dedup=re.sub(r'[-_]{2,}','-',canon)
    if dedup not in variants:
        variants[dedup]='DEDUP_SEP'
    return variants

def make_alias_record(canonical: str, variant: str, variant_type: str, kind: str, source: str):
    return {
        'alias_id': stable_alias_id(canonical, variant),
        'canonical_name': canonical,
        'alias': variant,
        'variant_type': variant_type,
        'kind': kind,
        'source_hint': source
    }
