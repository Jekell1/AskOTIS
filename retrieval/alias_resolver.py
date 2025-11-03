"""Repository-wide alias resolver for PROGRAM and COPYBOOK identifiers.

Public API:
    resolve(text: str) -> (canonical: str|None, kind: str|None, score: float)

Resolution Pipeline (ordered):
  1. Canonicalization & direct map lookup (rules-first)
  2. Direct alias variant lookup (pre-generated variants loaded from data/aliases.json)
  3. Fuzzy match against canonical universe using:
       a) Normalized Levenshtein distance similarity (1 - dist/len)
       b) 3-gram Jaccard similarity
       c) Combined score = 0.65 * levenshtein_sim + 0.35 * jaccard
     Only candidates with combined >= MIN_FUZZY_SCORE (default 0.72) considered.

Canonical Reference Set Structure (data/aliases.json):
  {
    "PROGRAM": { "CANON": ["alias1","alias2",...] },
    "COPYBOOK": { "CANON-CPY": ["aliasX", ...] }
  }

Copybook canonical forms intentionally suffixed with '-CPY' to avoid collisions.

Miss Logging:
  Unresolvable inputs append a line to logs/alias_misses.log with timestamp and raw text.

Thread-safety: Basic (single-process diagnostics). In multi-thread contexts, you may
wrap resolve() with your own locking if concurrent writes to the miss log are a concern.
"""
from __future__ import annotations
import os, json, re, time
from functools import lru_cache
from typing import Dict, List, Tuple, Optional, Set

ALIAS_FILE = os.getenv('ALIAS_FILE','data/aliases.json')
MISS_LOG = os.getenv('ALIAS_MISS_LOG','logs/alias_misses.log')
MIN_FUZZY_SCORE = float(os.getenv('ALIAS_MIN_FUZZY','0.68'))  # lowered slightly to allow single-char OCR slips
TRIGRAM = 3

EXT_RE = re.compile(r'\.(CBL|COB|CPY)$', re.IGNORECASE)
SEP_RE = re.compile(r'[ ._\-]+')
NON_ALNUM_RE = re.compile(r'[^A-Za-z0-9]')

class _AliasStore:
    def __init__(self, path: str):
        self.path = path
        self.alias_to_canon: Dict[str,Tuple[str,str]] = {}
        # Lowercase fast-path map to avoid O(N) scans for case-insensitive lookups
        self.alias_lower_to_canon: Dict[str,Tuple[str,str]] = {}
        self.canon_program: Set[str] = set()
        self.canon_copybook: Set[str] = set()
        self.trigrams_program: Dict[str,Set[str]] = {}
        self.trigrams_copybook: Dict[str,Set[str]] = {}
        self._loaded = False

    def load(self):
        if self._loaded:
            return
        try:
            data=json.load(open(self.path,'r',encoding='utf-8'))
        except Exception:
            data={}
        for canon, aliases in (data.get('PROGRAM') or {}).items():
            self.canon_program.add(canon)
            self.alias_to_canon.setdefault(canon,(canon,'PROGRAM'))
            for a in aliases:
                self.alias_to_canon.setdefault(a,(canon,'PROGRAM'))
        for canon, aliases in (data.get('COPYBOOK') or {}).items():
            self.canon_copybook.add(canon)
            self.alias_to_canon.setdefault(canon,(canon,'COPYBOOK'))
            for a in aliases:
                self.alias_to_canon.setdefault(a,(canon,'COPYBOOK'))
        # Build lowercase map (exclude exact duplicates already captured)
        for a,(c,k) in self.alias_to_canon.items():
            low=a.lower()
            # Preserve first-seen to keep deterministic canonical preference
            self.alias_lower_to_canon.setdefault(low,(c,k))
        for c in self.canon_program:
            self.trigrams_program[c]=_trigrams(c)
        for c in self.canon_copybook:
            self.trigrams_copybook[c]=_trigrams(c)
        self._loaded=True

STORE=_AliasStore(ALIAS_FILE)

def canonicalize(raw: str) -> str:
    if raw is None:
        return ''
    name=os.path.basename(raw).strip()
    name=EXT_RE.sub('', name)
    name=SEP_RE.sub('-', name)
    name=NON_ALNUM_RE.sub('-', name)
    name=name.strip('-')
    return name.upper()

def _levenshtein(a: str, b: str) -> int:
    if a==b: return 0
    if not a: return len(b)
    if not b: return len(a)
    prev=list(range(len(b)+1))
    cur=[0]*(len(b)+1)
    for i,ca in enumerate(a,1):
        cur[0]=i
        for j,cb in enumerate(b,1):
            cost=0 if ca==cb else 1
            cur[j]=min(prev[j]+1, cur[j-1]+1, prev[j-1]+cost)
        prev,cur=cur,prev
    return prev[-1]

def _trigrams(s: str) -> Set[str]:
    s=f"#{s}#"
    return {s[i:i+TRIGRAM] for i in range(len(s)-TRIGRAM+1)}

@lru_cache(maxsize=4096)
def _trigram_set_cached(s: str) -> Set[str]:
    return _trigrams(s)

def _jaccard(a: Set[str], b: Set[str]) -> float:
    if not a or not b: return 0.0
    inter=len(a & b)
    if inter==0: return 0.0
    return inter / len(a | b)

def _ensure_loaded():
    if not STORE._loaded:
        STORE.load()

@lru_cache(maxsize=8192)
def _fuzzy_lookup(norm: str) -> Tuple[Optional[str], Optional[str], float]:
    best_c=None; best_k=None; best_score=0.0
    qtris=_trigram_set_cached(norm)
    for canon,tris in STORE.trigrams_program.items():
        if not (qtris & tris):
            continue
        lev=_levenshtein(norm, canon)
        lev_sim=1 - (lev / max(len(norm), len(canon)))
        jac=_jaccard(qtris, tris)
        score=0.65*lev_sim + 0.35*jac
        if score>best_score:
            best_c=canon; best_k='PROGRAM'; best_score=score
    for canon,tris in STORE.trigrams_copybook.items():
        if not (qtris & tris):
            continue
        lev=_levenshtein(norm, canon)
        lev_sim=1 - (lev / max(len(norm), len(canon)))
        jac=_jaccard(qtris, tris)
        score=0.65*lev_sim + 0.35*jac
        if score>best_score:
            best_c=canon; best_k='COPYBOOK'; best_score=score
    if best_score >= MIN_FUZZY_SCORE:
        return best_c, best_k, best_score
    return None, None, 0.0

def resolve(text: str) -> Tuple[Optional[str], Optional[str], float]:
    _ensure_loaded()
    if not text:
        return None, None, 0.0
    raw=text.strip()
    norm=canonicalize(raw)
    if raw in STORE.alias_to_canon:
        c,k=STORE.alias_to_canon[raw]; return c,k,1.0
    if norm in STORE.alias_to_canon:
        c,k=STORE.alias_to_canon[norm]; return c,k,0.99
    low=raw.lower()
    if low in STORE.alias_lower_to_canon:
        c,k=STORE.alias_lower_to_canon[low]
        # Distinguish from exact match by score tier
        if (raw, (c,k)) not in STORE.alias_to_canon.items():
            return c,k,0.95
        else:
            return c,k,1.0
    c,k,score=_fuzzy_lookup(norm)
    if c:
        return c,k,score
    # 4. Dynamic fallback: attempt OCR swaps & hyphen removal & copybook suffix guess.
    #    We broaden the ambiguous character set and perform limited multi-swap expansion.
    ocr_swaps=[
        ('0','O'),('O','0'),
        ('1','I'),('I','1'),
        ('L','I'),('I','L'),
        ('5','S'),('S','5'),
        ('2','Z'),('Z','2'),
        ('8','B'),('B','8'),
        ('4','A'),('A','4')
    ]
    variants={norm, norm.replace('-','')}
    if not norm.endswith('-CPY'):
        variants.add(f"{norm}-CPY")
    # single-pass global swaps (all occurrences)
    for a,b in ocr_swaps:
        if a in norm:
            variants.add(norm.replace(a,b))
    # per-position single swaps to avoid over-replacement (e.g., DALLY -> DAILY)
    ambig_map = {}
    for a,b in ocr_swaps:
        ambig_map.setdefault(a, set()).add(b)
    chars=list(norm)
    for i,ch in enumerate(chars):
        if ch in ambig_map:
            for repl in ambig_map[ch]:
                if repl!=ch:
                    v = ''.join(chars[:i] + [repl] + chars[i+1:])
                    variants.add(v)
    # limited breadth multi-swap (apply second layer on newly added variants once)
    expanded=list(variants)
    for v in expanded:
        for a,b in ocr_swaps:
            if a in v and len(variants) < 64:  # guard explosion
                variants.add(v.replace(a,b))
    for v in variants:
        if v in STORE.alias_to_canon:
            c,k=STORE.alias_to_canon[v]; return c,k,0.70
    _log_miss(raw)
    return None, None, 0.0

def _log_miss(raw: str):
    try:
        os.makedirs(os.path.dirname(MISS_LOG) or '.', exist_ok=True)
        with open(MISS_LOG,'a',encoding='utf-8') as f:
            f.write(f"{time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())}\t{raw}\n")
    except Exception:
        pass

if __name__=='__main__':
    import sys
    for token in sys.argv[1:] or ['APIPAY','TIM-360','LONPF2','IRMAIN','IRMAlN','DAlLY','screen.cpy','SCREEN']:
        print(token,'->',resolve(token))

