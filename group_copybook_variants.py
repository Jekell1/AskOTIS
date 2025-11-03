#!/usr/bin/env python3
"""group_copybook_variants.py

Detects variant groups among copybooks and assigns:
  - canonical_base
  - variant_kind (IDENTICAL|EXACT_NAME|PREFIX|SUFFIX|OTHER)
  - variant_group_id (stable hash)

Writes partial updates to `cobol-copybooks` index.

Variant logic (initial heuristic):
  1. Normalize name => upper, strip extension, collapse multiple underscores/hyphens.
  2. Derive canonical base by removing trailing digits or single letter suffix.
  3. Group by canonical base; inside group compute similarity features.
  4. variant_kind rules:
      * Exact same original name => EXACT_NAME
      * Name differs only by trailing digits => SUFFIX
      * Name shares canonical prefix >= 80% length => PREFIX
      * Else OTHER
  5. variant_group_id = sha1(canonical_base).hexdigest()[:16]

Future enhancements: structural diff on contents, semantic tags, frequency analysis.
"""
import os, sys, re, json, argparse, hashlib, requests
from typing import List, Dict, Tuple, DefaultDict
from collections import defaultdict

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
ENDPOINT = os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT")
KEY = os.environ.get("AZURE_SEARCH_KEY") or os.environ.get("SEARCH_KEY")
BASE_INDEX = "cobol-copybooks"

session = requests.Session()

NAME_CLEAN_RE = re.compile(r"[^A-Z0-9]+")
TRAILING_VARIANT_RE = re.compile(r"([_-]?)(\d+|[A-Z])$")


def fetch_copybooks(search_term: str = "*", index: str = None, debug: bool=False) -> List[Dict]:
    if index is None:
        raise ValueError("index required")
    url = f"{ENDPOINT}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body = {"search": search_term, "top": 8000}
    r = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body, timeout=60)
    if r.status_code >= 300:
        if debug:
            print("Copybook primary error", r.status_code, r.text[:400])
        r.raise_for_status()
    vals = r.json().get('value', [])
    if vals:
        return vals
    if search_term != "*":
        body2 = {"search": "*", "top": 8000}
        r2 = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body2, timeout=60)
        if r2.status_code >= 300:
            if debug:
                print("Copybook fallback error", r2.status_code, r2.text[:400])
            r2.raise_for_status()
        return r2.json().get('value', [])
    return []


def canonical_base(name: str) -> Tuple[str, str]:
    base = (name or '').upper().split('.')[0]
    base = NAME_CLEAN_RE.sub('_', base)
    base = re.sub(r"__+", "_", base).strip('_')
    kind_detail = "RAW"
    m = TRAILING_VARIANT_RE.search(base)
    if m and len(base) > 3:
        base2 = base[:m.start()].rstrip('_')
        if len(base2) >= 3:
            kind_detail = "TRUNCATED_SUFFIX"
            base = base2
    return base, kind_detail


def classify_variant(canon: str, original: str) -> str:
    oclean = original.upper().split('.')[0]
    if oclean == canon:
        return "EXACT_NAME"
    if oclean.startswith(canon) and len(canon) >= int(0.8 * len(oclean)):
        return "PREFIX"
    if canon.startswith(oclean) and len(oclean) >= int(0.8 * len(canon)):
        return "PREFIX"
    # trailing digits
    if re.sub(r"\d+$", "", oclean) == canon:
        return "SUFFIX"
    return "OTHER"


def group_variants(rows: List[Dict]):
    groups: DefaultDict[str, List[Dict]] = defaultdict(list)
    decorated = []
    for r in rows:
        # actual field present is copybook_name
        nm = r.get('copybook_name') or r.get('name','')
        cbase, ckind = canonical_base(nm)
        r['_canonical'] = cbase
        r['_canon_kind'] = ckind
        groups[cbase].append(r)
        decorated.append(r)
    outputs = []
    for cbase, items in groups.items():
        gid = hashlib.sha1(cbase.encode()).hexdigest()[:16]
        for it in items:
            nm = it.get('copybook_name') or it.get('name','')
            vkind = classify_variant(cbase, nm)
            outputs.append((it, cbase, vkind, gid))
    return outputs


def build_actions(enriched):
    acts = []
    for it, cbase, vkind, gid in enriched:
        acts.append({
            "@search.action": "merge",
            "copybook_id": it['copybook_id'],
            "canonical_base": cbase,
            "variant_kind": vkind,
            "variant_group_id": gid,
        })
    return acts


def upload(actions, index_name: str):
    if not actions:
        print("No actions to upload")
        return
    url = f"{ENDPOINT}/indexes/{index_name}/docs/index?api-version={API_VERSION}"
    chunk = 900
    for i in range(0, len(actions), chunk):
        batch = {"value": actions[i:i+chunk]}
        r = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=batch, timeout=120)
        if r.status_code >= 300:
            print("Upload error", r.status_code, r.text[:400])
            r.raise_for_status()
        else:
            print(f"Uploaded {len(batch['value'])} copybook variant updates")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--term", default="*", help="Search term to prefilter (program or pattern)")
    ap.add_argument("--index-suffix", default="", help="Optional index name suffix like -v2")
    ap.add_argument("--debug", action="store_true")
    ap.add_argument("--dry", action="store_true")
    args = ap.parse_args()
    if not ENDPOINT or not KEY:
        print("Missing search endpoint or key env vars.", file=sys.stderr)
        sys.exit(2)
    suffix = args.index_suffix or ""
    if suffix and not suffix.startswith('-'):
        suffix = '-' + suffix
    index_name = BASE_INDEX + suffix
    rows = fetch_copybooks(args.term, index=index_name, debug=args.debug)
    print(f"Fetched {len(rows)} copybooks")
    enriched = group_variants(rows)
    acts = build_actions(enriched)
    if args.dry:
        print(json.dumps(acts[:15], indent=2))
        print(f"Prepared {len(acts)} actions (dry run)")
        return
    upload(acts, index_name)

if __name__ == '__main__':
    main()
