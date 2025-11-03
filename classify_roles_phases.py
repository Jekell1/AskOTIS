#!/usr/bin/env python3
"""classify_roles_phases.py

Assigns role & phase metadata to paragraphs and uploads partial updates to the
`cobol-paragraphs` index.

Strategy (initial heuristic skeleton):
 1. Fetch paragraphs for target program(s) via Search REST.
 2. For each paragraph name, infer role from token patterns.
 3. Infer coarse phase ordering by scanning line numbers and role transitions.
 4. Produce merge actions (partial) with new fields: role, phase, phase_seq, role_confidence, phase_confidence.
 5. POST to /indexes('cobol-paragraphs')/docs/index with batch size <= 1000.

Heuristics (expand later):
  - Names containing INIT/OPEN/LOAD/CONNECT => INIT phase
  - TEST/VALIDATE/SKIP => GATING phase
  - PAYOFF/PAYMENT/POSTING/REVERSAL/BANKRUPT => POSTING phase
  - CLOSE/END/REMOVE => FINALIZE phase
  - Default fallback => PROCESS phase

NOTE: This script is intentionally lightweight; richer ML/classification can be layered later.
"""
import os, sys, json, re, argparse, requests, math
from typing import List, Dict, Tuple

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
ENDPOINT = os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT")
KEY = os.environ.get("AZURE_SEARCH_KEY") or os.environ.get("SEARCH_KEY")
BASE_INDEX = "cobol-paragraphs"
FILES_INDEX = "cobol-files"
CHUNKS_INDEX = "code-chunks"

ROLE_PATTERNS = [
    (re.compile(r"INIT|OPEN|LOAD|CONNECT"), "INIT"),
    (re.compile(r"TEST|VALIDATE|SKIP"), "GATING"),
    (re.compile(r"PAYOFF|PAYMENT|POSTING|REVERSAL|BANKRUPT|LATE"), "POSTING"),
    (re.compile(r"CLOSE|END|REMOVE"), "FINALIZE"),
]

PHASE_ORDER = ["INIT", "GATING", "POSTING", "FINALIZE"]

session = requests.Session()

def search_paragraphs(program_id: str, index: str, top: int = 5000, debug: bool=False) -> List[Dict]:
    url = f"{ENDPOINT}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body_primary = {"search": program_id, "top": top}
    r = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body_primary, timeout=60)
    if r.status_code >= 300:
        if debug:
            print("Primary search error", r.status_code, r.text[:400])
        r.raise_for_status()
    vals = r.json().get('value', [])
    if vals:
        return vals
    # Fallback: get broader set then filter
    body_fb = {"search": "*", "top": top}
    r2 = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body_fb, timeout=60)
    if r2.status_code >= 300:
        if debug:
            print("Fallback search error", r2.status_code, r2.text[:400])
        r2.raise_for_status()
    all_vals = r2.json().get('value', [])
    token = program_id.upper()
    filtered = [p for p in all_vals if token in (p.get('file_id','').upper() + p.get('name','').upper())]
    if debug:
        print(f"Fallback broad fetched {len(all_vals)}; filtered to {len(filtered)} by token {token}")
    return filtered

def fetch_paragraphs_by_file_ids(file_ids: List[str], index: str, debug: bool=False, batch: int=32) -> List[Dict]:
    out = []
    headers = {"api-key": KEY, "Content-Type": "application/json"}
    url = f"{ENDPOINT}/indexes/{index}/docs/search?api-version={API_VERSION}"
    for i in range(0, len(file_ids), batch):
        subset = file_ids[i:i+batch]
        # Use filter with search=* to narrow by file_id
        flt = ' or '.join([f"file_id eq '{fid}'" for fid in subset])
        body = {"search": "*", "filter": flt, "top": 2000}
        try:
            r = session.post(url, headers=headers, json=body, timeout=60)
            if r.status_code < 300:
                vals = r.json().get('value', [])
                out.extend(vals)
            elif debug:
                print('file_id batch error', r.status_code, r.text[:200])
        except Exception as e:
            if debug:
                print('file_id batch exception', e)
    if debug:
        print(f"Fetched {len(out)} paragraphs via direct file_id filtering")
    return out

def resolve_file_ids(program_token: str, debug: bool=False, max_ids: int=20) -> List[str]:
    token = program_token.upper()
    headers = {"api-key": KEY, "Content-Type": "application/json"}
    url_files = f"{ENDPOINT}/indexes/{FILES_INDEX}/docs/search?api-version={API_VERSION}"
    body = {"search": program_token, "top": 100}
    file_ids = set()
    try:
        r = session.post(url_files, headers=headers, json=body, timeout=30)
        if r.status_code < 300:
            for d in r.json().get('value', []):
                # Candidate field names: file_id, program, name, path
                fid = d.get('file_id') or d.get('id')
                if not fid:
                    continue
                # Check textual fields for token presence
                concat = ' '.join(str(d.get(k,'')) for k in ('program','name','path','file_name'))
                if token in concat.upper():
                    file_ids.add(fid)
        elif debug:
            print('files index search error', r.status_code, r.text[:200])
    except Exception as e:
        if debug:
            print('files resolver exception', e)
    if not file_ids:
        # fallback via code-chunks
        url_chunks = f"{ENDPOINT}/indexes/{CHUNKS_INDEX}/docs/search?api-version={API_VERSION}"
        body2 = {"search": program_token, "top": 200}
        try:
            r2 = session.post(url_chunks, headers=headers, json=body2, timeout=45)
            if r2.status_code < 300:
                for d in r2.json().get('value', []):
                    fid = d.get('file_id')
                    if not fid:
                        continue
                    # Accept if program token appears in any path/name/snippet
                    concat = ' '.join(str(d.get(k,'')) for k in ('file_path','path','name','program','chunk_name'))
                    if token in concat.upper():
                        file_ids.add(fid)
            elif debug:
                print('chunks resolver error', r2.status_code, r2.text[:200])
        except Exception as e:
            if debug:
                print('chunks resolver exception', e)
    out = list(file_ids)[:max_ids]
    if debug:
        print(f"Resolved {len(out)} file_ids for token {program_token}: {out}")
    return out

def infer_role(name: str) -> Tuple[str, float]:
    up = name.upper()
    for rx, role in ROLE_PATTERNS:
        if rx.search(up):
            return role, 0.9
    return "PROCESS", 0.6

def assign_phases(paras: List[Dict]):
    # Sort by start_line if available; maintain original order otherwise
    def key_fn(p):
        return p.get("start_line", 10**9)
    ordered = sorted(paras, key=key_fn)
    phase_seq_counter = 0
    current_phase = None
    results = []
    for p in ordered:
        role = p.get("role") or p.get("_inferred_role")
        if not role:
            role, _ = infer_role(p.get("name", ""))
        # Map role to phase (could be identical set for now)
        if role in ("INIT",): ph = "INIT"
        elif role in ("GATING",): ph = "GATING"
        elif role in ("POSTING",): ph = "POSTING"
        elif role in ("FINALIZE",): ph = "FINALIZE"
        else: ph = "PROCESS"
        if ph != current_phase:
            current_phase = ph
            phase_seq_counter += 1
        results.append((p, role, ph, phase_seq_counter))
    return results

def build_actions(enriched):
    acts = []
    for p, role, phase, seq in enriched:
        conf_role = 0.9 if role != "PROCESS" else 0.6
        conf_phase = 0.85 if phase != "PROCESS" else 0.5
        acts.append({
            "@search.action": "merge",
            "para_id": p["para_id"],
            "role": role,
            "phase": phase,
            "phase_seq": seq,
            "role_confidence": conf_role,
            "phase_confidence": conf_phase,
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
            print(f"Uploaded {len(batch['value'])} role/phase updates")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("program", help="Program identifier token (search term)")
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
    file_ids = resolve_file_ids(args.program, debug=args.debug)
    paras = []
    if file_ids:
        paras = fetch_paragraphs_by_file_ids(file_ids, index_name, debug=args.debug)
    if not paras:  # fallback to token search
        token_paras = search_paragraphs(args.program, index_name, debug=args.debug)
        if file_ids:
            fidset = set(file_ids)
            token_paras = [p for p in token_paras if p.get('file_id') in fidset]
        paras = token_paras
    print(f"Fetched {len(paras)} paragraphs (raw)")
    enriched = assign_phases(paras)
    acts = build_actions(enriched)
    if args.dry:
        print(json.dumps(acts[:10], indent=2))
        print(f"Prepared {len(acts)} actions (dry run)")
        return
    upload(acts, index_name)

if __name__ == "__main__":
    main()
