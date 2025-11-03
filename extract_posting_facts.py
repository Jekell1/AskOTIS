#!/usr/bin/env python3
"""extract_posting_facts.py

Extract higher-level posting / action facts from paragraphs + code-chunks and
write enriched records (partial merge) back into `cobol-facts` or create new fact entries
if not present.

Initial scope:
  - Detect action_role (DEBIT, CREDIT, REVERSAL, ACCRUAL) heuristically
  - posting_type (LEDGER, CUSTOMER, GL, FEE, INTEREST) via keyword scan
  - gating_cond: best-effort extraction of IF condition lines referencing status/flags
  - sources: list of paragraph ids or chunk ids contributing evidence
  - fact_confidence: conservative numeric (0.5..0.9)

Approach (heuristic baseline):
 1. Fetch paragraphs for target program (or all) limited to those whose text contains keywords.
 2. For each candidate paragraph text, run regex patterns for action verbs and accounting terms.
 3. Derive a synthetic fact_id (sha1 of program + para name + role label) if new.
 4. Upsert via merge-or-upload to `cobol-facts`.

NOTE: This script purposely avoids LLM calls at first; can be extended later.
"""
import os, sys, re, json, argparse, hashlib, requests
from typing import List, Dict

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
ENDPOINT = os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT")
KEY = os.environ.get("AZURE_SEARCH_KEY") or os.environ.get("SEARCH_KEY")
PARA_BASE = "cobol-paragraphs"
FACT_BASE = "cobol-facts"
FILES_INDEX = "cobol-files"
CHUNKS_INDEX = "code-chunks"

session = requests.Session()

ACTION_PATTERNS = [
    (re.compile(r"\bREVERS(e|AL)\b", re.I), "REVERSAL"),
    (re.compile(r"\bADJUST\b", re.I), "ADJUST"),
    (re.compile(r"\bACCRUAL?\b", re.I), "ACCRUAL"),
    (re.compile(r"\bPOST\b", re.I), "POSTING"),
    (re.compile(r"\bPAY(MENT)?\b", re.I), "PAYMENT"),
    (re.compile(r"\bCHARGE\b", re.I), "CHARGE"),
]

POSTING_TYPE_PATTERNS = [
    (re.compile(r"GENERAL\s+LEDGER|\bGL\b", re.I), "GL"),
    (re.compile(r"CUSTOMER|CLIENT", re.I), "CUSTOMER"),
    (re.compile(r"INTEREST", re.I), "INTEREST"),
    (re.compile(r"FEE|CHARGE", re.I), "FEE"),
    (re.compile(r"AMORT(IZE|IZATION)", re.I), "AMORTIZATION"),
]

GATING_IF_RE = re.compile(r"^\s*(IF|EVALUATE)\s+(.+)", re.I)
STATUS_FLAG_HINT = re.compile(r"STATUS|FLAG|SWITCH|CODE|ERROR|VALID|ELIGIBLE", re.I)


def fetch_paragraphs(term: str, para_index: str, top: int = 4000, debug: bool=False):
    url = f"{ENDPOINT}/indexes/{para_index}/docs/search?api-version={API_VERSION}"
    body = {"search": term, "top": top}
    r = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body, timeout=90)
    if r.status_code >= 300:
        if debug:
            print("Paragraph primary error", r.status_code, r.text[:400])
        r.raise_for_status()
    vals = r.json().get('value', [])
    if vals:
        return vals
    body2 = {"search": "*", "top": top}
    r2 = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body2, timeout=90)
    if r2.status_code >= 300:
        if debug:
            print("Paragraph fallback error", r2.status_code, r2.text[:400])
        r2.raise_for_status()
    all_vals = r2.json().get('value', [])
    token = term.upper()
    filtered = [p for p in all_vals if token in (p.get('file_id','').upper() + p.get('name','').upper())]
    if debug:
        print(f"Fallback paragraphs broad {len(all_vals)} filtered {len(filtered)}")
    return filtered

def fetch_paragraphs_by_file_ids(file_ids, para_index: str, debug: bool=False, batch: int=32):
    out=[]
    headers={"api-key": KEY, "Content-Type": "application/json"}
    url = f"{ENDPOINT}/indexes/{para_index}/docs/search?api-version={API_VERSION}"
    for i in range(0, len(file_ids), batch):
        subset = file_ids[i:i+batch]
        flt = ' or '.join([f"file_id eq '{fid}'" for fid in subset])
        body = {"search": "*", "filter": flt, "top": 2000}
        try:
            r = session.post(url, headers=headers, json=body, timeout=60)
            if r.status_code < 300:
                out.extend(r.json().get('value', []))
            elif debug:
                print('file_id para fetch error', r.status_code, r.text[:200])
        except Exception as e:
            if debug:
                print('file_id para fetch exception', e)
    if debug:
        print(f"Fetched {len(out)} paragraphs via file_id filter")
    return out

def resolve_file_ids(program_token: str, debug: bool=False, max_ids: int=20):
    token = program_token.upper()
    headers = {"api-key": KEY, "Content-Type": "application/json"}
    file_ids = set()
    # first cobol-files
    try:
        url_files = f"{ENDPOINT}/indexes/{FILES_INDEX}/docs/search?api-version={API_VERSION}"
        r = session.post(url_files, headers=headers, json={"search": program_token, "top": 100}, timeout=30)
        if r.status_code < 300:
            for d in r.json().get('value', []):
                fid = d.get('file_id') or d.get('id')
                if not fid:
                    continue
                concat = ' '.join(str(d.get(k,'')) for k in ('program','name','path','file_name'))
                if token in concat.upper():
                    file_ids.add(fid)
        elif debug:
            print('files resolver status', r.status_code)
    except Exception as e:
        if debug: print('files resolver exception', e)
    if not file_ids:
        # fallback chunks
        try:
            url_chunks = f"{ENDPOINT}/indexes/{CHUNKS_INDEX}/docs/search?api-version={API_VERSION}"
            r2 = session.post(url_chunks, headers=headers, json={"search": program_token, "top": 200}, timeout=45)
            if r2.status_code < 300:
                for d in r2.json().get('value', []):
                    fid = d.get('file_id')
                    if not fid: continue
                    concat = ' '.join(str(d.get(k,'')) for k in ('file_path','path','name','program','chunk_name'))
                    if token in concat.upper():
                        file_ids.add(fid)
            elif debug:
                print('chunks resolver status', r2.status_code)
        except Exception as e:
            if debug: print('chunks resolver exception', e)
    out = list(file_ids)[:max_ids]
    if debug:
        print(f"Resolved file_ids for {program_token}: {out}")
    return out


def detect_action_role(text: str):
    for rx, label in ACTION_PATTERNS:
        if rx.search(text):
            return label
    return None


def detect_posting_type(text: str):
    for rx, label in POSTING_TYPE_PATTERNS:
        if rx.search(text):
            return label
    return None


def extract_gating_condition(text: str):
    lines = text.splitlines()
    for ln in lines:
        if GATING_IF_RE.search(ln) and STATUS_FLAG_HINT.search(ln):
            return ln.strip()[:300]
    return None


def build_fact_id(program: str, para_name: str, action_role: str):
    raw = f"{program}:{para_name}:{action_role}".upper()
    return hashlib.sha1(raw.encode()).hexdigest()[:20]


def synthesize_fact_text(program: str, para: Dict, action_role: str, posting_type: str, gating: str):
    """Create a concise human-readable fact statement.

    We avoid heavy NLP – just deterministic phrasing for now.
    """
    name = para.get('name') or para.get('para_id')
    pieces = [f"Program {program}"]
    if name:
        pieces.append(f"paragraph {name}")
    verb_bits = []
    if action_role:
        verb_bits.append(action_role.lower())
    if posting_type:
        verb_bits.append(posting_type.lower())
    if verb_bits:
        pieces.append("performs " + ' '.join(verb_bits))
    if gating:
        pieces.append(f"when condition: {gating}")
    return ' '.join(pieces)


def build_actions(program: str, paragraphs: List[Dict], synthesize: bool=False):
    acts = []
    for p in paragraphs:
        # Prefer full paragraph text if present (v3 index); fallback to name.
        text = p.get('text') or p.get('snippet') or p.get('name','')
        action_role = detect_action_role(text)
        posting_type = detect_posting_type(text)
        gating = extract_gating_condition(text)
        if not (action_role or posting_type or gating):
            continue  # skip paragraphs with no signal
        fact_id = build_fact_id(program, p.get('name','?'), action_role or posting_type or 'FACT')
        conf = 0.5 + 0.1 * sum(1 for x in [action_role, posting_type, gating] if x)
        sources = [p.get('para_id')]
        doc = {
            "@search.action": "mergeOrUpload",
            "fact_id": fact_id,
            "program_id": program,
            "action_role": action_role,
            "posting_type": posting_type,
            "gating_cond": gating,
            "fact_confidence": conf,
            "sources": sources,
        }
        if synthesize:
            doc["fact_text"] = synthesize_fact_text(program, p, action_role, posting_type, gating)
            # Explicitly mark has_vector False (will be updated by embedding script later)
            doc["has_vector"] = False
        acts.append(doc)
    return acts


def upload(actions, fact_index: str):
    if not actions:
        print("No facts to upload")
        return
    url = f"{ENDPOINT}/indexes/{fact_index}/docs/index?api-version={API_VERSION}"
    chunk = 900
    for i in range(0, len(actions), chunk):
        batch = {"value": actions[i:i+chunk]}
        r = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=batch, timeout=120)
        if r.status_code >= 300:
            print("Upload error", r.status_code, r.text[:400])
            r.raise_for_status()
        else:
            print(f"Uploaded {len(batch['value'])} facts")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("program", help="Program identifier token")
    ap.add_argument("--index-suffix", default="", help="Apply suffix to paragraph & fact indexes (e.g. -v2)")
    ap.add_argument("--debug", action="store_true")
    ap.add_argument("--dry", action="store_true")
    ap.add_argument("--synthesize-text", action="store_true", help="Generate fact_text field for each fact")
    args = ap.parse_args()
    if not ENDPOINT or not KEY:
        print("Missing search endpoint or key env vars.", file=sys.stderr)
        sys.exit(2)
    suffix = args.index_suffix or ""
    if suffix and not suffix.startswith('-'):
        suffix = '-' + suffix
    para_index = PARA_BASE + suffix
    fact_index = FACT_BASE + suffix
    file_ids = resolve_file_ids(args.program, debug=args.debug)
    paras = []
    if file_ids:
        paras = fetch_paragraphs_by_file_ids(file_ids, para_index, debug=args.debug)
    if not paras:
        fallback = fetch_paragraphs(args.program, para_index, debug=args.debug)
        if file_ids:
            fidset = set(file_ids)
            fallback = [p for p in fallback if p.get('file_id') in fidset]
        paras = fallback
    print(f"Fetched {len(paras)} paragraphs for candidate fact extraction")
    acts = build_actions(args.program, paras, synthesize=args.synthesize_text)
    print(f"Built {len(acts)} fact actions")
    if args.dry:
        print(json.dumps(acts[:15], indent=2))
        return
    upload(acts, fact_index)

if __name__ == '__main__':
    main()
