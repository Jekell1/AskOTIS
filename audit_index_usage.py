#!/usr/bin/env python3
"""audit_index_usage.py

List Azure Cognitive Search indexes, scan the local workspace for references to
index names, classify latest vs superseded versions, and recommend deletion
candidates. Generates a JSON report and optional delete script templates.

Usage:
  python audit_index_usage.py --emit-delete-script

Environment:
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT  - https://<service>.search.windows.net
  AZURE_SEARCH_KEY / SEARCH_KEY            - admin or query key (admin needed for deletion)

This script performs NO deletions. Generated delete scripts are templates only.
"""
import os, sys, re, json, argparse, time, pathlib, http.client
from urllib.parse import urlparse
from typing import Dict, Any, List

API_VERSION = "2024-07-01"
TEXT_EXTS = {".py",".ps1",".json",".yml",".yaml",".md",".txt",".html",".ts",".js"}
BINARY_SKIP = {".png",".jpg",".jpeg",".gif",".zip",".exe",".dll",".so",".pyc",".pyd"}

# Patterns that force retention
RETENTION_PATTERNS = [r"\bactive\b", r"\bprod\b"]

DEFAULT_OUTPUT = "index_audit_report.json"

# ----------------- Helpers -----------------

def load_endpoint_key():
    ep = os.getenv("AZURE_SEARCH_ENDPOINT") or os.getenv("SEARCH_ENDPOINT")
    key = os.getenv("AZURE_SEARCH_KEY") or os.getenv("SEARCH_KEY")
    if not ep or not key:
        print("ERROR: Missing AZURE_SEARCH_ENDPOINT/AZURE_SEARCH_KEY (or SEARCH_ENDPOINT/SEARCH_KEY)", file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

def http_json(method: str, endpoint: str, path: str, key: str, body: str|None=None):
    parsed = urlparse(endpoint)
    conn = http.client.HTTPSConnection(parsed.netloc)
    headers = {"api-key": key, "Content-Type": "application/json"}
    conn.request(method, path, body if body else "", headers)
    resp = conn.getresponse()
    data = resp.read()
    if resp.status >= 300:
        raise RuntimeError(f"{method} {path} => {resp.status} {data[:400]!r}")
    if not data:
        return {}
    return json.loads(data.decode('utf-8', errors='replace'))

def list_indexes(endpoint: str, key: str) -> List[str]:
    data = http_json("GET", endpoint, f"/indexes?api-version={API_VERSION}", key)
    return [v['name'] for v in data.get('value', [])]

def version_token(name: str) -> str:
    m = re.search(r"(v\d+l|v\d+)$", name)
    return m.group(1) if m else ""

def version_rank(tok: str) -> int:
    if not tok: return -1
    # v3l > v3 > v2 > v1 using multiplier; treat 'l' as +5 weight
    try:
        if tok.endswith('l'):
            base = int(tok[1:-1])
            return base * 10 + 5
        return int(tok[1:]) * 10
    except Exception:
        return -1

def classify_family(names: List[str]) -> Dict[str, Dict[str, Any]]:
    fam = {}
    for n in names:
        vt = version_token(n)
        base = n[:-len(vt)].rstrip('-') if vt and n.endswith(vt) else n
        fam.setdefault(base, []).append(n)
    decisions = {}
    for base, members in fam.items():
        if len(members) == 1:
            decisions[members[0]] = {"family_base": base, "is_latest": True, "newer_exists": False}
            continue
        ranked = sorted(members, key=lambda x: version_rank(version_token(x)), reverse=True)
        latest = ranked[0]
        for m in members:
            decisions[m] = {
                "family_base": base,
                "is_latest": m == latest,
                "newer_exists": m != latest
            }
    return decisions

def scan_workspace(root: pathlib.Path, names: List[str]):
    refs = {n: {"count":0, "files": set()} for n in names}
    patterns = {n: re.compile(rf"\b{re.escape(n)}\b") for n in names}
    for p in root.rglob('*'):
        if not p.is_file():
            continue
        ext = p.suffix.lower()
        if ext in BINARY_SKIP:
            continue
        if ext and ext not in TEXT_EXTS:
            continue
        try:
            txt = p.read_text(encoding='utf-8', errors='ignore')
        except Exception:
            continue
        for name, rgx in patterns.items():
            hits = rgx.findall(txt)
            if hits:
                refs[name]["count"] += len(hits)
                refs[name]["files"].add(str(p))
    return refs

def retention_reasons(name: str, ref_entry: Dict[str,Any], fam: Dict[str,Any]):
    reasons = []
    if ref_entry['count']>0:
        reasons.append('referenced_in_repo')
    if fam.get('is_latest'):
        reasons.append('latest_version')
    for pat in RETENTION_PATTERNS:
        if re.search(pat, name):
            reasons.append('policy_pattern')
    return reasons

def deletion_reason(name: str, ref_entry: Dict[str,Any], fam: Dict[str,Any]):
    if ref_entry['count']>0:
        return None
    if fam.get('newer_exists'):
        return 'superseded_version'
    if version_token(name)=="":
        if not fam.get('is_latest'):
            return 'unversioned_superseded'
    return 'unused_unreferenced'

# ----------------- Main -----------------

def main():
    ap = argparse.ArgumentParser(description='Audit Azure Cognitive Search index usage and recommend deletions.')
    ap.add_argument('--root', default='.', help='Workspace root to scan')
    ap.add_argument('--output', default=DEFAULT_OUTPUT, help='JSON report output path')
    ap.add_argument('--emit-delete-script', action='store_true', help='Generate delete_indexes.ps1 / .sh templates')
    args = ap.parse_args()

    endpoint, key = load_endpoint_key()
    print(f"[info] Endpoint: {endpoint}")
    print('[info] Listing indexes...')
    indexes = sorted(list_indexes(endpoint, key))
    print(f"[info] Found {len(indexes)} indexes")
    fam_meta = classify_family(indexes)
    print('[info] Scanning workspace for references...')
    refs = scan_workspace(pathlib.Path(args.root), indexes)

    report = {
        'endpoint': endpoint,
        'index_count': len(indexes),
        'generated_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
        'indexes': []
    }
    delete_candidates = []
    keep = []

    for name in indexes:
        r = refs[name]
        meta = fam_meta.get(name, {})
        keep_reasons = retention_reasons(name, r, meta)
        del_reason = deletion_reason(name, r, meta)
        entry = {
            'name': name,
            'references': r['count'],
            'files': sorted(r['files']),
            'family_base': meta.get('family_base'),
            'version_token': version_token(name),
            'is_latest_in_family': meta.get('is_latest', False),
            'newer_exists': meta.get('newer_exists', False),
            'retain_reasons': keep_reasons,
            'delete_reason': del_reason
        }
        if del_reason:
            delete_candidates.append(name)
        else:
            keep.append(name)
        report['indexes'].append(entry)

    report['summary'] = {
        'keep_count': len(keep),
        'delete_candidate_count': len(delete_candidates),
        'delete_candidates': delete_candidates
    }

    with open(args.output,'w',encoding='utf-8') as f:
        json.dump(report, f, indent=2)
    print(f"[info] Report written: {args.output}")

    if args.emit_delete_script:
        if delete_candidates:
            ps_lines = []
            sh_lines = ["set -e"]
            for name in delete_candidates:
                # Escape curly braces inside f-string so the PowerShell hashtable literal is emitted correctly.
                ps_lines.append(
                    f"Invoke-RestMethod -Method Delete -Uri \"$env:AZURE_SEARCH_ENDPOINT/indexes/{name}?api-version={API_VERSION}\" -Headers @{{'api-key'=$env:AZURE_SEARCH_KEY}}"
                )
                sh_lines.append(f"curl -s -X DELETE \"$AZURE_SEARCH_ENDPOINT/indexes/{name}?api-version={API_VERSION}\" -H 'api-key: $AZURE_SEARCH_KEY'")
            pathlib.Path('delete_indexes.ps1').write_text('\n'.join(ps_lines)+'\n', encoding='utf-8')
            pathlib.Path('delete_indexes.sh').write_text('\n'.join(sh_lines)+'\n', encoding='utf-8')
            print('[info] Delete script templates created: delete_indexes.ps1 / delete_indexes.sh')
        else:
            print('[info] No deletion candidates; no scripts created.')

    print('\n=== Deletion Candidates ===')
    if delete_candidates:
        for d in delete_candidates: print(f'- {d}')
    else:
        print('None')
    print('\n=== Retained ===')
    for k in keep: print(f'- {k}')

if __name__ == '__main__':
    main()
