#!/usr/bin/env python3
"""diagnose_search_keys.py

Quick diagnostic for Azure AI Search keys. Determines if each key is:
  - admin (can create index)
  - query (can list indexes but cannot create)
  - invalid (cannot even list indexes)

Usage:
  python diagnose_search_keys.py --endpoint https://<service>.search.windows.net \
      --keys KEY1,KEY2 [--keep] [--api-version 2024-07-01]

Security: Only prints masked keys and SHA256 hash (first16). Does not persist created index unless --keep.
"""
import argparse, hashlib, json, os, random, string, sys, time
from typing import List
import requests

DEFAULT_API_VERSION = "2024-07-01"

def sha16(v: str) -> str:
    return hashlib.sha256(v.encode()).hexdigest()[:16]

def mask(v: str) -> str:
    if len(v) <= 8:
        return v[:2] + "*" * (len(v)-4 if len(v)>4 else 0) + v[-2:]
    return v[:4] + "..." + v[-4:]

def rand_index_name(base="diag-key-probe") -> str:
    suffix = ''.join(random.choices(string.ascii_lowercase + string.digits, k=6))
    return f"{base}-{suffix}"

def req(method: str, endpoint: str, key: str, path: str, body=None, api_version=DEFAULT_API_VERSION):
    url = f"{endpoint.rstrip('/')}{path}{'&' if '?' in path else '?'}api-version={api_version}"
    headers = {"api-key": key, "Content-Type": "application/json"}
    r = requests.request(method, url, headers=headers, json=body, timeout=60)
    return r

def classify_key(endpoint: str, key: str, api_version: str, keep: bool):
    info = {
        "hash": sha16(key),
        "masked": mask(key),
        "get_status": None,
        "post_status": None,
        "result": None,
        "index_name": None,
        "details": []
    }
    # GET indexes
    r_get = req("GET", endpoint, key, "/indexes", api_version=api_version)
    info["get_status"] = r_get.status_code
    if r_get.status_code == 200:
        # Try create index
        idx = rand_index_name()
        info["index_name"] = idx
        body = {"name": idx, "fields": [{"name": "id", "type": "Edm.String", "key": True}]}
        r_post = req("POST", endpoint, key, "/indexes", body=body, api_version=api_version)
        info["post_status"] = r_post.status_code
        if r_post.status_code in (201, 409):
            info["result"] = "admin"
            if not keep and r_post.status_code == 201:
                # clean up newly created index
                _ = req("DELETE", endpoint, key, f"/indexes('{idx}')", api_version=api_version)
        elif r_post.status_code == 403:
            info["result"] = "query"
            info["details"].append("POST 403 => lacks write perms")
        else:
            info["result"] = "unknown"
            info["details"].append(f"Unexpected POST status {r_post.status_code}: {r_post.text[:150]}")
    elif r_get.status_code == 403:
        info["result"] = "invalid_or_not_admin"
        info["details"].append("GET 403 => key not recognized as admin or query")
    else:
        info["result"] = "invalid"
        info["details"].append(f"GET status {r_get.status_code}: {r_get.text[:150]}")
    return info

def main(argv: List[str]):
    ap = argparse.ArgumentParser()
    ap.add_argument("--endpoint", required=True, help="Search service endpoint (https://<name>.search.windows.net)")
    ap.add_argument("--keys", required=True, help="Comma-separated keys to test")
    ap.add_argument("--api-version", default=DEFAULT_API_VERSION)
    ap.add_argument("--keep", action="store_true", help="Keep created probe indexes (default: delete if created)")
    ap.add_argument("--json", action="store_true", help="Output JSON summary")
    args = ap.parse_args(argv)

    endpoint = args.endpoint.strip()
    keys = [k.strip() for k in args.keys.split(',') if k.strip()]
    results = []
    for k in keys:
        try:
            info = classify_key(endpoint, k, args.api_version, args.keep)
            results.append(info)
        except Exception as e:
            results.append({
                "hash": sha16(k),
                "masked": mask(k),
                "error": str(e),
                "result": "error"
            })

    if args.json:
        print(json.dumps(results, indent=2))
    else:
        for r in results:
            print(f"Key {r['masked']} hash={r['hash']} -> {r.get('result')} (GET={r.get('get_status')} POST={r.get('post_status')})")
            if r.get('index_name'): print(f"  probe index: {r['index_name']}")
            for d in r.get('details', []): print(f"  detail: {d}")
            if r.get('error'): print(f"  error: {r['error']}")

    # Exit code: 0 if any admin, else 1
    if any(r.get('result') == 'admin' for r in results):
        return 0
    return 1

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
