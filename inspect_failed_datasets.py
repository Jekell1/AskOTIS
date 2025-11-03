#!/usr/bin/env python3
"""inspect_failed_datasets.py

Diagnose failing Azure AI Search indexers by inspecting JSONL blobs for required key fields
Targets (hard-coded for now): paragraphs, procedure_facts, flow_edges, copybooks

Usage:
  python inspect_failed_datasets.py --container aisearch --prefix S35-Source/JSONL
"""
import argparse, json, os, sys, io
from typing import Dict, List, Tuple
from azure.storage.blob import BlobServiceClient

REQUIRED_FIELDS = {
    "paragraphs": ["para_id","file_id","name","kind","start_line","end_line"],
    "procedure_facts": ["fact_id","file_id","para","line","kind","snippet","callee","callee_data_name","is_dynamic","using_raw","target","source_raw","expr_raw"],
    "flow_edges": ["edge_id","file_id","caller_para","target_para","line","kind"],
    "copybooks": ["copybook_id","file_id","parent_path","copybook_name","line","replacing_clause"],
}

DATASET_FOLDERS = ["paragraphs","procedure_facts","flow_edges","copybooks"]

def load_conn():
    with open("local.settings.json","r",encoding="utf-8") as f:
        raw = json.load(f)
    vals = raw.get("Values", {})
    conn = vals.get("AzureWebJobsStorage")
    if not conn:
        raise SystemExit("AzureWebJobsStorage missing")
    return conn

def sample_blob_lines(blob_client, max_lines=10) -> List[str]:
    data = blob_client.download_blob(max_concurrency=1).readall()
    text = data.decode("utf-8","replace")
    lines = text.splitlines()[:max_lines]
    return lines

def analyze_lines(lines: List[str], required: List[str]) -> Dict[str, int]:
    present_counts = {f:0 for f in required}
    parsed = 0
    for ln in lines:
        ln = ln.strip()
        if not ln: continue
        try:
            obj = json.loads(ln)
            parsed += 1
            for f in required:
                if f in obj and obj[f] not in (None,""):
                    present_counts[f] += 1
        except Exception:
            continue
    return {"parsed_lines": parsed, "field_presence": present_counts}

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--container", required=True)
    ap.add_argument("--prefix", required=True, help="Base prefix ending in /JSONL (or /JSONL/..)")
    ap.add_argument("--max-lines", type=int, default=25)
    args = ap.parse_args()

    conn = load_conn()
    bsc = BlobServiceClient.from_connection_string(conn)
    cc = bsc.get_container_client(args.container)

    base = args.prefix.rstrip('/')
    print(f"Analyzing datasets under: {base}")

    summary = []
    for folder in DATASET_FOLDERS:
        folder_prefix = f"{base}/{folder}/"
        blobs = list(cc.list_blobs(name_starts_with=folder_prefix))
        print(f"\nDataset: {folder}  (blob prefix: {folder_prefix})  count={len(blobs)}")
        if not blobs:
            summary.append({"dataset": folder, "blobs":0, "issue":"no blobs"})
            continue
        # find first *.jsonl blob
        target = None
        for b in blobs:
            if b.name.lower().endswith('.jsonl'):
                target = b
                break
        if not target:
            print("  No .jsonl blob found")
            summary.append({"dataset": folder, "blobs":len(blobs), "issue":"no jsonl"})
            continue
        print(f"  Sample blob: {target.name}")
        lines = sample_blob_lines(cc.get_blob_client(target), max_lines=args.max_lines)
        for i,l in enumerate(lines):
            print(f"    L{i+1}: {l[:180]}")
        required = REQUIRED_FIELDS.get(folder, [])
        if required:
            analysis = analyze_lines(lines, required)
            missing = [f for f,c in analysis['field_presence'].items() if c==0]
            print(f"  Parsed lines: {analysis['parsed_lines']}  Missing fields (never seen): {missing if missing else 'None'}")
            summary.append({"dataset": folder, "blobs":len(blobs), "parsed_sample":analysis['parsed_lines'], "missing_fields": missing})
        else:
            summary.append({"dataset": folder, "blobs":len(blobs), "note":"no required list"})

    print("\n=== Summary ===")
    for s in summary:
        print(json.dumps(s))

if __name__ == "__main__":
    main()
