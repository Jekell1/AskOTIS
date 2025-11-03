"""
Download COBOL source corpus from Azure Blob Storage to a local folder using
configuration in local.settings.json (preferred) or environment variables.

Security:
- Does NOT embed or print secrets.
- Reads connection string only via existing local.settings.json values or env var.
- Supports future switch to Managed Identity by honoring AZURE_STORAGE_CONNECTION_STRING absence
  (user could adapt to DefaultAzureCredential + blob URL if needed).

Usage:
  python download_source_corpus.py --out cobol_src --prefix S35-Source/ --max-files 0 \
      --include-ext .cbl .CBL .cpy .CPY .cob .COB

Key behaviors:
- Skips JSONL/ or other non-source subfolders by default unless --include-json is provided.
- Will not overwrite identical files (hash compare) unless --force.
- Provides a manifest JSON summarizing counts and skipped files.
- Retries transient blob download errors with exponential backoff.

Future Enhancements (not implemented yet):
- Optional parallel downloads (ThreadPool) for large corpora
- Optional checksum verification against stored metadata
- Optional integration with paragraph recovery pipeline directly
"""
from __future__ import annotations
import os, json, argparse, sys, time, hashlib
from dataclasses import dataclass
from typing import Iterable, List, Optional, Dict

try:
    from azure.storage.blob import BlobServiceClient # type: ignore
except ImportError:
    BlobServiceClient = None  # runtime guard below

COBOL_DEFAULT_EXTS = [".cbl", ".cpy", ".cob"]

@dataclass
class DownloadResult:
    downloaded: int
    skipped_existing: int
    skipped_non_source: int
    errors: int
    files: List[str]

# --------------------------------- Helpers ----------------------------------

def read_local_settings(path: str = "local.settings.json") -> Dict[str,str]:
    if not os.path.isfile(path):
        return {}
    try:
        data = json.load(open(path, "r", encoding="utf-8"))
        return data.get("Values", {}) or {}
    except Exception:
        return {}


def resolve_connection_string(values: Dict[str,str]) -> Optional[str]:
    # Order of preference: DEPLOYMENT_STORAGE_CONNECTION_STRING, AzureWebJobsStorage, explicit AZURE_STORAGE_CONNECTION_STRING env
    for key in ["DEPLOYMENT_STORAGE_CONNECTION_STRING", "AzureWebJobsStorage"]:
        if values.get(key):
            return values[key]
    env = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
    if env:
        return env
    return None


def get_blob_service(connection_string: Optional[str], account_name: Optional[str]):  # -> BlobServiceClient
    if BlobServiceClient is None:
        raise RuntimeError("azure-storage-blob not installed. pip install azure-storage-blob")
    if connection_string:
        return BlobServiceClient.from_connection_string(connection_string)
    if account_name:  # Placeholder for future MSI-based auth
        raise RuntimeError("Managed Identity flow not implemented. Provide connection string locally.")
    raise RuntimeError("No connection string available. Add to local.settings.json or set AZURE_STORAGE_CONNECTION_STRING.")


def should_include(name: str, include_exts: List[str], include_json: bool) -> bool:
    lower = name.lower()
    # Exclude JSONL and typical processed folders unless explicitly desired
    if not include_json and ("/jsonl/" in lower or lower.endswith(".jsonl")):
        return False
    ext = os.path.splitext(lower)[1]
    return ext in [e.lower() for e in include_exts]


def hash_bytes(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


def safe_write(path: str, data: bytes, force: bool) -> bool:
    if os.path.isfile(path) and not force:
        # Compare hash to avoid redundant writes
        try:
            existing = open(path, "rb").read()
            if hash_bytes(existing) == hash_bytes(data):
                return False  # unchanged
        except Exception:
            pass
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as f:
        f.write(data)
    return True


def iter_source_blobs(container, prefix: str) -> Iterable:
    for blob in container.list_blobs(name_starts_with=prefix):
        yield blob


def download_with_retry(container, blob_name: str, max_attempts: int = 4, base_delay: float = 0.8) -> Optional[bytes]:
    attempt = 1
    while True:
        try:
            bc = container.get_blob_client(blob_name)
            return bc.download_blob().readall()
        except Exception as e:
            if attempt >= max_attempts:
                print(f"ERROR: Failed {blob_name}: {e}")
                return None
            sleep = base_delay * (2 ** (attempt - 1))
            print(f"WARN: Attempt {attempt} failed for {blob_name}: {e} -> retrying in {sleep:.1f}s")
            time.sleep(sleep)
            attempt += 1


def perform_download(container, prefix: str, out_dir: str, include_exts: List[str], include_json: bool, max_files: int, force: bool) -> DownloadResult:
    downloaded = 0
    skipped_existing = 0
    skipped_non_source = 0
    errors = 0
    written_files: List[str] = []

    for blob in iter_source_blobs(container, prefix):
        name = blob.name
        if not should_include(name, include_exts, include_json):
            skipped_non_source += 1
            continue
        rel_path = name[len(prefix):] if name.startswith(prefix) else name
        local_path = os.path.join(out_dir, rel_path)
        if os.path.isfile(local_path) and not force:
            skipped_existing += 1
            continue
        data = download_with_retry(container, name)
        if data is None:
            errors += 1
            continue
        changed = safe_write(local_path, data, force)
        if changed:
            downloaded += 1
            written_files.append(local_path)
        else:
            skipped_existing += 1
        if max_files and downloaded >= max_files:
            break
    return DownloadResult(downloaded, skipped_existing, skipped_non_source, errors, written_files)


def write_manifest(out_dir: str, prefix: str, result: DownloadResult, include_exts: List[str]):
    manifest = {
        "prefix": prefix,
        "downloaded": result.downloaded,
        "skipped_existing": result.skipped_existing,
        "skipped_non_source": result.skipped_non_source,
        "errors": result.errors,
        "files": result.files,
        "include_exts": include_exts,
    }
    path = os.path.join(out_dir, "_download_manifest.json")
    with open(path, "w", encoding="utf-8") as f:
        json.dump(manifest, f, indent=2)
    print(f"Manifest written: {path}")

# ---------------------------------- Main ------------------------------------

def parse_args(argv: List[str]) -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Download COBOL source corpus locally")
    p.add_argument("--out", default="cobol_src", help="Local output directory root")
    p.add_argument("--prefix", default=None, help="Blob prefix (overrides SOURCE_CODE_PREFIX)")
    p.add_argument("--include-ext", nargs="*", default=None, help="File extensions to include (default COBOL set)")
    p.add_argument("--include-json", action="store_true", help="Also include JSON/JSONL blobs")
    p.add_argument("--max-files", type=int, default=0, help="Stop after downloading this many new files (0 = no limit)")
    p.add_argument("--force", action="store_true", help="Overwrite even if content unchanged")
    return p.parse_args(argv)


def main(argv: List[str]) -> int:
    args = parse_args(argv)
    values = read_local_settings()

    prefix = args.prefix or values.get("SOURCE_CODE_PREFIX") or "S35-Source/"
    container_name = values.get("SOURCE_BLOB_CONTAINER", "aisearch")
    account = values.get("SOURCE_STORAGE_ACCOUNT")  # not yet used for MSI
    conn_str = resolve_connection_string(values)

    include_exts = args.include_ext or COBOL_DEFAULT_EXTS

    print(f"Using container={container_name} prefix={prefix} include_exts={include_exts}")

    try:
        bsc = get_blob_service(conn_str, account)
    except Exception as e:
        print(f"ERROR: {e}")
        return 2

    container = bsc.get_container_client(container_name)

    result = perform_download(
        container=container,
        prefix=prefix,
        out_dir=args.out,
        include_exts=include_exts,
        include_json=args.include_json,
        max_files=args.max_files,
        force=args.force,
    )

    print(f"Download complete: {result.downloaded} new, {result.skipped_existing} skipped existing, {result.skipped_non_source} non-source ignored, {result.errors} errors")
    write_manifest(args.out, prefix, result, include_exts)
    return 0 if result.errors == 0 else 3

if __name__ == "__main__":  # pragma: no cover
    sys.exit(main(sys.argv[1:]))
