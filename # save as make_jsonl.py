# save as make_jsonl.py


import json
from azure.storage.blob import BlobServiceClient

# Azure connection string from local.settings.json
CONNECTION_STRING = "DefaultEndpointsProtocol=https;AccountName=waazuse1aistorage;AccountKey=CC+qIlIhGidHQycBZ/MZ19KN2kmKSiMkLgU8kXr16//yE3aGbWK85kEfTpE7K4In7vKi0xn5MzZI+AStxHZ/sw==;EndpointSuffix=core.windows.net"
CONTAINER_NAME = "aisearch"
SOURCE_PREFIX = "S35-Source/"
OUTPUT_PREFIX = "S35-Source/S35JSON/"

blob_service_client = BlobServiceClient.from_connection_string(CONNECTION_STRING)
container_client = blob_service_client.get_container_client(CONTAINER_NAME)

i = 0
processed_files = 0
for blob in container_client.list_blobs(name_starts_with=SOURCE_PREFIX):
    if not any(blob.name.lower().endswith(ext) for ext in [".cbl", ".cob", ".cpy"]):
        continue
    print(f"Processing: {blob.name}", flush=True)
    try:
        rel = blob.name[len(SOURCE_PREFIX):].replace("/", "__")
        rows = []
        blob_client = container_client.get_blob_client(blob.name)
        stream = blob_client.download_blob()
        # download as bytes and decode with errors='ignore' to handle non-utf8 safely
        data = stream.readall()
        text = data.decode("utf-8", errors="ignore")
        for ln, line in enumerate(text.splitlines(), start=1):
            rows.append({
                "id": f"{blob.name}:{ln}",
                "repo_path": blob.name,
                "line": ln,
                "code": line.rstrip("\n")
            })
        # Prepare output blob path
        out_blob_name = OUTPUT_PREFIX + rel + ".jsonl"
        out_blob_client = container_client.get_blob_client(out_blob_name)
        # Write rows to a string and upload
        output_data = "\n".join(json.dumps(r, ensure_ascii=False) for r in rows)
        out_blob_client.upload_blob(output_data, overwrite=True)
        i += len(rows)
        processed_files += 1
        print(f"Uploaded: {out_blob_name} ({len(rows)} rows)", flush=True)
    except Exception as e:
        print(f"Error processing {blob.name}: {e}", flush=True)
        continue

print(f"wrote {i} rows from {processed_files} files")
