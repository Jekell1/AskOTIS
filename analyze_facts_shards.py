import json, os
from azure.storage.blob import BlobServiceClient

with open('local.settings.json','r',encoding='utf-8') as f:
    vals=json.load(f)['Values']
conn=vals['AzureWebJobsStorage']

bsc=BlobServiceClient.from_connection_string(conn)
cc=bsc.get_container_client('aisearch')

prefix='S35-Source/JSONL/procedure_facts'
print('Listing shards under', prefix)
for blob in cc.list_blobs(name_starts_with=prefix):
    if not blob.name.endswith('.jsonl'): continue
    bc=cc.get_blob_client(blob.name)
    sample=bc.download_blob(offset=0,length=min(blob.size,50000)).readall().decode('utf-8','replace').splitlines()
    first=[ln for ln in sample if ln.strip()][:5]
    print(f"{os.path.basename(blob.name)} size={blob.size} first_lines={len(first)}")
    for ln in first:
        try:
            obj=json.loads(ln)
            print('  keys:', sorted(obj.keys()))
            break
        except Exception as e:
            print('  parse error first line', e)
            break
