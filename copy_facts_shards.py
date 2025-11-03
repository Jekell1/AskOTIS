import json
from azure.storage.blob import BlobServiceClient
vals=json.load(open('local.settings.json'))['Values']
bsc=BlobServiceClient.from_connection_string(vals['AzureWebJobsStorage'])
cc=bsc.get_container_client('aisearch')
base='S35-Source/JSONL'
for name in ['procedure_facts_0000.jsonl','procedure_facts_0001.jsonl']:
    src=f'{base}/{name}'
    dst=f'{base}/procedure_facts/{name}'
    data=cc.get_blob_client(src).download_blob().readall()
    cc.get_blob_client(dst).upload_blob(data,overwrite=True)
    print('Copied',src,'->',dst)
print('Done')
