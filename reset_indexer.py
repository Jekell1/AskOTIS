import json,sys,requests
import argparse

API='2024-07-01'

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--name',required=True)
    args=ap.parse_args()
    vals=json.load(open('local.settings.json'))['Values']
    ep=vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(2)
    url=f"{ep.rstrip('/')}/indexers/{args.name}/reset?api-version={API}"
    r=requests.post(url,headers={'api-key':key})
    print('Reset',args.name,'status',r.status_code,r.text[:200])

if __name__=='__main__':
    main()
