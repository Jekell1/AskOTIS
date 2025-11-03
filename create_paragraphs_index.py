"""Create the new_cobol_paragraphs index (paragraph-level retrieval).

Fields:
  para_id (key)
  program_id (filterable, facetable, sortable, searchable)
  file_id (filterable, sortable)
  paragraph_name (searchable, filterable)
  section_name (searchable, filterable)
  kind (filterable)
  line_start (filterable, sortable)
  line_end (filterable, sortable)
  length_lines (sortable)
  source_excerpt (searchable, retrievable)
  hash (filterable)
  has_vector (filterable, sortable)
  para_vector (vector 3072-dim, non-retrievable)
  ingested_at (filterable, sortable)

Vector profile reuses default profile used by other new_ indexes.
"""
import os, json, sys, requests, hashlib

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_paragraphs'
VECTOR_DIM = 3072

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def index_exists(ep, key):
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200

def create_index(ep, key):
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type':'application/json'}
    body = {
        "name": INDEX_NAME,
        "fields": [
            {"name":"para_id","type":"Edm.String","key":True,"searchable":False,"filterable":True,"facetable":False,"sortable":False,"retrievable":True},
            {"name":"program_id","type":"Edm.String","searchable":True,"filterable":True,"facetable":True,"sortable":True,"retrievable":True},
            {"name":"file_id","type":"Edm.String","searchable":False,"filterable":True,"facetable":False,"sortable":True,"retrievable":True},
            {"name":"paragraph_name","type":"Edm.String","searchable":True,"filterable":True,"facetable":False,"sortable":False,"retrievable":True},
            {"name":"section_name","type":"Edm.String","searchable":True,"filterable":True,"facetable":False,"sortable":False,"retrievable":True},
            {"name":"kind","type":"Edm.String","searchable":False,"filterable":True,"facetable":True,"sortable":False,"retrievable":True},
            {"name":"line_start","type":"Edm.Int32","searchable":False,"filterable":True,"facetable":False,"sortable":True,"retrievable":True},
            {"name":"line_end","type":"Edm.Int32","searchable":False,"filterable":True,"facetable":False,"sortable":True,"retrievable":True},
            {"name":"length_lines","type":"Edm.Int32","searchable":False,"filterable":False,"facetable":False,"sortable":True,"retrievable":True},
            {"name":"source_excerpt","type":"Edm.String","searchable":True,"filterable":False,"facetable":False,"sortable":False,"retrievable":True},
            {"name":"hash","type":"Edm.String","searchable":False,"filterable":True,"facetable":False,"sortable":False,"retrievable":True},
            {"name":"has_vector","type":"Edm.Boolean","searchable":False,"filterable":True,"facetable":True,"sortable":True,"retrievable":True},
            {"name":"para_vector","type":"Collection(Edm.Single)","searchable":True,"dimensions":VECTOR_DIM,"vectorSearchProfile":"vector-profile"},
            {"name":"ingested_at","type":"Edm.DateTimeOffset","searchable":False,"filterable":True,"facetable":False,"sortable":True,"retrievable":True}
        ],
        "vectorSearch": {
            "algorithms": [ {"name":"hnsw-alg","kind":"hnsw"} ],
            "profiles": [ {"name":"vector-profile","algorithm":"hnsw-alg"} ]
        },
        "semantic": {
            "configurations": [
                {
                    "name":"semantic-default",
                    "prioritizedFields": {
                        "titleField": {"fieldName": "paragraph_name"},
                        "prioritizedContentFields": [
                            {"fieldName": "source_excerpt"},
                            {"fieldName": "section_name"}
                        ]
                    }
                }
            ]
        }
    }
    r = requests.put(url, headers=headers, json=body)
    if r.status_code not in (200,201):
        print('Create failed', r.status_code, r.text[:500])
        sys.exit(1)
    print(f"Created {INDEX_NAME}")

def main():
    load_local_settings()
    ep, key = resolve()
    if index_exists(ep, key):
        print(f"Index {INDEX_NAME} already exists.")
        return
    create_index(ep, key)

if __name__ == '__main__':
    main()
