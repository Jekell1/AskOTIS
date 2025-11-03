import os, json, requests
from secrets_loader import load_secrets

load_secrets()

ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
ep = ep.rstrip('/')

print("=" * 80)
print("NAME_ALIASES INDEX INVESTIGATION")
print("=" * 80)
print()

# Get index schema
r = requests.get(
    f'{ep}/indexes/new_cobol_name_aliases?api-version=2025-08-01-preview',
    headers={'api-key': key}
)

if r.status_code == 200:
    fields = r.json().get('fields', [])
    print("📋 All fields in new_cobol_name_aliases:")
    for f in fields:
        field_type = f.get('type')
        is_key = ' [KEY]' if f.get('key') else ''
        is_vector = ' [VECTOR]' if 'Collection(Edm.Single)' in field_type else ''
        print(f"  • {f['name']:30} {field_type:35} {is_key}{is_vector}")
    
    print()
    
    # Find key and vector fields
    key_field = [f['name'] for f in fields if f.get('key')]
    vector_fields = [f['name'] for f in fields if 'Collection(Edm.Single)' in f.get('type', '')]
    text_fields = [f['name'] for f in fields if f.get('type') == 'Edm.String' and 'text' in f['name'].lower()]
    
    print("🔑 Key field:", key_field[0] if key_field else 'NOT FOUND')
    print("📊 Vector field(s):", ', '.join(vector_fields) if vector_fields else 'NOT FOUND')
    print("📝 Text field(s):", ', '.join(text_fields) if text_fields else 'NOT FOUND')
    print()
else:
    print(f"❌ Error getting index: {r.status_code}")
    print(r.text)
    exit(1)

# Get total count
r_count = requests.get(
    f'{ep}/indexes/new_cobol_name_aliases/docs/$count?api-version=2025-08-01-preview',
    headers={'api-key': key}
)
total_docs = int(r_count.text) if r_count.status_code == 200 else 0

print(f"📊 Total documents: {total_docs:,}")
print()

# Get sample documents
r_sample = requests.post(
    f'{ep}/indexes/new_cobol_name_aliases/docs/search?api-version=2025-08-01-preview',
    headers={'api-key': key, 'Content-Type': 'application/json'},
    json={'search': '*', 'top': 3}
)

if r_sample.status_code == 200:
    samples = r_sample.json().get('value', [])
    print("📄 Sample documents:")
    for i, doc in enumerate(samples, 1):
        print(f"\n  Sample {i}:")
        for k, v in doc.items():
            if k != '@search.score' and not k.startswith('_'):
                if isinstance(v, list) and len(v) > 3:
                    print(f"    {k}: [{v[0]}, {v[1]}, ... {len(v)} items]")
                else:
                    print(f"    {k}: {v}")
print()

# Check for vector field coverage
if vector_fields:
    vector_field = vector_fields[0]
    print(f"🔍 Checking coverage for vector field: {vector_field}")
    
    # Check for has_vector field
    has_vector_field = [f for f in fields if f['name'] == 'has_vector']
    
    if has_vector_field:
        print(f"   Found has_vector field!")
        
        # Count with vectors
        r_with = requests.post(
            f'{ep}/indexes/new_cobol_name_aliases/docs/search?api-version=2025-08-01-preview',
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json={'search': '*', 'filter': 'has_vector eq true', 'top': 0, 'count': True}
        )
        
        # Count without vectors
        r_without = requests.post(
            f'{ep}/indexes/new_cobol_name_aliases/docs/search?api-version=2025-08-01-preview',
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json={'search': '*', 'filter': '(has_vector eq false) or (has_vector eq null)', 'top': 0, 'count': True}
        )
        
        if r_with.status_code == 200 and r_without.status_code == 200:
            with_vectors = r_with.json().get('@odata.count', 0)
            without_vectors = r_without.json().get('@odata.count', 0)
            coverage = (with_vectors / total_docs * 100) if total_docs > 0 else 0
            
            print(f"   ✅ With vectors: {with_vectors:,}")
            print(f"   ❌ Without vectors: {without_vectors:,}")
            print(f"   📊 Coverage: {coverage:.1f}%")
    else:
        print(f"   No has_vector field found - checking for null vectors...")
        
        # Try to count docs with non-null vectors
        # This is approximate since we can't easily filter on vector field nullness
        print(f"   Cannot easily determine coverage without has_vector field")

print()
print("=" * 80)
