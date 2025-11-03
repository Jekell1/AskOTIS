"""Debug why LPMENU chunks aren't creating screens."""
import os, json, requests, hashlib, re
from ingestion_common import stable_hash
from pathlib import Path

settings = json.load(open('local.settings.json'))
endpoint = settings['Values']['SEARCH_ENDPOINT']
key = settings['Values']['SEARCH_KEY']

# Fetch LPMENU chunks
url = f"{endpoint}/indexes/new_code_chunks/docs/search?api-version=2024-07-01"
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'search': '"LPMENU_SCN.CPY"',
    'select': 'chunk_id,program_id,text,path,name',
    'top': 15
}

r = requests.post(url, headers=headers, json=payload)
chunks = r.json().get('value', [])

print(f"Fetched {len(chunks)} LPMENU_SCN chunks")

# Show unique paths
unique_paths = {(ch.get('program_id'), ch.get('path')) for ch in chunks}
print(f"From {len(unique_paths)} unique file paths:")
for prog_id, path in sorted(unique_paths):
    print(f"  {prog_id[:16]}... -> {path}")

print("="*80)

# Process like build_screen_nodes does
label_pattern = re.compile(
    r'LABEL\s+LINE\s+(\d+)\s+COL\s+(\d+)\s*\n?\s*["\']([^"\']+)["\']',
    re.IGNORECASE | re.MULTILINE
)

seen_hashes = set()
screens_created = 0

for ch in chunks:
    prog = (ch.get('program_id') or '').upper()
    content = ch.get('text') or ''
    
    # Check for LABEL LINE
    has_label = 'LABEL LINE' in content.upper()
    
    if not has_label:
        print(f"\n❌ Chunk skipped (no LABEL LINE): {ch.get('chunk_id')[:16]}...")
        continue
    
    # Hash the section
    h = hashlib.sha1(content.encode('utf-8','ignore')).hexdigest()
    dup_key = f"{prog}:{h}"
    
    if dup_key in seen_hashes:
        print(f"\n⏭️  Chunk skipped (duplicate): {ch.get('chunk_id')[:16]}...")
        continue
    
    seen_hashes.add(dup_key)
    
    # Extract labels
    matches = label_pattern.findall(content)
    
    print(f"\n✅ Processing chunk: {ch.get('chunk_id')[:16]}...")
    print(f"   Program ID: {prog[:16]}...")
    print(f"   Has LABEL LINE: {has_label}")
    print(f"   Labels extracted: {len(matches)}")
    
    if matches:
        print("   First 5 labels:")
        for m in matches[:5]:
            print(f"     Line {m[0]:>2} Col {m[1]:>2}: {m[2][:40]}")
        screens_created += 1
    else:
        print("   ⚠️  NO LABELS EXTRACTED (regex didn't match)")
        # Show first few lines
        print("   Content sample:")
        for line in content.split('\n')[:5]:
            if 'LABEL' in line.upper():
                print(f"     {line}")

print(f"\n{'='*80}")
print(f"Summary: Would create {screens_created} screen(s) from {len(chunks)} chunks")
