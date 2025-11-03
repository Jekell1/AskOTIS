"""Backup schemas of legacy indexes before deletion.

Creates JSON files with the full schema definition so they can be restored if needed.
"""
import os, json, requests
from datetime import datetime

API = '2025-08-01-preview'

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_settings()
ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

legacy_indexes = [
    'cobol-facts-v3',
    'cobol-facts-v3l',
    'cobol-symbols',
    'cobol-xrefs'
]

# Create backup directory
backup_dir = 'index_backups'
os.makedirs(backup_dir, exist_ok=True)

timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')

print("=" * 80)
print("  BACKING UP LEGACY INDEX SCHEMAS".center(80))
print("=" * 80)
print()

for idx_name in legacy_indexes:
    print(f"Backing up: {idx_name}...", end=' ')
    
    try:
        r = requests.get(f'{ep}/indexes/{idx_name}?api-version={API}', 
                        headers={'api-key': key}, timeout=30)
        
        if r.status_code == 200:
            schema = r.json()
            
            # Save schema
            filename = f"{backup_dir}/{idx_name}_{timestamp}.json"
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(schema, f, indent=2)
            
            print(f"✅ Saved to {filename}")
        else:
            print(f"❌ Failed: {r.status_code}")
            
    except Exception as e:
        print(f"❌ Error: {e}")

print()
print("=" * 80)
print(f"✅ Backup complete! Files saved in: {backup_dir}/")
print()
print("You can now safely run: python delete_legacy_indexes.py")
print("=" * 80)
