"""
Monitor the copybook embedding process in real-time.
Run this periodically to see progress updates.
"""

import os
import json
import requests
import time
from datetime import datetime

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
KEY = os.getenv('AZURE_SEARCH_KEY')
API_VERSION = '2024-07-01'
INDEX_NAME = 'code-chunks'

HEADERS = {
    'Content-Type': 'application/json',
    'api-key': KEY
}

def get_detailed_status():
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}'
    
    # Total chunks
    body = {'search': '*', 'top': 0, 'count': True}
    r = requests.post(url, headers=HEADERS, json=body)
    total = r.json().get('@odata.count', 0)
    
    # Chunks with embeddings
    body = {'search': '*', 'filter': 'has_vector eq true', 'top': 0, 'count': True}
    r = requests.post(url, headers=HEADERS, json=body)
    embedded = r.json().get('@odata.count', 0)
    
    # .CBL chunks (for reference)
    body = {'search': '*', 'searchFields': 'path', 'search': '.CBL', 'top': 0, 'count': True}
    r = requests.post(url, headers=HEADERS, json=body)
    cbl_approx = r.json().get('@odata.count', 0)
    
    missing = total - embedded
    pct = (embedded / total * 100) if total > 0 else 0
    
    # Copybook estimate
    cpy_chunks = total - 102654  # Known .CBL chunks
    cpy_embedded = embedded - 102654 if embedded > 102654 else 0
    cpy_pct = (cpy_embedded / cpy_chunks * 100) if cpy_chunks > 0 else 0
    
    print('=' * 80)
    print('COPYBOOK EMBEDDING MONITOR')
    print(f'Time: {datetime.now().strftime("%H:%M:%S")}')
    print('=' * 80)
    print()
    print('OVERALL STATUS:')
    print(f'  Total chunks:         {total:,}')
    print(f'  With embeddings:      {embedded:,} ({pct:.1f}%)')
    print(f'  Missing embeddings:   {missing:,}')
    print()
    print('BREAKDOWN:')
    print(f'  .CBL chunks:          ~102,654 (programs)')
    print(f'  .CPY chunks:          ~{cpy_chunks:,} (copybooks)')
    print()
    print('COPYBOOK PROGRESS:')
    print(f'  Embedded:             {cpy_embedded:,} / {cpy_chunks:,} ({cpy_pct:.1f}%)')
    print(f'  Remaining:            {cpy_chunks - cpy_embedded:,}')
    print()
    
    if missing > 0:
        # Progress bar
        bar_width = 50
        filled = int(bar_width * pct / 100)
        bar = '█' * filled + '░' * (bar_width - filled)
        print(f'PROGRESS: [{bar}] {pct:.1f}%')
        print()
        
        rate = 25  # Conservative estimate chunks/second
        eta_sec = missing / rate
        eta_min = eta_sec / 60
        print(f'ESTIMATED TIME REMAINING: {eta_min:.1f} minutes')
        print(f'  (@ ~{rate} chunks/sec average rate)')
    else:
        print('✅ ✅ ✅ ALL CHUNKS HAVE EMBEDDINGS! ✅ ✅ ✅')
        print()
        print('COPYBOOK INTEGRATION COMPLETE!')
        print(f'  Total files: 9,951 (1,740 .CBL + 8,211 .CPY)')
        print(f'  Total chunks: {total:,}')
        print(f'  All embedded: 100%')
    
    print('=' * 80)

if __name__ == '__main__':
    get_detailed_status()
