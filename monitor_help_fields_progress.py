"""Monitor embedding progress for help_fields."""
import json
import requests
import time
import env_autoload

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT'].rstrip('/')
key = settings['SEARCH_KEY']
index_name = 'help_fields'

print("📊 HELP_FIELDS EMBEDDING PROGRESS MONITOR")
print("=" * 70)

search_url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Get total count
response = requests.post(search_url, headers=headers, json={"search": "*", "count": True, "top": 0})
total = response.json().get('@odata.count', 0) if response.status_code == 200 else 0

print(f"\nTotal documents: {total:,}")
print()
print("Monitoring coverage (updates every 10 seconds, press Ctrl+C to stop)...")
print()

previous_count = 0
start_time = time.time()

try:
    while True:
        # Get count with vectors
        response = requests.post(search_url, headers=headers, json={
            "search": "*",
            "filter": "has_vector eq true",
            "count": True,
            "top": 0
        })
        
        with_vectors = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
        remaining = total - with_vectors
        coverage = (with_vectors / total * 100) if total > 0 else 0
        
        # Calculate rate
        if with_vectors > previous_count:
            new_docs = with_vectors - previous_count
            elapsed = time.time() - start_time
            rate = new_docs / 10 if elapsed >= 10 else 0  # docs per second (approx)
            
            if rate > 0 and remaining > 0:
                eta_seconds = remaining / rate
                eta_minutes = eta_seconds / 60
                eta_str = f"{eta_minutes:.0f}m" if eta_minutes < 60 else f"{eta_minutes/60:.1f}h"
            else:
                eta_str = "calculating..."
        else:
            rate = 0
            eta_str = "stalled?"
        
        # Progress bar
        bar_width = 40
        filled = int(bar_width * coverage / 100)
        bar = "█" * filled + "░" * (bar_width - filled)
        
        print(f"\r[{bar}] {coverage:5.1f}% | {with_vectors:,}/{total:,} | "
              f"{remaining:,} remaining | {rate:.1f} docs/sec | ETA: {eta_str}    ", 
              end='', flush=True)
        
        previous_count = with_vectors
        
        # Check if complete
        if with_vectors >= total:
            print()
            print()
            print("🎉 100% COMPLETE!")
            break
        
        time.sleep(10)
        start_time = time.time()

except KeyboardInterrupt:
    print()
    print()
    print("⏸️  Monitoring stopped")
    print(f"\nFinal status: {with_vectors:,}/{total:,} ({coverage:.1f}%)")
