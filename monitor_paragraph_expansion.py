"""Monitor paragraph expansion progress in real-time."""
import os, json, requests, time

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
api = '2025-08-01-preview'

print("=" * 70)
print("PARAGRAPH EXPANSION - LIVE PROGRESS MONITOR")
print("=" * 70)
print("\nTarget: Expand from 41.1% (3,974 programs) to ~100% (9,678 programs)")
print("Goal: Add ~5,704 programs worth of paragraphs\n")

# Initial count
try:
    r = requests.get(f'{ep}/indexes/new_cobol_paragraphs/docs/$count?api-version={api}', 
                    headers={'api-key': key}, timeout=30)
    start_count = int(r.text) if r.status_code == 200 else 224655
except:
    start_count = 224655

print(f"Starting document count: {start_count:,}")
print(f"Monitoring... (Ctrl+C to stop)\n")
print(f"{'Time':<10} {'Total Docs':>12} {'New Docs':>12} {'Rate/min':>10}")
print("-" * 70)

last_count = start_count
last_time = time.time()

try:
    while True:
        time.sleep(30)  # Check every 30 seconds
        
        try:
            r = requests.get(f'{ep}/indexes/new_cobol_paragraphs/docs/$count?api-version={api}', 
                            headers={'api-key': key}, timeout=30)
            if r.status_code == 200:
                current_count = int(r.text)
                new_docs = current_count - start_count
                docs_since_last = current_count - last_count
                current_time = time.time()
                elapsed_mins = (current_time - last_time) / 60
                rate = docs_since_last / elapsed_mins if elapsed_mins > 0 else 0
                
                timestamp = time.strftime("%H:%M:%S")
                print(f"{timestamp:<10} {current_count:>12,} {new_docs:>12,} {rate:>9.0f}/m")
                
                last_count = current_count
                last_time = current_time
                
                # Check if we've reached target
                if current_count >= 540000:  # Rough estimate for ~100% coverage
                    print(f"\n🎉 Target reached! Final count: {current_count:,}")
                    break
        except Exception as e:
            print(f"Error: {e}")
            
except KeyboardInterrupt:
    print(f"\n\nMonitoring stopped.")
    print(f"Final count: {last_count:,}")
    print(f"New documents added: {last_count - start_count:,}")
