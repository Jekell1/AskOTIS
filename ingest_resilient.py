"""Resilient ingestion with automatic resume and retry logic."""
import subprocess
import time
import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

def get_current_count():
    """Get current document count from index."""
    cfg = json.load(open('local.settings.json'))['Values']
    client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_code_chunks', AzureKeyCredential(cfg['SEARCH_KEY']))
    return client.get_document_count()

def run_ingestion_batch(skip_count, batch_size=5000, max_retries=3):
    """Run ingestion for a batch with retries."""
    limit = batch_size
    cmd = [
        'python', 'ingest_code_chunks.py',
        '--include-copybooks',
        '--stream-batch-size', '100',
        '--embed-batch-size', '10',
        '--skip', str(skip_count),
        '--limit', str(limit)
    ]
    
    for attempt in range(max_retries):
        print(f"\n{'='*80}")
        print(f"BATCH: Skip={skip_count}, Limit={limit}, Attempt={attempt+1}/{max_retries}")
        print(f"{'='*80}\n")
        
        try:
            result = subprocess.run(cmd, timeout=1800)  # 30 min timeout per batch
            if result.returncode == 0:
                print(f"\n✅ Batch completed successfully")
                return True
            else:
                print(f"\n⚠️  Batch exited with code {result.returncode}")
                if attempt < max_retries - 1:
                    print(f"Retrying in 10 seconds...")
                    time.sleep(10)
        except subprocess.TimeoutExpired:
            print(f"\n⚠️  Batch timed out after 30 minutes")
            if attempt < max_retries - 1:
                print(f"Retrying in 10 seconds...")
                time.sleep(10)
        except KeyboardInterrupt:
            print(f"\n❌ User interrupted")
            raise
    
    print(f"\n❌ Batch failed after {max_retries} attempts")
    return False

def main():
    """Main resilient ingestion loop."""
    total_expected = 180301
    batch_size = 5000
    
    print(f"🚀 Starting resilient ingestion")
    print(f"Total expected: {total_expected} documents")
    print(f"Batch size: {batch_size} documents")
    print(f"{'='*80}\n")
    
    # Get initial count from index
    current_count = get_current_count()
    print(f"📊 Current index count: {current_count:,}")
    
    while current_count < total_expected:
        remaining = total_expected - current_count
        progress_pct = (current_count / total_expected) * 100
        
        print(f"\n📊 PROGRESS: {current_count:,}/{total_expected:,} ({progress_pct:.1f}%) | Remaining: {remaining:,}")
        
        # Run next batch
        batch_to_process = min(batch_size, remaining)
        success = run_ingestion_batch(current_count, batch_to_process)
        
        if success:
            # Update our count manually (don't wait for index to refresh)
            current_count += batch_to_process
            print(f"✅ Updated position: {current_count:,}")
            time.sleep(2)  # Brief pause between batches
        else:
            print(f"\n❌ Batch failed. Pausing for 30 seconds before retry...")
            time.sleep(30)
    
    print(f"\n🎉 COMPLETE! All {total_expected:,} documents processed")
    print(f"Verifying final count...")
    final_count = get_current_count()
    print(f"Final index count: {final_count:,}")

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print(f"\n\n⏸️  Ingestion paused. Run script again to resume from current position.")
    except Exception as e:
        print(f"\n\n❌ Error: {e}")
