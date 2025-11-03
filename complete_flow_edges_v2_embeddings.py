"""Complete flow_edges_v2 embeddings with simple batch processing.

Simple, reliable script to finish the remaining ~17.9k flow edge embeddings.
Based on proven backfill_screen_node_embeddings.py pattern.

Usage:
  python complete_flow_edges_v2_embeddings.py [--batch 128] [--limit 5000]
"""
import os, json, argparse, requests, time
from typing import List, Dict
from embedding_utils import batch_embed

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_flow_edges_v2'
VECTOR_FIELD = 'edge_vector'
HAS_FIELD = 'has_vector'
TEXT_FIELD = 'edge_text'
KEY_FIELD = 'edge_id'

def load_settings():
    """Load credentials from local.settings.json"""
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY',
                  'AZURE_OPENAI_ENDPOINT', 'AZURE_OPENAI_KEY', 'OPENAI_API_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_search():
    """Get search endpoint and key"""
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('❌ Missing SEARCH_ENDPOINT/SEARCH_KEY')
    return ep.rstrip('/'), key

def fetch_batch(ep: str, key: str, skip: int, top: int) -> List[Dict]:
    """Fetch batch of edges without embeddings"""
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    
    # Filter: has_vector is null or false
    body = {
        'search': '*',
        'filter': f"({HAS_FIELD} eq null) or ({HAS_FIELD} eq false)",
        'select': f"{KEY_FIELD},{TEXT_FIELD}",
        'top': top,
        'skip': skip
    }
    
    try:
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                         json=body, timeout=30)
        r.raise_for_status()
        return r.json().get('value', [])
    except Exception as e:
        print(f"❌ Fetch error: {e}")
        return []

def upload_batch(ep: str, key: str, docs: List[Dict]) -> bool:
    """Upload embeddings"""
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    
    actions = [{'@search.action': 'mergeOrUpload', **doc} for doc in docs]
    body = {'value': actions}
    
    try:
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'},
                         json=body, timeout=60)
        if r.status_code not in (200, 201):
            print(f"❌ Upload failed: {r.status_code} - {r.text[:300]}")
            return False
        return True
    except Exception as e:
        print(f"❌ Upload error: {e}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Complete flow_edges_v2 embeddings')
    parser.add_argument('--batch', type=int, default=128, help='Batch size (default: 128)')
    parser.add_argument('--limit', type=int, help='Max edges to process (optional)')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be done')
    args = parser.parse_args()
    
    print("=" * 80)
    print("COMPLETE FLOW_EDGES_V2 EMBEDDINGS")
    print("=" * 80)
    print()
    
    load_settings()
    ep, key = resolve_search()
    
    total_processed = 0
    total_success = 0
    skip = 0
    batch_num = 0
    
    start_time = time.time()
    
    while True:
        batch_num += 1
        
        # Fetch next batch
        batch = fetch_batch(ep, key, skip, args.batch * 2)  # Fetch 2x for buffer
        
        if not batch:
            print("✅ No more edges to process!")
            break
        
        # Limit to batch size
        batch = batch[:args.batch]
        skip += len(batch)
        
        if args.limit and total_processed >= args.limit:
            print(f"✅ Reached limit of {args.limit} edges")
            break
        
        if args.limit:
            remaining = args.limit - total_processed
            batch = batch[:remaining]
        
        print(f"\n📦 BATCH {batch_num} ({len(batch)} edges)")
        print(f"   Skip: {skip - len(batch)}, Total processed: {total_processed}")
        
        if args.dry_run:
            print("   [DRY RUN] Would embed these edges")
            total_processed += len(batch)
            continue
        
        # Extract texts
        texts = []
        for doc in batch:
            text = doc.get(TEXT_FIELD, '')
            if not text:
                # Fallback: use edge_id if no text
                text = doc.get(KEY_FIELD, 'empty edge')
            texts.append(text)
        
        # Embed
        try:
            print(f"   Embedding {len(texts)} texts...")
            vectors = batch_embed(texts, batch_size=args.batch, target_dim=3072)
            
            # Prepare upload
            upload_docs = []
            for doc, vec in zip(batch, vectors):
                upload_docs.append({
                    KEY_FIELD: doc[KEY_FIELD],
                    VECTOR_FIELD: vec,
                    HAS_FIELD: True
                })
            
            # Upload
            print(f"   Uploading {len(upload_docs)} vectors...")
            if upload_batch(ep, key, upload_docs):
                total_success += len(upload_docs)
                total_processed += len(batch)
                
                # Show progress
                elapsed = time.time() - start_time
                rate = total_processed / elapsed if elapsed > 0 else 0
                eta_seconds = (17885 - total_processed) / rate if rate > 0 else 0
                eta_minutes = eta_seconds / 60
                
                print(f"   ✅ Success! Progress: {total_processed:,} edges")
                print(f"   📊 Rate: {rate:.1f} edges/sec | ETA: {eta_minutes:.1f} minutes")
            else:
                print(f"   ❌ Upload failed for this batch")
                
        except Exception as e:
            print(f"   ❌ Batch failed: {str(e)[:150]}")
            print(f"   Continuing to next batch...")
        
        # Brief pause between batches
        time.sleep(0.3)
    
    elapsed = time.time() - start_time
    print()
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"Total processed: {total_processed:,} edges")
    print(f"Total success: {total_success:,} embeddings")
    print(f"Time elapsed: {elapsed/60:.1f} minutes")
    if total_success > 0:
        print(f"Average rate: {total_success/elapsed:.1f} edges/second")
    print()

if __name__ == '__main__':
    main()
