"""Complete variable_usage embeddings with simple batch processing.

Adapted from complete_flow_edges_v2_embeddings.py and backfill_variable_usage_vectors.py.
Complete the remaining ~45k variable usage embeddings to reach 100% coverage.

Usage:
  python complete_variable_usage_embeddings.py [--batch 128] [--limit 5000]
"""
import os, json, argparse, requests, time
from typing import List, Dict
from embedding_utils import batch_embed

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_variable_usage'
VECTOR_FIELD = 'usage_summary_vector'
HAS_FIELD = 'has_vector'
KEY_FIELD = 'symbol_id_global'

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

def build_summary(doc: Dict) -> str:
    """Build text summary from variable usage metadata"""
    parts = [f"Variable {doc.get(KEY_FIELD)}"]
    
    # Add symbol name if available
    if doc.get('symbol_name'):
        parts.append(f"Name {doc['symbol_name']}")
    
    # Add program if available
    if doc.get('program_id'):
        parts.append(f"Program {doc['program_id']}")
    
    # Add read/write counts
    read_count = doc.get('read_count', 0) or 0
    write_count = doc.get('write_count', 0) or 0
    total_refs = doc.get('total_refs', 0) or 0
    if read_count or write_count:
        parts.append(f"Reads {read_count} Writes {write_count} Total {total_refs}")
    
    # Add first write info if available
    if doc.get('first_write_location'):
        parts.append(f"FirstWrite {doc['first_write_location']}")
    if doc.get('first_write_program'):
        parts.append(f"WriteProgram {doc['first_write_program']}")
    if doc.get('first_write_paragraph'):
        parts.append(f"WriteParagraph {doc['first_write_paragraph']}")
    
    return ' | '.join(parts)[:800]

def fetch_batch(ep: str, key: str, skip: int, top: int) -> List[Dict]:
    """Fetch batch of variable usages without embeddings"""
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    
    # Filter: has_vector is null or false
    body = {
        'search': '*',
        'filter': f"({HAS_FIELD} eq null) or ({HAS_FIELD} eq false)",
        'select': f"{KEY_FIELD},symbol_name,program_id,read_count,write_count,total_refs,first_write_location,first_write_program,first_write_paragraph",
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
    parser = argparse.ArgumentParser(description='Complete variable_usage embeddings')
    parser.add_argument('--batch', type=int, default=128, help='Batch size (default: 128)')
    parser.add_argument('--limit', type=int, help='Max records to process (optional)')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be done')
    args = parser.parse_args()
    
    print("=" * 80)
    print("COMPLETE VARIABLE_USAGE EMBEDDINGS")
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
            print("✅ No more variable usages to process!")
            break
        
        # Limit to batch size
        batch = batch[:args.batch]
        skip += len(batch)
        
        if args.limit and total_processed >= args.limit:
            print(f"✅ Reached limit of {args.limit} records")
            break
        
        if args.limit:
            remaining = args.limit - total_processed
            batch = batch[:remaining]
        
        print(f"\n📦 BATCH {batch_num} ({len(batch)} records)")
        print(f"   Skip: {skip - len(batch)}, Total processed: {total_processed}")
        
        if args.dry_run:
            print("   [DRY RUN] Would embed these records")
            total_processed += len(batch)
            continue
        
        # Build text summaries from metadata
        texts = []
        for doc in batch:
            summary = build_summary(doc)
            if not summary:
                # Fallback: use symbol_id if summary fails
                summary = f"Variable {doc.get(KEY_FIELD, 'unknown')}"
            texts.append(summary)
        
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
                
                print(f"   ✅ Success! Progress: {total_processed:,} records")
                print(f"   📊 Rate: {rate:.1f} records/sec | Elapsed: {elapsed/60:.1f} min")
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
    print(f"Total processed: {total_processed:,} records")
    print(f"Total success: {total_success:,} embeddings")
    print(f"Time elapsed: {elapsed/60:.1f} minutes")
    if total_success > 0:
        print(f"Average rate: {total_success/elapsed:.1f} records/second")
    print()

if __name__ == '__main__':
    main()
