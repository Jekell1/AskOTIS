"""Complete name_aliases embeddings with simple batch processing.

Adapted from complete_variable_usage_embeddings.py (proven working script).
Complete all ~55,600 name alias embeddings to reach 100% coverage.

Usage:
  python complete_name_aliases_embeddings.py [--batch 128] [--limit 5000]
"""
import os, json, argparse, requests, time
from typing import List, Dict
from embedding_utils import batch_embed

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_name_aliases'
VECTOR_FIELD = 'alias_vector'
HAS_FIELD = 'has_vector'
KEY_FIELD = 'alias_id'

def build_summary(doc: Dict) -> str:
    """Build text summary from name alias metadata"""
    parts = []
    
    # Canonical name
    if doc.get('canonical_name'):
        parts.append(f"Canonical: {doc['canonical_name']}")
    
    # Alias (if different from canonical)
    alias = doc.get('alias')
    canonical = doc.get('canonical_name')
    if alias and alias != canonical:
        parts.append(f"Alias: {alias}")
    
    # Variant type
    if doc.get('variant_type'):
        parts.append(f"Type: {doc['variant_type']}")
    
    # Kind (PROGRAM, PARAGRAPH, COPYBOOK, etc.)
    if doc.get('kind'):
        parts.append(f"Kind: {doc['kind']}")
    
    # Occurrence counts
    canon_count = doc.get('canonical_occurrences', 0) or 0
    alias_count = doc.get('alias_occurrences', 0) or 0
    if canon_count or alias_count:
        parts.append(f"Occurrences: {canon_count}+{alias_count}")
    
    # Source hint
    if doc.get('source_hint'):
        parts.append(f"Source: {doc['source_hint']}")
    
    return ' | '.join(parts)[:800] if parts else f"Alias {doc.get(KEY_FIELD, 'unknown')}"

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
    """Fetch batch of name aliases without embeddings"""
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    
    # Filter: has_vector is null or false
    body = {
        'search': '*',
        'filter': f"({HAS_FIELD} eq null) or ({HAS_FIELD} eq false)",
        'select': f"{KEY_FIELD},canonical_name,alias,variant_type,kind,source_hint,canonical_occurrences,alias_occurrences",
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
    parser = argparse.ArgumentParser(description='Complete name_aliases embeddings')
    parser.add_argument('--batch', type=int, default=128, help='Batch size (default: 128)')
    parser.add_argument('--limit', type=int, help='Max records to process (optional)')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be done')
    args = parser.parse_args()
    
    print("=" * 80)
    print("COMPLETE NAME_ALIASES EMBEDDINGS")
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
            print("✅ No more name aliases to process!")
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
