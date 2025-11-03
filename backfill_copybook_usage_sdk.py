#!/usr/bin/env python3
"""Backfill copybook_usage vectors - Azure SDK mode (bypasses skip=100K limit).

Uses azure.search.documents SearchClient iterator with continuation tokens to access
ALL documents in the index, including those beyond skip=100,000.

Key differences from REST API approach:
- Uses SearchClient.search() iterator which handles continuation tokens internally
- No skip limit - can iterate through entire index regardless of size
- Slightly different API but same embedding and upload logic
- Requires azure-search-documents package (>=11.6.0)

Environment:
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY
  AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT

Usage:
  python backfill_copybook_usage_sdk.py --batch 64
  python backfill_copybook_usage_sdk.py --batch 64 --resume-skip 100000
"""
import os, json, time, argparse
from typing import List, Dict
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from embedding_utils import batch_embed

INDEX = 'new_cobol_copybook_usage'
VEC_FIELD = 'context_vector'
DIM = 3072
TOTAL_EXPECTED = 114307

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def build_text(d):
    """Build embedding text from document."""
    return f"Program {d.get('program_id')} copies {d.get('copybook_name')} snippet: {(d.get('context_snippet') or '')[:900]}"[:1000]

def upload_vectors_sdk(client: SearchClient, docs: List[Dict]):
    """Upload vectors using SDK merge."""
    if not docs:
        return
    # Upload in batches of 128 for reliability
    for i in range(0, len(docs), 128):
        batch = docs[i:i+128]
        actions = [{'@search.action': 'merge', **d} for d in batch]
        try:
            result = client.upload_documents(documents=actions)
            failed = [r for r in result if not r.succeeded]
            if failed:
                print(f"  [WARN] {len(failed)} uploads failed in batch")
        except Exception as e:
            print(f"  [ERROR] Upload batch failed: {e}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    ap.add_argument('--resume-skip', type=int, default=0, help='Skip first N docs (resume from previous run)')
    ap.add_argument('--max-docs', type=int, default=None, help='Max docs to process (for testing)')
    args = ap.parse_args()
    
    load_settings()
    endpoint, key = resolve()
    
    print("=" * 80)
    print(f"BACKFILL: {INDEX}.{VEC_FIELD} (AZURE SDK MODE)")
    print(f"Expected total: {TOTAL_EXPECTED:,} docs")
    print(f"Resume skip: {args.resume_skip:,}")
    if args.max_docs:
        print(f"Max docs limit: {args.max_docs:,}")
    print("=" * 80)
    
    # Create SDK client
    credential = AzureKeyCredential(key)
    client = SearchClient(
        endpoint=endpoint,
        index_name=INDEX,
        credential=credential
    )
    
    # Iterate through ALL documents using SDK (no skip limit!)
    # Select only needed fields for efficiency
    select_fields = ['usage_id', 'program_id', 'copybook_name', 'context_snippet']
    
    embedded = 0
    skipped = 0
    start = time.time()
    buffer = []
    
    try:
        # SDK search iterator automatically handles continuation tokens
        search_results = client.search(
            search_text="*",
            select=select_fields,
            include_total_count=True
        )
        
        # Get total count if available
        try:
            total = search_results.get_count()
            if total:
                print(f"SDK reports {total:,} total documents\n")
        except:
            pass
        
        # Iterate through results - SDK handles pagination internally
        for doc in search_results:
            # Resume logic: skip first N docs if resuming
            if skipped < args.resume_skip:
                skipped += 1
                if skipped % 10000 == 0:
                    print(f"Skipping to resume point: {skipped:,}/{args.resume_skip:,}")
                continue
            
            # Build embedding text
            text = build_text(doc)
            if not text.strip():
                continue
            
            buffer.append({
                'id': doc['usage_id'],
                'text': text
            })
            
            # Process batch when full
            if len(buffer) >= args.batch:
                texts = [b['text'] for b in buffer]
                vectors = batch_embed(texts, dim=DIM)
                
                # Prepare upload docs
                upload_docs = [
                    {'usage_id': b['id'], VEC_FIELD: vec}
                    for b, vec in zip(buffer, vectors)
                ]
                
                upload_vectors_sdk(client, upload_docs)
                embedded += len(buffer)
                
                elapsed = time.time() - start
                rate = embedded / elapsed if elapsed > 0 else 0
                pct = (embedded / TOTAL_EXPECTED) * 100 if TOTAL_EXPECTED else 0
                eta_sec = (TOTAL_EXPECTED - embedded) / rate if rate > 0 else 0
                eta_min = int(eta_sec / 60)
                
                print(f"{embedded:,}/{TOTAL_EXPECTED:,} ({pct:.1f}%) | {rate:.1f}/s | ETA: {eta_min}min")
                
                buffer = []
                
                # Check max docs limit
                if args.max_docs and embedded >= args.max_docs:
                    print(f"\nReached max docs limit: {args.max_docs:,}")
                    break
        
        # Process final partial batch
        if buffer:
            print(f"Processing final {len(buffer)} docs...")
            texts = [b['text'] for b in buffer]
            vectors = batch_embed(texts, dim=DIM)
            upload_docs = [
                {'usage_id': b['id'], VEC_FIELD: vec}
                for b, vec in zip(buffer, vectors)
            ]
            upload_vectors_sdk(client, upload_docs)
            embedded += len(buffer)
        
    except KeyboardInterrupt:
        print(f"\n\nInterrupted at {embedded:,} docs")
        print(f"Resume with: --resume-skip {args.resume_skip + embedded}")
        return
    except Exception as e:
        print(f"\n\nERROR: {e}")
        import traceback
        traceback.print_exc()
        print(f"\nProcessed {embedded:,} docs before error")
        print(f"Resume with: --resume-skip {args.resume_skip + embedded}")
        return
    
    elapsed = time.time() - start
    print("\n" + "=" * 80)
    print(f"COMPLETED: {embedded:,}/{TOTAL_EXPECTED:,} embeddings in {elapsed/60:.1f} minutes")
    print(f"Average rate: {embedded/elapsed:.1f} docs/sec")
    print("=" * 80)

if __name__ == '__main__':
    main()
