#!/usr/bin/env python3
"""
Parallel Enhanced Dependency Embeddings Backfill
Generates vector embeddings for enhanced dependency records using multiple parallel workers.
Much faster than sequential processing - can use 4-8 workers simultaneously.
"""

import asyncio
import logging
import time
import json
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import List, Optional, Dict, Any
import argparse

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

# Import existing embedding utilities
try:
    from embedding_utils import batch_embed, provider_info
except ImportError:
    print("⚠️  embedding_utils.py not found - make sure it's in the same directory")
    exit(1)

# Configuration (load from local.settings.json like sequential version)
def load_config():
    """Load configuration from local.settings.json"""
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)['Values']
        return settings['SEARCH_ENDPOINT'], settings['SEARCH_KEY']
    except Exception as e:
        print(f"Error loading configuration: {e}")
        print("Make sure local.settings.json exists with SEARCH_ENDPOINT and SEARCH_KEY")
        exit(1)

SEARCH_ENDPOINT, SEARCH_KEY = load_config()
INDEX_NAME = "new_cobol_calls"

# Parallel processing configuration
DEFAULT_NUM_WORKERS = 4
DEFAULT_BATCH_SIZE = 50
DEFAULT_MAX_RECORDS = None

@dataclass
class EmbeddingStats:
    """Track embedding statistics across workers."""
    total_records: int = 0
    embedded_records: int = 0
    failed_records: int = 0
    start_time: float = 0
    errors: List[str] = None
    
    def __post_init__(self):
        if self.errors is None:
            self.errors = []

class ParallelEnhancedDependencyEmbedder:
    def __init__(self, num_workers: int = DEFAULT_NUM_WORKERS, batch_size: int = DEFAULT_BATCH_SIZE):
        self.num_workers = num_workers
        self.batch_size = batch_size
        
        # Set up logging first - use INFO level to reduce verbosity
        logging.basicConfig(
            level=logging.INFO,  # Use INFO level to hide debug noise
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        
        # Suppress HTTP request logging
        logging.getLogger('azure.core.pipeline.policies.http_logging_policy').setLevel(logging.WARNING)
        logging.getLogger('httpx').setLevel(logging.WARNING)
        self.logger = logging.getLogger(__name__)
        
        # Initialize search client
        self.search_client = SearchClient(
            endpoint=SEARCH_ENDPOINT,
            index_name=INDEX_NAME,
            credential=AzureKeyCredential(SEARCH_KEY)
        )
        
        # Initialize embedding provider (using existing utilities)
        self.logger.info(f"Using embedding provider: {provider_info()}")
        
        # Initialize statistics
        self.stats = EmbeddingStats()

    def get_records_without_embeddings(self, skip: int = 0, top: int = 1000) -> List[Dict[str, Any]]:
        """Get enhanced dependency records that don't have embeddings yet."""
        try:
            # Search for records with enhanced_data but no has_vector or has_vector=false
            search_filter = "enhanced_data eq true and has_vector eq false"
            
            results = self.search_client.search(
                search_text="*",
                filter=search_filter,
                select=["call_id", "caller_program", "callee_program", "reference_type", "reference_description", 
                       "category", "snippet", "enhanced_data"],
                skip=skip,
                top=top,
                include_total_count=True
            )
            
            records = []
            for result in results:
                records.append(dict(result))
            
            return records, getattr(results, 'get_count', lambda: len(records))()
            
        except Exception as e:
            self.logger.error(f"Error fetching records: {str(e)}")
            return [], 0

    def create_embedding_text(self, record: Dict[str, Any]) -> str:
        """Create comprehensive text for embedding generation."""
        parts = []
        
        # Caller and callee context
        if record.get('caller_program'):
            parts.append(f"Caller: {record['caller_program']}")
        
        if record.get('callee_program'):
            parts.append(f"Callee: {record['callee_program']}")
        
        # Reference type and category
        ref_type = record.get('reference_type', '')
        if ref_type:
            parts.append(f"Type: {ref_type}")
        
        category = record.get('category', '')
        if category:
            parts.append(f"Category: {category}")
        
        # Description provides semantic context
        description = record.get('reference_description', '')
        if description:
            parts.append(f"Description: {description}")
        
        # Code snippet provides implementation context
        snippet = record.get('snippet', '')
        if snippet:
            # Clean up snippet
            clean_snippet = snippet.replace('\n', ' ').replace('\r', ' ').strip()
            if clean_snippet:
                parts.append(f"Code: {clean_snippet}")
        
        # Additional context based on type
        if 'COPY' in ref_type:
            parts.append(f"Copybook inclusion relationship")
        elif 'CALL' in ref_type:
            parts.append(f"Program invocation relationship")
        elif 'EXEC' in ref_type:
            parts.append(f"System integration relationship")
        
        return " | ".join(parts)

    def generate_embedding_for_record(self, record: Dict[str, Any]) -> Optional[List[float]]:
        """Generate embedding for a single record."""
        try:
            text = self.create_embedding_text(record)
            if not text.strip():
                record_id = record.get('call_id', record.get('id', 'unknown'))
                self.logger.warning(f"Empty text for record {record_id}")
                return None
            
            # Debug: log before embedding (only if debug enabled)
            record_id = record.get('call_id', record.get('id', 'unknown'))
            # Remove verbose debug logging
            
            # Use existing batch_embed utility with correct dimensions (3072 for text-embedding-3-large)
            embeddings = batch_embed([text], target_dim=3072)
            
            # Debug: log after embedding (only if debug enabled)
            # Remove verbose debug logging
            
            return embeddings[0] if embeddings else None
            
        except Exception as e:
            record_id = record.get('call_id', record.get('id', 'unknown'))
            error_msg = f"Error generating embedding for record {record_id}: {str(e)}"
            # More detailed error logging to track exact exception
            import traceback
            self.logger.error(f"Full exception details for {record_id}: {type(e).__name__}: {str(e)}")
            self.logger.error(f"Traceback: {traceback.format_exc()}")
            return None

    def process_batch_worker(self, worker_id: int, records: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Process a batch of records in a worker thread."""
        worker_stats = {
            'worker_id': worker_id,
            'processed': 0,
            'embedded': 0,
            'failed': 0,
            'errors': []
        }
        
        # Don't log individual worker starts to reduce verbosity
        
        # Generate embeddings for all records in batch
        updates = []
        for record in records:
            try:
                embedding = self.generate_embedding_for_record(record)
                if embedding:
                    # Get record ID - handle both call_id and id fields
                    record_id = record.get('call_id') or record.get('id')
                    if not record_id:
                        self.logger.error(f"No ID found in record: {list(record.keys())}")
                        worker_stats['failed'] += 1
                        continue
                    
                    # Prepare update document (matching sequential version format)
                    update_doc = {
                        'call_id': record_id,  # Use whatever ID field we found
                        'has_vector': True,
                        'snippet_vector': embedding,  # Use snippet_vector field like sequential version
                        'ingested_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())  # Fixed: removed %f which is invalid for time.strftime
                    }
                    updates.append(update_doc)
                    worker_stats['embedded'] += 1
                else:
                    worker_stats['failed'] += 1
                    worker_stats['errors'].append(f"Failed to generate embedding for {record.get('call_id', record.get('id', 'unknown'))}")
                
                worker_stats['processed'] += 1
                
            except Exception as e:
                worker_stats['failed'] += 1
                record_id = record.get('call_id', record.get('id', 'unknown'))
                error_msg = f"Error processing {record_id}: {str(e)}"
                worker_stats['errors'].append(error_msg)
                # Use %s to avoid format string issues with record IDs containing {}
                self.logger.error("❌ Worker %s: %s", worker_id, error_msg)
        
        # Batch upload to search index
        if updates:
            try:
                # Use SearchClient upload_documents method with proper format
                upload_docs = []
                for update_doc in updates:
                    upload_doc = {
                        "@search.action": "merge",
                        **update_doc  # Include all fields from update_doc
                    }
                    upload_docs.append(upload_doc)
                
                result = self.search_client.upload_documents(upload_docs)
                upload_success = sum(1 for r in result if r.succeeded)
                upload_failed = len(upload_docs) - upload_success
                
                if upload_failed > 0:
                    worker_stats['failed'] += upload_failed
                    worker_stats['errors'].append(f"Failed to upload {upload_failed} documents")
                
                # Don't log individual uploads to reduce verbosity
                
            except Exception as e:
                worker_stats['failed'] += len(updates)
                worker_stats['errors'].append(f"Batch upload failed: {str(e)}")
                self.logger.error(f"❌ Worker {worker_id}: Upload error: {str(e)}")
        
        return worker_stats

    def run_parallel_embedding(self, max_records: Optional[int] = None, resume_from: int = 0):
        """Run parallel embedding generation across multiple workers."""
        self.stats.start_time = time.time()
        
        self.logger.info("🚀 STARTING PARALLEL ENHANCED DEPENDENCY EMBEDDING BACKFILL")
        self.logger.info("=" * 70)
        self.logger.info(f"⚙️  Configuration:")
        self.logger.info(f"   Workers: {self.num_workers}")
        self.logger.info(f"   Batch size: {self.batch_size}")
        self.logger.info(f"   Resume from: {resume_from}")
        
        # Get initial count
        _, total_count = self.get_records_without_embeddings(skip=0, top=1)
        self.stats.total_records = total_count
        self.logger.info(f"📊 Total records needing embeddings: {total_count}")
        
        if max_records:
            self.logger.info(f"📝 Will process up to {max_records} records")
        
        # Process in parallel chunks
        processed_total = 0
        skip_offset = resume_from
        
        with ThreadPoolExecutor(max_workers=self.num_workers) as executor:
            while True:
                # Check if we've hit the max_records limit
                if max_records and processed_total >= max_records:
                    self.logger.info(f"🏁 Reached max_records limit: {max_records}")
                    break
                
                # Calculate how many records to fetch for this round
                records_to_fetch = self.num_workers * self.batch_size
                if max_records:
                    remaining = max_records - processed_total
                    records_to_fetch = min(records_to_fetch, remaining)
                
                # Fetch records for this parallel round
                records, _ = self.get_records_without_embeddings(skip=skip_offset, top=records_to_fetch)
                
                if not records:
                    self.logger.info("🏁 No more records to process")
                    break
                
                # Split records into chunks for workers
                chunks = [records[i:i + self.batch_size] for i in range(0, len(records), self.batch_size)]
                
                if not chunks:
                    break
                
                self.logger.info(f"🔄 Processing {len(records)} records with {len(chunks)} workers (skip: {skip_offset})")
                
                # Submit all chunks to workers
                future_to_worker = {
                    executor.submit(self.process_batch_worker, i, chunk): i 
                    for i, chunk in enumerate(chunks)
                }
                
                # Collect results as they complete
                round_embedded = 0
                round_failed = 0
                
                for future in as_completed(future_to_worker):
                    worker_stats = future.result()
                    round_embedded += worker_stats['embedded']
                    round_failed += worker_stats['failed']
                    
                    # Log critical errors only (not every minor issue)
                    for error in worker_stats['errors']:
                        self.stats.errors.append(error)
                        if 'Failed to upload' in error or 'Batch upload failed' in error:
                            self.logger.error(f"Critical error: {error}")
                
                # Update overall stats
                self.stats.embedded_records += round_embedded
                self.stats.failed_records += round_failed
                processed_total += len(records)
                skip_offset += len(records)
                
                # Progress report
                elapsed = time.time() - self.stats.start_time
                rate = self.stats.embedded_records / elapsed if elapsed > 0 else 0
                percent = (processed_total / total_count * 100) if total_count > 0 else 0
                
                self.logger.info(f"📈 Progress: {processed_total}/{total_count} ({percent:.1f}%)")
                self.logger.info(f"   Embedded: {self.stats.embedded_records}, Failed: {self.stats.failed_records}")
                self.logger.info(f"   Rate: {rate:.1f} records/sec, Elapsed: {elapsed/60:.1f} min")
                
                # Brief pause between rounds to avoid overwhelming the API
                time.sleep(1)

        # Final statistics
        elapsed = time.time() - self.stats.start_time
        rate = self.stats.embedded_records / elapsed if elapsed > 0 else 0
        
        self.logger.info("🎉 PARALLEL EMBEDDING COMPLETE!")
        self.logger.info("=" * 50)
        self.logger.info(f"📊 Final Statistics:")
        self.logger.info(f"   Total processed: {processed_total}")
        self.logger.info(f"   Successfully embedded: {self.stats.embedded_records}")
        self.logger.info(f"   Failed: {self.stats.failed_records}")
        self.logger.info(f"   Total time: {elapsed/60:.1f} minutes")
        self.logger.info(f"   Average rate: {rate:.1f} records/second")
        
        if self.stats.errors:
            self.logger.warning(f"⚠️  {len(self.stats.errors)} errors occurred (check logs for details)")

def main():
    parser = argparse.ArgumentParser(description="Parallel Enhanced Dependency Embeddings Backfill")
    parser.add_argument("--workers", type=int, default=DEFAULT_NUM_WORKERS, 
                       help=f"Number of parallel workers (default: {DEFAULT_NUM_WORKERS})")
    parser.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE,
                       help=f"Batch size per worker (default: {DEFAULT_BATCH_SIZE})")
    parser.add_argument("--max-records", type=int, default=DEFAULT_MAX_RECORDS,
                       help="Maximum number of records to process (default: all)")
    parser.add_argument("--resume-from", type=int, default=0,
                       help="Resume from this record offset (default: 0)")
    
    args = parser.parse_args()
    
    # Create and run embedder
    embedder = ParallelEnhancedDependencyEmbedder(
        num_workers=args.workers,
        batch_size=args.batch_size
    )
    
    embedder.run_parallel_embedding(
        max_records=args.max_records,
        resume_from=args.resume_from
    )

if __name__ == "__main__":
    main()