"""
Backfill vector embeddings for enhanced external reference data in new_cobol_calls index.
This adds semantic search capabilities to the comprehensive dependency data.
"""

import json
import requests
from datetime import datetime, timezone
import time
from typing import List, Dict, Optional
import logging
from embedding_utils import batch_embed, provider_info

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class EnhancedDependencyEmbedder:
    """Handles embedding generation for enhanced dependency records."""
    
    def __init__(self):
        """Initialize the embedder with configuration."""
        
        # Load configuration
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)['Values']
        
        self.search_endpoint = settings['SEARCH_ENDPOINT']
        self.search_key = settings['SEARCH_KEY']
        
        self.index_name = "new_cobol_calls"
        self.batch_size = 50  # Smaller batches for embedding API limits
        self.max_retries = 3
        self.retry_delay = 2
        
        self.headers = {
            'Content-Type': 'application/json',
            'api-key': self.search_key
        }
        
        # Stats tracking
        self.processed_count = 0
        self.embedded_count = 0
        self.error_count = 0
        self.start_time = time.time()
        
        # Log the embedding provider being used
        logger.info(f"Using embedding provider: {provider_info()}")
    
    def create_embedding_text(self, record: Dict) -> str:
        """Create comprehensive text for embedding from dependency record."""
        
        caller = record.get('caller_program', '')
        callee = record.get('callee_program', '')
        ref_type = record.get('reference_type', '')
        description = record.get('reference_description', '')
        category = record.get('category', '')
        snippet = record.get('snippet', '')
        
        # Build contextual text for embedding
        parts = []
        
        # Core dependency relationship
        parts.append(f"Program {caller} references {callee}")
        
        # Reference type and category
        if ref_type:
            parts.append(f"Reference type: {ref_type}")
        if category:
            parts.append(f"Category: {category}")
        
        # Description provides semantic context
        if description:
            parts.append(f"Description: {description}")
        
        # Code snippet provides implementation context
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
    
    def generate_embeddings(self, texts: List[str]) -> Optional[List[List[float]]]:
        """Generate embeddings for a batch of texts using the configured provider."""
        
        if not texts:
            return None
        
        try:
            # Use the existing embedding utilities with 3072 dimensions to match the index
            embeddings = batch_embed(texts, target_dim=3072)  # Match existing index dimension
            return embeddings
                
        except Exception as e:
            logger.error(f"Failed to generate embeddings: {e}")
            return None
    
    def get_unembedded_records(self, skip: int = 0, limit: int = 1000) -> List[Dict]:
        """Get enhanced records that don't have embeddings yet."""
        
        # Query for enhanced records without vectors
        search_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2024-07-01"
        
        search_body = {
            "search": "*",
            "filter": "enhanced_data eq true and has_vector eq false",
            "select": "call_id,caller_program,callee_program,reference_type,reference_description,category,snippet",
            "skip": skip,
            "top": limit
            # Remove orderby since call_id is not sortable
        }
        
        response = requests.post(search_url, headers=self.headers, json=search_body)
        
        if response.status_code == 200:
            data = response.json()
            return data.get('value', [])
        else:
            logger.error(f"Failed to fetch unembedded records: {response.status_code} - {response.text}")
            return []
    
    def update_records_with_embeddings(self, records_with_embeddings: List[tuple]) -> int:
        """Update records with their embeddings."""
        
        update_docs = []
        timestamp = datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
        
        for record, embedding in records_with_embeddings:
            update_doc = {
                "@search.action": "merge",
                "call_id": record['call_id'],
                "snippet_vector": embedding,
                "has_vector": True,
                "ingested_at": timestamp  # Use existing field instead of vector_generated_at
            }
            update_docs.append(update_doc)
        
        # Upload updates
        upload_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/index?api-version=2024-07-01"
        
        response = requests.post(upload_url, headers=self.headers, json={"value": update_docs})
        
        if response.status_code in [200, 201]:
            return len(update_docs)
        else:
            logger.error(f"Failed to update embeddings: {response.status_code} - {response.text}")
            return 0
    
    def process_batch(self, records: List[Dict]) -> int:
        """Process a batch of records to add embeddings."""
        
        if not records:
            return 0
        
        # Create embedding texts
        texts = [self.create_embedding_text(record) for record in records]
        
        # Generate embeddings
        embeddings = self.generate_embeddings(texts)
        
        if not embeddings:
            logger.error(f"Failed to generate embeddings for batch of {len(records)} records")
            self.error_count += len(records)
            return 0
        
        # Pair records with embeddings
        records_with_embeddings = list(zip(records, embeddings))
        
        # Update records in index
        updated_count = self.update_records_with_embeddings(records_with_embeddings)
        
        self.embedded_count += updated_count
        self.processed_count += len(records)
        
        return updated_count
    
    def get_embedding_stats(self) -> Dict:
        """Get current embedding statistics."""
        
        # Get total enhanced records
        total_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs?api-version=2024-07-01&$count=true&$top=0&$filter=enhanced_data eq true"
        total_response = requests.get(total_url, headers=self.headers)
        
        total_enhanced = 0
        if total_response.status_code == 200:
            total_enhanced = total_response.json().get('@odata.count', 0)
        
        # Get embedded enhanced records
        embedded_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs?api-version=2024-07-01&$count=true&$top=0&$filter=enhanced_data eq true and has_vector eq true"
        embedded_response = requests.get(embedded_url, headers=self.headers)
        
        embedded_enhanced = 0
        if embedded_response.status_code == 200:
            embedded_enhanced = embedded_response.json().get('@odata.count', 0)
        
        return {
            'total_enhanced': total_enhanced,
            'embedded_enhanced': embedded_enhanced,
            'pending_embedding': total_enhanced - embedded_enhanced,
            'processed_this_run': self.processed_count,
            'embedded_this_run': self.embedded_count,
            'errors_this_run': self.error_count,
            'runtime_minutes': (time.time() - self.start_time) / 60
        }
    
    def backfill_embeddings(self, max_records: Optional[int] = None, resume_skip: int = 0):
        """Main function to backfill embeddings for enhanced dependency records."""
        
        logger.info("🚀 STARTING ENHANCED DEPENDENCY EMBEDDING BACKFILL")
        logger.info("=" * 70)
        
        # Get initial stats
        stats = self.get_embedding_stats()
        logger.info(f"📊 Initial Status:")
        logger.info(f"   Total enhanced records: {stats['total_enhanced']:,}")
        logger.info(f"   Already embedded: {stats['embedded_enhanced']:,}")
        logger.info(f"   Pending embedding: {stats['pending_embedding']:,}")
        
        if stats['pending_embedding'] == 0:
            logger.info("✅ All enhanced records already have embeddings!")
            return
        
        # Determine processing limit
        records_to_process = min(max_records or stats['pending_embedding'], stats['pending_embedding'])
        logger.info(f"📝 Will process up to {records_to_process:,} records")
        
        if resume_skip > 0:
            logger.info(f"⏭️  Resuming from skip position: {resume_skip}")
        
        # Process in batches
        current_skip = resume_skip
        batch_number = (resume_skip // self.batch_size) + 1
        
        while self.processed_count < records_to_process:
            # Get batch of unembedded records
            remaining = min(self.batch_size, records_to_process - self.processed_count)
            
            logger.info(f"\n📦 Processing batch {batch_number} (skip: {current_skip}, size: {remaining})")
            
            records = self.get_unembedded_records(skip=current_skip, limit=remaining)
            
            if not records:
                logger.info("   No more records to process")
                break
            
            # Process batch
            embedded_in_batch = self.process_batch(records)
            
            logger.info(f"   ✅ Embedded {embedded_in_batch}/{len(records)} records")
            
            # Update progress
            current_skip += len(records)
            batch_number += 1
            
            # Show progress every 10 batches
            if batch_number % 10 == 0:
                current_stats = self.get_embedding_stats()
                completion = (current_stats['embedded_enhanced'] / current_stats['total_enhanced']) * 100
                logger.info(f"\n📈 Progress: {current_stats['embedded_enhanced']:,}/{current_stats['total_enhanced']:,} ({completion:.1f}%)")
            
            # Rate limiting
            time.sleep(0.5)  # Small delay to avoid API rate limits
        
        # Final stats
        final_stats = self.get_embedding_stats()
        logger.info(f"\n🎯 EMBEDDING BACKFILL COMPLETE")
        logger.info("=" * 50)
        logger.info(f"📊 Final Statistics:")
        logger.info(f"   Total enhanced records: {final_stats['total_enhanced']:,}")
        logger.info(f"   Successfully embedded: {final_stats['embedded_enhanced']:,}")
        logger.info(f"   Completion rate: {(final_stats['embedded_enhanced']/final_stats['total_enhanced']*100):.1f}%")
        logger.info(f"   Processing time: {final_stats['runtime_minutes']:.1f} minutes")
        logger.info(f"   Records processed this run: {final_stats['processed_this_run']:,}")
        logger.info(f"   Successfully embedded this run: {final_stats['embedded_this_run']:,}")
        logger.info(f"   Errors this run: {final_stats['errors_this_run']:,}")

def main():
    """Main function for enhanced dependency embedding."""
    
    import argparse
    
    parser = argparse.ArgumentParser(description='Backfill embeddings for enhanced dependency records')
    parser.add_argument('--max-records', type=int, help='Maximum number of records to process')
    parser.add_argument('--resume-skip', type=int, default=0, help='Skip position to resume from')
    parser.add_argument('--batch-size', type=int, default=50, help='Batch size for processing')
    
    args = parser.parse_args()
    
    try:
        embedder = EnhancedDependencyEmbedder()
        
        if args.batch_size:
            embedder.batch_size = args.batch_size
        
        embedder.backfill_embeddings(
            max_records=args.max_records,
            resume_skip=args.resume_skip
        )
        
    except Exception as e:
        logger.error(f"❌ Error during embedding backfill: {e}")
        raise

if __name__ == "__main__":
    main()