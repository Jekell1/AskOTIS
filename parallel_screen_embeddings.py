#!/usr/bin/env python3
"""
Parallel Screen Embeddings Generator
Launches multiple parallel processes to generate embeddings for screen nodes efficiently
"""

import os
import json
import requests
import time
import threading
import queue
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from secrets_loader import load_secrets

class ParallelScreenEmbeddings:
    def __init__(self, num_workers=5, batch_size=25):
        load_secrets()
        self.search_endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
        self.search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        self.openai_endpoint = (os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')).rstrip('/')
        self.openai_key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
        self.openai_version = os.getenv('OPENAI_API_VERSION', '2024-08-01-preview')
        self.deployment_name = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-ada-002')
        
        self.index_name = 'new_cobol_screen_nodes'
        self.num_workers = num_workers
        self.batch_size = batch_size
        
        # Thread-safe counters
        self.lock = threading.Lock()
        self.processed_count = 0
        self.error_count = 0
        self.start_time = None
        
        # Headers
        self.search_headers = {
            'api-key': self.search_key,
            'Content-Type': 'application/json'
        }
        
        self.openai_headers = {
            'api-key': self.openai_key,
            'Content-Type': 'application/json'
        }
    
    def get_total_without_embeddings(self):
        """Get total count of screens without embeddings"""
        search_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-11-01"
        search_body = {
            "search": "*",
            "filter": "has_vector eq false",
            "top": 0,
            "count": True
        }
        
        try:
            response = requests.post(search_url, headers=self.search_headers, json=search_body)
            if response.status_code == 200:
                result = response.json()
                return result.get('@odata.count', 0)
            else:
                return 0
        except:
            return 0
    
    def get_screen_batch(self, skip=0, top=None):
        """Get a batch of screens that need embeddings"""
        if top is None:
            top = self.batch_size
            
        # Azure Search has a max limit of 1000 per query, so we need to handle larger requests
        max_per_query = 1000
        actual_top = min(top, max_per_query)
            
        search_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-11-01"
        search_body = {
            "search": "*",
            "filter": "has_vector eq false",
            "select": "screen_id,summary_text,raw_span_text,screen_name,program_id",
            "top": actual_top,
            "skip": skip,
            "orderby": "screen_id"
        }
        
        try:
            response = requests.post(search_url, headers=self.search_headers, json=search_body)
            if response.status_code == 200:
                result = response.json()
                return result.get('value', [])
            else:
                return []
        except Exception as e:
            print(f"   ❌ Error getting batch: {e}")
            return []
    
    def generate_embedding(self, text):
        """Generate embedding for text with retry logic"""
        url = f"{self.openai_endpoint}/openai/deployments/{self.deployment_name}/embeddings?api-version={self.openai_version}"
        
        # Truncate text to avoid token limits
        text = text[:8000] if text else "empty screen"
        
        body = {'input': text}
        
        # Retry logic
        for attempt in range(3):
            try:
                response = requests.post(url, headers=self.openai_headers, json=body, timeout=30)
                if response.status_code == 200:
                    return response.json()['data'][0]['embedding']
                elif response.status_code == 429:  # Rate limit
                    time.sleep(2 ** attempt)  # Exponential backoff
                    continue
                else:
                    break
            except Exception as e:
                if attempt == 2:  # Last attempt
                    break
                time.sleep(1)
        
        return None
    
    def build_embedding_text(self, screen):
        """Build text for embedding from screen data"""
        parts = []
        
        # Screen identification
        if screen.get('screen_name'):
            parts.append(f"Screen: {screen['screen_name']}")
        
        if screen.get('program_id'):
            parts.append(f"Program: {screen['program_id']}")
        
        # Summary text (most important)
        if screen.get('summary_text'):
            parts.append(f"Summary: {screen['summary_text']}")
        
        # Raw content (truncated for context)
        if screen.get('raw_span_text'):
            raw_text = screen['raw_span_text'][:2000]  # Limit to avoid token overrun
            parts.append(f"Content: {raw_text}")
        
        return '\n'.join(parts)
    
    def update_screen_with_embedding(self, screen_id, embedding):
        """Update screen with generated embedding"""
        upload_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/index?api-version=2023-11-01"
        
        update_doc = {
            "screen_id": screen_id,
            "summary_vector": embedding,
            "has_vector": True
        }
        
        upload_body = {
            "value": [
                {
                    "@search.action": "merge",
                    **update_doc
                }
            ]
        }
        
        try:
            response = requests.post(upload_url, headers=self.search_headers, json=upload_body)
            return response.status_code == 200
        except:
            return False
    
    def process_screen_batch(self, worker_id, screens):
        """Process a batch of screens (worker function)"""
        worker_processed = 0
        worker_errors = 0
        
        for screen in screens:
            try:
                # Build embedding text
                embed_text = self.build_embedding_text(screen)
                
                # Generate embedding
                embedding = self.generate_embedding(embed_text)
                
                if embedding:
                    # Update screen with embedding
                    success = self.update_screen_with_embedding(screen['screen_id'], embedding)
                    
                    if success:
                        worker_processed += 1
                    else:
                        worker_errors += 1
                else:
                    worker_errors += 1
                
                # Small delay to avoid overwhelming the API
                time.sleep(0.1)
                
            except Exception as e:
                worker_errors += 1
            
            # Update global counters
            with self.lock:
                self.processed_count += 1
                if worker_errors > 0:
                    self.error_count += worker_errors
                    worker_errors = 0  # Reset after adding to global count
                
                # Progress update
                if self.processed_count % 50 == 0:
                    elapsed = time.time() - self.start_time
                    rate = self.processed_count / elapsed
                    print(f"   📊 Worker {worker_id}: {self.processed_count} processed, {rate:.1f}/sec, {self.error_count} errors")
        
        return worker_processed, worker_errors
    
    def create_work_shards(self, total_screens):
        """Create work shards for parallel processing"""
        shards = []
        screens_per_shard = max(1, total_screens // self.num_workers)
        
        for i in range(self.num_workers):
            start_index = i * screens_per_shard
            if i == self.num_workers - 1:  # Last shard gets remainder
                end_index = total_screens
            else:
                end_index = start_index + screens_per_shard
            
            shard = {
                'worker_id': i + 1,
                'start_index': start_index,
                'count': end_index - start_index,
                'skip': start_index
            }
            shards.append(shard)
        
        return shards
    
    def run_parallel_embeddings(self):
        """Main method to run parallel embedding generation"""
        print("🚀 PARALLEL SCREEN EMBEDDINGS GENERATOR")
        print("=" * 55)
        
        # Get initial status
        print("📊 Step 1: Getting current status...")
        total_without_embeddings = self.get_total_without_embeddings()
        
        if total_without_embeddings == 0:
            print("   ✅ All screens already have embeddings!")
            return
        
        print(f"   📈 Screens needing embeddings: {total_without_embeddings:,}")
        print(f"   👥 Workers: {self.num_workers}")
        print(f"   📦 Batch size per worker: {self.batch_size}")
        
        estimated_time = (total_without_embeddings / self.num_workers) * 0.5  # ~0.5 sec per embedding
        print(f"   ⏱️ Estimated time: {estimated_time/60:.1f} minutes")
        
        # Create work shards
        print(f"\n⚙️ Step 2: Creating work shards...")
        shards = self.create_work_shards(total_without_embeddings)
        
        print(f"   📊 Work distribution:")
        for shard in shards:
            print(f"      Worker {shard['worker_id']}: {shard['count']} screens (skip {shard['skip']})")
        
        # Start parallel processing
        print(f"\n🔄 Step 3: Starting parallel processing...")
        self.start_time = time.time()
        
        with ThreadPoolExecutor(max_workers=self.num_workers) as executor:
            # Submit work for each shard
            futures = []
            
            for shard in shards:
                # For large shards, we need to process in chunks due to Azure Search limits
                shard_size = shard['count']
                skip_start = shard['skip']
                
                # Process this shard in chunks of 1000 (Azure Search limit)
                chunks_processed = 0
                current_skip = skip_start
                
                while chunks_processed < shard_size:
                    chunk_size = min(1000, shard_size - chunks_processed)
                    screens = self.get_screen_batch(skip=current_skip, top=chunk_size)
                    
                    if screens:
                        # Submit batch processing job for this chunk
                        future = executor.submit(self.process_screen_batch, shard['worker_id'], screens)
                        futures.append((shard['worker_id'], future))
                        print(f"   🚀 Worker {shard['worker_id']} chunk started with {len(screens)} screens (skip {current_skip})")
                    
                    chunks_processed += chunk_size
                    current_skip += chunk_size
                    
                    # Small delay to avoid overwhelming Azure Search
                    time.sleep(0.1)
            
            # Wait for all workers to complete
            print(f"\n📊 Step 4: Monitoring progress...")
            
            for worker_id, future in futures:
                try:
                    processed, errors = future.result()
                    print(f"   ✅ Worker {worker_id} completed: {processed} processed, {errors} errors")
                except Exception as e:
                    print(f"   ❌ Worker {worker_id} failed: {e}")
        
        # Final statistics
        elapsed_time = time.time() - self.start_time
        overall_rate = self.processed_count / elapsed_time if elapsed_time > 0 else 0
        
        print(f"\n🎯 PARALLEL PROCESSING COMPLETE!")
        print(f"   📊 Total processed: {self.processed_count:,}")
        print(f"   ❌ Total errors: {self.error_count}")
        print(f"   ⏱️ Total time: {elapsed_time/60:.1f} minutes")
        print(f"   📈 Overall rate: {overall_rate:.1f} embeddings/second")
        print(f"   🎉 Success rate: {((self.processed_count-self.error_count)/self.processed_count*100):.1f}%" if self.processed_count > 0 else "N/A")
        
        # Verify final status
        print(f"\n✅ Step 5: Verifying completion...")
        final_without_embeddings = self.get_total_without_embeddings()
        print(f"   📈 Remaining without embeddings: {final_without_embeddings:,}")
        
        if final_without_embeddings == 0:
            print(f"   🎉 ALL SCREENS NOW HAVE EMBEDDINGS!")
        elif final_without_embeddings < 100:
            print(f"   💡 Only {final_without_embeddings} remaining - running cleanup...")
            # Could run a single-threaded cleanup here if needed
        
        print(f"\n🚀 PARALLEL EMBEDDING GENERATION COMPLETE!")

if __name__ == "__main__":
    # Configuration
    num_workers = 8  # Adjust based on your API rate limits
    batch_size = 50  # Screens per worker batch
    
    print(f"🎯 Starting parallel embedding generation with {num_workers} workers...")
    
    generator = ParallelScreenEmbeddings(num_workers=num_workers, batch_size=batch_size)
    generator.run_parallel_embeddings()