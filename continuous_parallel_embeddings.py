#!/usr/bin/env python3
"""
Continuous Parallel Screen Embeddings
Simplified approach that continuously processes screens until complete
"""

import os
import json
import requests
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from secrets_loader import load_secrets

class ContinuousParallelEmbeddings:
    def __init__(self, num_workers=8):
        load_secrets()
        self.search_endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
        self.search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        self.openai_endpoint = (os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')).rstrip('/')
        self.openai_key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
        self.openai_version = os.getenv('OPENAI_API_VERSION', '2024-08-01-preview')
        self.deployment_name = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-ada-002')
        
        self.index_name = 'new_cobol_screen_nodes'
        self.num_workers = num_workers
        
        # Thread-safe counters
        self.lock = threading.Lock()
        self.processed_count = 0
        self.error_count = 0
        self.start_time = None
        self.should_stop = False
        
        # Headers
        self.search_headers = {
            'api-key': self.search_key,
            'Content-Type': 'application/json'
        }
        
        self.openai_headers = {
            'api-key': self.openai_key,
            'Content-Type': 'application/json'
        }
    
    def get_next_batch(self, batch_size=100):
        """Get next batch of screens needing embeddings"""
        search_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-11-01"
        search_body = {
            "search": "*",
            "filter": "has_vector eq false",
            "select": "screen_id,summary_text,raw_span_text,screen_name,program_id",
            "top": batch_size,
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
    
    def worker_process(self, worker_id):
        """Continuous worker process"""
        worker_processed = 0
        worker_errors = 0
        
        print(f"   🚀 Worker {worker_id} started")
        
        while not self.should_stop:
            try:
                # Get a small batch for this worker
                screens = self.get_next_batch(batch_size=25)
                
                if not screens:
                    # No more screens to process
                    print(f"   ✅ Worker {worker_id}: No more screens found")
                    break
                
                # Process each screen in the batch
                for screen in screens:
                    if self.should_stop:
                        break
                    
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
                        if self.processed_count % 100 == 0:
                            elapsed = time.time() - self.start_time
                            rate = self.processed_count / elapsed if elapsed > 0 else 0
                            print(f"   📊 Progress: {self.processed_count:,} processed, {rate:.1f}/sec, {self.error_count} errors")
                
            except Exception as e:
                print(f"   ❌ Worker {worker_id} error: {e}")
                time.sleep(1)
        
        print(f"   ✅ Worker {worker_id} completed: {worker_processed} processed")
        return worker_processed
    
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
    
    def run_continuous_processing(self):
        """Main method to run continuous parallel processing"""
        print("🔄 CONTINUOUS PARALLEL EMBEDDINGS GENERATOR")
        print("=" * 60)
        
        # Get initial status
        print("📊 Initial status check...")
        initial_total = self.get_total_without_embeddings()
        
        if initial_total == 0:
            print("   ✅ All screens already have embeddings!")
            return
        
        print(f"   📈 Screens needing embeddings: {initial_total:,}")
        print(f"   👥 Workers: {self.num_workers}")
        
        estimated_time = initial_total / (self.num_workers * 10)  # ~10 per worker per minute
        print(f"   ⏱️ Estimated time: {estimated_time:.1f} minutes")
        
        # Start continuous processing
        print(f"\n🚀 Starting continuous parallel processing...")
        self.start_time = time.time()
        
        with ThreadPoolExecutor(max_workers=self.num_workers) as executor:
            # Submit worker processes
            futures = []
            for i in range(self.num_workers):
                future = executor.submit(self.worker_process, i + 1)
                futures.append(future)
            
            # Wait for all workers to complete
            print(f"\n📊 Monitoring workers...")
            
            for future in as_completed(futures):
                try:
                    processed = future.result()
                except Exception as e:
                    print(f"   ❌ Worker failed: {e}")
        
        # Final statistics
        elapsed_time = time.time() - self.start_time
        overall_rate = self.processed_count / elapsed_time if elapsed_time > 0 else 0
        
        print(f"\n🎯 CONTINUOUS PROCESSING COMPLETE!")
        print(f"   📊 Total processed: {self.processed_count:,}")
        print(f"   ❌ Total errors: {self.error_count}")
        print(f"   ⏱️ Total time: {elapsed_time/60:.1f} minutes")
        print(f"   📈 Overall rate: {overall_rate:.1f} embeddings/second")
        
        success_rate = ((self.processed_count-self.error_count)/self.processed_count*100) if self.processed_count > 0 else 0
        print(f"   🎉 Success rate: {success_rate:.1f}%")
        
        # Verify final status
        print(f"\n✅ Final verification...")
        final_remaining = self.get_total_without_embeddings()
        print(f"   📈 Remaining without embeddings: {final_remaining:,}")
        
        if final_remaining == 0:
            print(f"   🎉 ALL SCREENS NOW HAVE EMBEDDINGS!")
        else:
            print(f"   💡 {final_remaining:,} screens still need processing")
        
        print(f"\n🚀 EMBEDDING GENERATION COMPLETE!")

if __name__ == "__main__":
    print(f"🎯 Starting continuous parallel embedding generation...")
    
    generator = ContinuousParallelEmbeddings(num_workers=6)  # Slightly fewer workers for stability
    generator.run_continuous_processing()