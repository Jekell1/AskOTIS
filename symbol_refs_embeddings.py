import os
import requests
import json
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from secrets_loader import load_secrets

class SymbolRefsEmbeddingGenerator:
    """
    Dedicated embedding generator for new_cobol_symbol_refs (1,104,574 docs)
    The largest remaining index that needs embeddings
    """
    
    def __init__(self):
        load_secrets()
        self.search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
        self.search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        self.openai_endpoint = os.getenv('AZURE_OPENAI_ENDPOINT')
        self.openai_key = os.getenv('AZURE_OPENAI_API_KEY')
        self.deployment_name = "text-embedding-ada-002"
        
        self.search_headers = {
            'api-key': self.search_key,
            'Content-Type': 'application/json'
        }
        
        self.openai_headers = {
            'api-key': self.openai_key,
            'Content-Type': 'application/json'
        }
        
        # Thread-safe counters
        self.lock = threading.Lock()
        self.processed_count = 0
        self.error_count = 0
        self.success_count = 0
        
        self.index_name = 'new_cobol_symbol_refs'
        
    def get_documents_without_embeddings(self, batch_size=40):
        """Get a batch of documents that don't have embeddings using has_vector flag"""
        try:
            search_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-11-01"
            
            # Use has_vector flag to find documents without embeddings
            query = {
                "search": "*",
                "filter": "has_vector eq false",
                "top": batch_size,
                "select": "*"
            }
            
            response = requests.post(search_url, headers=self.search_headers, json=query, timeout=30)
            
            if response.status_code == 200:
                data = response.json()
                return data.get('value', [])
            else:
                print(f"❌ Error getting batch: {response.status_code}")
                return []
                
        except Exception as e:
            print(f"❌ Error getting batch: {e}")
            return []
    
    def generate_embedding(self, text):
        """Generate embedding using Azure OpenAI"""
        max_retries = 3
        base_delay = 1
        
        for attempt in range(max_retries):
            try:
                url = f"{self.openai_endpoint}/openai/deployments/{self.deployment_name}/embeddings?api-version=2023-05-15"
                
                # Clean and truncate text for embedding
                clean_text = str(text)[:8000] if text else ""
                
                if not clean_text.strip():
                    return None
                    
                data = {
                    "input": clean_text,
                    "model": self.deployment_name
                }
                
                response = requests.post(url, headers=self.openai_headers, json=data, timeout=30)
                
                if response.status_code == 200:
                    result = response.json()
                    return result['data'][0]['embedding']
                elif response.status_code == 429:  # Rate limit
                    wait_time = base_delay * (2 ** attempt)
                    time.sleep(wait_time)
                    continue
                else:
                    return None
                    
            except Exception as e:
                if attempt < max_retries - 1:
                    wait_time = base_delay * (2 ** attempt)
                    time.sleep(wait_time)
                    continue
                return None
        
        return None
    
    def create_combined_text(self, doc):
        """Create combined text from symbol ref fields"""
        text_parts = []
        
        # Core fields for symbol references
        fields = ['excerpt_text', 'symbol_name', 'paragraph_name', 'program_id', 'file_path']
        
        for field in fields:
            value = doc.get(field)
            if value and str(value).strip() and str(value).lower() not in ['null', 'none']:
                text_parts.append(str(value))
        
        return " ".join(text_parts)
    
    def upload_embedding(self, ref_id, embedding):
        """Upload embedding to Azure Search using merge operation"""
        max_retries = 3
        
        for attempt in range(max_retries):
            try:
                upload_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/index?api-version=2023-11-01"
                
                # Create the update document - merge to add embedding field
                update_doc = {
                    "@search.action": "merge",
                    "ref_id": ref_id,
                    "contentVector": embedding,
                    "has_vector": True
                }
                
                upload_data = {
                    "value": [update_doc]
                }
                
                response = requests.post(upload_url, headers=self.search_headers, json=upload_data, timeout=30)
                
                if response.status_code in [200, 201]:
                    return True
                elif response.status_code == 429:  # Rate limit
                    time.sleep(2 ** attempt)
                    continue
                else:
                    return False
                    
            except Exception as e:
                if attempt < max_retries - 1:
                    time.sleep(2 ** attempt)
                    continue
                return False
        
        return False
    
    def process_worker_batch(self, worker_id):
        """Process documents for a specific worker"""
        batch_processed = 0
        batch_errors = 0
        consecutive_empty_batches = 0
        
        print(f"🚀 Worker {worker_id} started")
        
        while consecutive_empty_batches < 3:
            try:
                # Get documents without embeddings
                documents = self.get_documents_without_embeddings(40)
                
                if not documents:
                    consecutive_empty_batches += 1
                    if consecutive_empty_batches >= 3:
                        print(f"✅ Worker {worker_id}: No more documents found")
                        break
                    time.sleep(5)
                    continue
                else:
                    consecutive_empty_batches = 0
                
                for doc in documents:
                    try:
                        ref_id = doc.get('ref_id')
                        if not ref_id:
                            batch_errors += 1
                            continue
                        
                        # Create combined text
                        combined_text = self.create_combined_text(doc)
                        
                        if not combined_text.strip():
                            batch_errors += 1
                            continue
                        
                        # Generate embedding
                        embedding = self.generate_embedding(combined_text)
                        
                        if embedding:
                            # Upload embedding
                            if self.upload_embedding(ref_id, embedding):
                                batch_processed += 1
                                
                                # Update global counters
                                with self.lock:
                                    self.processed_count += 1
                                    self.success_count += 1
                            else:
                                batch_errors += 1
                                with self.lock:
                                    self.error_count += 1
                        else:
                            batch_errors += 1
                            with self.lock:
                                self.error_count += 1
                        
                        # Respect rate limits
                        time.sleep(0.12)  # ~8/second
                        
                    except Exception as e:
                        batch_errors += 1
                        with self.lock:
                            self.error_count += 1
                
            except Exception as e:
                print(f"❌ Worker {worker_id} batch error: {e}")
                batch_errors += 1
                time.sleep(5)
        
        print(f"✅ Worker {worker_id} completed: {batch_processed} processed, {batch_errors} errors")
        return batch_processed
    
    def get_current_status(self):
        """Get current embedding status"""
        try:
            # Get total count
            count_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/$count?api-version=2023-11-01"
            total_response = requests.get(count_url, headers=self.search_headers, timeout=30)
            total_count = int(total_response.text)
            
            # Count documents without embeddings
            search_url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-11-01"
            no_embedding_query = {
                "search": "*",
                "filter": "has_vector eq false",
                "count": True,
                "top": 0
            }
            
            response = requests.post(search_url, headers=self.search_headers, json=no_embedding_query, timeout=30)
            if response.status_code == 200:
                no_embedding_data = response.json()
                no_embedding_count = no_embedding_data.get("@odata.count", 0)
                
                with_embedding_count = total_count - no_embedding_count
                completion_percentage = (with_embedding_count / total_count * 100) if total_count > 0 else 0
                
                return {
                    'total': total_count,
                    'with_embeddings': with_embedding_count,
                    'without_embeddings': no_embedding_count,
                    'completion_rate': completion_percentage
                }
            
            return None
            
        except Exception as e:
            return None
    
    def run_symbol_refs_embeddings(self, num_workers=6):
        """Run embedding generation for symbol refs"""
        print(f"🚀 SYMBOL REFS EMBEDDING GENERATION")
        print(f"📅 {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("=" * 70)
        
        # Get initial status
        initial_status = self.get_current_status()
        if initial_status:
            print(f"📊 Initial Status:")
            print(f"   📈 Total symbol refs: {initial_status['total']:,}")
            print(f"   ✅ With embeddings: {initial_status['with_embeddings']:,}")
            print(f"   ❌ Pending: {initial_status['without_embeddings']:,}")
            print(f"   📊 Completion: {initial_status['completion_rate']:.1f}%")
            
            if initial_status['without_embeddings'] == 0:
                print("🎉 Symbol refs already have embeddings!")
                return
            
            # Calculate estimated time
            pending = initial_status['without_embeddings']
            estimated_rate = 8 * num_workers  # 8 per second per worker
            estimated_hours = pending / (estimated_rate * 3600)
            
            print(f"⏱️ Estimated time: {estimated_hours:.1f} hours")
            print(f"🔧 Starting {num_workers} workers")
        else:
            print("❌ Could not get initial status")
            return
        
        print()
        
        # Start workers
        with ThreadPoolExecutor(max_workers=num_workers) as executor:
            futures = []
            
            for worker_id in range(1, num_workers + 1):
                future = executor.submit(self.process_worker_batch, worker_id)
                futures.append(future)
            
            print("📊 Monitoring progress...")
            
            # Monitor progress
            start_time = time.time()
            last_count = 0
            
            while futures:
                time.sleep(20)  # Check every 20 seconds
                
                current_count = self.processed_count
                elapsed_time = time.time() - start_time
                rate = current_count / elapsed_time if elapsed_time > 0 else 0
                
                if current_count > last_count:
                    remaining = pending - current_count
                    remaining_hours = remaining / rate / 3600 if rate > 0 else 0
                    print(f"   📊 Progress: {current_count:,} processed, {rate:.1f}/sec, {self.error_count} errors, ~{remaining_hours:.1f}h remaining")
                    last_count = current_count
                
                # Remove completed futures
                futures = [f for f in futures if not f.done()]
        
        # Final status
        print()
        final_status = self.get_current_status()
        if final_status:
            print("🎯 FINAL STATUS:")
            print(f"   ✅ {final_status['with_embeddings']:,}/{final_status['total']:,} ({final_status['completion_rate']:.1f}%)")
        
        total_time = time.time() - start_time
        print(f"⏱️ Total time: {total_time/3600:.1f} hours")
        print(f"📊 Final stats: {self.success_count:,} successful, {self.error_count:,} errors")

if __name__ == "__main__":
    generator = SymbolRefsEmbeddingGenerator()
    generator.run_symbol_refs_embeddings(num_workers=6)