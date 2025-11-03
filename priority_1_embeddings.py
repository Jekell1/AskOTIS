import os
import requests
import json
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from secrets_loader import load_secrets

class Priority1EmbeddingGenerator:
    """
    Generates embeddings for Priority 1 indexes:
    1. new-cobol-files (9,956 docs) - Full program content
    2. new_code_chunks (84,205 docs) - Code snippets  
    3. new_cobol_paragraphs (224,655 docs) - Function-level code
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
        
        # Priority 1 index configurations
        self.priority_indexes = {
            'new-cobol-files': {
                'text_fields': ['content', 'summary'],
                'vector_field': 'contentVector',
                'batch_size': 10,  # Smaller batches for large content
                'description': 'Full COBOL program content'
            },
            'new_code_chunks': {
                'text_fields': ['text'],
                'vector_field': 'contentVector', 
                'batch_size': 20,
                'description': 'Code snippets and chunks'
            },
            'new_cobol_paragraphs': {
                'text_fields': ['source_excerpt'],
                'vector_field': 'contentVector',
                'batch_size': 25,
                'description': 'Function-level code paragraphs'
            }
        }
    
    def get_documents_without_embeddings(self, index_name, vector_field, batch_size=50):
        """Get a batch of documents that don't have embeddings"""
        try:
            search_url = f"{self.search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
            
            query = {
                "search": "*",
                "filter": f"{vector_field} eq null",
                "top": batch_size,
                "select": "*"
            }
            
            response = requests.post(search_url, headers=self.search_headers, json=query, timeout=30)
            
            if response.status_code == 200:
                data = response.json()
                return data.get('value', [])
            else:
                print(f"❌ Error getting batch from {index_name}: {response.status_code}")
                return []
                
        except Exception as e:
            print(f"❌ Error getting batch from {index_name}: {e}")
            return []
    
    def generate_embedding(self, text):
        """Generate embedding using Azure OpenAI"""
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
            else:
                return None
                
        except Exception as e:
            return None
    
    def combine_text_fields(self, doc, text_fields):
        """Combine multiple text fields for embedding"""
        combined_text = []
        
        for field in text_fields:
            if field in doc and doc[field]:
                value = str(doc[field])
                if value.strip():
                    combined_text.append(value)
        
        return " ".join(combined_text)
    
    def upload_embedding(self, index_name, doc_id, vector_field, embedding):
        """Upload embedding to Azure Search"""
        try:
            upload_url = f"{self.search_endpoint}/indexes/{index_name}/docs/index?api-version=2023-11-01"
            
            # Create the update document
            update_doc = {
                "@search.action": "merge",
                "id": doc_id,  # Use the document's key field
                vector_field: embedding
            }
            
            # Handle different key field names
            if index_name == 'new-cobol-files':
                update_doc = {
                    "@search.action": "merge",
                    "id": doc_id,
                    vector_field: embedding
                }
            elif index_name == 'new_code_chunks':
                update_doc = {
                    "@search.action": "merge", 
                    "chunk_id": doc_id,
                    vector_field: embedding
                }
            elif index_name == 'new_cobol_paragraphs':
                update_doc = {
                    "@search.action": "merge",
                    "para_id": doc_id, 
                    vector_field: embedding
                }
            
            upload_data = {
                "value": [update_doc]
            }
            
            response = requests.post(upload_url, headers=self.search_headers, json=upload_data, timeout=30)
            return response.status_code in [200, 201]
            
        except Exception as e:
            return False
    
    def process_document_batch(self, index_name, config, worker_id):
        """Process a batch of documents for a specific index"""
        batch_size = config['batch_size']
        text_fields = config['text_fields']
        vector_field = config['vector_field']
        
        batch_processed = 0
        batch_errors = 0
        
        while True:
            try:
                # Get documents without embeddings
                documents = self.get_documents_without_embeddings(index_name, vector_field, batch_size)
                
                if not documents:
                    print(f"✅ Worker {worker_id} ({index_name}): No more documents found")
                    break
                
                for doc in documents:
                    try:
                        # Get document ID based on index type
                        if index_name == 'new-cobol-files':
                            doc_id = doc.get('id')
                        elif index_name == 'new_code_chunks':
                            doc_id = doc.get('chunk_id')
                        elif index_name == 'new_cobol_paragraphs':
                            doc_id = doc.get('para_id')
                        
                        if not doc_id:
                            batch_errors += 1
                            continue
                        
                        # Combine text fields
                        combined_text = self.combine_text_fields(doc, text_fields)
                        
                        if not combined_text.strip():
                            batch_errors += 1
                            continue
                        
                        # Generate embedding
                        embedding = self.generate_embedding(combined_text)
                        
                        if embedding:
                            # Upload embedding
                            if self.upload_embedding(index_name, doc_id, vector_field, embedding):
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
                        
                        # Small delay to respect rate limits
                        time.sleep(0.1)
                        
                    except Exception as e:
                        batch_errors += 1
                        with self.lock:
                            self.error_count += 1
                
            except Exception as e:
                print(f"❌ Worker {worker_id} ({index_name}) batch error: {e}")
                batch_errors += 1
                time.sleep(5)  # Wait before retrying
        
        print(f"✅ Worker {worker_id} ({index_name}) completed: {batch_processed} processed, {batch_errors} errors")
        return batch_processed
    
    def get_index_status(self, index_name, vector_field):
        """Get current status of an index"""
        try:
            # Get total count
            count_url = f"{self.search_endpoint}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
            total_response = requests.get(count_url, headers=self.search_headers)
            total_count = int(total_response.text)
            
            # Count documents without embeddings
            search_url = f"{self.search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
            no_embedding_query = {
                "search": "*",
                "filter": f"{vector_field} eq null",
                "count": True,
                "top": 0
            }
            no_embedding_response = requests.post(search_url, headers=self.search_headers, json=no_embedding_query)
            no_embedding_data = no_embedding_response.json()
            no_embedding_count = no_embedding_data.get("@odata.count", 0)
            
            with_embedding_count = total_count - no_embedding_count
            completion_percentage = (with_embedding_count / total_count * 100) if total_count > 0 else 0
            
            return {
                'total': total_count,
                'with_embeddings': with_embedding_count,
                'without_embeddings': no_embedding_count,
                'completion_rate': completion_percentage
            }
        except Exception as e:
            return None
    
    def run_priority_1_embeddings(self, workers_per_index=2):
        """Run embedding generation for all Priority 1 indexes"""
        print(f"🚀 PRIORITY 1 EMBEDDING GENERATION")
        print(f"📅 {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("=" * 80)
        
        # Show initial status
        print("📊 Initial Status:")
        total_pending = 0
        for index_name, config in self.priority_indexes.items():
            status = self.get_index_status(index_name, config['vector_field'])
            if status:
                print(f"   📋 {index_name}: {status['without_embeddings']:,} pending, {status['completion_rate']:.1f}% complete")
                total_pending += status['without_embeddings']
            else:
                print(f"   📋 {index_name}: Status check failed")
        
        print(f"   🎯 Total documents to process: {total_pending:,}")
        print()
        
        if total_pending == 0:
            print("🎉 All Priority 1 indexes already have embeddings!")
            return
        
        # Calculate estimated time
        estimated_rate = 8  # embeddings per second based on previous performance
        estimated_seconds = total_pending / estimated_rate
        estimated_hours = estimated_seconds / 3600
        
        print(f"⏱️ Estimated completion time: {estimated_hours:.1f} hours")
        print(f"🔧 Starting {workers_per_index} workers per index ({workers_per_index * len(self.priority_indexes)} total workers)")
        print()
        
        # Start workers for each index
        with ThreadPoolExecutor(max_workers=workers_per_index * len(self.priority_indexes)) as executor:
            futures = []
            worker_id = 1
            
            for index_name, config in self.priority_indexes.items():
                print(f"🚀 Starting {workers_per_index} workers for {index_name} ({config['description']})")
                
                for i in range(workers_per_index):
                    future = executor.submit(self.process_document_batch, index_name, config, worker_id)
                    futures.append(future)
                    print(f"   🚀 Worker {worker_id} started")
                    worker_id += 1
            
            print()
            print("📊 Monitoring progress...")
            
            # Monitor progress
            start_time = time.time()
            last_count = 0
            
            while futures:
                time.sleep(10)  # Check every 10 seconds
                
                current_count = self.processed_count
                elapsed_time = time.time() - start_time
                rate = current_count / elapsed_time if elapsed_time > 0 else 0
                
                if current_count > last_count:
                    print(f"   📊 Progress: {current_count:,} processed, {rate:.1f}/sec, {self.error_count} errors")
                    last_count = current_count
                
                # Remove completed futures
                futures = [f for f in futures if not f.done()]
        
        # Final status
        print()
        print("🎯 FINAL STATUS:")
        for index_name, config in self.priority_indexes.items():
            status = self.get_index_status(index_name, config['vector_field'])
            if status:
                print(f"   ✅ {index_name}: {status['with_embeddings']:,}/{status['total']:,} ({status['completion_rate']:.1f}%)")
        
        total_time = time.time() - start_time
        print(f"\n⏱️ Total processing time: {total_time/3600:.1f} hours")
        print(f"📊 Final stats: {self.success_count:,} successful, {self.error_count:,} errors")

if __name__ == "__main__":
    generator = Priority1EmbeddingGenerator()
    generator.run_priority_1_embeddings(workers_per_index=2)