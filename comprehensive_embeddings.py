import os
import requests
import json
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from secrets_loader import load_secrets

class ComprehensiveEmbeddingGenerator:
    """
    Generates embeddings for all remaining indexes to achieve 100% coverage
    Primary target: new_cobol_symbol_refs (1,104,574 docs) - The largest remaining index
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
        
        # All remaining index configurations
        self.remaining_indexes = {
            'new_cobol_symbol_refs': {
                'text_fields': ['excerpt_text', 'symbol_name', 'paragraph_name', 'program_id'],
                'vector_field': 'contentVector',
                'batch_size': 40,
                'key_field': 'ref_id',
                'description': 'Symbol references and cross-references',
                'priority': 1  # Highest priority - largest dataset
            },
            'new_cobol_flow_edges_v2': {
                'text_fields': ['edge_text', 'caller_para', 'target_para', 'program_id'],
                'vector_field': 'contentVector',
                'batch_size': 35,
                'key_field': 'edge_id', 
                'description': 'Program flow and control edges',
                'priority': 2
            },
            'new_cobol_copybook_usage': {
                'text_fields': ['context_snippet', 'copybook_name', 'program_id', 'raw_copy_line'],
                'vector_field': 'contentVector',
                'batch_size': 35,
                'key_field': 'usage_id',
                'description': 'Copybook usage patterns',
                'priority': 3
            },
            'new_cobol_program_copybook_edges': {
                'text_fields': ['copybook_name_plain', 'program_id'],
                'vector_field': 'contentVector',
                'batch_size': 40,
                'key_field': 'edge_id',
                'description': 'Program-copybook relationships',
                'priority': 4
            },
            'new_cobol_variable_usage': {
                'text_fields': ['symbol_id_global'],
                'vector_field': 'contentVector',
                'batch_size': 40,
                'key_field': 'symbol_id_global',
                'description': 'Variable usage statistics',
                'priority': 5
            },
            'cobol-xrefs': {
                'text_fields': ['snippet', 'qualified_name', 'simple_name', 'path'],
                'vector_field': 'contentVector',
                'batch_size': 35,
                'key_field': 'xref_id',
                'description': 'Legacy cross-references',
                'priority': 6
            },
            'new_cobol_name_aliases': {
                'text_fields': ['alias', 'canonical_name', 'kind'],
                'vector_field': 'contentVector',
                'batch_size': 40,
                'key_field': 'alias_id',
                'description': 'Name aliases and variations',
                'priority': 7
            },
            'cobol-symbols': {
                'text_fields': ['name', 'qualified_name', 'section', 'pic', 'value'],
                'vector_field': 'contentVector',
                'batch_size': 35,
                'key_field': 'item_id',
                'description': 'Legacy symbol definitions',
                'priority': 8
            }
        }
    
    def get_documents_without_embeddings(self, index_name, vector_field, batch_size=50):
        """Get a batch of documents that don't have embeddings"""
        try:
            search_url = f"{self.search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
            
            # Use different filter approaches for robustness
            filters = [
                f"not search.ismatch('*', '{vector_field}')",
                f"{vector_field} eq null",
                f"not search.ismatch('', '{vector_field}')"
            ]
            
            for filter_expr in filters:
                query = {
                    "search": "*",
                    "filter": filter_expr,
                    "top": batch_size,
                    "select": "*"
                }
                
                response = requests.post(search_url, headers=self.search_headers, json=query, timeout=30)
                
                if response.status_code == 200:
                    data = response.json()
                    documents = data.get('value', [])
                    if documents:
                        return documents
                
            return []
            
        except Exception as e:
            print(f"❌ Error getting batch from {index_name}: {e}")
            return []
    
    def generate_embedding(self, text):
        """Generate embedding using Azure OpenAI with enhanced error handling"""
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
    
    def combine_text_fields(self, doc, text_fields):
        """Combine multiple text fields for embedding"""
        combined_text = []
        
        for field in text_fields:
            if field in doc and doc[field]:
                value = str(doc[field])
                if value.strip() and value.lower() not in ['null', 'none', '']:
                    combined_text.append(value)
        
        return " ".join(combined_text)
    
    def upload_embedding(self, index_name, doc_id, key_field, vector_field, embedding):
        """Upload embedding to Azure Search with enhanced error handling"""
        max_retries = 3
        
        for attempt in range(max_retries):
            try:
                upload_url = f"{self.search_endpoint}/indexes/{index_name}/docs/index?api-version=2023-11-01"
                
                # Create the update document
                update_doc = {
                    "@search.action": "merge",
                    key_field: doc_id,
                    vector_field: embedding
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
    
    def process_document_batch(self, index_name, config, worker_id):
        """Process a batch of documents for a specific index"""
        batch_size = config['batch_size']
        text_fields = config['text_fields']
        vector_field = config['vector_field']
        key_field = config['key_field']
        
        batch_processed = 0
        batch_errors = 0
        consecutive_empty_batches = 0
        
        while consecutive_empty_batches < 3:  # Stop after 3 consecutive empty batches
            try:
                # Get documents without embeddings
                documents = self.get_documents_without_embeddings(index_name, vector_field, batch_size)
                
                if not documents:
                    consecutive_empty_batches += 1
                    if consecutive_empty_batches >= 3:
                        print(f"✅ Worker {worker_id} ({index_name}): No more documents found")
                        break
                    time.sleep(5)  # Wait before retry
                    continue
                else:
                    consecutive_empty_batches = 0  # Reset counter
                
                for doc in documents:
                    try:
                        # Get document ID
                        doc_id = doc.get(key_field)
                        
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
                            if self.upload_embedding(index_name, doc_id, key_field, vector_field, embedding):
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
                        
                        # Dynamic delay based on error rate
                        error_rate = batch_errors / max(batch_processed + batch_errors, 1)
                        if error_rate > 0.1:  # If error rate > 10%
                            time.sleep(0.2)
                        else:
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
            total_response = requests.get(count_url, headers=self.search_headers, timeout=30)
            total_count = int(total_response.text)
            
            # Count documents without embeddings using the most reliable filter
            search_url = f"{self.search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
            
            # Try multiple filter approaches
            filters = [
                f"not search.ismatch('*', '{vector_field}')",
                f"{vector_field} eq null"
            ]
            
            for filter_expr in filters:
                no_embedding_query = {
                    "search": "*",
                    "filter": filter_expr,
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
    
    def run_comprehensive_embeddings(self, workers_per_index=4):
        """Run embedding generation for all remaining indexes"""
        print(f"🚀 COMPREHENSIVE EMBEDDING GENERATION")
        print(f"📅 {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("=" * 80)
        
        # Show initial status
        print("📊 Initial Status:")
        total_pending = 0
        active_indexes = {}
        
        # Sort indexes by priority
        sorted_indexes = sorted(self.remaining_indexes.items(), 
                               key=lambda x: x[1]['priority'])
        
        for index_name, config in sorted_indexes:
            status = self.get_index_status(index_name, config['vector_field'])
            if status:
                pending = status['without_embeddings']
                print(f"   📋 {index_name}: {pending:,} pending, {status['completion_rate']:.1f}% complete")
                total_pending += pending
                
                if pending > 0:
                    active_indexes[index_name] = config
            else:
                print(f"   📋 {index_name}: Status check failed")
        
        print(f"   🎯 Total documents to process: {total_pending:,}")
        print(f"   📋 Active indexes: {len(active_indexes)}")
        print()
        
        if total_pending == 0:
            print("🎉 All indexes already have embeddings!")
            return
        
        # Calculate estimated time
        estimated_rate = 8  # embeddings per second based on previous performance
        estimated_seconds = total_pending / estimated_rate
        estimated_hours = estimated_seconds / 3600
        
        print(f"⏱️ Estimated completion time: {estimated_hours:.1f} hours")
        print(f"🔧 Starting {workers_per_index} workers per active index")
        print()
        
        # Start workers for each active index
        total_workers = workers_per_index * len(active_indexes)
        
        with ThreadPoolExecutor(max_workers=total_workers) as executor:
            futures = []
            worker_id = 1
            
            for index_name, config in active_indexes.items():
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
                time.sleep(20)  # Check every 20 seconds for large datasets
                
                current_count = self.processed_count
                elapsed_time = time.time() - start_time
                rate = current_count / elapsed_time if elapsed_time > 0 else 0
                
                if current_count > last_count:
                    remaining_estimate = (total_pending - current_count) / rate if rate > 0 else 0
                    remaining_hours = remaining_estimate / 3600
                    print(f"   📊 Progress: {current_count:,} processed, {rate:.1f}/sec, {self.error_count} errors, ~{remaining_hours:.1f}h remaining")
                    last_count = current_count
                
                # Remove completed futures
                futures = [f for f in futures if not f.done()]
        
        # Final status
        print()
        print("🎯 FINAL STATUS:")
        total_final_embedded = 0
        total_final_docs = 0
        
        for index_name, config in sorted_indexes:
            status = self.get_index_status(index_name, config['vector_field'])
            if status:
                print(f"   ✅ {index_name}: {status['with_embeddings']:,}/{status['total']:,} ({status['completion_rate']:.1f}%)")
                total_final_embedded += status['with_embeddings']
                total_final_docs += status['total']
        
        total_time = time.time() - start_time
        print(f"\n⏱️ Total processing time: {total_time/3600:.1f} hours")
        print(f"📊 Final stats: {self.success_count:,} successful, {self.error_count:,} errors")
        print(f"🎯 Overall completion: {total_final_embedded:,}/{total_final_docs:,} ({(total_final_embedded/total_final_docs*100):.1f}%)")

if __name__ == "__main__":
    generator = ComprehensiveEmbeddingGenerator()
    generator.run_comprehensive_embeddings(workers_per_index=4)