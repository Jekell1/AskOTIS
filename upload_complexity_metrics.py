"""
Upload Program Complexity Metrics to Azure Search

Uploads complexity metrics with embeddings to the program_complexity index.
"""

import os
import json
from pathlib import Path
from typing import List
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from openai import AzureOpenAI
import time

# Configuration - load from local.settings.json
def load_settings():
    """Load settings from local.settings.json"""
    with open("local.settings.json", 'r') as f:
        settings = json.load(f)
    return settings['Values']

settings = load_settings()
SEARCH_ENDPOINT = settings.get("SEARCH_ENDPOINT") or settings.get("AZURE_SEARCH_ENDPOINT")
SEARCH_KEY = settings.get("SEARCH_KEY") or settings.get("AZURE_SEARCH_KEY")
OPENAI_ENDPOINT = settings.get("AZURE_OPENAI_ENDPOINT")
OPENAI_KEY = settings.get("AZURE_OPENAI_KEY") or settings.get("OPENAI_API_KEY")
EMBEDDING_DEPLOYMENT = settings.get("AZURE_OPENAI_EMBED_DEPLOYMENT", "text-embedding-3-large")

INDEX_NAME = "program_complexity"
INPUT_FILE = "program_complexity_metrics.json"


def generate_embeddings(texts: List[str], batch_size: int = 100) -> List[List[float]]:
    """Generate embeddings for a list of texts."""
    client = AzureOpenAI(
        api_key=OPENAI_KEY,
        api_version="2024-02-01",
        azure_endpoint=OPENAI_ENDPOINT
    )
    
    all_embeddings = []
    
    for i in range(0, len(texts), batch_size):
        batch = texts[i:i + batch_size]
        print(f"  Generating embeddings for batch {i//batch_size + 1} ({len(batch)} texts)...")
        
        try:
            response = client.embeddings.create(
                input=batch,
                model=EMBEDDING_DEPLOYMENT
            )
            
            batch_embeddings = [item.embedding for item in response.data]
            all_embeddings.extend(batch_embeddings)
            
            # Rate limiting
            time.sleep(0.1)
            
        except Exception as e:
            print(f"  Error generating embeddings: {e}")
            # Return zero vectors as fallback
            all_embeddings.extend([[0.0] * 3072] * len(batch))
    
    return all_embeddings


def upload_complexity_metrics():
    """Upload complexity metrics to Azure Search."""
    print(f"\nLoading complexity metrics from {INPUT_FILE}...")
    
    if not Path(INPUT_FILE).exists():
        print(f"✗ Error: {INPUT_FILE} not found!")
        print("Please run calculate_complexity_metrics.py first.")
        return 0
    
    with open(INPUT_FILE, 'r', encoding='utf-8') as f:
        metrics_list = json.load(f)
    
    print(f"Loaded complexity metrics for {len(metrics_list)} programs")
    
    # Generate embeddings for complexity descriptions
    print("\nGenerating embeddings for complexity descriptions...")
    descriptions = [m.get("complexity_description", "") for m in metrics_list]
    embeddings = generate_embeddings(descriptions)
    
    # Prepare documents for upload
    print("\nPreparing documents for upload...")
    documents = []
    
    for metrics, embedding in zip(metrics_list, embeddings):
        program_name = metrics["program"]
        
        doc = {
            "program_id": program_name,  # Use bare program name as ID
            "program": program_name,
            "total_lines": metrics["total_lines"],
            "code_lines": metrics["code_lines"],
            "comment_lines": metrics["comment_lines"],
            "blank_lines": metrics["blank_lines"],
            "cyclomatic_complexity": metrics["cyclomatic_complexity"],
            "paragraph_count": metrics["paragraph_count"],
            "call_count": metrics["call_count"],
            "perform_count": metrics["perform_count"],
            "file_io_count": metrics["file_total_io"],
            "max_nesting_depth": metrics["max_nesting_depth"],
            "complexity_category": metrics["complexity_category"],
            "complexity_description": metrics["complexity_description"],
            "complexity_description_vector": embedding,
            "has_vector": True,
            "timestamp": metrics.get("timestamp", "2024-01-15")
        }
        
        documents.append(doc)
    
    print(f"  Prepared {len(documents)} documents")
    
    # Upload to Azure Search
    print(f"\nUploading complexity metrics to {INDEX_NAME}...")
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    # Upload in batches
    batch_size = 100
    success_count = 0
    
    for i in range(0, len(documents), batch_size):
        batch = documents[i:i + batch_size]
        try:
            result = search_client.upload_documents(documents=batch)
            succeeded = sum(1 for r in result if r.succeeded)
            success_count += succeeded
            print(f"  Batch {i//batch_size + 1}: Uploaded {succeeded}/{len(batch)} documents")
        except Exception as e:
            print(f"  Error uploading batch: {e}")
    
    print(f"\n✓ Uploaded {success_count}/{len(documents)} complexity metrics")
    
    # Verify upload
    print("\nVerifying upload...")
    try:
        results = list(search_client.search("*", top=5, select=["program", "complexity_category", "cyclomatic_complexity"]))
        print(f"  Index contains {len(results)} sample documents:")
        for r in results[:5]:
            print(f"    - {r.get('program')}: {r.get('complexity_category')} (complexity: {r.get('cyclomatic_complexity')})")
    except Exception as e:
        print(f"  Error verifying: {e}")
    
    return success_count


if __name__ == "__main__":
    print("=" * 80)
    print("UPLOAD PROGRAM COMPLEXITY METRICS")
    print("=" * 80)
    
    count = upload_complexity_metrics()
    
    if count > 0:
        print("\n✓ Upload complete!")
        print(f"\nComplexity metrics are now searchable in the '{INDEX_NAME}' index")
        print(f"\nExample queries:")
        print(f"  - Find very complex programs: complexity_category eq 'very_complex'")
        print(f"  - High cyclomatic complexity: cyclomatic_complexity gt 100")
        print(f"  - Deep nesting: max_nesting_depth gt 10")
        print(f"  - Many file operations: file_io_count gt 50")
    else:
        print("\n✗ Upload failed")
