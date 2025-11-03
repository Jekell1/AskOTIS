"""
Update Program Meta Index with Complexity Metrics

This script:
1. Reads program_complexity_metrics.json
2. Updates existing program_meta documents with complexity fields
3. Generates embeddings for complexity descriptions
4. Uploads to Azure Search new_cobol_programs index
"""

import os
import json
from pathlib import Path
from typing import List, Dict
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from openai import AzureOpenAI
from dotenv import load_dotenv
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

INDEX_NAME = "new_cobol_program_meta"  # Corrected index name
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


def fetch_existing_program_docs(program_names: List[str]) -> Dict[str, Dict]:
    """Fetch existing program documents from Azure Search."""
    print(f"\nFetching existing program documents from {INDEX_NAME}...")
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    existing_docs = {}
    
    # Fetch in batches to avoid timeout
    batch_size = 100
    for i in range(0, len(program_names), batch_size):
        batch_names = program_names[i:i + batch_size]
        
        # Build filter for this batch
        filter_parts = [f"program_name eq '{name}'" for name in batch_names]
        filter_str = " or ".join(filter_parts)
        
        try:
            results = search_client.search(
                search_text="*",
                filter=filter_str,
                select=["program_id", "program_name", "program_summary", "file_paths_json"]
            )
            
            for doc in results:
                existing_docs[doc.get("program_name", "")] = doc
                
        except Exception as e:
            print(f"  Error fetching batch {i//batch_size + 1}: {e}")
    
    print(f"  Found {len(existing_docs)} existing program documents")
    return existing_docs


def update_programs_with_complexity():
    """Update program documents with complexity metrics."""
    print(f"\nLoading complexity metrics from {INPUT_FILE}...")
    
    if not Path(INPUT_FILE).exists():
        print(f"✗ Error: {INPUT_FILE} not found!")
        print("Please run calculate_complexity_metrics.py first.")
        return
    
    with open(INPUT_FILE, 'r', encoding='utf-8') as f:
        metrics_list = json.load(f)
    
    print(f"Loaded complexity metrics for {len(metrics_list)} programs")
    
    # Fetch existing program documents
    program_names = [m["program"] for m in metrics_list]
    existing_docs = fetch_existing_program_docs(program_names)
    
    # Build enriched summaries (only for programs that exist in index)
    print("\nBuilding enriched summaries...")
    enriched_texts = []
    valid_metrics = []
    
    for metrics in metrics_list:
        program_name = metrics["program"]
        
        # Check if program exists in index
        if program_name not in existing_docs:
            print(f"  Warning: {program_name} not found in index, skipping...")
            continue
        
        existing_doc = existing_docs[program_name]
        existing_summary = existing_doc.get("program_summary", "")
        complexity_info = f"\n\nComplexity Analysis:\n{metrics['complexity_description']}"
        enriched_summary = existing_summary + complexity_info if existing_summary else metrics['complexity_description']
        
        enriched_texts.append(enriched_summary)
        valid_metrics.append(metrics)
    
    print(f"  Built {len(enriched_texts)} enriched summaries")
    
    # Generate embeddings for enriched summaries
    print("\nGenerating embeddings for enriched summaries...")
    embeddings = generate_embeddings(enriched_texts)
    
    # Prepare update documents
    print("\nPreparing update documents...")
    update_docs = []
    
    for metrics, enriched_summary, embedding in zip(valid_metrics, enriched_texts, embeddings):
        program_name = metrics["program"]
        existing_doc = existing_docs[program_name]
        
        # Use the enriched summary we already built
        update_doc = {
            "program_id": existing_doc["program_id"],
            "program_name": program_name,
            "file_paths_json": existing_doc.get("file_paths_json"),
            "program_summary": enriched_summary,
            "program_summary_vector": embedding,
            "has_vector": True,
            "updated_at": metrics.get("timestamp", "2024-01-15")
        }
        
        update_docs.append(update_doc)
    
    print(f"Prepared {len(update_docs)} documents for update")
    
    # Upload to Azure Search (merge mode)
    print(f"\nUploading complexity metrics to {INDEX_NAME}...")
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    # Upload in batches
    batch_size = 50
    success_count = 0
    
    for i in range(0, len(update_docs), batch_size):
        batch = update_docs[i:i + batch_size]
        try:
            result = search_client.merge_or_upload_documents(documents=batch)
            succeeded = sum(1 for r in result if r.succeeded)
            success_count += succeeded
            print(f"  Batch {i//batch_size + 1}: Updated {succeeded}/{len(batch)} documents")
        except Exception as e:
            print(f"  Error updating batch: {e}")
    
    print(f"\n✓ Updated {success_count}/{len(update_docs)} program documents with complexity metrics")
    
    return success_count


def verify_updates():
    """Verify the complexity fields were added."""
    print("\nVerifying complexity fields...")
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    try:
        # Search for programs with high complexity
        results = search_client.search(
            search_text="*",
            filter="complexity_category eq 'very_complex'",
            select=["program", "code_lines", "cyclomatic_complexity", "complexity_category", "complexity_description"],
            top=5
        )
        
        print("\nSample of very complex programs:")
        for i, doc in enumerate(results, 1):
            print(f"\n  {i}. {doc['program']}:")
            print(f"     Lines: {doc.get('code_lines', 'N/A')}")
            print(f"     Complexity: {doc.get('cyclomatic_complexity', 'N/A')}")
            print(f"     Category: {doc.get('complexity_category', 'N/A')}")
            print(f"     Description: {doc.get('complexity_description', 'N/A')[:150]}...")
        
        return True
    except Exception as e:
        print(f"✗ Error verifying updates: {e}")
        return False


def generate_statistics():
    """Generate statistics about updated programs."""
    print("\n" + "=" * 80)
    print("COMPLEXITY STATISTICS IN INDEX")
    print("=" * 80)
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    categories = ["simple", "moderate", "medium", "complex", "very_complex"]
    
    print("\nPrograms by complexity category:")
    for category in categories:
        try:
            results = search_client.search(
                search_text="*",
                filter=f"complexity_category eq '{category}'",
                include_total_count=True,
                top=0
            )
            count = results.get_count()
            print(f"  {category:15s}: {count:4d} programs")
        except Exception as e:
            print(f"  {category:15s}: Error - {e}")


def main():
    print("=" * 80)
    print("UPDATE PROGRAM META WITH COMPLEXITY METRICS")
    print("=" * 80)
    
    # Update programs
    success_count = update_programs_with_complexity()
    
    if success_count > 0:
        # Verify updates
        verify_updates()
        
        # Generate statistics
        generate_statistics()
        
        print("\n" + "=" * 80)
        print("SUCCESS!")
        print("=" * 80)
        print(f"""
Updated {success_count} programs with complexity metrics!

Index: {INDEX_NAME}

New searchable fields:
- code_lines, cyclomatic_complexity, paragraph_count
- call_count, perform_count, file_io_count
- max_nesting_depth, complexity_category
- complexity_description (with embeddings)

Example queries:
- "Which programs are most complex?"
- "Show me simple programs that process payments"
- "Find programs with high nesting depth"
- "What are performance risks in SPMEMO?"
        """)
    else:
        print("\n✗ No programs were updated. Check for errors above.")


if __name__ == "__main__":
    main()
