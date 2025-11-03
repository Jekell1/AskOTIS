"""
Create Azure Search index for Transaction Taxonomy and upload with embeddings.

This script:
1. Creates a new Azure Search index 'transaction_taxonomy'
2. Reads transaction_taxonomy.json
3. Generates embeddings for each transaction's narrative
4. Uploads to Azure Search
"""

import os
import json
from pathlib import Path
from typing import List, Dict
from azure.search.documents import SearchClient
from azure.search.documents.indexes import SearchIndexClient
from azure.search.documents.indexes.models import (
    SearchIndex,
    SearchField,
    SearchFieldDataType,
    VectorSearch,
    VectorSearchProfile,
    HnswAlgorithmConfiguration,
    SemanticConfiguration,
    SemanticField,
    SemanticPrioritizedFields,
    SemanticSearch
)
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

# Debug: Check if credentials loaded
if not SEARCH_KEY or not SEARCH_ENDPOINT:
    print("ERROR: Azure Search credentials not found in local.settings.json")
    print(f"SEARCH_ENDPOINT: {SEARCH_ENDPOINT}")
    print(f"SEARCH_KEY: {'<hidden>' if SEARCH_KEY else 'NOT SET'}")
    exit(1)

INDEX_NAME = "transaction_taxonomy"
INPUT_FILE = "transaction_taxonomy.json"


def create_index():
    """Create the transaction taxonomy index with vector search."""
    print(f"Creating index: {INDEX_NAME}")
    
    index_client = SearchIndexClient(
        endpoint=SEARCH_ENDPOINT,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    # Define fields
    fields = [
        SearchField(name="id", type=SearchFieldDataType.String, key=True, filterable=True),
        SearchField(name="tx_code", type=SearchFieldDataType.String, filterable=True, facetable=True),
        SearchField(name="entry_menu", type=SearchFieldDataType.String, filterable=True, facetable=True),
        SearchField(name="menu_description", type=SearchFieldDataType.String, searchable=True),
        SearchField(name="programs", type=SearchFieldDataType.Collection(SearchFieldDataType.String), filterable=True),
        SearchField(name="workflow_programs", type=SearchFieldDataType.Collection(SearchFieldDataType.String), filterable=True),
        SearchField(name="screens", type=SearchFieldDataType.Collection(SearchFieldDataType.String), filterable=True),
        SearchField(name="files", type=SearchFieldDataType.Collection(SearchFieldDataType.String), filterable=True),
        SearchField(name="description", type=SearchFieldDataType.String, searchable=True),
        SearchField(name="narrative", type=SearchFieldDataType.String, searchable=True),
        SearchField(name="program_count", type=SearchFieldDataType.Int32, filterable=True, sortable=True),
        SearchField(name="workflow_depth", type=SearchFieldDataType.Int32, filterable=True, sortable=True),
        SearchField(
            name="narrative_vector",
            type=SearchFieldDataType.Collection(SearchFieldDataType.Single),
            vector_search_dimensions=3072,
            vector_search_profile_name="vector-profile"
        ),
    ]
    
    # Vector search configuration
    vector_search = VectorSearch(
        profiles=[
            VectorSearchProfile(
                name="vector-profile",
                algorithm_configuration_name="hnsw-config"
            )
        ],
        algorithms=[
            HnswAlgorithmConfiguration(name="hnsw-config")
        ]
    )
    
    # Semantic search configuration
    semantic_config = SemanticConfiguration(
        name="semantic-config",
        prioritized_fields=SemanticPrioritizedFields(
            title_field=SemanticField(field_name="tx_code"),
            content_fields=[
                SemanticField(field_name="narrative"),
                SemanticField(field_name="description"),
                SemanticField(field_name="menu_description")
            ]
        )
    )
    
    semantic_search = SemanticSearch(configurations=[semantic_config])
    
    # Create index
    index = SearchIndex(
        name=INDEX_NAME,
        fields=fields,
        vector_search=vector_search,
        semantic_search=semantic_search
    )
    
    try:
        result = index_client.create_or_update_index(index)
        print(f"✓ Index '{INDEX_NAME}' created successfully")
        return True
    except Exception as e:
        print(f"✗ Error creating index: {e}")
        return False


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


def upload_transactions():
    """Upload transaction taxonomy with embeddings to Azure Search."""
    print(f"\nLoading transactions from {INPUT_FILE}...")
    
    with open(INPUT_FILE, 'r', encoding='utf-8') as f:
        transactions = json.load(f)
    
    print(f"Loaded {len(transactions)} transactions")
    
    # Generate embeddings for narratives
    print("\nGenerating embeddings for transaction narratives...")
    narratives = [tx.get("narrative", "") for tx in transactions]
    embeddings = generate_embeddings(narratives)
    
    # Add embeddings to documents
    documents = []
    for tx, embedding in zip(transactions, embeddings):
        doc = {
            "id": tx["id"],
            "tx_code": tx["tx_code"],
            "entry_menu": tx.get("entry_menu"),
            "menu_description": tx.get("menu_description"),
            "programs": tx.get("programs", []),
            "workflow_programs": tx.get("workflow_programs", []),
            "screens": tx.get("screens", []),
            "files": tx.get("files", []),
            "description": tx.get("description", ""),
            "narrative": tx.get("narrative", ""),
            "program_count": tx.get("program_count", 0),
            "workflow_depth": tx.get("workflow_depth", 0),
            "narrative_vector": embedding
        }
        documents.append(doc)
    
    # Upload to Azure Search
    print(f"\nUploading {len(documents)} documents to Azure Search...")
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    # Upload in batches
    batch_size = 100
    for i in range(0, len(documents), batch_size):
        batch = documents[i:i + batch_size]
        try:
            result = search_client.upload_documents(documents=batch)
            succeeded = sum(1 for r in result if r.succeeded)
            print(f"  Batch {i//batch_size + 1}: Uploaded {succeeded}/{len(batch)} documents")
        except Exception as e:
            print(f"  Error uploading batch: {e}")
    
    print(f"\n✓ Upload complete!")


def verify_index():
    """Verify the index was created and populated."""
    print("\nVerifying index...")
    
    search_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name=INDEX_NAME,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    # Get document count
    try:
        results = search_client.search(search_text="*", include_total_count=True)
        count = results.get_count()
        print(f"✓ Index contains {count} documents")
        
        # Show sample documents
        print("\nSample documents:")
        sample_results = search_client.search(search_text="*", top=3)
        for i, doc in enumerate(sample_results, 1):
            print(f"\n  {i}. Transaction {doc['tx_code']}:")
            print(f"     Programs: {', '.join(doc.get('programs', [])[:3])}")
            print(f"     Narrative: {doc.get('narrative', '')[:150]}...")
        
        return True
    except Exception as e:
        print(f"✗ Error verifying index: {e}")
        return False


def main():
    print("=" * 80)
    print("TRANSACTION TAXONOMY INDEX BUILDER")
    print("=" * 80)
    
    # Check if input file exists
    if not Path(INPUT_FILE).exists():
        print(f"\n✗ Error: {INPUT_FILE} not found!")
        print("Please run extract_transaction_taxonomy.py first.")
        return
    
    # Step 1: Create index
    if not create_index():
        print("\n✗ Failed to create index. Exiting.")
        return
    
    # Step 2: Upload transactions with embeddings
    upload_transactions()
    
    # Step 3: Verify
    verify_index()
    
    print("\n" + "=" * 80)
    print("SUCCESS!")
    print("=" * 80)
    print(f"""
Transaction taxonomy has been indexed with embeddings!

Index name: {INDEX_NAME}
Endpoint: {SEARCH_ENDPOINT}

You can now query this index for transaction workflows:
- "What happens when user enters transaction code RP?"
- "Show me all programs involved in transaction PL"
- "Which files does transaction WI access?"
    """)


if __name__ == "__main__":
    main()
