"""
Create Program Complexity Index

Creates a dedicated Azure Search index for program complexity metrics
with vector search capabilities.
"""

import json
from azure.search.documents.indexes import SearchIndexClient
from azure.search.documents.indexes.models import (
    SearchIndex,
    SimpleField,
    SearchableField,
    SearchField,
    SearchFieldDataType,
    VectorSearch,
    HnswAlgorithmConfiguration,
    VectorSearchProfile,
    SemanticConfiguration,
    SemanticPrioritizedFields,
    SemanticField,
    SemanticSearch
)
from azure.core.credentials import AzureKeyCredential

# Configuration - load from local.settings.json
def load_settings():
    """Load settings from local.settings.json"""
    with open("local.settings.json", 'r') as f:
        settings = json.load(f)
    return settings['Values']

settings = load_settings()
SEARCH_ENDPOINT = settings.get("SEARCH_ENDPOINT") or settings.get("AZURE_SEARCH_ENDPOINT")
SEARCH_KEY = settings.get("SEARCH_KEY") or settings.get("AZURE_SEARCH_KEY")

INDEX_NAME = "program_complexity"


def create_complexity_index():
    """Create the program complexity index with vector search."""
    
    print(f"\nCreating index: {INDEX_NAME}")
    
    # Create the search index client
    index_client = SearchIndexClient(
        endpoint=SEARCH_ENDPOINT,
        credential=AzureKeyCredential(SEARCH_KEY)
    )
    
    # Define the index schema
    fields = [
        SimpleField(name="program_id", type=SearchFieldDataType.String, key=True, filterable=True, sortable=True),
        SearchableField(name="program", type=SearchFieldDataType.String, filterable=True, sortable=True),
        
        # Complexity metrics
        SimpleField(name="total_lines", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="code_lines", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="comment_lines", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="blank_lines", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="cyclomatic_complexity", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="paragraph_count", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="call_count", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="perform_count", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="file_io_count", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SimpleField(name="max_nesting_depth", type=SearchFieldDataType.Int32, filterable=True, sortable=True, facetable=True),
        SearchableField(name="complexity_category", type=SearchFieldDataType.String, filterable=True, facetable=True),
        
        # Natural language description (searchable)
        SearchableField(name="complexity_description", type=SearchFieldDataType.String),
        
        # Vector field for semantic search
        SearchField(
            name="complexity_description_vector",
            type=SearchFieldDataType.Collection(SearchFieldDataType.Single),
            searchable=True,
            vector_search_dimensions=3072,
            vector_search_profile_name="complexity-vector-profile"
        ),
        
        # Metadata
        SimpleField(name="has_vector", type=SearchFieldDataType.Boolean, filterable=True, facetable=True),
        SimpleField(name="timestamp", type=SearchFieldDataType.String, filterable=True, sortable=True)
    ]
    
    # Configure vector search
    vector_search = VectorSearch(
        algorithms=[
            HnswAlgorithmConfiguration(
                name="complexity-hnsw-config",
                parameters={
                    "m": 4,
                    "ef_construction": 400,
                    "ef_search": 500,
                    "metric": "cosine"
                }
            )
        ],
        profiles=[
            VectorSearchProfile(
                name="complexity-vector-profile",
                algorithm_configuration_name="complexity-hnsw-config"
            )
        ]
    )
    
    # Configure semantic search
    semantic_search = SemanticSearch(
        configurations=[
            SemanticConfiguration(
                name="complexity-semantic-config",
                prioritized_fields=SemanticPrioritizedFields(
                    title_field=SemanticField(field_name="program"),
                    content_fields=[
                        SemanticField(field_name="complexity_description"),
                        SemanticField(field_name="complexity_category")
                    ]
                )
            )
        ]
    )
    
    # Create the index
    index = SearchIndex(
        name=INDEX_NAME,
        fields=fields,
        vector_search=vector_search,
        semantic_search=semantic_search
    )
    
    try:
        result = index_client.create_or_update_index(index)
        print(f"✓ Index '{result.name}' created successfully")
        print(f"  Fields: {len(result.fields)}")
        print(f"  Vector search: Enabled (3072 dimensions)")
        print(f"  Semantic search: Enabled")
        return True
    except Exception as e:
        print(f"✗ Error creating index: {e}")
        return False


if __name__ == "__main__":
    print("=" * 80)
    print("CREATE PROGRAM COMPLEXITY INDEX")
    print("=" * 80)
    
    success = create_complexity_index()
    
    if success:
        print("\n✓ Index creation complete!")
        print(f"\nNext steps:")
        print(f"  1. Run: python upload_complexity_metrics.py")
        print(f"  2. Add '{INDEX_NAME}' to otis_rag/config.py indexes")
        print(f"  3. Update router.py to use complexity data")
    else:
        print("\n✗ Index creation failed")
