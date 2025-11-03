"""
Create Azure Search index for A15 help field documentation.
"""

import sys
sys.path.insert(0, '.')

from azure.search.documents.indexes import SearchIndexClient
from azure.search.documents.indexes.models import (
    SearchIndex,
    SimpleField,
    SearchableField,
    SearchField,
    SearchFieldDataType,
    VectorSearch,
    VectorSearchProfile,
    HnswAlgorithmConfiguration,
)
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

# Load config
config = Config()
search_endpoint = config.search_endpoint
search_key = config.search_key

INDEX_NAME = "help_fields"


def create_help_fields_index():
    """Create the help_fields index with proper schema."""
    
    credential = AzureKeyCredential(search_key)
    index_client = SearchIndexClient(endpoint=search_endpoint, credential=credential)
    
    # Define vector search configuration
    vector_search = VectorSearch(
        profiles=[
            VectorSearchProfile(
                name="help_vector_profile",
                algorithm_configuration_name="help_algorithm_config"
            )
        ],
        algorithms=[
            HnswAlgorithmConfiguration(name="help_algorithm_config")
        ]
    )
    
    # Define index fields
    fields = [
        # Identity fields
        SimpleField(
            name="id",
            type=SearchFieldDataType.String,
            key=True,
            filterable=True,
            sortable=True
        ),
        SimpleField(
            name="full_identifier",
            type=SearchFieldDataType.String,
            filterable=True,
            sortable=True
        ),
        
        # Core fields
        SearchableField(
            name="field_id",
            type=SearchFieldDataType.String,
            filterable=True,
            sortable=True
        ),
        SearchableField(
            name="screen_id",
            type=SearchFieldDataType.String,
            filterable=True,
            sortable=True
        ),
        SimpleField(
            name="module",
            type=SearchFieldDataType.String,
            filterable=True,
            facetable=True,
            sortable=True
        ),
        SimpleField(
            name="field_type",
            type=SearchFieldDataType.String,
            filterable=True,
            facetable=True,
            sortable=True
        ),
        
        # Help text content
        SearchableField(
            name="help_text",
            type=SearchFieldDataType.String,
            analyzer_name="en.microsoft"
        ),
        
        # Metadata fields
        SimpleField(
            name="char_length",
            type=SearchFieldDataType.Int32,
            filterable=True,
            sortable=True
        ),
        
        # Validation info (stored as JSON strings)
        SimpleField(
            name="has_scan_window",
            type=SearchFieldDataType.Boolean,
            filterable=True,
            facetable=True
        ),
        SimpleField(
            name="required",
            type=SearchFieldDataType.Boolean,
            filterable=True,
            facetable=True
        ),
        SimpleField(
            name="optional",
            type=SearchFieldDataType.Boolean,
            filterable=True,
            facetable=True
        ),
        SimpleField(
            name="numeric",
            type=SearchFieldDataType.Boolean,
            filterable=True,
            facetable=True
        ),
        SimpleField(
            name="date_field",
            type=SearchFieldDataType.Boolean,
            filterable=True,
            facetable=True
        ),
        SearchableField(
            name="function_keys",
            type=SearchFieldDataType.String,
            collection=True,
            filterable=True,
            facetable=True
        ),
        SearchableField(
            name="valid_values_json",
            type=SearchFieldDataType.String
        ),
        
        # Vector field for semantic search
        SearchField(
            name="help_vector",
            type=SearchFieldDataType.Collection(SearchFieldDataType.Single),
            vector_search_dimensions=3072,
            vector_search_profile_name="help_vector_profile",
            searchable=True
        ),
        SimpleField(
            name="has_vector",
            type=SearchFieldDataType.Boolean,
            filterable=True
        )
    ]
    
    # Create the index
    index = SearchIndex(
        name=INDEX_NAME,
        fields=fields,
        vector_search=vector_search
    )
    
    try:
        result = index_client.create_or_update_index(index)
        print(f"✅ Index '{INDEX_NAME}' created successfully!")
        print(f"   Fields: {len(result.fields)}")
        print(f"   Vector dimensions: 3072")
        return result
    except Exception as e:
        print(f"❌ Error creating index: {e}")
        raise


def main():
    """Main function."""
    print("=" * 80)
    print("Create Help Fields Index")
    print("=" * 80)
    print()
    
    if not search_endpoint or not search_key:
        print("❌ Error: Azure Search credentials not found in environment")
        print("   Set AZURE_SEARCH_ENDPOINT and AZURE_SEARCH_KEY")
        return
    
    print(f"Search Endpoint: {search_endpoint}")
    print(f"Index Name: {INDEX_NAME}")
    print()
    
    create_help_fields_index()


if __name__ == '__main__':
    main()
