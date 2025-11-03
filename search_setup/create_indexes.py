#!/usr/bin/env python3
"""
Azure Search Index Creation Script
==================================

Creates Azure Search indexes for COBOL analysis using JSONL data from blob storage.

Features:
- Idempotent operation (delete existing indexes before recreating)
- Dry-run mode to preview operations without making changes
- No-delete flag to skip deletion of existing indexes
"""

import os
import sys
import json
import argparse
from typing import Dict, List, Optional

# Check and import required dependencies
def check_dependencies():
    """Check if required packages are installed"""
    required_packages = [
        'azure-search-documents',
        'azure-identity', 
        'azure-storage-blob',
        'requests'
    ]
    
    missing_packages = []
    
    try:
        from azure.search.documents import SearchClient
        from azure.search.documents.indexes import SearchIndexClient, SearchIndexerClient
        from azure.search.documents.indexes.models import (
            SearchIndex, SearchField, SearchFieldDataType,
            VectorSearch, VectorSearchAlgorithmConfiguration,
            HnswAlgorithmConfiguration, VectorSearchProfile,
            AzureOpenAIVectorizer, AzureOpenAIVectorizerParameters,
            SearchIndexerDataSourceConnection, SearchIndexerDataContainer,
            HighWaterMarkChangeDetectionPolicy, SearchIndexerSkillset,
            InputFieldMappingEntry, OutputFieldMappingEntry,
            AzureOpenAIEmbeddingSkill, WebApiSkill,
            SearchIndexer, FieldMapping, IndexingSchedule,
            IndexingParameters
        )
    except ImportError:
        missing_packages.append('azure-search-documents')
    
    try:
        from azure.identity import DefaultAzureCredential
    except ImportError:
        missing_packages.append('azure-identity')
    
    try:
        from azure.storage.blob import BlobServiceClient
    except ImportError:
        missing_packages.append('azure-storage-blob')
    
    try:
        import requests
    except ImportError:
        missing_packages.append('requests')
    
    if missing_packages:
        print("❌ Missing required packages:")
        for package in missing_packages:
            print(f"   {package}")
        print(f"\nInstall with: pip install {' '.join(missing_packages)}")
        return False
    
    return True

def import_dependencies():
    """Import dependencies after checking they exist"""
    from azure.search.documents import SearchClient
    from azure.search.documents.indexes import SearchIndexClient, SearchIndexerClient
    from azure.search.documents.indexes.models import (
        SearchIndex, SearchField, SearchFieldDataType,
        VectorSearch, VectorSearchAlgorithmConfiguration,
        HnswAlgorithmConfiguration, VectorSearchProfile,
        AzureOpenAIVectorizer, AzureOpenAIVectorizerParameters,
        SimpleField, SearchableField, ComplexField,
        SearchIndexerDataSourceConnection, SearchIndexerDataContainer,
        HighWaterMarkChangeDetectionPolicy, SearchIndexerSkillset,
        InputFieldMappingEntry, OutputFieldMappingEntry,
        AzureOpenAIEmbeddingSkill, WebApiSkill,
        SearchIndexer, FieldMapping, IndexingSchedule,
        IndexingParameters
    )
    from azure.core.credentials import AzureKeyCredential
    from azure.storage.blob import BlobServiceClient
    import requests
    
    return {
        'SearchClient': SearchClient,
        'SearchIndexClient': SearchIndexClient,
        'SearchIndexerClient': SearchIndexerClient,
        'SearchIndex': SearchIndex,
        'SearchField': SearchField,
        'SearchFieldDataType': SearchFieldDataType,
        'VectorSearch': VectorSearch,
        'VectorSearchAlgorithmConfiguration': VectorSearchAlgorithmConfiguration,
        'HnswAlgorithmConfiguration': HnswAlgorithmConfiguration,
        'VectorSearchProfile': VectorSearchProfile,
        'AzureOpenAIVectorizer': AzureOpenAIVectorizer,
        'AzureOpenAIVectorizerParameters': AzureOpenAIVectorizerParameters,
        'SimpleField': SimpleField,
        'SearchableField': SearchableField,
        'ComplexField': ComplexField,
        'SearchIndexerDataSourceConnection': SearchIndexerDataSourceConnection,
        'SearchIndexerDataContainer': SearchIndexerDataContainer,
        'HighWaterMarkChangeDetectionPolicy': HighWaterMarkChangeDetectionPolicy,
        'SearchIndexerSkillset': SearchIndexerSkillset,
        'InputFieldMappingEntry': InputFieldMappingEntry,
        'OutputFieldMappingEntry': OutputFieldMappingEntry,
        'AzureOpenAIEmbeddingSkill': AzureOpenAIEmbeddingSkill,
        'WebApiSkill': WebApiSkill,
        'SearchIndexer': SearchIndexer,
        'FieldMapping': FieldMapping,
        'IndexingSchedule': IndexingSchedule,
        'IndexingParameters': IndexingParameters,
        'AzureKeyCredential': AzureKeyCredential,
        'BlobServiceClient': BlobServiceClient,
        'requests': requests
    }

def create_hnsw_vector_profile() -> Dict:
    """
    Create an HNSW vector profile configuration for Azure AI Search.
    Returns a Python dict compatible with Azure AI Search REST create-index payloads.
    
    Configuration:
    - Profile name: "vprof"
    - Algorithm: HNSW with m=24, efConstruction=200
    """
    return {
        "name": "vprof",
        "algorithm": "hnsw-algo"
    }

def create_hnsw_algorithm_config() -> Dict:
    """
    Create HNSW algorithm configuration with custom parameters.
    Returns a Python dict compatible with Azure AI Search REST create-index payloads.
    """
    return {
        "name": "hnsw-algo",
        "kind": "hnsw",
        "parameters": {
            "m": 24,
            "efConstruction": 200
        }
    }

def create_semantic_configuration() -> Dict:
    """
    Create a semantic configuration for Azure AI Search.
    Returns a Python dict compatible with Azure AI Search REST create-index payloads.
    
    Configuration:
    - Name: "semcfg"
    - Title field: "name"
    - Content fields: ["text", "path"]
    """
    return {
        "name": "semcfg",
        "prioritizedFields": {
            "titleField": {"fieldName": "name"},
            "contentFields": [{"fieldName": "text"}, {"fieldName": "path"}],
            "keywordsFields": []
        }
    }

def create_vector_search_config() -> Dict:
    """
    Create a complete vector search configuration combining HNSW profile and algorithm.
    Returns a Python dict compatible with Azure AI Search REST create-index payloads.
    """
    return {
        "algorithms": [create_hnsw_algorithm_config()],
        "profiles": [create_hnsw_vector_profile()]
    }

def create_complete_index_payload(index_name: str, fields: List[Dict], 
                                 include_vector_search: bool = False,
                                 include_semantic_config: bool = False) -> Dict:
    """
    Create a complete index payload for Azure AI Search REST API.
    
    Args:
        index_name: Name of the index
        fields: List of field definitions
        include_vector_search: Whether to include vector search configuration
        include_semantic_config: Whether to include semantic configuration
    
    Returns:
        Complete index payload as Python dict
    """
    payload = {
        "name": index_name,
        "fields": fields
    }
    
    if include_vector_search:
        payload["vectorSearch"] = create_vector_search_config()
    
    if include_semantic_config:
        payload["semantic"] = {
            "configurations": [
                create_semantic_configuration()
            ]
        }
    
    return payload

def demo_helper_functions():
    """
    Demonstration of the helper functions for creating Azure AI Search configurations.
    """
    print("🔧 AZURE AI SEARCH HELPER FUNCTIONS DEMO")
    print("=" * 60)
    
    # Demo HNSW vector profile
    print("\n📊 HNSW Vector Profile Configuration:")
    hnsw_profile = create_hnsw_vector_profile()
    print(json.dumps(hnsw_profile, indent=2))
    
    # Demo HNSW algorithm configuration
    print("\n⚙️ HNSW Algorithm Configuration:")
    hnsw_algo = create_hnsw_algorithm_config()
    print(json.dumps(hnsw_algo, indent=2))
    
    # Demo semantic configuration
    print("\n🔍 Semantic Search Configuration:")
    semantic_config = create_semantic_configuration()
    print(json.dumps(semantic_config, indent=2))
    
    # Demo complete vector search configuration
    print("\n🚀 Complete Vector Search Configuration:")
    vector_search = create_vector_search_config()
    print(json.dumps(vector_search, indent=2))
    
    # Demo complete index payload
    print("\n📋 Sample Complete Index Payload:")
    sample_fields = [
        {
            "name": "id",
            "type": "Edm.String",
            "key": True,
            "searchable": False,
            "filterable": False,
            "sortable": False,
            "facetable": False
        },
        {
            "name": "name", 
            "type": "Edm.String",
            "searchable": True,
            "filterable": True,
            "sortable": True,
            "facetable": False
        },
        {
            "name": "text",
            "type": "Edm.String", 
            "searchable": True,
            "filterable": False,
            "sortable": False,
            "facetable": False
        },
        {
            "name": "path",
            "type": "Edm.String",
            "searchable": True,
            "filterable": True,
            "sortable": False,
            "facetable": False
        },
        {
            "name": "text_vector",
            "type": "Collection(Edm.Single)",
            "searchable": False,
            "filterable": False,
            "sortable": False,
            "facetable": False,
            "dimensions": 1536,
            "vectorSearchProfile": "vprof"
        }
    ]
    
    complete_payload = create_complete_index_payload(
        "sample-index",
        sample_fields,
        include_vector_search=True,
        include_semantic_config=True
    )
    print(json.dumps(complete_payload, indent=2)[:500] + "...")
    
    print(f"\n✅ All helper functions working correctly!")
    print(f"   • HNSW profile 'vprof' with m=24, efConstruction=200")
    print(f"   • Semantic config 'semcfg' with title=name, content=[text, path]")
    print(f"   • REST API compatible Python dictionaries")

class SearchIndexManager:
    """Manages Azure Search indexes for COBOL analysis"""
    
    def __init__(self, search_endpoint: str, search_key: str, 
                 openai_endpoint: str, openai_key: str, embed_deployment: str,
                 storage_conn_str: str, container: str, prefix: str, deps: Dict):
        self.search_endpoint = search_endpoint
        self.search_key = search_key
        self.openai_endpoint = openai_endpoint
        self.openai_key = openai_key
        self.embed_deployment = embed_deployment
        self.storage_conn_str = storage_conn_str
        self.container = container
        self.prefix = prefix
        self.deps = deps
        self.http_error_count = 0  # Track HTTP errors for exit code
        
        # Initialize clients
        SearchIndexClient = deps['SearchIndexClient']
        AzureKeyCredential = deps['AzureKeyCredential']
        BlobServiceClient = deps['BlobServiceClient']
        
        self.search_client = SearchIndexClient(
            endpoint=search_endpoint,
            credential=AzureKeyCredential(search_key)
        )
        
        self.blob_client = BlobServiceClient.from_connection_string(storage_conn_str)
    
    def log_http_response(self, operation: str, response, url: str = None):
        """Log HTTP response details and track errors"""
        if hasattr(response, 'status_code'):
            status_code = response.status_code
            if 200 <= status_code < 300:
                print(f"   📡 HTTP {status_code}: {operation} {'to ' + url if url else ''}")
            else:
                print(f"   ❌ HTTP {status_code}: {operation} {'to ' + url if url else ''}")
                if hasattr(response, 'text'):
                    print(f"       Error: {response.text[:200]}")
                self.http_error_count += 1
        else:
            # For SDK responses that don't have status_code
            print(f"   ✅ SDK: {operation}")
    
    def has_http_errors(self) -> bool:
        """Check if any HTTP errors occurred"""
        return self.http_error_count > 0
        
    def list_existing_indexes(self) -> List[str]:
        """List existing search indexes"""
        try:
            indexes = list(self.search_client.list_indexes())
            return [idx.name for idx in indexes]
        except Exception as e:
            print(f"❌ Error listing indexes: {e}")
            return []
    
    def delete_index(self, index_name: str, dry_run: bool = False) -> bool:
        """Delete an existing index"""
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would delete index: {index_name}")
                return True
            
            self.search_client.delete_index(index_name)
            print(f"   ✅ Deleted index: {index_name}")
            return True
        except Exception as e:
            print(f"   ❌ Error deleting index {index_name}: {e}")
            return False
    
    def create_cobol_files_index(self, dry_run: bool = False) -> bool:
        """Create index for COBOL files metadata"""
        index_name = "cobol-files"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        
        fields = [
            SimpleField(name="file_id", type=SearchFieldDataType.String, key=True),
            SearchableField(name="path", type=SearchFieldDataType.String),
            SearchableField(name="program_id", type=SearchFieldDataType.String),
            SearchableField(name="author", type=SearchFieldDataType.String),
            SearchableField(name="date_written", type=SearchFieldDataType.String),
            SimpleField(name="lines", type=SearchFieldDataType.Int32),
            SimpleField(name="para_count", type=SearchFieldDataType.Int32),
            SimpleField(name="item_count", type=SearchFieldDataType.Int32),
            SearchableField(name="description", type=SearchFieldDataType.String)
        ]
        
        index = SearchIndex(name=index_name, fields=fields)
        
        return self._create_index(index, dry_run)
    
    def create_cobol_paragraphs_index(self, dry_run: bool = False) -> bool:
        """Create index for COBOL paragraphs with vector search"""
        index_name = "cobol-paragraphs"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchField = self.deps['SearchField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        VectorSearch = self.deps['VectorSearch']
        HnswAlgorithmConfiguration = self.deps['HnswAlgorithmConfiguration']
        VectorSearchProfile = self.deps['VectorSearchProfile']
        AzureOpenAIVectorizer = self.deps['AzureOpenAIVectorizer']
        AzureOpenAIVectorizerParameters = self.deps['AzureOpenAIVectorizerParameters']
        
        # Configure vector search
        vector_search = VectorSearch(
            algorithms=[
                HnswAlgorithmConfiguration(name="hnsw-config")
            ],
            profiles=[
                VectorSearchProfile(
                    name="embedding-profile",
                    algorithm_configuration_name="hnsw-config",
                    vectorizer_name="openai-vectorizer"
                )
            ],
            vectorizers=[
                AzureOpenAIVectorizer(
                    vectorizer_name="openai-vectorizer",
                    parameters=AzureOpenAIVectorizerParameters(
                        resource_url=self.openai_endpoint,
                        deployment_name=self.embed_deployment,
                        api_key=self.openai_key
                    )
                )
            ]
        )
        
        fields = [
            SimpleField(name="para_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="name", type=SearchFieldDataType.String),
            SearchableField(name="kind", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="start_line", type=SearchFieldDataType.Int32),
            SimpleField(name="end_line", type=SearchFieldDataType.Int32),
            SearchableField(name="content", type=SearchFieldDataType.String),
            SearchField(
                name="content_vector",
                type=SearchFieldDataType.Collection(SearchFieldDataType.Single),
                searchable=True,
                vector_search_dimensions=1536,
                vector_search_profile_name="embedding-profile"
            )
        ]
        
        index = SearchIndex(name=index_name, fields=fields, vector_search=vector_search)
        
        return self._create_index(index, dry_run)
    
    def create_cobol_data_items_index(self, dry_run: bool = False) -> bool:
        """Create index for COBOL data items"""
        index_name = "cobol-data-items"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        
        fields = [
            SimpleField(name="item_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="name", type=SearchFieldDataType.String),
            SearchableField(name="qualified_name", type=SearchFieldDataType.String),
            SimpleField(name="level", type=SearchFieldDataType.Int32, filterable=True),
            SearchableField(name="section", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="pic", type=SearchFieldDataType.String),
            SearchableField(name="usage", type=SearchFieldDataType.String),
            SimpleField(name="occurs_low", type=SearchFieldDataType.Int32),
            SimpleField(name="occurs_high", type=SearchFieldDataType.Int32),
            SearchableField(name="value", type=SearchFieldDataType.String),
            SimpleField(name="start_line", type=SearchFieldDataType.Int32),
            SimpleField(name="end_line", type=SearchFieldDataType.Int32)
        ]
        
        index = SearchIndex(name=index_name, fields=fields)
        
        return self._create_index(index, dry_run)
    
    def create_cobol_xrefs_index(self, dry_run: bool = False) -> bool:
        """Create index for COBOL cross-references with vector search"""
        index_name = "cobol-xrefs"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchField = self.deps['SearchField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        VectorSearch = self.deps['VectorSearch']
        HnswAlgorithmConfiguration = self.deps['HnswAlgorithmConfiguration']
        VectorSearchProfile = self.deps['VectorSearchProfile']
        AzureOpenAIVectorizer = self.deps['AzureOpenAIVectorizer']
        AzureOpenAIVectorizerParameters = self.deps['AzureOpenAIVectorizerParameters']
        
        # Configure vector search
        vector_search = VectorSearch(
            algorithms=[
                HnswAlgorithmConfiguration(name="hnsw-config")
            ],
            profiles=[
                VectorSearchProfile(
                    name="embedding-profile", 
                    algorithm_configuration_name="hnsw-config",
                    vectorizer_name="openai-vectorizer"
                )
            ],
            vectorizers=[
                AzureOpenAIVectorizer(
                    vectorizer_name="openai-vectorizer",
                    parameters=AzureOpenAIVectorizerParameters(
                        resource_url=self.openai_endpoint,
                        deployment_name=self.embed_deployment,
                        api_key=self.openai_key
                    )
                )
            ]
        )
        
        fields = [
            SimpleField(name="xref_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="path", type=SearchFieldDataType.String),
            SearchableField(name="program_id", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="line", type=SearchFieldDataType.Int32),
            SearchableField(name="qualified_name", type=SearchFieldDataType.String),
            SearchableField(name="simple_name", type=SearchFieldDataType.String),
            SearchableField(name="kind", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="snippet", type=SearchFieldDataType.String),
            SearchableField(name="direction", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="start_col", type=SearchFieldDataType.Int32),
            SimpleField(name="end_col", type=SearchFieldDataType.Int32),
            SearchField(
                name="snippet_vector",
                type=SearchFieldDataType.Collection(SearchFieldDataType.Single),
                searchable=True,
                vector_search_dimensions=1536,
                vector_search_profile_name="embedding-profile"
            )
        ]
        
        index = SearchIndex(name=index_name, fields=fields, vector_search=vector_search)
        
        return self._create_index(index, dry_run)
    
    def create_cobol_calls_index(self, dry_run: bool = False) -> bool:
        """Create index for COBOL call relationships"""
        index_name = "cobol-calls"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        
        fields = [
            SimpleField(name="call_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="calling_name", type=SearchFieldDataType.String),
            SearchableField(name="called_name", type=SearchFieldDataType.String),
            SimpleField(name="line", type=SearchFieldDataType.Int32),
            SearchableField(name="call_type", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="is_dynamic", type=SearchFieldDataType.Boolean, filterable=True)
        ]
        
        index = SearchIndex(name=index_name, fields=fields)
        
        return self._create_index(index, dry_run)
    
    def create_index_symbols(self, dry_run: bool = False) -> bool:
        """Create index for COBOL symbols"""
        index_name = "cobol-symbols"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        
        fields = [
            # Updated: primary key now item_id (was symbol_id)
            SimpleField(name="item_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="path", type=SearchFieldDataType.String),
            SearchableField(name="program_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="name", type=SearchFieldDataType.String),
            SearchableField(name="qualified_name", type=SearchFieldDataType.String),
            SearchableField(name="section", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="level", type=SearchFieldDataType.Int32, filterable=True),
            SearchableField(name="pic", type=SearchFieldDataType.String),
            SearchableField(name="usage", type=SearchFieldDataType.String),
            SimpleField(name="occurs_low", type=SearchFieldDataType.Int32),
            SimpleField(name="occurs_high", type=SearchFieldDataType.Int32),
            SearchableField(name="depends_on", type=SearchFieldDataType.String),
            SearchableField(name="redefines", type=SearchFieldDataType.String),
            SearchableField(name="value", type=SearchFieldDataType.String),
            SimpleField(name="start_line", type=SearchFieldDataType.Int32, filterable=True),
            SimpleField(name="end_line", type=SearchFieldDataType.Int32, filterable=True)
        ]
        
        index = SearchIndex(name=index_name, fields=fields)
        
        return self._create_index(index, dry_run)
    
    def create_index_xrefs(self, dry_run: bool = False) -> bool:
        """Create index for COBOL cross-references"""
        index_name = "cobol-xrefs-simple"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        
        fields = [
            SimpleField(name="xref_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="path", type=SearchFieldDataType.String),
            SimpleField(name="program_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="qualified_name", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="simple_name", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="kind", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="direction", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="line", type=SearchFieldDataType.Int32, filterable=True),
            SearchableField(name="snippet", type=SearchFieldDataType.String),
            SimpleField(name="start_col", type=SearchFieldDataType.Int32),
            SimpleField(name="end_col", type=SearchFieldDataType.Int32)
        ]
        
        index = SearchIndex(name=index_name, fields=fields)
        
        return self._create_index(index, dry_run)
    
    def create_index_calls(self, dry_run: bool = False) -> bool:
        """Create index for COBOL call relationships"""
        index_name = "cobol-calls-simple"
        
        # Get dependencies
        SimpleField = self.deps['SimpleField']
        SearchableField = self.deps['SearchableField']
        SearchFieldDataType = self.deps['SearchFieldDataType']
        SearchIndex = self.deps['SearchIndex']
        
        fields = [
            SimpleField(name="call_id", type=SearchFieldDataType.String, key=True),
            SimpleField(name="file_id", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="caller_para", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="callee_program", type=SearchFieldDataType.String, filterable=True),
            SearchableField(name="callee_data_name", type=SearchFieldDataType.String, filterable=True),
            SimpleField(name="is_dynamic", type=SearchFieldDataType.Boolean, filterable=True),
            SimpleField(name="line", type=SearchFieldDataType.Int32, filterable=True),
            SearchableField(name="snippet", type=SearchFieldDataType.String)
        ]
        
        index = SearchIndex(name=index_name, fields=fields)
        
        return self._create_index(index, dry_run)
    
    def create_index_code_chunks(self, dry_run: bool = False, no_delete: bool = False) -> bool:
        """Create index for code chunks using REST API with advanced vector search and semantic configurations"""
        index_name = "code-chunks"
        api_version = "2024-07-01-Preview"
        
        try:
            # Check if index exists and delete if needed
            if not no_delete:
                existing_indexes = self.list_existing_indexes()
                if index_name in existing_indexes:
                    if dry_run:
                        print(f"   [DRY-RUN] Would delete existing index: {index_name}")
                    else:
                        self.delete_index(index_name)
                        print(f"   ✅ Deleted existing index: {index_name}")
            
            if dry_run:
                print(f"   [DRY-RUN] Would create index: {index_name} with vector search and semantic config")
                return True
            
            # Define fields for the index
            fields = [
                {
                    "name": "chunk_id",
                    "type": "Edm.String",
                    "key": True,
                    "searchable": False,
                    "filterable": True,
                    "sortable": False,
                    "facetable": False
                },
                {
                    "name": "file_id", 
                    "type": "Edm.String",
                    "searchable": False,
                    "filterable": True,
                    "sortable": False,
                    "facetable": True
                },
                {
                    "name": "path",
                    "type": "Edm.String", 
                    "searchable": True,
                    "filterable": False,
                    "sortable": False,
                    "facetable": False
                },
                {
                    "name": "program_id",
                    "type": "Edm.String",
                    "searchable": False,
                    "filterable": True,
                    "sortable": False,
                    "facetable": True
                },
                {
                    "name": "scope",
                    "type": "Edm.String",
                    "searchable": False,
                    "filterable": True,
                    "sortable": False,
                    "facetable": True
                },
                {
                    "name": "name",
                    "type": "Edm.String",
                    "searchable": True,
                    "filterable": True,
                    "sortable": False,
                    "facetable": False
                },
                {
                    "name": "start_line",
                    "type": "Edm.Int32",
                    "searchable": False,
                    "filterable": True,
                    "sortable": True,
                    "facetable": False
                },
                {
                    "name": "end_line",
                    "type": "Edm.Int32",
                    "searchable": False,
                    "filterable": True,
                    "sortable": True,
                    "facetable": False
                },
                {
                    "name": "text",
                    "type": "Edm.String",
                    "searchable": True,
                    "filterable": False,
                    "sortable": False,
                    "facetable": False
                },
                # New: embedding field (replaces text_vector) dimension 3072, not searchable, not retrievable
                {
                    "name": "embedding",
                    "type": "Collection(Edm.Single)",
                    "searchable": False,
                    "retrievable": False,
                    "filterable": False,
                    "sortable": False,
                    "facetable": False,
                    "dimensions": 3072,
                    "vectorSearchProfile": "vprof"
                }
            ]
            
            # Create enhanced index payload with integrated vectorization
            index_payload = {
                "name": index_name,
                "fields": fields,
                "vectorSearch": {
                    "algorithms": [create_hnsw_algorithm_config()],
                    "profiles": [create_hnsw_vector_profile()],
                    "vectorizers": [
                        {
                            "name": "openai-vectorizer",
                            "kind": "azureOpenAI",
                            "azureOpenAIParameters": {
                                "resourceUri": self.openai_endpoint,
                                "deploymentId": self.embed_deployment,
                                "apiKey": self.openai_key
                            }
                        }
                    ]
                },
                "semantic": {
                    "configurations": [
                        create_semantic_configuration()
                    ]
                }
            }
            
            # Debug payload structure
            print(f"   📋 Index payload preview:")
            print(json.dumps(index_payload, indent=2)[:1200])
            if len(json.dumps(index_payload, indent=2)) > 1200:
                print("   ... (truncated)")
            
            # Make REST API call
            url = f"{self.search_endpoint}/indexes?api-version={api_version}"
            headers = {
                "Content-Type": "application/json",
                "api-key": self.search_key
            }
            
            requests = self.deps['requests']
            response = requests.post(url, headers=headers, json=index_payload)
            
            # Log HTTP response
            self.log_http_response(f"Create index {index_name}", response, url)
            
            if response.status_code == 201:
                print(f"   ✅ Created index: {index_name} with integrated vectorization (1536 dims) and semantic config")
                return True
            else:
                print(f"   ❌ Error creating index {index_name}: {response.status_code}")
                print(f"   Response: {response.text}")
                self.http_error_count += 1  # Count REST errors
                return False
                
        except Exception as e:
            print(f"   ❌ Error creating index {index_name}: {e}")
            return False
    
    def _create_index(self, index, dry_run: bool = False) -> bool:
        """Create a search index"""
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would create index: {index.name} with {len(index.fields)} fields")
                if index.vector_search:
                    print(f"   [DRY-RUN] Vector search enabled with {len(index.vector_search.profiles)} profiles")
                return True
            
            result = self.search_client.create_index(index)
            self.log_http_response(f"Create index {index.name}", result)
            print(f"   ✅ Created index: {index.name}")
            return True
        except Exception as e:
            print(f"   ❌ Error creating index {index.name}: {e}")
            self.http_error_count += 1  # Count SDK errors as HTTP errors
            return False
    
    def create_data_source(self, name: str, container: str, prefix: str, dry_run: bool = False) -> bool:
        """Create or update a Blob data source for JSONL files"""
        try:
            # Get dependencies for data source creation
            SearchIndexerClient = self.deps.get('SearchIndexerClient')
            if not SearchIndexerClient:
                # Try to import it if not already available
                try:
                    from azure.search.documents.indexes import SearchIndexerClient
                    from azure.search.documents.indexes.models import (
                        SearchIndexerDataSourceConnection, SearchIndexerDataContainer,
                        HighWaterMarkChangeDetectionPolicy
                    )
                    SearchIndexerClient = SearchIndexerClient
                except ImportError as e:
                    print(f"   ❌ Cannot import SearchIndexerClient: {e}")
                    return False
            else:
                from azure.search.documents.indexes.models import (
                    SearchIndexerDataSourceConnection, SearchIndexerDataContainer,
                    HighWaterMarkChangeDetectionPolicy
                )
            
            if dry_run:
                print(f"   [DRY-RUN] Would create data source: {name}")
                print(f"   [DRY-RUN]   Container: {container}")
                print(f"   [DRY-RUN]   Prefix: {prefix}")
                return True
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            # Create data container with query (prefix)
            data_container = SearchIndexerDataContainer(
                name=container,
                query=prefix
            )
            
            # Create change detection policy using _ts (timestamp) if supported
            try:
                change_detection_policy = HighWaterMarkChangeDetectionPolicy(
                    high_water_mark_column_name="_ts"
                )
            except Exception:
                # If HighWaterMark not supported, use None
                change_detection_policy = None
            
            # Create the data source connection
            data_source = SearchIndexerDataSourceConnection(
                name=name,
                type="azureblob",
                connection_string=self.storage_conn_str,
                container=data_container,
                data_change_detection_policy=change_detection_policy
            )
            
            # Create or update the data source
            result = self.indexer_client.create_or_update_data_source_connection(data_source)
            self.log_http_response(f"Create data source {name}", result)
            print(f"   ✅ Created data source: {name}")
            print(f"       Container: {container}, Prefix: {prefix}")
            return True
            
        except Exception as e:
            print(f"   ❌ Error creating data source {name}: {e}")
            self.http_error_count += 1
            return False
    
    def list_existing_data_sources(self) -> List[str]:
        """List existing data sources"""
        try:
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            data_sources = list(self.indexer_client.get_data_source_connections())
            return [ds.name for ds in data_sources]
        except Exception as e:
            print(f"❌ Error listing data sources: {e}")
            return []
    
    def delete_data_source(self, data_source_name: str, dry_run: bool = False) -> bool:
        """Delete an existing data source"""
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would delete data source: {data_source_name}")
                return True
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            self.indexer_client.delete_data_source_connection(data_source_name)
            print(f"   ✅ Deleted data source: {data_source_name}")
            return True
        except Exception as e:
            print(f"   ❌ Error deleting data source {data_source_name}: {e}")
            return False
    
    def create_data_sources(self, dry_run: bool = False) -> bool:
        """Create all required data sources for COBOL analysis"""
        print(f"\n📊 CREATING DATA SOURCES")
        
        data_sources = [
            ("ds_chunks", self.container, f"{self.prefix}chunks.jsonl"),
            ("ds_symbols", self.container, f"{self.prefix}data_items.jsonl"),
            ("ds_xrefs", self.container, f"{self.prefix}xrefs.jsonl"),
            ("ds_calls", self.container, f"{self.prefix}calls.jsonl")
        ]
        
        success_count = 0
        for name, container, prefix in data_sources:
            print(f"Creating data source: {name}...")
            if self.create_data_source(name, container, prefix, dry_run=dry_run):
                success_count += 1
        
        print(f"   Created {success_count}/{len(data_sources)} data sources")
        return success_count == len(data_sources)
    
    def create_skillset_embeddings(self, dry_run: bool = False) -> str:
        """Create skillset for generating embeddings using Azure OpenAI"""
        skillset_name = "sk_code_chunks_embed"
        
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would create skillset: {skillset_name}")
                print(f"   [DRY-RUN]   Azure OpenAI Embedding Skill")
                print(f"   [DRY-RUN]   Input: /document/text")
                print(f"   [DRY-RUN]   Output: /document/embedding")
                return skillset_name
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            # Get required dependencies
            SearchIndexerSkillset = self.deps['SearchIndexerSkillset']
            InputFieldMappingEntry = self.deps['InputFieldMappingEntry']
            OutputFieldMappingEntry = self.deps['OutputFieldMappingEntry']
            AzureOpenAIEmbeddingSkill = self.deps['AzureOpenAIEmbeddingSkill']
            
            # Create Azure OpenAI embedding skill
            embedding_skill = AzureOpenAIEmbeddingSkill(
                name="embedding_skill",
                description="Generate embeddings for text content using Azure OpenAI",
                context="/document",
                resource_uri=self.openai_endpoint,
                deployment_id=self.embed_deployment,
                api_key=self.openai_key,
                inputs=[
                    InputFieldMappingEntry(name="text", source="/document/text")
                ],
                outputs=[
                    OutputFieldMappingEntry(name="embedding", target_name="/document/embedding")
                ]
            )
            
            # Create skillset with the embedding skill
            skillset = SearchIndexerSkillset(
                name=skillset_name,
                description="Skillset for generating embeddings from code chunks",
                skills=[embedding_skill]
            )
            
            # Create or update the skillset
            result = self.indexer_client.create_or_update_skillset(skillset)
            self.log_http_response(f"Create skillset {skillset_name}", result)
            print(f"   ✅ Created skillset: {skillset_name}")
            print(f"       Azure OpenAI Embedding Skill configured")
            return skillset_name
            
        except Exception as e:
            print(f"   ❌ Error creating skillset {skillset_name}: {e}")
            
            # Try with Web API skill as fallback if Azure OpenAI skill not supported
            try:
                print(f"   🔄 Attempting fallback with Web API skill...")
                
                WebApiSkill = self.deps['WebApiSkill']
                
                # Create Web API skill for Azure OpenAI embeddings
                web_api_skill = WebApiSkill(
                    name="webapi_embedding_skill",
                    description="Generate embeddings using Azure OpenAI Web API",
                    context="/document",
                    uri=f"{self.openai_endpoint.rstrip('/')}/openai/deployments/{self.embed_deployment}/embeddings?api-version=2024-02-01",
                    http_method="POST",
                    http_headers={
                        "Content-Type": "application/json",
                        "api-key": self.openai_key
                    },
                    inputs=[
                        InputFieldMappingEntry(name="input", source="/document/text")
                    ],
                    outputs=[
                        OutputFieldMappingEntry(name="embedding", target_name="/document/embedding")
                    ]
                )
                
                # Create skillset with Web API skill
                skillset = SearchIndexerSkillset(
                    name=skillset_name,
                    description="Skillset for generating embeddings from code chunks (Web API)",
                    skills=[web_api_skill]
                )
                
                result = self.indexer_client.create_or_update_skillset(skillset)
                self.log_http_response(f"Create skillset {skillset_name} (fallback)", result)
                print(f"   ✅ Created skillset: {skillset_name} (Web API fallback)")
                return skillset_name
                
            except Exception as fallback_error:
                print(f"   ❌ Error creating skillset with Web API fallback: {fallback_error}")
                return ""
    
    def list_existing_skillsets(self) -> List[str]:
        """List existing skillsets"""
        try:
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            skillsets = list(self.indexer_client.get_skillsets())
            return [ss.name for ss in skillsets]
        except Exception as e:
            print(f"❌ Error listing skillsets: {e}")
            return []
    
    def delete_skillset(self, skillset_name: str, dry_run: bool = False) -> bool:
        """Delete an existing skillset"""
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would delete skillset: {skillset_name}")
                return True
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            self.indexer_client.delete_skillset(skillset_name)
            print(f"   ✅ Deleted skillset: {skillset_name}")
            return True
        except Exception as e:
            print(f"   ❌ Error deleting skillset {skillset_name}: {e}")
            return False
    
    def list_existing_indexers(self) -> List[str]:
        """List existing indexers"""
        try:
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            indexers = list(self.indexer_client.get_indexers())
            return [idx.name for idx in indexers]
        except Exception as e:
            print(f"❌ Error listing indexers: {e}")
            return []
    
    def delete_indexer(self, indexer_name: str, dry_run: bool = False) -> bool:
        """Delete an existing indexer"""
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would delete indexer: {indexer_name}")
                return True
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            self.indexer_client.delete_indexer(indexer_name)
            print(f"   ✅ Deleted indexer: {indexer_name}")
            return True
        except Exception as e:
            print(f"   ❌ Error deleting indexer {indexer_name}: {e}")
            return False
    
    def create_indexer_chunks(self, data_source_name: str, skillset_name: str, dry_run: bool = False) -> str:
        """Create indexer for code chunks with embedding skills"""
        indexer_name = "idx_code_chunks"
        index_name = "code-chunks"
        
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would create indexer: {indexer_name}")
                print(f"   [DRY-RUN]   Data source: {data_source_name}")
                print(f"   [DRY-RUN]   Skillset: {skillset_name}")
                print(f"   [DRY-RUN]   Target index: {index_name}")
                print(f"   [DRY-RUN]   Schedule: Every 4 hours")
                print(f"   [DRY-RUN]   Parsing mode: JSON Lines")
                return indexer_name
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            # Get required dependencies
            SearchIndexer = self.deps['SearchIndexer']
            FieldMapping = self.deps['FieldMapping']
            IndexingSchedule = self.deps['IndexingSchedule']
            IndexingParameters = self.deps['IndexingParameters']
            
            # Create field mappings (source → target)
            field_mappings = [
                FieldMapping(source_field_name="chunk_id", target_field_name="chunk_id"),
                FieldMapping(source_field_name="file_id", target_field_name="file_id"),
                FieldMapping(source_field_name="path", target_field_name="path"),
                FieldMapping(source_field_name="program_id", target_field_name="program_id"),
                FieldMapping(source_field_name="scope", target_field_name="scope"),
                FieldMapping(source_field_name="name", target_field_name="name"),
                FieldMapping(source_field_name="start_line", target_field_name="start_line"),
                FieldMapping(source_field_name="end_line", target_field_name="end_line"),
                FieldMapping(source_field_name="text", target_field_name="text"),
            ]
            
            # Create output field mappings (from skillset)
            output_field_mappings = [
                FieldMapping(source_field_name="/document/embedding", target_field_name="embedding")
            ]
            
            # Create schedule (every 4 hours)
            schedule = IndexingSchedule(interval="PT4H")  # PT4H = 4 hours in ISO 8601 duration format
            
            # Create indexing parameters for JSON Lines
            parameters = IndexingParameters(
                parsing_mode="jsonLines",
                configuration={"parsingMode": "jsonLines"}
            )
            
            # Create the indexer
            indexer = SearchIndexer(
                name=indexer_name,
                description="Indexer for COBOL code chunks with embedding generation",
                data_source_name=data_source_name,
                target_index_name=index_name,
                skillset_name=skillset_name,
                field_mappings=field_mappings,
                output_field_mappings=output_field_mappings,
                schedule=schedule,
                parameters=parameters
            )
            
            # Create or update the indexer
            result = self.indexer_client.create_or_update_indexer(indexer)
            self.log_http_response(f"Create indexer {indexer_name}", result)
            print(f"   ✅ Created indexer: {indexer_name}")
            print(f"       Target: {index_name}, Schedule: Every 4 hours")
            return indexer_name
            
        except Exception as e:
            print(f"   ❌ Error creating indexer {indexer_name}: {e}")
            self.http_error_count += 1
            return ""
    
    def create_indexer_symbols(self, data_source_name: str, dry_run: bool = False) -> str:
        """Create indexer for COBOL symbols (from data_items.jsonl)"""
        indexer_name = "idx_symbols"
        index_name = "cobol-symbols"
        
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would create indexer: {indexer_name}")
                print(f"   [DRY-RUN]   Data source: {data_source_name}")
                print(f"   [DRY-RUN]   Target index: {index_name}")
                print(f"   [DRY-RUN]   Schedule: Every 4 hours")
                print(f"   [DRY-RUN]   Parsing mode: JSON Lines")
                print(f"   [DRY-RUN]   Key field: item_id")
                return indexer_name
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            # Get required dependencies
            SearchIndexer = self.deps['SearchIndexer']
            FieldMapping = self.deps['FieldMapping']
            IndexingSchedule = self.deps['IndexingSchedule']
            IndexingParameters = self.deps['IndexingParameters']
            
            # Create field mappings (key item_id -> item_id)
            field_mappings = [
                FieldMapping(source_field_name="item_id", target_field_name="item_id"),
                FieldMapping(source_field_name="file_id", target_field_name="file_id"),
                FieldMapping(source_field_name="path", target_field_name="path", mapping_function=None),
                FieldMapping(source_field_name="program_id", target_field_name="program_id", mapping_function=None),
                FieldMapping(source_field_name="name", target_field_name="name"),
                FieldMapping(source_field_name="qualified_name", target_field_name="qualified_name"),
                FieldMapping(source_field_name="section", target_field_name="section"),
                FieldMapping(source_field_name="level", target_field_name="level"),
                FieldMapping(source_field_name="pic", target_field_name="pic"),
                FieldMapping(source_field_name="usage", target_field_name="usage"),
                FieldMapping(source_field_name="occurs_low", target_field_name="occurs_low"),
                FieldMapping(source_field_name="occurs_high", target_field_name="occurs_high"),
                FieldMapping(source_field_name="depends_on", target_field_name="depends_on"),
                FieldMapping(source_field_name="redefines", target_field_name="redefines"),
                FieldMapping(source_field_name="value", target_field_name="value"),
                FieldMapping(source_field_name="start_line", target_field_name="start_line"),
                FieldMapping(source_field_name="end_line", target_field_name="end_line")
            ]
            
            # Create schedule (every 4 hours)
            schedule = IndexingSchedule(interval="PT4H")
            
            # Create indexing parameters for JSON Lines
            parameters = IndexingParameters(
                parsing_mode="jsonLines",
                configuration={"parsingMode": "jsonLines"}
            )
            
            # Create the indexer
            indexer = SearchIndexer(
                name=indexer_name,
                description="Indexer for COBOL symbols from data items",
                data_source_name=data_source_name,
                target_index_name=index_name,
                field_mappings=field_mappings,
                schedule=schedule,
                parameters=parameters
            )
            
            # Create or update the indexer
            result = self.indexer_client.create_or_update_indexer(indexer)
            self.log_http_response(f"Create indexer {indexer_name}", result)
            print(f"   ✅ Created indexer: {indexer_name}")
            print(f"       Target: {index_name}, Schedule: Every 4 hours")
            return indexer_name
            
        except Exception as e:
            print(f"   ❌ Error creating indexer {indexer_name}: {e}")
            self.http_error_count += 1
            return ""
    
    def create_indexer_xrefs(self, data_source_name: str, dry_run: bool = False) -> str:
        """Create indexer for COBOL cross-references"""
        indexer_name = "idx_xrefs"
        index_name = "cobol-xrefs-simple"
        
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would create indexer: {indexer_name}")
                print(f"   [DRY-RUN]   Data source: {data_source_name}")
                print(f"   [DRY-RUN]   Target index: {index_name}")
                print(f"   [DRY-RUN]   Schedule: Every 4 hours")
                print(f"   [DRY-RUN]   Parsing mode: JSON Lines")
                return indexer_name
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            # Get required dependencies
            SearchIndexer = self.deps['SearchIndexer']
            FieldMapping = self.deps['FieldMapping']
            IndexingSchedule = self.deps['IndexingSchedule']
            IndexingParameters = self.deps['IndexingParameters']
            
            # Create field mappings
            field_mappings = [
                FieldMapping(source_field_name="xref_id", target_field_name="xref_id"),
                FieldMapping(source_field_name="file_id", target_field_name="file_id"),
                FieldMapping(source_field_name="path", target_field_name="path"),
                FieldMapping(source_field_name="program_id", target_field_name="program_id"),
                FieldMapping(source_field_name="qualified_name", target_field_name="qualified_name"),
                FieldMapping(source_field_name="simple_name", target_field_name="simple_name"),
                FieldMapping(source_field_name="kind", target_field_name="kind"),
                FieldMapping(source_field_name="direction", target_field_name="direction"),
                FieldMapping(source_field_name="line", target_field_name="line"),
                FieldMapping(source_field_name="snippet", target_field_name="snippet"),
                FieldMapping(source_field_name="start_col", target_field_name="start_col"),
                FieldMapping(source_field_name="end_col", target_field_name="end_col")
            ]
            
            # Create schedule (every 4 hours)
            schedule = IndexingSchedule(interval="PT4H")
            
            # Create indexing parameters for JSON Lines
            parameters = IndexingParameters(
                parsing_mode="jsonLines",
                configuration={"parsingMode": "jsonLines"}
            )
            
            # Create the indexer
            indexer = SearchIndexer(
                name=indexer_name,
                description="Indexer for COBOL cross-references",
                data_source_name=data_source_name,
                target_index_name=index_name,
                field_mappings=field_mappings,
                schedule=schedule,
                parameters=parameters
            )
            
            # Create or update the indexer
            result = self.indexer_client.create_or_update_indexer(indexer)
            self.log_http_response(f"Create indexer {indexer_name}", result)
            print(f"   ✅ Created indexer: {indexer_name}")
            print(f"       Target: {index_name}, Schedule: Every 4 hours")
            return indexer_name
            
        except Exception as e:
            print(f"   ❌ Error creating indexer {indexer_name}: {e}")
            self.http_error_count += 1
            return ""
    
    def create_indexer_calls(self, data_source_name: str, dry_run: bool = False) -> str:
        """Create indexer for COBOL call relationships"""
        indexer_name = "idx_calls"
        index_name = "cobol-calls-simple"
        
        try:
            if dry_run:
                print(f"   [DRY-RUN] Would create indexer: {indexer_name}")
                print(f"   [DRY-RUN]   Data source: {data_source_name}")
                print(f"   [DRY-RUN]   Target index: {index_name}")
                print(f"   [DRY-RUN]   Schedule: Every 4 hours")
                print(f"   [DRY-RUN]   Parsing mode: JSON Lines")
                return indexer_name
            
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            # Get required dependencies
            SearchIndexer = self.deps['SearchIndexer']
            FieldMapping = self.deps['FieldMapping']
            IndexingSchedule = self.deps['IndexingSchedule']
            IndexingParameters = self.deps['IndexingParameters']
            
            # Create field mappings
            field_mappings = [
                FieldMapping(source_field_name="call_id", target_field_name="call_id"),
                FieldMapping(source_field_name="file_id", target_field_name="file_id"),
                FieldMapping(source_field_name="caller_para", target_field_name="caller_para"),
                FieldMapping(source_field_name="callee_program", target_field_name="callee_program"),
                FieldMapping(source_field_name="callee_data_name", target_field_name="callee_data_name"),
                FieldMapping(source_field_name="is_dynamic", target_field_name="is_dynamic"),
                FieldMapping(source_field_name="line", target_field_name="line"),
                FieldMapping(source_field_name="snippet", target_field_name="snippet")
            ]
            
            # Create schedule (every 4 hours)
            schedule = IndexingSchedule(interval="PT4H")
            
            # Create indexing parameters for JSON Lines
            parameters = IndexingParameters(
                parsing_mode="jsonLines",
                configuration={"parsingMode": "jsonLines"}
            )
            
            # Create the indexer
            indexer = SearchIndexer(
                name=indexer_name,
                description="Indexer for COBOL call relationships",
                data_source_name=data_source_name,
                target_index_name=index_name,
                field_mappings=field_mappings,
                schedule=schedule,
                parameters=parameters
            )
            
            # Create or update the indexer
            result = self.indexer_client.create_or_update_indexer(indexer)
            self.log_http_response(f"Create indexer {indexer_name}", result)
            print(f"   ✅ Created indexer: {indexer_name}")
            print(f"       Target: {index_name}, Schedule: Every 4 hours")
            return indexer_name
            
        except Exception as e:
            print(f"   ❌ Error creating indexer {indexer_name}: {e}")
            self.http_error_count += 1
            return ""
    
    def _supports_mapping_functions(self) -> bool:
        """Check if the Azure Search service supports input field mapping functions"""
        # This is a placeholder for checking service capabilities
        # In practice, you might want to check the service tier or API version
        return True
    
    def create_indexers(self, dry_run: bool = False) -> bool:
        """Create all required indexers for COBOL analysis"""
        print(f"\n🔄 CREATING INDEXERS")
        
        # Get data source and skillset names
        skillset_name = "sk_code_chunks_embed"
        
        indexers_to_create = [
            ("Chunks", lambda: self.create_indexer_chunks("ds_chunks", skillset_name, dry_run=dry_run)),
            ("Symbols", lambda: self.create_indexer_symbols("ds_symbols", dry_run=dry_run)),
            ("XRefs", lambda: self.create_indexer_xrefs("ds_xrefs", dry_run=dry_run)),
            ("Calls", lambda: self.create_indexer_calls("ds_calls", dry_run=dry_run))
        ]
        
        success_count = 0
        for name, create_func in indexers_to_create:
            print(f"Creating {name} indexer...")
            indexer_name = create_func()
            if indexer_name:
                success_count += 1
        
        print(f"   Created {success_count}/{len(indexers_to_create)} indexers")
        return success_count == len(indexers_to_create)
    
    def run_indexers_immediately(self, dry_run: bool = False) -> bool:
        """Run all indexers immediately after creation"""
        print(f"\n🚀 RUNNING INDEXERS IMMEDIATELY")
        
        if dry_run:
            print("   [DRY-RUN] Would run all indexers now")
            return True
        
        indexer_names = ["idx_code_chunks", "idx_symbols", "idx_xrefs", "idx_calls"]
        
        try:
            # Create indexer client if not already exists
            if not hasattr(self, 'indexer_client'):
                AzureKeyCredential = self.deps['AzureKeyCredential']
                SearchIndexerClient = self.deps.get('SearchIndexerClient')
                if not SearchIndexerClient:
                    from azure.search.documents.indexes import SearchIndexerClient
                
                self.indexer_client = SearchIndexerClient(
                    endpoint=self.search_endpoint,
                    credential=AzureKeyCredential(self.search_key)
                )
            
            success_count = 0
            for indexer_name in indexer_names:
                try:
                    print(f"   Running indexer: {indexer_name}...")
                    result = self.indexer_client.run_indexer(indexer_name)
                    self.log_http_response(f"Run indexer {indexer_name}", result)
                    print(f"   ✅ Started indexer: {indexer_name}")
                    success_count += 1
                except Exception as e:
                    print(f"   ❌ Error running indexer {indexer_name}: {e}")
            
            print(f"   Started {success_count}/{len(indexer_names)} indexers")
            return success_count == len(indexer_names)
            
        except Exception as e:
            print(f"   ❌ Error running indexers: {e}")
            return False
    
    def create_required_indexes(self, dry_run: bool = False, no_delete: bool = False) -> bool:
        """Create the four required indexes: code_chunks, symbols, xrefs, calls"""
        print(f"\n🏗️  CREATING REQUIRED INDEXES")
        
        required_indexes = [
            ("code_chunks", lambda: self.create_index_code_chunks(dry_run=dry_run, no_delete=no_delete)),
            ("symbols", lambda: self.create_index_symbols(dry_run=dry_run)),
            ("xrefs", lambda: self.create_index_xrefs(dry_run=dry_run)),
            ("calls", lambda: self.create_index_calls(dry_run=dry_run))
        ]
        
        success_count = 0
        for name, create_func in required_indexes:
            print(f"Creating {name} index...")
            if create_func():
                success_count += 1
        
        print(f"   Created {success_count}/{len(required_indexes)} required indexes")
        return success_count == len(required_indexes)
    
    def verify_blob_data(self) -> Dict[str, int]:
        """Verify JSONL data availability in blob storage"""
        print(f"\n📊 VERIFYING BLOB DATA")
        print(f"   Container: {self.container}")
        print(f"   Prefix: {self.prefix}")
        
        container_client = self.blob_client.get_container_client(self.container)
        
        jsonl_files = {}
        expected_files = [
            'files.jsonl', 'paragraphs.jsonl', 'data_items.jsonl', 
            'xrefs.jsonl', 'calls.jsonl', 'procedure_facts.jsonl',
            'chunks.jsonl', 'flow_edges.jsonl', 'copybooks.jsonl'
        ]
        
        for blob in container_client.list_blobs(name_starts_with=self.prefix):
            if blob.name.endswith('.jsonl'):
                file_name = os.path.basename(blob.name)
                
                # Count records in JSONL file
                blob_client = container_client.get_blob_client(blob.name)
                content = blob_client.download_blob().readall().decode('utf-8')
                lines = [line for line in content.strip().split('\n') if line.strip()]
                jsonl_files[file_name] = len(lines)
        
        # Report findings
        for file_name in expected_files:
            count = jsonl_files.get(file_name, 0)
            status = "✅" if count > 0 else "❌"
            print(f"   {status} {file_name:<20}: {count:>4} records")
        
        return jsonl_files

def load_environment_variables() -> Dict[str, str]:
    """Load required environment variables"""
    required_vars = [
        'AZURE_SEARCH_ENDPOINT',
        'AZURE_SEARCH_KEY', 
        'AZURE_OPENAI_ENDPOINT',
        'AZURE_OPENAI_API_KEY',
        'AOAI_EMBED_DEPLOYMENT',
        'AZURE_STORAGE_CONNECTION_STRING',
        'BLOB_CONTAINER',
        'BLOB_PREFIX'
    ]
    
    # Try to load from local.settings.json first
    env_vars = {}
    settings_path = "local.settings.json"
    
    if os.path.exists(settings_path):
        try:
            with open(settings_path, 'r') as f:
                settings = json.load(f)
                values = settings.get("Values", {})
                
                # Map settings to environment variable names
                var_mapping = {
                    'AZURE_SEARCH_ENDPOINT': 'SEARCH_ENDPOINT',
                    'AZURE_SEARCH_KEY': 'SEARCH_KEY',
                    'AZURE_OPENAI_ENDPOINT': 'AZURE_OPENAI_ENDPOINT',
                    'AZURE_OPENAI_API_KEY': 'AZURE_OPENAI_KEY',
                    'AOAI_EMBED_DEPLOYMENT': 'AZURE_OPENAI_DEPLOYMENT',
                    'AZURE_STORAGE_CONNECTION_STRING': 'DEPLOYMENT_STORAGE_CONNECTION_STRING',
                    'BLOB_CONTAINER': 'BLOB_CONTAINER',
                    'BLOB_PREFIX': 'BLOB_PREFIX'
                }
                
                for env_var, settings_key in var_mapping.items():
                    if settings_key in values and values[settings_key]:
                        env_vars[env_var] = values[settings_key]
                        print(f"   ✅ Loaded {env_var} from local.settings.json")
        except Exception as e:
            print(f"   ⚠️ Warning: Could not read local.settings.json: {e}")
    
    # Override with actual environment variables if they exist
    for var in required_vars:
        env_value = os.getenv(var)
        if env_value:
            env_vars[var] = env_value
            print(f"   ✅ Loaded {var} from environment")
    
    # Set defaults if not specified
    if 'BLOB_CONTAINER' not in env_vars:
        env_vars['BLOB_CONTAINER'] = 'aisearch'
        print(f"   ✅ Using default BLOB_CONTAINER: aisearch")
    if 'BLOB_PREFIX' not in env_vars:
        env_vars['BLOB_PREFIX'] = 'S35-Source/JSONL/'
        print(f"   ✅ Using default BLOB_PREFIX: S35-Source/JSONL/")
    
    # Check for missing variables
    missing_vars = [var for var in required_vars if var not in env_vars or not env_vars[var]]
    
    if missing_vars:
        print(f"\n❌ Missing required environment variables:")
        for var in missing_vars:
            print(f"   {var}")
        print(f"\nAdd them to local.settings.json or set as environment variables.")
        return {}
    
    return env_vars

def main():
    """Main function - Create Azure Search resources for COBOL analysis"""
    parser = argparse.ArgumentParser(
        description="Create Azure Search indexes for COBOL analysis",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Environment Variables (load from local.settings.json or environment):
  AZURE_SEARCH_ENDPOINT      Azure Search service endpoint
  AZURE_SEARCH_KEY          Azure Search admin key  
  AZURE_OPENAI_ENDPOINT     Azure OpenAI service endpoint
  AZURE_OPENAI_API_KEY      Azure OpenAI API key
  AOAI_EMBED_DEPLOYMENT     Azure OpenAI embedding deployment name
  AZURE_STORAGE_CONNECTION_STRING  Azure Storage connection string
  BLOB_CONTAINER            Blob container name (default: 'aisearch')
  BLOB_PREFIX               Blob prefix for JSONL files (default: 'S35-Source/JSONL/')

Examples:
  python create_indexes.py                    # Create all data sources, skillsets, indexers, and indexes
  python create_indexes.py --dry-run          # Preview operations
  python create_indexes.py --no-delete        # Skip deletion of existing resources
  python create_indexes.py --data-sources-only  # Only create data sources
  python create_indexes.py --skillsets-only   # Only create skillsets
  python create_indexes.py --indexers-only    # Only create indexers
  python create_indexes.py --verify-only      # Only verify blob data availability
  python create_indexes.py --run-indexers     # Run indexers immediately after creation
        """
    )
    
    parser.add_argument('--dry-run', action='store_true',
                       help='Preview operations without making changes')
    parser.add_argument('--no-delete', action='store_true',
                       help='Skip deletion of existing indexes')
    parser.add_argument('--verify-only', action='store_true',
                       help='Only verify blob data availability')
    parser.add_argument('--demo', action='store_true',
                       help='Run helper functions demonstration')
    parser.add_argument('--data-sources-only', action='store_true',
                       help='Only create data sources, skip indexes')
    parser.add_argument('--skillsets-only', action='store_true',
                       help='Only create skillsets, skip data sources and indexes')
    parser.add_argument('--indexers-only', action='store_true',
                       help='Only create indexers, skip data sources, skillsets, and indexes')
    parser.add_argument('--run-indexers', action='store_true',
                       help='Run indexers immediately after creation')
    parser.add_argument('--all', action='store_true',
                       help='Run complete setup: verify, create data sources, skillsets, indexes, and indexers, then run indexers')
    
    args = parser.parse_args()
    
    # Handle demo mode
    if args.demo:
        demo_helper_functions()
        return
    
    print("🔍 AZURE SEARCH INDEX SETUP")
    print("=" * 60)
    
    # Check dependencies
    print("📦 Checking dependencies...")
    if not check_dependencies():
        sys.exit(1)
    print("   ✅ All dependencies available")
    
    # Load environment variables
    print(f"\n🔧 Loading configuration...")
    env_vars = load_environment_variables()
    if not env_vars:
        sys.exit(1)
    
    # Import dependencies
    deps = import_dependencies()
    
    # Initialize manager
    manager = SearchIndexManager(
        search_endpoint=env_vars['AZURE_SEARCH_ENDPOINT'],
        search_key=env_vars['AZURE_SEARCH_KEY'],
        openai_endpoint=env_vars['AZURE_OPENAI_ENDPOINT'],
        openai_key=env_vars['AZURE_OPENAI_API_KEY'],
        embed_deployment=env_vars['AOAI_EMBED_DEPLOYMENT'],
        storage_conn_str=env_vars['AZURE_STORAGE_CONNECTION_STRING'],
        container=env_vars['BLOB_CONTAINER'],
        prefix=env_vars['BLOB_PREFIX'],
        deps=deps
    )
    
    # Verify blob data
    blob_data = manager.verify_blob_data()
    if args.verify_only:
        return
    
    if not any(count > 0 for count in blob_data.values()):
        print("\n❌ No JSONL data found in blob storage. Run cobolparser-clean.py first.")
        sys.exit(1)
    
    # List existing indexes
    print(f"\n📋 EXISTING INDEXES")
    existing_indexes = manager.list_existing_indexes()
    if existing_indexes:
        for idx in existing_indexes:
            print(f"   📂 {idx}")
    else:
        print("   (none)")
    
    # List existing data sources
    print(f"\n📊 EXISTING DATA SOURCES")
    existing_data_sources = manager.list_existing_data_sources()
    if existing_data_sources:
        for ds in existing_data_sources:
            print(f"   🔗 {ds}")
    else:
        print("   (none)")
    
    # List existing skillsets
    print(f"\n🛠️  EXISTING SKILLSETS")
    existing_skillsets = manager.list_existing_skillsets()
    if existing_skillsets:
        for ss in existing_skillsets:
            print(f"   ⚙️ {ss}")
    else:
        print("   (none)")
    
    # List existing indexers
    print(f"\n🔄 EXISTING INDEXERS")
    existing_indexers = manager.list_existing_indexers()
    if existing_indexers:
        for idx in existing_indexers:
            print(f"   🔄 {idx}")
    else:
        print("   (none)")
    
    # Delete existing indexes, data sources, skillsets, and indexers if requested
    if not args.no_delete and (existing_indexes or existing_data_sources or existing_skillsets or existing_indexers):
        # Delete indexers first (they depend on data sources and skillsets)
        if existing_indexers:
            print(f"\n🗑️  DELETING EXISTING INDEXERS")
            cobol_indexers = [idx for idx in existing_indexers if idx.startswith('idx_')]
            for idx in cobol_indexers:
                manager.delete_indexer(idx, dry_run=args.dry_run)
        
        if existing_indexes:
            print(f"\n🗑️  DELETING EXISTING INDEXES")
            cobol_indexes = [idx for idx in existing_indexes if idx.startswith('cobol-') or idx.startswith('code-')]
            for idx in cobol_indexes:
                manager.delete_index(idx, dry_run=args.dry_run)
        
        if existing_skillsets:
            print(f"\n🗑️  DELETING EXISTING SKILLSETS")
            cobol_skillsets = [ss for ss in existing_skillsets if ss.startswith('sk_')]
            for ss in cobol_skillsets:
                manager.delete_skillset(ss, dry_run=args.dry_run)
        
        if existing_data_sources:
            print(f"\n🗑️  DELETING EXISTING DATA SOURCES")
            cobol_data_sources = [ds for ds in existing_data_sources if ds.startswith('ds_')]
            for ds in cobol_data_sources:
                manager.delete_data_source(ds, dry_run=args.dry_run)
    
    # Create data sources
    print(f"\n🔗 CREATING DATA SOURCES")
    data_sources_success = manager.create_data_sources(dry_run=args.dry_run)
    
    # Create skillsets
    print(f"\n🛠️  CREATING SKILLSETS")
    print(f"Creating embedding skillset...")
    skillset_name = manager.create_skillset_embeddings(dry_run=args.dry_run)
    skillsets_success = bool(skillset_name)
    
    # Handle --all flag (complete setup)
    if args.all:
        print(f"\n🎯 COMPLETE SETUP MODE (--all)")
        # Step 1: Create required indexes (code_chunks, symbols, xrefs, calls)
        required_indexes_success = manager.create_required_indexes(dry_run=args.dry_run, no_delete=args.no_delete)
        
        # Step 2: Create indexers for the required indexes
        print(f"\n🔄 CREATING INDEXERS")
        indexers_success = manager.create_indexers(dry_run=args.dry_run)
        
        # Step 3: Run indexers immediately
        run_indexers_success = manager.run_indexers_immediately(dry_run=args.dry_run)
        
        # Summary for --all mode
        overall_success = (data_sources_success and skillsets_success and 
                          required_indexes_success and indexers_success and 
                          run_indexers_success)
        
        if manager.has_http_errors():
            print(f"   ❌ HTTP errors occurred (see logs above)")
            overall_success = False
        
        if overall_success:
            print(f"\n✅ COMPLETE SETUP SUCCESSFUL!")
            print(f"   ✅ All Azure Search resources created and running")
            print(f"   🎯 Ready for COBOL code analysis and search!")
        else:
            print(f"\n❌ COMPLETE SETUP FAILED")
            print(f"   Check the errors above for details.")
            sys.exit(1)
        return
    
    if args.data_sources_only:
        print(f"\n🎯 DATA SOURCES ONLY MODE")
        if data_sources_success:
            print(f"   ✅ All data sources created successfully!")
        else:
            print(f"   ⚠️ Some data sources failed to create.")
        return
    
    if args.skillsets_only:
        print(f"\n🎯 SKILLSETS ONLY MODE")
        if skillsets_success:
            print(f"   ✅ Skillset created successfully: {skillset_name}")
        else:
            print(f"   ⚠️ Skillset failed to create.")
        return
    
    # Handle indexers-only mode
    if args.indexers_only:
        print(f"\n🎯 INDEXERS ONLY MODE")
        indexers_success = manager.create_indexers(dry_run=args.dry_run)
        
        # Optionally run indexers immediately
        if args.run_indexers and indexers_success:
            run_success = manager.run_indexers_immediately(dry_run=args.dry_run)
            if not run_success:
                print(f"   ⚠️ Some indexers failed to start.")
        
        # Check for HTTP errors and exit appropriately
        if manager.has_http_errors():
            print(f"   ❌ HTTP errors occurred during indexer operations")
            sys.exit(1)
        elif indexers_success:
            print(f"   ✅ All indexers created successfully!")
        else:
            print(f"   ⚠️ Some indexers failed to create.")
            sys.exit(1)
        return
    
    # =============================================================================
    # MAIN PIPELINE: Create all required resources in correct order
    # =============================================================================
    
    # Step 1: Create required indexes (code_chunks, symbols, xrefs, calls)
    required_indexes_success = manager.create_required_indexes(dry_run=args.dry_run, no_delete=args.no_delete)
    
    # Step 2: Create indexers for the required indexes
    print(f"\n🔄 CREATING INDEXERS")
    indexers_success = manager.create_indexers(dry_run=args.dry_run)
    
    # Step 3: Optionally run indexers immediately
    run_indexers_success = True
    if args.run_indexers and indexers_success:
        run_indexers_success = manager.run_indexers_immediately(dry_run=args.dry_run)
    
    # Summary and exit code handling
    print(f"\n🎯 FINAL SUMMARY")
    if args.dry_run:
        print(f"   [DRY-RUN] Would create 4/4 data sources")
        print(f"   [DRY-RUN] Would create 1/1 skillsets")
        print(f"   [DRY-RUN] Would create 4/4 required indexes")
        print(f"   [DRY-RUN] Would create 4/4 indexers")
        if args.run_indexers:
            print(f"   [DRY-RUN] Would run 4/4 indexers immediately")
        print(f"   ✅ Dry-run completed successfully!")
    else:
        print(f"   Data sources: {'✅' if data_sources_success else '❌'}")
        print(f"   Skillsets: {'✅' if skillsets_success else '❌'}")
        print(f"   Required indexes: {'✅' if required_indexes_success else '❌'}")
        print(f"   Indexers: {'✅' if indexers_success else '❌'}")
        if args.run_indexers:
            print(f"   Run indexers: {'✅' if run_indexers_success else '❌'}")
        
        # Check for overall success
        overall_success = (data_sources_success and skillsets_success and 
                          required_indexes_success and indexers_success and 
                          run_indexers_success)
        
        # Check for HTTP errors
        if manager.has_http_errors():
            print(f"   ❌ HTTP errors occurred (see logs above)")
            overall_success = False
        
        if overall_success:
            print(f"   ✅ All Azure Search resources created and configured successfully!")
            print(f"   🎯 Ready for COBOL code analysis and search!")
        else:
            print(f"   ❌ Some operations failed. Check the errors above.")
            sys.exit(1)

if __name__ == "__main__":
    main()
