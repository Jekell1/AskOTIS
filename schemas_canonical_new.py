"""Canonical Azure AI Search index schema definitions (NEW generation).

All new indexes are *prefixed* with ``new-`` to allow grouped blue/green side-by-side deployment
and easy bulk filtering / cleanup.

Usage:
  from create_new_indexes import create_or_update_all

Vector & semantic settings are shared where appropriate.
Adjust embedding dimension (VECTOR_DIMS) if your embedding model differs.
"""
from __future__ import annotations

VECTOR_DIMS = 3072  # Adjust if using different embedding size (updated after detecting actual embedding model dims)
VECTOR_ALGO_NAME = "hnswAlgo"
VECTOR_PROFILE_NAME = "hnswProfile"
SEMANTIC_CONFIG_NAME = "default"

# Shared vectorSearch configuration (single algorithm + profile modern syntax)
VECTOR_SEARCH = {
    "algorithms": [
        {
            "name": VECTOR_ALGO_NAME,
            "kind": "hnsw"
        }
    ],
    "profiles": [
        {"name": VECTOR_PROFILE_NAME, "algorithm": VECTOR_ALGO_NAME}
    ]
}

# Utility to build a standard semantic configuration referencing key content fields

def semantic_cfg(content_fields: list[str], title_field: str = "programId", keywords_fields: list[str] | None = None):
    return {
        "configurations": [
            {
                "name": SEMANTIC_CONFIG_NAME,
                "prioritizedFields": {
                    "titleField": {"fieldName": title_field},
                    "prioritizedContentFields": [{"fieldName": f} for f in content_fields],
                    "prioritizedKeywordsFields": [{"fieldName": k} for k in (keywords_fields or [])]
                }
            }
        ]
    }

# Field helpers

def str_field(name, **kw):
    base = {"name": name, "type": "Edm.String"}
    base.update(kw)
    return base

def int_field(name, **kw):
    base = {"name": name, "type": "Edm.Int32"}
    base.update(kw)
    return base

def bool_field(name, **kw):
    base = {"name": name, "type": "Edm.Boolean"}
    base.update(kw)
    return base

def collection_str_field(name, **kw):
    base = {"name": name, "type": "Collection(Edm.String)"}
    base.update(kw)
    return base

def vector_field(name):
    return {
        "name": name,
        "type": "Collection(Edm.Single)",
        "searchable": True,
        "retrievable": False,
        "stored": False,
        "dimensions": VECTOR_DIMS,
        "vectorSearchProfile": VECTOR_PROFILE_NAME
    }

# Index Definitions (each a dict) -------------------------------------------------

code_chunks_New = {
    "name": "new-code-chunks",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("path", filterable=True, facetable=True),
        str_field("language", filterable=True),
        str_field("section", filterable=True),
        str_field("paragraph", filterable=True, facetable=True),
        int_field("lineStart", filterable=True),
        int_field("lineEnd", filterable=True),
        collection_str_field("definedNames", filterable=True),
        collection_str_field("usedNames", filterable=True),
        collection_str_field("copybooksUsed", filterable=True),
        str_field("summary", searchable=True),
        # Short excerpt for semantic ranking (optional, populated at ingest time)
        str_field("contentShort", searchable=True, retrievable=True),
        # Full content: searchable + retrievable only
        str_field("content", searchable=True, retrievable=True),
        vector_field("contentVector"),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["summary", "contentShort"], title_field="programId", keywords_fields=["paragraph"])
}

# Full COBOL file index (primary source-of-truth for raw program & copybook bodies)
cobol_files_New = {
    "name": "new-cobol-files",
    "fields": [
        str_field("id", key=True, filterable=True),  # typically path-based unique id
        str_field("programId", filterable=True, facetable=True),
        str_field("kind", filterable=True, facetable=True),  # PROGRAM | COPYBOOK
        str_field("path", filterable=True, facetable=True),
        str_field("language", filterable=True),
        int_field("sloc", filterable=True),
        bool_field("hasSQL", filterable=True),
        bool_field("hasCICS", filterable=True),
        bool_field("hasScreens", filterable=True),
        collection_str_field("paragraphs", filterable=True, facetable=True),
        collection_str_field("definesNames", filterable=True),
        collection_str_field("referencesNames", filterable=True),
        collection_str_field("copybooksUsed", filterable=True, facetable=True),
        str_field("summary", searchable=True),
        # Short excerpt used for semantic ranking & faster snippets (populated at ingest time)
        str_field("contentShort", searchable=True, retrievable=True),
    # Full content: searchable (default analyzer), retrievable, not filter/facet/sort
    str_field("content", searchable=True, retrievable=True, filterable=False, facetable=False, sortable=False),
        vector_field("contentVector"),
        str_field("commit", filterable=True),
        str_field("updatedAt", filterable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    # Use summary + contentShort (short excerpt) for semantic; paragraphs & copybooksUsed remain keywords
    "semantic": semantic_cfg(["summary", "contentShort"], title_field="programId", keywords_fields=["paragraphs", "copybooksUsed"])
}

cobol_paragraphs_New = {
    "name": "new-cobol-paragraphs",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("path", filterable=True),
        str_field("paragraph", filterable=True, facetable=True),
        str_field("section", filterable=True),
        int_field("lineStart", filterable=True),
        int_field("lineEnd", filterable=True),
        str_field("summary", searchable=True),
        str_field("content", searchable=True),
        vector_field("contentVector"),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["summary", "content"], title_field="programId", keywords_fields=["paragraph"])
}

cobol_symbols_New = {
    "name": "new-cobol-symbols",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("name", searchable=True, filterable=True, facetable=True),
        str_field("kind", filterable=True, facetable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("definedInPath", filterable=True),
        int_field("definedAtLine", filterable=True),
        str_field("picture", filterable=True),
        str_field("usage", filterable=True),
        str_field("valueClause", filterable=True),
        str_field("doc", searchable=True),
        vector_field("contentVector"),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["doc"], title_field="name", keywords_fields=["programId", "kind"])
}

cobol_xrefs_New = {
    "name": "new-cobol-xrefs",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("name", filterable=True, facetable=True),
        str_field("op", filterable=True, facetable=True),
        str_field("rw", filterable=True, facetable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("path", filterable=True),
        str_field("paragraph", filterable=True, facetable=True),
        int_field("line", filterable=True),
        str_field("snippet", searchable=True),
        # Optional small vector (commented out initially)
        # vector_field("snippetVector"),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["snippet"], title_field="name", keywords_fields=["op", "rw", "programId"])
}

cobol_facts_New = {
    "name": "new-cobol-facts",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("kind", filterable=True, facetable=True),
        str_field("subtype", filterable=True, facetable=True),
        str_field("resource", filterable=True, facetable=True),
        str_field("path", filterable=True),
        str_field("paragraph", filterable=True),
        int_field("line", filterable=True),
        str_field("details", searchable=True),
        str_field("snippet", searchable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["details", "snippet"], title_field="programId", keywords_fields=["kind", "resource"])
}

cobol_calls_New = {
    "name": "new-cobol-calls",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("callerProgram", filterable=True, facetable=True),
        str_field("calleeProgram", filterable=True, facetable=True),
        str_field("path", filterable=True),
        int_field("line", filterable=True),
        str_field("paragraph", filterable=True),
        str_field("callKind", filterable=True, facetable=True),
        collection_str_field("argNames", filterable=True),
        str_field("snippet", searchable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["snippet"], title_field="callerProgram", keywords_fields=["calleeProgram", "callKind"])
}

cobol_flow_edges_New = {
    "name": "new-cobol-flow-edges",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("fromParagraph", filterable=True, facetable=True),
        str_field("toParagraph", filterable=True, facetable=True),
        str_field("edgeKind", filterable=True, facetable=True),
        str_field("path", filterable=True),
        int_field("line", filterable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg([], title_field="programId")
}

cobol_includes_New = {
    "name": "new-cobol-includes",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("copybook", filterable=True, facetable=True),
        str_field("path", filterable=True),
        int_field("line", filterable=True),
        str_field("snippet", searchable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["snippet"], title_field="programId", keywords_fields=["copybook"])
}

cobol_screens_New = {
    "name": "new-cobol-screens",
    "fields": [
        str_field("id", key=True, filterable=True),
        str_field("programId", filterable=True, facetable=True),
        str_field("mapset", filterable=True, facetable=True),
        str_field("map", filterable=True, facetable=True),
        collection_str_field("fields", filterable=True),
        str_field("path", filterable=True),
        str_field("paragraph", filterable=True),
        int_field("line", filterable=True),
        str_field("op", filterable=True, facetable=True),
        str_field("snippet", searchable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["snippet"], title_field="programId", keywords_fields=["mapset", "op"])
}

program_summaries_New = {
    "name": "new-program-summaries",
    "fields": [
        str_field("programId", key=True, filterable=True, facetable=True),
        str_field("summary", searchable=True),
        collection_str_field("calls", filterable=True),
        collection_str_field("tables", filterable=True),
        collection_str_field("files", filterable=True),
        collection_str_field("screens", filterable=True),
        str_field("updatedAt", filterable=True),
    ],
    "vectorSearch": VECTOR_SEARCH,
    "semantic": semantic_cfg(["summary"], title_field="programId", keywords_fields=["tables", "files", "screens"])
}

ALL_NEW_SCHEMAS = [
    code_chunks_New,
    cobol_files_New,
    cobol_paragraphs_New,
    cobol_symbols_New,
    cobol_xrefs_New,
    cobol_facts_New,
    cobol_calls_New,
    cobol_flow_edges_New,
    cobol_includes_New,
    cobol_screens_New,
    program_summaries_New,
]
