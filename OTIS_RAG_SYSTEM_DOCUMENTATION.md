# OTIS RAG System - Comprehensive Documentation

**Version:** 2.0  
**Last Updated:** January 2025  
**System:** OTIS COBOL Codebase Q&A System

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Azure Search Indexes](#azure-search-indexes)
3. [RAG Architecture](#rag-architecture)
4. [Component Breakdown](#component-breakdown)
5. [Query Processing Workflow](#query-processing-workflow)
6. [Dynamic Token Allocation](#dynamic-token-allocation)
7. [Limits and Constraints](#limits-and-constraints)
8. [Deployment Architecture](#deployment-architecture)
9. [Index Relationships](#index-relationships)
10. [Code Examples](#code-examples)

---

## 1. System Overview

The OTIS RAG (Retrieval-Augmented Generation) system is an intelligent Q&A chatbot designed to answer questions about a large COBOL codebase (~20,000 programs and copybooks). The system combines:

- **21 specialized Azure AI Search indexes** (~3.5M documents total)
- **Hybrid retrieval** (semantic vector search + lexical keyword search)
- **Intelligent query routing** (determines question type and intent)
- **Dynamic context allocation** (adapts retrieval and generation based on question complexity)
- **Conversation memory** (maintains context across 30 turns)
- **Azure OpenAI GPT-4o** for natural language generation

### Key Features

✅ **100% Code Coverage** - All 9,951 CBL and CPY files indexed  
✅ **Deterministic Retrieval** - Transaction-specific queries return filtered results  
✅ **Semantic Understanding** - Vector embeddings (3072-dim) for conceptual matching  
✅ **Flow Tracing** - Follows program call chains and UI navigation paths  
✅ **Real-time Streaming** - Progressive answer generation for better UX  
✅ **Production Deployment** - Azure Functions with Application Insights telemetry

---

## 2. Azure Search Indexes

The system uses **21 specialized indexes** totaling **~3.5 million documents** across **~25GB** of indexed data.

### Index Categories

#### **Code & Content (2 indexes)**

| Index Name | Purpose | Doc Count | Size | Key Fields | Vector Dim |
|------------|---------|-----------|------|------------|------------|
| `code-chunks` | Legacy code chunks | 167,000 | 3.8GB | `chunk_id`, `path`, `content`, `content_vector` | 3072 |
| `new_code_chunks` | New code chunks with deterministic retrieval | 180,000 | 3.7GB | `chunk_id`, `file_id`, `path`, `program_id`, `name`, `start_line`, `end_line`, `text`, `text_vector` | 3072 |

**Sample Document (new_code_chunks):**
```json
{
  "chunk_id": "LONPF2_2676_2700",
  "file_id": "LONPF2",
  "path": "cobol_src\\LP\\LONPF2.CBL",
  "program_id": "LONPF2",
  "name": "LONPF2.CBL",  // Filterable field - can use in filter="name eq 'LONPF2.CBL'"
  "start_line": 2676,    // Filterable field - can use in filter="start_line ge 2000"
  "end_line": 2700,      // Filterable field - can use in filter="end_line le 3000"
  "text": "      IF LP-TRCD = \"RP\" OR \"CP\"\n         PERFORM PROCESS-REPAYMENT\n      END-IF.",
  "text_vector": [0.023, -0.045, ...],  // 3072 dimensions - searchable vector field
  "has_vector": true
}
```

**Usage in RAG:**
- Primary source for code retrieval
- Semantic search on `text_vector` + keyword search on `text`
- Line-range filtering for transaction-specific queries (uses filterable fields: `name`, `start_line`, `end_line`)
- Supports deterministic copybook retrieval

---

#### **Core Metadata (5 indexes)**

| Index Name | Purpose | Doc Count | Size | Vector Dim |
|------------|---------|-----------|------|------------|
| `new-cobol-files` | File metadata | 10,000 | 100MB | None |
| `new_cobol_program_meta` | Program summaries & metrics | 9,700 | 50MB | 3072 |
| `new_cobol_paragraphs` | Paragraph details | 225,000 | 500MB | None |
| `new_cobol_data_items` | Data structure definitions | 536,000 | 1.2GB | None |
| `new_cobol_copybook_meta` | Copybook summaries | 7,800 | 25MB | 3072 |

**Sample Document (new_cobol_program_meta):**
```json
{
  "program_id": "LONPF2",
  "program_name": "LONPF2",
  "file_paths_json": "[\"cobol_src/LP/LONPF2.CBL\"]",
  "program_summary": "Loan payment processing program. Handles RP (repayment) and CP (charge payment) transactions. Calls 15 external programs including GB01SE, LP01CH. Has 288 copybook dependencies. Depth score: 5.",
  "program_summary_vector": [0.012, -0.034, ...],  // 3072 dimensions
  "program_role": "DATA_ACCESS",
  "ui_flag": false,
  "outgoing_count": 15,
  "incoming_count": 8,
  "has_cycles": false,
  "call_depth_score": 5,
  "reach_out_size": 47,
  "reach_in_size": 12,
  "has_vector": true,
  "updated_at": "2025-01-15T10:30:00Z"
}
```

**Usage in RAG:**
- Semantic search on `program_summary_vector` for conceptual queries
- Provides program classification and complexity metrics
- Used for enriching answers with metadata

---

**Sample Document (new_cobol_copybook_meta):**
```json
{
  "copybook_name": "LPFSREPAYTRP",
  "file_paths_json": "[\"cobol_src/LIBLP/LPFSREPAYTRP.CPY\"]",
  "summary": "Repayment transaction processing data structures. Defines fields for payment amount, date, transaction code, and account balances.",
  "summary_vector": [0.045, -0.023, ...],  // 3072 dimensions
  "include_count": 12,
  "program_like": false,
  "lines_total": 89,
  "lines_non_comment": 67,
  "data_division_present": true,
  "working_storage_items": 23,
  "has_vector": true,
  "updated_at": "2025-01-15T09:00:00Z"
}
```

**Usage in RAG:**
- Semantic search for copybook discovery
- Hybrid approach: find transaction code in programs, then semantic search on code to find relevant copybooks
- Provides structural metadata for copybook analysis

---

#### **Relationships & Usage (5 indexes)**

| Index Name | Purpose | Doc Count | Size | Vector Dim |
|------------|---------|-----------|------|------------|
| `new_cobol_calls` | Program call edges | 16,000 | 75MB | 3072 (snippet) |
| `new_cobol_variable_usage` | Variable usage statistics | 107,000 | 250MB | None |
| `new_cobol_copybook_usage` | Copybook inclusion points | 115,000 | 300MB | 3072 (context) |
| `new_cobol_symbol_refs` | Symbol references | 1,900,000 | 4.5GB | None |
| `new_cobol_name_aliases` | Name aliases & synonyms | 56,000 | 100MB | None |

**Sample Document (new_cobol_calls):**
```json
{
  "call_id": "LONPF2_2890_GB01SE",
  "caller_program": "LONPF2",
  "callee_program": "GB01SE",
  "line_number": 2890,
  "call_snippet": "CALL 'GB01SE' USING WS-ACCOUNT-RECORD",
  "snippet_vector": [0.034, -0.012, ...],  // 3072 dimensions
  "call_context": "PROCEDURE DIVISION",
  "has_vector": true
}
```

**Sample Document (new_cobol_copybook_usage):**
```json
{
  "usage_id": "LONPF2_897_LPFSWK",
  "program_id": "LONPF2",
  "program_name": "LONPF2",
  "copybook_name": "LIBLP/LPFSWK.CPY",
  "copybook_name_plain": "LPFSWK",
  "section": "WORKING-STORAGE",
  "line_number": 897,
  "raw_copy_line": "       COPY LPFSWK.",
  "context_snippet": "WORKING-STORAGE SECTION.\n       COPY LPFSWK.\n01  WS-VARIABLES.",
  "context_vector": [0.023, -0.056, ...],  // 3072 dimensions
  "has_vector": true
}
```

**Usage in RAG:**
- Deterministic retrieval: filter by `program_id` and `line_number` ranges
- Semantic fallback: vector search on `context_vector` when line-based retrieval fails
- Builds dependency graphs and call chains

---

#### **Flow & Navigation (5 indexes)**

| Index Name | Purpose | Doc Count | Size | Vector Dim |
|------------|---------|-----------|------|------------|
| `new_cobol_program_flows` | Program flow diagrams | 9,700 | 100MB | 3072 |
| `new_cobol_flow_edges_v2` | Detailed flow edges | 385,000 | 1.8GB | 3072 |
| `new_cobol_ui_paths` | UI navigation paths | 2,000 | 25MB | 3072 |
| `new_cobol_menu_trees` | Menu structures | 9,700 | 75MB | None |
| `new_cobol_screen_nodes` | Screen definitions | 1,600 | 15MB | 1536 |

**Sample Document (new_cobol_program_flows):**
```json
{
  "program_id": "LONPF2",
  "mermaid_flow": "graph TD\n  LONPF2[LONPF2]\n  LONPF2 --> GB01SE[GB01SE]\n  LONPF2 --> LP01CH[LP01CH]\n  ...",
  "flow_summary": "LONPF2 orchestrates loan payment processing with 15 downstream calls. Main flow: validate input -> process transaction -> update records -> generate response.",
  "flow_vector": [0.067, -0.023, ...],  // 3072 dimensions
  "node_count": 18,
  "edge_count": 22,
  "has_vector": true,
  "updated_at": "2025-01-14T15:00:00Z"
}
```

**Sample Document (new_cobol_ui_paths):**
```json
{
  "path_id": "UI_PATH_LOAN_001",
  "path_name": "Loan Payment Entry Flow",
  "start_program": "MAINMENU",
  "end_program": "LONPF2",
  "path_sequence": "MAINMENU -> LOANMENU -> LONPF2",
  "path_description": "User selects 'Loans' from main menu, then 'Process Payment', navigates to LONPF2 for repayment transaction entry.",
  "path_vector": [0.045, -0.089, ...],  // 3072 dimensions
  "hop_count": 3,
  "screen_count": 3,
  "has_vector": true
}
```

**Sample Document (new_cobol_screen_nodes):**
```json
{
  "screen_id": "LONPF2_SCREEN_MAIN",
  "program_id": "LONPF2",
  "screen_name": "LOAN-PAYMENT-SCREEN",
  "fields_json": "[{\"name\":\"ACCOUNT-NO\",\"pic\":\"9(10)\",\"line\":5},{\"name\":\"PAYMENT-AMT\",\"pic\":\"9(7)V99\",\"line\":7}]",
  "actions_json": "[{\"key\":\"F3\",\"action\":\"EXIT\"},{\"key\":\"ENTER\",\"action\":\"SUBMIT\"}]",
  "transitions_json": "[{\"target\":\"GB01SE\",\"condition\":\"VALID-ACCOUNT\"}]",
  "summary_text": "Loan payment entry screen with account number, payment amount, and transaction code fields.",
  "summary_vector": [0.034, -0.012, ...],  // 1536 dimensions (different from other indexes!)
  "field_count": 2,  // Matches the 2 fields in fields_json array
  "action_count": 2,
  "has_vector": true
}
```

**⚠️ Important:** `new_cobol_screen_nodes` uses **1536-dimension vectors** (text-embedding-ada-002) instead of 3072 dimensions. The retriever handles this automatically.

**Usage in RAG:**
- Flow tracing queries retrieve from `program_flows` and `flow_edges_v2`
- UI/menu questions boost `screen_nodes` with 3.0x weight
- Navigation path queries use `ui_paths` for end-to-end journey visualization

---

#### **Dependencies (2 indexes)**

| Index Name | Purpose | Doc Count | Size | Vector Dim |
|------------|---------|-----------|------|------------|
| `new_cobol_program_deps` | Aggregated program dependencies | 9,700 | 50MB | None |
| `enhanced_cobol_dependencies` | Enhanced dependencies with externals | 9,700 | 60MB | None |

**Sample Document (new_cobol_program_deps):**
```json
{
  "program_id": "LONPF2",
  "program_role": "DATA_ACCESS",
  "outgoing_programs_json": "[\"GB01SE\",\"LP01CH\",\"LP01TRP\",\"LPWSCH\",...]",
  "incoming_programs_json": "[\"LOANMENU\",\"APILN01\",\"LONPB0\",...]",
  "external_programs_json": "[\"CICS-API\",\"DB2-CALL\"]",
  "copybooks_used_json": "[\"LPFSWK\",\"LPFSREPAYTRP\",\"GB01BR\",...]",
  "screens_touched_json": "[\"LOAN-PAYMENT-SCREEN\",\"LOAN-CONFIRM-SCREEN\"]",
  "outgoing_count": 15,
  "incoming_count": 8,
  "copybook_count": 288,
  "external_count": 2,
  "screens_count": 2,
  "has_outgoing": true,
  "has_incoming": true,
  "dependency_blob": "LONPF2 calls GB01SE LP01CH LP01TRP and uses copybooks LPFSWK LPFSREPAYTRP",
  "updated_at": "2025-01-15T11:00:00Z"
}
```

**Usage in RAG:**
- One-shot dependency lookups: "What does LONPF2 call?"
- Reverse dependencies: "What calls LONPF2?"
- Copybook usage aggregation
- External interface detection

---

#### **Phase 2: Business Context (2 indexes)**

| Index Name | Purpose | Doc Count | Size | Vector Dim |
|------------|---------|-----------|------|------------|
| `transaction_taxonomy` | Transaction codes & workflows | 51 | 5MB | 3072 |
| `program_complexity` | Complexity metrics | 1,736 | 20MB | None |

**Sample Document (transaction_taxonomy):**
```json
{
  "transaction_code": "RP",
  "transaction_name": "Repayment Transaction",
  "description": "Processes loan repayments including principal and interest calculations. Updates account balance and generates receipt.",
  "entry_programs": ["LONPF2", "APILN01"],
  "business_rules": "Validates account status, checks payment amount against balance, applies payment priority rules, updates history.",
  "related_screens": ["LOAN-PAYMENT-SCREEN"],
  "description_vector": [0.056, -0.034, ...],  // 3072 dimensions
  "has_vector": true
}
```

**Sample Document (program_complexity):**
```json
{
  "program_id": "LONPF2",
  "lines_of_code": 9881,
  "cyclomatic_complexity": 187,
  "nesting_depth": 8,
  "call_fan_out": 15,
  "call_fan_in": 8,
  "copybook_count": 288,
  "paragraph_count": 45,
  "complexity_score": 8.7,  // Scale: 0-10 (0=simple, 10=very complex)
  "risk_level": "HIGH",  // Based on complexity_score and other metrics
  "maintainability_index": 42.3  // Scale: 0-100 (>85=High, 65-85=Medium, <65=Low maintainability)
}
```

**Usage in RAG:**
- Transaction workflow queries
- Complexity analysis and risk assessment
- Business rule extraction

---

### Index Size Summary

**Total System Stats:**
- **Total Documents:** ~3.65 million
  - Code chunks: 251K (167K legacy + 84K new)
  - Metadata: 788K (files, programs, paragraphs, data_items, copybooks)
  - Relationships: 2.19M (calls, variables, copybook_usage, symbol_refs, name_aliases)
  - Flow/Navigation: 408K (flows, flow_edges_v2, ui_paths, menu_trees, screen_nodes)
  - Dependencies & Complexity: ~13K (program_deps, enhanced_deps, transactions, complexity)
- **Total Size:** ~7.5 GB (excluding embeddings)
  - Code chunks: ~7.5 GB (3.8GB legacy + 3.7GB new)
  - Other indexes: Primarily metadata (minimal storage)
  - Vector embeddings: Additional storage (not included in size estimate)
- **Vector Dimensions:** 3072 (most indexes), 1536 (screen_nodes only)
- **Embedding Model:** text-embedding-3-large (Azure OpenAI)
- **Search API Version:** 2025-08-01-preview

**⚠️ Note:** Document counts are approximate and based on config.py comments. Actual totals may vary as data is continuously updated.

---

## 3. RAG Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         User Query                               │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                    1. QUERY ROUTER                               │
│  • Detects question type (menu, trace_flow, explain, etc.)      │
│  • Determines if OTIS-specific or general COBOL                  │
│  • Selects appropriate indexes to search                        │
│  • Applies index weights (e.g., 3.0x for screen_nodes)          │
│  • Extracts program names, transaction codes                    │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                   2. HYBRID RETRIEVER                            │
│  • Generates query embedding (3072-dim or 1536-dim)             │
│  • Semantic vector search across selected indexes               │
│  • Keyword search (lexical matching)                            │
│  • Deterministic handling for transaction copybooks             │
│  • Filters & deduplicates results                               │
│  • Ranks by relevance + applied weights                         │
│  • Returns top-K documents (dynamic: 20-200 per index)          │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                  3. CONVERSATION MEMORY                          │
│  • Retrieves last 3 conversation turns                          │
│  • Provides context for follow-up questions                     │
│  • Maintains up to 30 turns in history                          │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                  4. RESPONSE GENERATOR                           │
│  • Builds comprehensive prompt with context                     │
│  • Calls Azure OpenAI GPT-4o (gpt-4o)                          │
│  • Dynamic max_tokens: 2000-15000 based on question type       │
│  • Temperature: 0.1 (factual responses)                         │
│  • Post-processes response (adds file hyperlinks)               │
│  • Returns formatted answer with citations                      │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                         Answer                                   │
│  • Natural language response with citations                     │
│  • Embedded file links: [[FILE:LONPF2.CBL|LONPF2.CBL]]        │
│  • Timing breakdown (route, retrieve, memory, generate)        │
└─────────────────────────────────────────────────────────────────┘
```

---

## 4. Component Breakdown

### 4.1 Config (`otis_rag/config.py`)

**Purpose:** Central configuration management

**Key Attributes:**
```python
class Config:
    # Azure Search
    search_endpoint: str
    search_key: str
    
    # Azure OpenAI
    openai_endpoint: str
    openai_key: str
    chat_deployment: str = "gpt-4o"
    embed_deployment: str = "text-embedding-3-large"
    
    # Index mappings (21 indexes total)
    indexes: Dict[str, str] = {
        'code': 'code-chunks',
        'code_new': 'new_code_chunks',
        'programs': 'new_cobol_program_meta',
        # ... 18 more indexes (truncated for brevity)
        # Full list includes: copybooks, calls, program_deps, program_flows,
        # flow_edges_v2, screen_nodes, ui_paths, data_items, variable_usage, etc.
    }
    
    # RAG Parameters
    max_results_per_index: int = 50  # Default top-K
    max_context_length: int = 32000  # Token limit for context
    temperature: float = 0.1  # Low for factual responses
    max_conversation_turns: int = 30
```

**Environment Variables:**
- `AZURE_SEARCH_ENDPOINT`
- `AZURE_SEARCH_KEY`
- `AZURE_OPENAI_ENDPOINT`
- `AZURE_OPENAI_KEY`
- `AZURE_OPENAI_DEPLOYMENT`
- `AZURE_OPENAI_EMBED_DEPLOYMENT`

---

### 4.2 Query Router (`otis_rag/router.py`)

**Purpose:** Intelligent query routing and index selection

**Question Types Detected:**
1. **`menu`** - Menu/screen text questions (boosts `screen_nodes` 3.0x)
2. **`explain_program`** - Program explanation queries
3. **`find_code`** - Code search queries
4. **`explain_data`** - Data structure questions
5. **`transaction_copybooks`** - Transaction-specific copybook queries (deterministic)
6. **`trace_flow`** - Flow tracing and call chains
7. **`transaction`** - Business transaction workflows
8. **`complexity`** - Complexity analysis
9. **`list`** - List/inventory requests
10. **`general`** - Default/catchall

**Key Methods:**
```python
def route(query: str) -> dict:
    """
    Returns:
    {
        'is_otis': bool,
        'question_type': str,
        'search_indexes': List[str],
        'clean_query': str,
        'has_menu_intent': bool,
        'index_weights': Dict[str, float]  # e.g., {'screen_nodes': 3.0}
    }
    """
```

**Index Selection Logic:**
- **Menu questions:** `['screen_nodes', 'ui_paths', 'menu_trees', 'programs']`
- **Trace flow:** `['calls', 'flows', 'flow_edges', 'programs', 'ui_paths']`
- **Transaction copybooks:** `['copybook_usage', 'copybooks', 'code_new']` (deterministic mode)
- **General:** `['code_new', 'programs', 'paragraphs', 'data_items', 'copybooks']`

**Weight Application:**
- Menu/UI questions apply `screen_nodes: 3.0` weight to boost screen content
- Other question types use default `1.0` weight

---

### 4.3 Hybrid Retriever (`otis_rag/retriever.py`)

**Purpose:** Multi-index hybrid search with semantic + lexical retrieval

**Key Features:**
1. **Dual embedding support:** 3072-dim (default) + 1536-dim (screen_nodes)
2. **Deterministic mode:** Transaction copybook queries use filtered line-range lookup
3. **Semantic fallback:** If deterministic returns 0, falls back to vector search
4. **Score boosting:** Applies index weights to search scores
5. **Deduplication:** Removes duplicate documents across indexes

**Core Method:**
```python
def retrieve(
    query: str,
    indexes: List[str],
    max_results: int = 5,
    index_weights: Dict[str, float] = None,
    question_type: str = None
) -> List[Dict[str, Any]]:
    """
    Returns list of documents with metadata:
    {
        '@search.score': float,      # Relevance score (boosted by weight)
        '_index_type': str,           # Source index
        '_weight_applied': float,     # Weight used
        '_is_filtered_relationship': bool,  # If deterministic filtering applied
        # ... index-specific fields
    }
    """
```

**Deterministic Transaction Copybook Retrieval:**
```python
def _handle_transaction_copybooks(query: str, max_results: int):
    # Extract program name and transaction code from query
    program_name = "LONPF2"
    transaction_code = "RP"
    
    # 1. Get all code chunks for program
    # Note: 'name' field must be filterable in index schema for filter syntax to work
    code_chunks = search(index='new_code_chunks', filter=f"name eq '{program_name}.CBL'")
    
    # 2. Find RP entry points in code
    rp_chunks = [c for c in code_chunks if "IF LP-TRCD = \"RP\"" in c['text']]
    
    # 3. Build ±100 line ranges around entry points
    line_ranges = [(chunk['start_line']-100, chunk['end_line']+100) for chunk in rp_chunks]
    
    # 4. Query copybook_usage for those line ranges
    copybooks = search(
        filter=f"program_id eq '{program_name}' and (line_number ge {start} and line_number le {end})"
    )
    
    # 5. If 0 results, fall back to semantic search on RP code
    if not copybooks:
        tx_code_text = "\n\n".join([c['text'] for c in rp_chunks])
        embedding = generate_embedding(tx_code_text)
        
        # Semantic search on copybook_meta (global index, no program filter)
        # Copybooks are shared across programs, rely on semantic similarity
        copybooks = semantic_search(copybook_meta, embedding, top=50)
    
    return copybooks
```

**Search Timing:**
- Embedding generation: ~0.2-0.5s (3072-dim), ~0.1-0.3s (1536-dim)
- Index search: ~0.1-0.5s per index
- Total retrieval: ~1-3s for typical queries

---

### 4.4 Conversation Memory (`otis_rag/memory.py`)

**Purpose:** Maintain conversation context for follow-up questions

**Features:**
- Stores last 30 conversation turns **in-memory per instance**
- Each turn includes: `user_query`, `assistant_response`, `metadata`
- Provides last N turns as context string
- Thread-safe for concurrent requests within same instance

**⚠️ Important Limitations:**
- **Stateless in Azure Functions:** Memory resets when instance is recycled (cold start)
- **No persistence:** Conversation history is **not** stored in external storage (Azure Table, CosmosDB, etc.)
- **Per-instance only:** Each Azure Functions instance has separate memory
- **Session scope:** Effective for single-session conversations, not multi-session user history
- **Production consideration:** For persistent conversation history, implement external storage (e.g., Redis, CosmosDB with session ID)

**Usage:**
```python
memory = ConversationMemory(max_turns=30)

# Add turn
memory.add_turn(
    user_query="What does LONPF2 do?",
    assistant_response="LONPF2 processes loan payments...",
    metadata={'routing': {...}, 'num_docs': 45}
)

# Get context
context = memory.get_context(last_n=3)
# Returns formatted string of last 3 exchanges
```

---

### 4.5 Response Generator (`otis_rag/generator.py`)

**Purpose:** Generate natural language responses using LLM

**Key Features:**
1. **Dynamic token allocation** based on question type
2. **Enhanced system prompts** for OTIS-specific vs general COBOL
3. **Hyperlink injection** for file references
4. **Citation formatting** with source references

**Core Method:**
```python
def generate(
    query: str,
    context_docs: List[Dict],
    conversation_context: str = "",
    is_otis: bool = False,
    question_type: str = "general"
) -> str:
    # Determine max_tokens based on question type
    if question_type in ('menu', 'list', 'simple'):
        max_tokens = 2000  # Short answers
    elif question_type == 'trace_flow':
        max_tokens = 15000  # Detailed flow analysis
    else:
        max_tokens = 4000  # Default
    
    # Build prompt with context
    prompt = build_prompt(query, context_docs, conversation_context)
    
    # Calculate prompt length and enforce model limits
    # GPT-4o: 128k context window, 16,384 max output tokens
    prompt_tokens = estimate_tokens(prompt + system_prompt)
    max_output_limit = 16384  # GPT-4o hard limit
    available_tokens = 128000 - prompt_tokens  # Total - used
    
    # Guard against exceeding limits
    max_tokens = min(max_tokens, max_output_limit, available_tokens - 1000)  # -1000 buffer
    
    # Call GPT-4o
    response = openai_client.chat.completions.create(
        model="gpt-4o",
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": prompt}
        ],
        temperature=0.1,
        max_tokens=max_tokens
    )
    
    # Post-process: add file hyperlinks
    result = add_file_hyperlinks(response.choices[0].message.content)
    return result
```

**Hyperlink Injection:**
```python
def add_file_hyperlinks(text: str) -> str:
    # Pattern 1: Files with extensions
    text = re.sub(r'\b([A-Z][A-Z0-9_-]{0,30}\.(?:CBL|CPY))\b', 
                  r'[[FILE:\1|\1]]', text)
    
    # Pattern 2: Bare program names in citations
    text = re.sub(r'(Source:\s*)([A-Z][A-Z0-9-]{0,7})', 
                  r'\1[[FILE:\2|\2]]', text)
    
    return text
```

**Example Output:**
```
LONPF2 processes loan repayments...

(Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Index: new_cobol_program_meta)
```

Frontend converts `[[FILE:LONPF2.CBL|LONPF2.CBL]]` to clickable link.

---

### 4.6 Main RAG Orchestrator (`otis_rag/rag.py`)

**Purpose:** Coordinates all components

**Simple Interface:**
```python
from otis_rag.rag import OTISRAG

rag = OTISRAG()
answer = rag.ask("What does LONPF2 do?")
```

**With Timing:**
```python
result = rag.ask_with_timing("What does LONPF2 do?", verbose=True)
print(result['answer'])
print(result['timing'])  # Breakdown by component
```

---

## 5. Query Processing Workflow

### 5.1 End-to-End Example

**User Query:** *"List all copybooks used in LONPF2 when handling RP transactions?"*

**Step 1: Routing (0.05s)**
```python
routing = {
    'is_otis': True,
    'question_type': 'transaction_copybooks',  # DETERMINISTIC MODE
    'search_indexes': ['copybook_usage', 'copybooks', 'code_new'],
    'clean_query': 'copybooks used LONPF2 RP transactions',
    'index_weights': {}
}
```

**Step 2: Retrieval (2.3s)**

*2a. Generate Embedding (0.4s)*
```python
query_vector = generate_embedding("copybooks used LONPF2 RP transactions")
# Returns 3072-dimension vector
```

*2b. Deterministic Handler (1.5s)*
```python
# Extract: program_name="LONPF2", transaction_code="RP"

# Get code chunks for LONPF2
# Note: 'name' is filterable field in new_code_chunks index schema
code_chunks = search(index='new_code_chunks', filter="name eq 'LONPF2.CBL'")
# Returns 392 chunks

# Find RP entry points
rp_chunks = [c for c in code_chunks if 'IF LP-TRCD = "RP"' in c['text']]
# Found 4 chunks at lines 2676, 2701, 5751, 9526

# Build line ranges (±100 lines)
line_ranges = [
    (2576, 2800),
    (2601, 2825),
    (5651, 5875),
    (9426, 9650)
]

# Query copybook_usage for line ranges
copybooks = search(
    index='new_cobol_copybook_usage',
    filter="program_id eq 'LONPF2' and ((line_number ge 2576 and line_number le 2800) or ...)"
)
# Returns 0 results (copybooks are in DATA DIVISION, not near RP code)

# FALLBACK: Semantic search on RP code
rp_code_text = "\n\n".join([c['text'] for c in rp_chunks])
rp_embedding = generate_embedding(rp_code_text[:8000])

copybooks = search(
    index='new_cobol_copybook_meta',
    vector=rp_embedding,
    filter="program_id eq 'LONPF2'",
    top=50
)
# Returns 35 semantically relevant copybooks
```

*2c. Search Other Indexes (0.4s)*
```python
# Search new_code_chunks for additional context
code_results = hybrid_search(index='new_code_chunks', query_vector, top=50)
# Returns 50 code chunks
```

**Step 3: Memory Context (0.01s)**
```python
conv_context = memory.get_context(last_n=3)
# Returns last 3 conversation turns (if any)
```

**Step 4: Generation (3.2s)**

*4a. Build Prompt*
```python
prompt = f"""
Answer this question: {query}

Context:
{format_context_docs(copybooks + code_results)}

Conversation History:
{conv_context}
"""
```

*4b. Call GPT-4o*
```python
response = openai.chat.completions.create(
    model="gpt-4o",
    messages=[
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": prompt}
    ],
    temperature=0.1,
    max_tokens=4000  # Default for transaction_copybooks type
)
```

*4c. Post-process Response*
```python
answer = add_file_hyperlinks(response.choices[0].message.content)
```

**Step 5: Store in Memory**
```python
memory.add_turn(
    user_query=query,
    assistant_response=answer,
    metadata={'question_type': 'transaction_copybooks', 'num_docs': 85}
)
```

**Final Response (Total: 5.56s)**
```
When handling RP (repayment) transactions, LONPF2 uses the following copybooks:

**Transaction Processing:**
- [[FILE:LPFSREPAYTRP.CPY|LPFSREPAYTRP.CPY]] - Repayment transaction structures
- [[FILE:LPFDREPAYTRP.CPY|LPFDREPAYTRP.CPY]] - Repayment data definitions

**Account Management:**
- [[FILE:GB01BR.CPY|GB01BR.CPY]] - Account balance record
- [[FILE:LP01CH.CPY|LP01CH.CPY]] - Charge history

**Working Storage:**
- [[FILE:LPFSWK.CPY|LPFSWK.CPY]] - Working storage variables

(Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Index: new_cobol_copybook_meta, new_code_chunks)

Timing: Total=5.56s (Route=0.05s, Retrieve=2.3s, Memory=0.01s, Generate=3.2s)
```

---

### 5.2 Workflow Diagram

```
User Query
    │
    ▼
┌─────────────────────────┐
│  1. ROUTER (0.05s)      │
│  • Detect type          │
│  • Select indexes       │
│  • Apply weights        │
└─────────┬───────────────┘
          │
          ▼
┌─────────────────────────┐
│  2a. Embedding (0.4s)   │
│  • Generate 3072-dim    │
│  • (1536-dim if screens)│
└─────────┬───────────────┘
          │
          ├─────────────────┬────────────────┐
          │                 │                │
          ▼                 ▼                ▼
┌──────────────────┐ ┌──────────────┐ ┌─────────────┐
│ 2b. Deterministic│ │ 2c. Semantic │ │ 2d. Keyword │
│ (if tx_copybooks)│ │ Vector Search│ │ Search      │
│ (1.5s)           │ │ (0.5s/index) │ │ (0.2s/index)│
└──────────┬───────┘ └──────┬───────┘ └──────┬──────┘
          │                 │                │
          └─────────────────┴────────────────┘
                          │
                          ▼
                ┌──────────────────┐
                │ 2e. Dedupe & Rank│
                │ (0.2s)            │
                └─────────┬─────────┘
                          │
                          ▼
┌───────────────────────────────────────────────┐
│  3. MEMORY (0.01s)                            │
│  • Retrieve last 3 turns                      │
└─────────┬─────────────────────────────────────┘
          │
          ▼
┌───────────────────────────────────────────────┐
│  4a. Build Prompt (0.1s)                      │
│  • Format context docs                        │
│  • Add conversation history                   │
└─────────┬─────────────────────────────────────┘
          │
          ▼
┌───────────────────────────────────────────────┐
│  4b. GPT-4 Generation (3.0s)                  │
│  • max_tokens: 2000-15000 (dynamic)           │
│  • temperature: 0.1                           │
└─────────┬─────────────────────────────────────┘
          │
          ▼
┌───────────────────────────────────────────────┐
│  4c. Post-process (0.1s)                      │
│  • Add hyperlinks                             │
│  • Format citations                           │
└─────────┬─────────────────────────────────────┘
          │
          ▼
┌───────────────────────────────────────────────┐
│  5. Store in Memory (0.01s)                   │
└─────────┬─────────────────────────────────────┘
          │
          ▼
       Answer
```

---

## 6. Dynamic Token Allocation

The system intelligently allocates tokens based on question complexity to optimize performance and cost.

### 6.1 Retrieval Limits (max_results per index)

| Question Type | Max Results | Rationale |
|---------------|-------------|-----------|
| `menu`, `list`, `simple` | 20 | Specific questions need fewer docs |
| `trace_flow` | 100 | Flow tracing needs comprehensive context |
| `transaction` | 50 | Business workflows need moderate context |
| Explicit "list all" | 200 | User wants comprehensive results |
| `general` (default) | 50 | Balanced approach |

**Code:**
```python
if question_type in ('menu', 'list', 'simple'):
    max_results_for_query = 20
elif question_type == 'trace_flow':
    max_results_for_query = 100
elif 'all' in query.lower() or 'list' in query.lower():
    max_results_for_query = 200
else:
    max_results_for_query = 50  # Default
```

---

### 6.2 Generation Limits (max_tokens for LLM response)

| Question Type | Target Max Tokens | Approx Words | Rationale |
|---------------|-------------------|--------------|-----------|
| `menu`, `list`, `simple` | 2,000 | ~1,500 | Short, focused answers |
| `trace_flow` | 15,000 | ~11,000 | Detailed flow analysis with diagrams |
| `general` (default) | 4,000 | ~3,000 | Standard explanations |

**⚠️ Model Constraints:**
- **GPT-4o Output Limit:** 16,384 tokens (hard cap)
- **Context Window:** 128,000 tokens total (input + output)
- **Guard Logic:** `min(target_tokens, 16384, 128000 - prompt_tokens - 1000)`
- **Safety Buffer:** 1,000 tokens reserved to prevent edge cases

**Code:**
```python
if question_type in ('menu', 'list', 'simple'):
    max_response_tokens = 2000
elif question_type == 'trace_flow':
    max_response_tokens = 15000  # Target, subject to runtime limits
else:
    max_response_tokens = 4000  # Default

# Enforce model constraints
prompt_tokens = estimate_tokens(prompt + system_prompt)
max_response_tokens = min(
    max_response_tokens,        # Target based on question type
    16384,                       # GPT-4o output limit
    128000 - prompt_tokens - 1000  # Available in context window
)
```

**Real-World Scenarios:**
- **Small context (5k prompt):** trace_flow gets full 15,000 tokens
- **Large context (100k prompt):** trace_flow limited to ~27,000 tokens (128k - 100k - 1k buffer)
- **Very large context (112k prompt):** trace_flow limited to ~15,000 tokens (128k - 112k - 1k)

**GPT-4o Context Window:** 128,000 tokens total
- **Typical prompt:** ~20,000-30,000 tokens (system + context + query)
- **Typical output:** 2,000-15,000 tokens (dynamic, enforced by guard logic)
- **Buffer:** 1,000 tokens reserved for safety

---

### 6.3 Context Length Management

**Max Context Length:** 32,000 tokens (~24,000 words)

**Context Build Priority:**
1. **System prompt:** ~500 tokens (fixed)
2. **Conversation history:** ~1,000 tokens (last 3 turns)
3. **Retrieved documents:** Up to 30,000 tokens
   - Filtered relationship docs (copybook_usage, calls) kept in full
   - Semantic results truncated if over limit
4. **User query:** ~50-200 tokens

**Truncation Strategy:**
```python
if len(context_docs) > max_context_length:
    # Keep all filtered relationship docs (deterministic results)
    filtered_docs = [d for d in context_docs if d['_is_filtered_relationship']]
    
    # Truncate semantic results
    semantic_docs = [d for d in context_docs if not d['_is_filtered_relationship']]
    semantic_docs = semantic_docs[:max_context_length - len(filtered_docs)]
    
    context_docs = filtered_docs + semantic_docs
```

---

## 7. Limits and Constraints

### 7.1 Azure Search Limits

| Limit | Value | Impact |
|-------|-------|--------|
| **Max results per query** | 1,000 (hard limit) | We request 20-200, well under limit |
| **Max query length** | 1,000 characters | Queries are typically <200 chars |
| **Max index size** | 2 TB | Current: ~25 GB (1% utilized) |
| **Max documents per index** | 200 million | Current: max 1.9M (symbol_refs) |
| **Facet results** | 20,000 (configurable) | Used for coverage checks |
| **Request timeout** | 30 seconds (configurable) | Typical: 0.1-0.5s per index |

---

### 7.2 Azure OpenAI Limits

| Resource | Limit | Current Usage |
|----------|-------|---------------|
| **Embedding API** (text-embedding-3-large) | 240,000 TPM | Typical: 5,000 TPM |
| **Chat API** (gpt-4o) | 150,000 TPM | Typical: 10,000 TPM |
| **Rate limit** | S0 tier | 429 errors with parallel indexing (expected) |
| **Max prompt tokens** | 128,000 | Typical: 20,000-30,000 |
| **Max completion tokens** | 16,000 (configurable) | Dynamic: 2,000-15,000 |

**TPM = Tokens Per Minute**

---

### 7.3 Performance Benchmarks

**Query Latency Breakdown:**

| Component | Typical Time | Notes |
|-----------|--------------|-------|
| **Routing** | 0.01-0.1s | Regex pattern matching |
| **Embedding generation** | 0.2-0.5s (3072-dim) | OpenAI API call |
|  | 0.1-0.3s (1536-dim) | Faster for screen_nodes |
| **Index search** | 0.1-0.5s per index | **Parallelized** across multiple indexes |
| **Deterministic retrieval** | 1.0-2.0s | Multiple sequential queries + semantic fallback |
| **Memory retrieval** | 0.01s | In-memory lookup |
| **LLM generation** | 2.0-5.0s | Depends on max_tokens (2k-15k) |
| **Post-processing** | 0.1s | Hyperlink injection |
| **TOTAL** | **3-8 seconds** | **End-to-end aggregate** |

**⚠️ Timing Notes:**
- **Parallelization:** Index searches run concurrently, not sequentially. Searching 5 indexes takes ~0.5s total, not 2.5s (5 × 0.5s).
- **Deterministic mode:** The 1-2s includes multiple sequential steps (code chunk retrieval → line-range filtering → optional semantic fallback), which partially overlaps with embedding generation.
- **Typical breakdown:** 0.1s routing + 0.4s embedding + 0.5s parallel index search + 3.0s LLM = **4.0s total**
- **Worst case (deterministic + large context):** 0.1s routing + 0.5s embedding + 2.0s deterministic retrieval + 5.0s LLM = **7.6s total**

**Indexing Performance:**
- Sequential: ~5 seconds/file (embedding bottleneck)
- Parallel (10 workers): ~0.5-1 seconds/file
- Rate limit handling: Exponential backoff (1s, 2s, 4s)

---

### 7.4 Resource Utilization

**Azure Search (Standard S1 tier):**
- **Storage:** 25 GB / 300 GB (8% used)
- **Documents:** 3.5M / 200M (1.75% used)
- **QPS (Queries Per Second):** <10 / 300 (3% used)

**Azure OpenAI (S0 tier):**
- **Embedding TPM:** 5,000 / 240,000 (2% used)
- **Chat TPM:** 10,000 / 150,000 (6.6% used)

**Azure Functions (Consumption Plan):**
- **Cold start:** ~5-10 seconds (first request or after idle)
- **Warm execution:** <100ms overhead
- **Memory:** ~500 MB per instance
- **Concurrent executions:** 200 (limit)
- **Instance lifecycle:** Instances may be recycled after idle periods
- **Note:** Singleton pattern provides performance benefit within single instance lifetime, but state is not guaranteed across cold starts

---

## 8. Deployment Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│                         PRODUCTION ARCHITECTURE                       │
└──────────────────────────────────────────────────────────────────────┘

┌────────────────────┐
│   Frontend (React) │
│   Port: 3000       │
└─────────┬──────────┘
          │ HTTPS
          ▼
┌──────────────────────────────────────────────────────────────────────┐
│                    Azure Functions (Consumption Plan)                 │
│  func-otis-rag.azurewebsites.net                                     │
│                                                                       │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  Endpoints:                                                     │ │
│  │  • POST /api/query  - Main Q&A endpoint                       │ │
│  │  • GET  /api/health - Health check                            │ │
│  │  • GET  /api/stats  - System statistics                       │ │
│  └────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  function_app.py                                               │ │
│  │  • Singleton RAG instance (per-instance optimization)          │ │
│  │  • Conversation memory (in-memory, resets on cold start)       │ │
│  │  • Application Insights telemetry                             │ │
│  │  • CORS enabled                                                │ │
│  │  • Note: Consumption Plan may recycle instances after idle    │ │
│  └────────────────────────────────────────────────────────────────┘ │
└──────────┬────────────────────────────────────────┬─────────────────┘
           │                                        │
           │ otis_rag/                             │
           ▼                                        ▼
┌─────────────────────────┐           ┌──────────────────────────────┐
│  Azure AI Search        │           │  Azure OpenAI                │
│  (Standard S1)          │           │  (GPT-4o + Embeddings)       │
│                         │           │                              │
│  21 Indexes:            │           │  • gpt-4o (chat)            │
│  • code-chunks          │           │  • text-embedding-3-large    │
│  • new_code_chunks      │           │    (3072-dim)                │
│  • new_cobol_*          │           │  • text-embedding-ada-002    │
│  • transaction_taxonomy │           │    (1536-dim, screen_nodes)  │
│  • program_complexity   │           │                              │
│                         │           │  Temperature: 0.1            │
│  Total: 3.5M docs, 25GB │           │  Max tokens: 2000-15000      │
└─────────────────────────┘           └──────────────────────────────┘
```

---

### 8.1 Function App Structure

```
OTISCodeResearcher/
├── function_app.py              # Azure Functions HTTP triggers
├── requirements.txt             # Python dependencies
├── host.json                    # Function host configuration
├── local.settings.json          # Local development config (gitignored)
│
└── otis_rag/                    # RAG system package
    ├── __init__.py
    ├── config.py                # Configuration management
    ├── router.py                # Query routing
    ├── retriever.py             # Hybrid retrieval
    ├── memory.py                # Conversation memory
    ├── generator.py             # Response generation
    ├── prompts.py               # System prompts
    ├── rag.py                   # Main orchestrator
    └── requirements.txt         # Package dependencies
```

---

### 8.2 Deployment Commands

**Local Testing:**
```bash
cd OTISCodeResearcher
func start --python
```

**Deploy to Azure:**
```bash
func azure functionapp publish func-otis-rag --python
```

**Environment Variables (Azure Portal):**
```
AZURE_SEARCH_ENDPOINT=https://<service>.search.windows.net
AZURE_SEARCH_KEY=<admin-key>
AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com/
AZURE_OPENAI_KEY=<api-key>
AZURE_OPENAI_DEPLOYMENT=gpt-4o
AZURE_OPENAI_EMBED_DEPLOYMENT=text-embedding-3-large
APPLICATIONINSIGHTS_CONNECTION_STRING=<app-insights-connection>
```

---

### 8.3 Hosting Plan Considerations

**Current Deployment: Consumption Plan**
- **Pros:** Pay-per-execution, automatic scaling, zero cost when idle
- **Cons:** Cold starts (5-10s), instance recycling, no persistent state across cold starts
- **Singleton Pattern:** Improves performance within single instance lifetime, but does not guarantee warm state across requests

**Alternative: Premium Plan (EP1+) for Production Workloads**
- **Pros:** Pre-warmed instances, no cold starts, persistent state possible, VNet integration
- **Cons:** Fixed monthly cost (~$150-300/month for EP1)
- **When to Use:** High request volume, latency-sensitive applications, need for session state

**Alternative: Dedicated App Service Plan**
- **Pros:** Predictable performance, dedicated resources, full control
- **Cons:** Higher cost, manual scaling
- **When to Use:** Enterprise production with SLA requirements

---

## 9. Index Relationships

### 9.1 Cross-Index Dependencies

```
                    ┌─────────────────────────┐
                    │  new_cobol_program_meta │ (Central Hub)
                    │  • program_id (key)     │
                    │  • program_summary      │
                    └────────────┬────────────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                  │                  │
              ▼                  ▼                  ▼
┌─────────────────────┐ ┌──────────────────┐ ┌─────────────────────┐
│ new_cobol_calls     │ │ new_cobol_       │ │ new_cobol_ui_paths  │
│ • caller_program    │ │   copybook_usage │ │ • start_program     │
│ • callee_program    │ │ • program_id     │ │ • path_sequence     │
└──────────┬──────────┘ └────────┬─────────┘ └──────────┬──────────┘
           │                     │                       │
           └─────────────────────┴───────────────────────┘
                                 │
                                 ▼
                    ┌─────────────────────────┐
                    │ new_cobol_program_deps  │
                    │ (Aggregated View)       │
                    │ • outgoing_programs_json│
                    │ • copybooks_used_json   │
                    └─────────────────────────┘
```

**Relationships:**
1. **`program_meta`** → Acts as central hub for all program metadata
2. **`calls`** → References `program_meta` for caller/callee enrichment
3. **`copybook_usage`** → Links programs to copybooks they include
4. **`ui_paths`** → References `program_meta` for navigation flows
5. **`program_deps`** → Aggregates data from `calls`, `copybook_usage`, `program_meta`
6. **`flow_edges_v2`** → Enhanced edges linking programs, copybooks, screens

---

### 9.2 Query Patterns Using Multiple Indexes

**Pattern 1: Program Dependency Analysis**
```
Query: "What does LONPF2 depend on?"

Indexes Used:
1. new_cobol_program_meta → Get program summary, role
2. new_cobol_program_deps → Get aggregated dependencies (one-shot)
3. new_cobol_calls → Get detailed call snippets
4. new_cobol_copybook_usage → Get copybook inclusion details

Response: Combines metadata + aggregated deps + detailed snippets
```

**Pattern 2: Transaction Flow Tracing**
```
Query: "Trace the flow when a user enters RP transaction in LONPF2"

Indexes Used:
1. new_cobol_program_flows → Get high-level flow diagram
2. new_cobol_flow_edges_v2 → Get detailed edge-by-edge flow
3. new_cobol_screen_nodes → Get screen UI details (boosted 3.0x)
4. new_cobol_ui_paths → Get navigation paths
5. new_code_chunks → Get actual code snippets

Response: Visual Mermaid diagram + narrative + code examples
```

**Pattern 3: Copybook Discovery**
```
Query: "Find copybooks related to loan repayment"

Indexes Used:
1. new_cobol_copybook_meta → Semantic search on summary_vector
2. new_cobol_copybook_usage → Find which programs use these copybooks
3. new_cobol_program_meta → Enrich with program context

Response: Relevant copybooks + usage statistics + program context
```

---

## 10. Code Examples

### 10.1 Basic Query

```python
from otis_rag.rag import OTISRAG

# Initialize (loads config from local.settings.json)
rag = OTISRAG()

# Ask a question
answer = rag.ask("What does program LONPF2 do?")
print(answer)
```

**Output:**
```
LONPF2 is a loan payment processing program that handles repayment (RP) 
and charge payment (CP) transactions...

(Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Index: new_cobol_program_meta)
```

---

### 10.2 Query with Timing Breakdown

```python
result = rag.ask_with_timing(
    "List all copybooks used in LONPF2 when handling RP transactions?",
    verbose=True
)

print("Answer:", result['answer'])
print("\nTiming:")
for component, seconds in result['timing'].items():
    print(f"  {component}: {seconds}s")
```

**Output:**
```
Route: is_otis=True, type=transaction_copybooks
Searching indexes: ['copybook_usage', 'copybooks', 'code_new']
Retrieved 85 documents

Answer: When handling RP transactions, LONPF2 uses...

Timing:
  route_seconds: 0.045
  retrieve_seconds: 2.301
  memory_seconds: 0.012
  generate_seconds: 3.187
  total_seconds: 5.545
```

---

### 10.3 Custom Index Configuration

```python
from otis_rag.config import Config
from otis_rag.rag import OTISRAG

# Custom config
config = Config()
config.max_results_per_index = 100  # Retrieve more docs
config.temperature = 0.2  # Slightly more creative
config.max_conversation_turns = 50  # Longer memory

rag = OTISRAG()
rag.config = config

answer = rag.ask("Explain the menu structure in OTIS")
```

---

### 10.4 Programmatic Index Search

```python
from otis_rag.retriever import HybridRetriever
from otis_rag.config import Config

config = Config()
retriever = HybridRetriever(config)

# Search specific indexes
docs = retriever.retrieve(
    query="loan repayment processing",
    indexes=['programs', 'code_new', 'copybooks'],
    max_results=20,
    index_weights={'code_new': 2.0}  # Boost code chunks
)

for doc in docs[:5]:
    print(f"Score: {doc['@search.score']:.4f}")
    print(f"Index: {doc['_index_type']}")
    print(f"Content: {doc.get('text', doc.get('program_summary', 'N/A'))[:100]}")
    print()
```

---

### 10.5 Azure Function Endpoint Usage

**cURL:**
```bash
curl -X POST https://func-otis-rag.azurewebsites.net/api/query \
  -H "Content-Type: application/json" \
  -d '{
    "question": "What programs handle elevator dispatching?"
  }'
```

**Python:**
```python
import requests

response = requests.post(
    "https://func-otis-rag.azurewebsites.net/api/query",
    json={"question": "What programs handle elevator dispatching?"}
)

result = response.json()
print(result['answer'])
print(f"Duration: {result['duration_ms']}ms")
```

**JavaScript (Frontend):**
```javascript
const response = await fetch('https://func-otis-rag.azurewebsites.net/api/query', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    question: 'What programs handle elevator dispatching?'
  })
});

const data = await response.json();
console.log(data.answer);
console.log(`Timing: ${JSON.stringify(data.timing)}`);
```

---

## Appendix: System Evolution & Future Roadmap

### Completed Phases

**Phase 0: Foundation (September 2024)**
- ✅ Basic RAG with 8 indexes
- ✅ Simple semantic search
- ✅ GPT-3.5-turbo chat

**Phase 1: Enhanced Prompts & Flows (November 2024)**
- ✅ Program flows (Mermaid diagrams)
- ✅ Flow edges v2 (detailed edge semantics)
- ✅ Enhanced system prompts
- ✅ Conversation memory (30 turns)
- ✅ GPT-4o upgrade

**Phase 2: Business Context (December 2024)**
- ✅ Transaction taxonomy index (51 transactions)
- ✅ Program complexity metrics
- ✅ Intelligent query routing
- ✅ Index weights (3.0x boost for screens)

**Phase 3: Deterministic Retrieval (January 2025)**
- ✅ Transaction copybook deterministic handler
- ✅ Hybrid semantic fallback
- ✅ 100% code coverage (9,951 files)
- ✅ Dynamic token allocation
- ✅ Production deployment with telemetry

---

### Planned Enhancements

**Q2 2025:**
- 🔄 Real-time streaming responses (SSE)
- 🔄 Multi-turn conversation refinement
- 🔄 User feedback loop for answer quality
- 🔄 Caching layer for common queries

**Q3 2025:**
- 🔄 Graph visualization in UI
- 🔄 Code diff analysis
- 🔄 Impact analysis for changes
- 🔄 Automated documentation generation

---

## Conclusion

The OTIS RAG system is a production-grade, enterprise-scale Q&A chatbot that combines:

- **21 specialized indexes** with 3.5M documents
- **Hybrid retrieval** (semantic + lexical + deterministic)
- **Intelligent routing** with dynamic token allocation
- **GPT-4o** for natural language generation
- **100% code coverage** with optimized indexing

**Key Strengths:**
- Handles complex COBOL codebase queries with high accuracy
- Deterministic mode for transaction-specific questions
- Semantic fallback ensures no query goes unanswered
- Dynamic resource allocation optimizes cost and latency
- Production-ready with telemetry and monitoring

**Contact:**
For questions or support, contact the OTIS RAG team.

---

**Document Version:** 2.0  
**Last Updated:** January 27, 2025
