# OTIS RAG - System Architecture

## 🎯 Design Philosophy

**Simple • Elegant • Minimalist**

- **Simple**: One-line API (`rag.ask("question")`)
- **Elegant**: Clean separation of concerns, 6 focused components
- **Minimalist**: Only essential features, no bloat

---

## 📐 Component Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         USER INTERFACE                          │
│                                                                 │
│  Python API:  rag.ask("question")                               │
│  CLI:         python -m otis_rag.cli "question"                 │
│  Interactive: python -m otis_rag.cli                            │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                      OTISRAG (Orchestrator)                     │
│  • Coordinates all components                                   │
│  • Manages request flow                                         │
│  • Simple stateful API                                          │
└─────────────────────────────────────────────────────────────────┘
         ↓              ↓              ↓              ↓
    ┌────────┐    ┌──────────┐   ┌─────────┐   ┌───────────┐
    │ Router │    │Retriever │   │ Memory  │   │Generator  │
    └────────┘    └──────────┘   └─────────┘   └───────────┘
         ↓              ↓              ↓              ↓
    [Routing]     [Search 10       [Store       [Build
     Logic]       indexes]          context]     prompts]
                       ↓                             ↓
              ┌──────────────┐              ┌──────────────┐
              │ Azure Search │              │Azure OpenAI  │
              │  (Indexes)   │              │   (LLM)      │
              └──────────────┘              └──────────────┘
```

---

## 🧩 Component Details

### 1. **Config** (Configuration Manager)
```
Purpose: Central configuration from local.settings.json
Responsibilities:
  • Load credentials (Azure Search, OpenAI)
  • Define index mappings
  • Set RAG parameters (max_results, temperature, etc.)
  • Validate configuration
  
Key Settings:
  • search_endpoint, search_key
  • openai_endpoint, openai_key
  • chat_deployment, embed_deployment
  • indexes: 10 COBOL indexes
  • max_results_per_index: 5
  • max_conversation_turns: 10
```

### 2. **Router** (Query Intelligence)
```
Purpose: Understand user intent and route intelligently
Responsibilities:
  • Detect OTIS/OTOS references → is_otis flag
  • Classify question type:
    - explain_program
    - find_code
    - explain_data
    - trace_flow
    - general
  • Select appropriate indexes to search
  • Clean query (remove OTIS references for better search)

Example:
  Input:  "What does the OTIS system do?"
  Output: {
    is_otis: True,
    question_type: 'general',
    search_indexes: ['code', 'programs', 'paragraphs'],
    clean_query: "What does the system do?"
  }
```

### 3. **Retriever** (Hybrid Search)
```
Purpose: Retrieve relevant documents from indexes
Responsibilities:
  • Generate query embedding (text-embedding-3-large)
  • Execute hybrid search per index:
    - Semantic: Vector similarity search
    - Lexical: Keyword/BM25 search
  • Combine results from multiple indexes
  • Deduplicate by document ID
  • Rank by search score
  • Return top N results

Search Pattern (per index):
  POST /indexes/{index}/docs/search
  {
    "search": "customer data",        # Lexical
    "vectorQueries": [{                # Semantic
      "vector": [0.02, -0.15, ...],
      "k": 5,
      "fields": "text_vector"
    }],
    "top": 5
  }
```

### 4. **Memory** (Conversation Context)
```
Purpose: Short-term memory for context continuity
Responsibilities:
  • Store conversation turns (user + assistant)
  • Maintain last N turns (default: 10)
  • Provide recent context for LLM
  • Enable follow-up questions
  • Auto-prune old turns

Structure per turn:
  {
    timestamp: "2025-10-16T14:30:00",
    user: "What does GB01SE do?",
    assistant: "GB01SE handles customer...",
    metadata: {
      routing: {...},
      num_docs: 12
    }
  }
```

### 5. **Generator** (Response Creation)
```
Purpose: Generate intelligent answers using LLM
Responsibilities:
  • Format retrieved documents into context
  • Build complete prompt with:
    - System prompt (OTIS-aware)
    - Conversation history
    - Retrieved context
    - User question
  • Call Azure OpenAI (gpt-4.1)
  • Return formatted response

Prompt Structure:
  [System Prompt: You are a COBOL expert...]
  
  ## Recent Conversation Context:
  [Last 3 turns if any]
  
  ## Retrieved Context:
  [Formatted documents from search]
  
  ## Question:
  [User's question]
  
  ## Answer:
  [LLM generates here]
```

### 6. **OTISRAG** (Main Orchestrator)
```
Purpose: Simple API coordinating all components
Responsibilities:
  • Initialize all components
  • Execute request flow:
    1. Route query
    2. Retrieve context
    3. Get conversation context
    4. Generate response
    5. Store in memory
  • Provide stats and utilities

Public API:
  • ask(query, verbose) → answer
  • clear_memory()
  • get_stats()
```

---

## 🔄 Request Flow

```
User asks: "What does the OTIS system handle?"

1. ROUTER
   ┌─────────────────────────────────┐
   │ Analyze query:                  │
   │ • Detect "OTIS" → is_otis=True  │
   │ • Type: general                 │
   │ • Indexes: code, programs, para │
   │ • Clean: "system handle"        │
   └─────────────────────────────────┘
               ↓
2. RETRIEVER
   ┌─────────────────────────────────┐
   │ Generate embedding for query    │
   │ Search 3 indexes:               │
   │   • code-chunks (5 results)     │
   │   • program_meta (5 results)    │
   │   • paragraphs (5 results)      │
   │ → Total: 15 docs                │
   └─────────────────────────────────┘
               ↓
3. MEMORY
   ┌─────────────────────────────────┐
   │ Get recent context:             │
   │ • Last 3 conversation turns     │
   │ • Format for prompt             │
   └─────────────────────────────────┘
               ↓
4. GENERATOR
   ┌─────────────────────────────────┐
   │ Build prompt:                   │
   │ • System: OTIS-specific mode    │
   │ • Context: 15 docs formatted    │
   │ • History: last 3 turns         │
   │ • Question: user query          │
   │                                 │
   │ Call Azure OpenAI → Answer      │
   └─────────────────────────────────┘
               ↓
5. MEMORY UPDATE
   ┌─────────────────────────────────┐
   │ Store turn:                     │
   │ • User: "What does OTIS..."     │
   │ • Assistant: [generated answer] │
   │ • Metadata: routing, docs       │
   └─────────────────────────────────┘
               ↓
         RETURN ANSWER
```

---

## 🎯 OTIS Detection Logic

```python
# Router checks for OTIS mentions:

OTIS_KEYWORDS = {
    'otis', 'otos',
    'otis system', 'otos system',
    'otis application', 'otos application'
}

def _is_otis_question(query_lower):
    # Exact keyword match
    for keyword in OTIS_KEYWORDS:
        if keyword in query_lower:
            return True
    
    # Context clues
    if 'this application' in query_lower:
        return True
    
    return False

# If is_otis = True:
#   • System prompt includes OTIS context
#   • Response tailored to OTIS application
#   • Query cleaned for better search

# If is_otis = False:
#   • Generic COBOL analysis mode
#   • No OTIS-specific context
```

---

## 📊 Index Selection Strategy

```python
# Router selects indexes based on question type:

QUESTION_TYPE → INDEXES SEARCHED

explain_program  → code, programs, flows, paragraphs
find_code        → code, programs, paragraphs
explain_data     → data_items, variables, copybooks
trace_flow       → flows, calls, ui_paths
general          → code, programs, paragraphs, data_items

# Always includes 'code' (source text) for grounding
```

---

## 🧠 Conversation Memory Pattern

```
Turn 1: "What does GB01SE do?"
  → Answer: "GB01SE handles customer accounts..."
  → Store in memory

Turn 2: "What variables does it use?"
  → Context: Previous turn about GB01SE
  → LLM understands "it" = GB01SE
  → Answer: "GB01SE uses SE-CUST-ID, SE-BALANCE..."
  → Store in memory

Turn 3: "Show me the code"
  → Context: Last 2 turns about GB01SE
  → LLM understands context
  → Answer: [Shows GB01SE code]
  → Store in memory

# Memory auto-prunes after 10 turns
# User can clear with: rag.clear_memory()
```

---

## 🔍 Hybrid Search Details

```
Query: "customer data"

Step 1: Generate Embedding
  text-embedding-3-large("customer data")
  → [0.02, -0.15, 0.08, ...] (3072 dims)

Step 2: Search Each Index (Hybrid)

  Semantic Search (Vector):
    • Cosine similarity: query_vector × document_vectors
    • Returns top K similar documents
  
  Lexical Search (BM25):
    • Keyword matching: "customer" AND "data"
    • TF-IDF weighted scoring
    • Returns top K matching documents
  
  Azure Search combines both automatically

Step 3: Aggregate Results
  • Collect results from all indexes
  • Deduplicate by document ID
  • Sort by @search.score (hybrid score)
  • Return top N overall

Result: ~15 most relevant documents
```

---

## 📈 Performance Characteristics

```
Component          Latency    Notes
────────────────────────────────────────────────
Router             <10ms      Pattern matching
Embedding Gen      ~200ms     Azure OpenAI API
Index Search       ~300ms     3 indexes parallel
LLM Generation     ~2s        gpt-4.1
────────────────────────────────────────────────
TOTAL              ~2-3s      End-to-end

Memory Usage:      ~50MB      Loaded models
Context Tokens:    ~8000      Max for LLM
Conversation:      10 turns   Auto-prune
```

---

## 🔒 Configuration Validation

```python
Required Settings (validated on init):
  ✓ SEARCH_ENDPOINT
  ✓ SEARCH_KEY
  ✓ AZURE_OPENAI_ENDPOINT
  ✓ AZURE_OPENAI_KEY

Optional (with defaults):
  • AZURE_OPENAI_DEPLOYMENT (default: gpt-4.1)
  • AZURE_OPENAI_EMBED_DEPLOYMENT (default: text-embedding-3-large)
  • max_results_per_index (default: 5)
  • max_conversation_turns (default: 10)
  • temperature (default: 0.1)
  • max_context_length (default: 8000)
```

---

## 🎨 Design Patterns Used

1. **Facade Pattern** - OTISRAG provides simple interface to complex system
2. **Strategy Pattern** - Router selects search strategy based on query type
3. **Repository Pattern** - Retriever abstracts index access
4. **Memento Pattern** - Memory stores conversation state
5. **Template Method** - Generator follows structured prompt building

---

## 🚀 Extending the System

### Add New Index:
```python
# In config.py
self.indexes = {
    ...
    'my_new_index': 'new_cobol_my_index'
}

# In retriever.py
vector_fields = {
    ...
    'new_cobol_my_index': 'my_vector_field'
}
```

### Add Question Type:
```python
# In router.py
QUESTION_PATTERNS = {
    ...
    'my_new_type': [r'\bmy pattern\b']
}

def _select_indexes(self, question_type, is_otis):
    ...
    elif question_type == 'my_new_type':
        indexes.extend(['my_indexes'])
```

### Customize System Prompt:
```python
# In generator.py
def _get_system_prompt(self, is_otis):
    base_prompt = "Your custom prompt..."
    # Add OTIS context if needed
    return base_prompt
```

---

**Architecture designed for simplicity, elegance, and extensibility** 🎯
