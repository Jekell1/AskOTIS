# OTIS RAG - Simple, Elegant RAG for COBOL Analysis

**A minimalist RAG system for querying COBOL source code with intelligence.**

---

## 🎯 Features

✅ **OTIS-Aware Routing** - Differentiates between OTIS application questions and general COBOL  
✅ **Hybrid Search** - Combines semantic (vector) and lexical (keyword) search  
✅ **Multi-Index Intelligence** - Searches across 10 specialized indexes  
✅ **Conversation Memory** - Maintains short-term context for follow-up questions  
✅ **Simple API** - One-line interface: `rag.ask("your question")`  

---

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         OTIS RAG                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  User Question                                              │
│       ↓                                                     │
│  ┌──────────────┐                                           │
│  │   Router     │  Is this about OTIS? What type of Q?     │
│  └──────────────┘                                           │
│       ↓                                                     │
│  ┌──────────────┐                                           │
│  │  Retriever   │  Hybrid search across indexes            │
│  └──────────────┘                                           │
│       ↓                                                     │
│  ┌──────────────┐                                           │
│  │   Memory     │  Get conversation context                │
│  └──────────────┘                                           │
│       ↓                                                     │
│  ┌──────────────┐                                           │
│  │  Generator   │  LLM creates answer with context         │
│  └──────────────┘                                           │
│       ↓                                                     │
│  Answer + Memory Update                                     │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 📦 Components

### 1. **Config** (`config.py`)
- Loads settings from `local.settings.json`
- Manages Azure Search and OpenAI credentials
- Defines indexes and parameters

### 2. **Router** (`router.py`)
- Detects OTIS/OTOS mentions
- Classifies question type (explain_program, find_code, etc.)
- Selects appropriate indexes to search
- Cleans query for optimal search

### 3. **Retriever** (`retriever.py`)
- Hybrid search (semantic + lexical)
- Multi-index search in parallel
- Deduplication and ranking
- Returns top relevant documents

### 4. **Memory** (`memory.py`)
- Stores recent conversation turns
- Provides context for follow-up questions
- Auto-prunes to max turns (default: 10)

### 5. **Generator** (`generator.py`)
- Formats context from retrieved docs
- Builds prompts with conversation history
- Calls Azure OpenAI for response
- Tailors response based on question type

### 6. **RAG Orchestrator** (`rag.py`)
- Coordinates all components
- Simple API: `rag.ask(question)`
- Manages conversation flow

---

## 🚀 Usage

### Python API

```python
from otis_rag import OTISRAG

# Initialize once
rag = OTISRAG()

# Ask questions
answer = rag.ask("What does program GB01SE do?")
print(answer)

# Follow-up (uses memory)
answer = rag.ask("What variables does it use?")
print(answer)

# Clear conversation
rag.clear_memory()
```

### Command Line

```bash
# Single question
python -m otis_rag.cli "What does OTIS do?"

# Interactive mode
python -m otis_rag.cli

# Verbose (show routing)
python -m otis_rag.cli -v "Find customer programs"
```

### Quick Script

```bash
# Simple test script
python ask_otis.py
```

---

## 🔍 How It Works

### Example: "What does the OTIS system handle?"

1. **Router** detects:
   - `is_otis = True` (mentions "OTIS")
   - `question_type = 'general'`
   - `search_indexes = ['code', 'programs', 'paragraphs']`

2. **Retriever** searches:
   - `code-chunks` index (source code)
   - `new_cobol_program_meta` (program metadata)
   - `new_cobol_paragraphs` (paragraph details)
   - Returns top 5 results per index (15 total)

3. **Memory** provides:
   - Last 3 conversation turns for context

4. **Generator** creates:
   - System prompt: "This is about OTIS application..."
   - Context: Retrieved docs formatted
   - Final answer with citations

---

## 📊 Indexes Searched

| Index | Purpose | Vector Field |
|-------|---------|--------------|
| `code-chunks` | Actual COBOL source code | `text_vector` |
| `new_cobol_program_meta` | Program summaries | `program_summary_vector` |
| `new_cobol_paragraphs` | Paragraph details | `paragraph_summary_vector` |
| `new_cobol_data_items` | Data structure definitions | `data_item_vector` |
| `new_cobol_variable_usage` | Variable references | `usage_summary_vector` |
| `new_cobol_calls` | Program calls | `call_summary_vector` |
| `new_cobol_program_flows` | Execution flows | `flow_summary_vector` |
| `new_cobol_ui_paths` | UI navigation paths | `sequence_vector` |
| `new_cobol_copybook_meta` | Copybook metadata | `copybook_summary_vector` |
| `new_cobol_symbol_refs` | Symbol references | `reference_vector` |

---

## ⚙️ Configuration

Edit `local.settings.json`:

```json
{
  "Values": {
    "SEARCH_ENDPOINT": "https://your-search.search.windows.net",
    "SEARCH_KEY": "your-search-key",
    "AZURE_OPENAI_ENDPOINT": "https://your-openai.openai.azure.com",
    "AZURE_OPENAI_KEY": "your-openai-key",
    "AZURE_OPENAI_DEPLOYMENT": "gpt-4.1",
    "AZURE_OPENAI_EMBED_DEPLOYMENT": "text-embedding-3-large"
  }
}
```

---

## 🎨 Design Principles

1. **Simplicity** - One-line API, minimal config
2. **Elegance** - Clean separation of concerns
3. **Intelligence** - Context-aware routing and memory
4. **Performance** - Hybrid search, focused results
5. **Transparency** - Verbose mode shows decision process

---

## 🧪 Testing

```python
# Test OTIS detection
from otis_rag.router import route_query

result = route_query("What does OTIS do?")
assert result['is_otis'] == True

result = route_query("Find COBOL programs")
assert result['is_otis'] == False

# Test retrieval
from otis_rag import OTISRAG

rag = OTISRAG()
docs = rag.retriever.retrieve("customer data", ['code', 'programs'])
assert len(docs) > 0

# Test memory
rag.ask("What is GB01SE?")
rag.ask("What does it call?")  # Should use memory
assert len(rag.memory) == 2
```

---

## 📈 Performance

- **Latency**: ~2-3 seconds per query (search + LLM)
- **Memory**: ~50MB (OpenAI client + index metadata)
- **Context**: Up to 8000 tokens for LLM
- **Throughput**: Limited by Azure OpenAI rate limits

---

## 🔮 Future Enhancements

- [ ] Streaming responses for real-time feedback
- [ ] Web UI (Streamlit/Gradio)
- [ ] Query rewriting for better retrieval
- [ ] Relevance feedback loop
- [ ] Multi-turn clarification questions
- [ ] Export conversation history

---

## 📝 Example Questions

**OTIS-Specific:**
- "What does the OTIS system handle?"
- "How does OTOS process loans?"
- "What are the main OTIS programs?"

**Code Analysis:**
- "Find programs that use GB01SE copybook"
- "What does program LP01BI do?"
- "Show me customer data structures"
- "Trace the flow from screen SE01 to database"

**Data Exploration:**
- "What fields are in SE-RECORD?"
- "Find all PIC X(10) fields"
- "What's the structure of loan data?"

**Follow-ups (using memory):**
- Q1: "What does GB01SE do?"
- Q2: "What variables does it define?"  ← Uses context from Q1
- Q3: "Show me the code"  ← Uses context from Q1 & Q2

---

## 🐛 Troubleshooting

**Error: Missing configuration**
- Check `local.settings.json` exists
- Verify all required keys are present

**Error: No results found**
- Check index names in `config.py`
- Verify indexes exist in Azure Search
- Try more specific keywords

**Error: Azure OpenAI timeout**
- Reduce `max_results_per_index` in config
- Check Azure OpenAI rate limits
- Verify deployment name (not model name)

---

## 📄 License

MIT

---

**Built with ❤️ for COBOL code understanding**
