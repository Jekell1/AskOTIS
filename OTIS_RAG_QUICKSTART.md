# OTIS RAG - Quick Start Guide

## 📦 Installation

```bash
# Install dependencies
pip install -r otis_rag/requirements.txt
```

## 🚀 Usage Examples

### 1. Simple Python Script

```python
from otis_rag import OTISRAG

# Initialize once
rag = OTISRAG()

# Ask questions
answer = rag.ask("What does program GB01SE do?")
print(answer)

# Follow-up questions use memory
answer = rag.ask("What variables does it use?")
print(answer)
```

### 2. Command Line - Single Question

```bash
python -m otis_rag.cli "What does the OTIS system handle?"
```

### 3. Command Line - Interactive Mode

```bash
python -m otis_rag.cli
```

Then type questions:
```
💬 You: What does OTIS do?
🤖 Assistant: [answer]

💬 You: What programs handle loans?
🤖 Assistant: [answer]

💬 You: quit
```

### 4. Verbose Mode (see routing decisions)

```bash
python -m otis_rag.cli -v "Find customer programs"
```

Output:
```
🧭 Route: is_otis=False, type=find_code
🔍 Searching indexes: ['code', 'programs', 'paragraphs']
📚 Retrieved 12 documents
```

### 5. Quick Test Script

```bash
python ask_otis.py
```

## 🎯 Example Questions

### OTIS Application Questions:
```python
# System detects "OTIS" keyword
rag.ask("What does the OTIS system handle?")
rag.ask("How does OTOS process loans?")
rag.ask("What are the main OTIS programs?")
```

### Code Analysis Questions:
```python
# Searches code and structure
rag.ask("What does program GB01SE do?")
rag.ask("Find programs that handle customer data")
rag.ask("Show me the loan processing flow")
```

### Data Structure Questions:
```python
# Focuses on data items and copybooks
rag.ask("What fields are in SE-RECORD?")
rag.ask("Find all PIC X(10) customer ID fields")
rag.ask("What's the structure of loan data?")
```

### Follow-up Questions (uses memory):
```python
rag.ask("What does program LP01BI do?")
rag.ask("What variables does it use?")      # Remembers LP01BI
rag.ask("Show me the code")                  # Still about LP01BI
```

## 🔧 Advanced Usage

### Clear Conversation Memory

```python
rag.clear_memory()  # Start fresh conversation
```

### Get System Statistics

```python
stats = rag.get_stats()
print(stats)
# {
#   'conversation_turns': 5,
#   'max_turns': 10,
#   'indexes_available': 10,
#   'last_query': 'What does GB01SE do?'
# }
```

### Custom Configuration

```python
from otis_rag import OTISRAG, Config

# Use custom settings file
rag = OTISRAG(config_file="my_settings.json")
```

## 📊 Understanding the Output

Answers include:
- Direct response to your question
- Code examples (when relevant)
- File/program names cited
- Structured explanations with bullet points

Example output:
```
The GB01SE program handles customer account processing.

Key functions:
• Validates customer ID (SE-CUST-ID)
• Retrieves account balance (SE-BALANCE)
• Updates transaction history

Source: LIBGB/GB01SE.CPY (Lines 45-120)
```

## 🐛 Troubleshooting

**"Missing required configuration"**
- Verify `local.settings.json` exists
- Check all required keys are present:
  - SEARCH_ENDPOINT
  - SEARCH_KEY
  - AZURE_OPENAI_ENDPOINT
  - AZURE_OPENAI_KEY

**"No results found"**
- Make questions more specific
- Check if indexes are populated
- Try different keywords

**Slow responses**
- Normal: 2-3 seconds per query
- Check Azure OpenAI rate limits
- Reduce `max_results_per_index` in config

## 📝 Tips for Best Results

1. **Be specific** - "What does program GB01SE do?" vs "What does this do?"
2. **Use names** - Reference actual program/file names when known
3. **Ask follow-ups** - System remembers last 10 conversation turns
4. **Mention OTIS** - Say "OTIS" or "OTOS" for application-specific context
5. **Use verbose mode** - See what the system is doing: `-v` flag

## 🎨 Components Overview

```
OTISRAG (main)
├── Config      → Loads settings
├── Router      → Detects OTIS, classifies questions
├── Retriever   → Hybrid search across indexes
├── Memory      → Conversation history
└── Generator   → LLM response creation
```

## 📚 Next Steps

- Read full documentation: `otis_rag/README.md`
- Run tests: `python test_otis_rag.py`
- Try examples: `python ask_otis.py`
- Start interactive: `python -m otis_rag.cli`

---

**Happy coding! 🚀**
