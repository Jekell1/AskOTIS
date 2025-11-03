# 🚀 COBOL RAG Chatbot System
## Retrieval-Augmented Generation with Conversational Memory

### 📋 Overview

This system transforms your COBOL Search API into an intelligent conversational assistant using RAG (Retrieval-Augmented Generation) architecture. It combines:

- **🔍 Real-time COBOL code search** via Azure AI Search
- **🧠 Conversational memory** for multi-turn interactions  
- **💬 Natural language interface** powered by OpenAI GPT
- **📊 Rich context understanding** from 1.3M+ COBOL lines

### 🏗️ Architecture

```
User Query → Search COBOL API → Format Context → Generate Response → Update Memory
     ↑                                                      ↓
     └──── Conversation Memory ←── Store Exchange ←────────┘
```

**Components:**
1. **COBOLSearchAPI**: Interfaces with Azure AI Search
2. **ConversationMemory**: Manages chat history and context
3. **COBOLRagAssistant**: Orchestrates RAG pipeline
4. **Streamlit UI**: Interactive web interface

### 🎯 Features

#### ✅ **Core RAG Capabilities**
- **Retrieval**: Searches 1,307,985 indexed COBOL documents
- **Context**: Formats search results for LLM consumption
- **Generation**: Creates natural language responses
- **Memory**: Maintains conversation context across turns

#### ✅ **Advanced Features**  
- **Multi-turn conversations** with memory
- **Symbol-specific filtering** (programs/variables/procedures)
- **Code context preservation** (file paths, line numbers)
- **Fallback responses** when OpenAI unavailable
- **Real-time search** with live results

#### ✅ **User Experience**
- **Streamlit web interface** with chat UI
- **Example queries** for quick start
- **Memory management** (clear history)
- **Visual code formatting** with syntax highlighting
- **Error handling** and user guidance

### 🚀 Quick Start

#### **1. Install Dependencies**
```bash
pip install streamlit openai requests python-dotenv
```

#### **2. Set Environment Variables**
```bash
# Copy template and edit
cp .env.template .env

# Edit .env file:
OPENAI_API_KEY=your_openai_key_here
```

#### **3. Launch Chatbot**
```bash
# Easy launcher
python launch_chatbot.py

# Or directly:
streamlit run cobol_rag_openai.py
```

#### **4. Open Browser**
Navigate to: `http://localhost:8501`

### 💡 Usage Examples

#### **Basic Searches**
```
"Find all CUSTOMER programs"
"Show me DATE variables"
"Search for IDENTIFICATION DIVISION"
```

#### **Advanced Queries**
```
"What programs call ACUMEM and what do they do?"
"Explain the relationship between these COBOL files"
"Find all variables that handle account numbers"
```

#### **Follow-up Questions**
```
User: "Find CUSTOMER programs"
Bot: [Shows results from CUSTOMER*.CBL files]

User: "Tell me more about the first one"
Bot: [Remembers context, explains specific program]
```

### 🔧 Configuration Options

#### **COBOLRagConfig Settings**
```python
@dataclass
class Config:
    azure_search_endpoint: str      # Your Azure Search endpoint
    azure_search_key: str          # Query API key
    openai_api_key: str            # OpenAI API key (optional)
    max_search_results: int = 15   # Max results per search
    max_context_length: int = 4000 # Max context for LLM
```

#### **Memory Settings**
```python
class ConversationMemory:
    def __init__(self, max_exchanges: int = 5):
        # Keeps last 5 exchanges in memory
```

### 🎨 Two Implementation Options

#### **1. Enhanced Version (Recommended)**
**File**: `cobol_rag_openai.py`
- Uses OpenAI GPT for intelligent responses
- Better context understanding
- More natural conversations
- Requires OpenAI API key

#### **2. Basic Version**
**File**: `cobol_rag_chatbot.py`  
- Rule-based response generation
- No external API dependencies
- Still includes search and memory
- Good for testing/development

### 🔍 How It Works

#### **1. Query Processing**
```python
def generate_response(self, user_query: str) -> str:
    # Search COBOL codebase
    search_results = self.search_api.search(user_query)
    
    # Format context from results
    context = self._format_search_context(search_results)
    
    # Generate response with LLM
    response = self._generate_openai_response(user_query, context)
    
    # Update conversation memory
    self.memory.add_exchange(user_query, response)
    
    return response
```

#### **2. Context Formation**
```python
def _format_search_context(self, search_results: Dict) -> str:
    # Extract relevant code snippets
    # Format with file paths and line numbers
    # Structure for LLM consumption
    return formatted_context
```

#### **3. Response Generation**
```python
# System prompt guides the LLM behavior
system_prompt = """You are a COBOL code expert assistant.
Use search results to answer questions accurately..."""

# User prompt includes query + search context + memory
user_prompt = f"{conversation_context}\n{query}\n{search_results}"

# Generate response with OpenAI
response = openai.ChatCompletion.create(...)
```

### 📊 Performance Metrics

| Metric | Value |
|--------|-------|
| **Search Latency** | ~500ms |
| **Response Time** | ~2-3 seconds |
| **Context Length** | Up to 4K tokens |
| **Memory Retention** | 5 exchanges |
| **Search Accuracy** | High (Azure AI Search) |
| **Concurrent Users** | Streamlit default |

### 🛠️ Customization

#### **Adding New Search Filters**
```python
# In COBOLSearchAPI.search()
if filter_type == "your_new_filter":
    search_body["filter"] = "your_odata_filter_here"
```

#### **Modifying Response Templates**
```python
# Update system_prompt in _generate_openai_response()
system_prompt = """Your custom instructions here..."""
```

#### **Extending Memory**
```python
# Increase memory capacity
memory = ConversationMemory(max_exchanges=10)
```

### 🔒 Security Considerations

- **API Keys**: Store securely in environment variables
- **Rate Limiting**: OpenAI API has usage limits
- **Input Validation**: User queries are sanitized
- **Error Handling**: Graceful fallbacks for API failures

### 🚀 Deployment Options

#### **Local Development**
```bash
streamlit run cobol_rag_openai.py --server.port=8501
```

#### **Docker Container**
```dockerfile
FROM python:3.9-slim
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY . .
EXPOSE 8501
CMD ["streamlit", "run", "cobol_rag_openai.py"]
```

#### **Cloud Deployment**  
- **Streamlit Cloud**: Direct deployment from GitHub
- **Azure Container Apps**: Scalable cloud hosting
- **AWS/GCP**: Container-based deployment

### 📈 Future Enhancements

#### **Planned Features**
- **Vector embeddings** for semantic search
- **Fine-tuned models** for COBOL-specific understanding
- **Multi-language support** for legacy code
- **Integration APIs** for external tools
- **Analytics dashboard** for usage insights

#### **Advanced RAG Features**
- **Hybrid search** (keyword + semantic)
- **Query expansion** for better results
- **Response caching** for performance
- **Multi-step reasoning** for complex queries

### 🎉 Success Metrics

Your RAG system is working well if you see:

✅ **Accurate code retrieval** from search queries  
✅ **Contextual responses** that reference specific files/lines  
✅ **Memory retention** across conversation turns  
✅ **Natural language** explanations of COBOL concepts  
✅ **User engagement** with follow-up questions  

**Ready to explore your COBOL codebase with AI assistance!** 🚀
