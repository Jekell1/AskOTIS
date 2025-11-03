"""
Enhanced COBOL RAG Chatbot with OpenAI Integration
=================================================
A more sophisticated version using OpenAI for response generation
"""

import os
import json
import requests
import streamlit as st
from typing import List, Dict, Any, Optional
from datetime import datetime
import openai
from dataclasses import dataclass

# Set up environment
@dataclass 
class Config:
    azure_search_endpoint: str = "https://az-use1-ai-search.search.windows.net"
    azure_search_key: str = "ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef"
    openai_api_key: str = os.getenv("OPENAI_API_KEY", "")
    max_search_results: int = 15
    max_context_length: int = 4000

class COBOLSearchAPI:
    """Interface to Azure AI Search for COBOL code"""
    
    def __init__(self, config: Config):
        self.config = config
        self.api_url = f"{config.azure_search_endpoint}/indexes/cobol-index/docs/search"
        self.headers = {
            'Content-Type': 'application/json',
            'api-key': config.azure_search_key
        }
        
    def search(self, query: str, filter_type: Optional[str] = None) -> Dict[str, Any]:
        """Search COBOL code and return results"""
        
        search_body = {
            "search": query,
            "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
            "top": self.config.max_search_results,
            "count": True
        }
        
        # Add filters for specific symbol types
        if filter_type == "programs":
            search_body["filter"] = "symbol_kind eq 'program'"
        elif filter_type == "variables":
            search_body["filter"] = "symbol_kind eq 'variable'"
        elif filter_type == "procedures":
            search_body["filter"] = "symbol_kind eq 'procedure'"
            
        try:
            response = requests.post(
                self.api_url,
                headers=self.headers,
                params={"api-version": "2024-07-01"},
                json=search_body
            )
            
            if response.status_code == 200:
                return response.json()
            else:
                return {"value": [], "@odata.count": 0, "error": f"HTTP {response.status_code}"}
                
        except Exception as e:
            return {"value": [], "@odata.count": 0, "error": str(e)}

class ConversationMemory:
    """Simple conversation memory management"""
    
    def __init__(self, max_exchanges: int = 5):
        self.max_exchanges = max_exchanges
        self.history: List[Dict[str, str]] = []
        
    def add_exchange(self, user_msg: str, assistant_msg: str):
        """Add a conversation exchange"""
        self.history.append({
            "user": user_msg,
            "assistant": assistant_msg,
            "timestamp": datetime.now().isoformat()
        })
        
        # Keep only recent exchanges
        if len(self.history) > self.max_exchanges:
            self.history = self.history[-self.max_exchanges:]
            
    def get_context(self) -> str:
        """Get conversation context for prompt"""
        if not self.history:
            return ""
            
        context_parts = ["Previous conversation:"]
        for exchange in self.history[-3:]:  # Last 3 exchanges
            context_parts.append(f"User: {exchange['user']}")
            context_parts.append(f"Assistant: {exchange['assistant'][:200]}...")
            
        return "\n".join(context_parts)
        
    def clear(self):
        """Clear conversation history"""
        self.history = []

class COBOLRagAssistant:
    """Main RAG assistant for COBOL code"""
    
    def __init__(self, config: Config):
        self.config = config
        self.search_api = COBOLSearchAPI(config)
        self.memory = ConversationMemory()
        
        # Set up OpenAI
        if config.openai_api_key:
            openai.api_key = config.openai_api_key
        else:
            st.warning("OpenAI API key not set. Using basic responses.")
            
    def generate_response(self, user_query: str) -> str:
        """Generate response using RAG approach"""
        
        # Step 1: Search COBOL codebase
        search_results = self.search_api.search(user_query)
        
        # Step 2: Format context from search results
        context = self._format_search_context(search_results)
        
        # Step 3: Generate response
        if self.config.openai_api_key:
            response = self._generate_openai_response(user_query, context)
        else:
            response = self._generate_basic_response(user_query, search_results)
            
        # Step 4: Update conversation memory
        self.memory.add_exchange(user_query, response)
        
        return response
        
    def _format_search_context(self, search_results: Dict[str, Any]) -> str:
        """Format search results into context for LLM"""
        
        results = search_results.get("value", [])
        total_count = search_results.get("@odata.count", 0)
        
        if not results:
            return "No specific COBOL code found for this query."
            
        context_parts = [
            f"Found {total_count} total matches in the COBOL codebase. Here are the most relevant results:",
            ""
        ]
        
        for i, result in enumerate(results[:10], 1):
            file_path = result.get("repo_path", "Unknown")
            line_num = result.get("line", "N/A")
            code = result.get("code", "").strip()
            symbol_name = result.get("symbol_name", "")
            symbol_kind = result.get("symbol_kind", "")
            
            context_parts.append(f"Result {i}:")
            context_parts.append(f"  File: {file_path}")
            context_parts.append(f"  Line: {line_num}")
            if symbol_name:
                context_parts.append(f"  Symbol: {symbol_name} ({symbol_kind})")
            context_parts.append(f"  Code: {code}")
            context_parts.append("")
            
        return "\n".join(context_parts)
        
    def _generate_openai_response(self, query: str, context: str) -> str:
        """Generate response using OpenAI"""
        
        # Get conversation history
        conversation_context = self.memory.get_context()
        
        system_prompt = """You are a COBOL code expert assistant. You have access to a comprehensive COBOL codebase through search results. Your role is to:

1. Answer questions about COBOL code accurately using the provided search results
2. Explain COBOL concepts clearly, especially for complex logic
3. Show file paths and line numbers when referencing specific code
4. Provide code examples and explanations when helpful
5. Remember previous conversation context for follow-up questions
6. If search results are limited, suggest more specific search terms

Always be helpful, accurate, and reference the specific files and line numbers from the search results."""

        user_prompt = f"""
{conversation_context}

Current Question: {query}

Search Results from COBOL Codebase:
{context}

Please provide a comprehensive answer based on the search results above."""

        try:
            response = openai.ChatCompletion.create(
                model="gpt-3.5-turbo",
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt}
                ],
                max_tokens=1000,
                temperature=0.1  # Low temperature for more factual responses
            )
            
            return response.choices[0].message.content.strip()
            
        except Exception as e:
            return f"Error generating response: {e}\n\nFalling back to basic response:\n{self._generate_basic_response(query, {'value': [], '@odata.count': 0})}"
            
    def _generate_basic_response(self, query: str, search_results: Dict[str, Any]) -> str:
        """Generate basic response without OpenAI"""
        
        results = search_results.get("value", [])
        total_count = search_results.get("@odata.count", 0)
        error = search_results.get("error")
        
        if error:
            return f"❌ Search error: {error}\n\nPlease try rephrasing your query or check the system status."
            
        if not results:
            return f"""🔍 No specific COBOL code found for "{query}".

Try these approaches:
• Use specific COBOL keywords (PROGRAM-ID, IDENTIFICATION, PROCEDURE)
• Search for variable names or program names
• Use broader terms like "CUSTOMER" instead of exact phrases
• Try different symbol types: programs, variables, procedures"""
            
        response_parts = [
            f"🎯 Found {total_count} matches for '{query}' in the COBOL codebase:",
            ""
        ]
        
        # Group by file for better organization
        files = {}
        for result in results:
            file_path = result.get("repo_path", "Unknown")
            if file_path not in files:
                files[file_path] = []
            files[file_path].append(result)
            
        for file_path, file_results in list(files.items())[:5]:  # Show top 5 files
            response_parts.append(f"📁 **{file_path}**")
            
            for result in file_results[:3]:  # Max 3 results per file
                line = result.get("line", "N/A")
                code = result.get("code", "").strip()
                symbol_name = result.get("symbol_name", "")
                symbol_kind = result.get("symbol_kind", "")
                
                if symbol_name:
                    response_parts.append(f"   • Line {line}: `{symbol_name}` ({symbol_kind})")
                else:
                    response_parts.append(f"   • Line {line}")
                    
                if code:
                    response_parts.append(f"     ```cobol\n     {code}\n     ```")
                    
                response_parts.append("")
                
        response_parts.append(f"💡 **Tip**: Ask follow-up questions about specific files or symbols for more details.")
        
        return "\n".join(response_parts)
        
    def clear_memory(self):
        """Clear conversation memory"""
        self.memory.clear()

def main():
    """Main Streamlit application"""
    
    st.set_page_config(
        page_title="COBOL RAG Assistant",
        page_icon="🔍",
        layout="wide"
    )
    
    st.title("🔍 COBOL Code Assistant")
    st.subheader("RAG-Powered Chatbot with Memory & Search")
    
    # Initialize configuration
    config = Config()
    
    # Sidebar
    with st.sidebar:
        st.markdown("### 📊 Codebase Stats")
        col1, col2 = st.columns(2)
        with col1:
            st.metric("Documents", "1.31M")
            st.metric("Files", "9,952")
        with col2:
            st.metric("Programs", "~800")
            st.metric("Variables", "~45K")
            
        st.markdown("### 🎯 Quick Examples")
        examples = [
            "Show me all CUSTOMER programs",
            "Find DATE variables", 
            "Search IDENTIFICATION DIVISION",
            "What calls program ACUMEM?",
            "Find PROCEDURE DIVISION examples"
        ]
        
        selected_example = st.selectbox("Try an example:", [""] + examples)
        if selected_example:
            st.session_state.example_query = selected_example
            
        # Memory controls
        st.markdown("### 🧠 Memory")
        if st.button("Clear Conversation"):
            if "assistant" in st.session_state:
                st.session_state.assistant.clear_memory()
            st.session_state.messages = []
            st.rerun()
            
        # OpenAI API key input
        st.markdown("### ⚙️ Settings")
        openai_key = st.text_input("OpenAI API Key (optional)", type="password")
        if openai_key:
            config.openai_api_key = openai_key
    
    # Initialize assistant
    if "assistant" not in st.session_state:
        st.session_state.assistant = COBOLRagAssistant(config)
        
    # Initialize messages
    if "messages" not in st.session_state:
        st.session_state.messages = []
        welcome_msg = """👋 Hello! I'm your **COBOL Code Assistant** with RAG capabilities.

I can help you:
• 🔍 **Search** through 1.3M+ lines of COBOL code
• 📝 **Find** specific programs, variables, and procedures  
• 💡 **Explain** COBOL code patterns and logic
• 🔗 **Trace** relationships between code components
• 💬 **Remember** our conversation for follow-ups

**What would you like to explore in the COBOL codebase?**"""
        
        st.session_state.messages.append({
            "role": "assistant",
            "content": welcome_msg
        })
    
    # Display chat messages
    for message in st.session_state.messages:
        with st.chat_message(message["role"]):
            st.markdown(message["content"])
    
    # Handle example query
    if "example_query" in st.session_state:
        query = st.session_state.example_query
        del st.session_state.example_query
    else:
        query = st.chat_input("Ask about the COBOL code...")
    
    # Process user input
    if query:
        # Add user message
        st.session_state.messages.append({"role": "user", "content": query})
        with st.chat_message("user"):
            st.markdown(query)
            
        # Generate and display response
        with st.chat_message("assistant"):
            with st.spinner("Searching COBOL codebase and generating response..."):
                try:
                    # Update assistant config if OpenAI key was provided
                    if openai_key:
                        st.session_state.assistant.config.openai_api_key = openai_key
                        
                    response = st.session_state.assistant.generate_response(query)
                    st.markdown(response)
                    
                    # Add to message history
                    st.session_state.messages.append({
                        "role": "assistant",
                        "content": response
                    })
                    
                except Exception as e:
                    error_msg = f"❌ I encountered an error: {e}\n\nPlease try rephrasing your question or check your API keys."
                    st.error(error_msg)
                    st.session_state.messages.append({
                        "role": "assistant",
                        "content": error_msg
                    })

if __name__ == "__main__":
    main()
