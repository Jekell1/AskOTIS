#!/usr/bin/env python3
"""
Simple COBOL RAG Test - Debug Version
"""

import json
import requests
import streamlit as st
import os
from typing import List, Dict, Any, Optional
from datetime import datetime

# Load configuration from local.settings.json
def load_local_settings():
    """Load configuration from local.settings.json"""
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except FileNotFoundError:
        return {}
    except Exception as e:
        print(f"Error loading local.settings.json: {e}")
        return {}

# Load local settings
LOCAL_SETTINGS = load_local_settings()

# Configuration  
AZURE_SEARCH_ENDPOINT = LOCAL_SETTINGS.get("SEARCH_ENDPOINT", "https://az-use1-ai-search.search.windows.net")
AZURE_SEARCH_KEY = LOCAL_SETTINGS.get("SEARCH_KEY", "ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef")
AZURE_SEARCH_INDEX = "cobol-index"

class SimpleCOBOLRAG:
    """Simple RAG implementation for COBOL code - Debug version"""
    
    def __init__(self):
        self.api_url = f"{AZURE_SEARCH_ENDPOINT}/indexes/{AZURE_SEARCH_INDEX}/docs/search"
        self.headers = {
            'Content-Type': 'application/json',
            'api-key': AZURE_SEARCH_KEY
        }
        
    def search_cobol(self, query: str, max_results: int = 25) -> Dict:
        """Search COBOL codebase"""
        
        search_body = {
            "search": query,
            "select": "repo_path,code",
            "top": max_results,
            "count": True
        }
        
        try:
            response = requests.post(
                self.api_url,
                headers=self.headers,
                params={"api-version": "2024-07-01"},
                json=search_body,
                timeout=15  # 15 second timeout
            )
            
            if response.status_code == 200:
                return response.json()
            else:
                return {"value": [], "@odata.count": 0, "error": f"HTTP {response.status_code}"}
                
        except Exception as e:
            return {"value": [], "@odata.count": 0, "error": str(e)}
    
    def generate_response(self, user_query: str, max_results: int = 25) -> str:
        """Generate simple response without LLM"""
        
        # Search for relevant code
        search_results = self.search_cobol(user_query, max_results=max_results)
        
        results = search_results.get("value", [])
        total_count = search_results.get("@odata.count", 0)
        error = search_results.get("error")
        
        if error:
            return f"❌ **Search Error**: {error}\\n\\nPlease try rephrasing your query."
            
        if not results:
            return f"🔍 **No results found** for '{user_query}'\\n\\nTry:\\n• Using different keywords\\n• Searching for COBOL terms like PROCEDURE DIVISION\\n• Looking for specific program names"
        
        # Format results without LLM
        response_parts = []
        response_parts.append(f"🔍 **Found {total_count:,} matches** for '{user_query}'")
        response_parts.append(f"\\n**Top {len(results)} results:**\\n")
        
        # Group by files
        files = {}
        for result in results[:10]:  # Show top 10
            file_path = result.get("repo_path", "Unknown")
            if file_path not in files:
                files[file_path] = []
            files[file_path].append(result)
            
        for file_path, file_results in files.items():
            response_parts.append(f"\\n**📄 {file_path.split('/')[-1]}**")
            for result in file_results[:2]:  # Top 2 per file
                code = result.get("code", "").strip()
                code_preview = code[:300] if len(code) > 300 else code
                response_parts.append(f"```cobol\\n{code_preview}\\n```")
                
        if total_count > len(results):
            response_parts.append(f"\\n*...and {total_count - len(results):,} more matches*")
            
        return "\\n".join(response_parts)

def main():
    """Streamlit app main function"""
    
    st.set_page_config(
        page_title="COBOL RAG Debug", 
        page_icon="🔍",
        layout="wide"
    )
    
    st.title("🔍 COBOL RAG Debug Chatbot")
    st.markdown("*Debug version - No LLM calls*")
    
    # Initialize session state
    if "messages" not in st.session_state:
        st.session_state.messages = []
        
    if "rag_system" not in st.session_state:
        with st.spinner("Initializing COBOL search..."):
            st.session_state.rag_system = SimpleCOBOLRAG()
    
    # Sidebar
    with st.sidebar:
        st.header("🎛️ Debug Controls")
        max_results = st.slider("Max Results", 5, 100, 25)
        
        if st.button("🧪 Test Search"):
            with st.spinner("Testing..."):
                test_result = st.session_state.rag_system.search_cobol("PROCEDURE DIVISION", max_results=3)
                if "error" in test_result:
                    st.error(f"❌ Search failed: {test_result['error']}")
                else:
                    count = test_result.get("@odata.count", 0)
                    results = len(test_result.get("value", []))
                    st.success(f"✅ Search works: {results} results of {count:,} total")
        
        st.markdown("---")
        st.markdown("**Try these queries:**")
        if st.button("PROCEDURE DIVISION", key="proc"):
            st.session_state.example_query = "PROCEDURE DIVISION"
            st.experimental_rerun()
        if st.button("WORKING-STORAGE", key="work"):
            st.session_state.example_query = "WORKING-STORAGE SECTION"
            st.experimental_rerun()
        if st.button("CALL statements", key="call"):
            st.session_state.example_query = "CALL"
            st.experimental_rerun()
    
    # Welcome message
    if not st.session_state.messages:
        welcome_msg = """👋 **Welcome to the COBOL RAG Debug Chatbot!**

This is a debug version that tests search without LLM calls.

**Try asking:**
• "Find PROCEDURE DIVISION examples"
• "Show WORKING-STORAGE sections" 
• "Search for CALL statements"
• "Find program LONPF2"

**What would you like to search for?**"""
        
        st.session_state.messages.append({
            "role": "assistant",
            "content": welcome_msg
        })
    
    # Display chat messages
    for message in st.session_state.messages:
        with st.chat_message(message["role"]):
            st.markdown(message["content"])
    
    # Handle example query from sidebar
    if "example_query" in st.session_state:
        query = st.session_state.example_query
        del st.session_state.example_query
    else:
        query = st.chat_input("Search the COBOL codebase...")
    
    # Process user input
    if query:
        # Add user message to chat
        st.session_state.messages.append({"role": "user", "content": query})
        with st.chat_message("user"):
            st.markdown(query)
            
        # Generate response
        with st.chat_message("assistant"):
            with st.spinner("🔍 Searching COBOL codebase..."):
                try:
                    response = st.session_state.rag_system.generate_response(query, max_results=max_results)
                    st.markdown(response)
                    
                    # Add to message history
                    st.session_state.messages.append({
                        "role": "assistant",
                        "content": response
                    })
                    
                except Exception as e:
                    error_msg = f"❌ **Error**: {e}\\n\\nTry refreshing or check connectivity."
                    st.error(error_msg)
                    st.session_state.messages.append({
                        "role": "assistant", 
                        "content": error_msg
                    })

if __name__ == "__main__":
    main()
