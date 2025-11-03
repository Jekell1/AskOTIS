"""
Simple Code RAG System
A clean, focused RAG system for answering questions about indexed code files.
"""

import json
import requests
import streamlit as st
import os
from typing import List, Dict, Any, Optional
from openai import AzureOpenAI

class SimpleCodeRAG:
    """Simple RAG system for code analysis"""
    
    def __init__(self):
        """Initialize the RAG system with Azure configuration"""
        self.config = self._load_config()
        self.search_endpoint = self.config.get("SEARCH_ENDPOINT")
        self.search_key = self.config.get("SEARCH_KEY")
        self.search_index = "cobol-index"
        
        # Azure OpenAI configuration
        self.azure_openai_endpoint = self.config.get("AZURE_OPENAI_ENDPOINT")
        self.azure_openai_key = self.config.get("AZURE_OPENAI_KEY")
        self.azure_openai_deployment = self.config.get("AZURE_OPENAI_DEPLOYMENT")
        
        # Validate configuration
        self._validate_config()
    
    def _load_config(self) -> Dict[str, str]:
        """Load configuration from local.settings.json"""
        try:
            with open('local.settings.json', 'r') as f:
                settings = json.load(f)
                return settings.get('Values', {})
        except FileNotFoundError:
            st.error("❌ local.settings.json not found")
            return {}
        except Exception as e:
            st.error(f"❌ Error loading configuration: {e}")
            return {}
    
    def _validate_config(self):
        """Validate required configuration"""
        required_fields = [
            ("SEARCH_ENDPOINT", self.search_endpoint),
            ("SEARCH_KEY", self.search_key),
            ("AZURE_OPENAI_ENDPOINT", self.azure_openai_endpoint),
            ("AZURE_OPENAI_KEY", self.azure_openai_key),
            ("AZURE_OPENAI_DEPLOYMENT", self.azure_openai_deployment)
        ]
        
        for field_name, field_value in required_fields:
            if not field_value:
                st.error(f"❌ Missing configuration: {field_name}")
                return False
        
        return True
    
    def search_code(self, query: str, max_results: int = 5) -> Dict[str, Any]:
        """Search the code index using Azure AI Search"""
        if not self.search_endpoint or not self.search_key:
            return {"value": []}
        
        url = f"{self.search_endpoint}/indexes/{self.search_index}/docs"
        
        headers = {
            "Content-Type": "application/json",
            "api-key": self.search_key
        }
        
        params = {
            "api-version": "2023-11-01",
            "search": query,
            "top": max_results,
            "queryType": "simple"
        }
        
        try:
            response = requests.get(url, headers=headers, params=params)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            st.error(f"Search error: {e}")
            return {"value": []}
    
    def _get_openai_client(self) -> Optional[AzureOpenAI]:
        """Get Azure OpenAI client"""
        if not all([self.azure_openai_endpoint, self.azure_openai_key, self.azure_openai_deployment]):
            return None
        
        try:
            return AzureOpenAI(
                azure_endpoint=self.azure_openai_endpoint,
                api_key=self.azure_openai_key,
                api_version="2024-05-01-preview"
            )
        except Exception as e:
            st.error(f"Failed to create OpenAI client: {e}")
            return None
    

    
    def _create_compact_context(self, search_results: List[Dict[str, Any]]) -> str:
        """Create a very compact context from search results"""
        if not search_results:
            return "<CONTEXT>\nNo relevant code found.\n</CONTEXT>"
        
        context_parts = ["<CONTEXT>"]
        
        # Only use top 2 results to keep it small
        for i, result in enumerate(search_results[:2], 1):
            repo_path = result.get('repo_path', 'Unknown')
            
            # Extract just the most important code lines
            code_raw = result.get('code', '')
            code_lines = []
            
            if isinstance(code_raw, str) and code_raw.strip().startswith('['):
                try:
                    parsed_lines = json.loads(code_raw)
                    if isinstance(parsed_lines, list):
                        code_lines = parsed_lines
                except json.JSONDecodeError:
                    code_lines = [code_raw[:200]]
            else:
                code_lines = str(code_raw).split('\n')
            
            # Find most important lines (PROGRAM-ID, DESCRIPTION, key patterns)
            important_lines = []
            for line in code_lines[:20]:  # Only check first 20 lines
                line_upper = line.upper().strip()
                if any(pattern in line_upper for pattern in [
                    'PROGRAM-ID', 'DESCRIPTION', 'CALLED FROM', 'USED TO', 
                    'PROCEDURE DIVISION', 'WORKING-STORAGE'
                ]):
                    important_lines.append(line.strip())
                if len(important_lines) >= 3:  # Max 3 lines per result
                    break
            
            if important_lines:
                # Extract filename from path for clarity
                filename = repo_path.split('/')[-1] if '/' in repo_path else repo_path
                context_parts.append(f"[S{i}] FILE: {filename} (Path: {repo_path})")
                context_parts.extend(important_lines)
                context_parts.append("")
        
        context_parts.append("</CONTEXT>")
        return '\n'.join(context_parts)

    def _extract_search_terms(self, user_query: str) -> str:
        """Extract the most relevant search terms from user query"""
        import re
        
        # Look for specific file names (*.CBL, *.PY, etc.)
        file_pattern = r'(\w+\.\w+)'
        files = re.findall(file_pattern, user_query.upper())
        if files:
            return files[0].replace('.CBL', '')  # Return program name without extension
        
        # Look for program names in quotes or specific mentions
        program_pattern = r'\b([A-Z][A-Z0-9]{3,})\b'
        programs = re.findall(program_pattern, user_query.upper())
        if programs:
            return programs[0]
        
        # Fallback: use the original query
        return user_query

    def _call_llm_with_context(self, user_query: str) -> str:
        """Call LLM with compact search context"""
        client = self._get_openai_client()
        if not client:
            return "❌ LLM not available."
        
        # Extract better search terms from the user query
        search_terms = self._extract_search_terms(user_query)
        
        # Search for relevant code using extracted terms
        search_results = self.search_code(search_terms, max_results=5)
        results = search_results.get('value', [])
        
        if not results:
            return "❌ No relevant code found for your question."
        
        # Create compact context
        context = self._create_compact_context(results)
        
        # More flexible system prompt
        system_prompt = f"""You are an expert code analyst. Answer questions using the retrieved data below.
Always cite sources using [S1], [S2], etc. Be helpful and provide detailed analysis when the data supports it.
Only say "Not enough information" if there truly isn't relevant data to answer the question.

{context}"""
        
        try:
            response = client.chat.completions.create(
                model=self.azure_openai_deployment,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_query}
                ],
                timeout=15  # Shorter timeout
            )
            
            return response.choices[0].message.content
            
        except Exception as e:
            return f"❌ Error calling LLM: {e}"
    

    
    def ask_question(self, query: str) -> str:
        """Main method to ask a question about the codebase"""
        # Use compact context approach - focused and efficient
        return self._call_llm_with_context(query)
    
    def get_status(self) -> Dict[str, Any]:
        """Get system status"""
        status = {
            "search_configured": bool(self.search_endpoint and self.search_key),
            "llm_configured": bool(self.azure_openai_endpoint and self.azure_openai_key and self.azure_openai_deployment),
            "search_index": self.search_index
        }
        
        # Test search connectivity
        if status["search_configured"]:
            try:
                test_results = self.search_code("test", max_results=1)
                status["search_working"] = "value" in test_results
            except:
                status["search_working"] = False
        
        # Test LLM connectivity  
        if status["llm_configured"]:
            client = self._get_openai_client()
            status["llm_working"] = client is not None
        
        return status


def main():
    """Streamlit app main function"""
    st.set_page_config(
        page_title="Simple Code RAG", 
        page_icon="🔍",
        layout="wide"
    )
    
    st.title("🔍 Simple Code RAG")
    st.subheader("Ask questions about your indexed code")
    
    # Initialize RAG system
    if "rag_system" not in st.session_state:
        st.session_state.rag_system = SimpleCodeRAG()
    
    rag = st.session_state.rag_system
    
    # Sidebar with status
    with st.sidebar:
        st.markdown("### ⚙️ System Status")
        status = rag.get_status()
        
        # Search status
        search_status = "✅" if status.get("search_working") else "❌"
        st.markdown(f"**Search:** {search_status}")
        st.markdown(f"Index: `{status.get('search_index', 'N/A')}`")
        
        # LLM status
        llm_status = "✅" if status.get("llm_working") else "❌"
        st.markdown(f"**LLM:** {llm_status}")
        
        if status.get("search_working") and status.get("llm_working"):
            st.success("🎉 System ready!")
        else:
            st.error("⚠️ Configuration needed")
    
    # Main interface
    st.markdown("### 💬 Ask a Question")
    
    # Example questions
    with st.expander("💡 Example Questions"):
        examples = [
            "What does ACUMEM.CBL do?",
            "Show me programs that handle SQL",
            "Find functions related to memory management",
            "What are the main COBOL programs in this codebase?",
            "Explain how error handling works",
        ]
        for example in examples:
            if st.button(example, key=f"example_{example}"):
                st.session_state.user_query = example
    
    # User input
    user_query = st.text_input(
        "Enter your question:",
        key="query_input",
        value=st.session_state.get("user_query", ""),
        placeholder="e.g., What does ACUMEM.CBL do?"
    )
    
    # Clear the session state after using it
    if "user_query" in st.session_state:
        del st.session_state.user_query
    
    if st.button("🔍 Ask Question", type="primary") and user_query:
        with st.spinner("Searching and analyzing..."):
            try:
                response = rag.ask_question(user_query)
                
                st.markdown("### 📝 Answer")
                st.markdown(response)
                
                # Show search info
                search_results = rag.search_code(user_query, max_results=5)
                result_count = len(search_results.get('value', []))
                st.info(f"Based on {result_count} search results from the code index.")
                
            except Exception as e:
                st.error(f"❌ Error: {e}")
    
    # Instructions
    st.markdown("---")
    st.markdown("### 📖 How to Use")
    st.markdown("""
    1. **Ask natural language questions** about your codebase
    2. **Get AI-powered answers** based on indexed code
    3. **Sources are cited** with [S1], [S2] references
    4. **Works with any code** - COBOL, Python, JavaScript, etc.
    
    The system searches your indexed code and uses AI to provide accurate, contextual answers.
    """)


if __name__ == "__main__":
    main()
