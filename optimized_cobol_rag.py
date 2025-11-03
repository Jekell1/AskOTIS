#!/usr/bin/env python3
"""
Performance-Optimized COBOL RAG - Smart Context Management
========================================================
Optimized version that intelligently manages context size for faster responses
"""

import json
import requests
import time
from typing import List, Dict, Any, Tuple

def load_local_settings():
    """Load configuration from local.settings.json"""
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except Exception as e:
        print(f"Error loading settings: {e}")
        return {}

class OptimizedCOBOLRAG:
    """Performance-optimized COBOL RAG with smart context management"""
    
    def __init__(self):
        settings = load_local_settings()
        
        self.search_endpoint = settings.get("SEARCH_ENDPOINT")
        self.search_key = settings.get("SEARCH_KEY")
        self.search_index = "cobol-index"
        
        self.azure_openai_endpoint = settings.get("AZURE_OPENAI_ENDPOINT")
        self.azure_openai_key = settings.get("AZURE_OPENAI_KEY")
        self.azure_openai_deployment = settings.get("AZURE_OPENAI_DEPLOYMENT")
        
        self.api_url = f"{self.search_endpoint}/indexes/{self.search_index}/docs/search"
        self.headers = {
            'Content-Type': 'application/json',
            'api-key': self.search_key
        }
    
    def smart_search_and_analyze(self, user_query: str) -> str:
        """Intelligently search and analyze based on query type"""
        
        start_time = time.time()
        
        # Step 1: Parse query type and optimize search strategy
        search_strategy = self._determine_search_strategy(user_query)
        print(f"🎯 Strategy: {search_strategy['type']}")
        
        # Step 2: Execute optimized search
        search_time_start = time.time()
        search_results = self._execute_optimized_search(search_strategy)
        search_time = time.time() - search_time_start
        print(f"🔍 Search completed in {search_time:.2f}s - {len(search_results['value'])} results")
        
        # Step 3: Intelligent context preparation
        context_time_start = time.time()
        optimized_context = self._prepare_smart_context(search_results, search_strategy)
        context_time = time.time() - context_time_start
        print(f"📝 Context prepared in {context_time:.2f}s - {len(optimized_context['context'])} chars")
        
        # Step 4: Fast LLM analysis
        llm_time_start = time.time()
        response = self._fast_llm_analysis(user_query, optimized_context, search_strategy)
        llm_time = time.time() - llm_time_start
        print(f"🤖 LLM analysis in {llm_time:.2f}s")
        
        total_time = time.time() - start_time
        
        # Add performance footer
        perf_info = f"\n\n⚡ **Performance**: Search {search_time:.1f}s + Context {context_time:.1f}s + AI {llm_time:.1f}s = **{total_time:.1f}s total**"
        
        return response + perf_info
    
    def _determine_search_strategy(self, user_query: str) -> Dict[str, Any]:
        """Determine optimal search strategy based on query"""
        
        query_lower = user_query.lower()
        
        # Pattern 1: Specific program analysis (like "What does LONPF2 do?")
        if any(phrase in query_lower for phrase in ['what does', 'what is']) and any(char.isupper() for char in user_query):
            # Extract program name
            import re
            program_matches = re.findall(r'\b[A-Z][A-Z0-9]{3,15}\b', user_query)
            if program_matches:
                program_name = program_matches[0]
                return {
                    'type': 'specific_program_analysis',
                    'program_name': program_name,
                    'max_results': 50,  # Focused results
                    'priority_sections': ['IDENTIFICATION', 'PROCEDURE', 'PROGRAM-ID'],
                    'context_limit': 5000  # Reasonable context size
                }
        
        # Pattern 2: Broad search queries
        if any(phrase in query_lower for phrase in ['find', 'search', 'show', 'list']):
            return {
                'type': 'broad_search',
                'max_results': 20,  # Reasonable for broad searches
                'context_limit': 3000
            }
        
        # Pattern 3: Concept explanations
        if any(phrase in query_lower for phrase in ['explain', 'describe', 'how']):
            return {
                'type': 'concept_explanation',
                'max_results': 30,
                'context_limit': 4000
            }
        
        # Default: Balanced approach
        return {
            'type': 'balanced_analysis',
            'max_results': 25,
            'context_limit': 4000
        }
    
    def _execute_optimized_search(self, strategy: Dict[str, Any]) -> Dict[str, Any]:
        """Execute search optimized for the strategy"""
        
        search_body = {
            "search": strategy.get('program_name', strategy.get('query', '')),
            "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
            "top": strategy['max_results'],
            "count": True
        }
        
        # Add intelligent filtering for specific program analysis
        if strategy['type'] == 'specific_program_analysis':
            program_name = strategy['program_name']
            # Search for the specific program with high relevance
            search_body['search'] = f'"{program_name}" OR PROGRAM-ID:{program_name}'
            search_body['searchMode'] = 'all'  # More precise matching
        
        try:
            response = requests.post(
                self.api_url,
                headers=self.headers,
                params={"api-version": "2024-07-01"},
                json=search_body,
                timeout=10  # Shorter timeout for speed
            )
            
            if response.status_code == 200:
                return response.json()
            else:
                return {"value": [], "@odata.count": 0, "error": f"HTTP {response.status_code}"}
                
        except Exception as e:
            return {"value": [], "@odata.count": 0, "error": str(e)}
    
    def _prepare_smart_context(self, search_results: Dict[str, Any], strategy: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare optimized context based on strategy"""
        
        results = search_results.get("value", [])
        context_limit = strategy['context_limit']
        
        if strategy['type'] == 'specific_program_analysis':
            return self._prepare_program_focused_context(results, strategy, context_limit)
        else:
            return self._prepare_general_context(results, context_limit)
    
    def _prepare_program_focused_context(self, results: List[Dict], strategy: Dict[str, Any], limit: int) -> Dict[str, Any]:
        """Prepare context focused on specific program analysis"""
        
        program_name = strategy['program_name']
        priority_sections = strategy.get('priority_sections', [])
        
        # Categorize results by importance
        high_priority = []
        medium_priority = []
        low_priority = []
        
        program_file = None
        
        for result in results:
            file_path = result.get("repo_path", "")
            code = result.get("code", "")
            symbol_name = result.get("symbol_name", "")
            
            # Highest priority: Direct program file match
            if program_name.lower() in file_path.lower() and file_path.endswith('.CBL'):
                program_file = file_path
                high_priority.append(result)
            # High priority: Program definition and key sections
            elif any(section in code.upper() for section in priority_sections):
                high_priority.append(result)
            # Medium priority: Same symbol name
            elif symbol_name == program_name:
                medium_priority.append(result)
            # Lower priority: Contains program name
            elif program_name in code.upper():
                low_priority.append(result)
        
        # Build context intelligently within limits
        context_parts = [f"=== ANALYSIS OF PROGRAM {program_name} ===\n"]
        current_length = len(context_parts[0])
        
        if program_file:
            context_parts.append(f"Main Program File: {program_file}\n")
            current_length += len(context_parts[-1])
        
        # Add high priority content first
        for result in high_priority[:10]:  # Limit high priority items
            section = self._format_result_section(result)
            if current_length + len(section) < limit:
                context_parts.append(section)
                current_length += len(section)
            else:
                break
        
        # Add medium priority if space allows
        for result in medium_priority[:5]:
            section = self._format_result_section(result)
            if current_length + len(section) < limit:
                context_parts.append(section)
                current_length += len(section)
            else:
                break
        
        return {
            'context': '\n'.join(context_parts),
            'program_file': program_file,
            'total_results': len(results),
            'high_priority_count': len(high_priority),
            'context_length': current_length
        }
    
    def _prepare_general_context(self, results: List[Dict], limit: int) -> Dict[str, Any]:
        """Prepare general context for non-program-specific queries"""
        
        context_parts = ["=== COBOL CODE ANALYSIS ===\n"]
        current_length = len(context_parts[0])
        
        # Group by file and take top examples
        files = {}
        for result in results:
            file_path = result.get("repo_path", "Unknown")
            if file_path not in files:
                files[file_path] = []
            files[file_path].append(result)
        
        # Add context from multiple files, but limited per file
        for file_path, file_results in list(files.items())[:5]:  # Max 5 files
            file_section = f"\n--- FILE: {file_path} ---\n"
            if current_length + len(file_section) >= limit:
                break
                
            context_parts.append(file_section)
            current_length += len(file_section)
            
            # Add top 3 results from this file
            for result in file_results[:3]:
                section = self._format_result_section(result)
                if current_length + len(section) < limit:
                    context_parts.append(section)
                    current_length += len(section)
                else:
                    break
        
        return {
            'context': '\n'.join(context_parts),
            'total_results': len(results),
            'files_included': len(files),
            'context_length': current_length
        }
    
    def _format_result_section(self, result: Dict[str, Any]) -> str:
        """Format a single result into a context section"""
        
        line = result.get("line", "N/A")
        code = result.get("code", "").strip()
        symbol_name = result.get("symbol_name", "")
        symbol_kind = result.get("symbol_kind", "")
        calls = result.get("calls", [])
        
        # Limit individual code sections to reasonable size
        if len(code) > 500:
            code = code[:500] + "... (truncated)"
        
        section_parts = [f"Line {line}: {code}"]
        
        if symbol_name and symbol_kind:
            section_parts.append(f"  [Symbol: {symbol_name} ({symbol_kind})]")
        if calls:
            section_parts.append(f"  [Calls: {', '.join(calls[:3])}]")  # Limit calls shown
        
        return '\n'.join(section_parts) + '\n'
    
    def _fast_llm_analysis(self, user_query: str, context: Dict[str, Any], strategy: Dict[str, Any]) -> str:
        """Fast LLM analysis with optimized prompt"""
        
        # Create focused system prompt based on strategy
        if strategy['type'] == 'specific_program_analysis':
            system_prompt = """You are a COBOL expert. Analyze the provided program code and answer the user's question concisely but thoroughly. 

Focus on:
1. What the program does (main purpose)
2. Key data structures and variables
3. Main processing logic
4. Important program calls or dependencies

Keep your response focused and well-structured. The context has been pre-filtered to show the most relevant code sections."""
        else:
            system_prompt = """You are a COBOL expert. Analyze the provided code snippets and answer the user's question clearly and concisely.

Provide a focused response that directly addresses the question. The context has been optimized to show the most relevant matches."""
        
        user_prompt = f"""Please analyze this COBOL code and answer the question.

QUESTION: {user_query}

RELEVANT COBOL CODE:
{context['context']}

Provide a clear, focused answer based on the code shown above."""
        
        try:
            from openai import AzureOpenAI
            
            client = AzureOpenAI(
                api_key=self.azure_openai_key,
                api_version="2024-12-01-preview",
                azure_endpoint=self.azure_openai_endpoint
            )
            
            response = client.chat.completions.create(
                model=self.azure_openai_deployment,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt}
                ],
                max_completion_tokens=800,  # Reasonable limit for faster response
                timeout=30  # Shorter timeout
            )
            
            llm_response = response.choices[0].message.content.strip()
            
            # Add smart footer with context info
            footer = self._create_smart_footer(context, strategy)
            
            return llm_response + footer
            
        except Exception as e:
            return f"❌ **LLM Analysis Error**: {str(e)}\n\nSearch found {context['total_results']} results but AI analysis failed."
    
    def _create_smart_footer(self, context: Dict[str, Any], strategy: Dict[str, Any]) -> str:
        """Create intelligent footer with context information"""
        
        total_results = context['total_results']
        context_length = context['context_length']
        
        if strategy['type'] == 'specific_program_analysis':
            program_name = strategy['program_name']
            high_priority = context.get('high_priority_count', 0)
            program_file = context.get('program_file')
            
            footer_parts = [
                f"\n\n🎯 **Program Analysis: {program_name}**",
                f"📊 Analyzed {high_priority} key sections from {total_results} total matches"
            ]
            
            if program_file:
                footer_parts.append(f"📁 Main file: {program_file}")
            
            footer_parts.extend([
                f"📝 Context: {context_length:,} characters (optimized for speed)",
                f"\n💡 **Follow-up**: Ask about specific aspects like variables, calls, or procedures in {program_name}"
            ])
        else:
            files_included = context.get('files_included', 1)
            footer_parts = [
                f"\n\n📊 **Analysis Summary**",
                f"🔍 Found {total_results} matches across {files_included} files",
                f"📝 Analyzed {context_length:,} characters of optimized context",
                f"\n💡 **Follow-up**: Ask more specific questions for deeper analysis"
            ]
        
        return '\n'.join(footer_parts)

def main():
    """Test the optimized RAG system"""
    
    rag = OptimizedCOBOLRAG()
    
    print("🚀 Testing Optimized COBOL RAG Performance")
    print("=" * 50)
    
    test_queries = [
        "What does LONPF2 do?",
        "What does CUSTOMER do?",
        "Find IDENTIFICATION DIVISION examples"
    ]
    
    for query in test_queries:
        print(f"\n🔍 Testing: '{query}'")
        print("-" * 30)
        
        start_time = time.time()
        response = rag.smart_search_and_analyze(query)
        total_time = time.time() - start_time
        
        print(f"\n📝 Response Preview:")
        print(response[:200] + "..." if len(response) > 200 else response)
        print(f"\n⏱️ **TOTAL TIME: {total_time:.2f} seconds**")
        print("=" * 50)

if __name__ == "__main__":
    main()
