"""Response Generator - Creates answers using LLM with retrieved context."""

import logging
import tiktoken
from typing import List, Dict
from openai import AzureOpenAI
from .prompts import get_enhanced_system_prompt

# Set up logging
logger = logging.getLogger(__name__)


class ResponseGenerator:
    """Generates intelligent responses using LLM with retrieved context."""
    
    def __init__(self, config):
        """Initialize generator with configuration."""
        self.config = config
        
        # Initialize Azure OpenAI for chat
        self.openai_client = AzureOpenAI(
            api_key=config.openai_key,
            api_version="2024-08-01-preview",
            azure_endpoint=config.openai_endpoint
        )
    
    def generate(
        self,
        query: str,
        context_docs: List[Dict],
        conversation_context: str = "",
        is_otis: bool = False,
        question_type: str = "general"
    ) -> str:
        """Generate response using LLM.
        
        Args:
            query: User's question
            context_docs: Retrieved documents
            conversation_context: Recent conversation history
            is_otis: Whether question is about OTIS application
            question_type: Type of question
        
        Returns:
            Generated response
        """
        # Build prompt
        prompt = self._build_prompt(
            query=query,
            context_docs=context_docs,
            conversation_context=conversation_context,
            is_otis=is_otis,
            question_type=question_type
        )
        
        # Call LLM
        try:
            import time
            
            # Optimize max_tokens based on question type
            # Menu/list questions: 2000 tokens (~1500 words) is plenty
            # Trace/flow questions: need more for detailed analysis
            # Copybook usage questions: need lots for complete lists
            if question_type in ('menu', 'list', 'simple'):
                max_response_tokens = 2000
            elif question_type == 'trace_flow':
                max_response_tokens = 15000
            elif question_type == 'copybook_usage':
                max_response_tokens = 16000  # Allow full listing of all programs
            else:
                max_response_tokens = 4000  # Default for most questions
            
            logger.info(f"⏱️ Generating response with max_tokens={max_response_tokens}, question_type={question_type}")
            
            start_llm = time.time()
            response = self.openai_client.chat.completions.create(
                model=self.config.chat_deployment,
                messages=[
                    {"role": "system", "content": self._get_system_prompt(is_otis)},
                    {"role": "user", "content": prompt}
                ],
                temperature=self.config.temperature,
                max_tokens=max_response_tokens
            )
            time_llm = time.time() - start_llm
            
            result = response.choices[0].message.content
            tokens_used = response.usage.total_tokens if hasattr(response, 'usage') else 'unknown'
            logger.info(f"⏱️ TIMING: LLM call took {time_llm:.3f}s (tokens: {tokens_used})")
            
            # Post-process to add file hyperlinks
            result = self._add_file_hyperlinks(result)
            
            return result
        
        except Exception as e:
            return f"❌ Error generating response: {str(e)}"
    
    def _estimate_tokens(self, text: str) -> int:
        """Estimate the number of tokens in text using tiktoken.
        
        Uses the appropriate encoding for the configured chat model.
        Falls back to cl100k_base encoding if model-specific encoding is unavailable.
        
        Args:
            text: Text to estimate tokens for
            
        Returns:
            Estimated token count
        """
        try:
            # Try to get encoding for the specific model
            enc = tiktoken.encoding_for_model(self.config.chat_deployment)
        except KeyError:
            # Fallback to cl100k_base (used by gpt-4, gpt-3.5-turbo, text-embedding-ada-002)
            logger.warning(f"Model {self.config.chat_deployment} not found in tiktoken, using cl100k_base encoding")
            enc = tiktoken.get_encoding("cl100k_base")
        
        return len(enc.encode(text))
    
    def _add_file_hyperlinks(self, text: str) -> str:
        """Add clickable hyperlinks to COBOL file references and program names.
        
        Wraps file references in a special format that the frontend can detect and convert to links.
        Format: [[FILE:filename|Display Text]]
        
        Handles:
        - Files with extensions: PROGRAM.CBL, COPY-BOOK.CPY
        - Bare program names: LOAN, APIPAY, LNCRE2
        
        Args:
            text: The response text
            
        Returns:
            Text with file references marked for hyperlink conversion
        """
        import re
        
        # Pattern 1: Match COBOL files with extensions (.CBL, .CPY, .cbl, .cpy)
        # Matches: PROGRAM.CBL, COPY-BOOK.CPY, etc.
        pattern_with_ext = r'\b([A-Z][A-Z0-9_-]{0,30}\.(?:CBL|CPY|cbl|cpy))\b'
        
        # Pattern 2: Match bare program names (uppercase, alphanumeric with optional hyphens)
        # Only in specific contexts to avoid false positives:
        # - After "Source:" or "Index:" in citations
        # - In lists separated by commas within those contexts
        # Matches: C, LOAN, FORM-PROGX, LONPB0, APIPAY, etc. but not common words
        # Look for program names in (Source: ..., Index: ...) or [Source: ..., Index: ...]
        # Updated to support single-letter programs (C, D, X) and hyphenated names (FORM-PROGX)
        pattern_bare_in_source = r'((?:Source|Index):\s*)([A-Z][A-Z0-9-]{0,7}(?:,\s*[A-Z][A-Z0-9-]{0,7})*)'
        
        def replace_file_with_ext(match):
            filename = match.group(1)
            return f'[[FILE:{filename}|{filename}]]'
        
        def replace_bare_programs(match):
            prefix = match.group(1)  # "Source: " or "Index: "
            programs_str = match.group(2)  # "LOAN, LONPB0, ZONPC0, ..."
            
            # Split by comma and process each program name
            programs = [p.strip() for p in programs_str.split(',')]
            linked_programs = [f'[[FILE:{prog}|{prog}]]' for prog in programs]
            
            return prefix + ', '.join(linked_programs)
        
        # First, apply pattern for files with extensions
        result = re.sub(pattern_with_ext, replace_file_with_ext, text)
        
        # Then, apply pattern for bare program names in Source/Index contexts
        result = re.sub(pattern_bare_in_source, replace_bare_programs, result)
        
        return result
    
    def _get_system_prompt(self, is_otis: bool) -> str:
        """
        Get system prompt based on context.
        Now uses Phase 1 enhanced prompts for better workflow analysis.
        """
        # Use Phase 1 enhanced prompts (FREE improvement)
        return get_enhanced_system_prompt(is_otis=is_otis)
    
    def _build_prompt(
        self,
        query: str,
        context_docs: List[Dict],
        conversation_context: str,
        is_otis: bool,
        question_type: str
    ) -> str:
        """Build complete prompt with context, applying token-aware trimming."""
        
        # Format context documents (initial formatting)
        context_text = self._format_context_docs(context_docs, question_type)
        
        # Build system prompt for token estimation
        system_prompt = self._get_system_prompt(is_otis)
        
        # Build initial prompt parts
        parts = []
        
        # Conversation context (if any)
        if conversation_context:
            parts.append(conversation_context)
            parts.append("\n---\n")
        
        # Retrieved context
        parts.append("## Retrieved Context:")
        parts.append(context_text)
        parts.append("\n---\n")
        
        # User question
        parts.append("## Question:")
        parts.append(query)
        
        if is_otis:
            parts.append("\n*Note: This question is about the OTIS/OTOS application.*")
        
        parts.append("\n## Answer:")
        
        prompt_text = "\n".join(parts)
        
        # 🔹 TOKEN-AWARE CONTEXT TRIMMING
        # Estimate total tokens (system prompt + user prompt)
        system_tokens = self._estimate_tokens(system_prompt)
        prompt_tokens = self._estimate_tokens(prompt_text)
        total_tokens = system_tokens + prompt_tokens
        
        # Get max context length from config (default 32000)
        max_context_tokens = self.config.max_context_length
        
        logger.info(f"📊 Token estimation: system={system_tokens}, prompt={prompt_tokens}, total={total_tokens}, max={max_context_tokens}")
        
        # If we exceed the limit, trim the context section intelligently
        if total_tokens > max_context_tokens:
            logger.warning(f"⚠️ Context exceeds {max_context_tokens} tokens ({total_tokens}), applying intelligent trimming")
            
            # Calculate how much to trim
            # Reserve space for: system prompt + question + overhead (500 tokens)
            reserved_tokens = system_tokens + self._estimate_tokens(query) + 500
            available_for_context = max_context_tokens - reserved_tokens
            
            if available_for_context < 1000:
                logger.error(f"❌ Not enough space for context after reserving {reserved_tokens} tokens")
                # Still try with minimal context
                available_for_context = 1000
            
            # Calculate trimming ratio
            context_tokens = self._estimate_tokens(context_text)
            trim_ratio = available_for_context / context_tokens if context_tokens > 0 else 1.0
            
            logger.info(f"📉 Trimming context: {context_tokens} → {available_for_context} tokens (ratio: {trim_ratio:.2%})")
            
            # Separate deterministic documents (copybook_usage, calls) from semantic docs
            # Priority: Keep deterministic results intact, trim semantic results
            deterministic_docs = [d for d in context_docs if d.get('_index_type') in ['copybook_usage', 'calls']]
            semantic_docs = [d for d in context_docs if d.get('_index_type') not in ['copybook_usage', 'calls']]
            
            if deterministic_docs and trim_ratio < 1.0:
                # Try to keep all deterministic docs, trim semantic docs more aggressively
                deterministic_text = self._format_context_docs(deterministic_docs, question_type)
                deterministic_tokens = self._estimate_tokens(deterministic_text)
                
                remaining_tokens = available_for_context - deterministic_tokens
                
                if remaining_tokens > 0 and semantic_docs:
                    # Format semantic docs with remaining space
                    semantic_text = self._format_context_docs(semantic_docs, question_type)
                    semantic_tokens = self._estimate_tokens(semantic_text)
                    semantic_trim_ratio = remaining_tokens / semantic_tokens if semantic_tokens > 0 else 1.0
                    
                    if semantic_trim_ratio < 1.0:
                        # Trim semantic context
                        char_trim_ratio = int(len(semantic_text) * semantic_trim_ratio)
                        semantic_text = semantic_text[:char_trim_ratio] + "\n\n[... context trimmed to fit token limit ...]"
                        logger.info(f"✂️ Trimmed semantic context: {semantic_tokens} → {remaining_tokens} tokens")
                    
                    context_text = deterministic_text + "\n\n" + semantic_text
                else:
                    # No room for semantic docs, use only deterministic
                    context_text = deterministic_text
                    if deterministic_tokens > available_for_context:
                        # Even deterministic docs need trimming
                        char_trim_ratio = int(len(deterministic_text) * trim_ratio)
                        context_text = deterministic_text[:char_trim_ratio] + "\n\n[... context trimmed to fit token limit ...]"
                        logger.warning(f"⚠️ Had to trim deterministic results: {deterministic_tokens} → {available_for_context} tokens")
            else:
                # No deterministic docs, or no trimming needed for them - trim proportionally
                char_trim_ratio = int(len(context_text) * trim_ratio)
                context_text = context_text[:char_trim_ratio] + "\n\n[... context trimmed to fit token limit ...]"
            
            # Rebuild prompt with trimmed context
            parts = []
            if conversation_context:
                parts.append(conversation_context)
                parts.append("\n---\n")
            parts.append("## Retrieved Context:")
            parts.append(context_text)
            parts.append("\n---\n")
            parts.append("## Question:")
            parts.append(query)
            if is_otis:
                parts.append("\n*Note: This question is about the OTIS/OTOS application.*")
            parts.append("\n## Answer:")
            
            prompt_text = "\n".join(parts)
            
            # Verify final size
            final_tokens = self._estimate_tokens(system_prompt) + self._estimate_tokens(prompt_text)
            logger.info(f"✅ Final token count after trimming: {final_tokens} (target: {max_context_tokens})")
        
        return prompt_text
    
    def _format_context_docs(self, docs: List[Dict], question_type: str) -> str:
        """Format retrieved documents into readable context."""
        if not docs:
            return "*No relevant context found.*"
        
        formatted_parts = []
        
        # For trace_flow questions, show more documents to capture all calls
        # For modification questions, show many documents to ensure MAX definitions surface
        # For copybook_usage questions, show ALL documents to list all programs
        if question_type == 'copybook_usage':
            max_docs = 1000  # Show all programs using the copybook
        elif question_type == 'trace_flow':
            max_docs = 500
        else:
            max_docs = 500  # Increased from 250
        
        # Add document statistics header
        formatted_parts.append(f"**CONTEXT INFORMATION:**")
        formatted_parts.append(f"- Total documents retrieved: {len(docs)}")
        formatted_parts.append(f"- Documents provided for analysis: up to {max_docs}")
        
        # 🔹 CRITICAL: For copybook usage questions, count UNIQUE programs and instruct LLM
        if question_type == 'copybook_usage':
            # Count unique programs (deduplicated by program_id)
            unique_programs = set()
            for doc in docs:
                if doc.get('_index_type') == 'copybook_usage':
                    program_id = doc.get('program_id', '')
                    if program_id:
                        unique_programs.add(program_id)
            
            unique_count = len(unique_programs)
            formatted_parts.append(f"- **UNIQUE PROGRAMS: {unique_count}** (deduplicated from {len(docs)} usage references)")
            formatted_parts.append(f"- **IMPORTANT: You MUST list ALL {unique_count} unique programs in your answer. Do not truncate or summarize.**")
        
        formatted_parts.append("")
        
        # Separate main menus and submenus for hierarchical display
        main_menus = [d for d in docs if d.get('_index_type') == 'screen_nodes' and not d.get('_is_submenu')]
        submenus = [d for d in docs if d.get('_index_type') == 'screen_nodes' and d.get('_is_submenu')]
        
        # If we have both main menu and submenus, format them hierarchically
        if main_menus and submenus:
            formatted_parts.append("**HIERARCHICAL MENU STRUCTURE:**")
            formatted_parts.append("") 
            
            for main_doc in main_menus[:1]:  # Show first main menu
                # Format main menu
                main_summary = main_doc.get('summary_text', '')
                formatted_parts.append("**Level 1: Main Menu**")
                formatted_parts.append(main_summary)
                formatted_parts.append("")
                
                # Format submenus under it
                if submenus:
                    formatted_parts.append("**Level 2: Submenus**")
                    for sub_doc in submenus:
                        submenu_name = sub_doc.get('_submenu_name', 'Submenu')
                        sub_summary = sub_doc.get('summary_text', '')
                        formatted_parts.append(f"\n**{submenu_name}:**")
                        formatted_parts.append(sub_summary)
                    formatted_parts.append("")
            
            # Continue with other documents - exclude only screen_nodes that were already shown hierarchically
            docs = [d for d in docs if d.get('_index_type') != 'screen_nodes' or (d not in main_menus and d not in submenus)]
        
        # Check if we have calls index documents - these should be prioritized for dependency questions
        calls_docs = [d for d in docs if d.get('_index_type') == 'calls']
        other_docs = [d for d in docs if d.get('_index_type') != 'calls']
        
        # If we have calls documents for a trace_flow query, show them first and more of them
        if calls_docs and question_type == 'trace_flow':
            formatted_parts.append("\n**Call/Reference Records (detailed):**")
            formatted_parts.append(f"*IMPORTANT: You MUST extract and list ALL {len(calls_docs)} call/reference records below.*\n")
            for i, doc in enumerate(calls_docs, 1):  # Show ALL calls
                caller = doc.get('caller_program', 'Unknown')
                callee = doc.get('callee_program', 'Unknown')
                ref_type = doc.get('reference_type', 'Unknown')
                category = doc.get('category', '')
                line = doc.get('line', '')
                
                ref_desc = f"{ref_type}"
                if category:
                    ref_desc += f" ({category})"
                if line:
                    ref_desc += f" at line {line}"
                
                formatted_parts.append(f"  [{i}] {caller} → {callee}: {ref_desc}")
            
            # Add verification reminder
            formatted_parts.append(f"\n*VERIFICATION REQUIRED: Count all unique callees above. Total records: {len(calls_docs)}*")
            formatted_parts.append("\n**Additional Context:**")
            docs_to_show = other_docs[:10]  # Show less other context to save space
        else:
            docs_to_show = docs[:max_docs]
        
        for i, doc in enumerate(docs_to_show, 1):
            index_type = doc.get('_index_type', 'unknown')

            # Build more descriptive document metadata with actual filenames
            doc_id = doc.get('id', doc.get('chunk_id', doc.get('program_id', 'unknown')))
            
            # Extract filename from repo_path or file_path
            file_path = doc.get('repo_path', doc.get('file_path', ''))
            if file_path:
                # Get just the filename from the full path
                filename = file_path.split('/')[-1] if '/' in file_path else file_path.split('\\')[-1]
            else:
                filename = doc.get('name', doc.get('program_id', 'Unknown'))
            
            # Ensure filename is a string
            if filename is None:
                filename = 'Unknown'
            
            # Truncate very long filenames to keep citations readable
            max_filename_length = 1000
            if len(filename) > max_filename_length:
                filename = filename[:max_filename_length] + " [truncated]"
            
            # Create descriptive metadata with truncated filename
            doc_metadata = f"[Source: {filename}, Index: {index_type}]"

            # Extract key information based on index type
            if index_type == 'code':
                # Code chunks - show actual source code
                text = doc.get('text', '')
                name = doc.get('name', filename)
                lines = f"Lines {doc.get('start_line', '?')}-{doc.get('end_line', '?')}"

                formatted_parts.append(f"\n**Document {i}: {name} ({lines})** {doc_metadata}")
                formatted_parts.append(f"```cobol\n{text[:500]}\n```")
            
            elif index_type == 'programs':
                # Program metadata
                prog_id = doc.get('program_id', 'Unknown')
                summary = doc.get('program_summary', doc.get('description', ''))
                
                formatted_parts.append(f"\n**Document {i}: Program {prog_id}** {doc_metadata}")
                formatted_parts.append(summary[:300])
            
            elif index_type == 'paragraphs':
                # Paragraph info
                para_name = doc.get('paragraph_name', 'Unknown')
                prog_id = doc.get('program_id', 'Unknown')
                summary = doc.get('paragraph_summary', '')
                
                formatted_parts.append(f"\n**Document {i}: Paragraph {para_name} (in {prog_id})** {doc_metadata}")
                formatted_parts.append(summary[:300])
            
            elif index_type == 'data_items':
                # Data item definitions
                data_name = doc.get('data_name', 'Unknown')
                pic = doc.get('picture_clause', '')
                level = doc.get('level_number', '')
                
                formatted_parts.append(f"\n**Document {i}: Data Item {data_name}** {doc_metadata}")
                formatted_parts.append(f"Level: {level}, PIC: {pic}")
            
            elif index_type == 'ui_paths':
                # UI path - show path structure
                path_json = doc.get('path_json', [])
                root = doc.get('root_screen', doc.get('root_transaction', 'Unknown'))
                leaf = doc.get('leaf_screen', doc.get('leaf_program', ''))
                
                formatted_parts.append(f"\n**Document {i}: UI Path - {root}** {doc_metadata}")
                if path_json:
                    formatted_parts.append(f"Path: {' → '.join(path_json[:10])}")
                if leaf:
                    formatted_parts.append(f"Leaf: {leaf}")
            
            elif index_type == 'menu_trees':
                # Menu tree - show hierarchical structure
                import json
                root_id = doc.get('root_program_id', 'Unknown')
                total_nodes = doc.get('total_nodes', 0)
                ui_nodes = doc.get('total_ui_nodes', 0)
                max_depth = doc.get('max_depth', 0)
                
                formatted_parts.append(f"\n**Document {i}: Menu Tree - {root_id}** {doc_metadata}")
                formatted_parts.append(f"Stats: {total_nodes} nodes, {ui_nodes} UI screens, max depth {max_depth}")
                
                # Parse and format tree structure
                tree_json_str = doc.get('tree_json', '[]')
                try:
                    tree_data = json.loads(tree_json_str) if isinstance(tree_json_str, str) else tree_json_str
                    if tree_data:
                        # Build hierarchical text representation
                        tree_lines = self._format_menu_tree(tree_data, max_lines=100)
                        if tree_lines:
                            formatted_parts.append("\nHierarchical Structure:")
                            formatted_parts.append('\n'.join(tree_lines))
                except Exception as e:
                    logger.warning(f"Could not parse menu tree: {e}")
                    formatted_parts.append("(Tree structure unavailable)")
            
            else:
                # Generic formatting
                # Try to find a title/name field
                title = (doc.get('name') or doc.get('title') or 
                        doc.get('program_id') or doc.get('root_screen') or f"Document {i}")
                
                # Try to find content - prioritize summary_text (screen_nodes) and enhanced_summary
                content = (doc.get('summary_text') or doc.get('enhanced_summary') or doc.get('text') or doc.get('summary') or 
                          doc.get('description') or str(doc)[:500])
                
                # DEBUG: Log available fields for screen_nodes
                if index_type == 'screen_nodes':
                    logger.info(f"   screen_nodes doc keys: {list(doc.keys())}")
                    if 'summary_text' in doc:
                        logger.info(f"   summary_text found, length: {len(doc['summary_text'])}")
                        logger.info(f"   summary_text preview: {doc['summary_text'][:200]}")
                
                formatted_parts.append(f"\n**Document {i}: {title}** {doc_metadata}")
                # For screen_nodes, show full summary (menu text can be long)
                # For other types, limit to 300 chars
                if index_type == 'screen_nodes':
                    formatted_parts.append(content)  # Show full summary with all menu items
                else:
                    formatted_parts.append(content[:300])
        
        return "\n".join(formatted_parts)
    
    def _format_menu_tree(self, tree_data, max_lines=100, max_depth=20):
        """Format menu tree data into hierarchical text representation.
        
        Args:
            tree_data: Either a single tree node dict or list of nodes
            max_lines: Maximum number of lines to generate
            max_depth: Maximum depth to traverse
            
        Returns:
            List of formatted tree lines
        """
        import json
        
        lines = []
        visited = set()  # Track visited nodes to avoid cycles
        
        def traverse(node, depth=0, prefix="", is_last=True):
            """Recursively traverse and format tree nodes."""
            if len(lines) >= max_lines or depth > max_depth:
                return
            
            # Handle different node formats
            if isinstance(node, str):
                return  # Skip string nodes
                
            if not isinstance(node, dict):
                return
            
            # Get node ID and detect cycles
            node_id = node.get('node_id') or node.get('program_id') or node.get('id', '')
            if not node_id:
                return
                
            node_key = f"{node_id}_{depth}"
            if node_key in visited:
                return
            visited.add(node_key)
            
            # Determine if this is a UI screen
            is_ui = node.get('is_ui_screen', False) or node.get('ui', False) or node.get('role', '').upper() == 'UI'
            ui_indicator = '🖥️ ' if is_ui else ''
            
            # Build the line
            connector = "└── " if is_last else "├── "
            if depth == 0:
                connector = ""
            
            line = f"{prefix}{connector}{ui_indicator}{node_id}"
            lines.append(line)
            
            # Get children
            children = node.get('children', [])
            if not children:
                return
            
            # Prepare prefix for children
            if depth == 0:
                child_prefix = ""
            else:
                extension = "    " if is_last else "│   "
                child_prefix = prefix + extension
            
            # Process children
            for i, child in enumerate(children):
                if len(lines) >= max_lines:
                    break
                is_last_child = (i == len(children) - 1)
                traverse(child, depth + 1, child_prefix, is_last_child)
        
        # Handle both single node and list of nodes
        if isinstance(tree_data, list):
            for node in tree_data:
                traverse(node, depth=0)
                if len(lines) >= max_lines:
                    break
        else:
            traverse(tree_data, depth=0)
        
        return lines

