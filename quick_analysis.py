"""
Ultra-Fast COBOL Pattern Analysis Integration
============================================
Quick analysis functions for immediate feedback
"""

import re
import time
from typing import Dict, List, Optional

def quick_analyze_program(program_name: str, search_results: Dict) -> Optional[Dict]:
    """
    Ultra-fast pattern-based analysis for specific programs
    Returns instant analysis without LLM processing
    """
    if not search_results.get('value'):
        return None
        
    start_time = time.time()
    
    # Extract the main program file content
    main_file = None
    total_lines = 0
    all_files = set()
    
    for result in search_results['value']:
        repo_path = result.get('repo_path', '')
        if repo_path:
            all_files.add(repo_path)
            
        # Look for the main program file
        if program_name.upper() in repo_path.upper() and repo_path.endswith('.CBL'):
            if not main_file or len(result.get('code', '')) > len(main_file.get('code', '')):
                main_file = result
                
        total_lines += 1
    
    if not main_file:
        return None
        
    code_content = main_file.get('code', '')
    file_size = len(code_content)
    
    # Pattern-based analysis
    analysis = {
        'program_name': program_name,
        'main_file': main_file.get('repo_path', ''),
        'file_size': file_size,
        'total_matches': len(search_results['value']),
        'analysis_time': round(time.time() - start_time, 2),
        'structure': _analyze_program_structure(code_content),
        'key_sections': _extract_key_sections(code_content),
        'dependencies': _find_dependencies(search_results['value']),
        'variables': _find_key_variables(code_content),
        'procedures': _find_procedures(code_content)
    }
    
    return analysis

def _analyze_program_structure(code: str) -> Dict:
    """Analyze COBOL program structure using patterns"""
    structure = {
        'is_main_program': False,
        'has_standard_structure': False,
        'has_executable_logic': False,
        'has_data_definitions': False,
        'calls_other_programs': False,
        'uses_copybooks': False,
        'has_working_storage': False,
        'has_procedures': False
    }
    
    code_upper = code.upper()
    
    # Check for main program indicators
    if 'PROGRAM-ID' in code_upper:
        structure['is_main_program'] = True
        
    # Check for standard COBOL structure
    divisions = ['IDENTIFICATION DIVISION', 'DATA DIVISION', 'PROCEDURE DIVISION']
    if all(div in code_upper for div in divisions):
        structure['has_standard_structure'] = True
        
    # Check for executable logic
    executable_keywords = ['PERFORM', 'MOVE', 'COMPUTE', 'IF', 'CALL', 'READ', 'WRITE']
    if any(keyword in code_upper for keyword in executable_keywords):
        structure['has_executable_logic'] = True
        
    # Check for data definitions
    data_keywords = ['PIC', 'PICTURE', '01 ', '05 ', '10 ']
    if any(keyword in code_upper for keyword in data_keywords):
        structure['has_data_definitions'] = True
        
    # Check for program calls
    if 'CALL ' in code_upper:
        structure['calls_other_programs'] = True
        
    # Check for copybooks
    if 'COPY ' in code_upper:
        structure['uses_copybooks'] = True
        
    # Check for working storage
    if 'WORKING-STORAGE' in code_upper:
        structure['has_working_storage'] = True
        
    # Check for procedures/sections
    if 'SECTION' in code_upper or 'PERFORM ' in code_upper:
        structure['has_procedures'] = True
        
    return structure

def _extract_key_sections(code: str, max_sections: int = 10) -> List[Dict]:
    """Extract key sections of code for display"""
    lines = code.split('\n')
    sections = []
    
    current_section = []
    section_start = 0
    
    for i, line in enumerate(lines):
        line_upper = line.upper().strip()
        
        # Look for interesting sections
        if any(keyword in line_upper for keyword in [
            'IDENTIFICATION DIVISION', 'PROGRAM-ID', 'DATE-WRITTEN',
            'DESCRIPTION:', 'NOTE:', 'REVISION:', 'AUTHOR:',
            '***', '*  ', 'WORKING-STORAGE', 'PROCEDURE DIVISION',
            'SECTION.', 'PERFORM ', 'CALL '
        ]):
            if current_section and len(sections) < max_sections:
                sections.append({
                    'line': section_start + 1,
                    'content': current_section
                })
            
            current_section = [line.strip()]
            section_start = i
        elif current_section and len(current_section) < 15:  # Limit section size
            current_section.append(line.strip())
            
    # Add the last section
    if current_section and len(sections) < max_sections:
        sections.append({
            'line': section_start + 1,
            'content': current_section
        })
        
    return sections

def _find_dependencies(search_results: List[Dict]) -> List[str]:
    """Find program dependencies from search results"""
    dependencies = set()
    
    for result in search_results:
        code = result.get('code', '').upper()
        
        # Look for CALL statements
        import re
        calls = re.findall(r'CALL\s+[\'"]([^\'\"]+)[\'"]', code)
        dependencies.update(calls)
        
        # Look for COPY statements
        copies = re.findall(r'COPY\s+[\'"]?([^\s\'\"]+)[\'"]?', code)
        dependencies.update(copies)
        
    return sorted(list(dependencies))

def _find_key_variables(code: str) -> List[str]:
    """Find key variable definitions"""
    variables = []
    lines = code.split('\n')
    
    for line in lines:
        line_stripped = line.strip().upper()
        
        # Look for variable definitions (01, 05, etc. level numbers)
        import re
        if re.match(r'^\d{2}\s+[A-Z][A-Z0-9-]*', line_stripped):
            var_match = re.match(r'^\d{2}\s+([A-Z][A-Z0-9-]*)', line_stripped)
            if var_match:
                variables.append(var_match.group(1))
                
        if len(variables) >= 20:  # Limit to first 20 variables
            break
            
    return variables

def _find_procedures(code: str) -> List[str]:
    """Find procedure/section names"""
    procedures = []
    lines = code.split('\n')
    
    for line in lines:
        line_stripped = line.strip().upper()
        
        # Look for sections and paragraphs
        if ' SECTION.' in line_stripped:
            section_name = line_stripped.replace(' SECTION.', '').strip()
            if section_name and not section_name.startswith('*'):
                procedures.append(section_name)
        elif line_stripped.endswith('.') and not line_stripped.startswith('*') and len(line_stripped) < 50:
            # Possible paragraph name
            para_name = line_stripped[:-1].strip()
            if para_name and ' ' not in para_name and len(para_name) > 3:
                procedures.append(para_name)
                
        if len(procedures) >= 15:  # Limit to first 15 procedures
            break
            
    return procedures

def format_quick_analysis(analysis: Dict) -> str:
    """Format the quick analysis results for display"""
    if not analysis:
        return "No analysis data available."
        
    output = f"""🚀 **Ultra-Fast {analysis['program_name']} Analysis**
========================================
🔍 **Search completed in {analysis['analysis_time']}s**
📊 **Found {analysis['total_matches']} total matches, analyzing top results**

⚡ **Quick Analysis (Pattern-Based):**
------------------------------
📁 **Main File**: {analysis['main_file']}
📝 **File Size**: {analysis['file_size']:,} characters
🔍 **Program Structure**:"""

    structure = analysis['structure']
    for key, value in structure.items():
        if value:
            display_name = key.replace('_', ' ').replace('has ', '').replace('is ', '').title()
            output += f"\n   ✓ {display_name}"

    # Key sections
    if analysis['key_sections']:
        output += f"\n\n📋 **Key Program Sections**:"
        for section in analysis['key_sections'][:5]:  # Show first 5 sections
            content_preview = section['content'][:3] if len(section['content']) >= 3 else section['content']
            for i, line in enumerate(content_preview):
                if i == 0:
                    output += f"\n   Line {section['line']}: {line[:100]}"
                else:
                    output += f"\n{line[:100]}"
            if len(section['content']) > 3:
                output += f"\n   ... (+{len(section['content'])-3} more lines)"
            output += "\n"

    # Dependencies
    if analysis['dependencies']:
        deps = analysis['dependencies'][:10]  # Show first 10
        output += f"\n\n🔗 **Dependencies** ({len(analysis['dependencies'])} total):\n   "
        output += ", ".join(deps)
        if len(analysis['dependencies']) > 10:
            output += f" ... (+{len(analysis['dependencies'])-10} more)"

    # Variables
    if analysis['variables']:
        vars = analysis['variables'][:10]  # Show first 10
        output += f"\n\n📝 **Key Variables** ({len(analysis['variables'])} total):\n   "
        output += ", ".join(vars)
        if len(analysis['variables']) > 10:
            output += f" ... (+{len(analysis['variables'])-10} more)"

    # Procedures
    if analysis['procedures']:
        procs = analysis['procedures'][:10]  # Show first 10
        output += f"\n\n⚙️ **Procedures/Sections** ({len(analysis['procedures'])} total):\n   "
        output += ", ".join(procs)
        if len(analysis['procedures']) > 10:
            output += f" ... (+{len(analysis['procedures'])-10} more)"

    output += f"""

⚡ **Quick Analysis completed in {analysis['analysis_time']} seconds**

🤖 **For AI-Powered Deep Analysis**:
   • Run the full chatbot for detailed explanation
   • Ask specific questions about variables, procedures, or logic
   • Note: AI analysis takes ~20 seconds due to GPT-5 processing time"""

    return output

def should_use_quick_analysis(query: str) -> bool:
    """Determine if we should use quick analysis for this query"""
    query_lower = query.lower()
    
    # Use quick analysis for simple "what does X do" queries about specific programs
    quick_patterns = [
        'what does lonpf2 do',
        'what is lonpf2',
        'lonpf2 purpose',
        'explain lonpf2'
    ]
    
    return any(pattern in query_lower for pattern in quick_patterns)

def extract_program_name_from_query(query: str) -> Optional[str]:
    """Extract program name from user query"""
    query_upper = query.upper()
    
    # Look for common COBOL program patterns
    import re
    
    # Direct program name patterns
    patterns = [
        r'\b([A-Z][A-Z0-9]{4,7})\b',  # 5-8 char program names
        r'\b([A-Z][A-Z0-9]*PF[0-9]*)\b',  # *PF* pattern programs
        r'\b(LON[A-Z0-9]+)\b'  # LON* programs
    ]
    
    for pattern in patterns:
        matches = re.findall(pattern, query_upper)
        for match in matches:
            if len(match) >= 4:  # Reasonable program name length
                return match
                
    return None
