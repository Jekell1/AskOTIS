"""
Comprehensive COBOL external program reference patterns.

This identifies all COBOL constructs that reference external programs,
not just CALL statements.
"""

import re
from typing import List, Dict, Set

def get_cobol_external_reference_patterns() -> Dict[str, Dict]:
    """
    Define all COBOL constructs that reference external programs.
    """
    
    patterns = {
        # 1. CALL statements (static and dynamic)
        "CALL_STATIC": {
            "pattern": r'CALL\s+"([^"]+)"',
            "description": "Direct CALL to named program",
            "example": 'CALL "SUBPROG" USING PARM1',
            "flags": re.IGNORECASE
        },
        
        "CALL_DYNAMIC": {
            "pattern": r'CALL\s+([A-Z0-9\-_]+)\s+USING',
            "description": "Dynamic CALL using variable",
            "example": 'CALL PROG-NAME USING PARM1',
            "flags": re.IGNORECASE
        },
        
        # 2. COPY statements (copybook inclusion)
        "COPY_SIMPLE": {
            "pattern": r'COPY\s+"([^"]+)"',
            "description": "COPY copybook by quoted name",
            "example": 'COPY "LIBCOPY/WORKAREA"',
            "flags": re.IGNORECASE
        },
        
        "COPY_LIBRARY": {
            "pattern": r'COPY\s+([A-Z0-9\-_/\.]+)',
            "description": "COPY copybook (library/member format)",
            "example": 'COPY LIBCOPY/WORKAREA',
            "flags": re.IGNORECASE
        },
        
        # 3. EXEC SQL statements (if using embedded SQL)
        "EXEC_SQL": {
            "pattern": r'EXEC\s+SQL\s+INCLUDE\s+([A-Z0-9\-_]+)',
            "description": "SQL INCLUDE statement",
            "example": 'EXEC SQL INCLUDE SQLCA',
            "flags": re.IGNORECASE
        },
        
        # 4. EXEC CICS statements (if using CICS)
        "EXEC_CICS_XCTL": {
            "pattern": r'EXEC\s+CICS\s+XCTL\s+PROGRAM\s*\(\s*["\']?([^"\')\s]+)["\']?\s*\)',
            "description": "CICS XCTL (transfer control) to program",
            "example": 'EXEC CICS XCTL PROGRAM("NEXTPROG")',
            "flags": re.IGNORECASE
        },
        
        "EXEC_CICS_LINK": {
            "pattern": r'EXEC\s+CICS\s+LINK\s+PROGRAM\s*\(\s*["\']?([^"\')\s]+)["\']?\s*\)',
            "description": "CICS LINK to program",
            "example": 'EXEC CICS LINK PROGRAM("SUBPROG")',
            "flags": re.IGNORECASE
        },
        
        "EXEC_CICS_LOAD": {
            "pattern": r'EXEC\s+CICS\s+LOAD\s+PROGRAM\s*\(\s*["\']?([^"\')\s]+)["\']?\s*\)',
            "description": "CICS LOAD program",
            "example": 'EXEC CICS LOAD PROGRAM("DYNPROG")',
            "flags": re.IGNORECASE
        },
        
        # 5. CANCEL statements
        "CANCEL": {
            "pattern": r'CANCEL\s+"([^"]+)"',
            "description": "CANCEL program (remove from memory)",
            "example": 'CANCEL "SUBPROG"',
            "flags": re.IGNORECASE
        },
        
        "CANCEL_DYNAMIC": {
            "pattern": r'CANCEL\s+([A-Z0-9\-_]+)',
            "description": "CANCEL program using variable",
            "example": 'CANCEL PROG-NAME',
            "flags": re.IGNORECASE
        },
        
        # 6. SORT/MERGE with INPUT/OUTPUT PROCEDURE
        "SORT_INPUT_PROC": {
            "pattern": r'INPUT\s+PROCEDURE\s+IS\s+([A-Z0-9\-_]+)',
            "description": "SORT INPUT PROCEDURE",
            "example": 'INPUT PROCEDURE IS SORT-INPUT-PROC',
            "flags": re.IGNORECASE
        },
        
        "SORT_OUTPUT_PROC": {
            "pattern": r'OUTPUT\s+PROCEDURE\s+IS\s+([A-Z0-9\-_]+)',
            "description": "SORT OUTPUT PROCEDURE",
            "example": 'OUTPUT PROCEDURE IS SORT-OUTPUT-PROC',
            "flags": re.IGNORECASE
        },
        
        # 7. Database stored procedure calls (DB2, Oracle, etc.)
        "EXEC_SQL_CALL": {
            "pattern": r'EXEC\s+SQL\s+CALL\s+([A-Z0-9\-_\.]+)',
            "description": "SQL stored procedure call",
            "example": 'EXEC SQL CALL SCHEMA.PROCEDURE',
            "flags": re.IGNORECASE
        },
        
        # 8. JCL-style program invocation (if embedded)
        "JCL_EXEC_PGM": {
            "pattern": r'//\s*\w+\s+EXEC\s+PGM=([A-Z0-9\-_]+)',
            "description": "JCL EXEC PGM statement",
            "example": '//STEP1 EXEC PGM=MYPROG',
            "flags": re.IGNORECASE
        },
        
        # 9. COBOL compiler directives
        "EXEC_PREPROCESSOR": {
            "pattern": r'EXEC\s+([A-Z0-9\-_]+)\s+INCLUDE',
            "description": "Preprocessor INCLUDE",
            "example": 'EXEC CICS INCLUDE DFHAID',
            "flags": re.IGNORECASE
        },
        
        # 10. Microfocus/Enterprise COBOL specific
        "CBL_CALL": {
            "pattern": r'\$SET\s+INCLUDE\s+"([^"]+)"',
            "description": "Compiler directive INCLUDE",
            "example": '$SET INCLUDE "COPYLIB"',
            "flags": re.IGNORECASE
        },
        
        # 11. Function calls (COBOL functions that might be external)
        "FUNCTION_CALL": {
            "pattern": r'FUNCTION\s+([A-Z0-9\-_]+)\s*\(',
            "description": "Function reference (might be external)",
            "example": 'FUNCTION CURRENT-DATE',
            "flags": re.IGNORECASE
        },
        
        # 12. GOBACK/EXIT PROGRAM (program termination, might return to caller)
        "EXIT_PROGRAM": {
            "pattern": r'EXIT\s+PROGRAM',
            "description": "Exit program (return to caller)",
            "example": 'EXIT PROGRAM',
            "flags": re.IGNORECASE
        },
        
        # 13. STOP RUN (program termination)
        "STOP_RUN": {
            "pattern": r'STOP\s+RUN',
            "description": "Stop program execution",
            "example": 'STOP RUN',
            "flags": re.IGNORECASE
        },
        
        # 14. ACCEPT/DISPLAY with UPON CONSOLE (system interaction)
        "ACCEPT_CONSOLE": {
            "pattern": r'ACCEPT\s+\w+\s+FROM\s+CONSOLE',
            "description": "Accept from system console",
            "example": 'ACCEPT WS-INPUT FROM CONSOLE',
            "flags": re.IGNORECASE
        },
        
        # 15. File system operations (might invoke external utilities)
        "DELETE_FILE": {
            "pattern": r'DELETE\s+FILE\s+([A-Z0-9\-_]+)',
            "description": "Delete file operation",
            "example": 'DELETE FILE INPUT-FILE',
            "flags": re.IGNORECASE
        }
    }
    
    return patterns

def extract_all_external_references(cobol_content: str, program_name: str) -> Dict:
    """
    Extract all external program references using comprehensive patterns.
    """
    
    patterns = get_cobol_external_reference_patterns()
    lines = cobol_content.split('\n')
    
    all_references = []
    reference_summary = {}
    
    for line_num, line in enumerate(lines, 1):
        line = line.strip()
        
        # Skip comments
        if line.startswith('*') or not line:
            continue
            
        for pattern_name, pattern_info in patterns.items():
            regex = re.compile(pattern_info['pattern'], pattern_info['flags'])
            matches = regex.findall(line)
            
            for match in matches:
                reference = {
                    'line': line_num,
                    'type': pattern_name,
                    'description': pattern_info['description'],
                    'referenced_name': match if isinstance(match, str) else match[0],
                    'statement': line,
                    'example': pattern_info['example']
                }
                
                all_references.append(reference)
                
                # Count by type
                if pattern_name not in reference_summary:
                    reference_summary[pattern_name] = []
                reference_summary[pattern_name].append(reference)
    
    # Get unique referenced names
    unique_references = set()
    for ref in all_references:
        unique_references.add(ref['referenced_name'])
    
    return {
        'program_name': program_name,
        'total_references': len(all_references),
        'unique_references': len(unique_references),
        'referenced_names': sorted(list(unique_references)),
        'all_references': all_references,
        'by_type': reference_summary,
        'patterns_found': list(reference_summary.keys())
    }

if __name__ == "__main__":
    # Display all patterns
    patterns = get_cobol_external_reference_patterns()
    
    print("🔍 COMPREHENSIVE COBOL EXTERNAL REFERENCE PATTERNS")
    print("=" * 80)
    print()
    
    for i, (pattern_name, info) in enumerate(patterns.items(), 1):
        print(f"{i:2d}. {pattern_name}")
        print(f"    Description: {info['description']}")
        print(f"    Example:     {info['example']}")
        print(f"    Pattern:     {info['pattern']}")
        print()
    
    print(f"Total patterns defined: {len(patterns)}")
    
    # Test with APIPAY if available
    try:
        from pathlib import Path
        apipay_path = Path('cobol_src/SP/APIPAY.CBL')
        if apipay_path.exists():
            with open(apipay_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            results = extract_all_external_references(content, 'APIPAY')
            
            print("\n" + "=" * 80)
            print("APIPAY EXTERNAL REFERENCES ANALYSIS")
            print("=" * 80)
            print(f"Total external references: {results['total_references']}")
            print(f"Unique references: {results['unique_references']}")
            print(f"Pattern types found: {len(results['patterns_found'])}")
            print()
            
            print("📋 REFERENCED NAMES:")
            for name in results['referenced_names']:
                print(f"  • {name}")
            
            print("\n📊 BY PATTERN TYPE:")
            for pattern_name in results['patterns_found']:
                refs = results['by_type'][pattern_name]
                print(f"  {pattern_name}: {len(refs)} occurrence(s)")
                for ref in refs[:3]:  # Show first 3
                    print(f"    Line {ref['line']:4d}: {ref['referenced_name']}")
                if len(refs) > 3:
                    print(f"    ... and {len(refs) - 3} more")
    
    except Exception as e:
        print(f"Could not analyze APIPAY: {e}")