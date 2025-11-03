"""
Extract Menu Option Display Text from COBOL Programs
====================================================

This script parses COBOL menu programs to extract the actual menu option text
that users see on screen (e.g., "1. Daily Processing", "2. Loan Maintenance").

Currently, the menu_trees index only shows which programs are called, but not
the display text. This enhancement will capture:
- Menu option numbers
- Display text shown to users
- Which program each option calls
- Help text or descriptions

Target Questions to Fix:
- Q71: What options are available in LPMENU?
- Q73: What does option 1 in LPMENU do?
- Q79: What options are under 'Daily Processing'?
"""

import json
import re
import os
from pathlib import Path
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from collections import defaultdict

# Load configuration
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    values = settings.get('Values', settings)

SEARCH_ENDPOINT = values.get('SEARCH_ENDPOINT') or values.get('AZURE_SEARCH_ENDPOINT')
SEARCH_KEY = values.get('SEARCH_KEY') or values.get('AZURE_SEARCH_KEY')

# Initialize search client for reading existing data
menu_trees_client = SearchClient(
    endpoint=SEARCH_ENDPOINT,
    index_name="cobol_menu_trees",
    credential=AzureKeyCredential(SEARCH_KEY)
)

code_chunks_client = SearchClient(
    endpoint=SEARCH_ENDPOINT,
    index_name="new_code_chunks",
    credential=AzureKeyCredential(SEARCH_KEY)
)

def extract_menu_options_from_code(code_text, program_name):
    """
    Extract menu options from COBOL code.
    
    Looks for patterns like:
    - DISPLAY "1. Daily Processing"
    - DISPLAY "2. Loan Maintenance"
    - VALUE "1" in screen definitions
    - 88-level conditions with menu options
    """
    options = []
    
    # Pattern 1: DISPLAY statements with menu options
    # Matches: DISPLAY "1. Daily Processing" or DISPLAY '1 - DAILY PROCESSING'
    display_pattern = r'DISPLAY\s+["\'](\d+)[.\s\-]+([^"\']+)["\']'
    for match in re.finditer(display_pattern, code_text, re.IGNORECASE):
        option_num = match.group(1)
        option_text = match.group(2).strip()
        options.append({
            "number": option_num,
            "text": option_text,
            "source": "DISPLAY statement"
        })
    
    # Pattern 2: Screen section VALUE clauses
    # Matches: 05 FILLER PIC X(30) VALUE "1. Daily Processing"
    value_pattern = r'VALUE\s+["\'](\d+)[.\s\-]+([^"\']+)["\']'
    for match in re.finditer(value_pattern, code_text, re.IGNORECASE):
        option_num = match.group(1)
        option_text = match.group(2).strip()
        # Avoid duplicates
        if not any(opt['number'] == option_num for opt in options):
            options.append({
                "number": option_num,
                "text": option_text,
                "source": "VALUE clause"
            })
    
    # Pattern 3: Menu text in comments (often documents the menu structure)
    # Matches: * 1 - Daily Processing
    comment_pattern = r'^\s*\*\s*(\d+)\s*[\-\.]\s*(.+)$'
    for match in re.finditer(comment_pattern, code_text, re.MULTILINE | re.IGNORECASE):
        option_num = match.group(1)
        option_text = match.group(2).strip()
        # Only add if not already found (comments are lower priority)
        if not any(opt['number'] == option_num for opt in options):
            options.append({
                "number": option_num,
                "text": option_text,
                "source": "comment"
            })
    
    # Pattern 4: ACCEPT/IF structure (inferred from code logic)
    # ACCEPT WS-CHOICE
    # IF WS-CHOICE = '1'
    #    CALL 'LPSMNU'
    accept_if_pattern = r'IF\s+[\w\-]+\s*=\s*["\'](\d+)["\'].*?CALL\s+["\'](\w+)["\']'
    for match in re.finditer(accept_if_pattern, code_text, re.IGNORECASE | re.DOTALL):
        option_num = match.group(1)
        called_program = match.group(2)
        # Try to find corresponding display text
        existing = next((opt for opt in options if opt['number'] == option_num), None)
        if existing:
            existing['calls'] = called_program
    
    return options

def get_program_calls(program_name):
    """Get the programs called by this menu program from existing index."""
    try:
        # Search for program dependencies
        results = menu_trees_client.search(
            search_text=f"program_id:{program_name}",
            top=1
        )
        
        for result in results:
            calls = result.get('called_programs', [])
            return calls if isinstance(calls, list) else []
    except Exception as e:
        print(f"  ⚠️ Could not fetch calls for {program_name}: {e}")
    
    return []

def get_program_code(program_name):
    """Fetch the source code for a program from code chunks."""
    try:
        # Search for code chunks from this program by name
        # The name field contains the file name like "LPMENU.CBL"
        search_name = f"{program_name}.CBL"
        results = code_chunks_client.search(
            search_text=search_name,
            select=["text", "chunk_id", "name"],
            top=100  # Increase to get more chunks
        )
        
        code_parts = []
        for result in results:
            # Filter to exact matches
            name = result.get('name', '')
            if name and program_name in name.upper():
                content = result.get('text', '')
                code_parts.append(content)
        
        if code_parts:
            print(f"  ✓ Retrieved {len(code_parts)} code chunks")
            return '\n'.join(code_parts)
    except Exception as e:
        print(f"  ⚠️ Could not fetch code for {program_name}: {e}")
    
    return None

def match_options_to_calls(options, called_programs):
    """
    Match menu options to the programs they call.
    Uses heuristics based on option number and call order.
    """
    enhanced_options = []
    
    for option in options:
        opt_copy = option.copy()
        opt_num = int(option['number'])
        
        # Try to match by position (option 1 -> first call, option 2 -> second call)
        if 0 < opt_num <= len(called_programs):
            opt_copy['calls'] = called_programs[opt_num - 1]
        
        enhanced_options.append(opt_copy)
    
    return enhanced_options

def process_menu_program(program_name):
    """Process a single menu program to extract options."""
    print(f"\n📋 Processing {program_name}...")
    
    # Get source code
    code = get_program_code(program_name)
    if not code:
        print(f"  ⚠️ No code found for {program_name}")
        return None
    
    print(f"  ✓ Retrieved {len(code)} characters of code")
    
    # Extract options from code
    options = extract_menu_options_from_code(code, program_name)
    print(f"  ✓ Found {len(options)} menu options")
    
    if not options:
        return None
    
    # Get called programs
    called_programs = get_program_calls(program_name)
    print(f"  ✓ Program calls {len(called_programs)} other programs")
    
    # Match options to calls
    enhanced_options = match_options_to_calls(options, called_programs)
    
    # Display results
    for opt in enhanced_options:
        calls = opt.get('calls', '?')
        print(f"    {opt['number']}. {opt['text']} → {calls}")
    
    return {
        "program_id": program_name,
        "menu_options": enhanced_options,
        "total_options": len(enhanced_options)
    }

def update_menu_trees_index(menu_data):
    """Update the menu_trees index with extracted option text."""
    print(f"\n💾 Updating menu_trees index...")
    
    try:
        # Prepare documents for upload
        documents = []
        for program_name, data in menu_data.items():
            if not data:
                continue
            
            doc = {
                "id": f"{program_name}_menu_options",
                "program_id": program_name,
                "menu_option_text": json.dumps(data['menu_options']),
                "options_count": data['total_options'],
                "has_menu_text": True
            }
            documents.append(doc)
        
        if documents:
            result = menu_trees_client.upload_documents(documents=documents)
            print(f"  ✓ Uploaded {len(documents)} menu option records")
            return True
    except Exception as e:
        print(f"  ❌ Upload failed: {e}")
        print(f"  💡 You may need to add these fields to the index schema:")
        print(f"     - menu_option_text (Edm.String)")
        print(f"     - options_count (Edm.Int32)")
        print(f"     - has_menu_text (Edm.Boolean)")
    
    return False

def save_to_file(menu_data, filename="menu_options_extracted.json"):
    """Save extracted menu data to a file for review."""
    with open(filename, 'w', encoding='utf-8') as f:
        json.dump(menu_data, f, indent=2, ensure_ascii=False)
    print(f"\n💾 Saved extracted menu data to {filename}")

def main():
    """Main extraction process."""
    print("=" * 80)
    print("MENU OPTION TEXT EXTRACTION")
    print("=" * 80)
    
    # Priority menu programs to process
    menu_programs = [
        "LPMENU",    # Main loan processing menu
        "LPSMNU",    # Daily processing menu
        "PGMENU",    # Another main menu
        "OPMENU",    # Operations menu
        "GTFORM",    # Menu tree root
        "RPMENU",    # Reports menu
    ]
    
    menu_data = {}
    
    for program in menu_programs:
        try:
            data = process_menu_program(program)
            if data:
                menu_data[program] = data
        except Exception as e:
            print(f"  ❌ Error processing {program}: {e}")
    
    # Save results
    save_to_file(menu_data)
    
    # Summary
    print("\n" + "=" * 80)
    print("📊 EXTRACTION SUMMARY")
    print("=" * 80)
    print(f"Programs processed: {len(menu_programs)}")
    print(f"Programs with menu text: {len(menu_data)}")
    print(f"Total options extracted: {sum(d['total_options'] for d in menu_data.values())}")
    
    # Show sample
    print("\n📋 Sample Results:")
    for prog, data in list(menu_data.items())[:3]:
        print(f"\n  {prog}:")
        for opt in data['menu_options'][:5]:
            calls = opt.get('calls', '?')
            print(f"    {opt['number']}. {opt['text'][:40]}... → {calls}")
    
    # Offer to update index
    print("\n" + "=" * 80)
    response = input("\n❓ Update menu_trees index with this data? (y/n): ")
    if response.lower() == 'y':
        update_menu_trees_index(menu_data)
    else:
        print("\n💡 Run this command to update the index later:")
        print("   python update_menu_trees_with_options.py menu_options_extracted.json")
    
    print("\n✅ Menu option extraction complete!")
    print("\n📄 Review results in menu_options_extracted.json")

if __name__ == "__main__":
    main()
