"""
Parse A15 help files and extract field-level documentation.

These help files contain context-sensitive help text for form fields,
keyed by field identifiers like \010\, \LAWI05-020\, etc.

Output: Structured data ready for Azure Search indexing.
"""

import os
import re
from pathlib import Path
from typing import List, Dict, Tuple
import json

HELP_ROOT = r"C:\Users\jeff.childers\Documents\OTISCodeResearcher\Documents\A15_help"


def parse_help_file(file_path: Path) -> List[Dict]:
    """
    Parse a single help file and extract field help entries.
    
    Returns list of dicts with:
    - field_id: The field identifier (e.g., "010", "LAWI05-020")
    - help_text: The help text content
    - screen_id: The screen/program name (derived from filename)
    - module: The module directory (AM, LP, etc.)
    """
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        return []
    
    # Extract module and screen name
    module = file_path.parent.name
    screen_id = file_path.name
    
    # Pattern to match field identifiers: \<identifier>\
    # Examples: \010\, \LAWI05-020\, \GENERALHELP\, \SEL\
    pattern = r'\\([^\\]+)\\\s*\n(.*?)(?=\\[^\\]+\\|\Z)'
    
    entries = []
    matches = re.finditer(pattern, content, re.DOTALL)
    
    for match in matches:
        field_id = match.group(1).strip()
        help_text = match.group(2).strip()
        
        if not help_text:
            continue
        
        # Clean up the help text
        help_text = help_text.replace('\n\n', ' ').replace('\n', ' ')
        help_text = re.sub(r'\s+', ' ', help_text)
        
        # Determine field type
        field_type = categorize_field(field_id)
        
        # Extract any validation rules or allowed values
        validation_info = extract_validation(help_text)
        
        entry = {
            'field_id': field_id,
            'help_text': help_text,
            'screen_id': screen_id,
            'module': module,
            'field_type': field_type,
            'validation': validation_info,
            'full_identifier': f"{module}_{screen_id}_{field_id}",
            'char_length': len(help_text)
        }
        
        entries.append(entry)
    
    return entries


def categorize_field(field_id: str) -> str:
    """Categorize the field identifier type."""
    if field_id in ['GENERALHELP', 'SEL', 'MSEL']:
        return 'system'
    elif field_id.startswith('SEL'):
        return 'selection'
    elif re.match(r'^\d{3,4}$', field_id):
        return 'numeric_position'
    elif re.match(r'^[A-Z]+\d+-\d+$', field_id):
        return 'program_field'
    elif re.match(r'^[A-Z]\d{3}$', field_id):
        return 'keyed_field'
    else:
        return 'other'


def extract_validation(help_text: str) -> Dict:
    """Extract validation rules from help text."""
    validation = {
        'has_scan_window': 'F6-KEY' in help_text and 'scan window' in help_text.lower(),
        'required': 'MUST' in help_text.upper() or 'REQUIRED' in help_text.upper(),
        'optional': 'SKIP this field' in help_text or 'press RETURN to SKIP' in help_text,
        'function_keys': extract_function_keys(help_text),
        'valid_values': extract_valid_values(help_text),
        'numeric': 'numeric' in help_text.lower() or 'number' in help_text.lower(),
        'date_field': 'DATE' in help_text.upper() or 'date' in help_text.lower()
    }
    return validation


def extract_function_keys(help_text: str) -> List[str]:
    """Extract function key references (F1-F10)."""
    keys = re.findall(r'F(\d+)-?KEY', help_text)
    return sorted(list(set(f"F{k}" for k in keys)))


def extract_valid_values(help_text: str) -> List[Dict]:
    """Extract valid values/codes from help text."""
    valid_values = []
    
    # Pattern: (X) - Description
    pattern = r'\(([A-Z0-9])\)\s*-\s*([^\n]+)'
    matches = re.findall(pattern, help_text)
    
    for code, description in matches:
        valid_values.append({
            'code': code,
            'description': description.strip()
        })
    
    return valid_values


def process_all_help_files() -> List[Dict]:
    """Process all help files in the A15_help directory."""
    all_entries = []
    
    help_path = Path(HELP_ROOT)
    
    if not help_path.exists():
        print(f"Error: Help directory not found: {HELP_ROOT}")
        return []
    
    # Process each module directory
    for module_dir in sorted(help_path.iterdir()):
        if not module_dir.is_dir():
            continue
        
        print(f"Processing module: {module_dir.name}")
        
        file_count = 0
        entry_count = 0
        
        # Process each help file in the module
        for help_file in sorted(module_dir.iterdir()):
            if not help_file.is_file():
                continue
            
            entries = parse_help_file(help_file)
            all_entries.extend(entries)
            
            file_count += 1
            entry_count += len(entries)
        
        print(f"  Files: {file_count}, Entries: {entry_count}")
    
    return all_entries


def generate_statistics(entries: List[Dict]) -> Dict:
    """Generate statistics about the parsed help entries."""
    from collections import Counter
    
    stats = {
        'total_entries': len(entries),
        'total_screens': len(set(e['screen_id'] for e in entries)),
        'modules': Counter(e['module'] for e in entries),
        'field_types': Counter(e['field_type'] for e in entries),
        'avg_help_length': sum(e['char_length'] for e in entries) / len(entries) if entries else 0,
        'fields_with_validation': sum(1 for e in entries if e['validation']['valid_values']),
        'fields_with_scan': sum(1 for e in entries if e['validation']['has_scan_window']),
        'required_fields': sum(1 for e in entries if e['validation']['required']),
        'optional_fields': sum(1 for e in entries if e['validation']['optional'])
    }
    
    return stats


def save_to_jsonl(entries: List[Dict], output_path: str):
    """Save entries to JSONL format for bulk upload."""
    with open(output_path, 'w', encoding='utf-8') as f:
        for entry in entries:
            json.dump(entry, f)
            f.write('\n')
    
    print(f"\nSaved {len(entries)} entries to {output_path}")


def main():
    """Main processing function."""
    print("=" * 80)
    print("A15 Help File Parser")
    print("=" * 80)
    print()
    
    # Process all help files
    print("Processing help files...")
    entries = process_all_help_files()
    
    if not entries:
        print("No entries found!")
        return
    
    print(f"\nTotal entries extracted: {len(entries)}")
    
    # Generate statistics
    print("\nGenerating statistics...")
    stats = generate_statistics(entries)
    
    print("\n" + "=" * 80)
    print("STATISTICS")
    print("=" * 80)
    print(f"Total Entries: {stats['total_entries']:,}")
    print(f"Total Screens: {stats['total_screens']:,}")
    print(f"Average Help Length: {stats['avg_help_length']:.0f} characters")
    print(f"\nFields with Validation Codes: {stats['fields_with_validation']:,}")
    print(f"Fields with Scan Windows: {stats['fields_with_scan']:,}")
    print(f"Required Fields: {stats['required_fields']:,}")
    print(f"Optional Fields: {stats['optional_fields']:,}")
    
    print(f"\nModules:")
    for module, count in sorted(stats['modules'].items()):
        print(f"  {module}: {count:,} entries")
    
    print(f"\nField Types:")
    for field_type, count in sorted(stats['field_types'].items()):
        print(f"  {field_type}: {count:,} entries")
    
    # Save to JSONL
    output_file = "help_fields.jsonl"
    save_to_jsonl(entries, output_file)
    
    # Save statistics
    stats_file = "help_fields_stats.json"
    with open(stats_file, 'w', encoding='utf-8') as f:
        json.dump(stats, f, indent=2, default=str)
    print(f"Saved statistics to {stats_file}")
    
    # Sample entries
    print("\n" + "=" * 80)
    print("SAMPLE ENTRIES")
    print("=" * 80)
    
    for i, entry in enumerate(entries[:3], 1):
        print(f"\nSample {i}:")
        print(f"  Module: {entry['module']}")
        print(f"  Screen: {entry['screen_id']}")
        print(f"  Field ID: {entry['field_id']}")
        print(f"  Field Type: {entry['field_type']}")
        print(f"  Help Text: {entry['help_text'][:150]}...")
        if entry['validation']['valid_values']:
            print(f"  Valid Values: {entry['validation']['valid_values']}")
        if entry['validation']['function_keys']:
            print(f"  Function Keys: {entry['validation']['function_keys']}")


if __name__ == '__main__':
    main()
