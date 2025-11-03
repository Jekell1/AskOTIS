#!/usr/bin/env python3
"""
Header Standardization Script for COBOL Documentation
Standardizes all documentation headers to match LONPF7 format
"""

import os
import re
from pathlib import Path

def read_file(filepath):
    """Read a text file with proper encoding handling"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.read()
    except UnicodeDecodeError:
        with open(filepath, 'r', encoding='latin-1') as f:
            return f.read()

def write_file(filepath, content):
    """Write a text file with UTF-8 encoding"""
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)

def extract_program_info(content):
    """Extract program information from existing headers"""
    lines = content.split('\n')
    
    # Extract title (first line)
    title_match = re.match(r'^# (.+)', lines[0]) if lines else None
    title = title_match.group(1) if title_match else "Unknown Program"
    
    # Extract program ID from title or metadata
    program_id = None
    location = None
    date_written = "Legacy system (pre-2000)"
    
    for line in lines[:20]:  # Check first 20 lines
        if line.startswith('**Program ID:**'):
            program_id = re.search(r'\*\*Program ID:\*\*\s*(.+)', line)
            program_id = program_id.group(1).strip() if program_id else None
        elif line.startswith('**Location:**'):
            location = re.search(r'\*\*Location:\*\*\s*`?(.+?)`?', line)
            location = location.group(1).strip() if location else None
        elif line.startswith('**Date Written:**'):
            date_written = re.search(r'\*\*Date Written:\*\*\s*(.+)', line)
            date_written = date_written.group(1).strip() if date_written else "Legacy system (pre-2000)"
    
    # If program ID not found, extract from title
    if not program_id:
        program_id_match = re.match(r'^# ([A-Z0-9\-]+)', lines[0])
        program_id = program_id_match.group(1) if program_id_match else "UNKNOWN"
    
    # Default location if not found
    if not location:
        if program_id.startswith('LONPF'):
            location = f".\\S35-Source\\LP\\{program_id}.CBL"
        elif program_id.startswith('C-'):
            location = f".\\{program_id}.cpy"
        else:
            location = f".\\APIPAY_Inlined.CBL"
    
    return {
        'title': title,
        'program_id': program_id,
        'location': location,
        'date_written': date_written
    }

def create_standard_header(info):
    """Create standardized header based on LONPF7 format"""
    return f"""# {info['title']}

**Location:** {info['location']}  
**Generated on:** July 22, 2025  
**Program ID:** {info['program_id']}  
**Date Written:** {info['date_written']}

## Table of Contents

1. [Program Overview](#program-overview)
2. [Transaction Types Supported](#transaction-types-supported)
3. [Input Parameters](#input-parameters)
4. [Output Fields](#output-fields)
5. [Program Flow Diagrams](#program-flow-diagrams)
6. [Batch Processing Timeline](#batch-processing-timeline)
7. [Paragraph-Level Flow Explanation](#paragraph-level-flow-explanation)
8. [Data Flow Mapping](#data-flow-mapping)
9. [Referenced Programs](#referenced-programs)
10. [Error Handling and Validation](#error-handling-and-validation)
11. [Technical Implementation](#technical-implementation)
12. [Integration Points](#integration-points)
13. [File Dependencies](#file-dependencies)
14. [Call Graph of PERFORMed Paragraphs](#call-graph-of-performed-paragraphs)

---"""

def standardize_file_header(file_path):
    """Standardize header for a single file"""
    print(f"Processing: {file_path.name}")
    
    content = read_file(file_path)
    lines = content.split('\n')
    
    # Find where the content after header starts (look for ## Program Overview or similar)
    content_start = 0
    for i, line in enumerate(lines):
        if line.startswith('## Program Overview') or (line.startswith('##') and 'overview' in line.lower()):
            content_start = i
            break
    
    if content_start == 0:
        # If no Program Overview found, look for first ## after header info
        for i, line in enumerate(lines):
            if i > 5 and line.startswith('##') and not line.startswith('## Table of Contents'):
                content_start = i
                break
    
    # Extract program info from existing header
    info = extract_program_info(content)
    
    # Create new standardized header
    new_header = create_standard_header(info)
    
    # Combine with rest of content
    if content_start > 0:
        remaining_content = '\n'.join(lines[content_start:])
        new_content = new_header + '\n\n' + remaining_content
    else:
        # If we can't find the split point, just replace the header section
        new_content = new_header + '\n\n' + content[content.find('## Program Overview'):]
    
    # Write the updated file
    write_file(file_path, new_content)
    print(f"Updated: {file_path.name}")

def main():
    """Main function to process all documentation files"""
    docs_dir = Path(__file__).parent / "Documents"
    
    print("COBOL Documentation Header Standardization")
    print("=" * 50)
    
    # Skip certain files
    skip_files = {'README.md', 'MAINTENANCE.md', 'README_STATIC.md'}
    
    # Process all markdown files
    processed = 0
    for md_file in docs_dir.glob("*.md"):
        if md_file.name not in skip_files:
            try:
                standardize_file_header(md_file)
                processed += 1
            except Exception as e:
                print(f"Error processing {md_file.name}: {e}")
    
    print(f"\nProcessed {processed} files")
    print("Header standardization completed!")

if __name__ == "__main__":
    main()
