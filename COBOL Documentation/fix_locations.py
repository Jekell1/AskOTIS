#!/usr/bin/env python3
"""
Quick fix for location fields that got truncated
"""

import os
from pathlib import Path

def read_file(filepath):
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.read()
    except UnicodeDecodeError:
        with open(filepath, 'r', encoding='latin-1') as f:
            return f.read()

def write_file(filepath, content):
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)

# Define correct locations for key files
locations = {
    'APIPAY': '.\APIPAY_Inlined.CBL',
    'CJUL': '.\APIPAY_Inlined.CBL',
    'JUL': '.\APIPAY_Inlined.CBL',
    'GET-GPENV': '.\APIPAY_Inlined.CBL',
    'GET-TIME': '.\APIPAY_Inlined.CBL',
    'MAIN-PROGRAM': '.\APIPAY_Inlined.CBL',
    'LONPF2': '.\S35-Source\LP\LONPF2.CBL',
    'LONPF7': '.\APIPAY_Inlined.CBL',
    'LONPF9': '.\S35-Source\LP\LONPF9.CBL',
    'LONPFA': '.\S35-Source\LP\LONPFA.CBL',
    'LONPFB': '.\S35-Source\LP\LONPFB.CBL',
    'LONPFC': '.\S35-Source\LP\LONPFC.CBL',
    'WEEKDAY': '.\APIPAY_Inlined.CBL',
    'TIM': '.\APIPAY_Inlined.CBL',
    'TIMALL': '.\APIPAY_Inlined.CBL',
    'TIM360': '.\APIPAY_Inlined.CBL',
    'TIM365': '.\APIPAY_Inlined.CBL',
    'TIMUPER': '.\APIPAY_Inlined.CBL',
}

docs_dir = Path(__file__).parent / "Documents"

for md_file in docs_dir.glob("*_Documentation.md"):
    content = read_file(md_file)
    
    # Extract program ID from filename
    program_id = md_file.stem.replace('_Documentation', '')
    
    # Handle special cases
    if program_id.startswith('C-'):
        location = f'.\{program_id}.cpy'
    elif program_id in locations:
        location = locations[program_id]
    else:
        continue  # Skip files we don't have specific locations for
    
    # Fix location if it's just "."
    if '**Location:** .' in content and f'**Location:** {location}' not in content:
        content = content.replace('**Location:** .', f'**Location:** {location}')
        write_file(md_file, content)
        print(f"Fixed location for {md_file.name}")

print("Location fixes completed!")
