#!/usr/bin/env python3
"""
Remove all "(sometimes referred to as OTOS)" phrases from markdown files
"""

import os
import re
from pathlib import Path

def clean_file(filepath):
    """Remove OTOS references from a file"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # Remove various forms of OTOS references
        content = re.sub(r'\s*\(sometimes referred to as OTOS\)', '', content, flags=re.IGNORECASE)
        content = re.sub(r'\s*\(also known as OTOS\)', '', content, flags=re.IGNORECASE)
        content = re.sub(r'\s*\(formerly OTOS\)', '', content, flags=re.IGNORECASE)
        content = re.sub(r'\s*\(or OTOS\)', '', content, flags=re.IGNORECASE)
        content = re.sub(r'\s*\(OTOS\)', '', content, flags=re.IGNORECASE)
        content = re.sub(r'\s*\(\s*sometimes\s+called\s+OTOS\s*\)', '', content, flags=re.IGNORECASE)
        content = re.sub(r'\s*\(\s*previously\s+OTOS\s*\)', '', content, flags=re.IGNORECASE)
        
        if content != original_content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(content)
            return True
        return False
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    print("=" * 70)
    print("Removing '(sometimes referred to as OTOS)' phrases")
    print("=" * 70)
    print()
    
    # Find all markdown files
    markdown_files = list(Path('.').glob('Chapter*.md')) + list(Path('.').glob('Appendices*.md'))
    
    updated_count = 0
    
    for md_file in sorted(markdown_files):
        if clean_file(md_file):
            print(f"✅ Updated: {md_file.name}")
            updated_count += 1
        else:
            print(f"⏭️  No changes: {md_file.name}")
    
    print()
    print("=" * 70)
    print(f"Summary: Updated {updated_count} file(s)")
    print("=" * 70)
    
    if updated_count > 0:
        print()
        print("Regenerating HTML book...")
        os.system("python create_complete_book_html.py")

if __name__ == "__main__":
    main()
