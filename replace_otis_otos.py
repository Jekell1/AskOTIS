#!/usr/bin/env python3
"""
Replace all instances of OTIS/OTOS with OTIS in markdown files
"""

import os
import re
from pathlib import Path

def replace_in_file(filepath):
    """Replace OTIS/OTOS with OTIS in a file"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # Replace various forms
        content = content.replace('OTIS/OTOS', 'OTIS')
        content = content.replace('OTOS/OTIS', 'OTIS')
        content = re.sub(r'OTIS\s*/\s*OTOS', 'OTIS', content)
        content = re.sub(r'OTOS\s*/\s*OTIS', 'OTIS', content)
        
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
    print("Replacing OTIS/OTOS with OTIS in all markdown files")
    print("=" * 70)
    print()
    
    # Find all markdown files
    markdown_files = list(Path('.').glob('Chapter*.md')) + list(Path('.').glob('Appendices*.md'))
    
    updated_count = 0
    
    for md_file in sorted(markdown_files):
        if replace_in_file(md_file):
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
        print("Next steps:")
        print("  1. Regenerate Word documents: python regenerate_all_chapters_to_word.py")
        print("  2. Regenerate HTML book: python create_complete_book_html.py")

if __name__ == "__main__":
    main()
