#!/usr/bin/env python3
"""
Replace the TOC section in HTML generator with enhanced version
"""

# Read both files
with open('create_complete_book_html.py', 'r', encoding='utf-8') as f:
    html_gen = f.read()

with open('toc_with_sections.html', 'r', encoding='utf-8') as f:
    new_toc = f.read()

# Find the TOC section boundaries
toc_start = html_gen.find('            <div class="toc-part">')
# Find where appendices start - that's where TOC ends
toc_end_marker = '        </div>\n        \n' + "'''"
toc_end = html_gen.find(toc_end_marker, toc_start)

if toc_start == -1 or toc_end == -1:
    print("❌ Could not find TOC boundaries")
    exit(1)

# Get everything before and after TOC
before_toc = html_gen[:toc_start]
after_toc = html_gen[toc_end:]

# Combine with new TOC
updated_html_gen = before_toc + new_toc + after_toc

# Write back
with open('create_complete_book_html.py', 'w', encoding='utf-8') as f:
    f.write(updated_html_gen)

print("✅ Successfully replaced TOC in HTML generator!")
print("   - Added section listings for all chapters")
print("   - Limited to 5 sections per chapter for clean TOC")
print("   - All sections are clickable with proper IDs")
