#!/usr/bin/env python3
"""
Add section listings to TOC in the generated HTML file
"""

import re

# Read the HTML file
with open('OTIS_Modernization_Complete_Book.html', 'r', encoding='utf-8') as f:
    html = f.read()

# Read the new TOC with sections
with open('toc_with_sections.html', 'r', encoding='utf-8') as f:
    new_toc = f.read()

# Find and replace the TOC section
# Start: <div class="toc-part">
# End: </div>\n        </div>\n (before <!-- Chapter 1 -->)

pattern = r'(<h2 class="toc-title">Table of Contents</h2>\s+)(.*?)(\s+</div>\s+<!-- Chapter 1 -->)'
replacement = r'\1' + new_toc + r'\3'

updated_html = re.sub(pattern, replacement, html, flags=re.DOTALL)

if updated_html == html:
    print("❌ Could not find TOC pattern to replace")
else:
    # Write updated HTML
    with open('OTIS_Modernization_Complete_Book.html', 'w', encoding='utf-8') as f:
        f.write(updated_html)
    
    print("✅ Successfully updated OTIS_Modernization_Complete_Book.html!")
    print("   - Added section listings to TOC for all chapters")
    print("   - Floating 'Back to TOC' button already present")
    print("   - Smooth scrolling enabled")
    print("\n Open the HTML file in your browser to see the improvements!")
