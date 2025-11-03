#!/usr/bin/env python3
"""Add export buttons to otis-rag-chat.html"""

file_path = r"c:\Users\jeff.childers\Documents\OTISCodeResearcher\otis-rag-chat.html"

# Read the file
with open(file_path, 'r', encoding='utf-8') as f:
    content = f.read()

# The HTML to insert
export_section = '''
                <div class="sidebar-section">
                    <h3>📥 Export</h3>
                    <button class="example-btn" onclick="exportToExcel()" style="background: linear-gradient(135deg, #10b981 0%, #059669 100%); border: none; color: white; font-weight: 600; margin-bottom: 8px;">
                        📊 Export to Excel
                    </button>
                    <button class="example-btn" onclick="exportToWord()" style="background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; color: white; font-weight: 600;">
                        📄 Export to Word
                    </button>
                    <div style="margin-top: 8px; font-size: 11px; color: #6b7280;">
                        Download conversation history
                    </div>
                </div>'''

# Find the closing of the sidebar content and insert before it
marker = '            </div>\n        </div>\n\n        <!-- Main Chat Area -->'

if marker in content:
    content = content.replace(marker, export_section + '\n' + marker)
    print("✅ Successfully added export buttons!")
else:
    print("❌ Could not find insertion point")
    exit(1)

# Write back
with open(file_path, 'w', encoding='utf-8') as f:
    f.write(content)

print(f"✅ File updated: {file_path}")
