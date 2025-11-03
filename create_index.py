#!/usr/bin/env python3
"""
Create an alphabetical index for the OTIS Modernization book
"""

# Key terms to index with their relevant chapters
index_entries = {
    # A
    "API": [10, 11, 12],
    "API Design": [10],
    "API Gateway": [7, 10, 11],
    "API Versioning": [10],
    "Agile Methodology": [5, 14],
    "Architecture": [3, 7],
    "Azure": [6, 7, 11, 15],
    "Automated Testing": [14],
    
    # B
    "Batch Processing": [1, 2, 3, 8],
    "Business Domain": [2],
    "Business Logic": [2, 8, 9, 11],
    "Business Rules": [4, 8, 9],
    
    # C
    "C#": [6, 9, 10, 11],
    "Call Chains": [4, 8],
    "CICS": [1, 3, 4],
    "Cloud Computing": [6, 7, 15],
    "COBOL": [1, 3, 4, 8, 9],
    "Code Analysis": [8],
    "Copybooks": [4, 8, 9],
    "Continuous Integration": [14, 15],
    
    # D
    "Data Migration": [13],
    "Data Structures": [4, 9],
    "Data Validation": [4, 9, 11],
    "Database": [3, 7, 11, 13],
    "Deployment": [15],
    "DevOps": [15],
    "Docker": [7, 15],
    
    # E
    "Entity Framework": [11, 13],
    "Error Handling": [9, 11],
    
    # F
    "File Formats": [4, 13],
    "Frontend Development": [12],
    
    # I
    "IaaS": [6, 7],
    "Integration Testing": [14],
    
    # J
    "JCL": [1, 3, 4, 8],
    
    # K
    "Kubernetes": [7, 15],
    
    # L
    "Legacy System": [1, 2, 3, 4],
    "Loan Servicing": [2, 16],
    
    # M
    "Mainframe": [1, 3, 4],
    "Microservices": [7, 10, 11],
    "Migration Strategy": [5, 13],
    "Modernization": [1, 5, 6],
    
    # O
    "OTIS": [1, 2, 3, 4],
    
    # P
    "PaaS": [6, 7],
    "Performance": [9, 14],
    "Program Flow": [4, 8],
    
    # Q
    "Quality Assurance": [14],
    
    # R
    "Refactoring": [9],
    "REST API": [10, 11],
    "Rollout Strategy": [15],
    
    # S
    "SaaS": [6, 7],
    "Security": [7, 10, 11],
    "Serverless": [7, 11],
    "SQL": [3, 11, 13],
    
    # T
    "Target Architecture": [7],
    "Testing": [14],
    "Transaction Processing": [1, 2, 3],
    
    # U
    "Unit Testing": [14],
    "User Interface": [12],
    
    # V
    "Validation Logic": [4, 9, 11],
    "Version Control": [6, 15],
    "VSAM": [3, 4, 13],
    
    # W
    "Workflow": [2, 8],
}

# Group by first letter
grouped_index = {}
for term, chapters in sorted(index_entries.items()):
    first_letter = term[0].upper()
    if first_letter not in grouped_index:
        grouped_index[first_letter] = []
    grouped_index[first_letter].append((term, chapters))

# Generate HTML
html_lines = []
html_lines.append('<h2 class="section-title" id="index">Index</h2>')
html_lines.append('')
html_lines.append('<p>')
html_lines.append('This index provides quick reference to key concepts, technologies, and topics covered throughout the book. Numbers refer to chapter numbers.')
html_lines.append('</p>')
html_lines.append('')

for letter in sorted(grouped_index.keys()):
    html_lines.append(f'<h3 class="subsection-title">{letter}</h3>')
    html_lines.append('<ul class="index-list">')
    
    for term, chapters in grouped_index[letter]:
        chapter_links = ', '.join([f'<a href="#chapter-{ch}">{ch}</a>' for ch in chapters])
        html_lines.append(f'    <li><strong>{term}</strong>: {chapter_links}</li>')
    
    html_lines.append('</ul>')
    html_lines.append('')

# Write to file
with open('index_section.html', 'w', encoding='utf-8') as f:
    f.write('\n'.join(html_lines))

print("✅ Generated index section: index_section.html")
print(f"   - {len(index_entries)} terms indexed")
print(f"   - Organized alphabetically ({len(grouped_index)} letter groups)")
