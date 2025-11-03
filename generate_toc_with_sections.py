#!/usr/bin/env python3
"""
Generate TOC entries with section lists for each chapter
"""

import re
from pathlib import Path

def extract_sections(md_file, chapter_idx):
    """Extract section titles (##) from markdown file"""
    sections = []
    try:
        with open(md_file, 'r', encoding='utf-8') as f:
            for line in f:
                if line.startswith('## ') and not line.startswith('### '):
                    title = line[3:].strip()
                    section_id = re.sub(r'[^a-z0-9]+', '-', title.lower()).strip('-')
                    sections.append({
                        'title': title,
                        'id': f"ch{chapter_idx}-{section_id}"
                    })
    except FileNotFoundError:
        pass
    return sections

chapters = [
    ("Chapter1_Introduction.md", 1, "Introduction"),
    ("Chapter2_Business_Domain.md", 2, "Business Domain Analysis"),
    ("Chapter3_Technical_Architecture.md", 3, "Technical Architecture"),
    ("Chapter4_Mapping_Codebase.md", 4, "Mapping the Legacy Codebase"),
    ("Chapter5_Modernization_Goals.md", 5, "Setting Modernization Goals"),
    ("Chapter6_Tools_Technologies.md", 6, "Choosing Tools and Technologies"),
    ("Chapter7_Target_Architecture.md", 7, "Designing Target Architecture"),
    ("Chapter8_Code_Analysis.md", 8, "Automated and Manual Code Analysis"),
    ("Chapter9_Refactoring_Optimization.md", 9, "Refactoring for Optimization"),
    ("Chapter10_API_Modeling_Design.md", 10, "API Modeling and Design"),
    ("Chapter11_Implementing_APIs.md", 11, "Implementing C# APIs"),
    ("Chapter12_Building_Frontend.md", 12, "Building the New Front End"),
    ("Chapter13_Data_Migration.md", 13, "Data Migration Strategies"),
    ("Chapter14_Testing_QA.md", 14, "Testing and Quality Assurance"),
    ("Chapter15_Deployment_Rollout.md", 15, "Deployment and Rollout"),
    ("Chapter16_Case_Studies.md", 16, "Real-World Conversion Examples"),
    ("Chapter17_Common_Pitfalls.md", 17, "Common Pitfalls and How to Avoid Them"),
    ("Chapter18_System_Maintenance.md", 18, "Maintaining and Evolving the New System"),
]

# Build TOC HTML with sections
toc_parts = {
    'Part I': [(1, 2)],
    'Part II': [(3, 5)],
    'Part III': [(6, 7)],
    'Part IV': [(8, 12)],
    'Part V': [(13, 15)],
    'Part VI': [(16, 18)]
}

part_titles = {
    'Part I': 'Understanding the Legacy System',
    'Part II': 'Assessment and Planning',
    'Part III': 'Designing the Target System',
    'Part IV': 'Conversion and Implementation',
    'Part V': 'Migration and Integration',
    'Part VI': 'Case Studies and Best Practices'
}

toc_html = []

for part, ranges in toc_parts.items():
    toc_html.append(f'            <div class="toc-part">')
    toc_html.append(f'                <h3 class="toc-part-title">{part}: {part_titles[part]}</h3>')
    toc_html.append(f'                <ul class="toc-list">')
    
    for start, end in ranges:
        for filename, idx, title in chapters:
            if start <= idx <= end:
                sections = extract_sections(filename, idx)
                toc_html.append(f'                    <li class="toc-item">')
                toc_html.append(f'                        <a href="#chapter-{idx}">')
                toc_html.append(f'                            <span class="toc-chapter-num">Chapter {idx}</span>')
                toc_html.append(f'                            {title}')
                toc_html.append(f'                        </a>')
                
                if sections:
                    toc_html.append(f'                        <ul class="toc-sections">')
                    for section in sections:  # Show ALL sections
                        toc_html.append(f'                            <li class="toc-section-item">')
                        toc_html.append(f'                                <a href="#{section["id"]}">{section["title"]}</a>')
                        toc_html.append(f'                            </li>')
                    toc_html.append(f'                        </ul>')
                
                toc_html.append(f'                    </li>')
    
    toc_html.append(f'                </ul>')
    toc_html.append(f'            </div>')
    toc_html.append('')

# Write the TOC HTML to a file
with open('toc_with_sections.html', 'w', encoding='utf-8') as f:
    f.write('\n'.join(toc_html))

print("✅ Generated TOC with sections: toc_with_sections.html")
print(f"   - Extracted sections from {len(chapters)} chapters")
print("   - Organized into 6 parts")
print("   - Showing ALL sections for each chapter")
print("\nYou can now copy this into the HTML generator!")
