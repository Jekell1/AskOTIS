#!/usr/bin/env python3
"""
Create a professional HTML book with all 18 chapters and table of contents
"""

import re
from pathlib import Path

def read_markdown_file(filepath):
    """Read a markdown file and return its content"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.read()
    except FileNotFoundError:
        print(f"Warning: {filepath} not found")
        return None

def extract_sections(md_content, chapter_num):
    """Extract ## section titles from markdown content for TOC"""
    sections = []
    if not md_content:
        return sections
    
    lines = md_content.split('\n')
    for line in lines:
        if line.startswith('## ') and not line.startswith('### '):
            title = line[3:].strip()
            section_id = re.sub(r'[^a-z0-9]+', '-', title.lower()).strip('-')
            sections.append({
                'title': title,
                'id': f"ch{chapter_num}-{section_id}"
            })
    return sections

def markdown_to_html(md_content, chapter_num):
    """Convert markdown to HTML with proper formatting"""
    if not md_content:
        return ""
    
    html = []
    lines = md_content.split('\n')
    in_code_block = False
    code_lang = ""
    in_paragraph = False
    in_table = False
    table_lines = []
    in_svg = False
    svg_lines = []
    
    for line in lines:
        # Handle SVG blocks
        if '<svg' in line.lower():
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            in_svg = True
            svg_lines = [line]
            continue
        
        if in_svg:
            svg_lines.append(line)
            if '</svg>' in line.lower():
                # End of SVG, add it directly to HTML
                html.append('<div class="svg-diagram">')
                html.extend(svg_lines)
                html.append('</div>')
                svg_lines = []
                in_svg = False
            continue
        
        # Handle table detection
        if '|' in line and line.strip().startswith('|'):
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            if not in_table:
                in_table = True
                table_lines = []
            table_lines.append(line)
            continue
        elif in_table and not line.strip():
            # End of table
            html.append(parse_markdown_table(table_lines))
            table_lines = []
            in_table = False
            continue
        elif in_table:
            # Continue collecting table lines if they contain |
            if '|' in line:
                table_lines.append(line)
                continue
            else:
                # End table and process current line
                html.append(parse_markdown_table(table_lines))
                table_lines = []
                in_table = False
        
        # Handle code blocks
        if line.startswith('```'):
            if not in_code_block:
                in_code_block = True
                code_lang = line[3:].strip() or 'plaintext'
                html.append(f'<pre><code class="language-{code_lang}">')
            else:
                in_code_block = False
                html.append('</code></pre>')
            continue
        
        if in_code_block:
            html.append(line.replace('<', '&lt;').replace('>', '&gt;'))
            continue
        
        # Handle headers
        if line.startswith('# '):
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            title = line[2:].strip()
            html.append(f'<h1 class="chapter-title" id="chapter-{chapter_num}">{title}</h1>')
        elif line.startswith('## '):
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            title = line[3:].strip()
            section_id = re.sub(r'[^a-z0-9]+', '-', title.lower()).strip('-')
            html.append(f'<h2 class="section-title" id="ch{chapter_num}-{section_id}">{title}</h2>')
        elif line.startswith('### '):
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            title = line[4:].strip()
            html.append(f'<h3 class="subsection-title">{title}</h3>')
        elif line.startswith('#### '):
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            title = line[5:].strip()
            html.append(f'<h4 class="subsubsection-title">{title}</h4>')
        # Handle lists
        elif line.strip().startswith('- ') or line.strip().startswith('* '):
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            item = line.strip()[2:]
            item = format_inline_markdown(item)
            html.append(f'<li>{item}</li>')
        # Handle paragraphs
        elif line.strip():
            if not in_paragraph:
                html.append('<p>')
                in_paragraph = True
            formatted_line = format_inline_markdown(line)
            html.append(formatted_line)
        else:
            if in_paragraph:
                html.append('</p>')
                in_paragraph = False
            html.append('')
    
    if in_paragraph:
        html.append('</p>')
    
    if in_table and table_lines:
        html.append(parse_markdown_table(table_lines))
    
    return '\n'.join(html)

def parse_markdown_table(table_lines):
    """Convert markdown table to HTML table"""
    if len(table_lines) < 2:
        return ""
    
    # Parse header row
    header_row = table_lines[0]
    headers = [cell.strip() for cell in header_row.split('|') if cell.strip()]
    
    if len(headers) == 0:
        return ""
    
    # Skip separator line (line with dashes)
    data_start = 2 if len(table_lines) > 2 and '-' in table_lines[1] else 1
    
    # Parse data rows
    data_rows = []
    for line in table_lines[data_start:]:
        cells = [cell.strip() for cell in line.split('|') if cell.strip()]
        if cells and len(cells) == len(headers):
            data_rows.append(cells)
    
    # Build HTML table
    html = ['<table class="content-table">']
    html.append('<thead>')
    html.append('<tr>')
    for header in headers:
        formatted_header = format_inline_markdown(header)
        html.append(f'<th>{formatted_header}</th>')
    html.append('</tr>')
    html.append('</thead>')
    html.append('<tbody>')
    
    for row in data_rows:
        html.append('<tr>')
        for cell in row:
            formatted_cell = format_inline_markdown(cell)
            html.append(f'<td>{formatted_cell}</td>')
        html.append('</tr>')
    
    html.append('</tbody>')
    html.append('</table>')
    
    return '\n'.join(html)

def format_inline_markdown(text):
    """Format inline markdown like bold, italic, code"""
    # Inline code
    text = re.sub(r'`([^`]+)`', r'<code>\1</code>', text)
    # Bold
    text = re.sub(r'\*\*([^*]+)\*\*', r'<strong>\1</strong>', text)
    # Italic
    text = re.sub(r'\*([^*]+)\*', r'<em>\1</em>', text)
    return text

def create_html_book():
    """Create the complete HTML book"""
    
    chapters = [
        ("Chapter1_Introduction.md", "Chapter 1", "Introduction"),
        ("Chapter2_Business_Domain.md", "Chapter 2", "Business Domain Analysis"),
        ("Chapter3_Technical_Architecture.md", "Chapter 3", "Technical Architecture"),
        ("Chapter4_Mapping_Codebase.md", "Chapter 4", "Mapping the Legacy Codebase"),
        ("Chapter5_Modernization_Goals.md", "Chapter 5", "Setting Modernization Goals"),
        ("Chapter6_Tools_Technologies.md", "Chapter 6", "Choosing Tools and Technologies"),
        ("Chapter7_Target_Architecture.md", "Chapter 7", "Designing Target Architecture"),
        ("Chapter8_Code_Analysis.md", "Chapter 8", "Automated and Manual Code Analysis"),
        ("Chapter9_Refactoring_Optimization.md", "Chapter 9", "Refactoring for Optimization"),
        ("Chapter10_API_Modeling_Design.md", "Chapter 10", "API Modeling and Design"),
        ("Chapter11_Implementing_APIs.md", "Chapter 11", "Implementing C# APIs"),
        ("Chapter12_Building_Frontend.md", "Chapter 12", "Building the New Front End"),
        ("Chapter13_Data_Migration.md", "Chapter 13", "Data Migration Strategies"),
        ("Chapter14_Testing_QA.md", "Chapter 14", "Testing and Quality Assurance"),
        ("Chapter15_Deployment_Rollout.md", "Chapter 15", "Deployment and Rollout"),
        ("Chapter16_Case_Studies.md", "Chapter 16", "Real-World Conversion Examples"),
        ("Chapter17_Common_Pitfalls.md", "Chapter 17", "Common Pitfalls and How to Avoid Them"),
        ("Chapter18_System_Maintenance.md", "Chapter 18", "Maintaining and Evolving the New System"),
    ]
    
    # HTML header with professional styling
    html = '''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Modernizing OTIS: COBOL to C# Transformation</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: 'Georgia', 'Times New Roman', serif;
            line-height: 1.8;
            color: #2c3e50;
            background: #f8f9fa;
            padding: 20px;
        }
        
        .book-container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            box-shadow: 0 0 40px rgba(0,0,0,0.1);
        }
        
        /* Cover Page */
        .cover-page {
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            padding: 100px 60px;
            text-align: center;
            min-height: 100vh;
            display: flex;
            flex-direction: column;
            justify-content: center;
        }
        
        .book-title {
            font-size: 3.5em;
            font-weight: bold;
            margin-bottom: 30px;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
            letter-spacing: 2px;
        }
        
        .book-subtitle {
            font-size: 1.8em;
            font-weight: 300;
            margin-bottom: 60px;
            font-style: italic;
            opacity: 0.95;
        }
        
        .book-meta {
            font-size: 1.2em;
            margin-top: 80px;
            opacity: 0.9;
        }
        
        /* Table of Contents */
        .toc-page {
            padding: 80px 60px;
            background: #fafbfc;
            page-break-after: always;
        }
        
        .toc-title {
            font-size: 2.5em;
            color: #1e3c72;
            margin-bottom: 40px;
            text-align: center;
            border-bottom: 3px solid #2a5298;
            padding-bottom: 20px;
        }
        
        .toc-part {
            margin: 40px 0;
        }
        
        .toc-part-title {
            font-size: 1.5em;
            color: #2a5298;
            margin-bottom: 20px;
            font-weight: bold;
            text-transform: uppercase;
            letter-spacing: 1px;
        }
        
        .toc-list {
            list-style: none;
            padding-left: 20px;
        }
        
        .toc-item {
            margin: 15px 0;
            padding: 12px 20px;
            background: white;
            border-left: 4px solid #2a5298;
            transition: all 0.3s ease;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        
        .toc-item:hover {
            background: #e8f4f8;
            border-left-width: 6px;
            transform: translateX(5px);
        }
        
        .toc-item a {
            text-decoration: none;
            color: #2c3e50;
            font-size: 1.1em;
            flex: 1;
        }
        
        .toc-item a:hover {
            color: #2a5298;
        }
        
        .toc-chapter-num {
            font-weight: bold;
            color: #2a5298;
            margin-right: 15px;
        }
        
        /* Chapter Content */
        .chapter {
            padding: 80px 60px;
            page-break-before: always;
        }
        
        .chapter-title {
            font-size: 2.8em;
            color: #1e3c72;
            margin-bottom: 40px;
            border-bottom: 3px solid #2a5298;
            padding-bottom: 20px;
        }
        
        .section-title {
            font-size: 2em;
            color: #2a5298;
            margin: 50px 0 25px 0;
            padding-left: 15px;
            border-left: 5px solid #2a5298;
        }
        
        .subsection-title {
            font-size: 1.5em;
            color: #34495e;
            margin: 35px 0 20px 0;
            font-weight: 600;
        }
        
        .subsubsection-title {
            font-size: 1.2em;
            color: #546e7a;
            margin: 25px 0 15px 0;
            font-weight: 600;
        }
        
        p {
            margin: 20px 0;
            text-align: justify;
            font-size: 1.05em;
            line-height: 1.9;
        }
        
        code {
            background: #f4f4f4;
            padding: 2px 8px;
            border-radius: 3px;
            font-family: 'Consolas', 'Monaco', monospace;
            font-size: 0.9em;
            color: #c7254e;
        }
        
        pre {
            background: #2d2d2d;
            color: #f8f8f2;
            padding: 25px;
            border-radius: 8px;
            overflow-x: auto;
            margin: 30px 0;
            border-left: 5px solid #2a5298;
        }
        
        pre code {
            background: none;
            padding: 0;
            color: #f8f8f2;
            font-size: 0.95em;
        }
        
        /* Tables */
        table.content-table {
            width: 100%;
            border-collapse: collapse;
            margin: 30px 0;
            font-size: 1em;
            box-shadow: 0 2px 15px rgba(0,0,0,0.1);
            border-radius: 8px;
            overflow: hidden;
        }
        
        table.content-table thead tr {
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            text-align: left;
            font-weight: bold;
        }
        
        table.content-table th,
        table.content-table td {
            padding: 15px 20px;
            border: 1px solid #ddd;
        }
        
        table.content-table th {
            font-size: 1.05em;
            letter-spacing: 0.5px;
        }
        
        table.content-table tbody tr {
            border-bottom: 1px solid #ddd;
        }
        
        table.content-table tbody tr:nth-of-type(even) {
            background-color: #f8f9fa;
        }
        
        table.content-table tbody tr:hover {
            background-color: #e3f2fd;
            transition: background-color 0.3s ease;
        }
        
        table.content-table tbody tr:last-of-type {
            border-bottom: 3px solid #2a5298;
        }
        
        /* SVG Diagrams */
        .svg-diagram {
            margin: 40px auto;
            text-align: center;
            padding: 20px;
            background: #fafbfc;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .svg-diagram svg {
            max-width: 100%;
            height: auto;
        }
        
        li {
            margin: 12px 0 12px 40px;
            font-size: 1.05em;
        }
        
        strong {
            color: #1e3c72;
            font-weight: 600;
        }
        
        /* Floating Back to TOC Button */
        .back-to-toc {
            position: fixed;
            bottom: 30px;
            right: 30px;
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            padding: 15px 25px;
            border-radius: 50px;
            text-decoration: none;
            box-shadow: 0 4px 20px rgba(30, 60, 114, 0.4);
            font-weight: 600;
            font-size: 0.95em;
            transition: all 0.3s ease;
            z-index: 1000;
            display: none;
        }
        
        .back-to-toc:hover {
            transform: translateY(-3px);
            box-shadow: 0 6px 30px rgba(30, 60, 114, 0.6);
            background: linear-gradient(135deg, #2a5298 0%, #1e3c72 100%);
        }
        
        .back-to-toc.visible {
            display: block;
            animation: fadeIn 0.3s ease;
        }
        
        @keyframes fadeIn {
            from {
                opacity: 0;
                transform: translateY(20px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }
        
        /* TOC Section List */
        .toc-sections {
            margin-left: 30px;
            margin-top: 10px;
            margin-bottom: 20px;
        }
        
        .toc-section-item {
            list-style: none;
            margin: 8px 0;
        }
        
        .toc-section-item a {
            color: #4a5568;
            text-decoration: none;
            font-size: 0.95em;
            padding: 5px 0;
            display: block;
            transition: color 0.2s;
        }
        
        .toc-section-item a:hover {
            color: #2a5298;
            padding-left: 10px;
        }
        
        .toc-section-item a::before {
            content: "• ";
            color: #cbd5e0;
            margin-right: 8px;
        }
        
        em {
            font-style: italic;
            color: #546e7a;
        }
        
        /* Appendices */
        .appendix {
            padding: 80px 60px;
            page-break-before: always;
            background: #fafbfc;
        }
        
        .appendix-title {
            font-size: 2.5em;
            color: #1e3c72;
            margin-bottom: 40px;
            text-align: center;
            border-bottom: 3px solid #2a5298;
            padding-bottom: 20px;
        }
        
        /* Print Styles */
        @media print {
            body {
                background: white;
                padding: 0;
            }
            
            .book-container {
                box-shadow: none;
            }
            
            .cover-page,
            .toc-page,
            .chapter,
            .appendix {
                page-break-after: always;
            }
        }
        

        /* Floating Back to TOC Button */
        .back-to-toc {
            position: fixed;
            bottom: 30px;
            right: 30px;
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            padding: 15px 25px;
            border-radius: 50px;
            text-decoration: none;
            box-shadow: 0 4px 20px rgba(30, 60, 114, 0.4);
            font-weight: 600;
            font-size: 0.95em;
            transition: all 0.3s ease;
            z-index: 1000;
            display: none;
        }
        
        .back-to-toc:hover {
            transform: translateY(-3px);
            box-shadow: 0 6px 30px rgba(30, 60, 114, 0.6);
            background: linear-gradient(135deg, #2a5298 0%, #1e3c72 100%);
        }
        
        .back-to-toc.visible {
            display: block;
            animation: fadeIn 0.3s ease;
        }
        
        @keyframes fadeIn {
            from {
                opacity: 0;
                transform: translateY(20px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }
        
        /* TOC Section List */
        .toc-sections {
            margin-left: 30px;
            margin-top: 10px;
            margin-bottom: 20px;
        }
        
        .toc-section-item {
            list-style: none;
            margin: 8px 0;
        }
        
        .toc-section-item a {
            color: #4a5568;
            text-decoration: none;
            font-size: 0.95em;
            padding: 5px 0;
            display: block;
            transition: color 0.2s;
        }
        
        .toc-section-item a:hover {
            color: #2a5298;
            padding-left: 10px;
        }
        
        .toc-section-item a::before {
            content: "• ";
            color: #cbd5e0;
            margin-right: 8px;
        }
        /* Responsive */
        @media (max-width: 768px) {
            .book-title {
                font-size: 2em;
            }
            
            .book-subtitle {
                font-size: 1.2em;
            }
            
            .chapter {
                padding: 40px 30px;
            }
            
            .chapter-title {
                font-size: 2em;
            }
        }
    </style>
</head>
<body>
    <div class="book-container">
        <!-- Cover Page -->
        <div class="cover-page">
            <h1 class="book-title">MODERNIZING OTIS</h1>
            <h2 class="book-subtitle">A Comprehensive Guide to COBOL-to-C# Transformation</h2>
            <p class="book-meta">Complete Edition • 2025</p>
        </div>
        
        <!-- Table of Contents -->
        <div class="toc-page" id="toc">
            <h2 class="toc-title">Table of Contents</h2>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Part I: Understanding the Legacy System</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#chapter-1">
                            <span class="toc-chapter-num">Chapter 1</span>
                            Introduction
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-2">
                            <span class="toc-chapter-num">Chapter 2</span>
                            Business Domain Analysis
                        </a>
                    </li>
                </ul>
            </div>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Part II: Assessment and Planning</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#chapter-3">
                            <span class="toc-chapter-num">Chapter 3</span>
                            Technical Architecture
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-4">
                            <span class="toc-chapter-num">Chapter 4</span>
                            Mapping the Legacy Codebase
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-5">
                            <span class="toc-chapter-num">Chapter 5</span>
                            Setting Modernization Goals
                        </a>
                    </li>
                </ul>
            </div>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Part III: Designing the Target System</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#chapter-6">
                            <span class="toc-chapter-num">Chapter 6</span>
                            Choosing Tools and Technologies
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-7">
                            <span class="toc-chapter-num">Chapter 7</span>
                            Designing Target Architecture
                        </a>
                    </li>
                </ul>
            </div>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Part IV: Conversion and Implementation</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#chapter-8">
                            <span class="toc-chapter-num">Chapter 8</span>
                            Automated and Manual Code Analysis
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-9">
                            <span class="toc-chapter-num">Chapter 9</span>
                            Refactoring for Optimization
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-10">
                            <span class="toc-chapter-num">Chapter 10</span>
                            API Modeling and Design
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-11">
                            <span class="toc-chapter-num">Chapter 11</span>
                            Implementing C# APIs
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-12">
                            <span class="toc-chapter-num">Chapter 12</span>
                            Building the New Front End
                        </a>
                    </li>
                </ul>
            </div>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Part V: Migration and Integration</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#chapter-13">
                            <span class="toc-chapter-num">Chapter 13</span>
                            Data Migration Strategies
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-14">
                            <span class="toc-chapter-num">Chapter 14</span>
                            Testing and Quality Assurance
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-15">
                            <span class="toc-chapter-num">Chapter 15</span>
                            Deployment and Rollout
                        </a>
                    </li>
                </ul>
            </div>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Part VI: Case Studies and Best Practices</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#chapter-16">
                            <span class="toc-chapter-num">Chapter 16</span>
                            Real-World Conversion Examples
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-17">
                            <span class="toc-chapter-num">Chapter 17</span>
                            Common Pitfalls and How to Avoid Them
                        </a>
                    </li>
                    <li class="toc-item">
                        <a href="#chapter-18">
                            <span class="toc-chapter-num">Chapter 18</span>
                            Maintaining and Evolving the New System
                        </a>
                    </li>
                </ul>
            </div>
            
            <div class="toc-part">
                <h3 class="toc-part-title">Appendices</h3>
                <ul class="toc-list">
                    <li class="toc-item">
                        <a href="#appendices">
                            <span class="toc-chapter-num">Appendix</span>
                            Reference Materials
                        </a>
                    </li>
                </ul>
            </div>
        </div>
'''
    
    # Add all chapters
    for idx, (filename, chapter_num, chapter_title) in enumerate(chapters, 1):
        print(f"Processing {chapter_num}: {chapter_title}...")
        md_content = read_markdown_file(filename)
        if md_content:
            chapter_html = markdown_to_html(md_content, idx)
            html += f'\n        <!-- {chapter_num} -->\n'
            html += f'        <div class="chapter">\n{chapter_html}\n        </div>\n'
    
    # Add Appendices
    print("Processing Appendices...")
    appendices_content = read_markdown_file("Appendices.md")
    if appendices_content:
        appendices_html = markdown_to_html(appendices_content, 'appendices')
        html += f'\n        <!-- Appendices -->\n'
        html += f'        <div class="appendix" id="appendices">\n{appendices_html}\n        </div>\n'
    
    # Close HTML
    html += '''

        <!-- Floating Back to TOC Button -->
        <a href="#toc" class="back-to-toc" id="backToToc">↑ Table of Contents</a>
    </div>
    
    <script>
        // Show/hide floating button based on scroll position
        window.addEventListener('scroll', function() {
            const backToToc = document.getElementById('backToToc');
            const tocPage = document.querySelector('.toc-page');
            if (tocPage) {
                const tocBottom = tocPage.offsetTop + tocPage.offsetHeight;
                
                if (window.pageYOffset > tocBottom + 200) {
                    backToToc.classList.add('visible');
                } else {
                    backToToc.classList.remove('visible');
                }
            }
        });
        
        // Smooth scrolling for all internal links
        document.querySelectorAll('a[href^="#"]').forEach(anchor => {
            anchor.addEventListener('click', function (e) {
                e.preventDefault();
                const target = document.querySelector(this.getAttribute('href'));
                if (target) {
                    target.scrollIntoView({
                        behavior: 'smooth',
                        block: 'start'
                    });
                }
            });
        });
    </script>
</body>
</html>'''
    
    # Write the complete HTML file
    output_file = "OTIS_Modernization_Complete_Book.html"
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(html)
    
    print(f"\n{'='*70}")
    print(f"✅ Complete HTML book created: {output_file}")
    print(f"{'='*70}")
    print(f"\nBook includes:")
    print(f"  - Professional cover page")
    print(f"  - Interactive table of contents")
    print(f"  - All 18 chapters with full content")
    print(f"  - Appendices with reference materials")
    print(f"  - Professional styling and typography")
    print(f"  - Print-ready formatting")
    print(f"\nOpen {output_file} in your web browser to view!")

if __name__ == "__main__":
    create_html_book()
