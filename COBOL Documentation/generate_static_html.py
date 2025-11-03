#!/usr/bin/env python3
"""
Static HTML Generator for COBOL Documentation
Converts Markdown files to HTML with external Mermaid diagram embedding
"""

import os
import re
import markdown
import sys
from pathlib import Path
from datetime import datetime

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

def convert_md_links_to_html(content):
    """Convert .md links to .html links"""
    # Pattern to match markdown links: [text](filename.md)
    pattern = r'\[([^\]]+)\]\(([^)]+)\.md\)'
    replacement = r'[\1](\2.html)'
    return re.sub(pattern, replacement, content)

def add_heading_anchors(html_content):
    """Add id attributes to headings for proper anchor linking"""
    def create_id(text):
        # Create a URL-friendly id from heading text
        id_text = re.sub(r'[^\w\s-]', '', text.lower())
        id_text = re.sub(r'[-\s]+', '-', id_text)
        return id_text.strip('-')
    
    def replace_heading(match):
        tag = match.group(1)
        content = match.group(2)
        anchor_id = create_id(content)
        return f'<{tag} id="{anchor_id}">{content}</{tag}>'
    
    # Add IDs to h1-h6 headings
    pattern = r'<(h[1-6])>([^<]+)</h[1-6]>'
    return re.sub(pattern, replace_heading, html_content)

def embed_mermaid_diagrams(html_content, diagrams_dir):
    """Replace Mermaid image references with embedded diagrams"""
    # Pattern for HTML img tags with .mmd files: <img alt="title" src="Diagrams/filename.mmd" />
    pattern = r'<img alt="([^"]*)" src="Diagrams/([^"]+)\.mmd"[^>]*/?>'
    
    def replace_diagram(match):
        title = match.group(1)
        filename = match.group(2)
        mermaid_file = os.path.join(diagrams_dir, f"{filename}.mmd")
        
        if os.path.exists(mermaid_file):
            try:
                mermaid_content = read_file(mermaid_file)
                # Clean up the mermaid content - remove any extra whitespace/newlines
                mermaid_content = mermaid_content.strip()
                
                # Create embedded Mermaid diagram
                embed_html = f'''
<div class="diagram-container">
    <h4 class="diagram-title">{title}</h4>
    <div class="mermaid">
{mermaid_content}
    </div>
</div>'''
                return embed_html
            except Exception as e:
                print(f"Warning: Could not embed diagram {filename}: {e}")
                return f'<p class="diagram-error">Diagram: {title} (Error loading {filename})</p>'
        else:
            print(f"Warning: Mermaid file not found: {mermaid_file}")
            return f'<p class="diagram-missing">Diagram: {title} (File not found: {filename})</p>'
    
    # Apply pattern
    html_content = re.sub(pattern, replace_diagram, html_content)
    return html_content

def process_embedded_mermaid(html_content):
    """Convert embedded Mermaid code blocks to Mermaid div elements"""
    import html
    
    def replace_mermaid_block(match):
        # Decode HTML entities in the Mermaid code
        mermaid_code = html.unescape(match.group(1))
        return f'<div class="mermaid">{mermaid_code}</div>'
    
    # Pattern to match <pre><code class="language-mermaid">...</code></pre>
    pattern = r'<pre><code class="language-mermaid">(.*?)</code></pre>'
    html_content = re.sub(pattern, replace_mermaid_block, html_content, flags=re.DOTALL)
    
    # Also handle the codehilite pattern if present
    pattern_codehilite = r'<pre class="codehilite"><code class="language-mermaid">(.*?)</code></pre>'
    html_content = re.sub(pattern_codehilite, replace_mermaid_block, html_content, flags=re.DOTALL)
    
    return html_content

def create_html_template(title, content, css_file="style.css"):
    """Create HTML page with embedded CSS and Mermaid support"""
    return f'''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{title}</title>
    <link rel="stylesheet" href="{css_file}">
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10.6.1/dist/mermaid.min.js"></script>
    <script>
        mermaid.initialize({{ 
            startOnLoad: true,
            theme: 'default',
            themeVariables: {{
                primaryColor: '#e1f5fe',
                primaryTextColor: '#01579b',
                primaryBorderColor: '#0277bd',
                lineColor: '#0288d1',
                secondaryColor: '#f3e5f5',
                tertiaryColor: '#e8f5e8'
            }}
        }});
    </script>
</head>
<body>
    <div class="container">
        <nav class="breadcrumb">
            <a href="index.html">Home</a> &gt; <span class="current">{title}</span>
        </nav>
        <main class="content">
{content}
        </main>
        <footer>
            <p>Generated on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
        </footer>
    </div>
</body>
</html>'''

def create_css_file(output_dir):
    """Create the CSS file for styling"""
    css_content = '''/* COBOL Documentation Styles - Professional Theme */
body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.6;
    margin: 0;
    padding: 0;
    background: #f4f6fa;
    color: #232946;
}

.container {
    max-width: 1200px;
    margin: 0 auto;
    background: #fff;
    box-shadow: 0 4px 24px rgba(44, 62, 80, 0.08);
    min-height: 100vh;
    border-radius: 14px;
}

.breadcrumb {
    background: #e9eef6;
    padding: 12px 28px;
    border-bottom: 1.5px solid #d1d9e6;
    font-size: 15px;
    border-top-left-radius: 14px;
    border-top-right-radius: 14px;
}

.breadcrumb a {
    color: #2a5d9f;
    text-decoration: none;
    font-weight: 500;
}

.breadcrumb a:hover {
    text-decoration: underline;
    color: #1b3a5d;
}

.current {
    font-weight: bold;
    color: #232946;
}

.content {
    padding: 32px 28px 28px 28px;
}

h1 {
    color: #2a5d9f;
    border-bottom: 3px solid #6ea8fe;
    padding-bottom: 12px;
    margin-bottom: 28px;
    font-size: 2.1em;
    letter-spacing: 0.5px;
}

h2 {
    color: #1b3a5d;
    margin-top: 36px;
    margin-bottom: 18px;
    border-left: 5px solid #6ea8fe;
    padding-left: 14px;
    background: #f4f8fc;
    border-radius: 5px;
}

h3 {
    color: #2a5d9f;
    margin-top: 28px;
}

h4 {
    color: #6ea8fe;
    margin-top: 22px;
}

code {
    background: #e9eef6;
    padding: 3px 8px;
    border-radius: 4px;
    font-family: 'Fira Mono', 'Courier New', Consolas, monospace;
    font-size: 92%;
    color: #1b3a5d;
}

pre {
    background: #f4f8fc;
    border: 1.5px solid #d1d9e6;
    border-radius: 8px;
    padding: 18px;
    overflow-x: auto;
    margin: 18px 0;
}

pre code {
    background: none;
    padding: 0;
    font-size: 90%;
    color: #232946;
}

.diagram-container {
    margin: 24px 0;
    border: 1.5px solid #6ea8fe;
    border-radius: 8px;
    padding: 18px;
    background: #f4f8fc;
}

.diagram-title {
    color: #2a5d9f;
    margin: 0 0 16px 0;
    font-size: 17px;
    font-weight: bold;
}

.mermaid {
    text-align: center;
    background: #fff;
    border-radius: 7px;
    padding: 12px;
    border: 1.5px solid #6ea8fe;
}

.diagram-error {
    color: #d90429;
    background: #ffe0e6;
    border: 1.5px solid #d90429;
    padding: 12px;
    border-radius: 7px;
    margin: 12px 0;
}

.diagram-missing {
    color: #b8860b;
    background: #fffbe6;
    border: 1.5px solid #b8860b;
    padding: 12px;
    border-radius: 7px;
    margin: 12px 0;
}

table {
    width: 100%;
    border-collapse: collapse;
    margin: 18px 0;
    background: #f8fafc;
    border-radius: 8px;
    overflow: hidden;
}

th, td {
    border: 1.5px solid #d1d9e6;
    padding: 14px;
    text-align: left;
}

th {
    background: #e9eef6;
    font-weight: bold;
    color: #2a5d9f;
}

ul, ol {
    margin: 12px 0;
    padding-left: 34px;
}

li {
    margin: 7px 0;
}

blockquote {
    border-left: 5px solid #6ea8fe;
    margin: 18px 0;
    padding: 12px 18px;
    background: #f4f8fc;
    color: #232946;
    border-radius: 5px;
}

a {
    color: #2a5d9f;
    text-decoration: none;
    font-weight: 500;
}

a:hover {
    text-decoration: underline;
    color: #1b3a5d;
}

footer {
    text-align: center;
    padding: 24px;
    background: #e9eef6;
    border-top: 1.5px solid #d1d9e6;
    color: #232946;
    font-size: 13px;
    border-bottom-left-radius: 14px;
    border-bottom-right-radius: 14px;
}

/* Program card styling for index */
.program-card {
    background: #fff;
    border: 1.5px solid #6ea8fe;
    border-radius: 8px;
    padding: 24px;
    margin: 18px 0;
    box-shadow: 0 2px 8px rgba(44, 62, 80, 0.10);
    transition: transform 0.2s, box-shadow 0.2s;
}

.program-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 6px 16px rgba(44, 62, 80, 0.18);
}

.program-title {
    color: #2a5d9f;
    margin: 0 0 12px 0;
    font-size: 20px;
}

.program-description {
    color: #232946;
    margin: 0 0 18px 0;
    line-height: 1.6;
}

.program-links a {
    display: inline-block;
    margin-right: 18px;
    color: #1b3a5d;
    font-weight: 600;
}
'''
    
    css_file = os.path.join(output_dir, "style.css")
    write_file(css_file, css_content)
    print(f"Created CSS file: {css_file}")

def process_markdown_files(docs_dir, diagrams_dir, output_dir):
    """Process all markdown files and convert to HTML"""
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Create CSS file
    create_css_file(output_dir)
    
    # Process each markdown file
    for md_file in Path(docs_dir).glob("*.md"):
        print(f"Processing: {md_file.name}")
        
        # Read markdown content
        md_content = read_file(md_file)
        
        # Convert .md links to .html links
        md_content = convert_md_links_to_html(md_content)
        
        # Convert markdown to HTML
        html_content = markdown.markdown(md_content, extensions=['tables', 'fenced_code', 'toc'])
        
        # Add heading anchors for table of contents navigation
        html_content = add_heading_anchors(html_content)
        
        # Process embedded Mermaid code blocks
        html_content = process_embedded_mermaid(html_content)
        
        # Embed Mermaid diagrams
        html_content = embed_mermaid_diagrams(html_content, diagrams_dir)
        
        # Extract title from first heading
        title_match = re.search(r'<h1[^>]*>(.*?)</h1>', html_content)
        title = title_match.group(1) if title_match else md_file.stem
        
        # Create full HTML page
        full_html = create_html_template(title, html_content)
        
        # Write HTML file
        output_file = os.path.join(output_dir, f"{md_file.stem}.html")
        write_file(output_file, full_html)
        print(f"Created: {output_file}")

def main():
    """Main function"""
    # Set up directories
    base_dir = Path(__file__).parent
    docs_dir = base_dir / "Documents"
    diagrams_dir = docs_dir / "Diagrams"  # Diagrams are inside Documents directory
    output_dir = docs_dir  # Output HTML files directly to Documents directory
    
    print("COBOL Documentation Static HTML Generator")
    print("=" * 50)
    print(f"Source directory: {docs_dir}")
    print(f"Diagrams directory: {diagrams_dir}")
    print(f"Output directory: {output_dir}")
    print()
    
    if not docs_dir.exists():
        print(f"Error: Documents directory not found: {docs_dir}")
        sys.exit(1)
    
    if not diagrams_dir.exists():
        print(f"Warning: Diagrams directory not found: {diagrams_dir}")
        print("Continuing without diagram embedding...")
    
    # Generate HTML files from markdown once
    process_markdown_files(docs_dir, diagrams_dir, output_dir)


# Run main if this script is executed directly
if __name__ == "__main__":
    main()
