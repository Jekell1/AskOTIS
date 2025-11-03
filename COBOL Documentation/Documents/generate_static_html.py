#!/usr/bin/env python3
"""
COBOL Documentation Static HTML Generator
Converts Markdown files to HTML and creates a static documentation viewer.
"""

import os
import re
import json
from pathlib import Path
import markdown
from markdown.extensions import codehilite, tables, toc

def read_file(file_path):
    """Read file content."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            return f.read()
    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        return ""

def write_file(file_path, content):
    """Write content to file."""
    try:
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        print(f"Generated: {file_path}")
    except Exception as e:
        print(f"Error writing {file_path}: {e}")

def read_mermaid_diagram(file_path):
    """Read Mermaid diagram content."""
    content = read_file(file_path)
    if content:
        # Clean up the content and ensure it's properly formatted
        content = content.strip()
        if not content.startswith('graph') and not content.startswith('gantt') and not content.startswith('sequenceDiagram') and not content.startswith('flowchart'):
            # Try to detect diagram type and add if missing
            if 'graph' in content.lower() and 'flowchart' not in content.lower():
                content = 'graph TD\n' + content
    return content

def process_external_diagram_references(markdown_content, diagrams_dir):
    """Process external diagram references and embed them inline."""
    import re
    
    # Pattern to match markdown links to .mmd files: [Text](path/file.mmd)
    pattern = r'\[([^\]]+)\]\(([^)]+\.mmd)\)'
    
    def replace_diagram_link(match):
        link_text = match.group(1)
        diagram_path = match.group(2)
        
        # Handle relative paths - remove 'Diagrams/' prefix if present
        if diagram_path.startswith('Diagrams/'):
            diagram_file = diagram_path[9:]  # Remove 'Diagrams/' prefix
        else:
            diagram_file = diagram_path
        
        # Full path to the diagram file
        full_diagram_path = os.path.join(diagrams_dir, diagram_file)
        
        print(f"  🔗 Processing external diagram reference: {diagram_path}")
        
        if os.path.exists(full_diagram_path):
            diagram_content = read_mermaid_diagram(full_diagram_path)
            if diagram_content:
                # Replace the link with the embedded diagram
                return f"""### {link_text}

```mermaid
{diagram_content}
```"""
            else:
                print(f"    ⚠️ Warning: Could not read diagram content from {full_diagram_path}")
                return f"### {link_text}\n\n*Diagram not available: {diagram_path}*"
        else:
            print(f"    ⚠️ Warning: Diagram file not found: {full_diagram_path}")
            return f"### {link_text}\n\n*Diagram file not found: {diagram_path}*"
    
    # Replace all external diagram references
    processed_content = re.sub(pattern, replace_diagram_link, markdown_content)
    
    return processed_content

def convert_markdown_to_html(markdown_content, title="Documentation", diagrams_dir=None):
    """Convert Markdown content to HTML with proper styling."""
    
    # Process external diagram references if diagrams directory is provided
    if diagrams_dir:
        markdown_content = process_external_diagram_references(markdown_content, diagrams_dir)
    
    # Replace ```mermaid blocks with Mermaid divs BEFORE markdown conversion
    def replace_mermaid(match):
        diagram_code = match.group(1)
        import html
        diagram_code = html.unescape(diagram_code)
        return f'<div class="mermaid">{diagram_code}</div>'

    # Replace all ```mermaid blocks in the markdown source
    markdown_content = re.sub(r'```mermaid\n(.*?)\n```', replace_mermaid, markdown_content, flags=re.DOTALL)

    # Configure markdown with extensions
    md = markdown.Markdown(extensions=[
        'codehilite',
        'tables',
        'toc',
        'fenced_code'
    ])

    # Convert markdown to HTML
    html_content = md.convert(markdown_content)

    # (Optional) Clean up any <pre><code class="language-mermaid"> blocks that may remain (legacy)
    html_content = re.sub(r'<pre><code class="language-mermaid">(.*?)</code></pre>', replace_mermaid, html_content, flags=re.DOTALL)
    html_content = re.sub(r'<pre class="codehilite"><code class="language-mermaid">(.*?)</code></pre>', replace_mermaid, html_content, flags=re.DOTALL)

    return html_content

def create_document_html(title, content, breadcrumb="", program_type=""):
    """Create complete HTML document."""
    
    icon_map = {
        'APIPAY': '💳',
        'LONPFB': '🏦',
        'LONPFC': '💰',
        'LONPF2': '🌐',
        'LONPF7': '🚗',
        'LONPF9': '🛡️'
    }
    
    icon = icon_map.get(program_type, '📄')
    
    html_template = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{title} - COBOL Documentation</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10.6.1/dist/mermaid.min.js"></script>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}

        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif;
            line-height: 1.6;
            color: #333;
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            min-height: 100vh;
        }}

        .container {{
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
        }}

        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 15px;
            margin-bottom: 30px;
            text-align: center;
            box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
        }}

        .header h1 {{
            margin: 0;
            font-size: 2.5rem;
            font-weight: 600;
        }}

        .header .meta {{
            opacity: 0.9;
            margin-top: 10px;
            font-size: 1.1rem;
        }}

        .nav-breadcrumb {{
            background: rgba(255, 255, 255, 0.95);
            backdrop-filter: blur(10px);
            padding: 15px 25px;
            border-radius: 10px;
            margin-bottom: 30px;
            font-size: 0.95rem;
            box-shadow: 0 4px 16px rgba(0, 0, 0, 0.1);
        }}

        .nav-breadcrumb a {{
            color: #667eea;
            text-decoration: none;
            font-weight: 500;
            transition: color 0.2s ease;
        }}

        .nav-breadcrumb a:hover {{
            text-decoration: underline;
            color: #5a6fd8;
        }}

        .content {{
            background: rgba(255, 255, 255, 0.95);
            backdrop-filter: blur(10px);
            padding: 40px;
            border-radius: 15px;
            box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
            margin-bottom: 30px;
        }}

        .content h1 {{
            color: #2c3e50;
            font-size: 2.2rem;
            margin-bottom: 1rem;
            border-bottom: 3px solid #667eea;
            padding-bottom: 10px;
        }}

        .content h2 {{
            color: #34495e;
            font-size: 1.8rem;
            margin: 2rem 0 1rem 0;
            border-bottom: 2px solid #ecf0f1;
            padding-bottom: 8px;
        }}

        .content h3 {{
            color: #667eea;
            font-size: 1.4rem;
            margin: 1.5rem 0 0.8rem 0;
        }}

        .content h4 {{
            color: #7f8c8d;
            font-size: 1.2rem;
            margin: 1.2rem 0 0.6rem 0;
        }}

        .content p {{
            margin-bottom: 1.2rem;
            text-align: justify;
        }}

        .content ul, .content ol {{
            margin: 1rem 0 1rem 2rem;
        }}

        .content li {{
            margin-bottom: 0.5rem;
        }}

        .content table {{
            width: 100%;
            border-collapse: collapse;
            margin: 1.5rem 0;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
            border-radius: 8px;
            overflow: hidden;
        }}

        .content th, .content td {{
            border: 1px solid #e0e0e0;
            padding: 12px 15px;
            text-align: left;
        }}

        .content th {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 0.9rem;
            letter-spacing: 0.5px;
        }}

        .content tr:nth-child(even) {{
            background-color: #f8f9fa;
        }}

        .content tr:hover {{
            background-color: #e8f4f8;
            transition: background-color 0.2s ease;
        }}

        .content code {{
            background: #f4f4f4;
            border: 1px solid #ddd;
            border-radius: 4px;
            padding: 2px 6px;
            font-family: 'Courier New', monospace;
            font-size: 0.9em;
            color: #d63384;
        }}

        .content pre {{
            background: #2d3748;
            color: #e2e8f0;
            padding: 20px;
            border-radius: 10px;
            overflow-x: auto;
            margin: 1.5rem 0;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }}

        .content pre code {{
            background: none;
            border: none;
            color: inherit;
            padding: 0;
        }}

        .content blockquote {{
            border-left: 4px solid #667eea;
            background: #f8f9fa;
            padding: 15px 20px;
            margin: 1.5rem 0;
            border-radius: 0 8px 8px 0;
            font-style: italic;
        }}

        .mermaid {{
            background: white;
            border: 1px solid #e0e0e0;
            border-radius: 10px;
            padding: 20px;
            margin: 20px 0;
            text-align: center;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.05);
        }}

        .toc {{
            background: #f8f9fa;
            border: 1px solid #e9ecef;
            border-radius: 10px;
            padding: 20px;
            margin: 20px 0;
        }}

        .toc h2 {{
            color: #495057;
            font-size: 1.3rem;
            margin-bottom: 15px;
            border: none;
            padding: 0;
        }}

        .toc ul {{
            list-style: none;
            margin: 0;
            padding: 0;
        }}

        .toc li {{
            margin: 5px 0;
        }}

        .toc a {{
            color: #667eea;
            text-decoration: none;
            padding: 5px 10px;
            display: block;
            border-radius: 5px;
            transition: background-color 0.2s ease;
        }}

        .toc a:hover {{
            background-color: #e9ecef;
            text-decoration: underline;
        }}

        .back-to-top {{
            position: fixed;
            bottom: 30px;
            right: 30px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border: none;
            border-radius: 50%;
            width: 50px;
            height: 50px;
            font-size: 20px;
            cursor: pointer;
            box-shadow: 0 4px 16px rgba(0, 0, 0, 0.2);
            transition: transform 0.2s ease;
            display: none;
        }}

        .back-to-top:hover {{
            transform: translateY(-3px);
        }}

        .footer {{
            text-align: center;
            padding: 30px;
            color: #666;
            font-size: 0.9rem;
            background: rgba(255, 255, 255, 0.7);
            backdrop-filter: blur(10px);
            border-radius: 10px;
            margin-top: 20px;
        }}

        @media (max-width: 768px) {{
            .container {{
                padding: 10px;
            }}
            
            .header h1 {{
                font-size: 2rem;
            }}
            
            .content {{
                padding: 20px;
            }}
            
            .content h1 {{
                font-size: 1.8rem;
            }}
            
            .content h2 {{
                font-size: 1.5rem;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>{icon} {title}</h1>
            <div class="meta">COBOL Payment Processing Documentation</div>
        </div>

        {f'<div class="nav-breadcrumb">{breadcrumb}</div>' if breadcrumb else ''}

        <div class="content">
            {content}
        </div>

        <div class="footer">
            <p>Generated: July 22, 2025 | COBOL Documentation System</p>
            <p>Static HTML version - No server required</p>
        </div>
    </div>

    <button class="back-to-top" onclick="scrollToTop()" id="backToTop">↑</button>

    <script>
        // Initialize Mermaid
        mermaid.initialize({{ 
            startOnLoad: true,
            theme: 'default',
            themeVariables: {{
                primaryColor: '#667eea',
                primaryTextColor: '#2c3e50',
                primaryBorderColor: '#e0e0e0',
                lineColor: '#7f8c8d',
                background: '#ffffff'
            }}
        }});

        // Back to top functionality
        window.onscroll = function() {{
            const button = document.getElementById('backToTop');
            if (document.body.scrollTop > 300 || document.documentElement.scrollTop > 300) {{
                button.style.display = 'block';
            }} else {{
                button.style.display = 'none';
            }}
        }};

        function scrollToTop() {{
            document.body.scrollTop = 0;
            document.documentElement.scrollTop = 0;
        }}
    </script>
</body>
</html>"""
    
    return html_template

def create_index_html(programs, diagrams):
    """Create the main index.html file."""
    
    program_cards = []
    for program_id, info in programs.items():
        icon = info['icon']
        title = info['title']
        description = info['description']
        tag = info['tag']
        filename = info['filename']
        
        card_html = f"""
        <div class="nav-card" onclick="window.location.href='{filename}'">
            <h3>{icon} {title}</h3>
            <p>{description}</p>
            <span class="tag">{tag}</span>
        </div>"""
        program_cards.append(card_html)
    
    diagram_cards = []
    for diagram_file, title in diagrams.items():
        card_html = f"""
        <div class="nav-card" onclick="window.location.href='diagrams.html#{diagram_file}'">
            <h3>📊 {title}</h3>
            <p>Interactive system flow diagram</p>
            <span class="tag">Mermaid</span>
        </div>"""
        diagram_cards.append(card_html)
    
    index_content = f"""
    <div class="welcome">
        <h1>Welcome to the COBOL Documentation System</h1>
        <p>Comprehensive static HTML documentation for COBOL payment processing systems. Browse detailed program documentation and interactive system diagrams.</p>
        
        <h2>📋 Program Documentation</h2>
        <div class="program-grid">
            {''.join(program_cards)}
        </div>
        
        <h2>📊 System Diagrams</h2>
        <div class="program-grid">
            {''.join(diagram_cards)}
        </div>
    </div>
    
    <style>
        .welcome {{
            text-align: center;
            padding: 20px;
        }}
        
        .welcome h1 {{
            color: #2c3e50;
            margin-bottom: 20px;
            font-size: 2.5rem;
        }}
        
        .welcome h2 {{
            color: #34495e;
            margin: 3rem 0 2rem 0;
            font-size: 2rem;
            border-bottom: 3px solid #667eea;
            padding-bottom: 10px;
            display: inline-block;
        }}
        
        .welcome p {{
            color: #7f8c8d;
            font-size: 1.2rem;
            max-width: 800px;
            margin: 0 auto 3rem auto;
        }}
        
        .program-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
            gap: 25px;
            margin: 30px 0;
            text-align: left;
        }}
        
        .nav-card {{
            background: linear-gradient(135deg, rgba(255, 255, 255, 0.9) 0%, rgba(248, 250, 252, 0.9) 100%);
            backdrop-filter: blur(10px);
            border: 1px solid rgba(226, 232, 240, 0.5);
            border-radius: 15px;
            padding: 25px;
            cursor: pointer;
            transition: all 0.3s ease;
            position: relative;
            overflow: hidden;
            box-shadow: 0 4px 16px rgba(0, 0, 0, 0.1);
        }}
        
        .nav-card::before {{
            content: '';
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            height: 4px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            transform: scaleX(0);
            transition: transform 0.3s ease;
        }}
        
        .nav-card:hover::before {{
            transform: scaleX(1);
        }}
        
        .nav-card:hover {{
            transform: translateY(-8px);
            box-shadow: 0 12px 32px rgba(0, 0, 0, 0.15);
            border-color: rgba(59, 130, 246, 0.3);
        }}
        
        .nav-card h3 {{
            color: #2c3e50;
            margin-bottom: 15px;
            font-size: 1.4rem;
            font-weight: 600;
        }}
        
        .nav-card p {{
            color: #7f8c8d;
            font-size: 1rem;
            margin-bottom: 20px;
            line-height: 1.6;
        }}
        
        .nav-card .tag {{
            background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%);
            color: #1d4ed8;
            padding: 6px 15px;
            border-radius: 20px;
            font-size: 0.85rem;
            font-weight: 600;
            display: inline-block;
        }}
        
        @media (max-width: 768px) {{
            .program-grid {{
                grid-template-columns: 1fr;
            }}
            
            .welcome h1 {{
                font-size: 2rem;
            }}
            
            .welcome h2 {{
                font-size: 1.5rem;
            }}
        }}
    </style>"""
    
    html = create_document_html(
        "COBOL Documentation System", 
        index_content,
        "",
        ""
    )
    
    return html

def create_diagrams_html(diagrams, diagrams_dir):
    """Create the diagrams.html file with all Mermaid diagrams."""
    
    diagram_sections = []
    
    for diagram_file, title in diagrams.items():
        diagram_path = os.path.join(diagrams_dir, diagram_file)
        diagram_content = read_mermaid_diagram(diagram_path)
        
        if diagram_content:
            section_html = f"""
            <div class="diagram-section" id="{diagram_file}">
                <h2>📊 {title}</h2>
                <div class="diagram-description">
                    <p>Interactive system flow diagram showing the {title.lower()} workflow and data relationships.</p>
                </div>
                <div class="diagram-container">
                    <div class="mermaid">{diagram_content}</div>
                </div>
            </div>"""
            diagram_sections.append(section_html)
    
    diagrams_content = f"""
    <h1>System Flow Diagrams</h1>
    <p>Interactive Mermaid diagrams showing system architecture, data flows, and process workflows for the COBOL payment processing system.</p>
    
    <div class="toc">
        <h2>Available Diagrams</h2>
        <ul>
            {''.join([f'<li><a href="#{diagram_file}">📊 {title}</a></li>' for diagram_file, title in diagrams.items()])}
        </ul>
    </div>
    
    {''.join(diagram_sections)}
    
    <style>
        .diagram-section {{
            margin-bottom: 60px;
            padding-bottom: 40px;
            border-bottom: 1px solid #e9ecef;
        }}
        
        .diagram-section:last-child {{
            border-bottom: none;
        }}
        
        .diagram-description {{
            background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
            border-left: 4px solid #667eea;
            padding: 20px;
            margin-bottom: 25px;
            border-radius: 0 10px 10px 0;
        }}
        
        .diagram-container {{
            background: white;
            border: 1px solid #e9ecef;
            border-radius: 15px;
            padding: 30px;
            box-shadow: 0 4px 16px rgba(0, 0, 0, 0.05);
        }}
    </style>"""
    
    breadcrumb = '<a href="index.html">🏠 Home</a> > 📊 System Diagrams'
    
    html = create_document_html(
        "System Diagrams",
        diagrams_content,
        breadcrumb,
        ""
    )
    
    return html

def scan_for_files(base_dir):
    """Scan for all available Markdown and Mermaid files."""
    docs_dir = base_dir
    diagrams_dir = os.path.join(docs_dir, 'Diagrams')
    
    # Scan for Markdown documentation files
    md_files = []
    if os.path.exists(docs_dir):
        for file in os.listdir(docs_dir):
            if file.endswith('.md') and 'Documentation' in file:
                md_files.append(file)
    
    # Scan for Mermaid diagram files
    mmd_files = []
    if os.path.exists(diagrams_dir):
        for file in os.listdir(diagrams_dir):
            if file.endswith('.mmd'):
                mmd_files.append(file)
    
    return sorted(md_files), sorted(mmd_files)

def extract_program_info(md_file):
    """Extract program information from markdown filename and content."""
    # Extract program name from filename
    program_name = md_file.replace('_Documentation.md', '').replace('.md', '')
    
    # Define program metadata
    program_metadata = {
        'APIPAY': {
            'title': 'APIPAY - Payment Processor',
            'description': 'Main payment processing system handling various transaction types',
            'tag': 'Core System',
            'icon': '💳'
        },
        'LONPFB': {
            'title': 'LONPFB - Bill Pay System',
            'description': 'Bill payment processing and transaction handling',
            'tag': 'Bill Processing',
            'icon': '🏦'
        },
        'LONPFC': {
            'title': 'LONPFC - Check Processor',
            'description': 'Check transaction processing and validation',
            'tag': 'Check Processing',
            'icon': '💰'
        },
        'LONPF2': {
            'title': 'LONPF2 - Wire Transfer Processor',
            'description': 'Wire transfer and electronic payment processing',
            'tag': 'Electronic Transfer',
            'icon': '🌐'
        },
        'LONPF7': {
            'title': 'LONPF7 - Repossession Processor',
            'description': 'Asset repossession and recovery transaction processing',
            'tag': 'Asset Recovery',
            'icon': '🚗'
        },
        'LONPF9': {
            'title': 'LONPF9 - Insurance Processor',
            'description': 'Insurance payment and disbursement processing',
            'tag': 'Insurance',
            'icon': '🛡️'
        }
    }
    
    # Return metadata or default for unknown programs
    if program_name in program_metadata:
        info = program_metadata[program_name].copy()
        info['file'] = md_file
        info['filename'] = md_file.replace('.md', '.html')
        return program_name, info
    else:
        # Create default metadata for new/unknown programs
        return program_name, {
            'title': f'{program_name} - Documentation',
            'description': f'Documentation for {program_name} program',
            'file': md_file,
            'filename': md_file.replace('.md', '.html'),
            'tag': 'Documentation',
            'icon': '📄'
        }

def extract_diagram_title(mmd_file):
    """Extract diagram title from filename."""
    # Remove .mmd extension and convert underscores to spaces
    title = mmd_file.replace('.mmd', '').replace('_', ' ')
    return title

def main():
    """Main function to generate static HTML documentation."""
    
    # Set up paths
    base_dir = os.path.dirname(os.path.abspath(__file__))
    docs_dir = base_dir
    diagrams_dir = os.path.join(docs_dir, 'Diagrams')
    
    print("COBOL Documentation Static HTML Generator")
    print("=" * 50)
    print(f"Base directory: {base_dir}")
    print(f"Docs directory: {docs_dir}")
    print(f"Diagrams directory: {diagrams_dir}")
    print()
    
    # Scan for available files
    print("🔍 Scanning for documentation files...")
    md_files, mmd_files = scan_for_files(base_dir)
    
    print(f"Found {len(md_files)} Markdown documentation files:")
    for md_file in md_files:
        print(f"  📄 {md_file}")
    
    print(f"\nFound {len(mmd_files)} Mermaid diagram files:")
    for mmd_file in mmd_files:
        print(f"  📊 {mmd_file}")
    print()
    
    
    # Build programs dictionary from discovered files
    programs = {}
    for md_file in md_files:
        program_id, program_info = extract_program_info(md_file)
        programs[program_id] = program_info
    
    # Build diagrams dictionary from discovered files
    diagrams = {}
    for mmd_file in mmd_files:
        title = extract_diagram_title(mmd_file)
        diagrams[mmd_file] = title
    
    if not programs and not diagrams:
        print("⚠️ No documentation or diagram files found!")
        print("Please ensure you have:")
        print("  - Markdown files ending with '_Documentation.md' in the Documents folder")
        print("  - Mermaid files ending with '.mmd' in the Documents/Diagrams folder")
        return
    
    print(f"📚 Processing {len(programs)} program(s) and {len(diagrams)} diagram(s)...")
    print()
    
    # Generate HTML files for each program
    generated_files = []
    for program_id, info in programs.items():
        md_file = os.path.join(docs_dir, info['file'])
        html_file = os.path.join(docs_dir, info['filename'])
        
        if os.path.exists(md_file):
            print(f"Converting {info['file']}...")
            
            # Read markdown content
            markdown_content = read_file(md_file)
            
            if markdown_content:
                # Convert to HTML with diagram processing
                html_content = convert_markdown_to_html(
                    markdown_content, 
                    info['title'],
                    diagrams_dir  # Pass diagrams directory for external reference processing
                )
                
                # Create breadcrumb
                breadcrumb = f'<a href="index.html">🏠 Home</a> > {info["icon"]} {info["title"]}'
                
                # Generate complete HTML document
                full_html = create_document_html(
                    info['title'],
                    html_content,
                    breadcrumb,
                    program_id
                )
                
                # Write HTML file
                write_file(html_file, full_html)
                generated_files.append(info['filename'])
            else:
                print(f"Warning: Could not read {info['file']}")
        else:
            print(f"Warning: {info['file']} not found")
    
    # Generate index.html
    print("Generating index.html...")
    index_html = create_index_html(programs, diagrams)
    write_file(os.path.join(docs_dir, 'index.html'), index_html)
    generated_files.append('index.html')
    
    # Generate diagrams.html (only if diagrams exist)
    if diagrams:
        print("Generating diagrams.html...")
        diagrams_html = create_diagrams_html(diagrams, diagrams_dir)
        write_file(os.path.join(docs_dir, 'diagrams.html'), diagrams_html)
        generated_files.append('diagrams.html')
    
    print()
    print("✅ Static HTML documentation generated successfully!")
    print(f"📂 Generated {len(generated_files)} file(s):")
    for file in generated_files:
        print(f"   📄 {file}")
    print()
    print(f"🌐 Main page: {os.path.join(docs_dir, 'index.html')}")
    print("💡 You can now open index.html in any web browser.")
    print("🔗 No server required - all content is static HTML.")
    print()
    print("🔄 To update: Simply rerun this script after adding/modifying files!")

if __name__ == "__main__":
    # Check if markdown module is available
    try:
        import markdown
    except ImportError:
        print("Error: Python markdown module not found.")
        print("Please install it with: pip install markdown")
        exit(1)
    
    main()
