#!/usr/bin/env python3
"""
Update the HTML generator to add section listings and floating TOC button
"""

import re

# Read the current file
with open('create_complete_book_html.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Add the floating button CSS before the closing of style section
css_addition = '''
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
'''

# Find where to insert the CSS (before the last media query or before </style>)
content = content.replace('        /* Responsive */', css_addition + '        /* Responsive */')

# Add JavaScript and floating button before </body>
js_addition = '''
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

content = content.replace('    </div>\n</body>\n</html>', js_addition)

# Add ID to TOC page for navigation
content = content.replace('<div class="toc-page">', '<div class="toc-page" id="toc">')

# Write the updated file
with open('create_complete_book_html.py', 'w', encoding='utf-8') as f:
    f.write(content)

print("✅ Updated create_complete_book_html.py with:")
print("   - Floating 'Back to TOC' button")
print("   - Smooth scrolling for navigation")
print("   - Enhanced CSS for sections")
print("\nNote: Section extraction needs to be added manually or run the generator")
print("      to see the improvements!")
