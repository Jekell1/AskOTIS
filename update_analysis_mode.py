"""
Quick fix to remove pattern matching and use only ChatGPT analysis
"""

# Read the file
with open('simple_cobol_rag.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace the analysis mode text
old_text = '**Comprehensive Analysis Mode**: System automatically analyzes ALL relevant data found for your questions - no artificial limits!'
new_text = '**ChatGPT Analysis Mode**: All queries are processed by GPT-5 for comprehensive, intelligent analysis of your COBOL codebase!'

content = content.replace(old_text, new_text)

# Write back
with open('simple_cobol_rag.py', 'w', encoding='utf-8') as f:
    f.write(content)

print("✅ Updated analysis mode description to reflect ChatGPT-only processing")
