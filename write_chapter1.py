#!/usr/bin/env python3
"""
Write Chapter 1: Introduction based on research data
"""

import json

def main():
    # Load research data
    with open('chapter1_introduction_research.json', 'r', encoding='utf-8') as f:
        research = json.load(f)
    
    # Build the chapter content
    content = []
    content.append("# Chapter 1: Introduction\n")
    content.append("## Overview\n")
    content.append("This chapter introduces the comprehensive guide to modernizing the OTIS (OTOS) COBOL loan servicing system to a modern C# API-driven architecture. It establishes the purpose, scope, and target audience for this transformation journey.\n")
    
    # Purpose of the Book (Q1-10)
    content.append("## 1.1 Purpose of the Book\n")
    for i in range(0, 10):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # Overview of OTIS/OTOS (Q11-25)
    content.append("## 1.2 Overview of OTIS/OTOS\n")
    for i in range(10, 25):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # Why Modernize? (Q26-40)
    content.append("## 1.3 Why Modernize?\n")
    for i in range(25, 40):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # Target Audience (Q41-50)
    content.append("## 1.4 Target Audience\n")
    for i in range(40, 50):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # Write to file
    output_file = "Chapter1_Introduction.md"
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(''.join(content))
    
    print(f"✅ Chapter 1 written to {output_file}")
    print(f"   Total sections: 4")
    print(f"   Total Q&A pairs: 50")

if __name__ == "__main__":
    main()
