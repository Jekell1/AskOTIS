#!/usr/bin/env python3
"""
Write Chapter 2: Business Domain Analysis based on research data
"""

import json

def main():
    # Load research data
    with open('chapter2_business_domain_research.json', 'r', encoding='utf-8') as f:
        research = json.load(f)
    
    # Build the chapter content
    content = []
    content.append("# Chapter 2: Business Domain Analysis\n")
    content.append("## Overview\n")
    content.append("This chapter provides a comprehensive analysis of the business domain served by OTIS, including loan servicing operations, financial processes, key business workflows, and the various user roles that interact with the system daily.\n")
    
    # Loan Servicing and Financial Operations (Q1-20)
    content.append("## 2.1 Loan Servicing and Financial Operations in OTIS\n")
    for i in range(0, 20):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # Key Business Processes Supported (Q21-35)
    content.append("## 2.2 Key Business Processes Supported\n")
    for i in range(20, 35):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # User Roles and Workflows (Q36-50)
    content.append("## 2.3 User Roles and Workflows\n")
    for i in range(35, 50):
        q = research[i]
        content.append(f"### {q['question']}\n\n")
        content.append(f"{q['answer']}\n\n")
    
    # Write to file
    output_file = "Chapter2_Business_Domain.md"
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(''.join(content))
    
    print(f"✅ Chapter 2 written to {output_file}")
    print(f"   Total sections: 3")
    print(f"   Total Q&A pairs: 50")

if __name__ == "__main__":
    main()
