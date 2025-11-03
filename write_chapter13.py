#!/usr/bin/env python3
"""
Write Chapter 13: Data Migration Strategies
Generates a comprehensive chapter from research Q&A data.
"""

import json

def main():
    # Load research data
    with open("chapter13_data_migration_research.json", "r", encoding="utf-8") as f:
        qa_data = json.load(f)
    
    # Build chapter content
    chapter = []
    
    # Chapter title and introduction
    chapter.append("# Chapter 13: Data Migration Strategies\n")
    chapter.append("## Introduction\n")
    chapter.append("""
Data migration is one of the most critical—and risky—phases of any legacy modernization project. When converting the OTIS COBOL loan servicing system to modern C# APIs, moving decades of transactional data, customer records, and business-critical information from legacy file structures to relational databases requires meticulous planning, rigorous testing, and robust execution strategies.

This chapter explores the three essential phases of data migration: **extracting data from legacy files**, **cleansing and transforming that data**, and **loading it into new systems**. Each phase presents unique challenges, from handling COBOL-specific data types (COMP-3, REDEFINES) to ensuring referential integrity in normalized relational schemas. We'll examine proven tools, techniques, and best practices that minimize risk, prevent data loss, and ensure business continuity throughout the migration process.

Successful data migration is not just about moving bytes from one system to another—it's about preserving the business value encoded in decades of operational data while preparing that data for a modern, API-driven future.
""")
    
    # Section 13.1: Extracting Data from Legacy Files
    chapter.append("\n## Section 13.1: Extracting Data from Legacy Files\n")
    chapter.append("### 13.1.1: Understanding Legacy File Structures\n")
    for i in range(0, 6):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 13.1.2: Extraction Tools and Techniques\n")
    for i in range(6, 11):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 13.1.3: Validation and Documentation\n")
    for i in range(11, 17):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Section 13.2: Data Cleansing and Transformation
    chapter.append("\n## Section 13.2: Data Cleansing and Transformation\n")
    chapter.append("### 13.2.1: Identifying and Resolving Data Quality Issues\n")
    for i in range(17, 23):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 13.2.2: Transformation Rules and ETL Tools\n")
    for i in range(23, 29):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 13.2.3: Testing and Validating Transformations\n")
    for i in range(29, 34):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Section 13.3: Loading Data into New Systems
    chapter.append("\n## Section 13.3: Loading Data into New Systems\n")
    chapter.append("### 13.3.1: Load Performance and Optimization\n")
    for i in range(34, 40):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 13.3.2: Validation and Reconciliation\n")
    for i in range(40, 45):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 13.3.3: Cutover and Incremental Loading\n")
    for i in range(45, 50):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Conclusion
    chapter.append("\n## Conclusion\n")
    chapter.append("""
Data migration is the bridge between legacy systems and modern architectures. The strategies outlined in this chapter—from careful extraction of COBOL file structures, through rigorous cleansing and transformation, to optimized loading into relational databases—provide a roadmap for preserving decades of business-critical data while preparing it for a modern, API-driven future.

Successful data migration requires more than technical skill; it demands deep understanding of both source and target systems, meticulous planning, comprehensive testing, and robust rollback strategies. By following the patterns and practices described here, teams can minimize migration risks, ensure data integrity, and deliver a modernized system that maintains business continuity while enabling future innovation.

The techniques explored in this chapter form the foundation for the testing and deployment strategies we'll examine in the following chapters, where we'll validate that the migrated data produces correct business outcomes in the new C# API environment.
""")
    
    # Write to file
    output = "".join(chapter)
    with open("Chapter13_Data_Migration.md", "w", encoding="utf-8") as f:
        f.write(output)
    
    print("✅ Chapter 13 written to Chapter13_Data_Migration.md")
    print(f"   Total sections: 6 (including Introduction and Conclusion)")
    print(f"   Q&A pairs included: {len(qa_data)}")

if __name__ == "__main__":
    main()
