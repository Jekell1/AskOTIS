"""
Generate Chapter 8: Automated and Manual Code Analysis
Reads research from chapter8_code_analysis_research.json and creates comprehensive markdown.
"""

import json

def load_research():
    """Load research Q&A from JSON file."""
    with open("chapter8_code_analysis_research.json", "r", encoding="utf-8") as f:
        return json.load(f)

def group_questions(data):
    """Group questions by section."""
    return {
        "parsing_cobol": data[0:15],           # Q1-15
        "extracting_logic": data[15:25],       # Q16-25
        "conversion_tools": data[25:40],       # Q26-40
        "copybooks_routines": data[40:50]      # Q41-50
    }

def write_chapter(data):
    """Generate complete Chapter 8 markdown."""
    groups = group_questions(data)
    
    md = []
    md.append("# Chapter 8: Automated and Manual Code Analysis\n")
    md.append("## Introduction\n")
    md.append("Converting the OTIS/OTOS COBOL codebase to C# requires a systematic approach to code analysis. ")
    md.append("This chapter explores both automated and manual techniques for parsing COBOL code, extracting ")
    md.append("business logic and data structures, leveraging conversion tools, and handling the special challenges ")
    md.append("of copybooks and common routines. Understanding how to analyze legacy code is foundational to a ")
    md.append("successful modernization effort.\n\n")
    
    # Section 8.1: Parsing COBOL Code
    md.append("## Section 8.1: Parsing COBOL Code\n")
    md.append("Parsing COBOL code is the first step in understanding the structure and semantics of legacy programs. ")
    md.append("This section covers techniques for parsing COBOL divisions, extracting metadata, interpreting data ")
    md.append("definitions, and handling the procedural logic that must be converted to C#. Effective parsing ")
    md.append("strategies are essential for accurate code conversion.\n\n")
    
    # Subsections for Parsing COBOL Code
    parsing_subsections = [
        (0, 3, "Challenges and Strategies in COBOL Parsing"),
        (3, 7, "Parsing COBOL Data Divisions and Structures"),
        (7, 11, "Parsing COBOL Procedure Division Logic"),
        (11, 15, "Handling COBOL Dependencies and SQL Statements")
    ]
    
    for start, end, title in parsing_subsections:
        md.append(f"### {title}\n")
        for qa in groups["parsing_cobol"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Section 8.2: Extracting Business Logic and Data Structures
    md.append("## Section 8.2: Extracting Business Logic and Data Structures\n")
    md.append("Beyond parsing, extracting meaningful business logic and data structures from COBOL code is critical ")
    md.append("for designing modern C# APIs and services. This section examines techniques for identifying reusable ")
    md.append("business rules, data validation logic, calculations, and domain entities embedded in procedural COBOL ")
    md.append("code, enabling a clean separation of concerns in the modernized architecture.\n\n")
    
    # Subsections for Extracting Logic
    extraction_subsections = [
        (0, 5, "Extracting Business Rules and Validation Logic"),
        (5, 10, "Identifying Services, APIs, and Domain Entities")
    ]
    
    for start, end, title in extraction_subsections:
        md.append(f"### {title}\n")
        for qa in groups["extracting_logic"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Section 8.3: Tools for COBOL-to-C# Conversion
    md.append("## Section 8.3: Tools for COBOL-to-C# Conversion\n")
    md.append("Automated tools can accelerate the conversion process by parsing COBOL code, generating C# skeletons, ")
    md.append("analyzing dependencies, and verifying functional equivalence. This section surveys the landscape of ")
    md.append("conversion tools—from static analysis and AST parsers to commercial conversion suites and AI-powered ")
    md.append("assistants—and provides guidance on integrating these tools into a comprehensive modernization workflow.\n\n")
    
    # Subsections for Conversion Tools
    tools_subsections = [
        (0, 5, "Automated Conversion and Static Analysis Tools"),
        (5, 10, "Code Generation and Dependency Analysis"),
        (10, 15, "Testing, Quality Assurance, and Documentation Tools")
    ]
    
    for start, end, title in tools_subsections:
        md.append(f"### {title}\n")
        for qa in groups["conversion_tools"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Section 8.4: Handling Copybooks and Common Routines
    md.append("## Section 8.4: Handling Copybooks and Common Routines\n")
    md.append("COBOL copybooks and common routines present unique challenges during conversion, as they are shared ")
    md.append("across many programs and must be consistently transformed into C# libraries or shared components. ")
    md.append("This section addresses strategies for analyzing copybook dependencies, converting data structures ")
    md.append("and utility routines, and managing versioning to ensure the integrity of the modernized codebase.\n\n")
    
    # Subsections for Copybooks and Routines
    copybook_subsections = [
        (0, 5, "Copybook Analysis and Conversion Strategies"),
        (5, 10, "Common Routines, Versioning, and Consistency")
    ]
    
    for start, end, title in copybook_subsections:
        md.append(f"### {title}\n")
        for qa in groups["copybooks_routines"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Conclusion
    md.append("## Conclusion\n")
    md.append("Automated and manual code analysis are the foundation of any successful COBOL-to-C# conversion project. ")
    md.append("By mastering COBOL parsing techniques, extracting business logic and data structures, leveraging the ")
    md.append("right conversion tools, and carefully handling copybooks and common routines, development teams can ")
    md.append("systematically transform legacy code into a modern, maintainable C# architecture. The insights gained ")
    md.append("from thorough code analysis inform every subsequent phase of the modernization effort, ensuring that ")
    md.append("the new system preserves business logic while embracing modern design patterns and best practices.\n")
    
    return "".join(md)

def main():
    """Main execution."""
    data = load_research()
    print(f"✅ Loaded {len(data)} research Q&A pairs")
    
    chapter_content = write_chapter(data)
    
    with open("Chapter8_Code_Analysis.md", "w", encoding="utf-8") as f:
        f.write(chapter_content)
    
    print(f"✅ Chapter 8 written to Chapter8_Code_Analysis.md")
    
    # Count sections
    sections = chapter_content.count("## Section")
    subsections = chapter_content.count("###")
    print(f"📄 Total sections: {sections + 2} (including Introduction and Conclusion)")
    print(f"📝 Total subsections: {subsections}")

if __name__ == "__main__":
    main()
