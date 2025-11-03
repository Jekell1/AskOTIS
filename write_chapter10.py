"""
Generate Chapter 10: API Modeling and Design
Converts research JSON into structured markdown chapter.
"""

import json

def load_research():
    with open("chapter10_api_design_research.json", "r", encoding="utf-8") as f:
        return json.load(f)

def write_chapter(qa_pairs):
    """Generate the chapter markdown from Q&A pairs"""
    
    # Group questions by section
    sections = {
        "10.1": (1, 18, "Translating COBOL Programs to API Endpoints", [
            "Identifying API Candidates and Resource Mapping",
            "Transaction Programs and CRUD Operations",
            "Batch Programs and Workflow Design",
            "Program Interactions and Service Boundaries"
        ]),
        "10.2": (19, 33, "Mapping Data Structures to DTOs and Models", [
            "COBOL to C# Data Type Mapping",
            "Complex Data Structures and Patterns",
            "DTO Design and Validation"
        ]),
        "10.3": (34, 50, "Designing CRUD Operations and Business Workflows", [
            "RESTful CRUD Endpoint Design",
            "Business Workflow and Process Design",
            "Advanced API Patterns and Operations"
        ])
    }
    
    md = []
    md.append("# Chapter 10: API Modeling and Design\n")
    
    # Introduction
    md.append("## Introduction\n")
    md.append("The transformation from COBOL programs to modern REST APIs represents one of the most critical architectural shifts in the migration journey. This chapter addresses the strategic design decisions required to expose COBOL business logic and data through well-designed, RESTful API endpoints that can support modern web and mobile applications. The goal is not simply to create a thin wrapper around legacy code, but to thoughtfully model APIs that are intuitive, maintainable, and aligned with industry best practices.\n")
    md.append("The process begins with understanding how to identify which COBOL programs should become public API endpoints versus internal service methods, and how to map COBOL's procedural operations to resource-oriented REST patterns. COBOL programs were designed around file operations and screen flows; APIs must be designed around business resources and workflows. This requires careful analysis of program purpose, data access patterns, and transaction boundaries to create cohesive service boundaries that make sense in a modern architecture.\n")
    md.append("Data transformation is equally critical—COBOL's rich type system with PICTURE clauses, REDEFINES, and packed decimals must be mapped to C# DTOs that are both faithful to the business domain and convenient for API consumers. Finally, designing CRUD operations and business workflows requires understanding how to represent complex multi-step COBOL processes as stateless or stateful API interactions, implementing validation, error handling, and transaction management in ways that preserve business logic integrity while embracing modern API patterns. This chapter provides a comprehensive framework for API design that balances pragmatism with architectural excellence.\n")
    
    # Process each section
    for section_key in ["10.1", "10.2", "10.3"]:
        start, end, title, subsections = sections[section_key]
        
        md.append(f"## Section {section_key}: {title}\n")
        
        # Calculate questions per subsection
        section_questions = qa_pairs[start-1:end]
        questions_per_subsection = len(section_questions) // len(subsections)
        
        subsection_idx = 0
        for i, qa in enumerate(section_questions):
            # Add subsection header
            if i % questions_per_subsection == 0 and subsection_idx < len(subsections):
                md.append(f"### {subsections[subsection_idx]}\n")
                subsection_idx += 1
            
            # Add Q&A
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n")
            md.append(f"{qa['answer']}\n")
    
    # Conclusion
    md.append("## Conclusion\n")
    md.append("API modeling and design represents the bridge between legacy COBOL systems and modern application architectures. This chapter has explored the three essential dimensions of API design: translating COBOL programs to REST endpoints, mapping data structures to DTOs and models, and designing CRUD operations and business workflows. Each dimension requires careful consideration of both technical constraints and business requirements to create APIs that are not only functional but also maintainable, scalable, and aligned with industry best practices.\n")
    md.append("The translation of COBOL programs to API endpoints demands a shift in thinking from procedural file operations to resource-oriented REST patterns. By identifying cohesive service boundaries, mapping transaction programs to HTTP operations, and designing workflow APIs that preserve business logic integrity, development teams can create APIs that accurately represent business capabilities while hiding the complexity of legacy implementations. The key is to think in terms of business resources and operations rather than direct translations of COBOL program logic.\n")
    md.append("Data structure mapping is more than a technical exercise—it's an opportunity to create clean, well-documented contracts between API consumers and the business domain. By thoughtfully converting COBOL PICTURE clauses to C# types, handling complex structures like REDEFINES and OCCURS, and designing DTOs that separate API contracts from internal models, teams can create APIs that are both accurate and convenient to use. Proper validation, clear naming conventions, and comprehensive documentation ensure that API consumers can work effectively without needing to understand COBOL internals.\n")
    md.append("Finally, designing CRUD operations and business workflows requires balancing RESTful principles with practical business requirements. By implementing proper error handling, transaction management, idempotency, and audit logging, APIs can provide the reliability and transparency that enterprise applications demand. Whether handling simple CRUD operations or complex multi-step workflows, the patterns and practices presented in this chapter provide a foundation for API designs that support both immediate migration needs and long-term architectural evolution. The result is an API layer that not only replaces COBOL functionality but enhances it with modern capabilities like asynchronous processing, fine-grained security, and comprehensive observability.\n")
    
    return "\n".join(md)

def main():
    print("Loading research data...")
    qa_pairs = load_research()
    print(f"✅ Loaded {len(qa_pairs)} research Q&A pairs")
    
    print("Generating chapter markdown...")
    chapter_md = write_chapter(qa_pairs)
    
    output_file = "Chapter10_API_Modeling_Design.md"
    with open(output_file, "w", encoding="utf-8") as f:
        f.write(chapter_md)
    
    print(f"✅ Chapter 10 written to {output_file}")
    print(f"📄 Total sections: 6 (including Introduction and Conclusion)")

if __name__ == "__main__":
    main()
