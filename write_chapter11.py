"""
Generate Chapter 11: Implementing the C# APIs
Converts research JSON into structured markdown chapter.
"""

import json

def load_research():
    with open("chapter11_implementing_apis_research.json", "r", encoding="utf-8") as f:
        return json.load(f)

def write_chapter(qa_pairs):
    """Generate the chapter markdown from Q&A pairs"""
    
    # Group questions by section
    sections = {
        "11.1": (1, 13, "Setting Up the Project Structure", [
            "Solution Organization and Architecture",
            "Configuration and Cross-Cutting Concerns",
            "Build, Deployment, and Testing Setup"
        ]),
        "11.2": (14, 26, "Implementing Business Logic", [
            "Business Rule Implementation Patterns",
            "Transaction and Workflow Management",
            "Domain Events and Audit Logging"
        ]),
        "11.3": (27, 38, "Integrating with Modern Data Stores", [
            "Data Access Patterns and ORM",
            "Transaction and Concurrency Management",
            "Performance Optimization and Caching"
        ]),
        "11.4": (39, 50, "Error Handling and Validation", [
            "Exception Handling Strategy",
            "Validation Implementation",
            "Monitoring and Resilience"
        ])
    }
    
    md = []
    md.append("# Chapter 11: Implementing the C# APIs\n")
    
    # Introduction
    md.append("## Introduction\n")
    md.append("With the API design complete, the focus shifts to the actual implementation of the C# APIs that will replace COBOL business logic. This chapter provides comprehensive guidance on building robust, maintainable, and performant APIs using modern .NET development practices. The implementation phase is where architectural vision becomes concrete code, requiring careful attention to project structure, coding standards, data access patterns, and error handling strategies.\n")
    md.append("A well-organized project structure is the foundation of maintainable software. We'll explore how to set up a layered architecture that separates concerns, facilitates testing, and supports team collaboration. The structure must accommodate both the immediate need to convert COBOL functionality and the long-term goal of creating a flexible, scalable platform for future enhancements.\n")
    md.append("Implementing business logic requires translating procedural COBOL code into object-oriented C# while preserving business rules and improving code quality. Integration with modern data stores replaces file-based COBOL I/O with robust database access using Entity Framework or other ORMs. Finally, comprehensive error handling and validation ensure that the new APIs are resilient, provide clear feedback, and maintain the reliability that users expect from the legacy system.\n")
    
    # Process each section
    for section_key in ["11.1", "11.2", "11.3", "11.4"]:
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
    md.append("Implementing C# APIs to replace COBOL functionality is a complex undertaking that requires careful attention to architecture, code quality, and operational excellence. This chapter has provided comprehensive guidance across four critical dimensions: project structure, business logic implementation, data access integration, and error handling. Each of these dimensions contributes to creating APIs that are not only functional but also maintainable, testable, and ready for production use.\n")
    md.append("A well-organized project structure provides the foundation for long-term success. By establishing clear separation of concerns, consistent naming conventions, and robust dependency injection, development teams can work efficiently and avoid the technical debt that often plagues legacy modernization projects. The structure must balance immediate conversion needs with future extensibility, supporting both monolithic and microservices architectures as the system evolves.\n")
    md.append("Business logic implementation is where COBOL's procedural paradigm meets C#'s object-oriented capabilities. By applying proven patterns like Service Layer, Repository, and Domain Events, teams can create code that is easier to understand, test, and modify than the original COBOL programs. Modern data access through Entity Framework or Dapper provides performance, maintainability, and flexibility that file-based COBOL I/O cannot match, while comprehensive error handling ensures that the new system is resilient and provides clear feedback when issues occur.\n")
    md.append("The implementation patterns and practices presented in this chapter form a blueprint for creating production-ready APIs. However, successful implementation requires more than technical excellence—it demands continuous testing, code review, performance monitoring, and user feedback. By combining the architectural principles from previous chapters with the implementation details provided here, organizations can confidently build C# APIs that not only replace COBOL functionality but significantly improve upon it, delivering better performance, enhanced features, and a platform ready for future innovation.\n")
    
    return "\n".join(md)

def main():
    print("Loading research data...")
    qa_pairs = load_research()
    print(f"✅ Loaded {len(qa_pairs)} research Q&A pairs")
    
    print("Generating chapter markdown...")
    chapter_md = write_chapter(qa_pairs)
    
    output_file = "Chapter11_Implementing_APIs.md"
    with open(output_file, "w", encoding="utf-8") as f:
        f.write(chapter_md)
    
    print(f"✅ Chapter 11 written to {output_file}")
    print(f"📄 Total sections: 6 (including Introduction and Conclusion)")

if __name__ == "__main__":
    main()
