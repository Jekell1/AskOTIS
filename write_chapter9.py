"""
Generate Chapter 9: Refactoring for Optimization
Converts research JSON into structured markdown chapter.
"""

import json

def load_research():
    with open("chapter9_refactoring_research.json", "r", encoding="utf-8") as f:
        return json.load(f)

def write_chapter(qa_pairs):
    """Generate the chapter markdown from Q&A pairs"""
    
    # Group questions by section
    sections = {
        "9.1": (1, 18, "Identifying Inefficient Patterns", [
            "Common Inefficient Coding Patterns",
            "File I/O Bottlenecks", 
            "Code Smells and Refactoring Opportunities",
            "Memory and Performance Issues"
        ]),
        "9.2": (19, 33, "Modernizing Data Access", [
            "From Flat Files to Relational Databases",
            "Database Design and Access Patterns",
            "Optimization and Caching Strategies"
        ]),
        "9.3": (34, 50, "Improving Performance and Maintainability", [
            "Performance Optimization Strategies",
            "Code Quality and Maintainability",
            "Testing, Deployment, and Monitoring"
        ])
    }
    
    md = []
    md.append("# Chapter 9: Refactoring for Optimization\n")
    
    # Introduction
    md.append("## Introduction\n")
    md.append("Converting a legacy COBOL application to modern C# is not just about translating syntax—it's an opportunity to fundamentally improve the system's performance, maintainability, and scalability. This chapter focuses on the critical refactoring strategies needed to optimize the OTIS codebase during conversion, addressing three key areas: identifying and eliminating inefficient patterns that plague legacy code, modernizing data access from flat files to relational databases, and implementing best practices that enhance both performance and long-term maintainability.\n")
    md.append("The refactoring process begins with a systematic analysis of the existing COBOL codebase to identify performance bottlenecks, code smells, and architectural weaknesses. By understanding these inefficiencies, we can prioritize refactoring efforts based on business value and technical impact. The transition from file-based data access to modern database systems represents one of the most significant improvements, enabling transaction management, concurrent access, and query optimization that were difficult or impossible in the legacy architecture.\n")
    md.append("Finally, this chapter explores how to leverage modern C# features, design patterns, and development practices to create a solution that is not only faster but also easier to maintain, test, and extend. By the end of this chapter, you'll have a comprehensive strategy for transforming legacy code into a high-performance, maintainable API-driven architecture.\n")
    
    # Process each section
    for section_key in ["9.1", "9.2", "9.3"]:
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
    md.append("Refactoring for optimization is a critical phase in the COBOL-to-C# conversion journey, transforming not just the language but the fundamental architecture and performance characteristics of the system. This chapter has explored three essential dimensions of optimization: identifying and eliminating inefficient patterns inherited from the legacy codebase, modernizing data access to leverage the power of relational databases, and implementing best practices that ensure long-term performance and maintainability.\n")
    md.append("The identification of inefficient patterns—from deeply nested IF statements to redundant file I/O operations—provides the foundation for targeted refactoring efforts. By understanding these code smells and their performance implications, development teams can prioritize improvements that deliver the greatest business value. The transition from flat file processing to modern database systems represents one of the most impactful changes, enabling transaction management, concurrent access, query optimization, and data integrity constraints that were difficult to achieve in the legacy environment.\n")
    md.append("Performance optimization and maintainability improvements go hand in hand. By leveraging modern C# features such as async/await for responsiveness, LINQ for data manipulation, and dependency injection for modularity, teams can create code that is not only faster but also easier to understand, test, and extend. The implementation of comprehensive testing strategies, continuous integration pipelines, and monitoring solutions ensures that performance gains are measurable and sustainable over time.\n")
    md.append("The strategies presented in this chapter provide a roadmap for systematic optimization, but they must be applied judiciously. Not every inefficient pattern requires immediate refactoring, and not every optimization provides equal value. The key is to measure, prioritize, and iterate—using performance metrics and business requirements to guide refactoring decisions. By combining technical excellence with business pragmatism, organizations can transform their legacy COBOL systems into modern, high-performance C# applications that meet today's demands while positioning the organization for future growth and innovation.\n")
    
    return "\n".join(md)

def main():
    print("Loading research data...")
    qa_pairs = load_research()
    print(f"✅ Loaded {len(qa_pairs)} research Q&A pairs")
    
    print("Generating chapter markdown...")
    chapter_md = write_chapter(qa_pairs)
    
    output_file = "Chapter9_Refactoring_Optimization.md"
    with open(output_file, "w", encoding="utf-8") as f:
        f.write(chapter_md)
    
    print(f"✅ Chapter 9 written to {output_file}")
    print(f"📄 Total sections: 6 (including Introduction and Conclusion)")

if __name__ == "__main__":
    main()
