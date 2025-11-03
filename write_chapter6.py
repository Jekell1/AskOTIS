"""
Chapter 6 Writer: Choosing the Right Tools and Technologies
Generates comprehensive chapter from research data
"""

import json

def load_research(filename='chapter6_tools_technologies_research.json'):
    """Load research Q&A data"""
    with open(filename, 'r', encoding='utf-8') as f:
        return json.load(f)

def write_chapter(research_data, output_file='Chapter6_Tools_Technologies.md'):
    """Generate Chapter 6 markdown from research data"""
    
    # Group questions by section
    why_csharp = research_data[0:20]      # Q1-20: Why C# and .NET?
    api_frameworks = research_data[20:40]  # Q21-40: API Frameworks
    data_storage = research_data[40:50]    # Q41-50: Data Storage Options
    
    with open(output_file, 'w', encoding='utf-8') as f:
        # Title and Introduction
        f.write("# Chapter 6: Choosing the Right Tools and Technologies\n\n")
        
        f.write("## Introduction\n\n")
        f.write("Modernizing the OTIS COBOL application requires careful selection of technologies ")
        f.write("that balance business needs, technical capabilities, developer productivity, and ")
        f.write("long-term maintainability. This chapter explores why C# and .NET are ideal choices ")
        f.write("for this transformation, examines API framework options, and evaluates data storage ")
        f.write("strategies. Each technology decision is grounded in the specific requirements and ")
        f.write("constraints of the OTIS loan servicing system.\n\n")
        
        f.write("The technology stack chosen for modernization must support:\n")
        f.write("- Seamless migration of complex COBOL business logic\n")
        f.write("- Modern API-driven architecture for flexible front-end integration\n")
        f.write("- Scalable, cloud-ready infrastructure\n")
        f.write("- Robust data management for financial transactions\n")
        f.write("- Strong developer tooling and community support\n\n")
        
        # Section 6.1: Why C# and .NET?
        f.write("## Section 6.1: Why C# and .NET?\n\n")
        
        subsections_csharp = [
            ("6.1.1: C# Advantages for COBOL Migration", [0]),
            ("6.1.2: Language Features and Ecosystem", [1, 2]),
            ("6.1.3: Language Similarities and Differences", [3, 4]),
            ("6.1.4: Programming Paradigms", [5, 6]),
            ("6.1.5: Error Handling and Type Systems", [7, 8]),
            ("6.1.6: Control Structures and Business Logic", [9, 10]),
            ("6.1.7: Runtime Environment Comparison", [11, 12]),
            ("6.1.8: Development Tools and Debugging", [13, 14]),
            ("6.1.9: Data Access and LINQ", [15, 16]),
            ("6.1.10: Code Reusability and Cross-Platform Support", [17, 18, 19])
        ]
        
        for subsection_title, q_indices in subsections_csharp:
            f.write(f"### {subsection_title}\n\n")
            for idx in q_indices:
                qa = why_csharp[idx]
                f.write(f"**Question {qa['question_number']}:** {qa['question']}\n\n")
                f.write(f"{qa['answer']}\n\n")
                f.write("---\n\n")
        
        # Section 6.2: API Frameworks
        f.write("## Section 6.2: API Frameworks (ASP.NET Core, REST, GraphQL)\n\n")
        
        subsections_api = [
            ("6.2.1: ASP.NET Core Overview", [0, 1]),
            ("6.2.2: Middleware and Dependency Injection", [2, 3, 4]),
            ("6.2.3: API Design Patterns", [5, 6]),
            ("6.2.4: Validation and Security", [7, 8, 9]),
            ("6.2.5: Protocol Support and Compatibility", [10, 11]),
            ("6.2.6: REST vs GraphQL", [12, 13, 14]),
            ("6.2.7: API Gateway and Service Communication", [15, 16]),
            ("6.2.8: Serialization and Real-time Communication", [17, 18]),
            ("6.2.9: Monitoring and Observability", [19])
        ]
        
        for subsection_title, q_indices in subsections_api:
            f.write(f"### {subsection_title}\n\n")
            for idx in q_indices:
                qa = api_frameworks[idx]
                f.write(f"**Question {qa['question_number']}:** {qa['question']}\n\n")
                f.write(f"{qa['answer']}\n\n")
                f.write("---\n\n")
        
        # Section 6.3: Data Storage Options
        f.write("## Section 6.3: Data Storage Options (SQL, NoSQL, Cloud)\n\n")
        
        subsections_data = [
            ("6.3.1: SQL Database Selection", [0, 1, 2]),
            ("6.3.2: Entity Framework Core", [3]),
            ("6.3.3: NoSQL Databases", [4, 5]),
            ("6.3.4: Caching with Redis", [6]),
            ("6.3.5: Document Databases", [7]),
            ("6.3.6: Data Migration Strategy", [8]),
            ("6.3.7: Cloud Storage Solutions", [9])
        ]
        
        for subsection_title, q_indices in subsections_data:
            f.write(f"### {subsection_title}\n\n")
            for idx in q_indices:
                qa = data_storage[idx]
                f.write(f"**Question {qa['question_number']}:** {qa['question']}\n\n")
                f.write(f"{qa['answer']}\n\n")
                f.write("---\n\n")
        
        # Conclusion
        f.write("## Conclusion\n\n")
        f.write("The technology choices outlined in this chapter form the foundation for a ")
        f.write("successful OTIS modernization:\n\n")
        
        f.write("**C# and .NET provide:**\n")
        f.write("- A mature, enterprise-ready platform for business logic migration\n")
        f.write("- Strong typing and language features that map well to COBOL patterns\n")
        f.write("- Excellent tooling, debugging, and testing capabilities\n")
        f.write("- Cross-platform support and cloud-native architecture options\n\n")
        
        f.write("**ASP.NET Core enables:**\n")
        f.write("- High-performance REST and GraphQL APIs\n")
        f.write("- Built-in security, versioning, and documentation features\n")
        f.write("- Flexible middleware pipeline for cross-cutting concerns\n")
        f.write("- Real-time communication and modern API patterns\n\n")
        
        f.write("**Modern data storage supports:**\n")
        f.write("- SQL databases for transactional integrity and complex queries\n")
        f.write("- NoSQL options for caching, session management, and flexible schemas\n")
        f.write("- Cloud storage for scalability and disaster recovery\n")
        f.write("- Entity Framework Core for simplified data access and migrations\n\n")
        
        f.write("These technology selections prioritize:\n")
        f.write("1. **Migration feasibility** - C# patterns align with COBOL business logic\n")
        f.write("2. **Developer productivity** - Modern tooling and frameworks accelerate development\n")
        f.write("3. **Scalability** - Cloud-ready architecture supports growth\n")
        f.write("4. **Maintainability** - Strong typing, testing, and community support\n")
        f.write("5. **Future-proofing** - Active ecosystem with ongoing innovation\n\n")
        
        f.write("The next chapter will detail the conversion strategy, breaking down how to ")
        f.write("systematically transform COBOL programs, data structures, and business logic ")
        f.write("into modern C# APIs using these selected technologies.\n")
    
    print(f"✅ Chapter 6 written to {output_file}")
    print(f"📄 Total sections: 3 major sections")
    print(f"📝 Total subsections: 26")

def main():
    # Load research data
    research = load_research()
    print(f"✅ Loaded {len(research)} research Q&A pairs")
    
    # Generate chapter
    write_chapter(research)

if __name__ == "__main__":
    main()
