"""
Generate Chapter 7: Designing the Target Architecture
Reads research from chapter7_target_architecture_research.json and creates comprehensive markdown.
"""

import json

def load_research():
    """Load research Q&A from JSON file."""
    with open("chapter7_target_architecture_research.json", "r", encoding="utf-8") as f:
        return json.load(f)

def group_questions(data):
    """Group questions by section."""
    return {
        "layered_architecture": data[0:15],   # Q1-15
        "microservices_vs_monolith": data[15:25],  # Q16-25
        "api_design_patterns": data[25:40],   # Q26-40
        "security_considerations": data[40:50]  # Q41-50
    }

def write_chapter(data):
    """Generate complete Chapter 7 markdown."""
    groups = group_questions(data)
    
    md = []
    md.append("# Chapter 7: Designing the Target Architecture\n")
    md.append("## Introduction\n")
    md.append("Converting the OTIS/OTOS COBOL system to a modern C# architecture requires careful design of the target system. ")
    md.append("This chapter explores the architectural decisions that will shape the modernized application: how to structure ")
    md.append("layers for separation of concerns, whether to adopt microservices or a modular monolith, how to design robust ")
    md.append("APIs, and how to secure the entire system. These architectural choices will determine the maintainability, ")
    md.append("scalability, and security of the modernized OTIS platform.\n\n")
    
    # Section 7.1: Layered Architecture
    md.append("## Section 7.1: Layered Architecture - Presentation, Business Logic, and Data Layers\n")
    md.append("A well-designed layered architecture separates concerns, making the system easier to maintain, test, and evolve. ")
    md.append("This section examines how to structure the OTIS C# application into distinct layers—presentation, business logic, ")
    md.append("and data access—while applying modern patterns like Domain-Driven Design, dependency injection, and CQRS.\n\n")
    
    # Subsections for Layered Architecture
    layered_subsections = [
        (0, 1, "Foundations of Layered Architecture"),
        (1, 3, "Presentation and Application Service Layers"),
        (3, 6, "Business Logic and Domain Layer Design"),
        (6, 9, "Data Access Layer and Repository Pattern"),
        (9, 12, "Cross-Cutting Concerns and Dependency Injection"),
        (12, 15, "Advanced Patterns: CQRS, DTOs, and Transaction Management")
    ]
    
    for start, end, title in layered_subsections:
        md.append(f"### {title}\n")
        for qa in groups["layered_architecture"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Section 7.2: Microservices vs Modular Monolith
    md.append("## Section 7.2: Microservices vs Modular Monolith\n")
    md.append("One of the most critical architectural decisions is whether to adopt a microservices architecture or a modular ")
    md.append("monolith. This section evaluates both approaches for the OTIS system, considering bounded contexts, operational ")
    md.append("complexity, data management, and the ability to evolve the architecture over time.\n\n")
    
    # Subsections for Microservices vs Monolith
    monolith_subsections = [
        (0, 3, "Understanding Microservices and Modular Monoliths"),
        (3, 6, "Bounded Contexts and Service Boundaries"),
        (6, 10, "Operational and Performance Considerations")
    ]
    
    for start, end, title in monolith_subsections:
        md.append(f"### {title}\n")
        for qa in groups["microservices_vs_monolith"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Section 7.3: API Design Patterns
    md.append("## Section 7.3: API Design Patterns\n")
    md.append("Modern APIs are the backbone of the OTIS system's communication with front-end applications and external integrations. ")
    md.append("This section covers essential API design patterns: RESTful principles, versioning strategies, error handling, ")
    md.append("performance optimization, documentation, and the trade-offs between REST, GraphQL, and gRPC.\n\n")
    
    # Subsections for API Design Patterns
    api_subsections = [
        (0, 3, "RESTful API Fundamentals"),
        (3, 7, "API Versioning, Error Handling, and Response Design"),
        (7, 10, "Performance Optimization: Caching, Rate Limiting, and Async Patterns"),
        (10, 13, "API Documentation and Gateway Patterns"),
        (13, 15, "Choosing Between REST, GraphQL, and gRPC")
    ]
    
    for start, end, title in api_subsections:
        md.append(f"### {title}\n")
        for qa in groups["api_design_patterns"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Section 7.4: Security Considerations
    md.append("## Section 7.4: Security Considerations\n")
    md.append("Security is paramount for a financial system like OTIS. This section addresses authentication and authorization ")
    md.append("mechanisms, data protection strategies, vulnerability prevention, API security, compliance requirements, and ")
    md.append("security testing practices to ensure the modernized system meets enterprise security standards.\n\n")
    
    # Subsections for Security Considerations
    security_subsections = [
        (0, 3, "Authentication and Authorization"),
        (3, 6, "Data Protection and Secrets Management"),
        (6, 9, "API Security and CORS"),
        (9, 10, "Compliance, Audit Logging, and Security Testing")
    ]
    
    for start, end, title in security_subsections:
        md.append(f"### {title}\n")
        for qa in groups["security_considerations"][start:end]:
            md.append(f"**Q{qa['question_number']}: {qa['question']}**\n\n")
            md.append(f"{qa['answer']}\n\n")
    
    # Conclusion
    md.append("## Conclusion\n")
    md.append("Designing the target architecture for OTIS modernization requires balancing multiple concerns: separation of layers ")
    md.append("for maintainability, choosing between microservices and a modular monolith based on operational readiness, crafting ")
    md.append("robust and performant APIs, and implementing comprehensive security measures. The architectural decisions made in ")
    md.append("this phase will shape the system's ability to evolve, scale, and remain secure for years to come. By applying ")
    md.append("modern patterns and best practices from the C# ecosystem, the modernized OTIS system will be well-positioned to ")
    md.append("meet both current business needs and future growth.\n")
    
    return "".join(md)

def main():
    """Main execution."""
    data = load_research()
    print(f"✅ Loaded {len(data)} research Q&A pairs")
    
    chapter_content = write_chapter(data)
    
    with open("Chapter7_Target_Architecture.md", "w", encoding="utf-8") as f:
        f.write(chapter_content)
    
    print(f"✅ Chapter 7 written to Chapter7_Target_Architecture.md")
    
    # Count sections
    sections = chapter_content.count("## Section")
    subsections = chapter_content.count("###")
    print(f"📄 Total sections: {sections + 2} (including Introduction and Conclusion)")
    print(f"📝 Total subsections: {subsections}")

if __name__ == "__main__":
    main()
