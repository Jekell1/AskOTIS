"""
Generate Chapter 12: Building the New Front End
Converts research JSON into structured markdown chapter.
"""

import json

def load_research():
    with open("chapter12_frontend_research.json", "r", encoding="utf-8") as f:
        return json.load(f)

def write_chapter(qa_pairs):
    """Generate the chapter markdown from Q&A pairs"""
    
    # Group questions by section
    sections = {
        "12.1": (1, 17, "Front End Technology Choices", [
            "Evaluating Framework Options",
            "Architecture and Tooling Decisions",
            "Standards and Requirements"
        ]),
        "12.2": (18, 33, "Consuming APIs", [
            "HTTP Client Implementation",
            "Authentication and Security",
            "Performance and Reliability Patterns"
        ]),
        "12.3": (34, 50, "UI/UX Modernization", [
            "Converting COBOL Screens to Modern UI",
            "Enhanced Interactions and Features",
            "User Experience Optimization"
        ])
    }
    
    md = []
    md.append("# Chapter 12: Building the New Front End\n")
    
    # Introduction
    md.append("## Introduction\n")
    md.append("The front end represents the most visible aspect of the COBOL-to-C# transformation. While users may never see the elegant API architecture or optimized database queries, they will immediately notice—and judge—the new user interface. This chapter addresses the critical decisions and implementation strategies for building a modern front end that not only replicates COBOL screen functionality but dramatically improves the user experience through contemporary design patterns, responsive layouts, and intuitive interactions.\n")
    md.append("Choosing the right front end technology stack is the first crucial decision. The choice between React, Angular, Vue, Blazor, or other frameworks must consider factors like team expertise, performance requirements, mobile support, and integration capabilities. Beyond the framework itself, decisions about state management, component libraries, and build tools will shape development velocity and long-term maintainability. This chapter provides a framework for evaluating options based on your organization's specific needs and constraints.\n")
    md.append("Consuming the C# APIs effectively requires more than just making HTTP requests. Modern front end applications must handle authentication, manage application state, provide responsive feedback during API calls, and gracefully handle errors. The patterns and practices for API consumption can significantly impact both user experience and application maintainability. Finally, UI/UX modernization represents an opportunity to rethink workflows that were constrained by character-based terminals. By analyzing user needs, applying modern design principles, and leveraging the capabilities of graphical interfaces, we can create applications that are not only more pleasant to use but also more productive and accessible.\n")
    
    # Process each section
    for section_key in ["12.1", "12.2", "12.3"]:
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
    md.append("Building a modern front end to replace COBOL terminal interfaces represents both a technical challenge and a tremendous opportunity. This chapter has explored the three essential dimensions of front end development: choosing the right technology stack, effectively consuming C# APIs, and modernizing the user experience. Success in these areas determines whether the migration is perceived as a mere technical exercise or as a transformational improvement that delights users and enhances productivity.\n")
    md.append("Technology choices must balance multiple concerns: team capabilities, performance requirements, mobile support, and long-term maintainability. Whether choosing React, Angular, Vue, or Blazor, the decision should be informed by a clear understanding of project requirements and organizational context. The supporting ecosystem—component libraries, state management solutions, build tools, and testing frameworks—is equally important. These choices establish the foundation for development velocity and code quality throughout the project lifecycle.\n")
    md.append("API consumption patterns significantly impact both user experience and application architecture. By implementing robust authentication, intelligent caching, graceful error handling, and responsive loading states, front end applications can provide a smooth, professional experience that builds user confidence. The patterns for handling real-time updates, optimistic updates, and offline capabilities enable functionality that was impossible in the terminal-based COBOL world.\n")
    md.append("UI/UX modernization is where the transformation becomes tangible for users. By replacing function-key navigation with intuitive menus, character-based forms with elegant input controls, and cryptic error codes with helpful messages, the new application can dramatically improve productivity and user satisfaction. Data visualization, responsive design, contextual help, and keyboard shortcuts combine to create an experience that respects power users while welcoming new ones. The front end is not merely a replacement for COBOL screens—it's an opportunity to reimagine how users interact with the system, delivering value that justifies the migration investment and positions the organization for future digital innovation.\n")
    
    return "\n".join(md)

def main():
    print("Loading research data...")
    qa_pairs = load_research()
    print(f"✅ Loaded {len(qa_pairs)} research Q&A pairs")
    
    print("Generating chapter markdown...")
    chapter_md = write_chapter(qa_pairs)
    
    output_file = "Chapter12_Building_Frontend.md"
    with open(output_file, "w", encoding="utf-8") as f:
        f.write(chapter_md)
    
    print(f"✅ Chapter 12 written to {output_file}")
    print(f"📄 Total sections: 6 (including Introduction and Conclusion)")

if __name__ == "__main__":
    main()
