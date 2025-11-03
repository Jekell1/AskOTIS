#!/usr/bin/env python3
"""
Write Chapter 14: Testing and Quality Assurance
Generates a comprehensive chapter from research Q&A data.
"""

import json

def main():
    # Load research data
    with open("chapter14_testing_qa_research.json", "r", encoding="utf-8") as f:
        qa_data = json.load(f)
    
    # Build chapter content
    chapter = []
    
    # Chapter title and introduction
    chapter.append("# Chapter 14: Testing and Quality Assurance\n")
    chapter.append("## Introduction\n")
    chapter.append("""
Testing is the safety net that ensures a modernization project delivers on its promises. When converting a mission-critical COBOL loan servicing system like OTIS to modern C# APIs, comprehensive testing strategies validate that business logic translates correctly, data migrations complete accurately, and the new system meets or exceeds the reliability and performance of the legacy platform.

This chapter explores three essential testing dimensions: **unit and integration testing for APIs**, which validate individual components and their interactions; **regression testing against legacy outputs**, which ensures the new system produces identical business results; and **user acceptance testing (UAT)**, which confirms the modernized system meets real-world business needs and user expectations.

Effective testing is not just about finding bugs—it's about building confidence. Confidence that decades of proven business logic have been faithfully translated. Confidence that data has migrated accurately. Confidence that end-users can perform their daily tasks efficiently in the new system. This chapter provides the frameworks, tools, and strategies to achieve that confidence systematically and comprehensively.
""")
    
    # Section 14.1: Unit and Integration Testing for APIs
    chapter.append("\n## Section 14.1: Unit and Integration Testing for APIs\n")
    chapter.append("### 14.1.1: Unit Testing Frameworks and Strategies\n")
    for i in range(0, 6):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 14.1.2: Integration Testing and Test Automation\n")
    for i in range(6, 11):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 14.1.3: API Testing Tools and Documentation\n")
    for i in range(11, 17):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Section 14.2: Regression Testing Against Legacy Outputs
    chapter.append("\n## Section 14.2: Regression Testing Against Legacy Outputs\n")
    chapter.append("### 14.2.1: Capturing and Comparing Legacy Outputs\n")
    for i in range(17, 23):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 14.2.2: Automated Regression Testing\n")
    for i in range(23, 29):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 14.2.3: Edge Cases and Migration Validation\n")
    for i in range(29, 34):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Section 14.3: User Acceptance Testing
    chapter.append("\n## Section 14.3: User Acceptance Testing\n")
    chapter.append("### 14.3.1: UAT Planning and Execution\n")
    for i in range(34, 40):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 14.3.2: Feedback, Defects, and Acceptance Criteria\n")
    for i in range(40, 45):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 14.3.3: Non-Functional and Usability Testing\n")
    for i in range(45, 50):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Conclusion
    chapter.append("\n## Conclusion\n")
    chapter.append("""
Comprehensive testing is the cornerstone of successful modernization. The multi-layered testing strategy outlined in this chapter—from unit tests validating individual business logic components, through regression tests ensuring equivalence with legacy outputs, to user acceptance testing confirming real-world usability—provides the confidence needed to retire a decades-old COBOL system and embrace a modern C# API architecture.

Testing is not a phase that happens at the end of development; it's a continuous practice integrated throughout the modernization journey. Automated unit and integration tests enable rapid development cycles. Regression testing against legacy outputs catches translation errors early. User acceptance testing validates that technical success translates to business value.

With rigorous testing strategies in place, validated through the frameworks and tools described here, teams can proceed to deployment with confidence—knowing that the modernized system has been thoroughly vetted against both technical specifications and real-world business requirements. The deployment and rollout strategies in the next chapter will build on this foundation of quality assurance.
""")
    
    # Write to file
    output = "".join(chapter)
    with open("Chapter14_Testing_QA.md", "w", encoding="utf-8") as f:
        f.write(output)
    
    print("✅ Chapter 14 written to Chapter14_Testing_QA.md")
    print(f"   Total sections: 6 (including Introduction and Conclusion)")
    print(f"   Q&A pairs included: {len(qa_data)}")

if __name__ == "__main__":
    main()
