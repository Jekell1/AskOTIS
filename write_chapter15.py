#!/usr/bin/env python3
"""
Write Chapter 15: Deployment and Rollout
Generates a comprehensive chapter from research Q&A data.
"""

import json

def main():
    # Load research data
    with open("chapter15_deployment_rollout_research.json", "r", encoding="utf-8") as f:
        qa_data = json.load(f)
    
    # Build chapter content
    chapter = []
    
    # Chapter title and introduction
    chapter.append("# Chapter 15: Deployment and Rollout\n")
    chapter.append("## Introduction\n")
    chapter.append("""
Deployment marks the transition from development to production—the moment when a modernization project shifts from theoretical promise to operational reality. When moving from a legacy COBOL loan servicing system to modern C# APIs and a new front end, deployment strategies must balance innovation with risk mitigation, ensuring business continuity while enabling transformative change.

This chapter explores three critical deployment dimensions: **DevOps practices for modernized systems**, which automate and standardize the deployment pipeline; **staging, production, and rollback plans**, which manage the transition from development through testing to live operation; and **training and documentation**, which ensure that both technical staff and end-users can effectively operate and support the new system.

Successful deployment is not a single event—it's a carefully orchestrated process that spans infrastructure automation, environment management, risk mitigation, knowledge transfer, and continuous improvement. The strategies and practices outlined in this chapter provide a roadmap for deploying a modernized system with confidence, minimizing disruption, and maximizing the chances of long-term success.
""")
    
    # Section 15.1: DevOps for Modernized Systems
    chapter.append("\n## Section 15.1: DevOps for Modernized Systems\n")
    chapter.append("### 15.1.1: CI/CD Pipelines and Infrastructure as Code\n")
    for i in range(0, 6):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 15.1.2: Containerization and Orchestration\n")
    for i in range(6, 11):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 15.1.3: Deployment Strategies and Security\n")
    for i in range(11, 17):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Section 15.2: Staging, Production, and Rollback Plans
    chapter.append("\n## Section 15.2: Staging, Production, and Rollback Plans\n")
    chapter.append("### 15.2.1: Environment Management and Testing\n")
    for i in range(17, 23):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 15.2.2: Production Deployment and Rollback\n")
    for i in range(23, 29):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 15.2.3: Validation and Coordination\n")
    for i in range(29, 34):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Section 15.3: Training and Documentation
    chapter.append("\n## Section 15.3: Training and Documentation\n")
    chapter.append("### 15.3.1: User and Technical Training\n")
    for i in range(34, 40):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 15.3.2: Documentation and Knowledge Transfer\n")
    for i in range(40, 45):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    chapter.append("\n### 15.3.3: Ongoing Support and Maintenance\n")
    for i in range(45, 50):
        qa = qa_data[i]
        chapter.append(f"\n**Q{qa['question_number']}: {qa['question']}**\n\n")
        chapter.append(f"{qa['answer']}\n")
    
    # Conclusion
    chapter.append("\n## Conclusion\n")
    chapter.append("""
Deployment and rollout represent the culmination of the modernization journey—the moment when careful planning, rigorous development, and comprehensive testing translate into operational reality. The DevOps practices, deployment strategies, and training programs outlined in this chapter provide a framework for managing this critical transition with confidence and minimal disruption.

Modern deployment is not a high-stakes, all-or-nothing gamble. Through infrastructure automation, progressive delivery, comprehensive rollback plans, and thorough documentation, teams can deploy incrementally, validate continuously, and adjust rapidly when issues arise. DevOps practices enable rapid iteration while maintaining stability. Staging environments provide safe testing grounds. Training and documentation ensure that success extends beyond the technical team to every stakeholder.

The strategies explored across this book—from understanding legacy systems, through architectural design and implementation, to testing and deployment—converge in this final chapter. By following these proven patterns and practices, organizations can successfully modernize mission-critical COBOL systems, transitioning from decades-old mainframe technology to modern, scalable, API-driven architectures that position them for future innovation and growth.
""")
    
    # Write to file
    output = "".join(chapter)
    with open("Chapter15_Deployment_Rollout.md", "w", encoding="utf-8") as f:
        f.write(output)
    
    print("✅ Chapter 15 written to Chapter15_Deployment_Rollout.md")
    print(f"   Total sections: 6 (including Introduction and Conclusion)")
    print(f"   Q&A pairs included: {len(qa_data)}")

if __name__ == "__main__":
    main()
