#!/usr/bin/env python3
"""
Generate Chapter 18: Maintaining and Evolving the New System
Uses research data to create comprehensive markdown chapter on system maintenance and evolution.
"""

import json
from pathlib import Path

# Load research data
research_file = Path("chapter18_maintenance_research.json")
with open(research_file, 'r', encoding='utf-8') as f:
    research_data = json.load(f)

# Chapter structure
output_file = Path("Chapter18_System_Maintenance.md")

# Build markdown content
content = """# Chapter 18: Maintaining and Evolving the New System

## Introduction

The completion of COBOL-to-C# conversion marks not an ending but a beginning—the start of the modernized system's operational life. While the conversion journey demanded intense focus on achieving functional parity and ensuring data integrity, the long-term success of the modernization depends on how well the new system can be maintained, improved, and evolved to meet changing business needs.

This chapter addresses three critical dimensions of post-conversion system stewardship. First, establishing continuous improvement practices ensures the C# codebase doesn't accumulate technical debt and stagnate as the legacy COBOL system did. Second, developing robust processes for adding new features enables the business to capitalize on the modernization investment by delivering capabilities that weren't feasible in the legacy environment. Third, planning the final sunset of the legacy COBOL system ensures a clean transition with proper knowledge preservation and risk management.

The modernized OTIS loan servicing system represents a significant investment and a platform for future innovation. Success requires:
- **Continuous Improvement**: Refactoring, quality metrics, and technical debt management to keep the codebase healthy
- **Feature Evolution**: Processes and governance for safely adding new capabilities while maintaining stability
- **Legacy Sunset**: Methodical decommissioning of COBOL systems with proper contingency planning and knowledge preservation

These practices transform modernization from a one-time project into an ongoing capability, ensuring the new system delivers value for years to come while avoiding the maintenance traps that plagued the legacy environment.

---

## Section 18.1: Continuous Improvement of the C# Codebase

"""

# Section 18.1: Continuous Improvement (Q1-17)
subsections_18_1 = [
    ("18.1.1", "Code Quality and Refactoring Practices", 1, 6),
    ("18.1.2", "Technical Debt Management", 7, 11),
    ("18.1.3", "Performance and Architecture Evolution", 12, 17)
]

for subsection_num, subsection_title, start_q, end_q in subsections_18_1:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

content += """## Section 18.2: Adding New Features to the Modernized System

"""

# Section 18.2: Adding New Features (Q18-34)
subsections_18_2 = [
    ("18.2.1", "Feature Development Governance", 18, 23),
    ("18.2.2", "Modern Capabilities and Innovation", 24, 29),
    ("18.2.3", "Quality and Stability Maintenance", 30, 34)
]

for subsection_num, subsection_title, start_q, end_q in subsections_18_2:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

content += """## Section 18.3: Planning for Legacy System Sunset

"""

# Section 18.3: Legacy Sunset (Q35-50)
subsections_18_3 = [
    ("18.3.1", "Sunset Readiness and Planning", 35, 40),
    ("18.3.2", "Final Cutover and Decommissioning", 41, 45),
    ("18.3.3", "Knowledge Preservation and Lessons Learned", 46, 50)
]

for subsection_num, subsection_title, start_q, end_q in subsections_18_3:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

# Conclusion
content += """## Conclusion

The successful modernization of the OTIS loan servicing system from COBOL to C# represents a transformative achievement, but the journey doesn't end with deployment. This final chapter has explored the essential practices that ensure the modernized system delivers long-term value: continuous improvement to prevent technical decay, robust processes for feature evolution, and careful planning for the legacy system's sunset.

Continuous improvement practices—from code quality monitoring to refactoring discipline to performance optimization—prevent the new C# system from accumulating the same technical debt that eventually doomed the legacy COBOL environment. By establishing quality metrics, architectural governance, and regular improvement cycles, the team maintains a healthy codebase that welcomes change rather than resisting it. The modernization investment pays dividends only if the new system remains maintainable and evolvable.

The ability to add new features safely and efficiently represents the primary business justification for modernization. Modern capabilities like cloud services, real-time processing, and advanced analytics become accessible only if proper governance, testing, and deployment processes exist. Feature flagging, API versioning, comprehensive testing, and stakeholder collaboration ensure new capabilities enhance the system without compromising stability. The modernized platform must be both stable foundation and innovation enabler.

Finally, the legacy COBOL system's sunset must be planned as carefully as the conversion itself. Validation of complete functionality migration, contingency planning for discovered issues, archival strategies for historical data, and preservation of institutional knowledge ensure the transition completes successfully without organizational disruption. The sunset marks not just the end of the legacy system but the full maturation of the modernized environment.

This book has guided you through every phase of COBOL-to-C# modernization: from initial assessment and planning, through conversion and implementation, to deployment and ongoing evolution. The patterns, practices, and lessons presented here represent proven approaches refined through real-world modernization efforts. Your journey may differ in details, but the fundamental principles remain constant: understand deeply before changing, preserve business logic faithfully, test exhaustively, deploy carefully, and improve continuously. Armed with this knowledge and guided by these principles, your modernization efforts can transform legacy COBOL systems into modern, maintainable, evolvable platforms that serve the business for years to come.

The modernization journey is complete. The evolution journey begins.
"""

# Write to file
with open(output_file, 'w', encoding='utf-8') as f:
    f.write(content)

print(f"✅ Chapter 18 written to {output_file}")
print(f"   Sections: 6 total (including Introduction and Conclusion)")
print(f"   Research Q&A pairs integrated: {len(research_data)}")
