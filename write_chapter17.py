#!/usr/bin/env python3
"""
Generate Chapter 17: Common Pitfalls and How to Avoid Them
Uses research data to create comprehensive markdown chapter on avoiding common mistakes.
"""

import json
from pathlib import Path

# Load research data
research_file = Path("chapter17_pitfalls_research.json")
with open(research_file, 'r', encoding='utf-8') as f:
    research_data = json.load(f)

# Chapter structure
output_file = Path("Chapter17_Common_Pitfalls.md")

# Build markdown content
content = """# Chapter 17: Common Pitfalls and How to Avoid Them

## Introduction

COBOL-to-C# modernization projects face predictable challenges that can derail even well-planned efforts. While each conversion has unique aspects, certain categories of problems recur across projects: data loss during migration, misunderstood business rules leading to incorrect implementations, and performance bottlenecks that emerge when legacy patterns translate poorly to modern architectures.

This chapter catalogs the most common pitfalls encountered during COBOL modernization, organized into three critical areas. Understanding these challenges before they occur enables proactive mitigation rather than reactive firefighting. Each pitfall is examined through the lens of the OTIS loan servicing system conversion, providing concrete examples of what can go wrong and proven strategies to prevent or resolve these issues.

The patterns discussed here represent hard-won lessons from actual modernization efforts:
- **Data Loss and Precision Issues**: How COBOL's unique data types and implicit conversions can cause silent corruption when translated to C#
- **Misinterpreting Business Rules**: The hidden logic embedded in COBOL code structures, execution patterns, and implicit behaviors that must be preserved
- **Performance Bottlenecks**: How naive translations of COBOL processing patterns can create severe scalability problems in modern systems

By learning from these common mistakes, your team can anticipate challenges, implement appropriate safeguards, and maintain project momentum even when complications arise.

---

## Section 17.1: Data Loss and Precision Issues

"""

# Section 17.1: Data Loss and Precision (Q1-17)
subsections_17_1 = [
    ("17.1.1", "COBOL Data Type Conversion Challenges", 1, 6),
    ("17.1.2", "Character Encoding and Comparison Issues", 7, 11),
    ("17.1.3", "Migration and Validation Strategies", 12, 17)
]

for subsection_num, subsection_title, start_q, end_q in subsections_17_1:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

content += """## Section 17.2: Misinterpreting Business Rules

"""

# Section 17.2: Misinterpreting Business Rules (Q18-34)
subsections_17_2 = [
    ("17.2.1", "Hidden Logic in COBOL Code Structures", 18, 23),
    ("17.2.2", "Sequential Processing and Control Flow Issues", 24, 29),
    ("17.2.3", "Discovery and Validation Techniques", 30, 34)
]

for subsection_num, subsection_title, start_q, end_q in subsections_17_2:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

content += """## Section 17.3: Performance Bottlenecks and Scalability Issues

"""

# Section 17.3: Performance Bottlenecks (Q35-50)
subsections_17_3 = [
    ("17.3.1", "Query and Data Access Anti-patterns", 35, 40),
    ("17.3.2", "Memory, I/O, and Concurrency Issues", 41, 45),
    ("17.3.3", "Monitoring and Optimization Strategies", 46, 50)
]

for subsection_num, subsection_title, start_q, end_q in subsections_17_3:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

# Conclusion
content += """## Conclusion

The pitfalls cataloged in this chapter represent the most common obstacles encountered during COBOL-to-C# modernization, but awareness transforms these potential disasters into manageable challenges. Data loss and precision issues, while technically complex, can be prevented through rigorous type mapping, comprehensive testing, and thorough validation during migration. Business rule misinterpretations, often the most insidious problems, require methodical code analysis, stakeholder engagement, and extensive regression testing against legacy outputs. Performance bottlenecks, though they may not surface until production load testing or deployment, can be anticipated and prevented through careful architectural design, appropriate use of modern patterns, and proactive monitoring.

The key to avoiding these pitfalls lies in three principles: **never assume**, **always verify**, and **test exhaustively**. Don't assume COBOL's packed decimal fields map cleanly to C# decimals—verify precision and test edge cases. Don't assume you understand a business rule from reading COBOL code—verify with stakeholders and test against real data. Don't assume your C# API will perform adequately—verify with load testing and monitor production behavior.

Every modernization project will encounter some of these challenges, but armed with the knowledge of what can go wrong and proven mitigation strategies, your team can navigate these obstacles successfully. The patterns and practices discussed here—from data reconciliation reports to regression testing frameworks to performance profiling—represent battle-tested approaches refined through multiple modernization efforts. Learn from these lessons, implement appropriate safeguards early, and maintain vigilance throughout the conversion process to ensure your modernization succeeds where others have struggled.
"""

# Write to file
with open(output_file, 'w', encoding='utf-8') as f:
    f.write(content)

print(f"✅ Chapter 17 written to {output_file}")
print(f"   Sections: 6 total (including Introduction and Conclusion)")
print(f"   Research Q&A pairs integrated: {len(research_data)}")
