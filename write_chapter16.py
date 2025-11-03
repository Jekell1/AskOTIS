#!/usr/bin/env python3
"""
Generate Chapter 16: Real-World Conversion Examples
Uses research data to create comprehensive markdown chapter with case studies.
"""

import json
from pathlib import Path

# Load research data
research_file = Path("chapter16_case_studies_research.json")
with open(research_file, 'r', encoding='utf-8') as f:
    research_data = json.load(f)

# Chapter structure
output_file = Path("Chapter16_Case_Studies.md")

# Build markdown content
content = """# Chapter 16: Real-World Conversion Examples

## Introduction

The theoretical frameworks, architectural patterns, and best practices discussed throughout this book become tangible when applied to actual COBOL-to-C# conversions. This chapter presents three detailed case studies drawn from the OTIS loan servicing system modernization, demonstrating how legacy COBOL components transform into modern C# APIs and services.

Each case study walks through a complete conversion journey: from analyzing the original COBOL structure, through designing the modern equivalent, to implementing and testing the C# solution. These real-world examples illustrate not only the technical transformations but also the business logic preservation, testing strategies, and deployment considerations that ensure successful modernization.

The three case studies represent different complexity levels and conversion patterns:
- **TIMEIO.CPY Utility Conversion**: Transforming a widely-used COBOL copybook utility into a reusable C# class library
- **Loan Payment Workflow**: Converting interconnected COBOL batch programs into a cohesive C# API workflow
- **MBUNR1H.CBL Branch Totals**: Redesigning a COBOL reporting program as a modern REST API with query capabilities

These examples provide concrete templates and patterns that can be adapted to your own modernization efforts, demonstrating proven approaches to common conversion challenges.

---

## Section 16.1: Case Study: Converting TIMEIO.CPY Utility

"""

# Section 16.1: TIMEIO.CPY Conversion (Q1-17)
subsections_16_1 = [
    ("16.1.1", "Understanding TIMEIO.CPY Structure and Usage", 1, 6),
    ("16.1.2", "Designing the C# TimeUtility Class", 7, 11),
    ("16.1.3", "Testing and Deployment", 12, 17)
]

for subsection_num, subsection_title, start_q, end_q in subsections_16_1:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

content += """## Section 16.2: Case Study: Loan Payment Workflow Conversion

"""

# Section 16.2: Loan Payment Workflow (Q18-34)
subsections_16_2 = [
    ("16.2.1", "Analyzing the COBOL Payment Processing Flow", 18, 23),
    ("16.2.2", "Designing the C# Payment API Architecture", 24, 29),
    ("16.2.3", "Implementation, Testing, and Cutover", 30, 34)
]

for subsection_num, subsection_title, start_q, end_q in subsections_16_2:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

content += """## Section 16.3: Case Study: Converting MBUNR1H.CBL to Branch Totals API

"""

# Section 16.3: MBUNR1H.CBL Branch Totals (Q35-50)
subsections_16_3 = [
    ("16.3.1", "Understanding MBUNR1H.CBL Business Logic", 35, 40),
    ("16.3.2", "Designing the Branch Totals REST API", 41, 45),
    ("16.3.3", "Optimization and Production Deployment", 46, 50)
]

for subsection_num, subsection_title, start_q, end_q in subsections_16_3:
    content += f"### {subsection_num}: {subsection_title}\n\n"
    for i in range(start_q - 1, end_q):
        qa = research_data[i]
        content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
        content += f"{qa['answer']}\n\n"
    content += "---\n\n"

# Conclusion
content += """## Conclusion

These three case studies demonstrate the practical application of COBOL-to-C# modernization principles across different scenarios. The TIMEIO.CPY conversion shows how to transform shared utilities into reusable libraries, preserving critical date/time logic while improving maintainability. The loan payment workflow illustrates the decomposition of sequential batch processing into cohesive API services, maintaining transactional integrity while enabling new integration patterns. The MBUNR1H.CBL transformation demonstrates how reporting batch jobs can evolve into flexible REST APIs, providing enhanced query capabilities while maintaining calculation accuracy.

Each case study reinforces key modernization themes: understanding the legacy system deeply before converting, designing modern equivalents that preserve business logic exactly, implementing comprehensive testing to verify behavioral equivalence, and planning careful cutover strategies to minimize risk. The patterns and techniques demonstrated here—from handling COMP-3 packed decimals to implementing API versioning to coordinating database migrations—apply broadly to COBOL modernization efforts beyond the OTIS system.

As you tackle your own conversion projects, these real-world examples provide reference implementations and proven approaches. The challenges encountered and solutions applied in these case studies represent common patterns you'll encounter, whether converting utilities, workflows, or reporting systems. Success comes from combining technical excellence with methodical planning, thorough testing, and careful attention to business continuity throughout the modernization journey.
"""

# Write to file
with open(output_file, 'w', encoding='utf-8') as f:
    f.write(content)

print(f"✅ Chapter 16 written to {output_file}")
print(f"   Sections: 6 total (including Introduction and Conclusion)")
print(f"   Research Q&A pairs integrated: {len(research_data)}")
