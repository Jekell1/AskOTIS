#!/usr/bin/env python3
"""
Generate Appendices
Creates comprehensive appendices including glossary, reference tables, and code samples.
"""

import json
from pathlib import Path

# Load research data
research_file = Path("appendices_research.json")
with open(research_file, 'r', encoding='utf-8') as f:
    research_data = json.load(f)

# Output file
output_file = Path("Appendices.md")

# Build markdown content
content = """# Appendices

The following appendices provide essential reference material for COBOL-to-C# modernization projects. Appendix A offers a comprehensive glossary of technical terms spanning legacy mainframe concepts, modern development practices, and migration strategies. Appendix B presents detailed mapping tables showing how COBOL constructs translate to C# equivalents, serving as a quick reference during conversion work. Appendix C provides concrete code examples demonstrating common conversion patterns with side-by-side COBOL and C# implementations.

These appendices are designed to serve as practical tools throughout the modernization journey, offering quick answers to common questions and providing templates for typical conversion scenarios.

---

## Appendix A: Glossary of Terms

This glossary defines key terminology used throughout the book, organized by domain to help readers quickly locate relevant definitions.

"""

# Appendix A: Glossary (Q1-20)
for i in range(0, 20):
    qa = research_data[i]
    content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
    content += f"{qa['answer']}\n\n"

content += "---\n\n"
content += """## Appendix B: Reference Tables (COBOL to C# Mapping)

This section provides comprehensive mapping tables showing how COBOL data types, operations, and structures translate to modern C# equivalents. These tables serve as quick references during conversion work, helping developers make appropriate choices when translating legacy code.

### Data Type Mappings

"""

# Appendix B: Reference Tables (Q21-35)
for i in range(20, 35):
    qa = research_data[i]
    content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
    content += f"{qa['answer']}\n\n"

content += "---\n\n"
content += """## Appendix C: Sample Code Listings

This appendix provides complete code examples demonstrating common conversion patterns. Each example shows the original COBOL implementation alongside its modern C# equivalent, highlighting important translation considerations and modernization opportunities.

### Representative Conversion Examples

"""

# Appendix C: Sample Code (Q36-50)
for i in range(35, 50):
    qa = research_data[i]
    content += f"**Q{qa['question_number']}: {qa['question']}**\n\n"
    content += f"{qa['answer']}\n\n"

# Final note
content += """---

## Using These Appendices

These appendices are designed as working references to support your modernization efforts:

- **Glossary**: Use when encountering unfamiliar terms in legacy documentation or modern architecture discussions
- **Reference Tables**: Consult during code conversion to identify appropriate C# equivalents for COBOL constructs
- **Sample Code**: Review when implementing specific conversion patterns or troubleshooting translation issues

The patterns and mappings presented here represent best practices refined through real-world COBOL modernization projects. While every legacy system has unique characteristics, these references provide solid starting points that can be adapted to specific project requirements.

For additional examples and detailed explanations of specific conversion scenarios, refer back to the relevant chapters in the main text, particularly Chapters 8-12 covering conversion and implementation strategies.
"""

# Write to file
with open(output_file, 'w', encoding='utf-8') as f:
    f.write(content)

print(f"✅ Appendices written to {output_file}")
print(f"   Total sections: 3 (Glossary, Reference Tables, Sample Code)")
print(f"   Research Q&A pairs integrated: {len(research_data)}")
