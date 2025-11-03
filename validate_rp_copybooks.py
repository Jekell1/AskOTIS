"""Validate which copybooks are actually used in LONPF2 RP logic by tracing code."""

import re
from pathlib import Path

# Find LONPF2 source
cobol_src = Path("cobol_src")
lonpf2_file = None

# Search for LONPF2.CBL
for f in cobol_src.rglob("LONPF2.CBL"):
    lonpf2_file = f
    break

if not lonpf2_file:
    print("❌ LONPF2.CBL not found")
    exit(1)

print(f"✅ Found: {lonpf2_file}")

# Read source
source = lonpf2_file.read_text(encoding='utf-8', errors='ignore')
lines = source.splitlines()

print(f"📄 Total lines: {len(lines)}\n")

# Find all COPY statements
copy_pattern = re.compile(r'^\s*COPY\s+([A-Z0-9/_-]+)', re.IGNORECASE)
all_copybooks = set()

for i, line in enumerate(lines, 1):
    match = copy_pattern.match(line)
    if match:
        copybook = match.group(1).strip()
        # Extract just the filename
        if '/' in copybook:
            copybook = copybook.split('/')[-1]
        if not copybook.endswith('.CPY'):
            copybook += '.CPY'
        all_copybooks.add(copybook)

print(f"📚 Total unique copybooks in LONPF2: {len(all_copybooks)}\n")

# Find RP entry points
print("="*80)
print("FINDING RP ENTRY POINTS")
print("="*80)

rp_patterns = [
    r'LP-TRCD.*=.*["\']RP["\']',  # Transaction code = "RP"
    r'LP-REFNO.*=.*["\']RP["\']',  # Reference number = "RP"
    r'PROCESS-RP',  # Paragraph name
    r'RP-TRANSACTION',
    r'RP-PAYMENT',
    r'REPAYMENT',
]

rp_entry_lines = []
for i, line in enumerate(lines, 1):
    for pattern in rp_patterns:
        if re.search(pattern, line, re.IGNORECASE):
            rp_entry_lines.append((i, line.strip()))
            break

print(f"Found {len(rp_entry_lines)} potential RP entry points:\n")
for line_num, line_text in rp_entry_lines[:10]:  # Show first 10
    print(f"  Line {line_num:5d}: {line_text[:80]}")

if len(rp_entry_lines) > 10:
    print(f"  ... and {len(rp_entry_lines) - 10} more")

# Find paragraphs executed for RP
print(f"\n{'='*80}")
print("FINDING RP-RELATED PARAGRAPHS")
print(f"{'='*80}")

# Look for PERFORM statements near RP logic
rp_paragraphs = set()
context_window = 50  # Lines before/after RP mentions

for rp_line_num, _ in rp_entry_lines:
    start = max(0, rp_line_num - context_window)
    end = min(len(lines), rp_line_num + context_window)
    
    for i in range(start, end):
        line = lines[i]
        # Find PERFORM statements
        perform_match = re.search(r'PERFORM\s+([A-Z0-9-]+)', line, re.IGNORECASE)
        if perform_match:
            para = perform_match.group(1)
            rp_paragraphs.add(para)

print(f"Found {len(rp_paragraphs)} paragraphs potentially used in RP logic:\n")
for para in sorted(list(rp_paragraphs)[:20]):
    print(f"  - {para}")
if len(rp_paragraphs) > 20:
    print(f"  ... and {len(rp_paragraphs) - 20} more")

# Now find which copybooks are used in those paragraphs
print(f"\n{'='*80}")
print("FINDING COPYBOOKS USED IN RP PARAGRAPHS")
print(f"{'='*80}")

# Build paragraph line ranges
paragraph_lines = {}
current_para = None
para_start = 0

para_label_pattern = re.compile(r'^([A-Z0-9][A-Z0-9-]{0,62})\.\s*$', re.IGNORECASE)

for i, line in enumerate(lines):
    stripped = line.strip()
    if para_label_pattern.match(stripped):
        if current_para:
            paragraph_lines[current_para] = (para_start, i)
        current_para = stripped[:-1].upper()  # Remove trailing dot
        para_start = i

if current_para:
    paragraph_lines[current_para] = (para_start, len(lines))

print(f"Found {len(paragraph_lines)} total paragraphs in LONPF2")

# Find copybooks in RP paragraphs
rp_copybooks_by_usage = set()

for para_name in rp_paragraphs:
    if para_name in paragraph_lines:
        start, end = paragraph_lines[para_name]
        para_text = '\n'.join(lines[start:end])
        
        # Find data items/structures used in this paragraph
        # This is a simplification - real analysis would need to know which copybook each data item comes from
        # For now, just flag that this paragraph exists
        pass

# Simpler approach: Find all copybooks mentioned in sections with RP references
print("\n📊 ANALYSIS APPROACH:")
print("   Looking for copybooks mentioned within 100 lines of RP logic...")

rp_section_copybooks = set()
context = 100

for rp_line_num, _ in rp_entry_lines:
    start = max(0, rp_line_num - context)
    end = min(len(lines), rp_line_num + context)
    
    section = '\n'.join(lines[start:end])
    
    # Find COPY statements in this section
    for match in re.finditer(r'COPY\s+([A-Z0-9/_-]+)', section, re.IGNORECASE):
        copybook = match.group(1).strip()
        if '/' in copybook:
            copybook = copybook.split('/')[-1]
        if not copybook.endswith('.CPY'):
            copybook += '.CPY'
        rp_section_copybooks.add(copybook)

print(f"\n{'='*80}")
print("RESULTS")
print(f"{'='*80}")
print(f"Total copybooks in LONPF2:           {len(all_copybooks)}")
print(f"Copybooks near RP logic (±100 lines): {len(rp_section_copybooks)}")
print(f"\nRAG Answer said:                      178 copybooks")
print(f"Previous user expectation:            28 copybooks")

print(f"\n📋 Copybooks found near RP logic:")
for cb in sorted(list(rp_section_copybooks)[:30]):
    print(f"  - {cb}")
if len(rp_section_copybooks) > 30:
    print(f"  ... and {len(rp_section_copybooks) - 30} more")

# Check if REPAY* copybooks are in the list
repay_copybooks = [cb for cb in rp_section_copybooks if 'REPAY' in cb.upper()]
print(f"\n✅ REPAY* copybooks found: {len(repay_copybooks)}")
for cb in sorted(repay_copybooks):
    print(f"  - {cb}")

print(f"\n{'='*80}")
print("VALIDATION SUMMARY")
print(f"{'='*80}")
print(f"""
This analysis shows copybooks mentioned within ±100 lines of RP logic references.
This is a HEURISTIC - not true control flow analysis.

To truly validate the 178 number, you would need:
1. Parse LONPF2's paragraph structure completely
2. Trace execution paths that handle RP transactions
3. Track which data items are used in those paths
4. Map data items back to their defining copybooks
5. Return unique copybooks in RP execution paths

The current count ({len(rp_section_copybooks)}) is based on proximity heuristic.
The RAG answer (178) is based on LLM analyzing code chunks semantically.

Both are approximations. True answer requires control flow + data flow analysis.
""")
