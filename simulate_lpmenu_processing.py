"""Simulate build_screen_nodes processing on LPMENU_SCN."""
import re, hashlib
from ingestion_common import stable_hash
from pathlib import Path

# Read LPMENU file
with open('cobol_src/LIBLP/LPMENU_SCN.CPY', 'r') as f:
    content = f.read()

# Calculate program_id
lpmenu_path = Path('cobol_src/LIBLP/LPMENU_SCN.CPY')
prog = stable_hash([str(lpmenu_path)])

print(f"Processing LPMENU_SCN.CPY")
print(f"Program ID (hash): {prog}")
print("="*80)

# Extract labels
label_pattern = re.compile(
    r'LABEL\s+LINE\s+(\d+)\s+COL\s+(\d+)\s*\n?\s*["\']([^"\']+)["\']',
    re.IGNORECASE | re.MULTILINE
)
matches = label_pattern.findall(content)
label_literals = [{'line': m[0], 'col': m[1], 'text': m[2]} for m in matches]

print(f"\nExtracted {len(label_literals)} labels:")
for lbl in label_literals:
    print(f"  Line {lbl['line']:>2} Col {lbl['col']:>2}: {lbl['text']}")

# Simulate filtering (from lines 265-279)
label_texts = [lbl.get('text', '') for lbl in label_literals if lbl.get('text')]
print(f"\n\nApplying filters...")
print(f"Before filtering: {len(label_texts)} texts")

filtered = []
for text in label_texts:
    text_upper = text.upper()
    text_stripped = text.strip()
    
    # Skip generic prompts
    if 'ENTER SELECTION' in text_upper:
        print(f"  SKIPPED (generic): {text}")
        continue
    
    # Skip menu titles
    if (text_upper.endswith('MENU') or 'MAINTENANCE MENU' in text_upper) and not any(text_stripped.startswith(prefix) for prefix in ['1.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '10.', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7']):
        print(f"  SKIPPED (menu title): {text}")
        continue
    
    filtered.append(text)

print(f"\nAfter filtering: {len(filtered)} texts")
print("\nFinal menu items that would appear in summary:")
for text in filtered:
    print(f"  {text}")

# Check if this would create a screen node
if filtered:
    print(f"\n✅ Would create screen node with {len(filtered)} menu items")
else:
    print("\n❌ Would NOT create screen node (no menu items after filtering)")
