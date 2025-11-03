"""
Analyze _SCN.CPY file usage patterns from grep results.
Shows which programs use screen definition files and identifies orphaned screens.
"""

import os
import re
from collections import defaultdict

def find_scn_copy_statements():
    """Walk through all .CBL files and find COPY statements for _SCN.CPY files."""
    cobol_src = r"c:\Users\jeff.childers\Documents\OTISCodeResearcher\cobol_src"
    
    # Pattern to match COPY statements with _SCN in the filename
    copy_pattern = re.compile(r'^\s*COPY\s+"([^"]*_?SCN[^"]*\.CPY)"', re.IGNORECASE)
    
    # Track usage: scn_file -> list of programs that use it
    scn_usage = defaultdict(list)
    # Track all .CBL files that use ANY _SCN file
    programs_using_scn = set()
    
    # Walk through all .CBL files
    for root, dirs, files in os.walk(cobol_src):
        for file in files:
            if file.upper().endswith('.CBL'):
                file_path = os.path.join(root, file)
                rel_path = os.path.relpath(file_path, cobol_src)
                
                try:
                    with open(file_path, 'r', encoding='latin-1', errors='ignore') as f:
                        for line in f:
                            match = copy_pattern.match(line)
                            if match:
                                scn_file = match.group(1).upper()
                                scn_usage[scn_file].append(rel_path)
                                programs_using_scn.add(rel_path)
                except Exception as e:
                    print(f"Error reading {rel_path}: {e}")
    
    return scn_usage, programs_using_scn

def main():
    print("=" * 80)
    print("ANALYSIS: _SCN.CPY FILE USAGE PATTERNS")
    print("=" * 80)
    print()
    
    scn_usage, programs_using_scn = find_scn_copy_statements()
    
    # Sort by usage count (descending)
    sorted_scn = sorted(scn_usage.items(), key=lambda x: len(x[1]), reverse=True)
    
    print(f"Total unique _SCN files found: {len(sorted_scn)}")
    print(f"Total programs using _SCN files: {len(programs_using_scn)}")
    print()
    
    # Show top 20 most-used screen files
    print("=" * 80)
    print("TOP 20 MOST-USED SCREEN FILES:")
    print("=" * 80)
    for scn_file, programs in sorted_scn[:20]:
        print(f"{scn_file:50} used by {len(programs):4} program(s)")
    print()
    
    # Show orphaned or rarely-used screen files (1 reference or less)
    print("=" * 80)
    print("ORPHANED OR RARELY-USED SCREEN FILES (0-1 references):")
    print("=" * 80)
    rarely_used = [(scn, progs) for scn, progs in sorted_scn if len(progs) <= 1]
    
    if rarely_used:
        for scn_file, programs in rarely_used:
            if len(programs) == 0:
                print(f"{scn_file:50} ORPHANED (0 references)")
            else:
                print(f"{scn_file:50} used by: {programs[0]}")
    else:
        print("No orphaned or rarely-used screen files found.")
    print()
    
    # Check specifically for LPCAMU_SCN.CPY
    print("=" * 80)
    print("SPECIFIC CHECK: LPCAMU_SCN.CPY")
    print("=" * 80)
    
    lpcamu_found = False
    for scn_file, programs in scn_usage.items():
        if 'LPCAMU_SCN.CPY' in scn_file.upper():
            lpcamu_found = True
            print(f"Found: {scn_file}")
            print(f"Referenced by {len(programs)} program(s):")
            for prog in sorted(programs):
                print(f"  - {prog}")
    
    if not lpcamu_found:
        print("LPCAMU_SCN.CPY NOT FOUND - File is orphaned (0 references)")
    print()
    
    # Show distribution of usage counts
    print("=" * 80)
    print("USAGE DISTRIBUTION:")
    print("=" * 80)
    usage_counts = defaultdict(int)
    for scn_file, programs in scn_usage.items():
        count = len(programs)
        if count == 0:
            usage_counts['0 (orphaned)'] += 1
        elif count == 1:
            usage_counts['1'] += 1
        elif count <= 5:
            usage_counts['2-5'] += 1
        elif count <= 10:
            usage_counts['6-10'] += 1
        elif count <= 20:
            usage_counts['11-20'] += 1
        elif count <= 50:
            usage_counts['21-50'] += 1
        else:
            usage_counts['51+'] += 1
    
    for category in ['0 (orphaned)', '1', '2-5', '6-10', '11-20', '21-50', '51+']:
        if category in usage_counts:
            print(f"{category:20} {usage_counts[category]:4} screen file(s)")
    print()
    
    # Show all SCN files in LIBLP directory
    print("=" * 80)
    print("ALL SCREEN FILES IN LIBLP DIRECTORY:")
    print("=" * 80)
    liblp_scn = [(scn, progs) for scn, progs in sorted_scn if scn.startswith('LIBLP/')]
    for scn_file, programs in sorted(liblp_scn, key=lambda x: len(x[1]), reverse=True):
        print(f"{scn_file:50} used by {len(programs):4} program(s)")
    print()

if __name__ == "__main__":
    main()
