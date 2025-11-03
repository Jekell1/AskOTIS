"""
Use grep to find which .CBL files COPY _SCN.CPY files.
"""
import os
import re
from pathlib import Path

def find_scn_copy_statements(cobol_src_dir):
    """Find all COPY statements for _SCN.CPY files in .CBL programs."""
    
    scn_usage = {}
    
    # Walk through all .CBL files
    for root, dirs, files in os.walk(cobol_src_dir):
        for file in files:
            if file.upper().endswith('.CBL'):
                file_path = os.path.join(root, file)
                
                try:
                    with open(file_path, 'r', encoding='latin-1', errors='ignore') as f:
                        content = f.read()
                    
                    # Find COPY statements with _SCN in them
                    copy_pattern = r'^\s*COPY\s+"([^"]*_SCN[^"]*\.CPY)"'
                    matches = re.findall(copy_pattern, content, re.MULTILINE | re.IGNORECASE)
                    
                    if matches:
                        rel_path = os.path.relpath(file_path, cobol_src_dir)
                        scn_usage[rel_path] = list(set(matches))  # Remove duplicates
                        
                except Exception as e:
                    print(f"Error reading {file_path}: {e}")
    
    return scn_usage

def main():
    cobol_src = "cobol_src"
    
    print("Scanning .CBL files for COPY statements with _SCN.CPY files...\n")
    
    scn_usage = find_scn_copy_statements(cobol_src)
    
    # Display results
    print(f"Found {len(scn_usage)} .CBL files that COPY _SCN.CPY files:\n")
    print("=" * 100)
    
    for cbll_file, scn_files in sorted(scn_usage.items()):
        print(f"\n{cbll_file}")
        print("-" * 100)
        for scn_file in sorted(scn_files):
            print(f"    COPY \"{scn_file}\"")
    
    # Reverse index: which _SCN files are used by which programs
    scn_to_programs = {}
    for cbl_file, scn_files in scn_usage.items():
        for scn_file in scn_files:
            if scn_file not in scn_to_programs:
                scn_to_programs[scn_file] = []
            scn_to_programs[scn_file].append(cbl_file)
    
    print("\n" + "=" * 100)
    print("\nMost commonly used _SCN.CPY files:")
    print("-" * 100)
    for scn_file, cbl_files in sorted(scn_to_programs.items(), key=lambda x: len(x[1]), reverse=True):
        print(f"{scn_file:50s} used by {len(cbl_files):2d} program(s)")
    
    # Check for LPCAMU specifically
    print("\n" + "=" * 100)
    print("\nLooking for LPCAMU_SCN.CPY specifically...")
    lpcamu_found = False
    for scn_file, cbl_files in scn_to_programs.items():
        if 'LPCAMU_SCN' in scn_file.upper():
            lpcamu_found = True
            print(f"  Found! '{scn_file}' is used by {len(cbl_files)} program(s):")
            for cbl in sorted(cbl_files):
                print(f"    {cbl}")
            break
    
    if not lpcamu_found:
        print("  ❌ LPCAMU_SCN.CPY is NOT referenced by any .CBL programs")
        print("  This confirms it's an orphaned/unused screen file.")
    
    print("\n" + "=" * 100)
    print(f"\nTotal .CBL programs using _SCN files: {len(scn_usage)}")
    print(f"Total unique _SCN files referenced: {len(scn_to_programs)}")

if __name__ == "__main__":
    main()
