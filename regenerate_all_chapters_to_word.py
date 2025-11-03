#!/usr/bin/env python3
"""
Regenerate all chapter Word documents with proper table formatting
"""

import subprocess
import os
from pathlib import Path

def run_export_script(script_name):
    """Run a single export script and report status."""
    if not os.path.exists(script_name):
        print(f"⚠️  Skipping {script_name} (not found)")
        return False
    
    print(f"Processing {script_name}...")
    try:
        result = subprocess.run(['python', script_name], 
                              capture_output=True, 
                              text=True, 
                              timeout=120)
        if result.returncode == 0:
            print(f"✅ {script_name} completed")
            return True
        else:
            print(f"❌ {script_name} failed:")
            print(result.stderr)
            return False
    except subprocess.TimeoutExpired:
        print(f"❌ {script_name} timed out")
        return False
    except Exception as e:
        print(f"❌ {script_name} error: {e}")
        return False

def main():
    print("=" * 70)
    print("Regenerating All Chapter Word Documents with Proper Tables")
    print("=" * 70)
    print()
    
    # List all export scripts
    export_scripts = [
        'export_chapter1_to_word.py',
        'export_chapter2_to_word.py',
        'export_chapter3_to_word.py',
        'export_chapter4_to_word.py',
        'export_chapter5_to_word.py',
        'export_chapter6_to_word.py',
        'export_chapter7_to_word.py',
        'export_chapter8_to_word.py',
        'export_chapter9_to_word.py',
        'export_chapter10_to_word.py',
        'export_chapter11_to_word.py',
        'export_chapter12_to_word.py',
        'export_chapter13_to_word.py',
        'export_chapter14_to_word.py',
        'export_chapter15_to_word.py',
        'export_chapter16_to_word.py',
        'export_chapter17_to_word.py',
        'export_chapter18_to_word.py',
        'export_appendices_to_word.py',
    ]
    
    success_count = 0
    failed_count = 0
    skipped_count = 0
    
    for script in export_scripts:
        if os.path.exists(script):
            if run_export_script(script):
                success_count += 1
            else:
                failed_count += 1
        else:
            print(f"⚠️  Skipped {script} (not found)")
            skipped_count += 1
        print()
    
    print("=" * 70)
    print("Summary:")
    print(f"  ✅ Successful: {success_count}")
    print(f"  ❌ Failed: {failed_count}")
    print(f"  ⚠️  Skipped: {skipped_count}")
    print("=" * 70)
    
    # List generated Word files
    docx_files = list(Path('.').glob('Chapter*.docx'))
    if docx_files:
        print()
        print("Generated Word Documents:")
        for f in sorted(docx_files):
            size_kb = f.stat().st_size / 1024
            print(f"  📄 {f.name} ({size_kb:.1f} KB)")

if __name__ == "__main__":
    main()
