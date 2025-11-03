#!/usr/bin/env python3
"""Monitor ingestion process in real-time and alert on errors."""

import subprocess
import sys
import re
from datetime import datetime

def monitor_ingestion(command):
    """Run ingestion and monitor for errors in real-time."""
    
    print("=" * 80)
    print("INGESTION MONITOR - Real-time Error Detection")
    print("=" * 80)
    print(f"Command: {command}")
    print(f"Started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 80)
    print()
    
    # Counters
    errors = []
    warnings = []
    successes = 0
    total_uploaded = 0
    total_failed = 0
    
    # Start process
    process = subprocess.Popen(
        command,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1
    )
    
    # Monitor output line by line
    for line in process.stdout:
        # Print the line
        print(line, end='')
        
        # Check for errors
        if '❌' in line or '[ERROR]' in line or 'ERROR' in line.upper():
            errors.append(line.strip())
            print(f"\n{'!'*80}")
            print(f"🚨 ERROR DETECTED: {line.strip()}")
            print(f"{'!'*80}\n")
        
        # Check for warnings
        if '⚠️' in line or '[WARNING]' in line or 'WARNING' in line.upper():
            warnings.append(line.strip())
        
        # Track upload progress
        if '✅' in line and 'success' in line:
            match = re.search(r'✅ (\d+) success', line)
            if match:
                total_uploaded = int(match.group(1))
        
        # Track failures
        if '❌' in line and 'failed' in line:
            match = re.search(r'❌ (\d+) failed', line)
            if match:
                total_failed = int(match.group(1))
        
        # Check for completion
        if 'Code chunks streaming ingestion complete' in line:
            successes += 1
    
    # Wait for process to complete
    return_code = process.wait()
    
    # Print summary
    print("\n" + "=" * 80)
    print("INGESTION MONITOR SUMMARY")
    print("=" * 80)
    print(f"Completed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Exit Code: {return_code}")
    print()
    
    if return_code == 0:
        print("✅ Process completed successfully")
    else:
        print(f"❌ Process exited with error code: {return_code}")
    
    print()
    print(f"📊 Statistics:")
    print(f"   Documents uploaded: {total_uploaded}")
    if total_failed > 0:
        print(f"   Documents failed: {total_failed}")
    print(f"   Errors detected: {len(errors)}")
    print(f"   Warnings detected: {len(warnings)}")
    
    if errors:
        print()
        print("🚨 ERRORS:")
        for i, err in enumerate(errors[:10], 1):
            print(f"   {i}. {err}")
        if len(errors) > 10:
            print(f"   ... and {len(errors) - 10} more errors")
    
    if warnings:
        print()
        print("⚠️  WARNINGS:")
        for i, warn in enumerate(warnings[:5], 1):
            print(f"   {i}. {warn}")
        if len(warnings) > 5:
            print(f"   ... and {len(warnings) - 5} more warnings")
    
    print("=" * 80)
    
    # Alert if issues found
    if errors or total_failed > 0:
        print()
        print("🚨" * 40)
        print("⚠️  ALERT: Issues detected during ingestion!")
        print("⚠️  The index may be incomplete or corrupted!")
        print("⚠️  Review errors above before proceeding!")
        print("🚨" * 40)
        return 1
    
    return return_code

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python monitor_ingestion.py <command>")
        print()
        print("Examples:")
        print("  python monitor_ingestion.py 'python ingest_code_chunks.py --include-copybooks --verbose'")
        print("  python monitor_ingestion.py 'python ingest_code_chunks.py --include-copybooks --limit 5000 --verbose'")
        sys.exit(1)
    
    command = ' '.join(sys.argv[1:])
    exit_code = monitor_ingestion(command)
    sys.exit(exit_code)
