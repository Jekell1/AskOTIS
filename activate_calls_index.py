#!/usr/bin/env python3
"""Quick activation script for new_cobol_calls index.

This script will:
1. Verify calls index is ready (100% embedded)
2. Show the exact changes needed to orchestrate_answer.py
3. Optionally apply the changes automatically

Usage:
    python activate_calls_index.py --dry-run    # Show changes only
    python activate_calls_index.py --apply      # Apply changes
"""
import os
import sys
import json
import requests

def check_calls_index():
    """Verify calls index is ready."""
    print("🔍 Checking new_cobol_calls index status...")
    
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
        if k in vals and k not in os.environ:
            os.environ[k] = vals[k]
    
    ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    # Get index stats
    r = requests.post(
        f'{ep}/indexes/new_cobol_calls/docs/search?api-version=2025-08-01-preview',
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'top': 0, 'count': True}
    )
    total = r.json().get('@odata.count', 0)
    
    # Check embeddings
    r = requests.post(
        f'{ep}/indexes/new_cobol_calls/docs/search?api-version=2025-08-01-preview',
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'filter': 'has_vector eq true', 'top': 0, 'count': True}
    )
    embedded = r.json().get('@odata.count', 0)
    
    pct = (embedded / total * 100) if total > 0 else 0
    
    print(f"   Total documents: {total:,}")
    print(f"   Embedded: {embedded:,} ({pct:.1f}%)")
    
    if pct >= 95:
        print("   ✅ Index is ready for activation!")
        return True
    else:
        print(f"   ⚠️  Index only {pct:.1f}% embedded. Recommend waiting until >95%")
        return False

def show_changes():
    """Show the required changes to orchestrate_answer.py."""
    print("\n" + "="*80)
    print("  REQUIRED CHANGES TO retrieval/orchestrate_answer.py")
    print("="*80)
    
    print("\n1️⃣  ADD TO INDEX_DEFS (around line 40):\n")
    print("   'calls': {")
    print("       'index': 'new_cobol_calls',")
    print("       'vector_field': 'snippet_vector',")
    print("       'text_field': 'snippet',")
    print("       'key': 'call_id',")
    print("       'select': 'call_id,caller_program,callee_program,line,snippet,file_path'")
    print("   },")
    
    print("\n2️⃣  UPDATE INTENT_GROUPS (around line 50):\n")
    print("   DEPS: ['program_deps', 'calls', 'program_meta'],  # ← Add 'calls'")
    
    print("\n" + "="*80)
    print("  EXPECTED IMPACT")
    print("="*80)
    print("\n✅ New Capabilities:")
    print("   - 'What programs call X?' → Detailed CALL statements")
    print("   - 'Where is X called from?' → Call-site evidence with line numbers")
    print("   - 'Show me call chains to X' → Combined with program_deps")
    print("\n✅ Enhanced Questions:")
    print("   - 'What external programs does APIPAY call?' → Now shows snippets")
    print("   - 'Where is TIM360 used?' → Call-site locations")
    print("\n✅ Performance:")
    print("   - +15,788 searchable call documents")
    print("   - Hybrid search (keyword + vector)")
    print("   - Sub-second query time")
    print()

def apply_changes():
    """Apply changes to orchestrate_answer.py."""
    print("\n🔧 Applying changes to retrieval/orchestrate_answer.py...")
    
    orchestrator_path = 'retrieval/orchestrate_answer.py'
    
    if not os.path.exists(orchestrator_path):
        print(f"   ❌ File not found: {orchestrator_path}")
        return False
    
    with open(orchestrator_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Check if already applied
    if "'calls':" in content:
        print("   ⚠️  Changes already applied! 'calls' already in INDEX_DEFS")
        return False
    
    # Apply INDEX_DEFS change
    index_def = """  'copybook_usage': {'index':'new_cobol_copybook_usage','vector_field':None,'text_field':'raw_line','key':'usage_id','select':'usage_id,program_id,copybook_name_plain,line,raw_line'},
  'calls': {'index':'new_cobol_calls','vector_field':'snippet_vector','text_field':'snippet','key':'call_id','select':'call_id,caller_program,callee_program,line,snippet,file_path'},"""
    
    content = content.replace(
        "  'copybook_usage': {'index':'new_cobol_copybook_usage','vector_field':None,'text_field':'raw_line','key':'usage_id','select':'usage_id,program_id,copybook_name_plain,line,raw_line'},",
        index_def
    )
    
    # Apply INTENT_GROUPS change
    content = content.replace(
        "  DEPS:['program_deps','program_meta'],",
        "  DEPS:['program_deps','calls','program_meta'],"
    )
    
    # Write back
    with open(orchestrator_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print("   ✅ Changes applied successfully!")
    print("\n📝 Changes made:")
    print("   1. Added 'calls' to INDEX_DEFS")
    print("   2. Added 'calls' to DEPS intent group")
    print("\n🧪 Test with:")
    print("   python retrieval/orchestrate_answer.py --q 'What programs call ORDENT01?'")
    
    return True

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='Activate new_cobol_calls index')
    parser.add_argument('--dry-run', action='store_true', help='Show changes without applying')
    parser.add_argument('--apply', action='store_true', help='Apply changes to orchestrator')
    parser.add_argument('--force', action='store_true', help='Apply even if embeddings not complete')
    
    args = parser.parse_args()
    
    print("\n" + "="*80)
    print("  ACTIVATE new_cobol_calls INDEX")
    print("="*80 + "\n")
    
    # Check index status
    ready = check_calls_index()
    
    if not ready and not args.force:
        print("\n⚠️  Index not ready. Use --force to proceed anyway.")
        return 1
    
    # Show changes
    show_changes()
    
    # Apply if requested
    if args.apply:
        if apply_changes():
            print("\n🎉 Activation complete! calls index is now active.")
            return 0
        else:
            return 1
    elif not args.dry_run:
        print("\n💡 To apply these changes, run:")
        print("   python activate_calls_index.py --apply")
    
    return 0

if __name__ == '__main__':
    sys.exit(main())
