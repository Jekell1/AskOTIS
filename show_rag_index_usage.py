"""
Show which indexes RAG uses for each question type
"""

print("="*80)
print("OTIS RAG INDEX USAGE ANALYSIS")
print("="*80)

# All 18 indexes available (from config.py)
all_indexes = {
    'code': 'code-chunks (167K docs, 3.8GB)',
    'code_new': 'new_code_chunks (84K docs, 3.7GB)',
    'files': 'new-cobol-files (10K docs)',
    'programs': 'new_cobol_program_meta (9.7K docs)',
    'paragraphs': 'new_cobol_paragraphs (225K docs)',
    'data_items': 'new_cobol_data_items (536K docs)',
    'copybooks': 'new_cobol_copybook_meta (7.8K docs)',
    'calls': 'new_cobol_calls (16K docs)',
    'variables': 'new_cobol_variable_usage (107K docs)',
    'copybook_usage': 'new_cobol_copybook_usage (115K docs)',
    'symbol_refs': 'new_cobol_symbol_refs (1.9M docs)',
    'name_aliases': 'new_cobol_name_aliases (56K docs)',
    'flows': 'new_cobol_program_flows (9.7K docs)',
    'flow_edges': 'new_cobol_flow_edges_v2 (385K docs)',
    'ui_paths': 'new_cobol_ui_paths (2K docs)',
    'menu_trees': 'new_cobol_menu_trees (9.7K docs)',
    'screen_nodes': 'new_cobol_screen_nodes (1.6K docs)',
    'program_deps': 'new_cobol_program_deps (9.7K docs)'
}

print("\n📊 ALL 18 AVAILABLE INDEXES:")
print("-" * 80)
for i, (key, name) in enumerate(all_indexes.items(), 1):
    print(f"  {i:2}. {key:20} → {name}")

# Index usage by question type (from router.py)
usage_by_type = {
    'explain_program': [
        'code', 'code_new', 'programs', 'flows', 'flow_edges', 
        'paragraphs', 'calls', 'program_deps', 'files'
    ],
    'find_code': [
        'code', 'code_new', 'programs', 'paragraphs', 'files'
    ],
    'explain_data': [
        'code', 'code_new', 'data_items', 'variables', 'copybooks',
        'copybook_usage', 'name_aliases'
    ],
    'trace_flow': [
        'code', 'code_new', 'flows', 'flow_edges', 'calls', 'ui_paths',
        'menu_trees', 'screen_nodes', 'program_deps'
    ],
    'general': [
        'code', 'code_new', 'programs', 'paragraphs', 'data_items',
        'flows', 'files', 'calls', 'program_deps'
    ]
}

print("\n" + "="*80)
print("🎯 INDEX USAGE BY QUESTION TYPE:")
print("="*80)

for q_type, indexes in usage_by_type.items():
    print(f"\n{q_type.upper().replace('_', ' ')}:")
    print("-" * 80)
    print(f"Uses {len(indexes)} indexes:")
    for idx in indexes:
        full_name = all_indexes.get(idx, idx)
        print(f"  • {idx:20} → {full_name}")

print("\n" + "="*80)
print("📈 INDEX USAGE STATISTICS:")
print("="*80)

# Count how often each index is used
usage_count = {}
for indexes in usage_by_type.values():
    for idx in indexes:
        usage_count[idx] = usage_count.get(idx, 0) + 1

# Sort by usage frequency
sorted_usage = sorted(usage_count.items(), key=lambda x: x[1], reverse=True)

print("\nMost Used Indexes:")
print("-" * 80)
for idx, count in sorted_usage:
    percentage = (count / 5) * 100  # 5 question types
    full_name = all_indexes.get(idx, idx)
    bar = "█" * int(percentage / 20)
    print(f"{idx:20} {count}/5 ({percentage:3.0f}%) {bar:6} {full_name}")

print("\n" + "="*80)
print("❌ UNUSED INDEXES:")
print("="*80)

used_indexes = set(usage_count.keys())
all_index_keys = set(all_indexes.keys())
unused = all_index_keys - used_indexes

if unused:
    print("\nThese indexes are NOT currently used by the RAG:")
    for idx in sorted(unused):
        full_name = all_indexes.get(idx, idx)
        print(f"  • {idx:20} → {full_name}")
    
    print(f"\n⚠️  {len(unused)}/{len(all_indexes)} indexes are configured but unused")
else:
    print("\n✅ All indexes are used!")

print("\n" + "="*80)
print("🚫 INTENTIONALLY EXCLUDED:")
print("="*80)
print("""
• symbol_refs (new_cobol_symbol_refs) - EXCLUDED
  Reason: Uses 1536-dimension embeddings (incompatible with text-embedding-3-large 3072d)
  Would require re-embedding with the current model to use
  
• enhanced_deps (enhanced_cobol_dependencies) - NOT USED YET
  Reason: Newly added, not yet integrated into router logic
  Should be added to question types that need comprehensive external references
""")

print("\n" + "="*80)
print("💡 SUMMARY:")
print("="*80)
print(f"""
Total Indexes Available: {len(all_indexes)}
Total Indexes Used: {len(used_indexes)}
Total Indexes Unused: {len(unused)}

The RAG intelligently selects indexes based on question type:
• Always includes: code, code_new (source code)
• Explain Program: +7 indexes (flows, calls, deps, metadata)
• Find Code: +3 indexes (programs, paragraphs, files)
• Explain Data: +5 indexes (data items, variables, copybooks)
• Trace Flow: +7 indexes (flows, menus, UI paths, screens)
• General: +7 indexes (broad coverage)

Maximum indexes per query: 9 (trace_flow type)
Minimum indexes per query: 5 (find_code type)

This selective approach balances:
✅ Comprehensive coverage (searches multiple relevant indexes)
✅ Performance (doesn't search all 18 indexes every time)
✅ Relevance (only searches indexes relevant to question type)
""")
