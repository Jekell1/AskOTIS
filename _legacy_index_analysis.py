"""
AZURE COGNITIVE SEARCH INDEX ANALYSIS
======================================

LEGACY INDEXES (6 total - 333,876 docs):

1. **cobol-facts-v3** (31 docs) - OBSOLETE?
   - Very small dataset
   - Has a "v3l" variant (duplicate?)
   - Likely superseded by structured indexes

2. **cobol-facts-v3l** (31 docs) - OBSOLETE?
   - Duplicate of cobol-facts-v3
   - "l" suffix unclear (large? legacy?)
   
3. **cobol-symbols** (41,550 docs) - CHECK OVERLAP
   - Potential overlap with: new_cobol_name_aliases (55,636)
   - Or: new_cobol_symbol_refs (1,104,574)
   - Need to verify if data is included in new indexes

4. **cobol-xrefs** (63,405 docs) - CHECK OVERLAP
   - Cross-references
   - Potential overlap with: new_cobol_symbol_refs (1,104,574)
   - Verify if superseded

5. **code-chunks** (102,654 docs) - REPLACED
   - Superseded by: new_code_chunks (84,205)
   - Old version, can likely be deleted

6. **new_code_chunks** (84,205 docs) - KEEP?
   - Newer version but not "new_cobol_" prefix
   - May be from different extraction run
   - Check if overlaps with new-cobol-files (9,956)

RECOMMENDATION:

DELETE CANDIDATES (safe to remove):
- cobol-facts-v3 (31 docs)
- cobol-facts-v3l (31 docs)  
- code-chunks (102,654 docs) - superseded by new_code_chunks

VERIFY BEFORE DELETING (check for overlap):
- cobol-symbols (41,550) - verify against new_cobol_name_aliases/symbol_refs
- cobol-xrefs (63,405) - verify against new_cobol_symbol_refs
- new_code_chunks (84,205) - verify against new-cobol-files

SPACE SAVINGS: ~265K documents (cobol-facts-v3/v3l + code-chunks)

POTENTIAL TOTAL CLEANUP: Up to 333K documents if all legacy confirmed obsolete
"""

print(__doc__)
