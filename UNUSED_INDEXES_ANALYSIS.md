# Azure Search Index Analysis - Unused Indexes

**Date:** October 12, 2025  
**Total Indexes:** 24

## 📊 Index Categories

### ✅ Active Indexes (9) - In Use for Chatbot
| Index | Documents | Embeddings | Purpose |
|-------|-----------|------------|---------|
| new_cobol_copybook_usage | 115,399 | 100% | COPY statement analysis |
| new_cobol_paragraphs | 224,655 | 100% | Program structure |
| new_cobol_data_items | 536,058 | 100% | DATA DIVISION variables |
| new_cobol_flow_edges_v2 | 385,121 | 95.4% | Control flow edges |
| new_cobol_screen_nodes | 1,609 | 100% | SCREEN SECTION UI |
| new_cobol_variable_usage | 106,669 | 9.0% | Variable usage stats |
| new_cobol_program_flows | 9,677 | 100% | Program flow docs |
| new_cobol_ui_paths | 2,014 | 100% | User interaction paths |
| new_cobol_program_meta | 9,678 | 100% | Program metadata |

**Total Active:** 1,496,880 documents

---

## ❓ Potentially Useful Indexes (8)

These indexes exist and have data but are not currently being used by the chatbot. They may have value:

### High Value - Consider Activating

| Index | Documents | Potential Use |
|-------|-----------|---------------|
| **new_cobol_symbol_refs** | 1,104,574 | Symbol cross-references - useful for "where is X used?" queries |
| **new_cobol_calls** | 15,788 | CALL relationships - useful for program dependency queries |
| **new_cobol_program_deps** | 9,687 | Program dependencies - overlaps with program_flows but may have different structure |
| **new_cobol_name_aliases** | 55,636 | Name normalization - useful for handling COBOL naming variations |

### Medium Value - Evaluate Need

| Index | Documents | Potential Use |
|-------|-----------|---------------|
| **new_cobol_copybook_meta** | 7,817 | Copybook metadata - may overlap with copybook_usage |
| **new_cobol_program_inventory** | 9,678 | Program inventory - may overlap with program_meta |
| **new_cobol_menu_trees** | 9,678 | Menu hierarchies - may overlap with ui_paths |
| **new_cobol_program_copybook_edges** | 107,569 | Program-copybook relationships - detailed edge data |

---

## 🔴 Legacy Indexes (4) - Recommend Deletion

These are old indexes from previous version, likely superseded by "new_cobol_*" versions:

| Index | Documents | Recommendation |
|-------|-----------|----------------|
| cobol-facts-v3 | 31 | DELETE - superseded by new indexes |
| cobol-facts-v3l | 31 | DELETE - superseded by new indexes |
| cobol-symbols | 41,550 | DELETE - replaced by new_cobol_symbol_refs (1.1M docs) |
| cobol-xrefs | 63,405 | DELETE - data now in multiple new_cobol_* indexes |

**Can reclaim:** ~105,017 documents

---

## ❔ Unknown Purpose (3)

| Index | Documents | Notes |
|-------|-----------|-------|
| code-chunks | 102,654 | Old version? Check if superseded by new_code_chunks |
| new-cobol-files | 9,956 | File metadata? May overlap with program_meta |
| new_code_chunks | 84,205 | Newer version of code-chunks? |

**Recommendation:** Investigate which is current between code-chunks and new_code_chunks

---

## 💡 Recommendations

### Immediate Actions (Safe)
1. **DELETE 4 legacy indexes:** Save resources, no loss of functionality
   - cobol-facts-v3
   - cobol-facts-v3l  
   - cobol-symbols
   - cobol-xrefs

### Short-term (After Investigation)
2. **Investigate code-chunks vs new_code_chunks:** Keep one, delete the other
3. **Check new-cobol-files:** Determine if needed or overlaps with program_meta

### Medium-term (For Chatbot Enhancement)
4. **Consider activating:**
   - **new_cobol_symbol_refs** (1.1M docs) - High value for "find usage" queries
   - **new_cobol_calls** (15.8k docs) - High value for dependency queries
   
5. **Evaluate if needed:**
   - new_cobol_program_deps (may overlap with program_flows)
   - new_cobol_copybook_meta (may overlap with copybook_usage)
   - new_cobol_menu_trees (may overlap with ui_paths)
   - new_cobol_program_inventory (may overlap with program_meta)

### Long-term
6. **Consolidate duplicates:** Once confirmed which indexes overlap, keep the better one

---

## 📈 Storage Impact

**Current Total:** 24 indexes, ~3.3M documents

**After cleanup (deleting 4 legacy):**
- 20 indexes, ~3.2M documents
- Savings: 4 indexes, ~105k documents

**After full consolidation (if all overlaps removed):**
- Estimated: 15-18 indexes
- Could save 6-9 indexes worth of storage/compute

---

## 🎯 Usage Validation Methods

To determine if "unused" indexes should be kept:

1. **Check build scripts:** Search for index name in `ingest/` and `search/` folders
2. **Check backfill scripts:** Search for index name in `backfill_*.py` files
3. **Check dependencies:** Some indexes may feed data into others
4. **Check session questions:** Do the 18 chatbot questions need this data?

Example questions that might need "unused" indexes:
- "Where is variable X used?" → **new_cobol_symbol_refs**
- "What programs call Y?" → **new_cobol_calls**
- "Show me program dependencies" → **new_cobol_program_deps**
- "What copybooks does X use?" → **new_cobol_program_copybook_edges**
