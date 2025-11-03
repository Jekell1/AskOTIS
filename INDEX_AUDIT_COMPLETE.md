# Complete Index Audit - Usage & Recommendations

**Date:** October 12, 2025  
**Total Indexes:** 24 (20 with vectors, 4 keyword-only)

---

## 📊 Summary by Category

| Category | Count | Documents | With Vectors |
|----------|-------|-----------|--------------|
| ✅ **Active (in use)** | 9 | 1,496,880 | 9 |
| ❓ **Unused but valuable** | 8 | 1,355,069 | 6 |
| 🔴 **Legacy (delete)** | 4 | 105,017 | 4 |
| ❔ **Unknown** | 3 | 196,815 | 2 |

---

## ✅ ACTIVE INDEXES (9) - Currently Used

| Index | Docs | Embeddings | Vectors | Purpose |
|-------|------|------------|---------|---------|
| new_cobol_copybook_usage | 115,399 | 100% | ✅ context_vector | COPY statement analysis |
| new_cobol_paragraphs | 224,655 | 100% | ✅ para_vector | Program structure |
| new_cobol_data_items | 536,058 | 100% | ✅ vector | DATA DIVISION items |
| new_cobol_flow_edges_v2 | 385,121 | 95% | ✅ edge_vector | Control flow edges |
| new_cobol_screen_nodes | 1,609 | 100% | ✅ summary_vector | UI screens |
| new_cobol_variable_usage | 106,669 | 9% | ✅ usage_summary_vector | Variable stats |
| new_cobol_program_flows | 9,677 | 100% | ✅ flow_vector | Flow documentation |
| new_cobol_ui_paths | 2,014 | 100% | ✅ path_vector | User paths |
| new_cobol_program_meta | 9,678 | 100% | ✅ summary_vector | Program metadata |

**Subtotal:** 1,496,880 docs, all have vector capabilities

---

## ❓ UNUSED BUT POTENTIALLY VALUABLE (8)

### 🎯 HIGH VALUE - Recommend Activating

| Index | Docs | Vectors | Embeddings | Chatbot Value |
|-------|------|---------|------------|---------------|
| **new_cobol_symbol_refs** | 1,104,574 | ✅ excerpt_vector | Has has_vector | **HIGH** - "Where is X used?" queries |
| **new_cobol_calls** | 15,788 | ✅ snippet_vector | Has has_vector | **HIGH** - "What calls X?" queries |
| **new_cobol_copybook_meta** | 7,817 | ✅ summary_vector | Has has_vector | **MEDIUM** - May overlap with copybook_usage |
| **new_cobol_program_deps** | 9,687 | ✅ dependency_blob_vector | Has has_vector | **MEDIUM** - May overlap with program_flows |

**Subtotal:** 1,137,866 docs with semantic search ready

### 📋 KEYWORD-ONLY - No Semantic Search

| Index | Docs | Vectors | Purpose |
|-------|------|---------|---------|
| new_cobol_menu_trees | 9,678 | ❌ | Menu hierarchies (may overlap with ui_paths) |
| new_cobol_name_aliases | 55,636 | ❌ | Name normalization/mapping |
| new_cobol_program_copybook_edges | 107,569 | ❌ | Program-copybook edges |
| new_cobol_program_inventory | 9,678 | ❌ | Program inventory (may overlap with program_meta) |

**Subtotal:** 182,561 docs, keyword search only

### 💡 Activation Assessment

**symbol_refs (1.1M docs):**
- ✅ Has vectors and has_vector tracking
- ✅ Would enable "find all usages of variable/program X"
- ⚠️ Need to check embedding completion %
- **Recommendation:** CHECK EMBEDDINGS, likely activate

**calls (15.8k docs):**
- ✅ Has vectors and has_vector tracking  
- ✅ Would enable "show all calls to program X"
- ⚠️ Need to check embedding completion %
- **Recommendation:** CHECK EMBEDDINGS, likely activate

**copybook_meta (7.8k docs):**
- ✅ Has vectors
- ❓ May duplicate copybook_usage functionality
- **Recommendation:** Compare with copybook_usage, keep better one

**program_deps (9.7k docs):**
- ✅ Has vectors
- ❓ May duplicate program_flows functionality
- **Recommendation:** Compare with program_flows, keep better one

**menu_trees, program_inventory, name_aliases, program_copybook_edges:**
- ❌ No vectors = No semantic search
- ❓ May have overlapping data with vector-enabled indexes
- **Recommendation:** Evaluate if keyword-only search provides unique value

---

## 🔴 LEGACY INDEXES (4) - Recommend Deletion

| Index | Docs | Vectors | Superseded By |
|-------|------|---------|---------------|
| cobol-facts-v3 | 31 | ✅ fact_vector | new_cobol_* indexes |
| cobol-facts-v3l | 31 | ✅ fact_vector | new_cobol_* indexes |
| cobol-symbols | 41,550 | ✅ name_vector | new_cobol_symbol_refs (1.1M) |
| cobol-xrefs | 63,405 | ✅ snippet_vector | Multiple new_cobol_* indexes |

**Impact of deletion:**
- Remove 4 indexes
- Reclaim ~105k documents
- All functionality covered by newer indexes
- **SAFE TO DELETE**

---

## ❔ UNKNOWN PURPOSE (3)

| Index | Docs | Vectors | Investigation Needed |
|-------|------|---------|---------------------|
| code-chunks | 102,654 | ✅ text_vector | Check vs new_code_chunks |
| new-cobol-files | 9,956 | ✅ contentVector | Check vs program_meta |
| new_code_chunks | 84,205 | ✅ text_vector | Check vs code-chunks |

**Recommendations:**
1. **code-chunks vs new_code_chunks:** Likely old/new versions, keep ONE
2. **new-cobol-files:** May be superseded by program_meta

---

## 🎯 ACTION PLAN

### Phase 1: Immediate (Safe Deletions)
Delete 4 legacy indexes that are definitively superseded:
```bash
# Safe to delete - old versions
- cobol-facts-v3
- cobol-facts-v3l
- cobol-symbols
- cobol-xrefs
```

### Phase 2: Investigation (This Week)

**Check embedding status:**
```bash
python check_all_indexes_quick.py  # Add symbol_refs and calls to the list
```

**Compare overlapping indexes:**
1. copybook_meta vs copybook_usage
2. program_deps vs program_flows  
3. program_inventory vs program_meta
4. menu_trees vs ui_paths
5. code-chunks vs new_code_chunks
6. new-cobol-files vs program_meta

### Phase 3: Activation (If embeddings complete)

**High priority to activate:**
- new_cobol_symbol_refs (1.1M docs) - IF embeddings >90%
- new_cobol_calls (15.8k docs) - IF embeddings >90%

**Benefits:**
- Answer "where is X used?" questions
- Answer "what calls program Y?" questions
- Complete cross-reference capabilities

### Phase 4: Consolidation (After comparison)

Keep only one index when duplicates found:
- Best data quality
- Most complete embeddings
- Better maintained build scripts

---

## 📈 Storage Optimization

**Current:**
- 24 indexes
- ~3.3M documents total

**After Phase 1 (delete 4 legacy):**
- 20 indexes
- ~3.2M documents
- Savings: 4 indexes, ~105k docs

**After Phase 4 (consolidate duplicates):**
- Estimated: 13-15 indexes (remove ~5-7 duplicates)
- Could save significant storage/compute

---

## 🔍 Next Steps

Run these checks to make informed decisions:

```bash
# 1. Check embedding status for unused indexes
python check_all_indexes_quick.py  # Add symbol_refs, calls, etc.

# 2. Compare schema and data quality
python compare_index_schemas.py copybook_meta copybook_usage
python compare_index_schemas.py program_deps program_flows

# 3. Verify which scripts actively use each index
grep -r "new_cobol_symbol_refs" ingest/ search/ backfill*
grep -r "new_cobol_calls" ingest/ search/ backfill*
```

Would you like me to:
1. Check embedding status for symbol_refs and calls?
2. Create comparison scripts for overlapping indexes?
3. Generate deletion scripts for legacy indexes?
