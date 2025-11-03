# COMPREHENSIVE INDEX STATUS REPORT
**Generated:** October 11, 2025  
**Total Indexes:** 24 (18 new + 6 legacy)  
**Total Documents:** 2,769,926

---

## EXECUTIVE SUMMARY

### By Tier Classification
| Tier | Indexes | Total Docs | Embeddings Complete | Source Coverage |
|------|---------|------------|---------------------|-----------------|
| **TIER-1** | 4 | 38,720 | 0% (0/4) | ✅ 100% (4/4) |
| **TIER-2** | 8 | 2,128,444 | 12.5% (1/8) | ⚠️ Partial |
| **TIER-3** | 5 | 90,476 | 0% (0/3*) | N/A |
| **LEGACY** | 6 | 302,286 | Unknown | N/A |
| **OTHER** | 1 | 210,000 | Unknown | N/A |

*Out of 3 indexes with vector fields

### Overall Health Metrics
- ✅ **Source Coverage Complete:** 4 indexes (TIER-1 only)
- ⚠️ **Source Coverage Partial:** 8 indexes (TIER-2)
- ✗ **Embeddings Missing:** 23 indexes need work
- ✅ **Embeddings Complete:** 1 index (new_cobol_screen_nodes)

---

## TIER-1: CORE PROGRAM METADATA

### Overall Status: ✅ SOURCE COVERAGE COMPLETE | ✗ EMBEDDINGS MISSING

| Index | Docs | Programs | Coverage | Vector Field | Embedded | Status |
|-------|------|----------|----------|--------------|----------|--------|
| **new_cobol_program_meta** | 9,678 | 9,678 | 100.0% | summary_vector | 0/9,678 | ✗ |
| **new_cobol_program_inventory** | 9,678 | 9,678 | 100.0% | None | N/A | ✅ |
| **new_cobol_program_deps** | 9,687 | 9,687 | 100.1% | dependency_blob_vector | 0/9,687 | ✗ |
| **new_cobol_program_flows** | 9,677 | 9,677 | 100.0% | flow_vector | 0/9,677 | ✗ |

#### Details

##### 📊 new_cobol_program_meta
- **Purpose:** Program summaries, metrics, and metadata
- **Documents:** 9,678 | **Fields:** 35 | **Vectors:** 1
- **Source Coverage:** ✅ 9,678/9,678 programs (100.0%)
- **Embeddings:** ✗ summary_vector: 0/9,678 (0.0%)
- **has_vector flag:** ✅ 9,678/9,678 (100.0%)
- **Action:** Backfill 9,678 summary_vector embeddings

##### 📦 new_cobol_program_inventory
- **Purpose:** Program inventory tracking
- **Documents:** 9,678 | **Fields:** 6 | **Vectors:** 0
- **Source Coverage:** ✅ 9,678/9,678 programs (100.0%)
- **Embeddings:** N/A (no vector fields)
- **Action:** ✅ COMPLETE

##### 🔗 new_cobol_program_deps
- **Purpose:** Program dependencies (calls in/out, copybooks)
- **Documents:** 9,687 | **Fields:** 13 | **Vectors:** 1
- **Source Coverage:** ✅ 9,687/9,678 programs (100.1%)
- **Embeddings:** ✗ dependency_blob_vector: 0/9,687 (0.0%)
- **has_vector flag:** ✅ 9,687/9,687 (100.0%)
- **Action:** Backfill 9,687 dependency_blob_vector embeddings

##### 🌊 new_cobol_program_flows
- **Purpose:** Program control flow graphs
- **Documents:** 9,677 | **Fields:** 18 | **Vectors:** 1
- **Source Coverage:** ✅ 9,677/9,678 programs (100.0%)
- **Embeddings:** ✗ flow_vector: 0/9,677 (0.0%)
- **has_vector flag:** ✅ 9,677/9,677 (100.0%)
- **Action:** Backfill 9,677 flow_vector embeddings

**TIER-1 SUMMARY:**
- ✅ All programs covered (100%)
- ✗ 29,042 embeddings needed across 3 indexes
- 🎯 Priority: HIGH (foundational metadata)

---

## TIER-2: PROGRAM INTERNALS

### Overall Status: ⚠️ PARTIAL COVERAGE | ✗ MOSTLY MISSING EMBEDDINGS

| Index | Docs | Programs | Coverage | Vector Field | Embedded | Status |
|-------|------|----------|----------|--------------|----------|--------|
| **new_cobol_paragraphs** | 224,655 | 2,267 | 23.4% | para_vector | 0/224,655 | ✗ |
| **new_cobol_program_copybook_edges** | 107,569 | 2,263 | 23.4% | None | N/A | ⚠️ |
| **new_cobol_screen_nodes** | 46,825 | 2,090 | 21.6% | summary_vector | 46,825/46,825 | ✅ |
| **new_cobol_copybook_usage** | 114,307 | 1,636 | 16.9% | context_vector | 0/114,307 | ✗ |
| **new_cobol_variable_usage** | 106,669 | 1,067 | 11.0% | None | N/A | ⚠️ |
| **new_cobol_data_items** | 267,769 | 791 | 8.2% | vector | 0/267,769 | ✗ |
| **new_cobol_flow_edges_v2** | 366,650 | 713 | 7.4% | edge_vector | 0/366,650 | ✗ |
| **new_cobol_symbol_refs** | 1,104,574 | 430 | 4.4% | excerpt_vector | 0/1,104,574 | ✗ |

#### Details

##### 📝 new_cobol_paragraphs
- **Purpose:** Paragraph definitions and code
- **Documents:** 224,655 | **Fields:** 14 | **Vectors:** 1
- **Source Coverage:** ⚠️ 2,267/9,678 programs (23.4%) - Missing 7,411
- **Embeddings:** ✗ para_vector: 0/224,655 (0.0%)
- **has_vector flag:** ✅ 224,655/224,655 (100.0%)
- **Action:** 1) Expand coverage to 7,411 programs 2) Backfill 224K+ embeddings

##### 🔗 new_cobol_program_copybook_edges
- **Purpose:** Program-to-copybook relationships
- **Documents:** 107,569 | **Fields:** 6 | **Vectors:** 0
- **Source Coverage:** ⚠️ 2,263/9,678 programs (23.4%) - Missing 7,415
- **Embeddings:** N/A (no vector fields)
- **Action:** Expand coverage to 7,415 programs

##### 🎨 new_cobol_screen_nodes
- **Purpose:** Screen/form definitions
- **Documents:** 46,825 | **Fields:** 17 | **Vectors:** 1
- **Source Coverage:** ⚠️ 2,090/9,678 programs (21.6%) - Missing 7,588
- **Embeddings:** ✅ summary_vector: 46,825/46,825 (100.0%)
- **has_vector flag:** ✅ 46,825/46,825 (100.0%)
- **Action:** Expand coverage to 7,588 programs (embeddings complete!)

##### 📋 new_cobol_copybook_usage
- **Purpose:** Copybook inclusion tracking
- **Documents:** 114,307 | **Fields:** 21 | **Vectors:** 1
- **Source Coverage:** ⚠️ 1,636/9,678 programs (16.9%) - Missing 8,042
- **Embeddings:** ✗ context_vector: 0/114,307 (0.0%)
- **has_vector flag:** ✅ 114,307/114,307 (100.0%)
- **Action:** 1) Expand coverage to 8,042 programs 2) Backfill 114K+ embeddings

##### 🔤 new_cobol_variable_usage
- **Purpose:** Variable usage patterns and statistics
- **Documents:** 106,669 | **Fields:** 17 | **Vectors:** 0
- **Source Coverage:** ⚠️ 1,067/9,678 programs (11.0%) - Missing 8,611
- **Embeddings:** N/A (no vector fields)
- **Action:** Expand coverage to 8,611 programs

##### 🗂️ new_cobol_data_items
- **Purpose:** Data structure definitions
- **Documents:** 267,769 | **Fields:** 21 | **Vectors:** 1
- **Source Coverage:** ⚠️ 791/9,678 programs (8.2%) - Missing 8,887
- **Embeddings:** ✗ vector: 0/267,769 (0.0%)
- **has_vector flag:** ✅ 267,769/267,769 (100.0%)
- **Action:** 1) Expand coverage to 8,887 programs 2) Backfill 267K+ embeddings

##### ⚡ new_cobol_flow_edges_v2
- **Purpose:** Control flow edges (perform, call, etc.)
- **Documents:** 366,650 | **Fields:** 13 | **Vectors:** 1
- **Source Coverage:** ⚠️ 713/9,678 programs (7.4%) - Missing 8,965
- **Embeddings:** ✗ edge_vector: 0/366,650 (0.0%)
- **has_vector flag:** ✅ 366,650/366,650 (100.0%)
- **Action:** 1) Expand coverage to 8,965 programs 2) Backfill 366K+ embeddings

##### 🔍 new_cobol_symbol_refs
- **Purpose:** Symbol references (1.1M+ references)
- **Documents:** 1,104,574 | **Fields:** 21 | **Vectors:** 1
- **Source Coverage:** ⚠️ 430/9,678 programs (4.4%) - Missing 9,248
- **Embeddings:** ✗ excerpt_vector: 0/1,104,574 (0.0%)
- **has_vector flag:** ✅ 1,104,574/1,104,574 (100.0%)
- **Action:** 1) Expand coverage to 9,248 programs 2) Backfill 1.1M+ embeddings

**TIER-2 SUMMARY:**
- ⚠️ Average coverage: 14.6% (needs ~85% expansion)
- ✗ 2,079,755 embeddings needed across 6 indexes
- 🎯 Priority: MEDIUM (detailed internals, large datasets)

---

## TIER-3: CROSS-PROGRAM ANALYSIS

### Overall Status: ⚪ NON-PROGRAM SCOPED | ✗ EMBEDDINGS MISSING

| Index | Docs | Programs | Coverage | Vector Field | Embedded | Status |
|-------|------|----------|----------|--------------|----------|--------|
| **new_cobol_calls** | 15,788 | 0 | 0.0% | snippet_vector | 0/15,788 | ✗ |
| **new_cobol_copybook_meta** | 7,817 | 0 | N/A | summary_vector | 0/7,817 | ✗ |
| **new_cobol_menu_trees** | 9,678 | 0 | N/A | None | N/A | ✅ |
| **new_cobol_ui_paths** | 1,437 | 0 | N/A | path_vector | 0/1,437 | ✗ |
| **new_cobol_name_aliases** | 55,636 | 0 | N/A | None | N/A | ✅ |

#### Details

##### 📞 new_cobol_calls
- **Purpose:** Cross-program CALL statements
- **Documents:** 15,788 | **Fields:** 15 | **Vectors:** 1
- **Source Coverage:** ✗ 0% (no program_id field - uses caller_program/callee_program)
- **Embeddings:** ✗ snippet_vector: 0/15,788 (0.0%)
- **has_vector flag:** ✅ 15,788/15,788 (100.0%)
- **Action:** 1) Add program_id field 2) Backfill 15,788 embeddings

##### 📚 new_cobol_copybook_meta
- **Purpose:** Copybook metadata summaries
- **Documents:** 7,817 | **Fields:** 7 | **Vectors:** 1
- **Source Coverage:** N/A (copybook-scoped, not program-scoped)
- **Embeddings:** ✗ summary_vector: 0/7,817 (0.0%)
- **has_vector flag:** ✅ 7,817/7,817 (100.0%)
- **Action:** Backfill 7,817 embeddings

##### 🌲 new_cobol_menu_trees
- **Purpose:** Menu navigation trees
- **Documents:** 9,678 | **Fields:** 9 | **Vectors:** 0
- **Source Coverage:** N/A (uses root_program_id)
- **Embeddings:** N/A (no vector fields by design)
- **Action:** ✅ COMPLETE

##### 🎨 new_cobol_ui_paths
- **Purpose:** UI navigation paths
- **Documents:** 1,437 | **Fields:** 35 | **Vectors:** 1
- **Source Coverage:** N/A (path-scoped)
- **Embeddings:** ✗ path_vector: 0/1,437 (0.0%)
- **has_vector flag:** ✅ 1,437/1,437 (100.0%)
- **Action:** Backfill 1,437 embeddings

##### 🏷️ new_cobol_name_aliases
- **Purpose:** Name normalization/alias resolution
- **Documents:** 55,636 | **Fields:** 9 | **Vectors:** 0
- **Source Coverage:** N/A (alias-scoped)
- **Embeddings:** N/A (no vector fields by design)
- **Action:** ✅ COMPLETE

**TIER-3 SUMMARY:**
- ⚪ Non-program scoped (relationship/aggregate data)
- ✗ 25,042 embeddings needed across 3 indexes
- 🎯 Priority: HIGH (smallest datasets, quick wins)

---

## LEGACY/OTHER INDEXES

### Status: ⚪ MAINTAINED SEPARATELY

| Index | Docs | Fields | Vectors | Status |
|-------|------|--------|---------|--------|
| **new-cobol-files** | 9,956 | 21 | 2 | Unknown |
| **cobol-facts-v3** | 31 | 10 | 1 | Unknown |
| **cobol-facts-v3l** | 31 | 10 | 1 | Unknown |
| **cobol-symbols** | 41,550 | 20 | 1 | Unknown |
| **cobol-xrefs** | 63,405 | 14 | 1 | Unknown |
| **code-chunks** | 102,654 | 11 | 1 | Unknown |
| **new_code_chunks** | 84,205 | 16 | 1 | Unknown |

#### Details

##### 📁 new-cobol-files
- **Purpose:** Original file ingestion index
- **Documents:** 9,956 | **Fields:** 21 | **Vectors:** 2
- **Note:** Being replaced by new_cobol_* indexes
- **Action:** Monitor, possibly deprecate

##### 🧪 cobol-facts-v3 / cobol-facts-v3l
- **Purpose:** Extracted facts/rules (test/experimental)
- **Documents:** 31 each | **Vectors:** fact_vector
- **Note:** Small experimental datasets
- **Action:** Keep for fact extraction R&D

##### 🏷️ cobol-symbols
- **Purpose:** Symbol definitions (legacy)
- **Documents:** 41,550 | **Vectors:** name_vector
- **Note:** Being replaced by new_cobol_data_items + symbol_refs
- **Action:** Maintain until migration complete

##### 🔗 cobol-xrefs
- **Purpose:** Cross-references (legacy)
- **Documents:** 63,405 | **Vectors:** snippet_vector
- **Note:** Being replaced by new_cobol_symbol_refs
- **Action:** Maintain until migration complete

##### 📄 code-chunks / new_code_chunks
- **Purpose:** Code chunk embeddings for RAG
- **Documents:** 102,654 / 84,205 | **Vectors:** text_vector
- **Note:** Complementary to new_cobol_* indexes
- **Action:** Maintain both for different chunk strategies

**LEGACY SUMMARY:**
- ⚪ Maintained for compatibility/migration
- 🔍 Needs separate analysis for embedding status
- 🎯 Priority: LOW (not part of new architecture)

---

## COMPREHENSIVE EMBEDDING BACKLOG

### Summary by Priority

| Priority | Indexes | Records | Estimated Time |
|----------|---------|---------|----------------|
| 🔴 **IMMEDIATE** | 3 | 18,932 | 1-2 hours |
| 🟠 **HIGH** | 3 | 29,042 | 2-3 hours |
| 🟡 **MEDIUM** | 6 | 2,079,755 | 15-20 hours |
| **TOTAL** | 12 | 2,127,729 | ~25 hours |

### Immediate Priority (Quick Wins)
1. **new_cobol_ui_paths:** 1,437 embeddings (path_vector)
2. **new_cobol_copybook_meta:** 7,817 embeddings (summary_vector)
3. **new_cobol_program_meta:** 9,678 embeddings (summary_vector)

### High Priority (TIER-1 Foundation)
4. **new_cobol_program_deps:** 9,687 embeddings (dependency_blob_vector)
5. **new_cobol_program_flows:** 9,677 embeddings (flow_vector)
6. **new_cobol_calls:** 15,788 embeddings (snippet_vector) - after schema fix

### Medium Priority (Large Datasets)
7. **new_cobol_copybook_usage:** 114,307 embeddings (context_vector)
8. **new_cobol_paragraphs:** 224,655 embeddings (para_vector)
9. **new_cobol_data_items:** 267,769 embeddings (vector)
10. **new_cobol_flow_edges_v2:** 366,650 embeddings (edge_vector)
11. **new_cobol_symbol_refs:** 1,104,574 embeddings (excerpt_vector)

---

## SOURCE COVERAGE EXPANSION NEEDED

### Programs Missing from TIER-2 Indexes

| Index | Current | Missing | Target | % Gap |
|-------|---------|---------|--------|-------|
| **new_cobol_symbol_refs** | 430 | 9,248 | 9,678 | 95.6% |
| **new_cobol_flow_edges_v2** | 713 | 8,965 | 9,678 | 92.6% |
| **new_cobol_data_items** | 791 | 8,887 | 9,678 | 91.8% |
| **new_cobol_variable_usage** | 1,067 | 8,611 | 9,678 | 89.0% |
| **new_cobol_copybook_usage** | 1,636 | 8,042 | 9,678 | 83.1% |
| **new_cobol_screen_nodes** | 2,090 | 7,588 | 9,678 | 78.4% |
| **new_cobol_program_copybook_edges** | 2,263 | 7,415 | 9,678 | 76.6% |
| **new_cobol_paragraphs** | 2,267 | 7,411 | 9,678 | 76.6% |

**Total Programs Needing Expansion:** ~7,000-9,000 per index

---

## RECOMMENDED ACTION PLAN

### Week 1: Quick Wins + TIER-1 Foundation
**Goal:** Complete small datasets + establish TIER-1 baseline

1. ✅ **Day 1-2:** TIER-3 Small Datasets
   - Run `backfill_ui_paths_vectors.py` (1,437 records)
   - Run `backfill_copybook_meta_vectors.py` (7,817 records)
   
2. ✅ **Day 3-4:** TIER-1 Core Metadata
   - Run `backfill_program_meta_vectors.py` (9,678 records)
   - Run `backfill_program_deps_vectors.py` (9,687 records)
   - Run `backfill_program_flows_vectors.py` (9,677 records)

3. 🔧 **Day 5:** Schema Fixes
   - Fix `new_cobol_calls` program_id issue
   - Run `backfill_calls_vectors.py` (15,788 records)

**Total Week 1:** ~54,000 embeddings | 4-6 hours processing

### Week 2: Medium Datasets (TIER-2 Start)
**Goal:** Tackle manageable TIER-2 indexes

4. ⚡ **Day 1-2:** Copybook Usage
   - Run `backfill_copybook_usage_vectors.py` (114,307 records)

5. ⚡ **Day 3-4:** Paragraphs
   - Run `backfill_paragraphs_vectors.py` (224,655 records)

**Total Week 2:** ~339,000 embeddings | 3-4 hours processing

### Week 3-4: Large Datasets (TIER-2 Completion)
**Goal:** Complete largest embedding jobs

6. 🚀 **Phase 1:** Data Items
   - Run `backfill_data_items_vectors.py` (267,769 records, batched)

7. 🚀 **Phase 2:** Flow Edges
   - Run `backfill_flow_edges_v2_vectors.py` (366,650 records, batched)

8. 🚀 **Phase 3:** Symbol Refs
   - Run `backfill_symbol_refs_vectors.py` (1,104,574 records, batched)

**Total Week 3-4:** ~1.7M embeddings | 12-15 hours processing

### Ongoing: Source Coverage Expansion
**Goal:** Bring all TIER-2 indexes to 100% program coverage

- Identify missing programs for each index
- Re-run extraction pipelines for missing programs
- Validate data quality and completeness

---

## SUCCESS METRICS

### Phase 1 Complete (Week 1)
- [ ] All TIER-1 indexes: 100% embedded
- [ ] All TIER-3 indexes: 100% embedded
- [ ] Total: 54,000+ embeddings complete
- [ ] Vector search functional for program metadata

### Phase 2 Complete (Week 2)
- [ ] Medium TIER-2 indexes: 100% embedded
- [ ] Total: 393,000+ embeddings complete
- [ ] Vector search functional for copybooks + paragraphs

### Phase 3 Complete (Week 4)
- [ ] All TIER-2 indexes: 100% embedded
- [ ] Total: 2,127,729+ embeddings complete
- [ ] Full vector search capability across all indexes

### Final Goal
- [ ] All 24 indexes analyzed and optimized
- [ ] All embeddings complete
- [ ] Source coverage expansion roadmap defined
- [ ] System ready for production queries

---

## STORAGE & PERFORMANCE METRICS

### Current State
- **Total Documents:** 2,769,926
- **Storage Used:** 64.8 GB / 172 GB (37.7%)
- **Vector Storage:** 25.5 GB / 37.6 GB (67.8%)
- **Indexes:** 24 / 50 (48%)

### After Full Embedding Completion
- **Estimated Vector Storage:** ~35-37 GB (approaching limit)
- **Recommendation:** Monitor vector storage; may need to upgrade tier

---

*End of Comprehensive Index Status Report*
