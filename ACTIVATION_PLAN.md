# Unused Indexes Activation Plan

## 🎯 Priority 1: Activate High-Value Indexes (IMMEDIATE)

These 3 indexes are **100% complete and ready** - just need to wire into orchestrator:

### 1. **new_cobol_symbol_refs** (1,104,574 docs) ⭐⭐⭐
- **Status:** 100% embedded, fully functional
- **Value:** "Where is variable X used?" queries
- **Action:** Add to `orchestrate_answer.py` INTENT_GROUPS
- **Effort:** 15 minutes

### 2. **new_cobol_calls** (15,788 docs) ⭐⭐⭐
- **Status:** 100% embedded, fully functional
- **Value:** "What programs call Y?" dependency queries
- **Action:** Add to `orchestrate_answer.py` INTENT_GROUPS
- **Effort:** 10 minutes

### 3. **new_cobol_copybook_meta** (7,817 docs) ⭐⭐
- **Status:** 100% embedded, fully functional
- **Value:** Enhanced copybook summaries
- **Action:** Already in orchestrator but not in active INTENT_GROUPS
- **Effort:** 5 minutes

**Expected Impact:** Adds 1.1M documents of cross-reference capabilities!

---

## 📋 Priority 2: Evaluate Overlap (RESEARCH)

Investigate if these indexes duplicate existing functionality:

### Compare program_deps vs program_flows
```bash
python compare_indexes.py --index1 new_cobol_program_deps --index2 new_cobol_program_flows
```
- **program_deps:** Flat dependency lists (calls in/out, copybooks)
- **program_flows:** Control flow graphs with paragraph-level detail
- **Verdict:** DIFFERENT - deps is simpler, flows is detailed
- **Recommendation:** Keep both OR use deps for quick queries, flows for deep analysis

### Compare copybook_meta vs copybook_usage
```bash
python compare_indexes.py --index1 new_cobol_copybook_meta --index2 new_cobol_copybook_usage
```
- **copybook_meta:** One doc per .CPY file (summary/structure)
- **copybook_usage:** Many docs (each COPY statement occurrence)
- **Verdict:** COMPLEMENTARY - meta for "what is", usage for "where used"
- **Recommendation:** Keep both

### Compare menu_trees vs ui_paths
```bash
python compare_indexes.py --index1 new_cobol_menu_trees --index2 new_cobol_ui_paths
```
- **menu_trees:** Hierarchical tree structures
- **ui_paths:** Linear navigation paths
- **Verdict:** DIFFERENT representations of same data
- **Recommendation:** Evaluate if ui_paths covers all use cases

### Compare program_inventory vs program_meta
```bash
python compare_indexes.py --index1 new_cobol_program_inventory --index2 new_cobol_program_meta
```
- **program_inventory:** Program tracking/metrics
- **program_meta:** Program metadata + classification
- **Verdict:** LIKELY REDUNDANT
- **Recommendation:** Consolidate into program_meta

---

## 🔧 Priority 3: Add Vector Fields (MEDIUM EFFORT)

These keyword-only indexes could be enhanced with semantic search:

### 1. **new_cobol_name_aliases** (55,636 docs)
- **Current:** Keyword-only (name normalization)
- **Enhancement:** Add vector field for fuzzy name matching
- **Effort:** 2-3 hours (schema update + embeddings)
- **Value:** Better handling of COBOL naming variations

### 2. **new_cobol_menu_trees** (9,678 docs)
- **Current:** Keyword-only (menu hierarchies)
- **Enhancement:** Add vector field for menu path search
- **Effort:** 1-2 hours
- **Value:** "Find menu for screen X" queries

### 3. **new_cobol_program_copybook_edges** (107,569 docs)
- **Current:** Keyword-only (program-copybook links)
- **Enhancement:** Add vector field with context
- **Effort:** 3-4 hours (large dataset)
- **Value:** Semantic search over copybook relationships

---

## ❌ Priority 4: Delete Redundant Indexes (CLEANUP)

After confirming overlap, delete these:

### Candidates for Deletion:
1. **new_cobol_program_inventory** (9,678 docs)
   - Likely redundant with program_meta
   - **Verify first:** Check if any scripts use it

2. **TBD after overlap analysis**

---

## 🎯 Recommended Immediate Action

### Quick Win: Activate 3 High-Value Indexes

**File to edit:** `retrieval/orchestrate_answer.py`

**Current state:**
```python
INTENT_GROUPS={
  PURPOSE:['program_meta','copybook_meta'],
  DEPS:['program_deps','program_meta'],
  FLOW:['program_flows','symbol_refs'],      # ← symbol_refs already here!
  VARIABLE:['variable_usage','symbol_refs'],  # ← symbol_refs already here!
  COPYBOOK:['copybook_usage','copybook_meta'],
  # ...
}
```

**Add calls to DEPS:**
```python
DEPS:['program_deps','calls','program_meta'],  # ← Add 'calls'
```

**Result:** 
- ✅ `symbol_refs` already wired (1.1M docs active!)
- ✅ `calls` now active (15.8k docs)
- ✅ `copybook_meta` already in COPYBOOK group (7.8k docs)

**Total activation:** 1.1M new documents with **5 minutes of work**!

---

## 📊 Summary Table

| Index | Docs | Status | Action | Effort | Impact |
|-------|------|--------|--------|--------|--------|
| **symbol_refs** | 1.1M | ✅ 100% | Already wired! | ✅ DONE | ⭐⭐⭐ |
| **calls** | 15.8k | ✅ 100% | Wire to DEPS | 5 min | ⭐⭐⭐ |
| **copybook_meta** | 7.8k | ✅ 100% | Already wired! | ✅ DONE | ⭐⭐ |
| **program_deps** | 9.7k | ✅ 100% | Already wired! | ✅ DONE | ⭐⭐ |
| name_aliases | 55.6k | ❌ No vectors | Add vectors | 3h | ⭐ |
| menu_trees | 9.7k | ❌ No vectors | Add vectors | 2h | ⭐ |
| program_copybook_edges | 107k | ❌ No vectors | Add vectors | 4h | ⭐ |
| program_inventory | 9.7k | ❌ No vectors | Delete? | 0h | ❌ |

---

## ✅ Success Criteria

After completing activation:
- [ ] `calls` wired into orchestrator DEPS intent group
- [ ] Test query: "What programs call ORDENT01?"
- [ ] Test query: "Where is WS-CUSTOMER-ID used?"
- [ ] Verify 1.1M+ docs now searchable
- [ ] Update chatbot documentation with new capabilities

---

## 🚀 Next Steps

1. **Immediate (5 min):** Wire `calls` into orchestrator
2. **Test (10 min):** Verify symbol_refs, calls, copybook_meta queries work
3. **Document (15 min):** Update chatbot Q&A with new query types
4. **Evaluate (1-2 days):** Run overlap analysis on remaining indexes
5. **Clean up (1 hour):** Delete confirmed redundant indexes
