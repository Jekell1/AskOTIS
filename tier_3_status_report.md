# TIER 3 INDEXES - COMPREHENSIVE STATUS REPORT
**Generated:** October 11, 2025

## INDEX TIER CLASSIFICATION

### TIER-1: Core Program Metadata (4 indexes)
✓ **100% Complete** - Program-level summary and dependency data
- `new_cobol_program_meta` - Program summaries and metrics
- `new_cobol_program_inventory` - Program inventory tracking
- `new_cobol_program_deps` - Program dependencies
- `new_cobol_program_flows` - Program control flow graphs

### TIER-2: Program Internals (8 indexes)
⚠️ **Partial Coverage** - Detailed program components
- `new_cobol_paragraphs` - Paragraph definitions
- `new_cobol_data_items` - Data structure definitions
- `new_cobol_variable_usage` - Variable usage patterns
- `new_cobol_symbol_refs` - Symbol references
- `new_cobol_flow_edges_v2` - Control flow edges
- `new_cobol_copybook_usage` - Copybook inclusions
- `new_cobol_program_copybook_edges` - Program-copybook relationships
- `new_cobol_screen_nodes` - Screen/form definitions

### TIER-3: Cross-Program Analysis (5 indexes) ⭐ FOCUS
🔧 **Needs Work** - System-wide relationships and patterns
- `new_cobol_calls` - Cross-program call relationships
- `new_cobol_copybook_meta` - Copybook metadata summaries
- `new_cobol_menu_trees` - Menu navigation trees
- `new_cobol_ui_paths` - User interface navigation paths
- `new_cobol_name_aliases` - Name normalization/aliases

---

## TIER-3 DETAILED STATUS

### 1. 📞 new_cobol_calls
**Purpose:** Cross-program CALL statement tracking

| Metric | Value | Status |
|--------|-------|--------|
| **Documents** | 15,788 | ✓ |
| **Fields** | 15 | ✓ |
| **Vector Fields** | `snippet_vector` (1) | ✗ NOT EMBEDDED |
| **Source Coverage** | 0% (0/9,678 programs) | ✗ MISSING program_id |
| **Embeddings** | 0/15,788 (0.0%) | ✗ |
| **has_vector Flag** | 15,788/15,788 (100.0%) | ✓ |

**Issues:**
- ✗ No `program_id` field in schema (only caller_program/callee_program)
- ✗ Zero embeddings generated for `snippet_vector`
- ⚠️ Can't track source coverage without standardized `program_id`

**Action Items:**
1. Add `program_id` field (use `caller_program` as source)
2. Backfill embeddings for all 15,788 call records
3. Update coverage tracking

---

### 2. 📚 new_cobol_copybook_meta
**Purpose:** Copybook-level metadata and summaries

| Metric | Value | Status |
|--------|-------|--------|
| **Documents** | 7,817 | ✓ |
| **Fields** | 7 | ✓ |
| **Vector Fields** | `summary_vector` (1) | ✗ NOT EMBEDDED |
| **Source Coverage** | N/A (copybook-level) | ⚪ |
| **Embeddings** | 0/7,817 (0.0%) | ✗ |
| **has_vector Flag** | 7,817/7,817 (100.0%) | ✓ |

**Issues:**
- ✗ Zero embeddings generated for `summary_vector`
- ⚪ Not program-scoped (copybook names are the key)

**Action Items:**
1. **HIGH PRIORITY:** Backfill embeddings for all 7,817 copybook summaries
2. Create script: `backfill_copybook_meta_vectors.py`

---

### 3. 🌲 new_cobol_menu_trees
**Purpose:** Menu navigation tree structures

| Metric | Value | Status |
|--------|-------|--------|
| **Documents** | 9,678 | ✓ |
| **Fields** | 9 | ✓ |
| **Vector Fields** | None (0) | ✓ COMPLETE |
| **Source Coverage** | 0% (no program_id) | ⚪ |
| **Embeddings** | N/A | ✓ |

**Issues:**
- ⚪ No vector fields by design (structural data)
- ⚪ Uses `root_program_id` instead of `program_id`

**Action Items:**
- ✓ **NONE** - Index is complete as designed

---

### 4. 🎨 new_cobol_ui_paths
**Purpose:** User interface navigation path analysis

| Metric | Value | Status |
|--------|-------|--------|
| **Documents** | 1,437 | ✓ |
| **Fields** | 35 | ✓ |
| **Vector Fields** | `path_vector` (1) | ✗ NOT EMBEDDED |
| **Source Coverage** | 0% (path-level) | ⚪ |
| **Embeddings** | 0/1,437 (0.0%) | ✗ |
| **has_vector Flag** | 1,437/1,437 (100.0%) | ✓ |

**Issues:**
- ✗ Zero embeddings generated for `path_vector`
- ⚪ Path-scoped, not program-scoped (uses root_program_id, end_program_id)

**Action Items:**
1. **HIGH PRIORITY:** Backfill embeddings for all 1,437 UI paths
2. Create script: `backfill_ui_paths_vectors.py`

---

### 5. 🏷️ new_cobol_name_aliases
**Purpose:** Name normalization and alias resolution

| Metric | Value | Status |
|--------|-------|--------|
| **Documents** | 55,636 | ✓ |
| **Fields** | 9 | ✓ |
| **Vector Fields** | None (0) | ✓ COMPLETE |
| **Source Coverage** | 0% (alias-level) | ⚪ |
| **Embeddings** | N/A | ✓ |

**Issues:**
- ⚪ No vector fields by design (lookup table)

**Action Items:**
- ✓ **NONE** - Index is complete as designed

---

## TIER-3 SUMMARY

### Overall Status
| Category | Count | Percentage |
|----------|-------|------------|
| **Total Indexes** | 5 | 100% |
| **Complete (no action)** | 2 | 40% |
| **Need Embeddings** | 2 | 40% |
| **Need Schema Fix** | 1 | 20% |

### Embedding Backlog
| Index | Records Needing Embeddings | Vector Field | Priority |
|-------|---------------------------|--------------|----------|
| `new_cobol_copybook_meta` | 7,817 | `summary_vector` | 🔴 HIGH |
| `new_cobol_ui_paths` | 1,437 | `path_vector` | 🔴 HIGH |
| `new_cobol_calls` | 15,788 | `snippet_vector` | 🟡 MEDIUM* |

*Medium priority because schema needs fixing first

**Total Embedding Backlog:** 25,042 records

### Documents by Status
- ✅ **Complete:** 65,314 docs (2 indexes)
- ⚠️ **Needs Embeddings:** 25,042 docs (3 indexes)
- 🚫 **Schema Issues:** 15,788 docs (1 index)

---

## RECOMMENDED ACTION PLAN

### Phase 1: Quick Wins (Small Datasets) ⭐
**Estimated Time:** 1-2 hours

1. **new_cobol_ui_paths** (1,437 records)
   ```bash
   python backfill_ui_paths_vectors.py --batch 128
   ```
   - Smallest dataset, fastest completion
   - High-value for navigation queries

2. **new_cobol_copybook_meta** (7,817 records)
   ```bash
   python backfill_copybook_meta_vectors.py --batch 256
   ```
   - Critical for copybook understanding
   - Reasonable size for quick processing

### Phase 2: Schema Fix
**Estimated Time:** 30 minutes

3. **new_cobol_calls** - Add program_id field
   ```bash
   python add_program_id_to_calls.py
   ```
   - Derive from `caller_program` field
   - Enables proper coverage tracking

### Phase 3: Large Embedding Job
**Estimated Time:** 2-3 hours

4. **new_cobol_calls** (15,788 records)
   ```bash
   python backfill_calls_vectors.py --batch 256 --resume
   ```
   - Run after schema fix
   - May need resumable processing

---

## TIER COMPARISON

### Completion by Tier
| Tier | Indexes | Embeddings Complete | Source Coverage Complete |
|------|---------|---------------------|--------------------------|
| **TIER-1** | 4 | 0/4 (0%) | 4/4 (100%) |
| **TIER-2** | 8 | 1/8 (12.5%) | 0/8 (0%) |
| **TIER-3** | 5 | 0/3* (0%) | N/A (mostly non-program) |

*Out of 3 indexes with vector fields

### Key Insight
All tiers have embedding backlogs, but TIER-3 is unique because:
- ✅ Smaller datasets (easier to complete)
- ✅ Less dependent on program_id coverage
- ✅ High-value for cross-program analysis queries
- ⚠️ Can be completed independently of TIER-1/TIER-2

---

## NEXT STEPS

**Immediate Actions:**
1. ✓ Create `backfill_ui_paths_vectors.py` script
2. ✓ Create `backfill_copybook_meta_vectors.py` script
3. ✓ Create `add_program_id_to_calls.py` script
4. Run Phase 1 embedding jobs
5. Validate embedding quality
6. Update this status report

**Success Criteria:**
- [ ] new_cobol_ui_paths: 100% embedded (1,437/1,437)
- [ ] new_cobol_copybook_meta: 100% embedded (7,817/7,817)
- [ ] new_cobol_calls: program_id field added to all records
- [ ] new_cobol_calls: 100% embedded (15,788/15,788)
- [ ] All TIER-3 vector searches functional

---

*Report generated by comprehensive index analysis pipeline*
