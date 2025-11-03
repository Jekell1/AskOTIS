# PRIORITY ANALYSIS: Which Index to Complete Next for Maximum Chatbot Value

## Current Status Review

### ✅ FULLY COMPLETE (3 indexes)
1. **new_cobol_program_meta** - Program summaries ✅
2. **new_cobol_program_deps** - Dependencies ✅
3. **new_cobol_program_flows** - Control flows ✅

### ⚠️ NEED SOURCE EXPANSION (Embeddings Already 100%)

| Index | Coverage | Missing Programs | Chatbot Value | Priority |
|-------|----------|------------------|---------------|----------|
| **new_cobol_screen_nodes** | 40.6% | 5,752 | 🔴 **HIGHEST** | **#1** |
| **new_cobol_paragraphs** | 41.1% | 5,704 | 🟠 HIGH | #2 |
| **new_cobol_ui_paths** | N/A (path-scoped) | N/A | 🔴 **HIGHEST** | **#1** |
| **new_cobol_copybook_usage** | 17.8% | 7,958 | 🟠 HIGH | #3 |
| **new_cobol_data_items** | 17.3% | 8,007 | 🟡 MEDIUM | #4 |
| **new_cobol_flow_edges_v2** | 16.6% | 8,068 | 🟡 MEDIUM | #5 |
| **new_cobol_symbol_refs** | 4.8% | 9,218 | 🟡 MEDIUM | #6 |
| **new_cobol_calls** | 0% | N/A | 🟠 HIGH | #3 |
| **new_cobol_copybook_meta** | 0% (copybook-scoped) | N/A | 🟢 LOW | #7 |

---

## 🎯 HIGHEST VALUE: UI/Screen Indexes

### **RECOMMENDATION: Focus on UI_PATHS + SCREEN_NODES**

**Why these are highest priority:**

From your sample questions:
- ✅ "Show me UI screen flow from main program"
- ✅ "Show complete user screen flow from main menu"
- ✅ "How is a loan payment received?" (involves screens)

**Impact:**
- Enables **3+ critical chatbot questions** about user workflows
- Only **1,437 UI path documents** (very manageable)
- Only **46,825 screen node documents** (already 40% covered)
- **Embeddings already 100% complete** for both!

---

## 🔴 TOP PRIORITY #1: new_cobol_ui_paths

### Why This is #1:
1. **Smallest dataset** - Only 1,437 documents total
2. **Embeddings 100% complete** - Just needs source data
3. **Unique value** - Shows user workflows that other indexes don't provide
4. **High chatbot impact** - 3+ sample questions depend on this

### Current Status:
- Total documents: 1,437
- Embeddings: ✅ 100% (path_vector complete)
- Source coverage: N/A (path-scoped, not program-scoped)
- **Issue:** May not have enough paths generated yet

### What's Needed:
```bash
# Build more UI paths from existing data
python build_ui_paths.py --root-mode mixed --limit-roots 80 --max-depth 24 --push
```

### Chatbot Questions Enabled:
- ✅ "Show me UI screen flow from main program"
- ✅ "Show complete user screen flow from main menu down to bottom"
- ✅ "How is a loan payment received?" (UI workflow)

### Estimated Time: **30 minutes to 1 hour** (path generation + validation)

---

## 🔴 TOP PRIORITY #2: new_cobol_screen_nodes

### Why This is #2:
1. **40% coverage already** - Good foundation (3,926/9,678 programs)
2. **Embeddings 100% complete** - summary_vector done for all 46,825 nodes
3. **Complements UI paths** - Provides screen details for workflows
4. **Moderate dataset** - 46,825 documents (manageable)

### Current Status:
- Total documents: 46,825
- Programs covered: 3,926/9,678 (40.6%)
- Missing programs: 5,752 (59.4%)
- Embeddings: ✅ 100% (summary_vector complete)

### What's Needed:
```bash
# Extract screen nodes from missing 5,752 programs
python build_screen_nodes.py --missing-only --batch 500
```

### Chatbot Questions Enabled:
- ✅ All screen/form detail questions
- ✅ "What screens does X use?"
- ✅ "Show me fields on screen Y"

### Estimated Time: **2-3 hours** (screen extraction from 5,752 programs)

---

## 🟠 HIGH PRIORITY #3: new_cobol_calls

### Why This is #3:
1. **Critical for cross-program analysis** - "What calls X?"
2. **Embeddings 100% complete** - snippet_vector done for all 15,788 calls
3. **Schema issue** - Missing program_id field (need to fix)
4. **High chatbot impact** - Multiple questions about program calls

### Current Status:
- Total documents: 15,788
- Programs covered: 0% (no program_id field!)
- Embeddings: ✅ 100% (snippet_vector complete)

### What's Needed:
```bash
# Fix: Add program_id field (derive from caller_program)
python add_program_id_to_calls.py --push
```

### Chatbot Questions Enabled:
- ✅ "Where is TIM360 used?"
- ✅ "What calls program X?"
- ✅ "Show me call chains"

### Estimated Time: **30 minutes** (schema fix only)

---

## 🟠 HIGH PRIORITY #4: new_cobol_copybook_usage

### Why This is #4:
1. **Critical for understanding dependencies**
2. **Multiple chatbot questions** - "What copybooks does X use?"
3. **Embeddings 100% complete** - context_vector done for all 114,307 usages
4. **17.8% coverage** - Need to expand to 7,958 more programs

### Current Status:
- Total documents: 114,307
- Programs covered: 1,720/9,678 (17.8%)
- Missing programs: 7,958 (82.2%)
- Embeddings: ✅ 100% (context_vector complete)

### What's Needed:
```bash
# Extract copybook usage from missing programs
python build_copybook_usage.py --missing-only --batch 500
```

### Chatbot Questions Enabled:
- ✅ "What copybooks are used in DAILY.CBL?"
- ✅ "What programs use SCREEN.CPY?"
- ✅ Dependency analysis questions

### Estimated Time: **3-4 hours** (extract from 7,958 programs)

---

## 📊 RECOMMENDED ACTION SEQUENCE

### Phase 1: Quick Wins (2-3 hours total)
**Goal:** Enable ALL UI/screen questions + call tracking

1. **new_cobol_ui_paths** (30 min-1 hr)
   - Generate more UI paths
   - Embeddings already done ✅
   
2. **new_cobol_calls** (30 min)
   - Fix program_id field
   - Embeddings already done ✅

3. **new_cobol_screen_nodes** (2-3 hours)
   - Extract screens from 5,752 missing programs
   - Embeddings already done ✅

**Result:** UI workflows + screen details + call tracking all working

---

### Phase 2: High-Value Expansion (3-4 hours)
**Goal:** Complete copybook tracking

4. **new_cobol_copybook_usage** (3-4 hours)
   - Extract copybook usage from 7,958 missing programs
   - Embeddings already done ✅

**Result:** Full copybook dependency tracking

---

### Phase 3: Deep Technical Data (Later)
**Goal:** Variable and symbol tracking

5. **new_cobol_paragraphs** (4-5 hours)
6. **new_cobol_data_items** (6-8 hours)
7. **new_cobol_symbol_refs** (10-12 hours)

---

## 🎯 MY RECOMMENDATION: START WITH UI_PATHS

### Why UI_PATHS First?
1. ✅ **Fastest win** - Only 30 minutes to 1 hour
2. ✅ **Unique value** - No other index provides this
3. ✅ **High impact** - Enables 3+ critical chatbot questions
4. ✅ **Smallest dataset** - Only 1,437 paths
5. ✅ **Already embedded** - 100% ready for vector search

### Next Steps:
```bash
# Step 1: Check current UI paths
python probe_ui_paths.py --common --k 20

# Step 2: Generate more paths if needed
python build_ui_paths.py --root-mode mixed --limit-roots 80 \
  --max-depth 24 --branching-limit 20 --push

# Step 3: Verify embeddings still 100%
python find_fully_complete_indexes.py
```

### Then: Move to CALLS → SCREEN_NODES → COPYBOOK_USAGE

This sequence gives you the **most chatbot capability** in the **shortest time**.

---

## 🎊 BOTTOM LINE

**Start with new_cobol_ui_paths:**
- Fastest (30 min - 1 hour)
- Highest unique value
- Enables critical UI workflow questions
- Embeddings already complete ✅

**Then new_cobol_calls:**
- Quick schema fix (30 min)
- High chatbot value
- Embeddings already complete ✅

**Then new_cobol_screen_nodes:**
- Moderate effort (2-3 hours)
- Complements UI paths perfectly
- Embeddings already complete ✅

**Total: 3-5 hours to enable ALL UI/screen/call questions** 🚀
