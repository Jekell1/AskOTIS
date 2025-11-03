# CHATBOT-DRIVEN INDEX PRIORITY ANALYSIS
**Generated:** October 11, 2025  
**Purpose:** Prioritize indexes by impact on chatbot question-answering capability

---

## SAMPLE QUESTION ANALYSIS

### Questions Mapped to Index Requirements

| Question | Primary Indexes Needed | Secondary Indexes |
|----------|------------------------|-------------------|
| "Explain how APIPAY.CBL works" | program_meta, program_flows | paragraphs, flow_edges_v2 |
| "What is the purpose of LONPF2?" | program_meta | program_flows |
| "Where is variable BT-BRANCH instigated?" | variable_usage, symbol_refs | data_items |
| "How is BT-BRANCH used?" | variable_usage, symbol_refs | - |
| "What external programs does APIPAY call?" | program_deps, calls | - |
| "What dependencies does LONPF2 have?" | program_deps, program_copybook_edges | copybook_usage |
| "Where is TIM360 used?" | calls, name_aliases | program_deps |
| "What does TIM360 do?" | program_meta, program_flows | paragraphs |
| "Show me a flow of DAILY.CBL" | program_flows, flow_edges_v2 | paragraphs |
| "What CopyBooks are used in DAILY.CBL" | copybook_usage, program_copybook_edges | program_deps |
| "What programs use SCREEN.CPY" | copybook_usage, program_copybook_edges | - |
| "Tell me about DS-BRANCH. Where defined/used?" | data_items, variable_usage, symbol_refs | - |
| "Show me UI screen flow from main program" | ui_paths, menu_trees, screen_nodes | program_flows |
| "What is IRMAIN.CBL used for?" | program_meta, program_deps | calls |
| "What does the full set of code do?" | program_meta, program_inventory | All indexes |
| "Show me a flow for LNQUOT.CBL" | program_flows, flow_edges_v2 | paragraphs |
| "How is a loan payment received?" | program_meta, program_flows, screen_nodes | ui_paths, paragraphs |
| "Show complete user screen flow from main menu" | ui_paths, menu_trees, screen_nodes | program_flows |

---

## CHATBOT IMPACT-BASED TIER CLASSIFICATION

### 🔴 TIER-1: CRITICAL (Answer 80% of Questions)

These indexes are **essential** for the chatbot to answer most user questions:

#### 1. **new_cobol_program_meta** ⭐⭐⭐
- **Impact:** HIGHEST - Answers "What does X do?" questions
- **Questions Supported:** 8+ (program purpose, summary, role)
- **Documents:** 9,678
- **Embeddings Needed:** 9,678 (summary_vector)
- **Source Coverage:** ✅ 100%
- **Priority:** 🔴 **#1 MUST HAVE**

#### 2. **new_cobol_program_flows** ⭐⭐⭐
- **Impact:** HIGHEST - Answers "Show me flow" questions
- **Questions Supported:** 6+ (control flow, program logic)
- **Documents:** 9,677
- **Embeddings Needed:** 9,677 (flow_vector)
- **Source Coverage:** ✅ 100%
- **Priority:** 🔴 **#2 MUST HAVE**

#### 3. **new_cobol_program_deps** ⭐⭐⭐
- **Impact:** HIGHEST - Answers "What calls/uses X?" questions
- **Questions Supported:** 5+ (dependencies, relationships)
- **Documents:** 9,687
- **Embeddings Needed:** 9,687 (dependency_blob_vector)
- **Source Coverage:** ✅ 100%
- **Priority:** 🔴 **#3 MUST HAVE**

#### 4. **new_cobol_ui_paths** ⭐⭐⭐
- **Impact:** HIGH - Answers "Show me screen flow" questions
- **Questions Supported:** 3+ (UI navigation, user workflows)
- **Documents:** 1,437
- **Embeddings Needed:** 1,437 (path_vector)
- **Source Coverage:** N/A (path-scoped)
- **Priority:** 🔴 **#4 MUST HAVE** (Smallest dataset!)

#### 5. **new_cobol_screen_nodes** ⭐⭐
- **Impact:** HIGH - Answers UI/screen questions
- **Questions Supported:** 3+ (screen details, forms)
- **Documents:** 46,825
- **Embeddings Needed:** ✅ **0 (COMPLETE!)**
- **Source Coverage:** ⚠️ 21.6% (needs expansion)
- **Priority:** 🟢 **EMBEDDINGS DONE** / 🟡 Expand coverage

#### 6. **new_cobol_copybook_usage** ⭐⭐
- **Impact:** HIGH - Answers "What copybooks does X use?"
- **Questions Supported:** 2+ (copybook relationships)
- **Documents:** 114,307
- **Embeddings Needed:** 114,307 (context_vector)
- **Source Coverage:** ⚠️ 16.9%
- **Priority:** 🟠 **HIGH** (but large dataset)

**TIER-1 SUMMARY:**
- **Total Embeddings Needed:** 135,171 (excluding screen_nodes ✅)
- **Estimated Time:** 3-4 hours
- **Impact:** Enables 80%+ of chatbot questions
- **Action:** Complete these FIRST for maximum chatbot capability

---

### 🟠 TIER-2: HIGH VALUE (Answer Detailed/Technical Questions)

These indexes provide **deeper technical details** and support complex queries:

#### 7. **new_cobol_calls** ⭐⭐
- **Impact:** HIGH - "Where is X called?" / "What does X call?"
- **Questions Supported:** 3+ (call relationships)
- **Documents:** 15,788
- **Embeddings Needed:** 15,788 (snippet_vector)
- **Source Coverage:** ✗ 0% (missing program_id field)
- **Priority:** 🟠 **HIGH** (after schema fix)

#### 8. **new_cobol_variable_usage** ⭐⭐
- **Impact:** HIGH - "Where is variable X used?"
- **Questions Supported:** 2+ (variable tracking)
- **Documents:** 106,669
- **Embeddings Needed:** None (no vectors)
- **Source Coverage:** ⚠️ 11.0%
- **Priority:** 🟡 **EXPAND COVERAGE**

#### 9. **new_cobol_data_items** ⭐⭐
- **Impact:** MEDIUM - "Tell me about variable X"
- **Questions Supported:** 2+ (variable definitions)
- **Documents:** 267,769
- **Embeddings Needed:** 267,769 (vector)
- **Source Coverage:** ⚠️ 8.2%
- **Priority:** 🟡 **MEDIUM** (large dataset)

#### 10. **new_cobol_symbol_refs** ⭐⭐
- **Impact:** MEDIUM - "Where is X defined/referenced?"
- **Questions Supported:** 2+ (symbol tracking)
- **Documents:** 1,104,574
- **Embeddings Needed:** 1,104,574 (excerpt_vector)
- **Source Coverage:** ⚠️ 4.4%
- **Priority:** 🟡 **MEDIUM** (very large)

#### 11. **new_cobol_paragraphs** ⭐
- **Impact:** MEDIUM - Detailed flow analysis
- **Questions Supported:** 1-2 (paragraph-level details)
- **Documents:** 224,655
- **Embeddings Needed:** 224,655 (para_vector)
- **Source Coverage:** ⚠️ 23.4%
- **Priority:** 🟡 **MEDIUM**

#### 12. **new_cobol_flow_edges_v2** ⭐
- **Impact:** MEDIUM - Detailed flow edges
- **Questions Supported:** 1-2 (granular flow analysis)
- **Documents:** 366,650
- **Embeddings Needed:** 366,650 (edge_vector)
- **Source Coverage:** ⚠️ 7.4%
- **Priority:** 🟡 **MEDIUM**

**TIER-2 SUMMARY:**
- **Total Embeddings Needed:** 1,979,436
- **Estimated Time:** 15-18 hours
- **Impact:** Enables detailed technical questions
- **Action:** Complete after TIER-1 for enhanced capabilities

---

### 🟡 TIER-3: SUPPORTING (Infrastructure/Context)

These indexes provide **supporting context** and infrastructure:

#### 13. **new_cobol_menu_trees** ⭐
- **Impact:** LOW-MEDIUM - Menu structure context
- **Questions Supported:** 1 (menu navigation)
- **Documents:** 9,678
- **Embeddings Needed:** None
- **Priority:** ✅ **COMPLETE**

#### 14. **new_cobol_copybook_meta** ⭐
- **Impact:** LOW - Copybook summaries
- **Questions Supported:** 1 (copybook descriptions)
- **Documents:** 7,817
- **Embeddings Needed:** 7,817 (summary_vector)
- **Priority:** 🟢 **LOW** (but small, easy win)

#### 15. **new_cobol_name_aliases** ⭐
- **Impact:** LOW - Name normalization
- **Questions Supported:** Indirect (name resolution)
- **Documents:** 55,636
- **Embeddings Needed:** None
- **Priority:** ✅ **COMPLETE**

#### 16. **new_cobol_program_copybook_edges**
- **Impact:** LOW - Redundant with copybook_usage
- **Documents:** 107,569
- **Embeddings Needed:** None
- **Priority:** ⚪ **MAINTAIN**

#### 17. **new_cobol_program_inventory**
- **Impact:** LOW - Inventory tracking
- **Documents:** 9,678
- **Embeddings Needed:** None
- **Priority:** ✅ **COMPLETE**

**TIER-3 SUMMARY:**
- **Total Embeddings Needed:** 7,817
- **Estimated Time:** 30 minutes
- **Impact:** Supporting/context only
- **Action:** Low priority, do last

---

## CHATBOT-OPTIMIZED ACTION PLAN

### 🚀 PHASE 1: MVP CHATBOT (Week 1)
**Goal:** Answer 80% of user questions

**Priority Order:**
1. ✅ **new_cobol_ui_paths** (1,437) - 20 minutes ⭐ FASTEST WIN
2. ⭐ **new_cobol_program_meta** (9,678) - 1 hour ⭐ BIGGEST IMPACT
3. ⭐ **new_cobol_program_flows** (9,677) - 1 hour ⭐ FLOW QUESTIONS
4. ⭐ **new_cobol_program_deps** (9,687) - 1 hour ⭐ DEPENDENCY QUESTIONS

**Total:** 30,479 embeddings | ~3-4 hours | **80% chatbot capability**

**Chatbot Questions Enabled:**
- ✅ "What does program X do?"
- ✅ "Show me a flow of X"
- ✅ "What dependencies does X have?"
- ✅ "What programs does X call?"
- ✅ "Show me UI screen flows"
- ✅ "What is X used for?"

---

### 🎯 PHASE 2: ENHANCED CHATBOT (Week 2)
**Goal:** Answer copybook and detailed relationship questions

**Priority Order:**
5. ⚡ **new_cobol_copybook_usage** (114,307) - 2-3 hours
6. 🔧 **new_cobol_calls** (15,788) - 1 hour (after schema fix)

**Total:** 130,095 embeddings | ~4 hours | **95% chatbot capability**

**Additional Questions Enabled:**
- ✅ "What copybooks does X use?"
- ✅ "What programs use copybook Y?"
- ✅ "Where is program X called?"
- ✅ "What external calls does X make?"

---

### 🔬 PHASE 3: EXPERT CHATBOT (Week 3-4)
**Goal:** Answer deep technical questions about variables and symbols

**Priority Order:**
7. 🔍 **new_cobol_data_items** (267,769) - 3-4 hours
8. 🔍 **new_cobol_variable_usage** - No embeddings needed (expand coverage)
9. 🔍 **new_cobol_symbol_refs** (1,104,574) - 10-12 hours
10. 📝 **new_cobol_paragraphs** (224,655) - 3-4 hours
11. ⚡ **new_cobol_flow_edges_v2** (366,650) - 4-5 hours

**Total:** 1,963,648 embeddings | ~20+ hours | **100% chatbot capability**

**Additional Questions Enabled:**
- ✅ "Tell me about variable X"
- ✅ "Where is variable X defined?"
- ✅ "How is variable X used?"
- ✅ "Show me detailed paragraph-level flows"
- ✅ "What are all references to symbol X?"

---

### 🎁 PHASE 4: BONUS FEATURES (Week 4)
**Goal:** Add nice-to-have context

11. **new_cobol_copybook_meta** (7,817) - 30 minutes
12. Everything else in TIER-3

---

## EMBEDDING PRIORITY MATRIX

### By Chatbot Impact vs. Effort

| Index | Impact | Records | Time | Priority | Phase |
|-------|--------|---------|------|----------|-------|
| **new_cobol_ui_paths** | HIGH | 1,437 | 20m | 🔴 #1 | 1 |
| **new_cobol_program_meta** | HIGHEST | 9,678 | 1h | 🔴 #2 | 1 |
| **new_cobol_program_flows** | HIGHEST | 9,677 | 1h | 🔴 #3 | 1 |
| **new_cobol_program_deps** | HIGHEST | 9,687 | 1h | 🔴 #4 | 1 |
| **new_cobol_copybook_usage** | HIGH | 114,307 | 3h | 🟠 #5 | 2 |
| **new_cobol_calls** | HIGH | 15,788 | 1h | 🟠 #6 | 2 |
| **new_cobol_data_items** | MEDIUM | 267,769 | 4h | 🟡 #7 | 3 |
| **new_cobol_paragraphs** | MEDIUM | 224,655 | 3h | 🟡 #8 | 3 |
| **new_cobol_flow_edges_v2** | MEDIUM | 366,650 | 5h | 🟡 #9 | 3 |
| **new_cobol_symbol_refs** | MEDIUM | 1,104,574 | 12h | 🟡 #10 | 3 |
| **new_cobol_copybook_meta** | LOW | 7,817 | 30m | 🟢 #11 | 4 |

---

## QUICK START: IMMEDIATE ACTIONS

### Today: MVP Foundation (3-4 hours)

```bash
# 1. Quick Win (20 minutes)
python backfill_ui_paths_vectors.py --batch 128

# 2. Core Metadata (1 hour)
python backfill_program_meta_vectors.py --batch 256

# 3. Flow Analysis (1 hour)
python backfill_program_flows_vectors.py --batch 256

# 4. Dependencies (1 hour)
python backfill_program_deps_vectors.py --batch 256
```

**After these 4 scripts:**
- ✅ Chatbot can answer 80% of user questions
- ✅ 30,479 embeddings complete
- ✅ All core "What/How/Where" questions work
- ✅ Ready for initial testing

---

## CHATBOT QUERY PATTERNS

### Phase 1 Capabilities (After TIER-1)

```
✅ Program Purpose: "What does X do?" → program_meta
✅ Program Flow: "Show me flow of X" → program_flows
✅ Dependencies: "What does X depend on?" → program_deps
✅ Calls: "What programs does X call?" → program_deps
✅ UI Flows: "Show me screen flows" → ui_paths, screen_nodes
✅ Program Usage: "Where is X used?" → program_deps (reverse)
```

### Phase 2 Capabilities (After Copybooks + Calls)

```
✅ Copybook Usage: "What copybooks does X use?" → copybook_usage
✅ Reverse Copybooks: "What uses copybook Y?" → copybook_usage
✅ Call Details: "Where is X called from?" → calls
✅ Call Chains: "What's the call path to X?" → calls + program_deps
```

### Phase 3 Capabilities (After Variables + Symbols)

```
✅ Variable Definition: "Where is variable X defined?" → data_items
✅ Variable Usage: "How is variable X used?" → variable_usage
✅ Symbol References: "Show all references to X" → symbol_refs
✅ Detailed Flow: "Show paragraph-level flow" → paragraphs + flow_edges_v2
✅ Data Flow: "How does data flow through X?" → All indexes combined
```

---

## SUCCESS METRICS

### Phase 1 Complete (MVP Chatbot)
- [ ] 30,479 embeddings complete
- [ ] Can answer 80% of sample questions
- [ ] Response time < 3 seconds for simple queries
- [ ] Vector search working for 4 core indexes

### Phase 2 Complete (Enhanced Chatbot)
- [ ] 160,574 total embeddings complete
- [ ] Can answer 95% of sample questions
- [ ] Copybook and call queries working
- [ ] Response time < 5 seconds for complex queries

### Phase 3 Complete (Expert Chatbot)
- [ ] 2,124,222 total embeddings complete
- [ ] Can answer 100% of sample questions
- [ ] Deep technical queries working
- [ ] Multi-index composite queries optimized

---

## RECOMMENDATION

### 🎯 START HERE: Phase 1 (Today)

Run these 4 scripts in order:
1. `backfill_ui_paths_vectors.py` - 1,437 records (fastest win)
2. `backfill_program_meta_vectors.py` - 9,678 records (biggest impact)
3. `backfill_program_flows_vectors.py` - 9,677 records (flow questions)
4. `backfill_program_deps_vectors.py` - 9,687 records (dependency questions)

**Why this order?**
- ✅ Delivers 80% chatbot capability in 3-4 hours
- ✅ Validates embedding pipeline with small dataset first
- ✅ Enables immediate chatbot testing and feedback
- ✅ All core question types work (What/How/Where/Show)
- ✅ Manageable size (30K records vs 2M+)

**Next Steps:**
Once Phase 1 is complete and validated, move to Phase 2 (copybooks + calls) to reach 95% capability.

---

*Report generated for chatbot-driven index prioritization*
