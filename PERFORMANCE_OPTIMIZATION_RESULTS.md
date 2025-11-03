# OTIS RAG Performance Optimization Results

## Summary
Implemented dynamic resource allocation system that adapts retrieval limits and max_tokens based on question complexity, achieving **75% performance improvement** for menu queries while maintaining quality for complex queries.

---

## Performance Improvements

### Menu Queries (e.g., "What choices are displayed in the COLLECTION MAINTENANCE MENU?")

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| **Total Time** | 23.79s | 5.94s | **75% faster** |
| **Retrieval Time** | 13.54s | 2.03s | **85% faster** |
| **Generation Time** | 5.52s | 2.62s | 53% faster |
| **Documents Retrieved** | 6,819 | 40 | **99.4% reduction** |

### General Queries (e.g., "What does program GB01SE do?")

| Metric | Result |
|--------|--------|
| **Total Time** | 5.56s |
| **Retrieval Time** | 1.89s |
| **Generation Time** | 3.48s |
| **Documents Retrieved** | 100 |

### Complex Flow Tracing (e.g., "Trace execution from GTMAIN to APIPAY")

| Metric | Result |
|--------|--------|
| **Total Time** | 9.53s |
| **Retrieval Time** | 2.01s |
| **Generation Time** | 7.30s |
| **Documents Retrieved** | 200 |
| **Max Tokens** | 15,000 |

---

## Architecture Changes

### 1. Dynamic Retrieval Limits (rag.py)

Questions are classified into types with appropriate document limits:

```python
if question_type in ('menu', 'list', 'simple'):
    max_results_for_query = 20        # Specific questions
elif question_type == 'trace_flow':
    max_results_for_query = 100       # Flow tracing
elif question_type == 'transaction':
    max_results_for_query = 50        # Business transactions
elif 'all' in query.lower() or 'list' in query.lower():
    max_results_for_query = 200       # Explicit list requests
else:
    max_results_for_query = 50        # General questions
```

### 2. Proportional Document Capping (retriever.py)

Final document count scales with question complexity:

```python
dynamic_cap = min(max_results * 2, 200)  # 2x per-index limit, max 200
final_count = min(dynamic_cap, max(10, raw_half))
```

**Examples:**
- Menu (20 per index) → 40 final docs
- General (50 per index) → 100 final docs  
- Trace (100 per index) → 200 final docs

### 3. Dynamic max_tokens (generator.py)

LLM response length adapts to question complexity:

```python
if question_type in ('menu', 'list', 'simple'):
    max_response_tokens = 2000
elif question_type == 'trace_flow':
    max_response_tokens = 15000
else:
    max_response_tokens = 4000
```

---

## Question Type Detection (router.py)

Enhanced router with priority-based classification:

### Menu Patterns (Highest Priority)
```python
'menu': [
    r'\bmenu\b.*\b(choice|option|selection|text|display|show)\w*\b',
    r'\b(choice|option|selection)\w*\b.*\b(menu|screen)\b',
    r'\bwhat\b.*\b(choice|option|selection)\w*\b.*\b(display|show|see)\w*\b',
    r'\b(display|show)\w*\b.*\b(menu|screen)\b',
    r'\buser\b.*\bsee\w*\b.*\b(menu|screen)\b',
]
```

Correctly identifies:
- ✅ "What choices are displayed in the COLLECTION MAINTENANCE MENU?" → `menu`
- ✅ "Trace execution from GTMAIN to APIPAY" → `trace_flow`
- ✅ "What does program GB01SE do?" → `explain_program`

---

## Configuration Changes

### Before
```python
self.max_results_per_index = 10000  # Caused retrieval of 6,819 docs
```

### After
```python
self.max_results_per_index = 50     # Fallback default (overridden dynamically)
```

---

## Timing Instrumentation

### API Response Format
```json
{
  "question": "What choices are displayed...",
  "answer": "The COLLECTION MAINTENANCE MENU displays...",
  "timing": {
    "total_seconds": 4.644,
    "route_seconds": 0.000,
    "retrieve_seconds": 2.028,
    "memory_seconds": 0.000,
    "generate_seconds": 2.615,
    "question_type": "menu",
    "num_documents": 40
  }
}
```

### Log Output
```
⏱️ TIMING: Routing took 0.000s
📊 Dynamic retrieval: question_type=menu, max_results=20
⏱️ TIMING: Embedding generation (3072-dim) took 0.245s
⏱️ TIMING: Search screen_nodes took 0.412s (20 results)
📄 Returning 40 documents (from 80 retrieved)
⏱️ TIMING: Retrieval took 2.028s
⏱️ Generating response with max_tokens=2000, question_type=menu
⏱️ TIMING: LLM call took 2.615s (tokens: 1847)
```

---

## Data Quality Improvements

### Screen Nodes Index (build_screen_nodes.py)

**Filters Applied:**
1. Remove menu titles (ending with "MENU", no number prefix)
2. Remove "ENTER SELECTION" prompts
3. Sort by menu number (1-18) then F-keys (F6-F7)

**Before:**
```
1. COLLECTION MAINTENANCE MENU
2. 1. 1. EXTRACTION/ACTION FILE MAINT.
3. 2. 2. CUSTOMER/CREDIT PROFILE MAINT.
...
22. ENTER SELECTION
```

**After:**
```
1. EXTRACTION/ACTION FILE MAINT.
2. CUSTOMER/CREDIT PROFILE MAINT.
3. LOCATION MAINT.
...
18. SCREEN 18 ITEM
F6. HELP
F7. BACK
```

---

## Performance Test Results

### Test Date: 2025-10-22

| Question Type | Time (s) | Docs | Retrieve (s) | Generate (s) |
|---------------|----------|------|--------------|--------------|
| Menu | 5.94 | 40 | 2.03 | 2.62 |
| Explain Program | 5.56 | 100 | 1.89 | 3.48 |
| List Programs | 7.51 | 100 | 2.14 | 1.95 |
| Trace Flow | 9.53 | 200 | 2.01 | 7.30 |

### Key Observations
- ✅ Menu queries: 75% faster (23.79s → 5.94s)
- ✅ Document counts scale appropriately (40-200 based on complexity)
- ✅ Retrieval time consistently 2-3s regardless of doc count (efficient Azure Search)
- ✅ Generation time scales with max_tokens (2s for 2K tokens, 7s for 15K tokens)
- ✅ All question types work correctly without hardcoded menu-specific logic

---

## Deployment Details

**Azure Function:** func-otis-rag.azurewebsites.net  
**Python Version:** 3.11.14  
**Deployment Date:** 2025-10-22 19:23:04 UTC  
**Deployment Method:** Remote build via Oryx

**Files Modified:**
- `otis_rag/rag.py` - Dynamic retrieval limits
- `otis_rag/retriever.py` - Proportional document capping
- `otis_rag/config.py` - Reduced default limit
- `otis_rag/generator.py` - Dynamic max_tokens (previous deployment)
- `otis_rag/router.py` - Menu pattern detection (previous deployment)
- `function_app.py` - Timing in API response
- `host.json` - INFO level logging (previous deployment)

---

## Lessons Learned

1. **Dynamic > Static Configuration**  
   One-size-fits-all limits (max_tokens=15K, max_results=10K) killed performance. Dynamic allocation based on question type provides optimal performance for all queries.

2. **Timing Instrumentation is Critical**  
   Without detailed timing breakdown, we initially thought LLM generation was the bottleneck. Instrumentation revealed retrieval (13.5s with 6,819 docs) was the real issue.

3. **End-to-End Testing Reveals Hidden Issues**  
   Testing with actual queries showed 6,819 documents being retrieved for simple menu questions - impossible to discover through code review alone.

4. **Proportional Scaling Works**  
   Using `max_results * 2` for final_count ensures document count scales appropriately:
   - Simple questions: 40 docs (2x 20 per index)
   - Complex questions: 200 docs (2x 100 per index)

5. **Azure Search is Fast**  
   Even with 200 documents, retrieval takes only ~2s. The bottleneck was transferring and processing 6,819 documents, not the search itself.

---

## Next Steps (Optional)

### Further Optimization Opportunities
1. **Caching:** Add Redis cache for common queries (could reduce repeat queries from 6s → <1s)
2. **Question Classification:** Refine patterns for better type detection
3. **Index Optimization:** Consider separate smaller indexes for menu/UI content
4. **Parallel Retrieval:** Search multiple indexes concurrently (may save 0.5-1s)
5. **Embedding Cache:** Cache embeddings for common queries to skip generation step

### Monitoring
- Application Insights: Track `question_type` distribution
- Custom metrics: Average retrieval/generation time by type
- Alert on: Total time > 15s or documents > 300

---

## Conclusion

The dynamic resource allocation system successfully:
- ✅ **Improved menu query performance by 75%** (23.79s → 5.94s)
- ✅ **Reduced document retrieval by 99.4%** for simple queries (6,819 → 40 docs)
- ✅ **Maintained quality for complex queries** (200 docs, 15K tokens for trace_flow)
- ✅ **Works for all question types** without hardcoded menu-specific logic
- ✅ **Provides timing visibility** via API response and logs

The system now adapts intelligently to question complexity, providing fast responses for simple queries while maintaining comprehensive context for complex analysis.
