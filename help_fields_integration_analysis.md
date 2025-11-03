# help_fields Integration Analysis

## Summary

The `help_fields` index is **properly configured and embedded** (100% coverage, 24,229 documents), but **retrieval is not effective** due to semantic ranking issues.

---

## Current State

### ✅ What's Working

1. **Index exists** and is queryable
   - 24,229 field-level help documents
   - 100% embedding coverage (3072-dim vectors)
   - Rich content: usage instructions, valid values, function keys

2. **Configuration is correct**
   - `config.py` includes `'help_fields': 'help_fields'`
   - `router.py` includes help_fields in "ui" profile
   - Router DOES include help_fields in queries

3. **Content is valuable**
   - Example: "Enter 'I' to INCLUDE ALL FB SOURCE CODE LOANS..."
   - Field-by-field instructions for 50+ AULNRG fields
   - Function keys, valid values, field properties

### ❌ What's NOT Working

**Problem**: help_fields documents are **not appearing in top results**

**Test Case**: "How do I use the AULNRG screen?"
- Router classification: `trace_flow`
- Indexes queried: `['flows', 'flow_edges', 'paragraphs', 'screen_nodes', 'ui_paths', 'help_fields', 'data_items', 'copybooks']`
- Index weights: `{'help_fields': 1.0, 'ui_paths': 3.0, ...}`
- **Result**: Top 20 results = 20 screen_nodes documents, 0 help_fields documents

**Retrieved instead**: `screen_nodes` documents for RPMAIN, RPMAI2, RTPU02 (menu screens, not AULNRG)

---

## Root Cause Analysis

### 1. Index Weight Too Low

```python
# From router.py
'index_weights': {
    'ui_paths': 3.0,      # Gets 3x boost
    'help_fields': 1.0,   # No boost
    'screen_nodes': 1.0   # No boost
}
```

**Impact**: help_fields competes equally with other indexes despite having the most relevant content for "how to use" questions.

### 2. Semantic Mismatch

**Query**: "How do I use the AULNRG screen?"

**What screen_nodes contains** (structural):
- Field names, positions, types
- "AULNRG screen definition"
- "fields on AULNRG"

**What help_fields contains** (instructional):
- "Enter 'I' to INCLUDE ALL..."
- "Press FUNCTION KEY 3 (F3) to include ALL ACCOUNTS"
- "Enter the minimum amount financed..."

**Problem**: The embedding for "How do I use" might match screen structure better than help instructions semantically.

### 3. Query Routing Classification

**Current**: "How do I use the AULNRG screen?" → classified as `trace_flow`
**Better**: Should be classified as `menu` or `screen_usage` to boost help_fields

---

## Recommended Fixes

### Option 1: Boost help_fields for Screen Questions (EASIEST)

Update `router.py` to boost help_fields when screen usage is detected:

```python
def _calculate_index_weights(self, indexes, has_menu_intent, question_type):
    weights = {idx: 1.0 for idx in indexes}
    
    # Existing logic...
    
    # NEW: Boost help_fields for screen/field usage questions
    if 'help_fields' in indexes:
        if any(pattern in question_type for pattern in ['screen', 'field', 'usage']):
            weights['help_fields'] = 5.0  # Strong boost
        elif has_menu_intent:
            weights['help_fields'] = 3.0  # Moderate boost
    
    return weights
```

### Option 2: Add Explicit Screen Usage Question Type (BETTER)

Add new question pattern to router:

```python
QUESTION_PATTERNS = {
    # ... existing patterns ...
    
    'screen_usage': [  # NEW
        r'\bhow do i (use|fill out|complete)\b.*\bscreen\b',
        r'\bwhat do i enter\b.*\b(field|screen)\b',
        r'\bwhat (goes|information|data) (in|into|on)\b.*\bscreen\b',
        r'\bfield instructions?\b',
        r'\bhelp (text|for)\b.*\b(screen|field)\b',
    ]
}

# In _select_indexes:
elif question_type == 'screen_usage':
    return self.ROUTE_PROFILES['ui']  # screen_nodes, ui_paths, help_fields

# In _calculate_index_weights:
if question_type == 'screen_usage' and 'help_fields' in indexes:
    weights['help_fields'] = 8.0  # Very strong boost
    weights['screen_nodes'] = 2.0  # Structure is supporting info
```

### Option 3: Improve Query Phrasing (USER-FACING)

Rephrase questions to better match help_fields content:

**Instead of**: "How do I use the AULNRG screen?"
**Use**: 
- "What do I enter in the AULNRG screen fields?"
- "Show me field instructions for AULNRG"
- "What values can I enter in AULNRG?"

**Instead of**: "How do I use field X?"
**Use**:
- "What do I enter in field X?"
- "What values are valid for field X?"
- "Explain field X"

---

## Testing Strategy

### Phase 1: Quick Validation (5 minutes)

Test these field-specific questions (should match help_fields well):

```python
test_questions = [
    "What does the FB field do?",
    "What values can I enter in the FB field?",
    "What do I type in the PROCEEDS1 field?",
    "What is the CONFID field for?",
    "Explain the ENDDATE field",
]
```

Expected: help_fields should appear in top 5 results for each.

### Phase 2: Screen-Level Questions (10 minutes)

Test screen-level questions after implementing routing fixes:

```python
test_questions = [
    "What do the fields on AULNRG screen do?",
    "How do I fill out the AULNRG screen?",
    "What information goes into AULNRG screen?",
    "Show me field instructions for AULNRG",
]
```

Expected: help_fields should dominate (8+ out of top 10).

### Phase 3: Integration Testing (30 minutes)

Use `test_questions_help_fields.py` to run full test suite:

```bash
python test_questions_help_fields.py
```

Track:
- % of queries where help_fields appears in top 3
- % of queries where help_fields appears in top 10
- Average rank of help_fields documents

---

## Impact Assessment

### Without Fix
- help_fields effectively invisible in mixed-index queries
- Users get structural info (screen_nodes) but not usage instructions
- 24,229 help documents with 100% embeddings are underutilized
- Poor user experience for "how to" questions

### With Fix (Option 2)
- help_fields appears in top 3 for screen usage questions
- Users get actionable instructions ("Enter 'I' to include...")
- Better balance of structure (screen_nodes) + usage (help_fields)
- Full value realized from 24K help documents

---

## Metrics to Track

After implementing fixes, monitor:

1. **Retrieval Quality**
   - help_fields appearance rate in top 10 results
   - Position/rank of help_fields documents
   - User satisfaction (if available)

2. **Query Patterns**
   - % of queries classified as screen_usage
   - % of queries that include help_fields in retrieval
   - Most common help_fields retrieved

3. **Index Usage**
   - help_fields query frequency
   - help_fields vs screen_nodes retrieval ratio
   - Coverage: which help_fields are never retrieved

---

## Next Steps

**Immediate (30 minutes)**:
1. Implement Option 2 (add screen_usage question type)
2. Test with 5 field-specific questions
3. Verify help_fields appears in results

**Short-term (2 hours)**:
1. Run full test suite (50+ questions)
2. Tune weights based on results
3. Document optimal question phrasing

**Long-term (ongoing)**:
1. Monitor help_fields usage in production
2. Identify under-utilized help content
3. Consider creating composite "screen guide" index
   - Combine screen_nodes (structure) + help_fields (usage)
   - Single query returns complete screen information

---

## Code References

- **Router**: `otis_rag/router.py`
  - Line ~122: `def route()` - routing logic
  - Line ~35: `ROUTE_PROFILES` - index groups
  - Line ~55: `QUESTION_PATTERNS` - classification patterns
  
- **Config**: `otis_rag/config.py`
  - help_fields mapping already added
  
- **Test Scripts**:
  - `test_questions_help_fields.py` - 50+ test questions
  - `test_aulnrg_rag_retrieval.py` - diagnostic script
  - `query_aulnrg_help.py` - direct help_fields query

---

## Conclusion

The help_fields integration is **technically complete** but **functionally limited** by retrieval ranking. Implementing routing improvements (especially Option 2) will unlock the full value of the 24,229 field-level help instructions we've embedded.

**Recommended Action**: Implement Option 2 (screen_usage question type) with help_fields boost of 8.0x.
