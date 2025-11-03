# Recommended Changes to Improve REGPAY Field Retrieval

## Problem Summary
The RAG system returns **all 24 screen fields** instead of the **3 modifiable fields (MAX=3)** because:
- Copybook data (screen definitions) dominates search results
- Business logic (MAX=3, ENTRY-MODULE) ranks lower despite being more relevant
- Search terms match field definitions better than constraint logic

## Solution: No Index Schema Changes Needed! ✅

**Good news:** The data is already indexed correctly in:
- `new_code_chunks` - Contains MAX=3 definition and ENTRY-MODULE logic
- `new_cobol_paragraphs` - Contains ENTER-ELE paragraph with field prompts

**What we need:** Better retrieval and ranking logic

---

## Recommended Changes

### 1. **Enhance Retriever with Business Logic Boosting** (HIGH PRIORITY)

**File:** `otis_rag/retriever.py`

**Change:** Add query enhancement to boost business logic keywords

```python
def _enhance_query_for_business_logic(self, query: str, index_name: str) -> str:
    """Enhance query to boost business logic terms over data definitions."""
    
    # Detect if query is asking about user actions/modifications
    action_keywords = ['modify', 'change', 'update', 'edit', 'user can', 'options', 'choices']
    is_action_query = any(keyword in query.lower() for keyword in action_keywords)
    
    if not is_action_query:
        return query
    
    # For code chunks and paragraphs, boost constraint/logic terms
    if index_name in ['new_code_chunks', 'new_cobol_paragraphs']:
        # Add boosting terms to the query
        boost_terms = [
            'MAX^5',           # Boost MAX definitions heavily
            'ENTER NEW^3',     # Boost field entry prompts
            'ENTRY-MODULE^3',  # Boost entry logic
            'SPEC-TABLE^2',    # Boost specification tables
            'CHFG^2',          # Boost field flags
            'ELE^2',           # Boost element handling
            'field number^2'   # Boost field numbering logic
        ]
        
        # Append boost terms (Azure Search syntax)
        enhanced_query = f"{query} {' '.join(boost_terms)}"
        return enhanced_query
    
    return query
```

**Integration point:** In `_search_index()` method, before building search body:

```python
def _search_index(self, index_name: str, query: str, query_vector: List[float], top: int) -> List[Dict]:
    """Search a single index using hybrid search."""
    url = f"{self.config.search_endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
    
    # Enhance query for business logic if needed
    enhanced_query = self._enhance_query_for_business_logic(query, index_name)
    
    # ... rest of method
    body = {
        "search": enhanced_query,  # Use enhanced query instead of original
        "top": top,
        # ...
    }
```

---

### 2. **Enhance Generator Prompt with Business Logic Instructions** (MEDIUM PRIORITY)

**File:** `otis_rag/generator.py`

**Change:** Add instructions to prioritize constraints over data definitions

In `_get_system_prompt()`, add this section:

```python
CRITICAL: When asked about USER ACTIONS or MODIFICATIONS:
1. Look for constraint definitions FIRST (e.g., MAX=3, field count limits)
2. Look for field entry logic (e.g., ENTRY-MODULE, ENTER-ELE paragraphs)
3. Look for user prompts (e.g., "ENTER NEW...", "MODIFY...")
4. THEN look at screen field definitions
5. Distinguish between:
   - Fields DISPLAYED on screen (read-only reference data)
   - Fields USER CAN MODIFY (with entry prompts and validation)

Priority order for answering "what can user modify?":
   a) MAX/limit definitions (highest priority)
   b) ENTRY-MODULE / field entry paragraphs
   c) "ENTER NEW..." prompts with specific field purposes
   d) Screen copybook field lists (lowest priority - these show ALL fields)

If you see MAX=N, the user can modify exactly N fields, not all fields shown.
```

---

### 3. **Add Two-Phase Retrieval for Modification Questions** (OPTIONAL - LOW PRIORITY)

**File:** `otis_rag/retriever.py`

**Change:** Add a pre-query phase to find constraints

```python
def retrieve_with_constraints(self, query: str, indexes: List[str], max_results: int = 5) -> List[Dict[str, Any]]:
    """Two-phase retrieval: Find constraints first, then details.
    
    Phase 1: Search for MAX definitions, field counts, constraints
    Phase 2: Search for specific field details based on constraint
    """
    
    # Detect if this is a modification question
    modification_keywords = ['modify', 'change', 'update', 'edit', 'user can', 'options']
    is_modification_query = any(kw in query.lower() for kw in modification_keywords)
    
    if not is_modification_query:
        # Use standard retrieval
        return self.retrieve(query, indexes, max_results)
    
    # Phase 1: Find constraints (MAX definitions, field counts)
    constraint_query = f"{query} MAX SPEC-TABLE field count limit"
    constraint_results = []
    
    for index_type in ['code_chunks', 'new_code_chunks']:
        if index_type in indexes:
            index_name = self.config.get_index_name(index_type)
            results = self._search_index(
                index_name=index_name,
                query=constraint_query,
                query_vector=self._generate_embedding(constraint_query),
                top=3  # Small number - just need the constraint
            )
            constraint_results.extend(results)
    
    # Phase 2: Get detailed field information
    detail_results = self.retrieve(query, indexes, max_results)
    
    # Merge: constraints first, then details
    all_results = constraint_results + detail_results
    
    # Deduplicate and return
    return self._rank_results(all_results, max_results * len(indexes))
```

---

### 4. **Add Metadata Tags to Code Chunks** (OPTIONAL - Future Enhancement)

This would require re-indexing, so it's lower priority. But for future:

**During code chunk creation**, add a field:

```python
{
    "chunk_id": "...",
    "text": "...",
    "chunk_category": "business_logic"  # or "data_definition", "procedure", etc.
}
```

Classification logic:
- **business_logic**: Contains MAX, ENTRY-MODULE, field validation, user prompts
- **data_definition**: Contains data structures (01 level items, copybooks)
- **procedure**: Contains PERFORM, GO TO, flow control
- **io_operation**: Contains READ, WRITE, SQL

Then in retriever, boost `chunk_category:business_logic` for modification questions.

---

## Priority Recommendation

**Implement in this order:**

1. ✅ **Generator prompt enhancement** (easiest, immediate impact)
   - Add the business logic instructions to system prompt
   - Takes 5 minutes, no testing needed

2. ✅ **Retriever query boosting** (medium effort, high impact)
   - Add `_enhance_query_for_business_logic()` method
   - Integrate into `_search_index()`
   - Test with REGPAY question

3. ⏸️ **Two-phase retrieval** (optional, if above don't work)
   - Only if simpler changes don't improve results

4. ⏸️ **Metadata tagging** (future enhancement)
   - Would require re-indexing all chunks
   - Save for next major index rebuild

---

## Testing

After implementing changes 1 & 2, test with:

```python
from otis_rag.rag import OTISRAG

rag = OTISRAG()

# Test question
result = rag.ask("What fields can a user modify in REGPAY?", verbose=True)

print(result)
```

**Expected result:** Should mention MAX=3 and list the 3 specific fields:
1. Monies toward Paid Through Date (TOTPAYMNTD)
2. Non-contractual monies (TOTEXCPAYMNTD)
3. Regular payment amount (REGPYAMT)

**Not:** List of all 24 screen fields

---

## Summary

**No index changes needed!** The data is already there.

We just need to:
1. Tell the LLM to prioritize constraints over data definitions
2. Boost business logic keywords in search queries

These are **code-only changes** that can be made without re-indexing.
