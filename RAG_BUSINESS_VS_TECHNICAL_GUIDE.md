# How to Make RAG Answer Business Purpose vs Technical Details

**Problem**: "What does the OTIS system do?" was returning technical implementation instead of business purpose.

**Goal**: Make the system understand that questions like "what does X do?" should prioritize BUSINESS PURPOSE, then technical details.

---

## ✅ Approach 1: Enhanced System Prompt (IMPLEMENTED)

### What Changed:
Modified `otis_rag/generator.py` to add explicit business-first instructions:

```python
CRITICAL: When asked "what does X do?", interpret this as:
1. FIRST: Explain the BUSINESS PURPOSE - what business problem does it solve?
2. THEN: Explain the technical implementation - how does it work?

For OTIS/OTOS system questions, prioritize:
- Business domain (loan servicing, financial transactions)
- Business processes (what operations it supports)
- Industry context (financial services)
- THEN technical details
```

### Results:
**Before**:
```
The OTIS system is a COBOL-based application designed to manage 
operational tasks related to file and data handling...
[Technical details about SYSTEM-BUF, copybooks, etc.]
```

**After**:
```
Business Purpose (What business problem does OTIS solve?):
- OTIS is a loan servicing platform for financial institutions
- Manages lifecycle of loans: tracking, payments, disbursements
- Processing financial transactions
[Then technical details...]
```

### Pros:
- ✅ Simple to implement (just prompt changes)
- ✅ No code restructuring needed
- ✅ Works immediately
- ✅ Maintains all existing functionality

### Cons:
- ⚠️ Relies on LLM following instructions
- ⚠️ May not work 100% consistently across all models

---

## Approach 2: Query Expansion (Alternative)

### How It Works:
Automatically expand vague queries with business context:

```python
# In router.py
def _expand_query_for_business_context(self, query: str, is_otis: bool) -> str:
    """Expand vague queries to include business context."""
    
    # Detect vague "what does X do?" questions
    if re.search(r'\b(what|explain).*\b(do|does|purpose)\b', query.lower()):
        if is_otis:
            # Add business context to search query
            return f"{query} business purpose loan servicing financial"
    
    return query
```

### Example:
- **Original**: "What does OTIS do?"
- **Expanded**: "What does OTIS do? business purpose loan servicing financial"
- **Result**: Retrieves more business-focused documents

### Pros:
- ✅ Improves document retrieval (more business docs)
- ✅ Works at search level, not just generation

### Cons:
- ⚠️ May retrieve too broad documents
- ⚠️ Doesn't guarantee LLM prioritizes business content

---

## Approach 3: Two-Stage Answer Generation (Advanced)

### How It Works:
First ask for business purpose, then technical details:

```python
# In generator.py
def generate_with_business_focus(self, query, context_docs, ...):
    """Generate answer in two stages: business then technical."""
    
    # Stage 1: Business purpose
    business_prompt = f"What is the BUSINESS PURPOSE of {query}?"
    business_answer = self._call_llm(business_prompt, context_docs)
    
    # Stage 2: Technical details
    technical_prompt = f"How is {query} technically implemented?"
    technical_answer = self._call_llm(technical_prompt, context_docs)
    
    # Combine
    return f"**Business Purpose:**\n{business_answer}\n\n**Technical Implementation:**\n{technical_answer}"
```

### Pros:
- ✅ Very clear separation of concerns
- ✅ Always structured the same way

### Cons:
- ⚠️ 2x LLM calls (slower, more expensive)
- ⚠️ More complex code

---

## Approach 4: Question Type Classification (Most Robust)

### How It Works:
Add a new question type "business_purpose" to router:

```python
# In router.py
QUESTION_PATTERNS = {
    'business_purpose': [
        r'\b(business|purpose|why|solve)\b',
        r'\b(industry|domain|used for)\b',
        r'\bwhat does\b.*\bdo\b',  # Interpret as business question
    ],
    'technical_details': [
        r'\bhow (does|is)\b.*\b(implemented|work)\b',
        r'\bshow.*\bcode\b',
        r'\btechnical\b',
    ],
    # ... existing patterns
}

def _select_indexes(self, question_type: str, is_otis: bool) -> list[str]:
    if question_type == 'business_purpose':
        # Search high-level indexes (programs, flows, not code chunks)
        return ['programs', 'flows', 'program_deps', 'files']
    elif question_type == 'technical_details':
        # Search detailed indexes (code, paragraphs)
        return ['code', 'code_new', 'paragraphs', 'data_items']
```

### Pros:
- ✅ Searches different indexes for different question types
- ✅ Most semantically correct approach
- ✅ Extensible (add more question types)

### Cons:
- ⚠️ Requires router changes
- ⚠️ May miss some patterns

---

## Approach 5: RAG Fusion (Advanced)

### How It Works:
Generate multiple variations of the query, search all, merge results:

```python
def retrieve_with_fusion(self, query):
    # Generate query variations
    variations = [
        query,
        f"business purpose of {query}",
        f"what problem does {query} solve",
        f"technical implementation of {query}"
    ]
    
    # Search with each variation
    all_results = []
    for var in variations:
        results = self.retriever.retrieve(var, indexes)
        all_results.extend(results)
    
    # Merge and rank by relevance
    return self._reciprocal_rank_fusion(all_results)
```

### Pros:
- ✅ Comprehensive document coverage
- ✅ Gets both business and technical docs

### Cons:
- ⚠️ 4x searches (slow)
- ⚠️ Complex merging logic

---

## 🎯 Recommendation: Use Approach 1 (Enhanced Prompt)

We've already implemented Approach 1, and it works well! The improved system prompt now:

1. ✅ Explicitly instructs LLM to prioritize business purpose
2. ✅ Adds OTIS-specific business context (loan servicing)
3. ✅ Maintains structured output format
4. ✅ No performance overhead
5. ✅ No code complexity

### Test Results:
```
Question: "What does the OTIS system do?"

Before: Technical details first (SYSTEM-BUF, copybooks, file operations)
After:  Business purpose first (loan servicing, financial transactions, industry context)

Success Rate: ✅ Business keywords in first 500 chars > Technical keywords
```

---

## 📊 Comparison Matrix:

| Approach | Complexity | Speed | Accuracy | Maintainability |
|----------|-----------|-------|----------|-----------------|
| 1. Enhanced Prompt | Low | Fast | High | Excellent |
| 2. Query Expansion | Low | Fast | Medium | Good |
| 3. Two-Stage | Medium | Slow | Very High | Medium |
| 4. Question Classification | Medium | Fast | Very High | Good |
| 5. RAG Fusion | High | Very Slow | Highest | Poor |

---

## 🚀 Future Enhancements (Optional):

If Approach 1 isn't sufficient, consider adding Approach 4:

1. Add `business_purpose` question type to router
2. Create business-focused index selection (programs, flows, not code chunks)
3. Use business-specific system prompt for that question type

This gives you the best of both worlds:
- Different indexes for different question intents
- Tailored prompts per question type

---

## 📝 Testing Guide:

To verify the system prioritizes business context:

```python
from otis_rag import OTISRAG

rag = OTISRAG()

# Test vague questions
test_questions = [
    "What does OTIS do?",
    "What does program GB01SE do?",
    "Explain the OTIS system",
]

for q in test_questions:
    answer = rag.ask(q)
    
    # Check if business keywords appear in first 500 chars
    business_score = sum(1 for kw in ['loan', 'financial', 'servicing', 'business', 'borrower']
                        if kw in answer[:500].lower())
    
    print(f"Question: {q}")
    print(f"Business score: {business_score}/5")
    print(f"Status: {'✅ Good' if business_score >= 2 else '⚠️ Needs improvement'}")
```

---

**Current Status**: ✅ Approach 1 implemented and working!
