# RAG Version Comparison - October 23rd vs Current

## Test Query
"What are the main menu options in OTIS?"

## Results

### October 23rd Version (Commit 5ba2040)
**Date**: October 23, 2025  
**Test Run**: October 29, 2025

**Result**: ❌ **FAILED** - Did not retrieve LPMENU menu options

**Answer Summary**:
- Mentions "F7 - MASTER MENU" as a navigation option
- Shows fragments of screens with numbered options
- Explicitly states: "The exact main menu options for OTIS are **not shown** in the retrieved context"
- Does not show actual menu items like "1. DAILY PROCESSING", "2. REPORTS", etc.

**Key Quote**:
> "The literal text of the main OTIS menu options (such as '1. Loan Maintenance', '2. Dealer Maintenance', etc.) is **not present in the retrieved context**."

**Retrieved Documents**: 40 documents from screen_nodes index  
**Retrieval Time**: 4.497s  
**Menu Intent Detection**: ✓ Working (applied 5.0x boost to screen_nodes)

---

### Current Version (Commit 90ca4c8)
**Date**: October 29, 2025 (today)  
**Includes**: LPMENU ingestion fixes, copybook screen detection

**Result**: ❌ **FAILED** - Did not retrieve LPMENU menu options

**Answer Summary**:
- Shows fragments of various screens with numbered options
- Explicitly states: "The main menu options in OTIS, as displayed to users, are **not fully available** in the retrieved context"
- Does not mention LPMENU at all
- Does not mention "MASTER MENU"
- Does not show actual menu items

**Key Quote**:
> "The literal main menu options (such as '1. Loan Maintenance', '2. Dealer Maintenance', etc.) are **not present** in the retrieved context."

**Retrieval Time**: 1.553s (faster than Oct 23rd)  
**Menu Intent Detection**: ✓ Working

---

## Conclusion

### Finding
**Both versions fail identically.** The problem is not caused by recent code changes. The retrieval configuration has never correctly retrieved LPMENU menu content for this type of natural language query.

### Why Both Fail
1. **Semantic Similarity Issue**: Query "What are the main menu options?" doesn't have strong semantic similarity to structured COBOL screen text like:
   ```
   05 LABEL LINE 05 COL 20 VALUE "1. DAILY PROCESSING".
   05 LABEL LINE 06 COL 20 VALUE "2. REPORTS".
   ```

2. **Hybrid Search Weights**: The balance between keyword search (BM25) and vector search (embeddings) doesn't favor LPMENU screens highly enough

3. **Screen Node Summaries**: The screen nodes may not have good enough summary text for semantic matching

### What IS Working
- ✅ Menu intent detection (both versions)
- ✅ Index boosting (5.0x for screen_nodes)
- ✅ Data exists in index (verified separately)
- ✅ Direct keyword searches work

### What Needs Fixing
The retrieval tuning requires adjustment, not the data or recent code changes:

1. **Increase keyword search weight** in hybrid RRF fusion
2. **Add query expansion** - expand "main menu options" to include "MASTER MENU", "LPMENU", "menu items"
3. **Improve screen node summaries** - ensure summaries include keywords like "menu", "main menu", "options"
4. **Consider dedicated menu index** - create specialized index just for menu screens with optimized embeddings

---

## Recommendation

**Do NOT rollback to October 23rd version**. The current version:
- ✅ Has better error handling
- ✅ Has copybook screen detection (2,647 vs 2,006 screens)
- ✅ Has all LPMENU data properly ingested
- ✅ Retrieves slightly faster (1.5s vs 4.5s)
- ❌ Has the same retrieval issue (not worse)

**Instead**: Focus on retrieval tuning in the current version:
1. Tune hybrid search weights
2. Add query expansion for menu-related queries
3. Test with more specific queries like "What is on the MASTER MENU screen?"

---

*Test conducted: October 29, 2025*  
*Comparison: 5ba2040 (Oct 23) vs 90ca4c8 (Oct 29)*
