# Fix: Copybook Names Incorrectly Treated as Program Names

## Problem

When asking "What programs use LPBW1IN.CPY", the system returned inconsistent results:
- **Sometimes 3 results** (incorrect - filtered keyword search)
- **Sometimes 57 results** (correct - semantic hybrid search)

## Root Cause

The `_extract_program_name()` method extracted **LPBW1IN.CPY** as a program name, causing `_search_index()` to apply a deterministic `program_id eq 'LPBW1IN'` filter to the `new_cobol_copybook_usage` index. This bypassed semantic search and only returned exact matches for that non-existent program.

## Solution

### 1. Updated `_extract_program_name()` (Line 324)

**Change:** Skip names ending in `.CPY` so they are never returned as program names.

```python
# Skip copybook names - they should not be treated as program names
if match.upper().endswith('.CPY'):
    logger.info(f"   ⚠️ Skipping copybook name as program filter: {match}")
    continue
return match.upper()
```

### 2. Updated `_search_index()` copybook_usage handling (Line 448)

**Change:** Only apply program_id filter if the detected name is NOT a copybook.

```python
# Special handling for 'copybook_usage' index - add filter if program name detected
# Only apply filter if the detected name is a program (.CBL), not a copybook (.CPY)
if index_name == 'new_cobol_copybook_usage' and program_name:
    # Verify it's not a copybook name being treated as a program
    if not program_name.endswith('.CPY'):
        # Add filter to find copybooks used by this program
        body["filter"] = f"program_id eq '{program_name}'"
        # ... (rest of deterministic logic)
        logger.info(f"   ✅ Filtering copybook_usage to program: {program_name}")
    else:
        # It's a copybook name - don't filter, use full semantic search
        logger.info(f"   ℹ️ Detected copybook name, using semantic search instead of program filter")
        program_name = None  # Clear to skip vector skip logic below
```

## Expected Behavior After Fix

### Copybook Usage Queries (Now Consistent)
**Query:** "What programs use LPBW1IN.CPY"
- ✅ Uses **semantic hybrid search** (vector + keyword)
- ✅ Returns **~40 programs** consistently
- ✅ Logs: "⚠️ Skipping copybook name as program filter: LPBW1IN.CPY"

### Program Copybook Queries (Unchanged)
**Query:** "What copybooks does LONPF2 use"
- ✅ Still uses **deterministic program_id filter** on LONPF2
- ✅ Returns all copybooks used by that program
- ✅ Logs: "✅ Filtering copybook_usage to program: LONPF2"

### Code Queries (Unchanged)
**Query:** "Show me code from LONPF2"
- ✅ Still uses **deterministic program_id filter** on code chunks
- ✅ No change to existing behavior

## Files Modified

- `otis_rag/retriever.py` (2 methods updated)

## Testing

Test queries to verify fix:
1. "What programs use LPBW1IN.CPY" → Should return ~40 results
2. "Which programs reference LPBW1RN.CPY" → Should use semantic search
3. "What copybooks does LONPF2 use" → Should still filter by program_id
4. "Show code from LONPF2.CBL" → Should still filter by program_id

## Actual Usage Data

**LPBW1IN.CPY is used by 40 programs across 7 directories:**
- FP (1): PSTPMC
- FX (4): FX1421, FXLIAB, FXLNSC, WR1065
- LP (25): BTPURG, BULKG2, BULKT2, BWPURG, CEASRM, CEASUP, CHLOAN, CHSSNO, CLAST2, EONIE0, LNARCH, LONIE0, LONIF1, LONPB2, LONPC2, LONPF2, LONPFF, LONPG4, LONPG6, LONPG8, LONPJ2, LONPMC, LONPN2, LONPQ1
- MN (4): BWMAN1, BWMAN2, BWMAN3, BWMAN4
- SP (10): BUYBAC, BWIMP, EMAIL, LIVEBW, PHOIMP, REDACT, SETOPT, TEXTIN, UN1065, WACCLR, WACFSB, WRLDCR (note: SP had 12 in search results but only 10 unique)
- UT (2): UTBORR (RZ1ERN commented out)
- WI (4): BWCREA, CHBORR, COSIGN, MODBOR

**Plus 3 with commented-out references:** AC1ERN, EOYUPD, RZ1ERN
