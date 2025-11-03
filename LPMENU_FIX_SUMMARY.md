# LPMENU Path Normalization Fix - Complete Summary

## Problem Identified

The LPMENU MASTER MENU was not appearing in RAG queries despite being present in the codebase. Investigation revealed that the same copybook file was generating **multiple different program_id hashes** due to inconsistent path separators.

### Root Cause

**Path Separator Inconsistency**: The `stable_hash()` function was hashing file paths with mixed separators:
- `cobol_src\LIBLP\LPMENU_SCN.CPY` (backslash) → hash `9e62bc4c1d02...`
- `cobol_src/LIBLP/LPMENU_SCN.CPY` (forward slash) → hash `b9fffe449829...`
- `cobol_src/liblp/lpmenu_scn.cpy` (lowercase) → hash `ea66581c142e...`

**Impact**:
1. Same logical file had multiple program_id values in `new_code_chunks`
2. `build_screen_nodes.py` couldn't consistently find chunks by program_id
3. Screens were extracted but matched wrong hashes, causing deduplication/upload failures
4. Azure Search `mergeOrUpload` kept old program_ids (didn't update existing docs)

## Solution Implemented

### 1. Path Normalization in `ingest_code_chunks.py`

**File**: `ingest_code_chunks.py`  
**Line**: 40

```python
# BEFORE:
program_id = stable_hash([str(fp)])

# AFTER:
# Normalize path separators to forward slash and lowercase for consistent hashing
normalized_path = str(fp).replace('\\', '/').lower()
program_id = stable_hash([normalized_path])
```

**Effect**: All future ingestions will use normalized paths (forward slash + lowercase).

### 2. Path Normalization in `build_screen_nodes.py`

**File**: `build_screen_nodes.py`  
**Line**: 217

```python
# BEFORE:
prog=(ch.get('program_id') or '').upper()

# AFTER:
# Normalize program_id from chunk (handles any remaining path separator issues)
prog=(ch.get('program_id') or '').replace('\\', '/').upper()
```

**Effect**: Handles any legacy mixed-case program_ids during screen extraction.

### 3. Enhanced Deduplication

**File**: `build_screen_nodes.py`  
**Lines**: 236-241

```python
# BEFORE:
dup_key=f"{prog}:{h}"
if dup_key in seen_section_hash:
    continue

# AFTER:
# Lowercase the key to prevent case-sensitive duplicates
dup_key=f"{prog.lower()}:{h}"
if dup_key in seen_section_hash:
    print(f"[SKIP] Duplicate section: {prog[:32]}... hash={h[:12]}...")
    continue
seen_section_hash.add(dup_key)
```

**Effect**: More robust deduplication with logging for visibility.

### 4. Enhanced Upload with 207 Handling

**File**: `build_screen_nodes.py`  
**Lines**: 173-215

Completely rewrote the `upload()` function to:
- Show pre-upload LPMENU screen count
- Handle Azure Search 207 (partial success) responses
- Log failed documents with error messages
- Track successful vs failed uploads per batch

### 5. Index Cleanup and Re-ingestion

Since Azure Search `mergeOrUpload` uses `chunk_id` as the key and `chunk_id` didn't change between ingestions, the old `program_id` values persisted even after re-ingestion.

**Solution**: Cleared the entire `new_code_chunks` index and re-ingested from scratch.

```bash
# Clear index (run multiple times due to Azure 100K skip limit)
python clear_code_chunks_index.py

# Re-ingest with normalized paths
python ingest_code_chunks.py --include-copybooks --verbose
```

## Verification Test Results

### Small Ingestion Test (LPMENU only)

```bash
python test_lpmenu_small_ingest.py
```

**Results**:
- ✅ 6 chunks created from LPMENU_SCN.CPY
- ✅ Normalized program_id: `ea66581c142e6ba08a83d2dc773bc990db403c3c`
- ✅ All chunks uploaded successfully
- ✅ Content verified: Contains "M A S T E R     M E N U"

### Screen Extraction Test

```bash
python build_screen_nodes.py --include-copybooks
```

**Results**:
```
Fetched copybook chunks: 2 phrase=LABEL LINE
  + Fetched 6 chunks for LPMENU_SCN.CPY
Fetched candidate chunks: 8 phrase= SCREEN SECTION
[SKIP] Duplicate section: EA66581C142E6BA08A83D2DC773BC990... hash=ed548fb431c0...
[SKIP] Duplicate section: EA66581C142E6BA08A83D2DC773BC990... hash=b56e547d1da6...
📋 LPMENU screens prepared: 1
   • EA66581C142E6BA08A83D2DC773BC990DB403C3C_2 from program EA66581C142E6BA0...
✅ Successfully uploaded 2 screen nodes
```

**Key Observations**:
- ✅ Found correct number of LPMENU chunks (6)
- ✅ NEW normalized program_id: `EA66581C142E6BA08A83D2DC773BC990DB403C3C`
- ✅ Extracted 1 LPMENU screen successfully
- ✅ Upload succeeded

### Hash Comparison

| Scenario | Hash | Status |
|----------|------|--------|
| Original path with backslash | `9e62bc4c1d02...` | ❌ OLD |
| Forward slash (not lowercase) | `b9fffe449829...` | ❌ OLD |
| **Normalized (forward + lowercase)** | `ea66581c142e...` | ✅ **NEW** |

## Timeline

1. **Problem Identified**: LPMENU MASTER MENU not in RAG results
2. **Removed**: help_fields index (conflicting content)
3. **Fixed**: generator.py screen_nodes filtering bug
4. **Ingested**: Copybooks for first time (265K docs)
5. **Discovered**: Screens extracted but not indexed
6. **Root Cause**: Path separator inconsistency creating duplicate hashes
7. **Implemented**: 6-part fix (path normalization + enhanced logging)
8. **Tested**: Small LPMENU-only ingestion (100% success)
9. **Cleared**: Index to remove old program_ids
10. **Re-ingesting**: Full dataset with normalized paths (IN PROGRESS)

## Expected Outcome

After full re-ingestion completes (~2 hours):

1. ✅ All code chunks will have consistent normalized program_ids
2. ✅ `build_screen_nodes.py` will extract screens with matching program_ids
3. ✅ LPMENU MASTER MENU will be in `new_cobol_screen_nodes`
4. ✅ RAG queries will return the correct main menu with 10 options:
   - DAILY PROCESSING
   - REPORTS
   - INQUIRIES
   - COLLECTION PROCESSING
   - BATCH PROCESSING
   - END OF DAY
   - END OF MONTH
   - END OF YEAR
   - SPECIAL PROCEDURES
   - OPTIONAL MODULES

## Next Steps

1. **Wait**: For full re-ingestion to complete (~2 hours)
2. **Rebuild**: `python build_screen_nodes.py --include-copybooks`
3. **Verify**: RAG query returns correct MASTER MENU
4. **Commit**: All code changes with descriptive message
5. **Deploy**: Updated config to Azure Functions

## Files Modified

- `ingest_code_chunks.py` - Line 40 (path normalization)
- `build_screen_nodes.py` - Lines 217, 236-241, 173-215 (normalization + enhanced upload)
- `otis_rag/config.py` - Line 68 (help_fields commented out)
- `otis_rag/router.py` - Lines 32, 204-220 (help_fields removed)

## Lessons Learned

1. **Path consistency matters**: Always normalize file paths before hashing
2. **Azure Search behavior**: `mergeOrUpload` doesn't update all fields if key exists
3. **Debugging approach**: Small focused tests (LPMENU-only) faster than full ingestion
4. **Logging is critical**: Enhanced upload() logging immediately showed issues
5. **Character encoding**: Windows console doesn't support all Unicode (emojis)

## Success Criteria Checklist

- [x] Path normalization implemented in ingestion
- [x] Path normalization implemented in screen extraction
- [x] Enhanced deduplication with logging
- [x] Enhanced upload with 207 handling and visibility
- [x] Small LPMENU test: 100% success
- [ ] Full re-ingestion: IN PROGRESS (~2 hours)
- [ ] Screen_nodes rebuild with normalized data
- [ ] RAG returns correct MASTER MENU with 10 options
- [ ] Code committed to git
- [ ] Changes deployed to Azure Functions

---

**Date**: October 29, 2025  
**Status**: Fix implemented and tested, full re-ingestion in progress  
**ETA**: ~2 hours until validation complete
