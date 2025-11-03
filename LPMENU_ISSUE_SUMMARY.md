# LPMENU MASTER MENU Not Appearing in screen_nodes Index - Troubleshooting Summary

## Problem Statement

The OTIS application's main user menu (LPMENU MASTER MENU) is not appearing in the `new_cobol_screen_nodes` Azure Search index, even though:
1. The source file `cobol_src/LIBLP/LPMENU_SCN.CPY` exists on disk
2. It has been successfully ingested into the `new_code_chunks` index
3. The extraction script `build_screen_nodes.py` is fetching LPMENU chunks
4. Debug simulation shows screens WOULD be created with the correct content

## Expected Behavior

When users query the RAG with "What are the main menu options for OTIS?", they should see:
```
M A S T E R     M E N U
1. DAILY PROCESSING
2. REPORTS
3. INQUIRIES
4. COLLECTION PROCESSING
5. BATCH PROCESSING
6. END OF DAY
7. END OF MONTH
8. END OF YEAR
9. SPECIAL PROCEDURES
10. OPTIONAL MODULES
```

## Actual Behavior

RAG returns a different menu (USER TRANSACTION MAINTENANCE menu) instead of the MASTER MENU. LPMENU screens are not found in the `new_cobol_screen_nodes` index under any search query or hash ID.

## Work Completed

### 1. Successfully Ingested Copybooks into code_chunks
- Modified `ingest_code_chunks.py` to handle 207 HTTP status codes (partial success responses)
- Ran ingestion with `--include-copybooks` flag
- Result: 265,652 total documents in `new_code_chunks` (up from 136,304)
- Verified LPMENU_SCN.CPY is present:
  ```
  Found 12 chunks for LPMENU_SCN.CPY
  - Lines 1-60, 26-50, 51-75, etc.
  - Contains "M A S T E R     M E N U" text
  - Contains all 10 menu options
  ```

### 2. Modified build_screen_nodes.py to Fetch LPMENU
Added targeted fetch for important copybooks:
```python
# Lines 198-207 in build_screen_nodes.py
important_copybooks = ['LPMENU_SCN.CPY', 'PGMENU_SCR1.CPY', 'FDMENU_SCN.CPY', 'ASMENU_SCN.CPY']
for copybook_name in important_copybooks:
    specific_chunks = fetch_chunks(ep, key, top=50, phrase=copybook_name)
    if specific_chunks:
        print(f'  + Fetched {len(specific_chunks)} chunks for {copybook_name}')
        copybook_chunks.extend(specific_chunks)
```

Result when running `python build_screen_nodes.py --include-copybooks`:
```
Fetched copybook chunks: 1000 phrase=LABEL LINE
  + Fetched 12 chunks for LPMENU_SCN.CPY
Fetched candidate chunks: 2022 phrase= SCREEN SECTION
Uploaded 2012 screen nodes
```

### 3. Confirmed Extraction Logic Works
Created simulation script `simulate_lpmenu_processing.py` that mimics build_screen_nodes processing:
```
Processing LPMENU_SCN.CPY
Program ID (hash): 9e62bc4c1d02b643a48147d509e3406dd916a8ef

Extracted 14 labels:
  Line 04 Col 29: M A S T E R     M E N U
  Line 08 Col 11: 1. DAILY PROCESSING
  Line 08 Col 49: 6. END OF DAY
  ... (10 menu options total)

✅ Would create screen node with 13 menu items
```

### 4. Debug Processing Shows Screens Would Be Created
Ran `debug_lpmenu_processing.py` which processes actual chunks from the index:
```
Fetched 12 LPMENU_SCN chunks
From 4 unique file paths:
  1778ece8d33afbb3... -> cobol_src\LIBLP\LPMENU_SCN.CPY
  7d8c4378a816f43b... -> cobol_src/LIBLP/LPMENU_SCN.CPY  (forward slash)
  9e62bc4c1d02b643... -> cobol_src\LIBLP\LPMENU_SCN.CPY  (backslash)
  d5ceadb3aafb528d... -> cobol_src\LP\LPMENU.CBL

✅ Processing chunk: 1f707cac47d8ed45...
   Program ID: 7D8C4378A816F43B...
   Has LABEL LINE: True
   Labels extracted: 14
   First 5 labels:
     Line 01 Col 63: Posting for:
     Line 04 Col 29: M A S T E R     M E N U
     Line 08 Col 11: 1. DAILY PROCESSING
     Line 08 Col 49: 6. END OF DAY
     Line 10 Col 11: 2. REPORTS

Summary: Would create 4 screen(s) from 12 chunks
```

### 5. Verified Screens NOT in Index
Searched `new_cobol_screen_nodes` with all possible hash IDs:
```python
hashes = [
    '1778ece8d33afbb3a4814d8a992a8dd54632dc92',
    '7d8c4378a816f43b483af36f4f9e86efe03e9b87',
    '9e62bc4c1d02b643a48147d509e3406dd916a8ef'
]
# Result: ❌ No screens found with any hash variation
```

Also searched by content:
- Search: "MASTER MENU" → 0 results
- Search: "DAILY PROCESSING REPORTS INQUIRIES" → 0 results  
- Search: "*" with filter on program_id → 0 results

## Root Cause Analysis

### Known Facts:
1. ✅ LPMENU_SCN.CPY file exists on disk (4,560 bytes)
2. ✅ File has 14 LABEL LINE statements with menu text
3. ✅ File is in `new_code_chunks` index (12 chunks found)
4. ✅ `build_screen_nodes.py` fetches LPMENU chunks (confirmed in output)
5. ✅ Extraction regex works correctly (simulations confirm)
6. ✅ Labels are extracted during processing (debug confirms 14 labels found)
7. ❌ Screens do NOT appear in `new_cobol_screen_nodes` index

### Suspected Issues:

**Issue A: Path Separator Inconsistency**
- Same file has 4 different program_id hashes due to path separator variations:
  - `cobol_src\LIBLP\LPMENU_SCN.CPY` (backslash)
  - `cobol_src/LIBLP/LPMENU_SCN.CPY` (forward slash)
- This creates different hash IDs for the same logical file
- May cause deduplication issues in `build_screen_nodes.py` line 234-237:
  ```python
  h=hashlib.sha1(sec.encode('utf-8','ignore')).hexdigest()
  dup_key=f"{prog}:{h}"
  if dup_key in seen_section_hash:
      continue  # skip duplicate section
  ```

**Issue B: Silent Upload Failure**
- Debug shows "Would create 4 screen(s)" but `find_lpmenu_any_hash.py` finds 0 screens
- This suggests screens are created in memory but not uploaded
- The upload function may be silently failing or filtering them out
- No errors are thrown during build process

**Issue C: Azure Search Pagination Limit**
- `fetch_chunks()` uses pagination but Azure Search limits skip+top to ~10,000 total
- Generic "LABEL LINE" search only returns 5,485 copybook chunks even with limit=50,000
- This is why targeted fetch by name was added (workaround implemented)
- However, this exposes that many copybooks are unreachable via search

## Technical Details

### File Content Sample
From `cobol_src/LIBLP/LPMENU_SCN.CPY`:
```cobol
 01  LPMENU-SCREEN.
     03  LABEL LINE 01 COL 63
      "Posting for:"
                                        LOW.
     03  LABEL LINE 04 COL 29
      "M A S T E R     M E N U"
                                        BOLD.
     03  LABEL LINE 08 COL 11
      "1. DAILY PROCESSING"
                                        LOW.
     03  LABEL LINE 08 COL 49
      "6. END OF DAY"
                                        LOW.
```

### Extraction Regex (Working Correctly)
From `build_screen_nodes.py` lines 134-137:
```python
label_pattern = re.compile(
    r'LABEL\s+LINE\s+(\d+)\s+COL\s+(\d+)\s*\n?\s*["\']([^"\']+)["\']',
    re.IGNORECASE | re.MULTILINE
)
```
Confirmed working: extracts all 14 labels from LPMENU_SCN.CPY

### Processing Logic
From `build_screen_nodes.py` lines 216-229:
```python
for ch in chunks:
    prog=(ch.get('program_id') or '').upper()
    content=ch.get('text') or ''
    
    # Check if content has SCREEN SECTION or LABEL LINE (copybooks)
    has_screen_section = 'SCREEN SECTION' in content.upper()
    has_label_line = 'LABEL LINE' in content.upper()
    
    if not has_screen_section and not has_label_line:
        continue
    
    # Extract sections
    if has_screen_section:
        sections=extract_screen_sections(content)
    else:
        # Copybook: treat entire content as one section
        sections=[content]
```

### Upload Function
From `build_screen_nodes.py` lines 349-360:
```python
def upload(ep, key, docs):
    if not docs:
        print('No documents to upload.')
        return
    url = f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VER}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    payload = {'value': [{'@search.action': 'upload', **doc} for doc in docs]}
    r = requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200, 201):
        raise RuntimeError(f"Upload failed: {r.status_code} {r.text[:500]}")
    print(f'Uploaded {len(docs)} screen nodes')
```

## Questions for Investigation

1. **Why are screens created in memory (confirmed by debug) but not appearing in the index after upload?**
   - Is the upload function being called with empty docs list for LPMENU screens?
   - Are LPMENU screens being filtered out before upload?
   - Is there a size limit or schema validation failing silently?

2. **How to handle path separator inconsistency?**
   - Should program_id hash normalize path separators before hashing?
   - Are duplicate program_ids causing conflicts during upload?
   - Should deduplication use a different key than program_id:content_hash?

3. **Why does the build report "Uploaded 2012 screen nodes" but LPMENU isn't among them?**
   - What are the 2012 screens that were uploaded?
   - Can we add verbose logging to see which screens are in the upload batch?
   - Is there pre-upload filtering happening?

4. **Is there a silent failure in the upload response?**
   - Azure Search returns 207 for partial success - is that being handled in upload()?
   - Are some documents failing validation and being skipped?
   - Should we log the full upload response to see per-document status?

## Reproduction Steps

1. Verify LPMENU in code_chunks:
   ```bash
   python check_liblp_copybooks.py
   # Should show: ❌ LPMENU_SCN.CPY is NOT in code chunks (if using old index)
   
   python verify_lpmenu_ingested.py  
   # Should show: ✅ FOUND 5 chunks (if in new_code_chunks)
   ```

2. Run screen extraction:
   ```bash
   python build_screen_nodes.py --include-copybooks
   # Output shows: + Fetched 12 chunks for LPMENU_SCN.CPY
   #               Uploaded 2012 screen nodes
   ```

3. Verify LPMENU not in index:
   ```bash
   python find_lpmenu_screen.py
   # Result: ❌ No screens found for LPMENU_SCN.CPY
   ```

4. Test RAG:
   ```bash
   python test_pgmenu.py
   # Returns wrong menu (USER TRANSACTION MAINTENANCE instead of MASTER MENU)
   ```

## Files to Review

Key files in repository:
- `build_screen_nodes.py` - Main extraction script (lines 195-360 critical)
- `ingest_code_chunks.py` - Copybook ingestion (modified to handle 207 status)
- `cobol_src/LIBLP/LPMENU_SCN.CPY` - Source file with MASTER MENU
- `simulate_lpmenu_processing.py` - Confirms extraction logic works
- `debug_lpmenu_processing.py` - Shows screens would be created
- `find_lpmenu_any_hash.py` - Proves screens not in index

## Environment

- Azure Cognitive Search indexes:
  - `new_code_chunks`: 265,652 documents (includes copybooks)
  - `new_cobol_screen_nodes`: 2,012 documents (missing LPMENU)
- Python version: 3.x
- Index API version: 2024-07-01
- Search action: 'upload' (not mergeOrUpload in screen_nodes upload function)

## Debugging Suggestions

1. Add verbose logging to `build_screen_nodes.py` upload():
   ```python
   print(f"Preparing to upload {len(docs)} docs")
   for doc in docs:
       if 'MASTER' in doc.get('summary_text', '').upper():
           print(f"  MASTER MENU found: {doc['screen_id']}")
   ```

2. Check if LPMENU screens are in the docs list before upload:
   ```python
   lpmenu_screens = [d for d in docs if 'DAILY PROCESSING' in d.get('summary_text', '')]
   print(f"LPMENU screens before upload: {len(lpmenu_screens)}")
   ```

3. Normalize paths in program_id calculation (ingestion_common.py):
   ```python
   program_id = stable_hash([str(fp).replace('\\', '/')])  # Normalize to forward slash
   ```

4. Handle 207 responses in screen_nodes upload (like ingest_code_chunks.py):
   ```python
   if r.status_code == 207:
       result = r.json()
       errors = [item for item in result.get('value', []) if not item.get('status')]
       if errors:
           print(f"⚠️ Warning: {len(errors)} docs failed upload")
   ```

## Success Criteria

Solution is complete when:
1. ✅ Query RAG: "What are the main menu options for OTIS?"
2. ✅ Response includes: "1. DAILY PROCESSING", "2. REPORTS", "3. INQUIRIES", etc.
3. ✅ `python find_lpmenu_screen.py` returns screens with MASTER MENU content
4. ✅ Screen_nodes index contains screens with program_id hash for LPMENU_SCN.CPY

---

## Request for Next LLM

Please analyze this issue and determine:

1. **Why are LPMENU screens not appearing in the index despite being processed?**
   - Review the build_screen_nodes.py flow from chunk fetch → extraction → upload
   - Identify where screens are being lost or filtered out
   - Check for silent failures in upload or deduplication logic

2. **What is the fix?**
   - Provide specific code changes with line numbers
   - Explain the root cause clearly
   - Include verification steps to confirm the fix works

3. **Are there systemic issues with copybook-based screens?**
   - Is LPMENU the only affected screen or are others missing too?
   - Should path normalization be added to prevent hash inconsistencies?
   - Should upload function handle partial failures (207 responses)?

The goal is to get LPMENU MASTER MENU retrievable by the RAG system so users can see the main menu options when they ask.
