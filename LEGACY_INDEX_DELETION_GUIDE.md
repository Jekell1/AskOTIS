# Legacy Index Deletion Guide

## Overview

This guide walks you through safely deleting 4 legacy Azure Search indexes that are superseded by newer versions.

**Indexes to delete:**
- `cobol-facts-v3` (31 docs) → superseded by new_cobol_* indexes
- `cobol-facts-v3l` (31 docs) → superseded by new_cobol_* indexes
- `cobol-symbols` (41,550 docs) → superseded by `new_cobol_symbol_refs` (1.1M docs)
- `cobol-xrefs` (63,405 docs) → superseded by multiple new_cobol_* indexes

**Impact:**
- Removes: 4 indexes, 105,017 documents
- Saves: ~1.2 GB of vector storage
- All functionality preserved in newer indexes

---

## 📋 Pre-Deletion Checklist

Before deleting, verify:

1. ✅ **New indexes are complete:**
   ```bash
   python check_all_indexes_quick.py
   ```
   Verify these are at 100%:
   - new_cobol_symbol_refs (1,104,574 docs)
   - new_cobol_flow_edges_v2 (385,121 docs) 
   - new_cobol_calls (15,788 docs)

2. ✅ **No scripts reference legacy indexes:**
   ```bash
   grep -r "cobol-facts-v3" *.py ingest/ search/
   grep -r "cobol-symbols" *.py ingest/ search/
   grep -r "cobol-xrefs" *.py ingest/ search/
   ```
   Expected: No active scripts use these

3. ✅ **Backup schemas (optional but recommended):**
   ```bash
   python backup_legacy_schemas.py
   ```
   Creates JSON backups in `index_backups/` folder

---

## 🔍 Step 1: Preview (Dry Run)

Run the preview script to see what will be deleted:

```bash
python preview_legacy_deletion.py
```

**Expected output:**
- List of 4 indexes
- Document counts (total: 105,017)
- Embedding counts (all at 100%)
- Estimated storage savings (~1.2 GB)

**✅ NO CHANGES MADE** - This is safe to run

---

## 💾 Step 2: Backup Schemas (Recommended)

Create JSON backups of index schemas:

```bash
python backup_legacy_schemas.py
```

**What it does:**
- Saves complete schema definitions to `index_backups/`
- Includes all field definitions, vector configs, semantic configs
- Allows restoration if needed

**Files created:**
- `index_backups/cobol-facts-v3_YYYYMMDD_HHMMSS.json`
- `index_backups/cobol-facts-v3l_YYYYMMDD_HHMMSS.json`
- `index_backups/cobol-symbols_YYYYMMDD_HHMMSS.json`
- `index_backups/cobol-xrefs_YYYYMMDD_HHMMSS.json`

---

## 🗑️ Step 3: Delete Legacy Indexes

**⚠️ WARNING: This action cannot be undone!**

Run the deletion script:

```bash
python delete_legacy_indexes.py
```

**What happens:**
1. Shows list of indexes to delete
2. Displays document counts
3. **Asks for confirmation** - Type `DELETE` to proceed
4. Deletes each index one by one
5. Reports success/failure for each

**Sample interaction:**
```
Type 'DELETE' to proceed, or anything else to cancel: DELETE

Deleting: cobol-facts-v3... ✅ SUCCESS
Deleting: cobol-facts-v3l... ✅ SUCCESS
Deleting: cobol-symbols... ✅ SUCCESS
Deleting: cobol-xrefs... ✅ SUCCESS

✅ Successfully deleted: 4/4 indexes
🎉 All legacy indexes successfully deleted!
   Reclaimed ~105,017 documents
```

---

## ✅ Step 4: Verify Deletion

Confirm indexes were deleted:

```bash
python analyze_all_indexes.py
```

**Expected changes:**
- Total indexes: 24 → 20
- Total documents: ~3.15M → ~3.05M
- Legacy count: 4 → 0

---

## 🔄 Rollback (If Needed)

If you need to restore a deleted index:

1. **Recreate from backup:**
   ```python
   import json, requests
   
   # Load backup
   schema = json.load(open('index_backups/cobol-symbols_TIMESTAMP.json'))
   
   # POST to create
   r = requests.put(
       f'{endpoint}/indexes/{schema["name"]}?api-version=2025-08-01-preview',
       headers={'api-key': key, 'Content-Type': 'application/json'},
       json=schema
   )
   ```

2. **Note:** Data will NOT be restored, only the schema
   - Would need to re-run original build scripts
   - Consider if truly needed vs using new indexes

---

## 📊 What's Replaced

### cobol-symbols (41k docs) → new_cobol_symbol_refs (1.1M docs)
- **Improvement:** 27x more comprehensive
- **Coverage:** All symbol types, all references
- **Embeddings:** 100% complete

### cobol-xrefs (63k docs) → Multiple new indexes
- **Flow edges:** new_cobol_flow_edges_v2 (385k edges)
- **Call relationships:** new_cobol_calls (16k calls)
- **Variable usage:** new_cobol_variable_usage (107k usages)
- **Coverage:** More detailed, better organized

### cobol-facts-v3/v3l (31 docs each) → new_cobol_* ecosystem
- **Replaced by:** Multiple specialized indexes
- **Better organization:** Facts now split by type
- **Enhanced:** More fields, better embeddings

---

## 🎯 Expected Benefits

1. **Storage savings:**
   - ~105k documents removed
   - ~1.2 GB vector storage freed

2. **Clarity:**
   - Eliminates confusion about which indexes to use
   - Clear naming: all active indexes start with `new_cobol_`

3. **Maintenance:**
   - Fewer indexes to monitor
   - No duplicate data

4. **Performance:**
   - Slightly faster Azure Search operations
   - Less resource overhead

---

## 🛡️ Safety Notes

**Why this is safe:**
1. ✅ All legacy indexes are fully superseded
2. ✅ Newer versions have MORE data, not less
3. ✅ All embeddings complete in new versions
4. ✅ Backup schemas saved before deletion
5. ✅ Confirmation required before deletion
6. ✅ No scripts actively use legacy indexes

**Not safe if:**
- ❌ New indexes are incomplete (check with step 1)
- ❌ Custom scripts still reference legacy indexes
- ❌ You have requirements for the old data structure

---

## 📞 Troubleshooting

**Error: "Index not found"**
- Index already deleted or never existed
- Safe to continue with remaining indexes

**Error: "Access denied"**
- Check API key has delete permissions
- Verify endpoint URL is correct

**Script hangs on deletion:**
- Wait up to 60 seconds (timeout)
- Azure may be processing other operations
- Try again in a few minutes

**Want to cancel mid-deletion:**
- Ctrl+C to stop script
- Partially deleted is OK (some gone, some remain)
- Re-run to continue deletion

---

## 📝 Post-Deletion Tasks

1. **Update documentation:**
   - Remove references to legacy indexes from README
   - Update architecture diagrams if any

2. **Clean up scripts:**
   - Remove any commented-out legacy index code
   - Update any documentation in scripts

3. **Monitor:**
   - Check chatbot still works correctly
   - Verify all queries return expected results

4. **Archive backups:**
   - Keep `index_backups/` folder for 30 days
   - Delete after confirming no issues

---

## ✨ Summary

**Quick deletion process:**
```bash
# 1. Preview what will be deleted
python preview_legacy_deletion.py

# 2. Backup schemas (optional)
python backup_legacy_schemas.py

# 3. Delete (requires typing 'DELETE')
python delete_legacy_indexes.py

# 4. Verify
python analyze_all_indexes.py
```

**Result:** Clean, modern index architecture with only `new_cobol_*` indexes!
