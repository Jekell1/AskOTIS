# Implementation Plan: Add Copybooks to code-chunks Index

**Date:** October 16, 2025  
**Objective:** Add 8,211 copybook files (~484K chunks) to code-chunks for complete LLM reference

---

## Phase 1: Chunking (READY TO START)

### Script: `add_copybooks_to_chunks.py`

**What it does:**
- Discovers 8,211 .CPY files in cobol_src
- Chunks each file into ~25-line segments (matching existing .CBL chunks)
- Uploads chunks to code-chunks index
- Does NOT add embeddings (Phase 2)

**Estimated:**
- Time: 20-30 minutes
- Chunks created: ~484,000
- No embeddings (vectors added separately)

**Command:**
```bash
python add_copybooks_to_chunks.py
```

**Expected output:**
```
Processed 8,211 files
Chunks created: ~484,000
Time: ~25 minutes
Rate: ~330 files/min
```

---

## Phase 2: Embedding (AFTER CHUNKING)

### Script: `backfill_copybook_chunks_embeddings.py`

**What it does:**
- Finds all chunks without embeddings (has_vector = false)
- Generates embeddings using text-embedding-3-large (1536 dimensions)
- Updates chunks with vectors
- Auto-resumes if interrupted

**Estimated:**
- Time: 4-6 hours
- Chunks to embed: ~484,000
- Rate: ~30-50 chunks/sec
- Cost: ~$150-200 for embeddings

**Command:**
```bash
python backfill_copybook_chunks_embeddings.py --batch-size 256
```

**Expected output:**
```
Found 484,000 chunks without embeddings
Estimated time: 4.5 hours
[Progress updates every 30 seconds]
SUCCESS: All chunks now have embeddings!
```

---

## Verification

### After Phase 1 (Chunking):
```python
# Check chunk counts
python -c "
import requests, os, json
with open('local.settings.json', 'r') as f:
    for k, v in json.load(f).get('Values', {}).items():
        os.environ[k] = v
        
ep = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY')
url = f'{ep}/indexes/code-chunks/docs/$count?api-version=2024-07-01'
r = requests.get(url, headers={'api-key': key})
print(f'Total chunks: {r.text}')
"
```

Expected: ~587,000 chunks (102,654 .CBL + 484,000 .CPY)

### After Phase 2 (Embedding):
```python
# Check embedding coverage
python -c "
# Check for chunks without embeddings
# Should return 0
"
```

Expected: 0 chunks missing embeddings (100% coverage)

---

## Rollback Plan (if needed)

If something goes wrong, delete copybook chunks:

```python
# Delete all .CPY chunks (NOT RECOMMENDED unless necessary)
# This would require filtering by path containing .CPY
# Better: Just leave them and re-run scripts
```

**Note:** The merge action is safe - re-running scripts will update existing chunks, not duplicate.

---

## Timeline

| Phase | Duration | Description |
|-------|----------|-------------|
| **Phase 1: Chunking** | 20-30 min | Upload ~484K chunk documents |
| **Phase 2: Embedding** | 4-6 hours | Add vectors to all chunks |
| **Verification** | 5 min | Confirm completion |
| **TOTAL** | **~5-7 hours** | Complete copybook integration |

---

## Success Criteria

After completion:

✅ code-chunks total: ~587,000 chunks  
✅ .CBL chunks: 102,654 (unchanged)  
✅ .CPY chunks: ~484,000 (new)  
✅ Embedding coverage: 100%  
✅ LLM can find copybook content via semantic search  

### Test Queries:

1. **Find data structure:**
   ```
   "SE-RECORD definition"
   → Should return LIBGB/GB01SE.CPY content
   ```

2. **Find field definitions:**
   ```
   "BI-CUST-ID PIC"
   → Should return LIBLP/LP01BI.CPY with PIC clause
   ```

3. **Find 88-levels:**
   ```
   "loan status values"
   → Should return copybooks with STATUS-ACTIVE, STATUS-CLOSED
   ```

---

## Ready to Start?

**Current status:** Scripts created and ready to run  
**Next step:** Run Phase 1 (chunking)  
**Command:** `python add_copybooks_to_chunks.py`

The script will:
1. Show plan summary
2. Ask for confirmation
3. Process all 8,211 copybooks
4. Report completion with statistics

**Proceed?** Type `yes` when prompted by the script.

---

*Implementation plan prepared: October 16, 2025*  
*Addresses critical gap in LLM reference material*
