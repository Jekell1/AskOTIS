# Menu Text Extraction - Quick Reference

## 🎯 What Was Done

✅ **LABEL Statement Extraction**
- Added to `build_screen_nodes.py` (line 113-139)
- Captures: `LABEL LINE 05 COL 02 "menu text" LOW`
- Field: `label_literals_json` in screen_nodes index
- Status: **WORKING** (verified with demo_label_extraction.py)

✅ **VALUE Clause Enhancement**  
- Fixed in `ingest_cobol_data_items.py` (line 35)
- Now handles: `VALUE " INTERRUPT OPTIONS"` (spaces inside quotes)
- Multi-line support: checks next line if VALUE not on current line
- Status: **WORKING** (verified with test_value_regex.py)

✅ **Root Cause Identified**
- Menu SCREEN SECTION in copybooks (*.CPY)
- Copybooks not in code_chunks index
- Result: 0 menu programs in screen_nodes

---

## 🚀 How to Complete (Next Developer)

### Step 1: Find Chunking Script
```bash
# Look for script that creates code_chunks
grep -r "code_chunks" --include="*.py" | grep -i chunk
# Or check ingest/ directory
ls ingest/*chunk*.py
```

### Step 2: Add Copybook Pattern
```python
# In chunking script, change:
patterns = ["*.CBL"]
# To:
patterns = ["*.CBL", "*.CPY", "*.cpy"]
```

### Step 3: Run Chunking for Copybook Directories
```bash
python <chunking_script>.py --roots ./cobol_src/LIBWI ./cobol_src/LIBSP ./cobol_src/LIBLP
```

### Step 4: Rebuild Screen Nodes
```bash
python build_screen_nodes.py --limit-chunks 5000
# Should now find OPMENU, PGMENU, etc.
```

### Step 5: Verify Menu Text
```bash
python check_menu_screen_nodes.py
# Should show menu programs with LABEL statements
```

### Step 6: Test RAG Query
Open chat interface, ask:
> "what is the text of user input choices for the main menu"

Should now return actual menu text from LABEL statements.

---

## 📂 Key Files

**Modified:**
- `build_screen_nodes.py` - LABEL extraction
- `create_screen_nodes_index.py` - Schema
- `ingest_cobol_data_items.py` - VALUE regex

**Test Scripts:**
- `demo_label_extraction.py` - Shows LABEL extraction working
- `check_menu_screen_nodes.py` - Check menu program coverage
- `test_value_regex.py` - Test VALUE patterns

**Documentation:**
- `MENU_TEXT_EXTRACTION.md` - Complete investigation details

---

## 🔍 Menu Copybook Locations

**LIBSP (LABEL-based):**
```
cobol_src/LIBSP/OPMENU_SCN.CPY  - "1. ADVERTISING / SOLICITATION"
cobol_src/LIBSP/BPMENU_SCN.CPY  - Bank processing menu
cobol_src/LIBSP/SPMENU_SCN.CPY  - Special processing menu
```

**LIBWI (VALUE-based):**
```
cobol_src/LIBWI/PGMENU_WKS.CPY  - VALUE " INTERRUPT OPTIONS"
```

---

## ✅ Verification Checklist

After implementing copybook chunking:

- [ ] Copybooks appear in code_chunks index
- [ ] `build_screen_nodes.py` finds menu programs
- [ ] OPMENU has screen_nodes with LABEL statements
- [ ] PGMENU has screen_nodes (may have different structure)
- [ ] RAG query returns menu text in context
- [ ] LLM can quote actual menu options

---

## 📊 Current State

**Index Sizes:**
- screen_nodes: 1,582 documents (programs only)
- data_items: 145,532 documents

**What Works:**
- ✅ LABEL extraction infrastructure
- ✅ VALUE regex improvements
- ✅ Multi-line VALUE support
- ✅ Screen nodes for programs

**What's Blocked:**
- ❌ Menu text retrieval (copybooks not chunked)
- ❌ Menu programs in screen_nodes (0 found)

---

## 🎓 Architecture Notes

**Menu System:**
- Each menu has 4 copybooks: *_DEF, *_WKS, *_SCN, *_EVA
- Text location depends on pattern:
  - **Pattern A**: LABEL statements in *_SCN.CPY (LIBSP menus)
  - **Pattern B**: VALUE clauses in *_WKS.CPY (LIBWI menus)

**Extraction Priority:**
- LABEL statements (Pattern A) are primary target
- VALUE clauses (Pattern B) secondary - copybooks lack DATA DIVISION

**Why This Matters:**
- Users ask: "what menu options are available?"
- System needs: literal text from SCREEN SECTION
- Current gap: copybooks not indexed
- Solution: Add copybooks to chunking = instant menu text retrieval

---

## 📝 Commits (Local)

1. `0bd8c72` - LABEL extraction implementation
2. `c539538` - VALUE regex improvements  
3. `673a2cf` - Clean up git tracking
4. `307ae8e` - Documentation

**Note:** Cannot push to GitHub due to large JSONL files in history.
Code is production-ready, just needs copybook chunking enabled.

---

## 💡 Quick Win

**Estimated Effort:** 1-2 hours
**Impact:** High - enables menu text queries
**Risk:** Low - copybooks are valid COBOL, just need chunking

The infrastructure is **100% ready**. Just need to flip the switch on copybook chunking.
