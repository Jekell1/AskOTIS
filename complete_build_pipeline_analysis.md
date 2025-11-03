# COMPLETE INDEX BUILD PIPELINE ANALYSIS

## 🔍 DISCOVERED: Comprehensive Build Infrastructure

You have a complete build pipeline with:
1. **Orchestrator:** `build_all.py` - Runs everything in sequence
2. **Build Scripts:** In `ingest/` folder - Creates/updates index data
3. **Backfill Scripts:** Root folder - Adds embeddings to existing data

---

## 📁 BUILD SCRIPTS IN `ingest/` FOLDER

| Script | Index | Purpose | Status |
|--------|-------|---------|--------|
| **build_ui_paths.py** | new_cobol_ui_paths | Generate UI navigation paths | ✅ EXISTS |
| **build_screen_nodes.py** | new_cobol_screen_nodes | Extract screen definitions | ✅ EXISTS |
| **build_copybook_usage.py** | new_cobol_copybook_usage | Extract copybook inclusions | ✅ EXISTS |
| **build_copybook_meta.py** | new_cobol_copybook_meta | Create copybook summaries | ✅ EXISTS |
| **build_program_deps.py** | new_cobol_program_deps | Extract program dependencies | ✅ EXISTS |
| **build_program_copybook_edges.py** | new_cobol_program_copybook_edges | Program-copybook relationships | ✅ EXISTS |
| **build_name_aliases.py** | new_cobol_name_aliases | Name normalization | ✅ EXISTS |

---

## 📁 BUILD SCRIPTS IN ROOT FOLDER

| Script | Index | Purpose | Status |
|--------|-------|---------|--------|
| **build_program_meta.py** | new_cobol_program_meta | Program metadata | ✅ EXISTS |
| **build_program_flows.py** | new_cobol_program_flows | Control flow graphs | ✅ EXISTS |
| **build_program_inventory.py** | new_cobol_program_inventory | Program tracking | ✅ EXISTS |
| **build_variable_usage.py** | new_cobol_variable_usage | Variable tracking | ✅ EXISTS |
| **build_symbol_refs.py** | new_cobol_symbol_refs | Symbol references | ✅ EXISTS |
| **build_full_menu_tree.py** | new_cobol_menu_trees | Menu navigation trees | ✅ EXISTS |

---

## 🚀 ORCHESTRATOR: `build_all.py`

### Usage:
```bash
# Run everything
python build_all.py --indexes --ingest --backfill

# Just rebuild data (no embeddings)
python build_all.py --ingest

# Just embeddings (for existing data)
python build_all.py --backfill

# Dry run to see what would execute
python build_all.py --ingest --dry-run
```

### Phases:
1. **--indexes**: Create index schemas (if needed)
2. **--ingest**: Run build scripts to populate data
3. **--backfill**: Generate embeddings for existing data

---

## ✅ REVISED RECOMMENDATION

Based on finding these build scripts, here's what you should do:

### 🎯 **OPTION 1: Rebuild Everything (Most Complete)**

Run the full pipeline to ensure all indexes have maximum data:

```bash
# This will rebuild ALL indexes with fresh data
python build_all.py --ingest --dry-run

# Review what it will do, then run for real:
python build_all.py --ingest
```

**Time:** 4-8 hours (depends on data volume)  
**Result:** All indexes rebuilt with maximum coverage

---

### 🎯 **OPTION 2: Targeted Rebuilds (Faster)**

Rebuild specific high-value indexes:

#### A. UI Paths (30 min - 1 hour)
```bash
python ingest/build_ui_paths.py --push
```

#### B. Screen Nodes (2-3 hours)
```bash
python ingest/build_screen_nodes.py --push
```

#### C. Copybook Usage (3-4 hours)
```bash
python ingest/build_copybook_usage.py --push
```

#### D. Calls Data (Need to check if extraction script exists)
```bash
# May need to create this or find existing script
python build_calls.py --push
```

---

### 🎯 **OPTION 3: Incremental Expansion (Smart Approach)**

Focus on expanding coverage for specific programs:

#### Check what each script supports:
```bash
# Most build scripts support these flags:
--help                  # See all options
--limit N               # Process N programs
--program-id PROG       # Process specific program
--missing-only          # Only process programs not in index yet
--push                  # Upload to search index
```

#### Example: Expand screen nodes for missing programs
```bash
python ingest/build_screen_nodes.py --missing-only --push
```

---

## 📊 CURRENT STATUS WITH BUILD SCRIPTS

| Index | Coverage | Embeddings | Build Script | Action |
|-------|----------|------------|--------------|--------|
| **new_cobol_ui_paths** | N/A | ✅ 100% | `ingest/build_ui_paths.py` | Run to generate more paths |
| **new_cobol_screen_nodes** | 40.6% | ✅ 100% | `ingest/build_screen_nodes.py` | Run with --missing-only |
| **new_cobol_copybook_usage** | 17.8% | ✅ 100% | `ingest/build_copybook_usage.py` | Run with --missing-only |
| **new_cobol_copybook_meta** | N/A | ✅ 100% | `ingest/build_copybook_meta.py` | Run to generate summaries |
| **new_cobol_program_deps** | ✅ 100% | ✅ 100% | `build_program_deps.py` | ✅ COMPLETE |
| **new_cobol_program_meta** | ✅ 100% | ✅ 100% | `build_program_meta.py` | ✅ COMPLETE |
| **new_cobol_program_flows** | ✅ 100% | ✅ 100% | `build_program_flows.py` | ✅ COMPLETE |

---

## 💡 MY RECOMMENDATION (Updated)

### **Phase 1: Quick Validation (5 minutes)**
Check what the scripts would do:
```bash
python ingest/build_ui_paths.py --help
python ingest/build_screen_nodes.py --help
python ingest/build_copybook_usage.py --help
```

### **Phase 2: Targeted Rebuilds (3-5 hours)**
Run these in order for maximum chatbot value:

```bash
# 1. UI Paths (30 min - generates user workflows)
python ingest/build_ui_paths.py --root-mode mixed --limit-roots 80 \
  --max-depth 24 --branching-limit 20 --push

# 2. Screen Nodes (2-3 hours - expands screen coverage)
python ingest/build_screen_nodes.py --missing-only --push

# 3. Copybook Usage (3-4 hours - expands copybook tracking)
python ingest/build_copybook_usage.py --missing-only --push
```

### **Phase 3: Verify Embeddings Still Complete (5 minutes)**
```bash
python find_fully_complete_indexes.py
```

---

## 🎊 BOTTOM LINE

**You already have all the infrastructure!**

- ✅ All build scripts exist
- ✅ Orchestrator (`build_all.py`) exists
- ✅ Just need to run them with `--missing-only` or regenerate

**Best approach:**
1. Start with **UI paths** (fastest, highest value)
2. Then **screen nodes** (moderate time, high value)
3. Then **copybook usage** (longer, high value)
4. Embeddings are already 100% - no backfill needed!

**Ready to start?** Let me check the build script parameters for you!
