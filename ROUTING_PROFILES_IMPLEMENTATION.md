# Routing Profiles Implementation

## Summary

Successfully implemented **routing profiles** in `otis_rag/router.py` to simplify index selection and reduce retrieval noise and latency.

## Changes Made

### 1. Added Routing Profiles (Class-Level Constants)

```python
ROUTE_PROFILES = {
    "program": ["code", "code_new", "programs", "calls", "program_deps"],
    "data": ["data_items", "variables", "copybooks", "copybook_usage", "name_aliases"],
    "ui": ["screen_nodes", "ui_paths", "menu_trees"],
    "transaction": ["transactions", "flows", "flow_edges", "programs"],
    "flow": ["flows", "flow_edges", "calls", "program_deps", "paragraphs"],
    "complexity": ["complexity", "programs", "flows", "paragraphs", "calls", "program_deps"],
    "files": ["files", "code", "code_new", "programs"]
}
```

### 2. Simplified `_select_indexes()` Method

Replaced ad-hoc index lists with focused profile selections:

- **explain_program**: program + flow profiles (8 indexes)
- **find_code**: files profile (7 indexes)
- **explain_data**: data profile (5 indexes)
- **trace_flow**: flow + ui + data items (10 indexes)
- **menu**: ui profile only (3 indexes) - **75% reduction!**
- **transaction**: transaction + ui profiles (7 indexes)
- **complexity**: complexity profile (6 indexes)
- **general**: program + data + files (11 indexes)

### 3. Updated `_calculate_index_weights()` Method

Adjusted weights to align with routing profiles:

- **Menu queries**: `screen_nodes=5.0x`, `ui_paths=3.0x`, `menu_trees=3.0x`
- **Transaction queries**: `transactions=3.0x`, `ui_paths=2.0x`
- **Data queries**: `copybook_usage=10.0x`, `copybooks=2.0x`
- **Complexity queries**: `complexity=2.5x`, `programs=1.5x`
- **Flow queries**: `flows=2.0x`, `flow_edges=2.0x`

## Results

### Index Count Reduction

| Query Type | Before | After | Reduction |
|------------|--------|-------|-----------|
| Program explanation | 11 | 8 | 27.3% |
| Flow tracing | 13 | 10 | 23.1% |
| **Menu questions** | 12 | 3 | **75.0%** |
| Transaction | 9 | 7 | 22.2% |
| General | 12 | 11 | 8.3% |

**Overall Impact:**
- **31.6% reduction** in total indexes searched (57 → 39)
- **Average per query**: 11.4 → 7.8 indexes

### Expected Benefits

1. **⚡ ~32% faster retrieval latency** - fewer indexes to search
2. **🎯 Improved precision** - less noise from irrelevant indexes
3. **💰 Lower Azure Search API costs** - fewer search operations
4. **🧹 Cleaner code** - profiles easier to maintain than ad-hoc lists

## Testing

Created two test scripts:

1. **`test_routing_profiles.py`** - Validates correct index selection and weights
   - ✅ All 7 test cases passing
   
2. **`compare_routing_impact.py`** - Shows before/after comparison
   - Demonstrates 31.6% reduction in search operations

## Files Modified

- `otis_rag/router.py`:
  - Added `ROUTE_PROFILES` class constant
  - Rewrote `_select_indexes()` method (67 lines → cleaner logic)
  - Updated `_calculate_index_weights()` method

## Next Steps

1. **Deploy to Production**:
   ```bash
   func azure functionapp publish func-otis-rag --python
   ```

2. **Monitor Performance**:
   - Track average retrieval latency
   - Measure precision improvements
   - Validate Azure Search cost reduction

3. **Optional Optimizations**:
   - Fine-tune profile compositions based on production metrics
   - Adjust weights based on real-world query patterns

## Notes

- Mixed-intent detection still works (e.g., adding complexity/transaction indexes to other query types)
- Duplicate removal preserves order
- Backward compatible with existing code
- No breaking changes to API
