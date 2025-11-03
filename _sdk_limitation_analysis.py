#!/usr/bin/env python3
"""Research Azure Search SDK pagination approaches.

The standard SearchClient.search() iterator hit the same skip=100K limit.
Let's investigate alternative approaches:
1. search() with orderby for consistent ordering
2. Different SDK versions or methods
3. Manual continuation token handling
4. Alternative query patterns

Based on Azure documentation:
- SearchClient.search() returns SearchItemPaged iterator
- Internally uses skip/top pagination (hit our 100K limit)
- True continuation tokens may not be available for search operations
- Only indexing/change tracking uses continuation tokens

Conclusion from test:
The Azure SDK (azure-search-documents 11.6.0b9) DOES NOT bypass the skip=100K limit.
It hit the exact same error at skip=100,000 as the REST API.

Alternative approaches needed:
1. **ID-based pagination**: Fetch documents using ordered IDs and $filter > last_id
2. **Index sharding**: Split large indexes into smaller partitions
3. **Accept limitation**: Only embed first 100K docs per index
4. **Custom API**: Use Azure Search internal APIs if available (not recommended)
"""

print(__doc__)

print("\n" + "="*80)
print("FINDING: Azure SDK still uses $skip internally")
print("="*80)
print("""
Test results from _test_sdk_iteration.py:
- Successfully iterated 100,050 documents
- Error: "(InvalidRequestParameter) Value must be between 0 and 100000. Parameter name: $skip"
- The SDK SearchClient.search() iterator uses skip/top pagination internally
- There is NO continuation token support for generic search queries

Azure Search API Limitation:
- $skip parameter has hard limit of 100,000 across ALL APIs (REST and SDK)
- This is a fundamental Azure Cognitive Search service limitation
- Applies to all search operations using skip/top pagination

Why continuation tokens don't help:
- Continuation tokens in Azure Search are for indexing/change tracking
- Search operations use stateless skip/top pagination
- No true "cursor" or "continuation" for search queries over 100K results

Workarounds:
1. **ID-based filtering** (Most viable):
   - Sort by ID: $orderby=usage_id
   - Use $filter: usage_id gt 'last_processed_id'
   - Fetch in ordered chunks without using $skip
   - Requires ordered ID field (string or int)

2. **Range-based filtering** (If timestamp/numeric range available):
   - Partition by date ranges, numeric ranges, etc.
   - Query each partition independently
   - Combine results from multiple filtered queries

3. **Accept 100K limit**:
   - Embed first 100K docs (most recent/important)
   - Document limitation clearly
   - Focus on high-value indexes

Recommendation:
Implement ID-based filtering approach for copybook_usage and paragraphs.
This will allow accessing ALL documents without $skip parameter.
""")

print("\n" + "="*80)
print("NEXT STEPS")
print("="*80)
print("""
Option A: Implement ID-based pagination (RECOMMENDED)
- Create backfill_copybook_usage_id_filter.py
- Use $orderby=usage_id and $filter=usage_id gt 'last_id'
- Fetch 1000 docs at a time, track last_id, repeat
- No $skip required, can access all 114K+ docs

Option B: Accept current 61.7% Tier 2 coverage
- Continue to Tier 3 (634K docs in data_items + flow_edges_v2)
- Document the 100K limitation
- Return to Tier 2 if solution found later

Option C: Investigate index redesign
- Consider splitting large indexes into partitions
- Rebuild indexes with different strategy
- May require significant rework

Recommended: Option A - Implement ID-based filtering
This is the most viable workaround within Azure Search constraints.
""")
