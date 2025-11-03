# Deprecation Plan: cobol-flow-edges / cobol-flow-edges-v2

## Rationale
The flow-edges indexes were intermediate artifacts to capture control-flow edge pairs (caller->callee / paragraph transitions) prior to richer graph synthesis. We now store and derive flow reasoning directly from:
- `cobol-calls` (canonical call relationships with resolved targets)
- `cobol-paragraphs` (paragraph content with structural metadata)
- `cobol-xrefs` (cross-reference edges enabling bidirectional traversal)
- `cobol-symbols` (identifiers and usage contexts)

Maintaining a large flow-edges index adds storage + ingestion cost without providing unique signal beyond what can be recomputed or inferred on-demand from the above canonical sets.

## Deprecation Stages
1. Freeze (Now)
   - Stop adding new documents to `cobol-flow-edges-v2`.
   - Mark index as legacy in configuration.
2. Shadow Queries (Optional)
   - For any component still querying `cobol-flow-edges-v2`, add a fallback implementation that reconstructs needed edges from calls + paragraphs + xrefs.
3. Validation
   - Run differential checks: randomly sample N edges from legacy index and ensure reconstructed equivalents exist via joins.
4. Code Removal
   - Remove direct search calls referencing `cobol-flow-edges*`.
   - Remove any ingestion scripts solely producing edge docs.
5. Archive / Delete
   - Export index (if needed for audit) then delete.

## Replacement Patterns
| Legacy Need | Replacement Strategy |
|-------------|----------------------|
| List outgoing edges from paragraph P | Use calls index (caller paragraph id -> target routine) + paragraphs for context |
| List incoming edges to paragraph P | Use xrefs (target id matches P) or reverse call lookup |
| Graph traversal for explanation | Compose from calls + xrefs + paragraph metadata |

## Action Items
- [ ] Tag index as legacy in index selection logic.
- [ ] Implement reconstruction helper `reconstruct_flow_edges.py` (planned).
- [ ] Add test comparing 500 sampled edges vs reconstruction (tolerance  >98% match).
- [ ] Schedule deletion after 2 successful validation runs.

## Notes
If a future feature needs explicit pre-computed path summaries (multi-hop), consider a compact `cobol-flow-paths` index that stores only normalized path signatures (length 2-5) with frequency + contextual hints, instead of raw pair edges.
