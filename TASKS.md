# Active Work Plan

This file tracks the current multi-phase COBOL knowledge graph / retrieval pipeline work. Checkmarks will be updated as tasks complete.

## Legend
- [ ] Pending
- [~] In Progress / Partial
- [x] Complete

## Phase 1: Flow & Edge Graph Stabilization
- [~] Rebuild all program flows using v2 edges (`build_program_flows.py --use-v2-edges`) – running
- [ ] Spot‑check sample programs for edge_count > 0 and realistic fanout
- [x] Add single-program flow probe script (optional)

## Phase 2: Edge Embedding Coverage
- [x] Add `has_vector` flag in embedding backfill
- [x] Add checkpoint file for resume (`edge_embedding_checkpoint.json`)
- [~] Continue embedding forward (current frontier skip ≈ 18840)
- [ ] Decide on strategy for historic retro-flag (frontier flagger vs re-embed vs ignore)
- [ ] (Optional) Implement frontier flag script `flag_edge_vectors_frontier.py`

## Phase 3: Monitoring & Metrics
- [x] Edge stats script (`summarize_flow_edges_v2.py`)
- [x] Embedding coverage probe (fallback + has_vector modes)
- [ ] Flow doc probe: per-program summary fetcher
- [ ] Metrics snapshot export (edge kind distribution → JSON)

## Phase 4: Retrieval Integration Enhancements
- [ ] Integrate edge_vector (when present) into multi-stage retrieval fallback
- [ ] Add mixed scoring: edge semantic + paragraph + program flow summary
- [ ] Regression probe for retrieval quality (edge-aware vs baseline)

## Phase 5: Quality & Optimization
- [ ] Detect and optionally suppress high-frequency self-loop edges
- [ ] Cycle-heavy program list output (risk ranking)
- [ ] Adaptive embedding batch sizing (increase when latency low)
- [ ] Parallel embed (threaded) prototype (guarded by flag)

## Phase 6: Documentation & DevX
- [x] VS Code tasks added for pipeline operations
- [ ] README section: "Using the Graph Tasks" referencing tasks & typical sequences
- [ ] Architecture diagram update (include v2 edges + flows relationship)

## Quick Reference
Frontier Skip (embedding): `18840` (from `edge_embedding_checkpoint.json`)
Total Edge Docs: `366,650`
Approx Embedded (legacy + new): `~18,880` (≈5.1%)

## Next Immediate Actions (Recommended)
1. Let current flow rebuild run to first few batches.
2. Run spot-check script (to be created) on 3–5 known programs with complex control.
3. Resume embedding in larger wave (e.g. 50 batches) after confirming flows.
4. Decide on whether to implement frontier flagger.

---
_Last updated: auto-generated at runtime; refresh after major steps._

# Pipeline Task Tracker (Persistent)

## Phase 1 – Immediate Ops / Observability

- [x] Single-program flow probe script (`probe_program_flow.py`) added
- [x] VS Code task: Probe Program Flow (single)
- [x] VS Code task: Embed Wave: Edges V2 (next batch)
- [x] Ran probe for CRNOR2 (baseline captured)
- [x] Started next embedding wave (resume-skip=18840)
- [ ] Monitor embedding wave progress (expect batches advancing from 15)
- [ ] Spot-check 2–3 programs with probe for sanity (depth, cycles, fanout)

## Phase 2 – Screen Nodes Implementation
- [x] Create index script (`create_screen_nodes_index.py`) confirms schema
- [x] Build heuristic extractor (`build_screen_nodes.py`) enhanced summaries
- [x] Vector field adder updated (3072 dim) (`add_vector_field_screen_nodes.py`)
- [x] Embedding backfill script dimension safety log
- [x] VS Code tasks for create/build/vector/add/backfill/coverage
- [ ] Run: screen: create index (if clean slate needed)
- [ ] Run: screen: build nodes
- [ ] Run: screen: add vector field
- [ ] Run: screen: backfill embeddings
- [ ] Verify coverage ≥90% (expect 100% if all summary_text populated)
- [ ] Retrieval smoke: query hitting screen nodes via `retrieve_chat_context.py`