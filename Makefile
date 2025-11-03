# Simple orchestration Makefile for COBOL retrieval stack
# Requires: Python available as `python`

PYTHON ?= python
SRC_ROOT ?= cobol_src
MIN_VECTOR_COV ?= 90
EVAL_PASS_REQ ?= 80
RANDOM_EVAL_PROGRAMS ?= 40
RANDOM_EVAL_SYMBOLS ?= 40
RANDOM_EVAL_UIPATHS ?= 15

.PHONY: indexes ingest embed eval mvp diagnostics

indexes:
	@echo "[phase] index creation (idempotent)"
	$(PYTHON) build_all.py --indexes || exit 1

ingest:
	@echo "[phase] ingest/build scripts"
	$(PYTHON) build_all.py --ingest || exit 1

embed:
	@echo "[phase] vector backfills + coverage enforcement"
	$(PYTHON) ops/run_backfills.py --batch 64 --min-coverage $(shell echo $$(( $(MIN_VECTOR_COV) )) )/100 || exit 1
	@echo "[info] consolidated vector health (non-fatal)"
	$(PYTHON) consolidated_vector_health.py --pretty || true

# Placeholder evaluation script; if not present this target will warn but not fail.
# Expect eval/run_random_eval.py to exit non-zero on < pass threshold.
eval:
	@if [ ! -f eval/run_random_eval.py ]; then \
	  echo "[WARN] eval/run_random_eval.py missing; skipping eval"; \
	else \
	  $(PYTHON) eval/run_random_eval.py --programs $(RANDOM_EVAL_PROGRAMS) --symbols $(RANDOM_EVAL_SYMBOLS) --uipaths $(RANDOM_EVAL_UIPATHS) --min-pass $(EVAL_PASS_REQ) --src-root $(SRC_ROOT); \
	fi

mvp: indexes ingest embed eval
	@echo "[OK] MVP pipeline completed"
