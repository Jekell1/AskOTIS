"""Compatibility wrapper delegating to root backfill_copybook_usage_vectors.py.

Historical command sequences call:
  python search/backfill/backfill_copybook_usage_embeddings.py --batch 128
We provide this thin wrapper to preserve those sequences while centralizing the
real logic in the root-level backfill_copybook_usage_vectors.py.
"""
from __future__ import annotations
import runpy, sys

if __name__=='__main__':
    # Map common args: --batch -> --batch, ignore unrecognized (let underlying script parse)
    sys.argv[0]='backfill_copybook_usage_vectors.py'
    runpy.run_module('backfill_copybook_usage_vectors', run_name='__main__')
