# Wrapper to backfill copybook meta embeddings.
# Ensure AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY / embedding creds are set in session.

python search/backfill/backfill_embeddings_copybook_meta.py --batch 64
