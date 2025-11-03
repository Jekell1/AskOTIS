# Sets required env vars then runs UI path embedding backfill.
# Replace <YOUR_KEY> with your real (preferably a query) key BEFORE running.

$env:AZURE_SEARCH_ENDPOINT = 'https://az-use1-ai-search.search.windows.net'
$env:AZURE_SEARCH_KEY = '<YOUR_KEY>'
# Optional Azure OpenAI variables if not already loaded in session
if (-not $env:AZURE_OPENAI_ENDPOINT) { Write-Host 'NOTE: AZURE_OPENAI_ENDPOINT not set in this session.' }
if (-not $env:AZURE_OPENAI_KEY) { Write-Host 'NOTE: AZURE_OPENAI_KEY not set in this session.' }
if (-not $env:AZURE_OPENAI_EMBED_DEPLOYMENT) { Write-Host 'NOTE: AZURE_OPENAI_EMBED_DEPLOYMENT not set in this session.' }

python search/backfill/backfill_ui_path_embeddings.py --batch 64
