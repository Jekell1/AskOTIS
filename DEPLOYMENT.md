# OTIS RAG System - Deployment Guide

## Overview

This guide covers deploying the OTIS RAG (Retrieval-Augmented Generation) system to Azure Functions.

## Prerequisites

- Azure CLI installed and configured
- Azure Functions Core Tools installed
- Python 3.10+ installed locally
- Azure subscription with appropriate permissions
- Git repository with latest changes committed

## Important: Repository Structure

The repository contains **two separate Azure Functions projects**:

1. **Repository Root** (`/`) - **MAIN RAG SYSTEM** ✅
   - Contains: `function_app.py` with RAG endpoints
   - Endpoints: `/api/query`, `/api/health`, `/api/stats`
   - **This is what you want to deploy for RAG functionality**

2. **cobol-function/** - Separate COBOL parsing utility ❌
   - Contains: `function_app.py` with only `/api/cobol-parse`
   - **Do NOT deploy from this directory for RAG**

## Deployment Steps

### Step 1: Verify Your Location

**CRITICAL**: Always deploy from the **repository root**, not from subdirectories.

```powershell
# Navigate to repository root
cd c:\Users\jeff.childers\Documents\OTISCodeResearcher

# Verify you're in the correct location (should show host.json)
ls host.json
```

### Step 2: Verify Configuration Changes

Before deploying, ensure your configuration changes are committed:

```powershell
# Check for uncommitted changes
git status

# If you have changes to otis_rag/config.py or other files, commit them:
git add otis_rag/config.py
git commit -m "Update RAG configuration"
git push origin main
```

### Step 3: Deploy to Azure Functions

```powershell
# Deploy from repository root
func azure functionapp publish func-otis-rag --python
```

**Expected Output:**
```
Getting site publishing info...
Creating archive for current directory...
Upload completed successfully.
Deployment completed successfully.
Syncing triggers...
Functions in func-otis-rag:
    health - [httpTrigger] https://func-otis-rag.azurewebsites.net/api/health
    query - [httpTrigger] https://func-otis-rag.azurewebsites.net/api/query
    stats - [httpTrigger] https://func-otis-rag.azurewebsites.net/api/stats
```

**⚠️ If you only see `cobol-parse` function**, you deployed from the wrong directory!

### Step 4: Verify Deployment

Wait 15-30 seconds for the function app to warm up, then test:

```powershell
# Test health endpoint
Invoke-RestMethod -Uri "https://func-otis-rag.azurewebsites.net/api/health"

# Expected response:
# status          : healthy
# rag_initialized : True
# message         : OTIS RAG system is operational
```

```powershell
# Test stats endpoint
Invoke-RestMethod -Uri "https://func-otis-rag.azurewebsites.net/api/stats"

# Expected response includes:
# - indexes_configured: 21
# - chat_model: gpt-4.1
# - embedding_model: text-embedding-3-large
# - status: operational
```

### Step 5: Test the Chat Interface

Open `otis-rag-chat.html` in your browser and verify:

1. Status shows: "✅ Connected - System Operational"
2. System Info panel displays index count, models
3. Try an example question to test the `/api/query` endpoint

## Common Configuration Changes

### Increasing Retrieval Limits

If you need to retrieve more results from Azure AI Search:

**File 1**: `otis_rag/config.py`

**Line 63**:
```python
self.max_results_per_index = 10000  # Increased to 10k to retrieve all 9,678 programs when needed
```

**Why**: The default was 1000, which limited how many programs could be retrieved.

**File 2**: `otis_rag/retriever.py`

**Lines 58-69** (in the `retrieve` method):
```python
# Deduplicate and rank by relevance
# For queries requesting "all" items, return everything (up to max_results)
# For targeted queries, keep half for focused results
query_lower = query.lower()
wants_all = any(word in query_lower for word in ['all', 'list', 'every', 'complete'])

if wants_all:
    # Return all unique results, up to max_results limit
    final_count = max_results
else:
    # Keep more results to ensure business logic chunks surface
    final_count = max(max_results, len(all_results) // 2)
```

**Why**: The retriever was limiting final results to half of what was retrieved, even for "list all" queries. This logic detects when users want comprehensive results and returns all retrieved items.

**After changing**: Commit, push, and redeploy (Steps 2-3 above).

## Troubleshooting

### Issue: Only `cobol-parse` function appears

**Problem**: You deployed from `cobol-function/` directory instead of repository root.

**Solution**:
```powershell
# Navigate to repository root
cd c:\Users\jeff.childers\Documents\OTISCodeResearcher

# Verify correct location
ls host.json  # Should exist

# Redeploy
func azure functionapp publish func-otis-rag --python
```

### Issue: Health endpoint returns 404

**Problem**: Functions haven't finished deploying or starting up.

**Solution**: Wait 15-30 seconds and try again. Azure Functions can take time to warm up after deployment.

### Issue: Deployment fails with Python version mismatch

**Problem**: Local Python version doesn't match Azure Functions requirement.

**Solution**: 
- Local development: Python 3.10+
- Azure Functions will use Python 3.11 (automatically handled during remote build)
- No action needed - remote build handles version differences

### Issue: Configuration changes not reflected

**Problem**: Code wasn't committed before deployment.

**Solution**:
```powershell
# Verify your changes are committed
git status

# If uncommitted, commit them
git add otis_rag/config.py
git commit -m "Update configuration"

# Redeploy
func azure functionapp publish func-otis-rag --python
```

### Issue: "List all" queries still return limited results

**Problem**: Even after increasing `max_results_per_index`, queries like "list all programs" still return a limited set (e.g., 127 programs instead of 9,678).

**Root Cause**: The retriever has ranking logic that filters results down to half of what was retrieved, even for comprehensive queries.

**Solution**: The fix is in `otis_rag/retriever.py` (see "Increasing Retrieval Limits" section above). The code now detects queries with words like "all", "list", "every", or "complete" and returns the full result set instead of filtering down.

**Verification**:
```powershell
# After deployment, test with a comprehensive query
# Open otis-rag-chat.html and ask: "List all COBOL programs"
# Should return thousands of programs, not just 127
```

### Issue: ImportError or module not found

**Problem**: Dependencies missing from `requirements.txt`.

**Solution**: Ensure all required packages are listed in `requirements.txt` at repository root:
```
azure-functions
azure-search-documents
azure-ai-inference
openai
tiktoken
requests
werkzeug
# ... other dependencies
```

## Deployment Checklist

Before each deployment:

- [ ] Navigate to repository root (not `cobol-function/`)
- [ ] Verify `host.json` exists in current directory
- [ ] Commit all configuration changes
- [ ] Push changes to GitHub (optional but recommended)
- [ ] Run deployment command: `func azure functionapp publish func-otis-rag --python`
- [ ] Wait for deployment to complete (~1-2 minutes)
- [ ] Verify all three endpoints appear in output: health, query, stats
- [ ] Test health endpoint after 15-30 second warm-up
- [ ] Test stats endpoint
- [ ] Test query endpoint via chat interface

## File Structure Reference

```
OTISCodeResearcher/                    # ← DEPLOY FROM HERE
├── function_app.py                    # Main RAG endpoints
├── host.json                          # Azure Functions config
├── requirements.txt                   # Python dependencies
├── otis_rag/
│   ├── config.py                      # RAG configuration
│   ├── retriever.py                   # Search logic
│   └── ...
├── otis-rag-chat.html                 # Chat interface
└── cobol-function/                    # ← DO NOT DEPLOY FROM HERE
    ├── function_app.py                # Separate COBOL parser
    └── requirements.txt
```

## Environment Variables

The function app requires these environment variables in Azure:

- `AZURE_SEARCH_ENDPOINT` - Azure AI Search service URL
- `AZURE_SEARCH_KEY` - Azure AI Search admin key
- `AZURE_OPENAI_ENDPOINT` - Azure OpenAI service URL (or GitHub Models endpoint)
- `AZURE_OPENAI_KEY` - Azure OpenAI API key (or GitHub token)

These are configured in the Azure Portal under Function App → Configuration → Application settings.

## Monitoring

After deployment, monitor your function app:

1. **Azure Portal**: Function App → Monitor → Logs
2. **Application Insights**: If enabled, provides detailed telemetry
3. **Debug Console**: The chat interface has a built-in debug console (📋 button)

## Rollback

If deployment causes issues:

```powershell
# Revert code changes
git revert HEAD

# Redeploy
func azure functionapp publish func-otis-rag --python
```

## Production Considerations

1. **Scaling**: Azure Functions automatically scales based on load
2. **Performance**: First request after idle may be slow (cold start)
3. **Costs**: Monitor usage in Azure Portal - Consumption plan charges per execution
4. **Security**: Use Azure Key Vault for sensitive credentials instead of app settings
5. **CI/CD**: Consider setting up GitHub Actions for automated deployments

## Support

For issues or questions:
- Check Azure Functions logs in Azure Portal
- Review debug console output in chat interface
- Verify all environment variables are configured correctly
- Ensure Azure AI Search and OpenAI services are operational

---

**Last Updated**: October 21, 2025  
**Function App**: func-otis-rag.azurewebsites.net  
**Python Version**: 3.10 (local) / 3.11 (Azure)
