# Azure Functions Deployment for OTIS RAG System

**Date:** October 20, 2025  
**Architecture:** Azure Functions (Python) + Static Web App  
**Cost:** ~$0-2/month (mostly FREE tier)

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                     FRONTEND LAYER                            │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure Static Web App (FREE tier)                     │  │
│  │   - HTML/CSS/JavaScript interface                      │  │
│  │   - OR: Storage Account Static Website ($0.02/month)   │  │
│  └────────────────────┬───────────────────────────────────┘  │
└─────────────────────────┼──────────────────────────────────────┘
                          │ HTTPS POST /api/query
                          ↓
┌──────────────────────────────────────────────────────────────┐
│                     SERVERLESS LAYER                          │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure Functions (Python 3.11)                        │  │
│  │   - Consumption Plan (FREE 1M requests/month)          │  │
│  │   - OTIS RAG system                                    │  │
│  │   - HTTP Trigger for /api/query                        │  │
│  │   - Auto-scales, no servers to manage                  │  │
│  └────────────────────┬───────────────────────────────────┘  │
└─────────────────────────┼──────────────────────────────────────┘
                          │ Azure SDK
                          ↓
┌──────────────────────────────────────────────────────────────┐
│                     DATA LAYER                                │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure AI Search (21 indexes)                         │  │
│  └────────────────────────────────────────────────────────┘  │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure OpenAI Service                                 │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

---

## Why Azure Functions?

### ✅ **Advantages**
- **FREE tier**: 1 million requests/month free
- **Simpler**: No Docker, no containers
- **Auto-scaling**: Scales to thousands of requests automatically
- **Pay per use**: Only pay for actual execution time
- **Integrated**: Works seamlessly with Static Web Apps
- **Easy deployment**: Deploy directly from VS Code or CLI

### ⚠️ **Trade-offs**
- **Cold start**: First request after idle may be slow (2-5 seconds)
- **Timeout**: Max 5 minutes per request (fine for RAG)
- **Limited customization**: Less control than containers

### 💰 **Cost Comparison**

| Component | Container Apps | Azure Functions |
|-----------|----------------|-----------------|
| Backend | ~$2-5/month | ~$0/month (free tier) |
| Frontend | FREE | FREE |
| Total | **$2-5/month** | **$0-1/month** |

**Winner: Azure Functions** (for low-medium traffic)

---

## Implementation

### Project Structure

```
OTISCodeResearcher/
├── otis_rag/              # Your existing RAG code
│   ├── __init__.py
│   ├── config.py
│   ├── rag.py
│   ├── router.py
│   ├── retriever.py
│   ├── generator.py
│   └── prompts.py
├── function_app.py        # NEW: Azure Functions entry point
├── host.json              # NEW: Function host config
├── local.settings.json    # Environment variables
├── requirements.txt       # Updated with Functions SDK
└── frontend/              # Static web files
    ├── index.html
    └── staticwebapp.config.json
```

---

## Files to Create

### 1. Azure Functions Entry Point

**File: `function_app.py`**

```python
"""
Azure Functions HTTP Trigger for OTIS RAG System

This function handles HTTP requests and routes them to the RAG system.
"""

import azure.functions as func
import logging
import json
from otis_rag.rag import OTISRAG
from otis_rag.config import load_config

# Create the function app
app = func.FunctionApp()

# Initialize RAG system (will be reused across invocations)
_rag_instance = None

def get_rag():
    """Get or create RAG instance (singleton pattern)."""
    global _rag_instance
    if _rag_instance is None:
        logging.info("Initializing RAG system...")
        config = load_config()
        _rag_instance = OTISRAG(config)
        logging.info("RAG system initialized")
    return _rag_instance


@app.route(route="query", methods=["POST"], auth_level=func.AuthLevel.ANONYMOUS)
def query_endpoint(req: func.HttpRequest) -> func.HttpResponse:
    """
    HTTP endpoint for RAG queries.
    
    POST /api/query
    Body: {"question": "...", "conversation_history": [...]}
    """
    logging.info('Processing RAG query request')

    # Enable CORS
    headers = {
        'Access-Control-Allow-Origin': '*',
        'Access-Control-Allow-Methods': 'POST, OPTIONS',
        'Access-Control-Allow-Headers': 'Content-Type',
        'Content-Type': 'application/json'
    }

    # Handle preflight OPTIONS request
    if req.method == 'OPTIONS':
        return func.HttpResponse(
            status_code=200,
            headers=headers
        )

    try:
        # Parse request body
        req_body = req.get_json()
        question = req_body.get('question')
        conversation_history = req_body.get('conversation_history', [])

        if not question:
            return func.HttpResponse(
                json.dumps({"error": "Question is required"}),
                status_code=400,
                headers=headers
            )

        logging.info(f"Question: {question}")

        # Get RAG instance and process query
        rag = get_rag()
        result = rag.answer(
            question,
            conversation_history=conversation_history
        )

        # Prepare response
        response_data = {
            "answer": result["answer"],
            "sources": result.get("sources", []),
            "question_type": result.get("question_type", "general"),
            "indexes_used": result.get("indexes_used", [])
        }

        logging.info(f"Query processed successfully. Type: {response_data['question_type']}")

        return func.HttpResponse(
            json.dumps(response_data),
            status_code=200,
            headers=headers
        )

    except ValueError as e:
        logging.error(f"Invalid request: {str(e)}")
        return func.HttpResponse(
            json.dumps({"error": "Invalid JSON in request body"}),
            status_code=400,
            headers=headers
        )
    except Exception as e:
        logging.error(f"Error processing query: {str(e)}", exc_info=True)
        return func.HttpResponse(
            json.dumps({"error": f"Internal server error: {str(e)}"}),
            status_code=500,
            headers=headers
        )


@app.route(route="health", methods=["GET"], auth_level=func.AuthLevel.ANONYMOUS)
def health_check(req: func.HttpRequest) -> func.HttpResponse:
    """Health check endpoint."""
    logging.info('Health check request')
    
    headers = {
        'Access-Control-Allow-Origin': '*',
        'Content-Type': 'application/json'
    }
    
    try:
        rag = get_rag()
        return func.HttpResponse(
            json.dumps({
                "status": "healthy",
                "version": "1.0.0",
                "phase1_active": True,
                "phase2_active": True,
                "total_indexes": 21
            }),
            status_code=200,
            headers=headers
        )
    except Exception as e:
        logging.error(f"Health check failed: {str(e)}")
        return func.HttpResponse(
            json.dumps({
                "status": "unhealthy",
                "error": str(e)
            }),
            status_code=503,
            headers=headers
        )


@app.route(route="stats", methods=["GET"], auth_level=func.AuthLevel.ANONYMOUS)
def stats_endpoint(req: func.HttpRequest) -> func.HttpResponse:
    """Get RAG system statistics."""
    logging.info('Stats request')
    
    headers = {
        'Access-Control-Allow-Origin': '*',
        'Content-Type': 'application/json'
    }
    
    try:
        config = load_config()
        return func.HttpResponse(
            json.dumps({
                "total_indexes": len(config.indexes),
                "indexes": list(config.indexes.keys()),
                "phase1_enhancements": [
                    "Enhanced graph traversal",
                    "Historical context emphasis",
                    "Cross-reference analysis",
                    "Improved comment surfacing"
                ],
                "phase2_indexes": [
                    "transaction_taxonomy (51 docs)",
                    "program_complexity (1,736 docs)"
                ]
            }),
            status_code=200,
            headers=headers
        )
    except Exception as e:
        logging.error(f"Stats request failed: {str(e)}")
        return func.HttpResponse(
            json.dumps({"error": str(e)}),
            status_code=500,
            headers=headers
        )
```

---

### 2. Function Configuration

**File: `host.json`**

```json
{
  "version": "2.0",
  "logging": {
    "applicationInsights": {
      "samplingSettings": {
        "isEnabled": true,
        "maxTelemetryItemsPerSecond": 20
      }
    },
    "logLevel": {
      "default": "Information",
      "Function": "Information"
    }
  },
  "extensions": {
    "http": {
      "routePrefix": "api"
    }
  },
  "functionTimeout": "00:05:00",
  "extensionBundle": {
    "id": "Microsoft.Azure.Functions.ExtensionBundle",
    "version": "[4.*, 5.0.0)"
  }
}
```

---

### 3. Updated Requirements

**File: `requirements.txt`**

```txt
# Azure Functions
azure-functions==1.18.0

# Existing RAG dependencies
azure-search-documents==11.4.0
azure-identity==1.15.0
openai==1.3.0
python-dotenv==1.0.0

# Additional dependencies
pydantic==2.5.0
```

---

### 4. Local Settings (for testing)

**File: `local.settings.json`**

```json
{
  "IsEncrypted": false,
  "Values": {
    "AzureWebJobsStorage": "",
    "FUNCTIONS_WORKER_RUNTIME": "python",
    "AZURE_SEARCH_ENDPOINT": "https://your-search.search.windows.net",
    "AZURE_SEARCH_KEY": "your-search-key",
    "AZURE_OPENAI_ENDPOINT": "https://your-openai.openai.azure.com",
    "AZURE_OPENAI_KEY": "your-openai-key",
    "AZURE_OPENAI_DEPLOYMENT": "gpt-4",
    "AZURE_OPENAI_EMBEDDING_DEPLOYMENT": "text-embedding-3-large"
  }
}
```

---

### 5. Frontend (Same as Before)

**File: `frontend/index.html`** (Updated API URL)

```javascript
// Update this line in the JavaScript section:
const API_BASE_URL = 'https://your-function-app.azurewebsites.net';

// Or for local testing:
// const API_BASE_URL = 'http://localhost:7071';
```

---

## Deployment Steps

### Option A: Deploy from VS Code (Easiest)

1. **Install Azure Functions Extension**
   - Open VS Code
   - Install "Azure Functions" extension
   - Install "Azure Static Web Apps" extension

2. **Deploy Function App**
   ```
   1. Click Azure icon in sidebar
   2. Sign in to Azure
   3. Click "Deploy to Function App"
   4. Select or create Function App
   5. Choose Python 3.11 runtime
   6. Wait for deployment
   ```

3. **Configure Settings**
   ```
   1. Right-click Function App
   2. Select "Upload Local Settings"
   3. Add environment variables from local.settings.json
   ```

4. **Deploy Frontend**
   ```
   1. Click "Deploy to Static Web App"
   2. Select or create Static Web App
   3. Choose frontend folder
   4. Wait for deployment
   ```

---

### Option B: Deploy from Azure CLI

#### Step 1: Create Azure Resources

```bash
# Variables
RESOURCE_GROUP="otis-rag-rg"
LOCATION="eastus"
FUNCTION_APP_NAME="otis-rag-func"
STORAGE_ACCOUNT="otisragstorage"
STATIC_WEB_APP="otis-rag-web"

# Login
az login

# Create resource group
az group create --name $RESOURCE_GROUP --location $LOCATION

# Create storage account (required for Functions)
az storage account create \
  --name $STORAGE_ACCOUNT \
  --resource-group $RESOURCE_GROUP \
  --location $LOCATION \
  --sku Standard_LRS

# Create Function App (Consumption Plan - FREE)
az functionapp create \
  --resource-group $RESOURCE_GROUP \
  --name $FUNCTION_APP_NAME \
  --storage-account $STORAGE_ACCOUNT \
  --runtime python \
  --runtime-version 3.11 \
  --functions-version 4 \
  --os-type Linux \
  --consumption-plan-location $LOCATION

# Create Static Web App (FREE tier)
az staticwebapp create \
  --name $STATIC_WEB_APP \
  --resource-group $RESOURCE_GROUP \
  --location eastus2 \
  --sku Free
```

#### Step 2: Configure Function App Settings

```bash
# Set environment variables
az functionapp config appsettings set \
  --name $FUNCTION_APP_NAME \
  --resource-group $RESOURCE_GROUP \
  --settings \
    AZURE_SEARCH_ENDPOINT="https://your-search.search.windows.net" \
    AZURE_SEARCH_KEY="your-key" \
    AZURE_OPENAI_ENDPOINT="https://your-openai.openai.azure.com" \
    AZURE_OPENAI_KEY="your-key" \
    AZURE_OPENAI_DEPLOYMENT="gpt-4" \
    AZURE_OPENAI_EMBEDDING_DEPLOYMENT="text-embedding-3-large"
```

#### Step 3: Deploy Function Code

```bash
# Install Azure Functions Core Tools (if not installed)
# Windows (PowerShell):
winget install Microsoft.Azure.FunctionsCoreTools

# Deploy function app
cd OTISCodeResearcher
func azure functionapp publish $FUNCTION_APP_NAME
```

#### Step 4: Deploy Static Web App

```bash
# Option 1: Using SWA CLI
npm install -g @azure/static-web-apps-cli
swa deploy ./frontend \
  --app-name $STATIC_WEB_APP \
  --resource-group $RESOURCE_GROUP

# Option 2: Using Azure CLI
az staticwebapp deploy \
  --name $STATIC_WEB_APP \
  --resource-group $RESOURCE_GROUP \
  --source ./frontend
```

#### Step 5: Update Frontend API URL

```bash
# Get Function App URL
FUNCTION_URL=$(az functionapp show \
  --name $FUNCTION_APP_NAME \
  --resource-group $RESOURCE_GROUP \
  --query defaultHostName -o tsv)

echo "Update frontend API_BASE_URL to: https://$FUNCTION_URL"
```

---

## Local Testing

### 1. Install Azure Functions Core Tools

```bash
# Windows (via winget)
winget install Microsoft.Azure.FunctionsCoreTools

# Windows (via npm)
npm install -g azure-functions-core-tools@4

# Mac (via Homebrew)
brew tap azure/functions
brew install azure-functions-core-tools@4
```

### 2. Install Python Dependencies

```bash
# Create virtual environment
python -m venv .venv
.venv\Scripts\activate  # Windows
# source .venv/bin/activate  # Mac/Linux

# Install dependencies
pip install -r requirements.txt
```

### 3. Run Locally

```bash
# Start the function app
func start

# Function will be available at:
# http://localhost:7071/api/query
# http://localhost:7071/api/health
# http://localhost:7071/api/stats
```

### 4. Test with curl

```bash
# Health check
curl http://localhost:7071/api/health

# Query
curl -X POST http://localhost:7071/api/query \
  -H "Content-Type: application/json" \
  -d '{
    "question": "What does APIPAY program do?",
    "conversation_history": []
  }'

# Stats
curl http://localhost:7071/api/stats
```

### 5. Test Frontend Locally

```bash
# Update frontend/index.html
# Change: const API_BASE_URL = 'http://localhost:7071';

# Serve frontend
cd frontend
python -m http.server 8080

# Open browser: http://localhost:8080
```

---

## Cost Breakdown

### Azure Functions (Consumption Plan)

**FREE Tier Includes:**
- 1 million requests/month
- 400,000 GB-seconds compute/month

**After Free Tier:**
- $0.20 per million executions
- $0.000016 per GB-second

**Expected Cost:**
- Low traffic (< 1M requests/month): **$0**
- Medium traffic (2-3M requests/month): **~$0.50-1/month**

### Storage Account (for Functions)

- **Cost**: ~$0.05-0.10/month (minimal)
- Required for Functions runtime

### Static Web App

- **FREE tier**: $0/month
- **Standard tier**: $9/month (if you need custom domain)

### Total Monthly Cost

| Usage Level | Cost |
|-------------|------|
| **Low** (< 1M requests) | **$0.05-0.10/month** |
| **Medium** (2-3M requests) | **$0.50-1/month** |
| **High** (5M requests) | **~$2/month** |

**Winner: Azure Functions is MUCH cheaper than Container Apps!**

---

## Cold Start Mitigation

Functions have a "cold start" issue (2-5 second delay after idle).

### Solutions:

**1. Always On (Basic Plan)**
```bash
# Upgrade to Basic plan (~$13/month)
az functionapp update \
  --name $FUNCTION_APP_NAME \
  --resource-group $RESOURCE_GROUP \
  --set kind="functionapp"

# Enable always on
az functionapp config set \
  --name $FUNCTION_APP_NAME \
  --resource-group $RESOURCE_GROUP \
  --always-on true
```

**2. Keep-Alive Ping**
```javascript
// In frontend, ping every 4 minutes
setInterval(() => {
  fetch(`${API_BASE_URL}/api/health`)
}, 240000);
```

**3. Premium Plan (for production)**
- Pre-warmed instances (no cold start)
- Cost: ~$150/month (overkill for most use cases)

---

## Monitoring & Debugging

### Application Insights (Automatic)

Functions automatically logs to Application Insights:

```bash
# View logs
az monitor app-insights query \
  --app your-app-insights \
  --analytics-query "traces | where message contains 'RAG' | top 10 by timestamp desc"
```

### View Logs in Portal

1. Go to Azure Portal
2. Open your Function App
3. Click "Log Stream"
4. See real-time logs

### Debug Locally

```bash
# Run with debugging
func start --verbose

# Check logs in terminal
# Logs show in real-time
```

---

## Security Best Practices

### 1. Use Managed Identity (Recommended)

```python
# Update otis_rag/config.py to use Managed Identity
from azure.identity import DefaultAzureCredential

credential = DefaultAzureCredential()
search_client = SearchClient(
    endpoint=search_endpoint,
    index_name=index_name,
    credential=credential  # No key needed!
)
```

Enable Managed Identity:
```bash
az functionapp identity assign \
  --name $FUNCTION_APP_NAME \
  --resource-group $RESOURCE_GROUP

# Grant permissions to Azure AI Search
# (Do this in Azure Portal)
```

### 2. Restrict CORS

```python
# In function_app.py, change:
'Access-Control-Allow-Origin': 'https://your-static-web-app.azurestaticapps.net'
```

### 3. Enable Authentication (Optional)

```bash
# Enable Azure AD authentication
az functionapp auth update \
  --name $FUNCTION_APP_NAME \
  --resource-group $RESOURCE_GROUP \
  --enabled true \
  --action LoginWithAzureActiveDirectory
```

---

## Comparison: Functions vs Container Apps

| Feature | Azure Functions | Container Apps |
|---------|----------------|----------------|
| **Cost (low traffic)** | 🏆 **$0-1/month** | $2-5/month |
| **Setup complexity** | 🏆 **Simpler** | More complex |
| **Cold start** | ❌ 2-5 seconds | ✅ Minimal |
| **Always on** | ❌ Requires Basic plan | ✅ Default |
| **Scalability** | ✅ Auto-scale | ✅ Auto-scale |
| **Customization** | Limited | 🏆 **Full control** |
| **Best for** | 🏆 **Low-medium traffic** | High traffic |

**Recommendation:**
- **Start with Functions** (cheaper, simpler)
- **Upgrade to Container Apps** if cold starts become an issue

---

## Next Steps

### Quick Start (15 minutes)

1. **Create `function_app.py`** (copy code above)
2. **Update `requirements.txt`** (add azure-functions)
3. **Test locally**: `func start`
4. **Deploy**: Use VS Code Azure extension

### Production Ready (1 hour)

1. **Create all files** (function_app.py, host.json, frontend)
2. **Test locally** (func start + frontend)
3. **Deploy to Azure** (VS Code or CLI)
4. **Configure monitoring** (Application Insights)
5. **Test in production**

---

## Files I Can Create

Would you like me to create:

1. ✅ **`function_app.py`** - Azure Functions code
2. ✅ **`host.json`** - Function configuration
3. ✅ **`frontend/index.html`** - Web UI (updated for Functions)
4. ✅ **Deployment scripts** - PowerShell/Bash
5. ✅ **Testing scripts** - Local testing helpers

Should I create these files now?
