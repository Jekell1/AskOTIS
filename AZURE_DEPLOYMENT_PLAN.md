# Azure Deployment Plan for OTIS RAG System

**Date:** October 20, 2025  
**Goal:** Deploy RAG system to Azure for web-based access  
**Architecture:** Container Apps (Backend) + Static Web App (Frontend)

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                     FRONTEND LAYER                            │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure Static Web App (FREE tier)                     │  │
│  │   - HTML/CSS/JavaScript interface                      │  │
│  │   - Chat-style UI for asking questions                 │  │
│  │   - Display formatted answers                          │  │
│  │   - Conversation history                               │  │
│  └────────────────────┬───────────────────────────────────┘  │
└─────────────────────────┼──────────────────────────────────────┘
                          │ HTTPS POST /api/query
                          ↓
┌──────────────────────────────────────────────────────────────┐
│                     BACKEND LAYER                             │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure Container Apps                                 │  │
│  │   - Python FastAPI service                             │  │
│  │   - OTIS RAG system (router, retriever, generator)     │  │
│  │   - Auto-scales 0→N based on load                      │  │
│  │   - Container: Python 3.13 + dependencies              │  │
│  └────────────────────┬───────────────────────────────────┘  │
└─────────────────────────┼──────────────────────────────────────┘
                          │ Azure SDK
                          ↓
┌──────────────────────────────────────────────────────────────┐
│                     DATA LAYER                                │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure AI Search                                      │  │
│  │   - 21 indexes (existing)                              │  │
│  │   - program_meta, flow_edges, transactions, etc.       │  │
│  │   - Phase 1+2 enhancements active                      │  │
│  └────────────────────────────────────────────────────────┘  │
│  ┌────────────────────────────────────────────────────────┐  │
│  │   Azure OpenAI Service                                 │  │
│  │   - GPT-4 for answer generation                        │  │
│  │   - text-embedding-3-large for embeddings              │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

---

## Components to Build

### 1. Backend API (FastAPI)

**File: `api_server.py`**

```python
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from otis_rag.rag import OTISRAG
from otis_rag.config import load_config
import logging

app = FastAPI(title="OTIS RAG API", version="1.0.0")

# Enable CORS for Static Web App
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Restrict in production
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize RAG system
config = load_config()
rag = OTISRAG(config)

class QueryRequest(BaseModel):
    question: str
    conversation_history: list = []

class QueryResponse(BaseModel):
    answer: str
    sources: list
    question_type: str
    indexes_used: list

@app.get("/")
def health_check():
    return {"status": "healthy", "version": "1.0.0"}

@app.post("/api/query", response_model=QueryResponse)
async def query_rag(request: QueryRequest):
    try:
        # Process query through RAG system
        result = rag.answer(
            request.question,
            conversation_history=request.conversation_history
        )
        
        return QueryResponse(
            answer=result["answer"],
            sources=result.get("sources", []),
            question_type=result.get("question_type", "general"),
            indexes_used=result.get("indexes_used", [])
        )
    except Exception as e:
        logging.error(f"Query failed: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/api/stats")
def get_stats():
    """Get system statistics."""
    return {
        "total_indexes": len(config.indexes),
        "indexes": list(config.indexes.keys()),
        "phase1_active": True,
        "phase2_active": True
    }
```

**Dependencies: `requirements.txt`**
```
fastapi==0.104.1
uvicorn[standard]==0.24.0
pydantic==2.5.0
azure-search-documents==11.4.0
azure-identity==1.15.0
openai==1.3.0
python-dotenv==1.0.0
```

---

### 2. Frontend Web App (HTML/JS)

**File: `frontend/index.html`**

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OTIS RAG - COBOL Code Assistant</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            height: 100vh;
            display: flex;
            justify-content: center;
            align-items: center;
            padding: 20px;
        }
        
        .container {
            background: white;
            border-radius: 12px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            width: 100%;
            max-width: 900px;
            height: 90vh;
            display: flex;
            flex-direction: column;
        }
        
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 12px 12px 0 0;
        }
        
        .header h1 {
            font-size: 24px;
            margin-bottom: 5px;
        }
        
        .header p {
            opacity: 0.9;
            font-size: 14px;
        }
        
        .chat-container {
            flex: 1;
            overflow-y: auto;
            padding: 20px;
            background: #f7fafc;
        }
        
        .message {
            margin-bottom: 20px;
            animation: slideIn 0.3s ease-out;
        }
        
        @keyframes slideIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        
        .message.user {
            display: flex;
            justify-content: flex-end;
        }
        
        .message.assistant {
            display: flex;
            justify-content: flex-start;
        }
        
        .message-content {
            max-width: 70%;
            padding: 12px 16px;
            border-radius: 8px;
            word-wrap: break-word;
        }
        
        .message.user .message-content {
            background: #667eea;
            color: white;
            border-bottom-right-radius: 4px;
        }
        
        .message.assistant .message-content {
            background: white;
            color: #2d3748;
            border: 1px solid #e2e8f0;
            border-bottom-left-radius: 4px;
        }
        
        .message-meta {
            font-size: 11px;
            opacity: 0.6;
            margin-top: 5px;
        }
        
        .input-container {
            padding: 20px;
            background: white;
            border-top: 1px solid #e2e8f0;
            border-radius: 0 0 12px 12px;
        }
        
        .input-wrapper {
            display: flex;
            gap: 10px;
        }
        
        #questionInput {
            flex: 1;
            padding: 12px 16px;
            border: 2px solid #e2e8f0;
            border-radius: 8px;
            font-size: 14px;
            outline: none;
            transition: border-color 0.2s;
        }
        
        #questionInput:focus {
            border-color: #667eea;
        }
        
        #sendButton {
            padding: 12px 24px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border: none;
            border-radius: 8px;
            font-size: 14px;
            cursor: pointer;
            transition: transform 0.2s, box-shadow 0.2s;
        }
        
        #sendButton:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
        }
        
        #sendButton:disabled {
            opacity: 0.5;
            cursor: not-allowed;
            transform: none;
        }
        
        .loader {
            display: none;
            text-align: center;
            padding: 10px;
            color: #667eea;
        }
        
        .loader.active {
            display: block;
        }
        
        .example-questions {
            padding: 10px 0;
            font-size: 12px;
            color: #718096;
        }
        
        .example-questions span {
            cursor: pointer;
            color: #667eea;
            text-decoration: underline;
            margin-right: 10px;
        }
        
        .example-questions span:hover {
            color: #764ba2;
        }
        
        code {
            background: #f7fafc;
            padding: 2px 6px;
            border-radius: 3px;
            font-family: 'Courier New', monospace;
            font-size: 12px;
        }
        
        pre {
            background: #2d3748;
            color: #f7fafc;
            padding: 12px;
            border-radius: 6px;
            overflow-x: auto;
            margin: 10px 0;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🔍 OTIS RAG - COBOL Code Assistant</h1>
            <p>Ask questions about the OTIS/OTOS COBOL codebase • Phase 1+2 Enhanced</p>
        </div>
        
        <div class="chat-container" id="chatContainer">
            <div class="message assistant">
                <div class="message-content">
                    <strong>Welcome to OTIS RAG!</strong><br><br>
                    I can help you understand the OTIS/OTOS COBOL codebase. Ask me about:
                    <ul style="margin: 10px 0; padding-left: 20px;">
                        <li>Program functionality and workflows</li>
                        <li>Transaction codes and processing</li>
                        <li>Program complexity and performance</li>
                        <li>Data flow and dependencies</li>
                        <li>Historical context and design decisions</li>
                    </ul>
                    Try asking a question below!
                </div>
            </div>
        </div>
        
        <div class="loader" id="loader">
            <p>🤔 Thinking...</p>
        </div>
        
        <div class="input-container">
            <div class="example-questions">
                Try: 
                <span onclick="askExample('What does APIPAY program do?')">APIPAY overview</span>
                <span onclick="askExample('What happens when user enters transaction code PL?')">Transaction PL</span>
                <span onclick="askExample('Which programs are most complex?')">Complex programs</span>
            </div>
            <div class="input-wrapper">
                <input 
                    type="text" 
                    id="questionInput" 
                    placeholder="Ask a question about the OTIS codebase..."
                    onkeypress="handleKeyPress(event)"
                />
                <button id="sendButton" onclick="sendQuestion()">Send</button>
            </div>
        </div>
    </div>

    <script>
        // Configuration
        const API_BASE_URL = 'https://your-container-app.azurecontainerapps.io';
        let conversationHistory = [];

        // Add message to chat
        function addMessage(content, role) {
            const chatContainer = document.getElementById('chatContainer');
            const messageDiv = document.createElement('div');
            messageDiv.className = `message ${role}`;
            
            const contentDiv = document.createElement('div');
            contentDiv.className = 'message-content';
            contentDiv.innerHTML = formatMessage(content);
            
            messageDiv.appendChild(contentDiv);
            chatContainer.appendChild(messageDiv);
            
            // Scroll to bottom
            chatContainer.scrollTop = chatContainer.scrollHeight;
        }

        // Format message with markdown-like styling
        function formatMessage(text) {
            // Convert code blocks
            text = text.replace(/```([^`]+)```/g, '<pre>$1</pre>');
            // Convert inline code
            text = text.replace(/`([^`]+)`/g, '<code>$1</code>');
            // Convert line breaks
            text = text.replace(/\n/g, '<br>');
            return text;
        }

        // Send question to API
        async function sendQuestion() {
            const input = document.getElementById('questionInput');
            const question = input.value.trim();
            
            if (!question) return;
            
            // Disable input
            input.disabled = true;
            document.getElementById('sendButton').disabled = true;
            document.getElementById('loader').classList.add('active');
            
            // Add user message
            addMessage(question, 'user');
            input.value = '';
            
            try {
                const response = await fetch(`${API_BASE_URL}/api/query`, {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify({
                        question: question,
                        conversation_history: conversationHistory
                    })
                });
                
                if (!response.ok) {
                    throw new Error(`HTTP error! status: ${response.status}`);
                }
                
                const data = await response.json();
                
                // Add assistant response
                let answer = data.answer;
                
                // Add metadata
                if (data.question_type) {
                    answer += `\n\n<small style="opacity: 0.6;">Question type: ${data.question_type} | Indexes: ${data.indexes_used.join(', ')}</small>`;
                }
                
                addMessage(answer, 'assistant');
                
                // Update conversation history
                conversationHistory.push({
                    role: 'user',
                    content: question
                });
                conversationHistory.push({
                    role: 'assistant',
                    content: data.answer
                });
                
            } catch (error) {
                addMessage(`❌ Error: ${error.message}. Please check the API connection.`, 'assistant');
            } finally {
                // Re-enable input
                input.disabled = false;
                document.getElementById('sendButton').disabled = false;
                document.getElementById('loader').classList.remove('active');
                input.focus();
            }
        }

        // Handle Enter key
        function handleKeyPress(event) {
            if (event.key === 'Enter') {
                sendQuestion();
            }
        }

        // Ask example question
        function askExample(question) {
            document.getElementById('questionInput').value = question;
            sendQuestion();
        }

        // Focus input on load
        document.addEventListener('DOMContentLoaded', () => {
            document.getElementById('questionInput').focus();
        });
    </script>
</body>
</html>
```

---

### 3. Docker Container Configuration

**File: `Dockerfile`**

```dockerfile
FROM python:3.13-slim

WORKDIR /app

# Copy requirements and install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY otis_rag/ ./otis_rag/
COPY api_server.py .
COPY local.settings.json .

# Expose port
EXPOSE 8000

# Run the FastAPI server
CMD ["uvicorn", "api_server:app", "--host", "0.0.0.0", "--port", "8000"]
```

**File: `docker-compose.yml`** (for local testing)

```yaml
version: '3.8'

services:
  api:
    build: .
    ports:
      - "8000:8000"
    environment:
      - AZURE_SEARCH_ENDPOINT=${AZURE_SEARCH_ENDPOINT}
      - AZURE_SEARCH_KEY=${AZURE_SEARCH_KEY}
      - AZURE_OPENAI_ENDPOINT=${AZURE_OPENAI_ENDPOINT}
      - AZURE_OPENAI_KEY=${AZURE_OPENAI_KEY}
    volumes:
      - ./local.settings.json:/app/local.settings.json
```

---

### 4. Azure Deployment Configuration

**File: `azure-deploy.yml`** (GitHub Actions)

```yaml
name: Deploy to Azure

on:
  push:
    branches: [ main ]
  workflow_dispatch:

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Log in to Azure Container Registry
      uses: docker/login-action@v2
      with:
        registry: ${{ secrets.ACR_LOGIN_SERVER }}
        username: ${{ secrets.ACR_USERNAME }}
        password: ${{ secrets.ACR_PASSWORD }}
    
    - name: Build and push Docker image
      run: |
        docker build -t ${{ secrets.ACR_LOGIN_SERVER }}/otis-rag:latest .
        docker push ${{ secrets.ACR_LOGIN_SERVER }}/otis-rag:latest
    
    - name: Deploy to Azure Container Apps
      uses: azure/container-apps-deploy-action@v1
      with:
        containerAppName: otis-rag-api
        resourceGroup: otis-rag-rg
        imageToDeploy: ${{ secrets.ACR_LOGIN_SERVER }}/otis-rag:latest
```

**File: `staticwebapp.config.json`** (for Static Web App)

```json
{
  "navigationFallback": {
    "rewrite": "/index.html"
  },
  "routes": [
    {
      "route": "/api/*",
      "allowedRoles": ["anonymous"]
    }
  ],
  "mimeTypes": {
    ".json": "application/json",
    ".js": "text/javascript",
    ".css": "text/css"
  }
}
```

---

## Deployment Steps

### Step 1: Create Azure Resources

```bash
# Login to Azure
az login

# Create resource group
az group create --name otis-rag-rg --location eastus

# Create Azure Container Registry (for Docker images)
az acr create --resource-group otis-rag-rg \
  --name otisragacr --sku Basic

# Create Container Apps environment
az containerapp env create \
  --name otis-rag-env \
  --resource-group otis-rag-rg \
  --location eastus

# Create Container App
az containerapp create \
  --name otis-rag-api \
  --resource-group otis-rag-rg \
  --environment otis-rag-env \
  --image mcr.microsoft.com/azuredocs/containerapps-helloworld:latest \
  --target-port 8000 \
  --ingress external \
  --env-vars \
    AZURE_SEARCH_ENDPOINT=$AZURE_SEARCH_ENDPOINT \
    AZURE_SEARCH_KEY=$AZURE_SEARCH_KEY \
    AZURE_OPENAI_ENDPOINT=$AZURE_OPENAI_ENDPOINT \
    AZURE_OPENAI_KEY=$AZURE_OPENAI_KEY

# Create Static Web App
az staticwebapp create \
  --name otis-rag-frontend \
  --resource-group otis-rag-rg \
  --location eastus2 \
  --sku Free
```

### Step 2: Build and Push Docker Image

```bash
# Build the Docker image locally
docker build -t otis-rag-api:latest .

# Tag for ACR
docker tag otis-rag-api:latest otisragacr.azurecr.io/otis-rag-api:latest

# Login to ACR
az acr login --name otisragacr

# Push to ACR
docker push otisragacr.azurecr.io/otis-rag-api:latest

# Update Container App to use new image
az containerapp update \
  --name otis-rag-api \
  --resource-group otis-rag-rg \
  --image otisragacr.azurecr.io/otis-rag-api:latest
```

### Step 3: Deploy Frontend

```bash
# Deploy frontend to Static Web App
# Option 1: Using Azure CLI
az staticwebapp deploy \
  --name otis-rag-frontend \
  --resource-group otis-rag-rg \
  --source ./frontend

# Option 2: Using GitHub Actions (recommended)
# Connect your GitHub repo to Azure Static Web Apps
# It will auto-deploy on push to main branch
```

### Step 4: Configure Environment Variables

```bash
# Update Container App with secrets
az containerapp update \
  --name otis-rag-api \
  --resource-group otis-rag-rg \
  --set-env-vars \
    AZURE_SEARCH_ENDPOINT=secretref:search-endpoint \
    AZURE_SEARCH_KEY=secretref:search-key \
    AZURE_OPENAI_ENDPOINT=secretref:openai-endpoint \
    AZURE_OPENAI_KEY=secretref:openai-key

# Add secrets
az containerapp secret set \
  --name otis-rag-api \
  --resource-group otis-rag-rg \
  --secrets \
    search-endpoint=$AZURE_SEARCH_ENDPOINT \
    search-key=$AZURE_SEARCH_KEY \
    openai-endpoint=$AZURE_OPENAI_ENDPOINT \
    openai-key=$AZURE_OPENAI_KEY
```

---

## Cost Estimation

### Monthly Costs (Assuming low-moderate usage)

| Component | Tier | Cost |
|-----------|------|------|
| **Azure Container Apps** | Consumption (0.5 vCPU, 1GB RAM) | ~$2-5/month |
| **Azure Static Web App** | Free tier | $0 |
| **Azure Container Registry** | Basic | $5/month |
| **Azure AI Search** | Existing | (already deployed) |
| **Azure OpenAI** | Existing | (pay per query) |
| **Total New Cost** | | **~$7-10/month** |

### Cost Optimization

- Container Apps scales to **zero** when not in use
- Static Web App is **FREE** tier
- ACR can be replaced with Docker Hub (free) if needed
- Can reduce to ~$1-2/month with careful configuration

---

## Security Considerations

### 1. Authentication (Optional but Recommended)

```javascript
// Add Azure AD authentication to frontend
// File: frontend/auth.js

const msalConfig = {
    auth: {
        clientId: "YOUR_CLIENT_ID",
        authority: "https://login.microsoftonline.com/YOUR_TENANT_ID",
        redirectUri: window.location.origin
    }
};

const msalInstance = new msal.PublicClientApplication(msalConfig);

async function login() {
    try {
        const response = await msalInstance.loginPopup();
        return response.accessToken;
    } catch (error) {
        console.error("Login failed:", error);
    }
}
```

### 2. API Key Protection

- Store all Azure keys in **Azure Key Vault**
- Use **Managed Identity** for Container Apps
- Never expose keys in frontend code

### 3. CORS Configuration

```python
# In api_server.py, restrict CORS to your domain
app.add_middleware(
    CORSMiddleware,
    allow_origins=["https://your-static-web-app.azurestaticapps.net"],
    allow_credentials=True,
    allow_methods=["POST", "GET"],
    allow_headers=["*"],
)
```

---

## Testing Locally

### 1. Test Backend API

```bash
# Install dependencies
pip install -r requirements.txt

# Run locally
uvicorn api_server:app --reload --port 8000

# Test endpoint
curl -X POST http://localhost:8000/api/query \
  -H "Content-Type: application/json" \
  -d '{"question": "What does APIPAY program do?"}'
```

### 2. Test Frontend

```bash
# Serve frontend locally
cd frontend
python -m http.server 8080

# Update API_BASE_URL in index.html to http://localhost:8000
# Open browser: http://localhost:8080
```

### 3. Test with Docker

```bash
# Build and run
docker-compose up --build

# Test
curl http://localhost:8000/
curl -X POST http://localhost:8000/api/query \
  -H "Content-Type: application/json" \
  -d '{"question": "What does APIPAY program do?"}'
```

---

## Monitoring & Logging

### Application Insights

```python
# Add to api_server.py
from opencensus.ext.azure.log_exporter import AzureLogHandler
import logging

logger = logging.getLogger(__name__)
logger.addHandler(AzureLogHandler(
    connection_string='InstrumentationKey=YOUR_KEY'
))

@app.post("/api/query")
async def query_rag(request: QueryRequest):
    logger.info(f"Query received: {request.question}")
    # ... rest of code
```

### Monitoring Dashboard

- View logs: Azure Portal → Container Apps → Log stream
- View metrics: Requests, Response times, Errors
- Set alerts: If error rate > 5% or response time > 5s

---

## Next Steps

1. **Create API Server** (api_server.py)
2. **Create Frontend** (frontend/index.html)
3. **Test Locally** (Docker Compose)
4. **Create Azure Resources** (az cli commands)
5. **Deploy Backend** (Container Apps)
6. **Deploy Frontend** (Static Web Apps)
7. **Test Production** (curl + browser)
8. **Add Authentication** (Azure AD - optional)
9. **Monitor & Iterate**

---

## Alternative: Simpler Option (If Budget is Tight)

### Just Static Web App + Azure Functions

**Even simpler:**
- Single Azure Functions app (consumption plan - mostly free)
- Frontend hosted as static files
- Total cost: ~$0-1/month

Would you like me to create this simpler version instead?

---

## Files to Create

Ready to implement? I can create:
1. ✅ `api_server.py` - FastAPI backend
2. ✅ `frontend/index.html` - Web interface
3. ✅ `Dockerfile` - Container configuration
4. ✅ `requirements.txt` - Python dependencies
5. ✅ Deployment scripts

Would you like me to create these files now?
