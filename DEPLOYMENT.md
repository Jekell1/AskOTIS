# AskOTIS Deployment Guide

## Repository Overview

The **AskOTIS** repository now contains only the essential files needed to build and run the application. All test scripts, analysis tools, and temporary files have been removed.

## What's Included

### 📁 Core Components

1. **`/deploy`** - Static Web App Frontend
   - `index.html` - Main chat interface
   - `maintenance.html` - Maintenance page
   - `staticwebapp.config.json` - Authentication and routing configuration

2. **`/cobol-function`** - Azure Functions Backend
   - `function_app.py` - Main HTTP trigger function
   - `requirements.txt` - Python dependencies
   - `host.json` - Function host configuration

3. **`/otis_rag`** - RAG System
   - `rag.py` - Main RAG orchestrator
   - `retriever.py` - Search and retrieval logic
   - `router.py` - Query classification and routing
   - `generator.py` - Response generation with GPT-4
   - `config.py` - Configuration management
   - `memory.py` - Conversation memory
   - `prompts.py` - System prompts

4. **Configuration Files**
   - `.env.template` - Environment variables template
   - `.gitignore` - Git ignore rules
   - `README.md` - Main documentation

## Quick Deployment

### Prerequisites

```bash
# Install Azure CLI
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash

# Install Azure Functions Core Tools
npm install -g azure-functions-core-tools@4 --unsafe-perm true

# Install Azure Static Web Apps CLI
npm install -g @azure/static-web-apps-cli
```

### Step 1: Configure Environment

```bash
# Copy template and edit with your values
cp .env.template .env

# Required variables:
# - AZURE_OPENAI_ENDPOINT
# - AZURE_OPENAI_KEY
# - AZURE_SEARCH_ENDPOINT
# - AZURE_SEARCH_ADMIN_KEY
```

### Step 2: Deploy Azure Function

```bash
cd cobol-function

# Login to Azure
az login

# Create function app (if not exists)
az functionapp create \
  --name <your-function-app-name> \
  --storage-account <your-storage-account> \
  --resource-group <your-resource-group> \
  --consumption-plan-location eastus2 \
  --runtime python \
  --runtime-version 3.11 \
  --functions-version 4

# Deploy function
func azure functionapp publish <your-function-app-name>
```

### Step 3: Deploy Static Web App

```bash
# Get deployment token
az staticwebapp secrets list \
  --name AskOTIS \
  --query "properties.apiKey" \
  --output tsv

# Deploy
npx @azure/static-web-apps-cli deploy ./deploy \
  --deployment-token <your-token>
```

### Step 4: Configure Authentication

1. Create Azure AD App Registration (see `IT-Request-AskOTIS-AppRegistration.txt` in old repo)
2. Add application settings to Static Web App:

```bash
az staticwebapp appsettings set \
  --name AskOTIS \
  --setting-names \
    AZURE_CLIENT_ID=<client-id> \
    AZURE_CLIENT_SECRET=<client-secret>
```

## Environment Variables Reference

### Required for Function

| Variable | Description | Example |
|----------|-------------|---------|
| `AZURE_OPENAI_ENDPOINT` | Azure OpenAI endpoint | `https://xxx.openai.azure.com/` |
| `AZURE_OPENAI_KEY` | Azure OpenAI API key | `abc123...` |
| `AZURE_OPENAI_DEPLOYMENT` | GPT model deployment name | `gpt-4` |
| `AZURE_SEARCH_ENDPOINT` | Azure AI Search endpoint | `https://xxx.search.windows.net` |
| `AZURE_SEARCH_ADMIN_KEY` | Search admin key | `xyz789...` |

### Optional

| Variable | Description | Default |
|----------|-------------|---------|
| `OPENAI_EMBEDDING_DEPLOYMENT` | Embedding model | `text-embedding-3-large` |
| `OPENAI_EMBEDDING_DIMENSIONS` | Embedding dimensions | `3072` |
| `LOG_LEVEL` | Logging level | `INFO` |

## Testing Locally

### Test RAG System

```bash
cd otis_rag
pip install -r requirements.txt
python cli.py
```

### Test Azure Function

```bash
cd cobol-function
pip install -r requirements.txt
func start
```

Test with curl:

```bash
curl -X POST http://localhost:7071/api/ask \
  -H "Content-Type: application/json" \
  -d '{"question": "What does LONPF2 do?"}'
```

### Test Frontend Locally

```bash
cd deploy
python -m http.server 8000
# Open http://localhost:8000
```

## Production Checklist

- [ ] All environment variables configured
- [ ] Azure Function deployed and running
- [ ] Static Web App deployed
- [ ] Azure AD authentication configured
- [ ] Search indexes populated
- [ ] Application Insights enabled
- [ ] Secrets rotated (after initial setup)
- [ ] Monitoring dashboards configured

## Security Notes

⚠️ **IMPORTANT**: 
- Never commit `.env` file
- Never hardcode secrets in code
- Rotate keys regularly
- Use Azure Key Vault for production secrets
- Enable Azure AD authentication before going live

## Monitoring

### View Function Logs

```bash
az functionapp logs tail \
  --name <your-function-app> \
  --resource-group <your-resource-group>
```

### View Static Web App Logs

```bash
az staticwebapp logs \
  --name AskOTIS \
  --resource-group <your-resource-group>
```

## Troubleshooting

### Function Not Responding

1. Check function logs in Azure Portal
2. Verify environment variables are set
3. Check Application Insights for errors

### Search Not Working

1. Verify search endpoint and key
2. Check indexes exist: `az search index list`
3. Verify documents in index

### Authentication Issues

1. Check Azure AD app registration
2. Verify redirect URIs match
3. Ensure client secret hasn't expired

## Support

For issues or questions:
- Email: jeff.childers@worldacceptance.com
- Teams: Jeff Childers

## Next Steps After Deployment

1. Test the application thoroughly
2. Monitor Application Insights for errors
3. Set up alerts for failures
4. Configure backup and disaster recovery
5. Document any customizations

---

**Last Updated**: November 3, 2025
**Version**: 1.0
**Status**: Production Ready ✅
