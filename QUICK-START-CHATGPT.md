# 🚀 ChatGPT Action Quick Setup Guide
## COBOL Code Researcher

### ⚡ 5-Minute Deployment

**1. Copy OpenAPI Schema**
Use: `chatgpt-action-openapi.yaml` (complete schema ready for ChatGPT)

**2. Action Settings**
```
Base URL: https://az-use1-ai-search.search.windows.net
Authentication: API Key (Header: api-key)
Secret: AZURE_SEARCH_API_KEY = ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef
```

**3. Test Query**
```json
{
  "search": "*",
  "count": true,
  "top": 0
}
```
Expected result: `~1,307,985` documents

### 🎯 User Prompt Examples

- "Find all COBOL programs that contain CUSTOMER"
- "Show me variables with 'DATE' in the name"
- "Search for IDENTIFICATION DIVISION statements"
- "How many COBOL documents are indexed?"
- "Find all CALL statements in the code"

### 🔧 Current Status
✅ **1,307,985 COBOL documents** indexed and searchable  
✅ **Line-level parsing** with symbol extraction  
✅ **Full-text search** with ranking  
✅ **Symbol filtering** (programs, variables, procedures)  
✅ **API tested and verified**

Your COBOL Researcher GPT Action is ready to deploy! 🎉
