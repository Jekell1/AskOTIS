# ChatGPT Action Deployment Guide
## COBOL Search API Integration

### 1. OpenAPI Schema Configuration

Copy this complete OpenAPI 3.1 spec into your ChatGPT Action:

```yaml
openapi: 3.1.0
info:
  title: COBOL Search API
  version: 1.0.0
  description: Search COBOL code at line level using Azure AI Search
servers:
  - url: https://az-use1-ai-search.search.windows.net
paths:
  /indexes/cobol-index/docs/search:
    post:
      summary: Search the COBOL index
      description: Search across indexed COBOL code lines with full-text search and symbol filtering
      security:
        - apiKeyHeader: []
      parameters:
        - in: query
          name: api-version
          required: true
          schema:
            type: string
            default: "2024-07-01"
      requestBody:
        required: true
        content:
          application/json:
            schema:
              type: object
              properties:
                search:
                  type: string
                  description: Search query - use quotes for exact matches, * for all
                  example: "CUSTOMER-ACCNT-NO"
                select:
                  type: string
                  description: Comma-separated fields to return
                  example: "repo_path,line,code,symbol_name,symbol_kind"
                filter:
                  type: string
                  description: OData filter expression
                  example: "symbol_kind eq 'program'"
                top:
                  type: integer
                  description: Number of results to return (max 1000)
                  default: 50
                  maximum: 1000
                count:
                  type: boolean
                  description: Include total count in response
                  default: false
              required:
                - search
      responses:
        "200":
          description: Search results
          content:
            application/json:
              schema:
                type: object
                properties:
                  value:
                    type: array
                    items:
                      type: object
                      properties:
                        id:
                          type: string
                        repo_path:
                          type: string
                        line:
                          type: integer
                        code:
                          type: string
                        symbol_name:
                          type: string
                        symbol_kind:
                          type: string
                        calls:
                          type: array
                          items:
                            type: string
        "400":
          description: Bad request
        "401":
          description: Unauthorized
components:
  securitySchemes:
    apiKeyHeader:
      type: apiKey
      in: header
      name: api-key
```

### 2. Action Settings Configuration

**Base URL:** `https://az-use1-ai-search.search.windows.net`

**Authentication:**
- Type: `API Key`
- Header Name: `api-key`
- Secret Name: `AZURE_SEARCH_API_KEY`

**Secret Value:**
```
AZURE_SEARCH_API_KEY = ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef
```

**Default Headers:**
```
api-key: {{AZURE_SEARCH_API_KEY}}
```

### 3. Testing Your Action

Once deployed, test with these queries:

**Count all documents:**
```json
{
  "search": "*",
  "count": true,
  "top": 0
}
```

**Find programs:**
```json
{
  "search": "*",
  "filter": "symbol_kind eq 'program'",
  "select": "repo_path,line,code,symbol_name",
  "top": 10
}
```

**Search for variables:**
```json
{
  "search": "CUSTOMER-ACCNT-NO",
  "filter": "symbol_kind eq 'variable'",
  "select": "repo_path,line,code,symbol_name,symbol_kind",
  "top": 20
}
```

### 4. GPT Instructions for Natural Language

Add these instructions to your GPT:

```
When users ask about COBOL code, use the search API:

1. **Variable searches**: Use filter "symbol_kind eq 'variable'" 
2. **Program searches**: Use filter "symbol_kind eq 'program'"
3. **Code patterns**: Search COBOL keywords without filters
4. **Always include**: repo_path, line, code in select for context
5. **Limit results**: Use top parameter (default 50, max 1000)
6. **Count queries**: Set count=true and top=0 for totals

Example prompts:
- "Find all uses of CUSTOMER-ACCNT-NO"
- "Show me COBOL programs starting with AC"
- "Search for IDENTIFICATION DIVISION"
- "How many COBOL lines are indexed?"
```

### 5. Troubleshooting Guide

**No Results?**
- Try broader searches (remove quotes)
- Check symbol_kind filter is correct
- Verify search term exists in codebase

**Authentication Errors?**
- Confirm API key is correctly set in Action secrets
- Check Azure Search service is running
- Verify index name is 'cobol-index'

**Skill/Processing Errors?**
- Check Function App logs for batch processing issues
- Verify skillset output mappings match index fields
- Confirm JSONL parsing mode is set correctly

### 6. Current Index Statistics
- **Documents**: ~965,669 COBOL lines indexed
- **Files**: ~9,952 COBOL source files  
- **Symbol Types**: program, variable, procedure, section
- **Search Capabilities**: Full-text search, symbol filtering, code pattern matching

Your COBOL Researcher GPT Action is ready to search through all COBOL code at line level!
