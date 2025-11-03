# GPT Action Configuration Guide
# ================================

## 1. OpenAPI Schema
# Use the content from gpt-action-openapi.yaml

## 2. Action Settings

### Base URL:
https://az-use1-ai-search.search.windows.net

### Authentication:
Type: API Key
Header Name: api-key
Secret Name: AZURE_SEARCH_API_KEY

### Secret Value:
AZURE_SEARCH_API_KEY = <AZURE_SEARCH_ADMIN_KEY>

## 3. Default Headers (Optional)
# The API key will be automatically added via the authentication setup above
# No additional default headers needed

## 4. Example Queries for Testing

### Search for programs:
{
  "search": "*",
  "filter": "symbol_kind eq 'program'",
  "select": "repo_path,line,code,symbol_name",
  "top": 10
}

### Search for specific variable:
{
  "search": "\"CUSTOMER-ACCNT-NO\"",
  "filter": "symbol_kind eq 'variable'",
  "select": "repo_path,line,code,symbol_name,symbol_kind",
  "top": 20
}

### Search code patterns:
{
  "search": "IDENTIFICATION DIVISION",
  "select": "repo_path,line,code",
  "top": 15
}

### Get document count:
{
  "search": "*",
  "count": true,
  "top": 0
}

## 5. GPT Instructions for the Action

When users ask about COBOL code, use the search API to:

1. **Variable searches**: Use filter "symbol_kind eq 'variable'" and search for the variable name
2. **Program searches**: Use filter "symbol_kind eq 'program'" to find program definitions  
3. **Code pattern searches**: Search for COBOL keywords, divisions, sections without filters
4. **File-specific searches**: Include repo_path in select to show which files contain results
5. **Always include**: repo_path, line, code in select for context
6. **For large results**: Use top parameter to limit results (default 50, max 1000)
7. **Count queries**: Set count=true and top=0 to get total matches

## 6. Sample Prompts for Users

"Find all occurrences of CUSTOMER-ACCNT-NO in the COBOL code"
"Show me all COBOL programs that start with 'AC'"  
"Search for IDENTIFICATION DIVISION statements"
"Find variables containing 'DATE' in their name"
"How many COBOL documents are indexed?"
"Show me PROCEDURE DIVISION statements"

## 7. Troubleshooting

If no results:
- Check if the search term exists in the codebase
- Try broader searches (remove quotes, use wildcards)
- Check symbol_kind filter is appropriate

If errors:
- Verify API key is correct
- Check Azure Search service is running
- Confirm index name is 'cobol-index'

## 8. Current Index Stats
- Total Documents: ~965,669 COBOL lines
- Symbol Types: program, variable, procedure, section
- Files: ~9,952 COBOL source files
- Search Fields: code, symbol_name, repo_path
