# AskOTIS - AI-Powered COBOL Code Assistant

AskOTIS is an intelligent chatbot system that helps developers understand and navigate the OTIS legacy COBOL codebase using Retrieval-Augmented Generation (RAG) and Azure AI Search.

## Overview

This system provides:
- **Natural Language Queries**: Ask questions about COBOL code in plain English
- **Code Navigation**: Find dependencies, call flows, and program relationships
- **Screen Flow Analysis**: Understand UI navigation paths through the system
- **Contextual Answers**: Get relevant code snippets and documentation

## Architecture

- **Frontend**: Static web application deployed on Azure Static Web Apps
- **Backend**: Azure Functions for serverless API hosting
- **Search**: Azure AI Search with vector embeddings for semantic search
- **AI**: OpenAI GPT-4 for natural language understanding and response generation

## Prerequisites

- Azure Subscription
- Python 3.9+
- Node.js 18+
- Azure CLI
- Git

## Project Structure

```
AskOTIS/
├── deploy/                      # Static web app frontend
│   ├── index.html              # Main chat interface
│   ├── maintenance.html        # Maintenance page
│   └── staticwebapp.config.json # SWA configuration
├── cobol-function/             # Azure Functions backend
│   ├── function_app.py         # Function implementation
│   ├── requirements.txt        # Python dependencies
│   └── host.json              # Function host config
├── otis_rag/                   # RAG system implementation
│   ├── rag.py                 # Main RAG orchestrator
│   ├── retriever.py           # Search retrieval logic
│   ├── router.py              # Query routing
│   ├── generator.py           # Response generation
│   └── config.py              # Configuration
├── .env.template               # Environment variables template
└── README.md                   # This file
```

## Quick Start

### 1. Clone the Repository

```bash
git clone https://github.com/Jekell1/AskOTIS.git
cd AskOTIS
```

### 2. Set Up Environment Variables

Copy the template and fill in your values:

```bash
cp .env.template .env
```

Required variables:
- `AZURE_SEARCH_ENDPOINT`: Your Azure AI Search endpoint
- `AZURE_SEARCH_ADMIN_KEY`: Admin key for search service
- `OPENAI_API_KEY`: OpenAI API key for GPT-4
- `OPENAI_EMBEDDING_DEPLOYMENT`: Name of your embedding model deployment

### 3. Deploy Azure Infrastructure

#### Create Azure AI Search Service

```bash
az search service create \
  --name <your-search-service> \
  --resource-group <your-rg> \
  --sku standard \
  --location eastus2
```

#### Create Static Web App

```bash
az staticwebapp create \
  --name AskOTIS \
  --resource-group <your-rg> \
  --location eastus2
```

#### Deploy Azure Function

```bash
cd cobol-function
func azure functionapp publish <your-function-app>
```

### 4. Deploy Frontend

Get your Static Web App deployment token:

```bash
az staticwebapp secrets list \
  --name AskOTIS \
  --query "properties.apiKey" \
  --output tsv
```

Deploy:

```bash
npx @azure/static-web-apps-cli deploy ./deploy \
  --deployment-token <your-token>
```

## Configuration

### Azure AD Authentication

Configure authentication in `deploy/staticwebapp.config.json`:

```json
{
  "auth": {
    "identityProviders": {
      "azureActiveDirectory": {
        "registration": {
          "openIdIssuer": "https://login.microsoftonline.com/<tenant-id>/v2.0",
          "clientIdSettingName": "AZURE_CLIENT_ID",
          "clientSecretSettingName": "AZURE_CLIENT_SECRET"
        }
      }
    }
  }
}
```

Add application settings to Static Web App:

```bash
az staticwebapp appsettings set \
  --name AskOTIS \
  --setting-names AZURE_CLIENT_ID=<your-client-id> \
                  AZURE_CLIENT_SECRET=<your-secret>
```

### Search Index Configuration

The system uses multiple Azure AI Search indexes:

- **program_meta**: Program metadata and descriptions
- **code_chunks**: Source code chunks with embeddings
- **copybook_usage**: Copybook dependencies
- **menu_trees**: UI navigation hierarchies
- **screen_nodes**: Screen definitions and flows
- **ui_paths**: Complete UI navigation paths

## Usage

### Web Interface

Navigate to your Static Web App URL and start asking questions:

- "What does LONPF2 do?"
- "Show me the flow for loan payment processing"
- "What programs use the SCREEN copybook?"
- "How do I get to the collection maintenance screen?"

### API Usage

```python
import requests

response = requests.post(
    "https://<your-function-app>.azurewebsites.net/api/ask",
    json={"question": "What does APIPAY do?"}
)

print(response.json()["answer"])
```

## Development

### Local Development

1. Install dependencies:

```bash
pip install -r cobol-function/requirements.txt
pip install -r otis_rag/requirements.txt
```

2. Run the RAG system locally:

```bash
python otis_rag/cli.py
```

3. Test the function locally:

```bash
cd cobol-function
func start
```

4. Serve the frontend locally:

```bash
cd deploy
python -m http.server 8000
```

### Testing

Run comprehensive tests:

```bash
python -m pytest tests/
```

## Maintenance

### Updating the Index

To rebuild search indexes with new data:

```bash
python build_complete_program_meta.py
python build_complete_ui_paths.py
```

### Monitoring

View function logs:

```bash
az functionapp logs tail \
  --name <your-function-app> \
  --resource-group <your-rg>
```

## Security

- All secrets must be stored in environment variables
- Azure AD authentication required for production
- API keys rotated regularly
- No hardcoded credentials in code

## Troubleshooting

### Common Issues

**Search returns no results:**
- Verify search endpoint and key
- Check index exists and has documents
- Ensure embeddings are generated

**Function timeout:**
- Increase timeout in `host.json`
- Optimize search queries
- Enable application insights

**Authentication fails:**
- Verify Azure AD app registration
- Check redirect URIs match
- Ensure client secret is valid

## Contributing

1. Create a feature branch
2. Make your changes
3. Test thoroughly
4. Submit a pull request

## License

Internal use only - World Acceptance Corporation

## Support

For issues or questions:
- Email: jeff.childers@worldacceptance.com
- Teams: Jeff Childers
