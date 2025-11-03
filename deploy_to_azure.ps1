# Azure Functions Deployment Helper
# Run this script to deploy the OTIS RAG system to Azure

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  OTIS RAG - Azure Functions Deployment" -ForegroundColor Cyan
Write-Host "========================================`n" -ForegroundColor Cyan

# Check if Azure CLI is installed
$azInstalled = Get-Command az -ErrorAction SilentlyContinue
if (-not $azInstalled) {
    Write-Host "❌ Azure CLI not found. Please install from: https://aka.ms/InstallAzureCli" -ForegroundColor Red
    exit 1
}

Write-Host "✅ Azure CLI found" -ForegroundColor Green

# Check if Azure Functions Core Tools is installed
$funcInstalled = Get-Command func -ErrorAction SilentlyContinue
if (-not $funcInstalled) {
    Write-Host "⚠️  Azure Functions Core Tools not found" -ForegroundColor Yellow
    Write-Host "   Install from: https://aka.ms/func-install" -ForegroundColor Yellow
    Write-Host "   Or use npm: npm install -g azure-functions-core-tools@4" -ForegroundColor Yellow
    $continue = Read-Host "`nContinue anyway? (y/n)"
    if ($continue -ne 'y') { exit 1 }
}

# Configuration
$RESOURCE_GROUP = "Azure_AI_RG"
$LOCATION = "eastus"
$STORAGE_ACCOUNT = "stotisrag$(Get-Random -Maximum 9999)"
$FUNCTION_APP = "func-otis-rag-$(Get-Random -Maximum 9999)"
$SUBSCRIPTION = "World Azure Prod"

Write-Host "`nConfiguration:" -ForegroundColor Cyan
Write-Host "  Resource Group: $RESOURCE_GROUP" -ForegroundColor Gray
Write-Host "  Location: $LOCATION" -ForegroundColor Gray
Write-Host "  Storage Account: $STORAGE_ACCOUNT" -ForegroundColor Gray
Write-Host "  Function App: $FUNCTION_APP" -ForegroundColor Gray
Write-Host "  Subscription: $SUBSCRIPTION`n" -ForegroundColor Gray

$proceed = Read-Host "Proceed with deployment? (y/n)"
if ($proceed -ne 'y') {
    Write-Host "Deployment cancelled." -ForegroundColor Yellow
    exit 0
}

Write-Host "`n📋 Step 1: Setting subscription..." -ForegroundColor Cyan
az account set --subscription $SUBSCRIPTION
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to set subscription" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Subscription set" -ForegroundColor Green

Write-Host "`n📋 Step 2: Verifying resource group exists..." -ForegroundColor Cyan
az group show --name $RESOURCE_GROUP --output none
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Resource group '$RESOURCE_GROUP' not found" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Resource group verified" -ForegroundColor Green

Write-Host "`n📋 Step 3: Creating storage account..." -ForegroundColor Cyan
az storage account create `
    --name $STORAGE_ACCOUNT `
    --resource-group $RESOURCE_GROUP `
    --location $LOCATION `
    --sku Standard_LRS
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to create storage account" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Storage account created" -ForegroundColor Green

Write-Host "`n📋 Step 4: Creating Function App..." -ForegroundColor Cyan
az functionapp create `
    --name $FUNCTION_APP `
    --resource-group $RESOURCE_GROUP `
    --storage-account $STORAGE_ACCOUNT `
    --consumption-plan-location $LOCATION `
    --runtime python `
    --runtime-version 3.11 `
    --functions-version 4 `
    --os-type Linux
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to create Function App" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Function App created" -ForegroundColor Green

Write-Host "`n📋 Step 5: Configuring application settings..." -ForegroundColor Cyan
Write-Host "   You need to provide these values from your .env file:" -ForegroundColor Yellow

$AZURE_SEARCH_ENDPOINT = Read-Host "   AZURE_SEARCH_ENDPOINT"
$AZURE_SEARCH_API_KEY = Read-Host "   AZURE_SEARCH_API_KEY" -AsSecureString
$AZURE_OPENAI_ENDPOINT = Read-Host "   AZURE_OPENAI_ENDPOINT"
$AZURE_OPENAI_API_KEY = Read-Host "   AZURE_OPENAI_API_KEY" -AsSecureString
$AZURE_OPENAI_CHAT_DEPLOYMENT = Read-Host "   AZURE_OPENAI_CHAT_DEPLOYMENT"
$AZURE_OPENAI_EMBEDDING_DEPLOYMENT = Read-Host "   AZURE_OPENAI_EMBEDDING_DEPLOYMENT"

# Convert SecureString to plain text for Azure CLI
$BSTR_SEARCH = [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($AZURE_SEARCH_API_KEY)
$PLAIN_SEARCH = [System.Runtime.InteropServices.Marshal]::PtrToStringAuto($BSTR_SEARCH)

$BSTR_OPENAI = [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($AZURE_OPENAI_API_KEY)
$PLAIN_OPENAI = [System.Runtime.InteropServices.Marshal]::PtrToStringAuto($BSTR_OPENAI)

az functionapp config appsettings set `
    --name $FUNCTION_APP `
    --resource-group $RESOURCE_GROUP `
    --settings `
        "AZURE_SEARCH_ENDPOINT=$AZURE_SEARCH_ENDPOINT" `
        "AZURE_SEARCH_API_KEY=$PLAIN_SEARCH" `
        "AZURE_OPENAI_ENDPOINT=$AZURE_OPENAI_ENDPOINT" `
        "AZURE_OPENAI_API_KEY=$PLAIN_OPENAI" `
        "AZURE_OPENAI_CHAT_DEPLOYMENT=$AZURE_OPENAI_CHAT_DEPLOYMENT" `
        "AZURE_OPENAI_EMBEDDING_DEPLOYMENT=$AZURE_OPENAI_EMBEDDING_DEPLOYMENT"

Write-Host "✅ Application settings configured" -ForegroundColor Green

Write-Host "`n📋 Step 6: Deploying function code..." -ForegroundColor Cyan
if ($funcInstalled) {
    func azure functionapp publish $FUNCTION_APP
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ Failed to deploy function code" -ForegroundColor Red
        exit 1
    }
    Write-Host "✅ Function code deployed" -ForegroundColor Green
} else {
    Write-Host "⚠️  Skipping code deployment (func command not found)" -ForegroundColor Yellow
    Write-Host "   Deploy manually using: func azure functionapp publish $FUNCTION_APP" -ForegroundColor Yellow
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  ✅ DEPLOYMENT COMPLETE!" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

Write-Host "Your Function App URL:" -ForegroundColor Cyan
Write-Host "  https://$FUNCTION_APP.azurewebsites.net`n" -ForegroundColor White

Write-Host "API Endpoints:" -ForegroundColor Cyan
Write-Host "  Health: https://$FUNCTION_APP.azurewebsites.net/api/health" -ForegroundColor Gray
Write-Host "  Query:  https://$FUNCTION_APP.azurewebsites.net/api/query" -ForegroundColor Gray
Write-Host "  Stats:  https://$FUNCTION_APP.azurewebsites.net/api/stats`n" -ForegroundColor Gray

Write-Host "Next Steps:" -ForegroundColor Cyan
Write-Host "  1. Update frontend/index.html with your Function App URL" -ForegroundColor Yellow
Write-Host "  2. Deploy frontend to Azure Static Web App" -ForegroundColor Yellow
Write-Host "  3. Test the endpoints" -ForegroundColor Yellow

Write-Host "`nTest with curl:" -ForegroundColor Cyan
Write-Host "  curl https://$FUNCTION_APP.azurewebsites.net/api/health`n" -ForegroundColor Gray
