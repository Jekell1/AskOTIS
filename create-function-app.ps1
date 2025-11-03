# Azure Function App Creation Script
# This script creates an Azure Function App for COBOL parsing

# Configuration based on your Azure environment
$RESOURCE_GROUP = "Azure_AI_RG"
$LOCATION = "eastus"
$STORAGE_ACCOUNT = "waazuse1aistorage"
$FUNCTION_APP_NAME = "cobol-parser-func-$(Get-Random -Minimum 1000 -Maximum 9999)"

Write-Host "Creating Azure Function App with the following configuration:" -ForegroundColor Green
Write-Host "Resource Group: $RESOURCE_GROUP" -ForegroundColor Cyan
Write-Host "Location: $LOCATION" -ForegroundColor Cyan
Write-Host "Storage Account: $STORAGE_ACCOUNT" -ForegroundColor Cyan
Write-Host "Function App Name: $FUNCTION_APP_NAME" -ForegroundColor Cyan
Write-Host ""

# Check if Azure CLI is logged in
Write-Host "Checking Azure CLI authentication..." -ForegroundColor Yellow
$azAccount = az account show 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Not logged in to Azure CLI. Please run 'az login' first." -ForegroundColor Red
    exit 1
}

Write-Host "✅ Azure CLI is authenticated" -ForegroundColor Green

# Create the Function App
Write-Host "Creating Function App '$FUNCTION_APP_NAME'..." -ForegroundColor Yellow
$command = @"
az functionapp create \
  --resource-group $RESOURCE_GROUP \
  --consumption-plan-location $LOCATION \
  --runtime python \
  --runtime-version 3.11 \
  --functions-version 4 \
  --name $FUNCTION_APP_NAME \
  --storage-account $STORAGE_ACCOUNT \
  --os-type Linux
"@

Write-Host "Executing command:" -ForegroundColor Cyan
Write-Host $command -ForegroundColor Gray
Write-Host ""

# Execute the command
Invoke-Expression "az functionapp create --resource-group $RESOURCE_GROUP --consumption-plan-location $LOCATION --runtime python --runtime-version 3.11 --functions-version 4 --name $FUNCTION_APP_NAME --storage-account $STORAGE_ACCOUNT --os-type Linux"

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "✅ Function App created successfully!" -ForegroundColor Green
    Write-Host "Function App URL: https://$FUNCTION_APP_NAME.azurewebsites.net" -ForegroundColor Cyan
    
    # Save configuration for later use
    $config = @{
        functionAppName = $FUNCTION_APP_NAME
        resourceGroup = $RESOURCE_GROUP
        storageAccount = $STORAGE_ACCOUNT
        location = $LOCATION
        functionAppUrl = "https://$FUNCTION_APP_NAME.azurewebsites.net"
    }
    
    $config | ConvertTo-Json | Out-File -FilePath "function-app-config.json" -Encoding UTF8
    Write-Host "✅ Configuration saved to function-app-config.json" -ForegroundColor Green
    
    Write-Host ""
    Write-Host "Next steps:" -ForegroundColor Yellow
    Write-Host "1. Deploy your COBOL parsing function code to the Function App" -ForegroundColor White
    Write-Host "2. Get the function key using: az functionapp function keys list" -ForegroundColor White
    Write-Host "3. Update your skillset with the Function App URL and key" -ForegroundColor White
    
} else {
    Write-Host ""
    Write-Host "❌ Failed to create Function App" -ForegroundColor Red
    Write-Host "Please check the error message above and try again." -ForegroundColor Red
}
