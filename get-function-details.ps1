# Get Azure Function Details
# This script retrieves the function URL and keys for the COBOL parser function

$RESOURCE_GROUP = "Azure_AI_RG"
$FUNCTION_APP_NAME = "cobol-parser-func-1474"
$SUBSCRIPTION_ID = "00de2929-ebe2-437d-bae3-f66a78372346"
$FUNCTION_NAME = "cobol-parse"

Write-Host "Getting Azure Function details..." -ForegroundColor Green
Write-Host "Resource Group: $RESOURCE_GROUP" -ForegroundColor Cyan
Write-Host "Function App: $FUNCTION_APP_NAME" -ForegroundColor Cyan
Write-Host "Function Name: $FUNCTION_NAME" -ForegroundColor Cyan
Write-Host "Subscription: $SUBSCRIPTION_ID" -ForegroundColor Cyan
Write-Host ""

# Check if Azure CLI is logged in
Write-Host "Checking Azure CLI authentication..." -ForegroundColor Yellow
$azAccount = az account show 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Not logged in to Azure CLI. Please run 'az login' first." -ForegroundColor Red
    exit 1
}
Write-Host "✅ Azure CLI is authenticated" -ForegroundColor Green
Write-Host ""

# Get the function URL template
Write-Host "Getting function URL template..." -ForegroundColor Yellow
$urlTemplate = az functionapp function show --resource-group $RESOURCE_GROUP --name $FUNCTION_APP_NAME --function-name $FUNCTION_NAME --query "invokeUrlTemplate" -o tsv

if ($LASTEXITCODE -eq 0 -and $urlTemplate) {
    Write-Host "✅ Function URL Template: $urlTemplate" -ForegroundColor Green
} else {
    Write-Host "❌ Failed to get function URL template" -ForegroundColor Red
    Write-Host "This might be because the function is not yet fully deployed or the name is incorrect." -ForegroundColor Yellow
}
Write-Host ""

# Get the function keys
Write-Host "Getting function keys..." -ForegroundColor Yellow
$keysUrl = "https://management.azure.com/subscriptions/$SUBSCRIPTION_ID/resourceGroups/$RESOURCE_GROUP/providers/Microsoft.Web/sites/$FUNCTION_APP_NAME/functions/$FUNCTION_NAME/listKeys?api-version=2022-03-01"

Write-Host "Keys URL: $keysUrl" -ForegroundColor Cyan

try {
    $keysResponse = az rest -m post -u $keysUrl | ConvertFrom-Json
    
    if ($keysResponse) {
        Write-Host "✅ Function Keys Retrieved:" -ForegroundColor Green
        
        if ($keysResponse.default) {
            Write-Host "  Default Key: $($keysResponse.default)" -ForegroundColor Cyan
            $defaultKey = $keysResponse.default
        }
        
        if ($keysResponse.keys) {
            Write-Host "  Available Keys:" -ForegroundColor Cyan
            foreach ($key in $keysResponse.keys.PSObject.Properties) {
                Write-Host "    $($key.Name): $($key.Value)" -ForegroundColor White
            }
        }
        
        # Construct the complete function URL
        if ($urlTemplate -and $defaultKey) {
            $completeUrl = $urlTemplate.Replace("{*restOfPath}", "").TrimEnd('/') + "?code=$defaultKey"
            Write-Host ""
            Write-Host "✅ Complete Function URL:" -ForegroundColor Green
            Write-Host "$completeUrl" -ForegroundColor Cyan
            
            # Save the configuration
            $config = @{
                functionAppName = $FUNCTION_APP_NAME
                resourceGroup = $RESOURCE_GROUP
                functionName = $FUNCTION_NAME
                urlTemplate = $urlTemplate
                defaultKey = $defaultKey
                completeUrl = $completeUrl
                subscriptionId = $SUBSCRIPTION_ID
                retrievedAt = (Get-Date).ToString()
            }
            
            $config | ConvertTo-Json | Out-File -FilePath "function-url-config.json" -Encoding UTF8
            Write-Host "✅ Configuration saved to function-url-config.json" -ForegroundColor Green
        }
        
    } else {
        Write-Host "❌ No keys response received" -ForegroundColor Red
    }
    
} catch {
    Write-Host "❌ Failed to get function keys: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host "This might be a permissions issue or the function might not be fully deployed." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Summary:" -ForegroundColor Yellow
Write-Host "- Function App: https://$FUNCTION_APP_NAME.azurewebsites.net" -ForegroundColor White
Write-Host "- Function Endpoint: https://$FUNCTION_APP_NAME.azurewebsites.net/api/$FUNCTION_NAME" -ForegroundColor White
Write-Host "- Resource Group: $RESOURCE_GROUP" -ForegroundColor White

Write-Host ""
Write-Host "Next Steps:" -ForegroundColor Yellow
Write-Host "1. Use the complete URL in your Azure Cognitive Search skillset" -ForegroundColor White
Write-Host "2. Test the function endpoint if needed" -ForegroundColor White
Write-Host "3. Create the skillset and indexer" -ForegroundColor White
