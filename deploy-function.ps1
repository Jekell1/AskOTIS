# Deploy COBOL Function to Azure
# This script packages and deploys the COBOL parsing function

$FUNCTION_APP_NAME = "cobol-parser-func-1474"
$RESOURCE_GROUP = "Azure_AI_RG"

Write-Host "Deploying COBOL Function to Azure..." -ForegroundColor Green
Write-Host "Function App: $FUNCTION_APP_NAME" -ForegroundColor Cyan
Write-Host "Resource Group: $RESOURCE_GROUP" -ForegroundColor Cyan
Write-Host ""

# Navigate to the function directory
Push-Location "cobol-function"

try {
    # Create a deployment package
    Write-Host "Creating deployment package..." -ForegroundColor Yellow
    
    # Remove any existing zip file
    if (Test-Path "deployment.zip") {
        Remove-Item "deployment.zip" -Force
    }
    
    # Create zip file with all necessary files
    $files = @(
        "function_app.py",
        "requirements.txt", 
        "host.json"
    )
    
    # Check that all required files exist
    $missingFiles = @()
    foreach ($file in $files) {
        if (!(Test-Path $file)) {
            $missingFiles += $file
        }
    }
    
    if ($missingFiles.Count -gt 0) {
        Write-Host "❌ Missing required files:" -ForegroundColor Red
        foreach ($file in $missingFiles) {
            Write-Host "  - $file" -ForegroundColor Red
        }
        exit 1
    }
    
    # Create the zip file
    Compress-Archive -Path $files -DestinationPath "deployment.zip" -Force
    Write-Host "✅ Created deployment.zip" -ForegroundColor Green
    
    # Deploy using Azure CLI
    Write-Host "Deploying to Azure Function App..." -ForegroundColor Yellow
    $deployResult = az functionapp deployment source config-zip --resource-group $RESOURCE_GROUP --name $FUNCTION_APP_NAME --src "deployment.zip" 2>&1
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Function deployed successfully!" -ForegroundColor Green
        
        # Get the function URL
        Write-Host "Getting function details..." -ForegroundColor Yellow
        $functionApp = az functionapp show --resource-group $RESOURCE_GROUP --name $FUNCTION_APP_NAME --query "defaultHostName" -o tsv
        
        if ($functionApp) {
            $functionUrl = "https://$functionApp/api/cobol-parse"
            Write-Host "Function URL: $functionUrl" -ForegroundColor Cyan
            
            # Try to get the function key
            Write-Host "Getting function key..." -ForegroundColor Yellow
            try {
                $functionKeys = az functionapp function keys list --resource-group $RESOURCE_GROUP --name $FUNCTION_APP_NAME --function-name "cobol-parse" 2>$null | ConvertFrom-Json
                if ($functionKeys -and $functionKeys.default) {
                    $functionKey = $functionKeys.default
                    Write-Host "Function Key: $functionKey" -ForegroundColor Cyan
                    
                    $fullUrl = "$functionUrl" + "?code=$functionKey"
                    Write-Host "Complete URL with key: $fullUrl" -ForegroundColor Green
                    
                    # Save configuration for skillset update
                    $config = @{
                        functionAppName = $FUNCTION_APP_NAME
                        functionUrl = $functionUrl
                        functionKey = $functionKey
                        fullUrl = $fullUrl
                        resourceGroup = $RESOURCE_GROUP
                        deploymentTime = (Get-Date).ToString()
                    }
                    
                    $config | ConvertTo-Json | Out-File -FilePath "../function-deployment-config.json" -Encoding UTF8
                    Write-Host "✅ Configuration saved to function-deployment-config.json" -ForegroundColor Green
                } else {
                    Write-Host "⚠️  Could not retrieve function key automatically" -ForegroundColor Yellow
                    Write-Host "You can get it manually using: az functionapp function keys list --resource-group $RESOURCE_GROUP --name $FUNCTION_APP_NAME --function-name cobol-parse" -ForegroundColor White
                }
            } catch {
                Write-Host "⚠️  Could not retrieve function key: $($_.Exception.Message)" -ForegroundColor Yellow
            }
        }
        
        Write-Host ""
        Write-Host "Next steps:" -ForegroundColor Yellow
        Write-Host "1. Test the function endpoint" -ForegroundColor White
        Write-Host "2. Update your skillset with the function URL and key" -ForegroundColor White
        Write-Host "3. Create an indexer to process your COBOL data" -ForegroundColor White
        
    } else {
        Write-Host "❌ Deployment failed:" -ForegroundColor Red
        Write-Host $deployResult -ForegroundColor Red
    }
    
} catch {
    Write-Host "❌ Error during deployment: $($_.Exception.Message)" -ForegroundColor Red
} finally {
    Pop-Location
}

Write-Host ""
Write-Host "Deployment process complete." -ForegroundColor Green
