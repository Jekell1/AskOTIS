# Deploy to Azure Static Web Apps using REST API
# This script packages and deploys your HTML file

$DeploymentToken = "efa2c0241885ec51c38418fefb948ba7162afbb68d2cc7489cd918bd88eb741103-bff85414-4a46-4abe-81be-68a877ed7e2700f1506022e4a70f"
$AppName = "AskOTIS"
$DeploymentApiUrl = "https://ambitious-ground-022e4a70f.3.azurestaticapps.net"

# Create a temporary directory for deployment
$TempDir = New-Item -ItemType Directory -Path "$env:TEMP\swa-deploy-$(Get-Random)" -Force
Write-Host "Created temp directory: $TempDir"

try {
    # Copy the HTML file to temp directory as index.html
    Copy-Item "otis-rag-chat.html" -Destination "$TempDir\index.html"
    Write-Host "Copied otis-rag-chat.html to index.html"

    # Create a zip file
    $ZipPath = "$env:TEMP\swa-deploy-$(Get-Random).zip"
    Compress-Archive -Path "$TempDir\*" -DestinationPath $ZipPath -Force
    Write-Host "Created deployment package: $ZipPath"

    # Use Azure CLI to deploy (simpler than REST API)
    Write-Host "`nAttempting deployment using Azure REST API..."
    
    # Note: Azure Static Web Apps doesn't have a direct REST API for file upload
    # The recommended approach is via GitHub Actions or SWA CLI
    
    Write-Host "`n=== MANUAL STEPS REQUIRED ==="
    Write-Host "1. Your deployment token is: $DeploymentToken"
    Write-Host "2. Your app URL is: $DeploymentApiUrl"
    Write-Host "3. Deployment package created at: $ZipPath"
    Write-Host "`nSince automated deployment is not available without SWA CLI,"
    Write-Host "please use one of these options:"
    Write-Host "  - Set up GitHub Actions integration in Azure Portal"
    Write-Host "  - Install SWA CLI manually: npm install -g @azure/static-web-apps-cli"
    Write-Host "  - Use Azure Portal to connect to a GitHub repository"
    
} finally {
    # Cleanup
    if (Test-Path $TempDir) {
        Remove-Item $TempDir -Recurse -Force
    }
}
