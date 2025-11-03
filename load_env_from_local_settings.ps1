# Load environment variables from local.settings.json
$settingsPath = ".\local.settings.json"

if (-not (Test-Path $settingsPath)) {
    Write-Host "❌ local.settings.json not found" -ForegroundColor Red
    exit 1
}

try {
    $settings = Get-Content $settingsPath | ConvertFrom-Json
    $values = $settings.Values
    
    # Set Azure OpenAI variables
    $env:AZURE_OPENAI_API_KEY = $values.AZURE_OPENAI_API_KEY
    $env:AZURE_OPENAI_ENDPOINT = $values.AZURE_OPENAI_ENDPOINT
    
    if ($values.AZURE_OPENAI_DEPLOYMENT) {
        $env:AZURE_OPENAI_DEPLOYMENT = $values.AZURE_OPENAI_DEPLOYMENT
    }
    
    Write-Host "✅ Loaded Azure OpenAI credentials from local.settings.json" -ForegroundColor Green
    Write-Host "   Endpoint: $($values.AZURE_OPENAI_ENDPOINT)" -ForegroundColor Gray
    
} catch {
    Write-Host "❌ Failed to load settings: $_" -ForegroundColor Red
    exit 1
}
