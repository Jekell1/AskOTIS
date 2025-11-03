# Test the deployed Azure Function - detailed JSON output

$body = @{
    question = "What is the text of user input choices for the main menu?"
} | ConvertTo-Json

Write-Host "Testing deployed RAG system..." -ForegroundColor Cyan
Write-Host ""

try {
    $response = Invoke-RestMethod -Uri "https://func-otis-rag.azurewebsites.net/api/query" `
        -Method POST `
        -Body $body `
        -ContentType "application/json"
    
    Write-Host "=== FULL RESPONSE ===" -ForegroundColor Yellow
    $response | ConvertTo-Json -Depth 10 | Write-Host
    
} catch {
    Write-Host "ERROR:" $_.Exception.Message -ForegroundColor Red
    Write-Host "Response:" $_.ErrorDetails.Message -ForegroundColor Gray
}
