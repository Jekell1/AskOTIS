# Test the deployed Azure Function with menu text query

$body = @{
    question = "What is the text of user input choices for the main menu?"
} | ConvertTo-Json

Write-Host "Testing deployed RAG system..." -ForegroundColor Cyan
Write-Host "Query: What is the text of user input choices for the main menu?" -ForegroundColor Yellow
Write-Host ""

try {
    $response = Invoke-RestMethod -Uri "https://func-otis-rag.azurewebsites.net/api/query" `
        -Method POST `
        -Body $body `
        -ContentType "application/json"
    
    Write-Host "=== ANSWER ===" -ForegroundColor Green
    Write-Host $response.answer
    Write-Host ""
    
    Write-Host "=== SOURCES USED ===" -ForegroundColor Green
    Write-Host "Question Type:" $response.question_type -ForegroundColor Cyan
    Write-Host "Indexes Used:" ($response.indexes_used -join ', ') -ForegroundColor Cyan
    Write-Host ""
    
    $sourceCount = $response.sources.Count
    Write-Host "Retrieved $sourceCount sources:" -ForegroundColor Cyan
    
    $screenNodeSources = $response.sources | Where-Object { $_.source -like "*screen_nodes*" }
    if ($screenNodeSources.Count -gt 0) {
        Write-Host "SUCCESS: Found" $screenNodeSources.Count "screen_nodes sources!" -ForegroundColor Green
        Write-Host ""
        Write-Host "Screen node details:" -ForegroundColor Yellow
        foreach ($source in $screenNodeSources) {
            Write-Host "  -" $source.source -ForegroundColor Gray
            $maxLen = [Math]::Min(150, $source.content.Length)
            $preview = $source.content.Substring(0, $maxLen)
            Write-Host "    $preview..." -ForegroundColor DarkGray
        }
    } else {
        Write-Host "WARNING: No screen_nodes sources found" -ForegroundColor Yellow
        Write-Host "Sources found:" -ForegroundColor Gray
        $response.sources | ForEach-Object { Write-Host "  -" $_.source -ForegroundColor Gray }
    }
    
} catch {
    Write-Host "ERROR:" $_.Exception.Message -ForegroundColor Red
}
