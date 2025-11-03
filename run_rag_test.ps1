# Run the 100-question RAG test suite with OpenAI grading
# This will take approximately 15-20 minutes to complete

Write-Host "🚀 OTIS RAG System - 100 Question Test Suite" -ForegroundColor Cyan
Write-Host "=" * 80

# Load environment variables from local.settings.json
Write-Host "`n📁 Loading credentials from local.settings.json..." -ForegroundColor Yellow
. .\load_env_from_local_settings.ps1

if (-not $?) {
    exit 1
}

Write-Host "`n📋 Test Details:" -ForegroundColor Cyan
Write-Host "   - 100 questions across 7 categories"
Write-Host "   - Automated grading using OpenAI (A-F scale)"
Write-Host "   - Comprehensive performance metrics"
Write-Host "   - Estimated time: 15-20 minutes"
Write-Host "`n⏳ Starting test..." -ForegroundColor Yellow
Write-Host ""

# Run the test
python test_rag_100_questions.py

Write-Host "`n✅ Test complete! Check the JSON file for detailed results." -ForegroundColor Green
