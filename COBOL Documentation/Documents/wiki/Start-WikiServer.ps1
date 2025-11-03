# COBOL Documentation Wiki Server - PowerShell Version
# This script starts a local web server for the wiki to avoid browser security restrictions

param(
    [int]$Port = 8080,
    [switch]$NoBrowser
)

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  COBOL Documentation Wiki Server" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check if Python is available
try {
    $pythonVersion = python --version 2>&1
    Write-Host "🐍 Python detected: $pythonVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ Python is not installed or not in PATH" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please install Python from: https://python.org" -ForegroundColor Yellow
    Write-Host "Make sure to check 'Add Python to PATH' during installation" -ForegroundColor Yellow
    Write-Host ""
    Read-Host "Press Enter to exit"
    exit 1
}

# Check if required files exist
$requiredFiles = @(
    "wiki_reader.html",
    "APIPAY_Documentation.html", 
    "LONPFB_Documentation.html"
)

$missingFiles = @()
foreach ($file in $requiredFiles) {
    if (!(Test-Path $file)) {
        $missingFiles += $file
    }
}

if ($missingFiles.Count -gt 0) {
    Write-Host "⚠️  Warning: Some files are missing:" -ForegroundColor Yellow
    foreach ($file in $missingFiles) {
        Write-Host "   - $file" -ForegroundColor Yellow
    }
    Write-Host ""
}

Write-Host "🚀 Starting server..." -ForegroundColor Green
Write-Host ""

# Start the Python server
try {
    if ($NoBrowser) {
        python start_wiki_server.py --no-browser
    } else {
        python start_wiki_server.py
    }
} catch {
    Write-Host "❌ Failed to start server: $($_.Exception.Message)" -ForegroundColor Red
    Read-Host "Press Enter to exit"
    exit 1
}

Write-Host ""
Write-Host "👋 Server stopped." -ForegroundColor Yellow
Read-Host "Press Enter to exit"
