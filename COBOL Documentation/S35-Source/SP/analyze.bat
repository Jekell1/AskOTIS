@echo off
REM COBOL Copybook Analyzer - Batch Runner
REM Usage: analyze.bat <command> <parameters>

setlocal enabledelayedexpansion

if "%1"=="" (
    echo COBOL Copybook Analyzer
    echo =======================
    echo.
    echo Usage:
    echo   analyze.bat analyze ^<cobol-file^> [output-dir]
    echo   analyze.bat compare ^<original-file^> ^<inlined-file^> [output-dir]
    echo   analyze.bat extract ^<inlined-file^> [output-dir]
    echo   analyze.bat validate ^<cobol-file^>
    echo.
    echo Examples:
    echo   analyze.bat analyze APIPAY.CBL
    echo   analyze.bat compare APIPAY.CBL APIPAY_Inlined.CBL
    echo   analyze.bat extract APIPAY_Inlined.CBL copybooks
    echo   analyze.bat extract "c:\Users\johnathan.tillman\Desktop\OTIS Modernization\Copybook stitching\APIPAY_Inlined.CBL"
    echo.
    goto end
)

REM Check if .NET is installed
dotnet --version >nul 2>&1
if errorlevel 1 (
    echo Error: .NET 6.0 or later is required but not found.
    echo Please install .NET from https://dotnet.microsoft.com/download
    goto end
)

REM Build the project if executable doesn't exist
if not exist "bin\Debug\net6.0\CobolCopybookAnalyzer.exe" (
    echo Building COBOL Copybook Analyzer...
    dotnet build --configuration Debug
    if errorlevel 1 (
        echo Build failed!
        goto end
    )
)

REM Run the analyzer with all parameters
echo Running COBOL Copybook Analyzer...
echo Command: %*
echo.

dotnet run --project CobolCopybookAnalyzer.csproj -- %* --no-pause

:end
pause
