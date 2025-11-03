@echo off
REM COBOL Documentation Regenerator
REM This script regenerates the static HTML documentation from current files

echo.
echo ====================================================
echo COBOL Documentation Regenerator
echo ====================================================
echo.

REM Change to the Documents directory
cd /d "%~dp0"

echo Current directory: %CD%
echo.

REM Check if Python is available
python --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Python is not installed or not in PATH
    echo Please install Python and ensure it's available in your system PATH
    pause
    exit /b 1
)

REM Check if markdown module is available
python -c "import markdown" >nul 2>&1
if %errorlevel% neq 0 (
    echo Installing required Python markdown module...
    pip install markdown
    if %errorlevel% neq 0 (
        echo ERROR: Failed to install markdown module
        echo Please run: pip install markdown
        pause
        exit /b 1
    )
)

echo Regenerating documentation...
echo.

REM Run the generator script
python generate_static_html.py

if %errorlevel% equ 0 (
    echo.
    echo ====================================================
    echo Documentation regenerated successfully!
    echo ====================================================
    echo.
    
    REM Ask if user wants to view the documentation
    set /p choice="Would you like to open the documentation now? (y/N): "
    if /i "%choice%"=="y" (
        echo Opening documentation...
        start index.html
    )
) else (
    echo.
    echo ERROR: Documentation generation failed!
    echo Please check the error messages above.
)

echo.
pause
