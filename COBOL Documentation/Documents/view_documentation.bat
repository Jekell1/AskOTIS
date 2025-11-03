@echo off
REM COBOL Documentation Viewer
REM Opens the static HTML documentation in the default browser

echo.
echo ===============================================================
echo  COBOL Documentation Static HTML Viewer
echo ===============================================================
echo.

REM Change to the directory containing this batch file
cd /d "%~dp0"

REM Check if index.html exists
if not exist "index.html" (
    echo ❌ Error: index.html not found in current directory
    echo    Current directory: %CD%
    echo.
    echo 💡 Run generate_static_html.py first to create the HTML files
    echo.
    pause
    exit /b 1
)

echo ✅ Documentation found
echo 🌐 Opening COBOL Documentation in your default browser...
echo.

REM Open the documentation in the default browser
start "" "index.html"

echo ✅ Documentation opened successfully!
echo.
echo 💡 Tips:
echo    • No server required - this is static HTML
echo    • Works completely offline
echo    • All diagrams and content are embedded
echo    • Share the entire Documents folder to share documentation
echo.
echo Press any key to exit...
pause >nul
