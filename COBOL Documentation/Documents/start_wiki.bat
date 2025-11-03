@echo off
REM COBOL Documentation Wiki Server Launcher
REM This batch file starts the Python server for the COBOL documentation wiki

echo.
echo ===============================================================
echo  COBOL Documentation Wiki Server Launcher
echo ===============================================================
echo.

REM Check if Python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo ❌ Error: Python is not installed or not in PATH
    echo.
    echo Please install Python 3.x from: https://www.python.org/downloads/
    echo Make sure to check "Add Python to PATH" during installation
    echo.
    pause
    exit /b 1
)

echo ✅ Python found
echo.

REM Change to the directory containing this batch file
cd /d "%~dp0"

REM Check if required files exist
if not exist "wiki_reader.html" (
    echo ❌ Error: wiki_reader.html not found in current directory
    echo    Current directory: %CD%
    echo.
    pause
    exit /b 1
)

if not exist "wiki_server.py" (
    echo ❌ Error: wiki_server.py not found in current directory
    echo    Current directory: %CD%
    echo.
    pause
    exit /b 1
)

echo ✅ Required files found
echo.

REM Start the server
echo 🚀 Starting COBOL Documentation Wiki Server...
echo.
python wiki_server.py

REM If we get here, server was stopped
echo.
echo Server stopped. Press any key to exit...
pause >nul
