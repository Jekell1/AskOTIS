#!/usr/bin/env python3
"""
COBOL RAG Chatbot Launcher
==========================
Easy launcher script for the COBOL RAG chatbot with automatic setup.
"""

import os
import sys
import subprocess
import json
from pathlib import Path

def check_requirements():
    """Check if required packages are installed"""
    required_packages = [
        'streamlit',
        'openai', 
        'requests',
        'python-dotenv'
    ]
    
    missing_packages = []
    
    for package in required_packages:
        try:
            __import__(package.replace('-', '_'))
        except ImportError:
            missing_packages.append(package)
    
    if missing_packages:
        print("❌ Missing required packages:")
        for pkg in missing_packages:
            print(f"   - {pkg}")
        print(f"\n📦 Install with: pip install {' '.join(missing_packages)}")
        return False
    
    print("✅ All required packages are installed!")
    return True

def setup_environment():
    """Set up environment file if it doesn't exist"""
    env_file = Path('.env')
    template_file = Path('.env.template')
    
    if not env_file.exists() and template_file.exists():
        print("🔧 Creating .env file from template...")
        env_content = template_file.read_text()
        env_file.write_text(env_content)
        print("✅ Created .env file - please update with your API keys!")
        return True
    elif env_file.exists():
        print("✅ Environment file exists")
        return True
    else:
        print("⚠️ No environment template found")
        return False

def check_api_connectivity():
    """Test connection to Azure Search API"""
    try:
        import requests
        
        url = "https://az-use1-ai-search.search.windows.net/indexes/cobol-index/docs/search"
        headers = {
            'Content-Type': 'application/json',
            'api-key': 'ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef'
        }
        params = {"api-version": "2024-07-01"}
        
        # Test query
        response = requests.post(
            url, 
            headers=headers, 
            params=params,
            json={"search": "*", "top": 1},
            timeout=10
        )
        
        if response.status_code == 200:
            data = response.json()
            doc_count = data.get('@odata.count', 0)
            print(f"✅ Azure Search API connected - {doc_count:,} documents available")
            return True
        else:
            print(f"❌ Azure Search API error: HTTP {response.status_code}")
            return False
            
    except Exception as e:
        print(f"❌ API connectivity check failed: {e}")
        return False

def launch_chatbot(mode='openai'):
    """Launch the appropriate chatbot version"""
    
    if mode == 'openai':
        script = 'cobol_rag_openai.py'
        print("🚀 Launching Enhanced COBOL RAG Chatbot with OpenAI...")
    else:
        script = 'cobol_rag_chatbot.py'
        print("🚀 Launching Basic COBOL RAG Chatbot...")
    
    if not Path(script).exists():
        print(f"❌ Script {script} not found!")
        return False
    
    try:
        # Launch Streamlit
        cmd = ['streamlit', 'run', script, '--server.port=8501', '--server.address=localhost']
        print(f"📍 Starting server at: http://localhost:8501")
        print("🔧 Use Ctrl+C to stop the server")
        print("=" * 50)
        
        subprocess.run(cmd)
        
    except KeyboardInterrupt:
        print("\n🛑 Server stopped by user")
    except Exception as e:
        print(f"❌ Error launching chatbot: {e}")
        return False
        
    return True

def main():
    """Main launcher function"""
    print("🔍 COBOL RAG Chatbot Launcher")
    print("=" * 40)
    
    # Check requirements
    if not check_requirements():
        sys.exit(1)
    
    # Setup environment
    setup_environment()
    
    # Check API connectivity  
    api_ok = check_api_connectivity()
    
    if not api_ok:
        print("⚠️ API connectivity issues detected, but you can still run the chatbot")
        
    # Choose version
    print("\n🎯 Choose chatbot version:")
    print("1. Enhanced (with OpenAI GPT) - Recommended")
    print("2. Basic (without OpenAI)")
    
    choice = input("\nEnter choice (1 or 2, default=1): ").strip()
    
    if choice == '2':
        launch_chatbot('basic')
    else:
        print("\n💡 For best results, set your OpenAI API key in the sidebar!")
        launch_chatbot('openai')

if __name__ == "__main__":
    main()
