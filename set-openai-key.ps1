# PowerShell Commands to Set OpenAI API Key
# Run these commands in PowerShell before starting the chatbot

# Method 1: Set for current session only
$env:OPENAI_API_KEY = "your_openai_api_key_here"

# Method 2: Set permanently for your user account
[Environment]::SetEnvironmentVariable("OPENAI_API_KEY", "your_openai_api_key_here", "User")

# Method 3: Launch chatbot with environment variable
$env:OPENAI_API_KEY = "your_openai_api_key_here"; python -m streamlit run simple_cobol_rag.py --server.port=8503

# Instructions:
# 1. Replace "your_openai_api_key_here" with your actual OpenAI API key
# 2. Run one of the commands above
# 3. Start or restart the chatbot
