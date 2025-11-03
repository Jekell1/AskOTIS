# 🔑 OpenAI API Key Setup Guide

The COBOL RAG chatbot now requires an OpenAI API key for intelligent code analysis. Without it, the system will only use basic pattern matching.

## 🎯 Quick Setup (Recommended)

1. **Get your OpenAI API key**: https://platform.openai.com/api-keys
2. **Open the chatbot**: http://localhost:8503
3. **Enter your key** in the sidebar under "⚙️ AI Configuration"
4. **Click "💾 Save Key"**
5. **Start asking intelligent questions!**

## 🔄 Alternative Setup Methods

### Method 1: Environment Variable (PowerShell)
```powershell
# Set for current session
$env:OPENAI_API_KEY = "sk-your-actual-api-key-here"

# Then start the chatbot
python -m streamlit run simple_cobol_rag.py --server.port=8503
```

### Method 2: Create .env File
```bash
# Create .env file in the project directory
echo OPENAI_API_KEY=sk-your-actual-api-key-here > .env

# The chatbot will automatically load it
```

### Method 3: Windows Environment Variables
```powershell
# Set permanently for your user
[Environment]::SetEnvironmentVariable("OPENAI_API_KEY", "sk-your-actual-api-key-here", "User")

# Restart PowerShell and the chatbot
```

## 🧠 What Changes With API Key

### Without API Key (Pattern Mode)
- ⚠️ Shows warning: "Pattern Mode: Basic keyword matching only"
- Returns formatted search results
- Limited understanding of code context
- Still functional for basic searches

### With API Key (LLM Mode) 
- ✅ Shows: "LLM Mode: Intelligent code analysis enabled"
- Uses GPT-4 to analyze COBOL code
- Provides intelligent explanations
- Understands business logic and data flow
- Answers questions about program purpose and function

## 🔐 Security Notes

- Your API key is stored in session state only
- Not saved to disk or transmitted anywhere except OpenAI
- Key is masked in the UI (shows only first 7 and last 4 characters)
- You can change or remove the key at any time

## 💰 OpenAI Pricing

- GPT-4 costs approximately $0.03 per 1K tokens input, $0.06 per 1K tokens output
- Each analysis typically uses 500-2000 tokens
- Cost per query: roughly $0.03-$0.15
- For testing: $5-10 credit should provide many hours of usage

## 🧪 Test Questions

Once configured, try these to see the difference:

**Basic Questions:**
- "What does OVPAID.CBL do?"
- "How does customer lookup work?"
- "Explain the data flow in program ACUMEM"

**Advanced Questions:**
- "What business function does this code serve?"
- "How would I modify this program to handle new requirements?"
- "What are the dependencies between these programs?"

## 🚨 Troubleshooting

**API Key Not Working:**
- Ensure key starts with "sk-"
- Check OpenAI account has credits
- Verify key has appropriate permissions

**Still Seeing Pattern Mode:**
- Refresh the browser
- Check the sidebar shows "✅ OpenAI API key configured"
- Try entering the key again

**Connection Issues:**
- Verify internet connection
- Check OpenAI service status
- Try using a different API key

## 📋 Current Status Check

Open your chatbot and check the sidebar:
- ✅ Green check = LLM mode active
- ❌ Red X = Need to configure API key
- 🔍 Pattern mode = Fallback to basic matching

Your enhanced RAG system is ready! 🚀
