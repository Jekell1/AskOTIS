# COBOL RAG Chatbot with Memory
# Requirements for open-source RAG implementation

import json
import asyncio
import logging
from typing import List, Dict, Any, Optional
from datetime import datetime
import os
from dataclasses import dataclass
from pathlib import Path

# Core dependencies to install
REQUIRED_PACKAGES = [
    "langchain>=0.1.0",
    "langchain-community>=0.0.10", 
    "langchain-openai>=0.0.5",
    "chromadb>=0.4.0",
    "sentence-transformers>=2.2.2",
    "tiktoken>=0.5.0",
    "streamlit>=1.28.0",
    "python-dotenv>=1.0.0",
    "requests>=2.31.0",
    "pydantic>=2.0.0"
]

print("📦 Required packages for COBOL RAG Chatbot:")
for pkg in REQUIRED_PACKAGES:
    print(f"  - {pkg}")
    
print("\n🚀 Install with: pip install " + " ".join(REQUIRED_PACKAGES))
