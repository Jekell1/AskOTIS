"""
View timing breakdown in Azure Function logs.

To see detailed timing:
1. Go to Azure Portal: https://portal.azure.com
2. Navigate to: func-otis-rag → Monitoring → Log stream
3. Look for lines with ⏱️ TIMING markers

Expected log output for a menu query:
==========================================
⏱️ TIMING: Routing took 0.XXXs
⏱️ TIMING: Embedding generation (3072-dim) took X.XXXs
⏱️ TIMING: Embedding generation (1536-dim) took X.XXXs
⏱️ TIMING: Search screen_nodes took X.XXXs (Y results)
⏱️ TIMING: Search transactions took X.XXXs (Y results)
⏱️ TIMING: Retrieval took X.XXXs
⏱️ TIMING: Memory context took 0.XXXs
⏱️ Generating response with max_tokens=2000, question_type=menu
⏱️ TIMING: LLM call took XX.XXXs (tokens: YYYY)
⏱️ TIMING: Generation took XX.XXXs
⏱️ TIMING SUMMARY: Total=23.XXXs (Route=X.XXXs, Retrieve=X.XXXs, Memory=0.XXXs, Generate=XX.XXXs)

This will tell you which part is slow:
- If LLM call is 20+ seconds → Azure OpenAI is slow
- If Retrieval is 15+ seconds → Azure Search or embeddings are slow
- If Generation is 20+ seconds but LLM call is fast → Prompt building is slow
==========================================

Recent test showed: 23.79 seconds total (down from 56s!)
The max_tokens optimization IS working.

To see logs now, run this command in PowerShell:
    func azure functionapp logstream func-otis-rag
"""

print(__doc__)
