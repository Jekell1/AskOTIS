from __future__ import annotations
from typing import List, Dict, Any
from .config import get_settings
import openai, os, time, re

SYSTEM_PROMPT = """You are a professional COBOL system analyst assistant. Provide concise, accurate answers.
Format your answer with:
- Executive Summary (2 sentences)
- Detailed Explanation (bullet points or paragraphs)
- If data structures are relevant, include a markdown table.
Always cite sources at the end under a heading 'Sources'.
If unsure, state the uncertainty explicitly.
"""

ANSWER_PROMPT = """SYSTEM:
{system}

USER QUESTION:
{question}

RETRIEVED CONTEXT (JSON Lines):
{context}

INSTRUCTIONS:
1. Use only factual details from the context.
2. Aggregate related points; avoid repeating identical facts.
3. Provide program names verbatim (uppercase) when citing.
4. End with a Sources section listing each unique chunk id with its program (if any) as a markdown table with columns: Chunk ID | Program | Index | Score | Excerpt (truncated)
5. Do not hallucinate unverified relationships.
"""

PII_PATTERNS = [
    (r"\b\d{3}-\d{2}-\d{4}\b", "[REDACTED_SSN]"),
    (r"\b\d{16}\b", "[REDACTED_CARD]"),
]

def redact_pii(text: str) -> str:
    for pat, repl in PII_PATTERNS:
        text = re.sub(pat, repl, text)
    return text

def classify_query(q: str) -> str:
    ql = q.lower()
    if any(x in ql for x in ["password","credential","secret"]):
        return "SENSITIVE_REQUEST"
    return "NORMAL"

class AnswerLLM:
    def __init__(self):
        s = get_settings()
        if s.azure_openai_endpoint and s.azure_openai_deployment:
            openai.azure_endpoint = s.azure_openai_endpoint
            openai.api_type = "azure"
            openai.api_key = s.openai_api_key
            self._deployment = s.azure_openai_deployment
        else:
            openai.api_key = s.openai_api_key
            self._deployment = None
        self.temperature = s.temperature

    def _chat_completion(self, messages: List[Dict[str,str]]) -> str:
        # Simple retry loop
        for attempt in range(3):
            try:
                if self._deployment:
                    resp = openai.chat.completions.create(
                        model=self._deployment,
                        temperature=self.temperature,
                        messages=messages,
                        max_tokens=900
                    )
                else:
                    resp = openai.chat.completions.create(
                        model="gpt-4o-mini",
                        temperature=self.temperature,
                        messages=messages,
                        max_tokens=900
                    )
                return resp.choices[0].message.content
            except Exception as e:
                if attempt == 2:
                    raise
                time.sleep(1.5 * (attempt+1))
        return "(LLM failure)"

    def answer(self, question: str, context_chunks: List[Dict[str,Any]]) -> Dict[str,Any]:
        s = get_settings()
        classification = classify_query(question)
        clean_question = redact_pii(question) if s.pii_redact else question
        # Prepare context as JSONL for structured prompt
        context_jsonl = "\n".join([
            {
                'id': c['id'],
                'program': c.get('program_id'),
                'index': c.get('index'),
                'score': c.get('score'),
                'content': c['content'][:800]
            }. __repr__() for c in context_chunks
        ])
        prompt = ANSWER_PROMPT.format(system=SYSTEM_PROMPT, question=clean_question, context=context_jsonl)
        messages = [
            {"role":"system", "content": SYSTEM_PROMPT},
            {"role":"user", "content": prompt}
        ]
        output = self._chat_completion(messages)
        if s.pii_redact:
            output = redact_pii(output)
        return {"answer": output, "chunks": context_chunks, "classification": classification}
