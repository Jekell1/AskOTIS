from typing import List
from dataclasses import dataclass
from textwrap import dedent
from .retriever import Passage  # relative import for package context

BUSINESS_RULES = dedent("""
Grounding Rules:
- Use only the retrieved COBOL fact passages; do not infer behavior not present.
- When describing logic, reference paragraph or action roles only if present verbatim.
- Never invent identifiers or program names.
- Every factual statement must end with a citation [P#].
- If the answer cannot be derived, respond with: "Insufficient retrieved evidence." and cite the most relevant passage.
""")

def build_prompt(question: str, passages: List[Passage]) -> str:
    passage_blocks = []
    for p in passages:
        text = p.content.replace('\n', ' ')
        passage_blocks.append(f"[{p.citation_tag}] {text}")

    prompt = f"""System: You are a precise COBOL analysis assistant. Use ONLY the provided passages.
{BUSINESS_RULES}
Passages:
""" + "\n".join(passage_blocks) + f"""

Question: {question}
Instructions:
1. Provide a concise, well-structured answer.
2. Cite each supporting fact with [P#] immediately after the claim.
3. If evidence is insufficient, state that explicitly.
4. End with a Sources line listing citation tags once, comma-separated (order of first use).
Answer:
"""
    return prompt
