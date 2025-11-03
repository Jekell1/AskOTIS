from typing import List, Sequence, Dict, Any
from textwrap import dedent
from .retriever import Passage

CHAT_RULES = dedent("""
Grounding Rules:
- Use ONLY the provided passages for factual claims; do not hallucinate unseen logic.
- Every factual sentence must end with its citation tag [P#].
- If no passages are provided, reply exactly: "Insufficient retrieved evidence." (no citations).
- Preserve program identifiers and paragraph names exactly as shown.
- If user asks follow-up pronoun references ("it", "that paragraph"), resolve using recent history.
"""
)

def _format_passages(passages: Sequence[Passage]) -> str:
    out = []
    for p in passages:
        text = p.content.replace('\n',' ')
        out.append(f"[{p.citation_tag}] {text}")
    return "\n".join(out) if out else "(none)"

def _format_history(turns: List[Dict[str,str]], max_pairs: int) -> str:
    if not turns:
        return "(no prior turns)"
    # keep last max_pairs*2 entries
    subset = turns[-max_pairs*2:]
    lines = []
    for t in subset:
        role = t.get('role','user')
        content = t.get('content','').replace('\n',' ')
        lines.append(f"{role}: {content}")
    return "\n".join(lines)

def build_chat_prompt(question: str, passages: Sequence[Passage], history: List[Dict[str,str]], *, max_history_turns: int = 10) -> str:
    passages_block = _format_passages(passages)
    history_block = _format_history(history, max_history_turns)
    prompt = f"""System: You are a precise COBOL analysis assistant operating in multi-turn chat mode.
{CHAT_RULES}

Conversation History (most recent first):
{history_block}

Passages:
{passages_block}

User Question: {question}
Instructions:
1. If passages == (none) respond exactly: Insufficient retrieved evidence.
2. Otherwise answer succinctly, grouping related logic.
3. Every factual sentence ends with citation tag.
4. End with a Sources line listing citation tags once, comma-separated.
Answer:
"""
    return prompt
from typing import List, Sequence
from textwrap import dedent
from .retriever import Passage

CHAT_RULES = dedent("""
Conversation & Grounding Rules:
- Use ONLY provided passages plus prior user/assistant turns shown in the conversation summary.
- Never fabricate COBOL identifiers or logic not evidenced in passages.
- Each factual statement referencing code must end with a citation [P#].
- If the user asks outside available evidence, transparently state insufficiency and suggest a clarifying follow-up.
"""
)

def build_chat_prompt(question: str, passages: List[Passage], history: Sequence[dict], max_history_turns: int = 6) -> str:
    # Compact recent history (user & assistant messages only)
    trimmed = history[-max_history_turns:]
    hist_lines = []
    for turn in trimmed:
        role = turn.get('role')
        content = (turn.get('content') or '').replace('\n', ' ')
        if role in ('user','assistant'):
            hist_lines.append(f"{role.capitalize()}: {content}")
    passage_blocks = []
    for p in passages:
        txt = p.content.replace('\n', ' ')
        passage_blocks.append(f"[{p.citation_tag}] {txt}")
    convo_summary = "\n".join(hist_lines) if hist_lines else "(No prior turns)"
    prompt = f"""System: You are a precise multi-turn COBOL analysis assistant.\n{CHAT_RULES}\n\nConversation Context:\n{convo_summary}\n\nPassages:\n""" + "\n".join(passage_blocks) + f"""\n\nCurrent User Question: {question}\nInstructions:\n1. Ground every claim in passages.\n2. Maintain continuity with prior turns if relevant.\n3. Use concise paragraphs, no bullet lists unless user asked.\n4. End with Sources: list of citation tags used once.\nAnswer:\n"""
    return prompt