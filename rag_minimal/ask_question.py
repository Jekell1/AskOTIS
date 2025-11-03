import os, argparse, logging, json, requests, sys, time
from difflib import SequenceMatcher
from openai import AzureOpenAI
from pathlib import Path
from typing import List, Any
try:
    from .embed import Embedder
    from .retriever import HybridRetriever, Passage
    from .prompt_builder import build_prompt
    from .chat_prompt_builder import build_chat_prompt
    from .router import classify
    from .expander import expand
    from .composer import compose_answer
except ImportError:
    # Allow running as a script: add parent directory to sys.path then retry absolute imports
    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(script_dir)
    if parent_dir not in sys.path:
        sys.path.insert(0, parent_dir)
    from rag_minimal.embed import Embedder  # type: ignore
    from rag_minimal.retriever import HybridRetriever, Passage  # type: ignore
    from rag_minimal.prompt_builder import build_prompt  # type: ignore
    from rag_minimal.chat_prompt_builder import build_chat_prompt  # type: ignore
    from rag_minimal.router import classify  # type: ignore
    from rag_minimal.expander import expand  # type: ignore
    from rag_minimal.composer import compose_answer  # type: ignore

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)

LOCAL_SETTINGS_FILE = 'local.settings.json'
_AUTOLOAD_KEYS = {
    # Search
    'AZURE_SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_ENDPOINT', 'SEARCH_KEY',
    # Azure OpenAI
    'AZURE_OPENAI_ENDPOINT', 'AZURE_OPENAI_KEY', 'AZURE_OPENAI_EMBED_DEPLOYMENT',
    'AZURE_OPENAI_EMBED_MODEL', 'AZURE_OPENAI_DEPLOYMENT', 'AZURE_OPENAI_CHAT_DEPLOYMENT',
    # OpenAI public
    'OPENAI_API_KEY', 'OPENAI_EMBED_MODEL', 'OPENAI_CHAT_MODEL'
}

def _autoload_local_settings(path: str = LOCAL_SETTINGS_FILE):
    """Load relevant credentials from local.settings.json into os.environ if missing.

    This avoids the need to export manually. We deliberately do NOT print values.
    """
    p = Path(path)
    if not p.exists():
        return
    try:
        data = json.loads(p.read_text(encoding='utf-8'))
        values = data.get('Values', {}) or {}
    except Exception as ex:
        logger.debug(f"Skipping local.settings.json autoload error: {ex}")
        return
    changed = 0
    for k, v in values.items():
        if k in _AUTOLOAD_KEYS and k not in os.environ and isinstance(v, str) and v.strip():
            os.environ[k] = v
            changed += 1
    # Normalization / aliases
    # Map SEARCH_* to AZURE_SEARCH_* if the latter missing
    if 'AZURE_SEARCH_ENDPOINT' not in os.environ and 'SEARCH_ENDPOINT' in os.environ:
        os.environ['AZURE_SEARCH_ENDPOINT'] = os.environ['SEARCH_ENDPOINT']
        changed += 1
    if 'AZURE_SEARCH_KEY' not in os.environ and 'SEARCH_KEY' in os.environ:
        os.environ['AZURE_SEARCH_KEY'] = os.environ['SEARCH_KEY']
        changed += 1
    # Chat deployment fallback
    if 'AZURE_OPENAI_CHAT_DEPLOYMENT' not in os.environ and 'AZURE_OPENAI_DEPLOYMENT' in os.environ:
        os.environ['AZURE_OPENAI_CHAT_DEPLOYMENT'] = os.environ['AZURE_OPENAI_DEPLOYMENT']
        changed += 1
    if changed:
        logger.info(f"Autoloaded {changed} credentials/settings from {path}")

def call_chat(prompt: str, *, azure_deployment: str | None = None, openai_model: str | None = None, azure_only: bool = False, api_version: str | None = None, stream: bool = False) -> str:
    # Attempt Azure OpenAI first (if endpoint/key present)
    aoai_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    aoai_key = os.getenv('AZURE_OPENAI_KEY')
    aoai_deploy = azure_deployment or os.getenv('AZURE_OPENAI_CHAT_DEPLOYMENT') or os.getenv('AZURE_OPENAI_DEPLOYMENT')
    azure_error: str | None = None
    if aoai_ep and aoai_key and aoai_deploy:
        try:
            # Resolve api-version precedence
            api_version_eff = api_version or os.getenv('AZURE_OPENAI_API_VERSION', '2024-12-01-preview')
            client = AzureOpenAI(
                api_key=aoai_key,
                api_version=api_version_eff,
                azure_endpoint=aoai_ep.rstrip('/')
            )
            if stream:
                collected: list[str] = []
                for event in client.chat.completions.create(
                    model=aoai_deploy,
                    messages=[{"role": "user", "content": prompt}],
                    temperature=0.1,
                    stream=True
                ):
                    if hasattr(event, 'choices') and event.choices:
                        delta = event.choices[0].delta  # type: ignore[attr-defined]
                        if delta and getattr(delta, 'content', None):
                            piece = delta.content
                            collected.append(piece)
                            sys.stdout.write(piece)
                            sys.stdout.flush()
                sys.stdout.write('\n')
                return ''.join(collected)
            else:
                resp = client.chat.completions.create(
                    model=aoai_deploy,
                    messages=[{"role": "user", "content": prompt}],
                    temperature=0.1
                )
                return resp.choices[0].message.content  # type: ignore
        except Exception as ex:  # Capture detailed failure; attempt categorization
            msg = str(ex)
            if '404' in msg or 'DeploymentNotFound' in msg:
                azure_error = ("Azure deployment not found (404). Provide a valid deployment via --azure-deployment or set "
                               f"AZURE_OPENAI_CHAT_DEPLOYMENT. Attempted: {aoai_deploy}")
            elif '401' in msg or 'Unauthorized' in msg:
                azure_error = "Azure authentication failed (401). Verify AZURE_OPENAI_KEY and endpoint access."
            else:
                azure_error = f"Azure Chat exception: {msg[:200]}"

    if azure_error and azure_only:
        raise RuntimeError(f"Azure chat failed and fallback disabled (--azure-only). Details: {azure_error}")

    # OpenAI fallback (only if API key exists and not forced Azure-only)
    oai_key = os.getenv('OPENAI_API_KEY')
    if oai_key:
        model = openai_model or os.getenv('OPENAI_CHAT_MODEL', 'gpt-4o-mini')
        url = 'https://api.openai.com/v1/chat/completions'
        headers = {"Authorization": f"Bearer {oai_key}", "Content-Type": "application/json"}
        body = {"model": model, "messages": [{"role": "user", "content": prompt}], "temperature": 0.1}
        resp = requests.post(url, json=body, headers=headers, timeout=120)
        if resp.status_code == 200:
            if azure_error:
                logger.info(f"Fell back to OpenAI after Azure failure: {azure_error}")
            return resp.json()['choices'][0]['message']['content']
        if resp.status_code == 401:
            raise RuntimeError(f"OpenAI authentication failed (401). Check OPENAI_API_KEY. (Prior Azure: {azure_error})")
        raise RuntimeError(f"OpenAI Chat error {resp.status_code}: {resp.text[:160]} (Prior Azure: {azure_error})")

    # Neither path succeeded
    if azure_error:
        raise RuntimeError(f"No successful chat completion. Azure failed: {azure_error}. Set a valid AZURE_OPENAI_CHAT_DEPLOYMENT or provide OPENAI_API_KEY.")
    raise RuntimeError("No chat provider configured. Set Azure (AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_CHAT_DEPLOYMENT) or OPENAI_API_KEY.")


def main():
    ap = argparse.ArgumentParser(description='Ask a question over hybrid RAG index.')
    ap.add_argument('--index', default='cobol-facts-v3l', help='Azure AI Search facts index name')
    ap.add_argument('--question', help='Single question (omit in --chat mode)')
    ap.add_argument('--program', help='Restrict to a specific COBOL program_id (e.g., APIPAY)')
    ap.add_argument('--top', type=int, default=12)
    ap.add_argument('--kv', type=int, default=15, help='Vector top K')
    ap.add_argument('--kt', type=int, default=15, help='BM25 top K')
    ap.add_argument('--azure-deployment', help='Override Azure chat deployment name (else env vars)')
    ap.add_argument('--openai-model', help='Override OpenAI chat model (else env var / default)')
    ap.add_argument('--azure-only', action='store_true', help='Fail if Azure chat fails; do not attempt OpenAI fallback')
    ap.add_argument('--api-version', help='Override Azure OpenAI API version (default 2024-12-01-preview)')
    ap.add_argument('--stream', action='store_true', help='Stream answer tokens as they arrive')
    ap.add_argument('--chat', action='store_true', help='Enter interactive multi-turn chat mode')
    ap.add_argument('--auto-plan', action='store_true', help='Classify intent & choose indexes automatically (router)')
    ap.add_argument('--history-file', help='Persist / load chat history JSON file')
    ap.add_argument('--max-history', type=int, default=10, help='Max history turns retained for prompt context')
    ap.add_argument('--debug-retrieval', action='store_true', help='Emit detailed retrieval fusion diagnostics and raw results')
    args = ap.parse_args()

    # Attempt credential autoload before any network operations
    _autoload_local_settings()

    if not args.chat and not args.question:
        ap.error('Either provide --question or use --chat for interactive mode.')

    embedder = Embedder()
    retriever = HybridRetriever(args.index)

    history = []
    if args.history_file and os.path.exists(args.history_file):
        try:
            history = json.loads(open(args.history_file,'r',encoding='utf-8').read())
            if not isinstance(history, list):
                history = []
        except Exception:
            history = []

    def persist_history():
        if args.history_file:
            try:
                with open(args.history_file,'w',encoding='utf-8') as f:
                    json.dump(history, f, indent=2)
            except Exception as ex:
                logger.warning(f"Failed to persist history: {ex}")

    def run_one(user_q: str, chat_mode: bool):
        user_q = (user_q or '').strip()
        if not user_q:
            return None
        plan = None
        if args.auto_plan:
            try:
                plan = classify(user_q)
            except Exception as ex:
                logger.warning(f"Router classify failed; falling back to single index. {ex}")
        vec = embedder.embed([user_q])[0]
        retrieval_debug: dict | None = None
        if plan and plan.intent != 'GENERAL_NL':
            # Guardrail: required entity presence for certain intents
            required_entity_map = {
                'DEFINITION_USAGE': 'symbol',
                'CALL_GRAPH': 'program',
                'PARAGRAPH_EXPLAIN': 'paragraph',
                'TABLE_IMPACT': 'table',
                'COPYBOOK_USAGE': 'copybook',
                'DATA_LINEAGE': 'entity'
            }
            need = required_entity_map.get(plan.intent)
            if need and need not in plan.primary_entities:
                refusal = f"Required {need} entity not identified; cannot proceed without hallucination risk."
                if not chat_mode:
                    print(refusal)
                    print('\n---\nJSON:\n' + json.dumps({
                        'answer': refusal,
                        'plan': plan.intent,
                        'entities': plan.primary_entities,
                        'notice': 'Refusal: missing primary entity',
                        'suggestions': [
                            'Rephrase specifying the exact symbol/program/table name',
                            'Ask: list symbols in PROGRAM X',
                            'Ask: what tables does PROGRAM X update'
                        ]
                    }, indent=2))
                else:
                    history.append({'role':'user','content':user_q})
                    history.append({'role':'assistant','content':refusal})
                    persist_history()
                return refusal
            # For now: if plan has program filter and user didn't pass --program, apply it
            prog = args.program or plan.filters.get('program_id')
            if args.debug_retrieval:
                passages_loc, retrieval_debug = retriever.search_debug(user_q, vec, top_k_vector=args.kv, top_k_text=args.kt, top_n=args.top, program=prog)
            else:
                passages_loc = retriever.search(user_q, vec, top_k_vector=args.kv, top_k_text=args.kt, top_n=args.top, program=prog)
        else:
            if args.debug_retrieval:
                passages_loc, retrieval_debug = retriever.search_debug(user_q, vec, top_k_vector=args.kv, top_k_text=args.kt, top_n=args.top, program=args.program)
            else:
                passages_loc = retriever.search(user_q, vec, top_k_vector=args.kv, top_k_text=args.kt, top_n=args.top, program=args.program)

        # Fallback enrichment for DEFINITION_USAGE when symbol not present in retrieved evidence
        missing_symbol_note_added = False
        variant_match_note: str | None = None
        if plan and plan.intent == 'DEFINITION_USAGE':
            symbol = plan.primary_entities.get('symbol')
            if symbol:
                symbol_present = any(symbol.lower() in (p.content or '').lower() for p in passages_loc)
                if not symbol_present:
                    # Simple REST search helper
                    def _simple_search(index: str, search_text: str, top: int = 5, filter_expr: str | None = None, select: list[str] | None = None):
                        ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
                        key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
                        if not (ep and key):
                            return []
                        api_version = '2024-05-01-preview'
                        url = f"{ep.rstrip('/')}/indexes/{index}/docs/search?api-version={api_version}"
                        headers = {"Content-Type": "application/json", "api-key": key}
                        body: dict[str, Any] = {"search": search_text, "top": top}
                        if filter_expr:
                            body['filter'] = filter_expr
                        if select:
                            body['select'] = ','.join(select)
                        try:
                            r = requests.post(url, json=body, headers=headers, timeout=20)
                            if r.status_code == 200:
                                return r.json().get('value', [])
                            logger.debug(f"_simple_search {index} status {r.status_code}: {r.text[:160]}")
                        except Exception:
                            pass
                        return []
                    # Fetch definition (symbol record) first pass plain search
                    sym_docs = _simple_search('cobol-symbols', symbol, top=5)
                    # If not found, attempt exact filter on name or qualified_name
                    if not sym_docs:
                        filt = f"name eq '{symbol}' or qualified_name eq '{symbol}'"
                        sym_docs = _simple_search('cobol-symbols', '*', top=5, filter_expr=filt)
                    # Variant attempts if still missing: hyphen insertion and prefix
                    if not sym_docs:
                        variants = set()
                        if len(symbol) > 4:
                            variants.add(symbol[:4] + '-' + symbol[4:])
                        # Add all splits after 3..len-3
                        for i in range(3, min(len(symbol)-2, 8)):
                            variants.add(symbol[:i] + '-' + symbol[i:])
                        # Substring base (first 4 chars) for broader search
                        variants.add(symbol[:4])
                        scored: list[tuple[float, dict]] = []
                        for v in variants:
                            cand_docs = _simple_search('cobol-symbols', v, top=3)
                            for cd in cand_docs:
                                nm = (cd.get('qualified_name') or cd.get('name') or '').replace('-', '').upper()
                                score_sim = SequenceMatcher(None, nm, symbol.upper()).ratio()
                                scored.append((score_sim, cd))
                        if scored:
                            scored.sort(key=lambda x: x[0], reverse=True)
                            # keep top 1 with similarity > 0.55
                            best = [d for s,d in scored if s >= 0.55][:1]
                            if best:
                                sym_docs = best
                    extra_passages: list[Passage] = []
                    if sym_docs:
                        sd = sym_docs[0]
                        found_name = (sd.get('qualified_name') or sd.get('name') or '').upper()
                        if found_name and found_name != symbol.upper():
                            variant_match_note = f"Approximate symbol match: requested '{symbol}' but closest indexed symbol is '{found_name}'."
                        def_txt_parts = [f"Symbol {sd.get('qualified_name') or sd.get('name')}"]
                        if sd.get('level') is not None:
                            def_txt_parts.append(f"level {sd.get('level')}")
                        if sd.get('pic'):
                            def_txt_parts.append(f"PIC {sd.get('pic')}")
                        if sd.get('usage'):
                            def_txt_parts.append(f"USAGE {sd.get('usage')}")
                        if sd.get('value'):
                            v = str(sd.get('value'))
                            if len(v) < 60:
                                def_txt_parts.append(f"VALUE {v}")
                        if sd.get('program_id'):
                            def_txt_parts.append(f"program {sd.get('program_id')}")
                        if sd.get('start_line') and sd.get('end_line'):
                            def_txt_parts.append(f"lines {sd.get('start_line')}-{sd.get('end_line')}")
                        def_content = ' '.join(def_txt_parts)
                        extra_passages.append(Passage(
                            id=str(sd.get('item_id') or sd.get('qualified_name') or symbol),
                            content=def_content,
                            score=0.05,
                            program_id=sd.get('program_id'),
                            source_path=sd.get('path'),
                            citation_tag=''  # will assign later
                        ))
                    # Fetch usages (xrefs)
                    xref_filter = None  # using search for now
                    xref_docs = _simple_search('cobol-xrefs', symbol, top=6)
                    for xd in xref_docs:
                        snippet = xd.get('snippet') or ''
                        usage_parts = ["XREF", xd.get('direction'), xd.get('kind'), f"line {xd.get('line')}"]
                        if xd.get('program_id'):
                            usage_parts.append(f"program {xd.get('program_id')}")
                        if snippet:
                            usage_parts.append(snippet[:160])
                        usage_content = ' '.join([str(p) for p in usage_parts if p])
                        extra_passages.append(Passage(
                            id=str(xd.get('xref_id') or f"xref-{len(extra_passages)+1}"),
                            content=usage_content,
                            score=0.03,
                            program_id=xd.get('program_id'),
                            source_path=xd.get('path'),
                            citation_tag=''  # assign later
                        ))
                    if extra_passages:
                        passages_loc.extend(extra_passages)
                        # Reassign citation tags sequentially
                        for idx, p in enumerate(passages_loc, 1):
                            p.citation_tag = f"P{idx}"
                        logger.info(f"Fallback enrichment added {len(extra_passages)} symbol/xref passages for {symbol}.")
                    else:
                        logger.info(f"Fallback enrichment attempted but no symbol/xref documents found for {symbol}.")
                        missing_symbol_note_added = True
                # If still not present after enrichment, guardrail: explicit not-found message (avoid hallucination)
                symbol_present_after = any(symbol.lower() in (p.content or '').lower() for p in passages_loc)
                if not symbol_present_after:
                    not_found_msg = f"Symbol '{symbol}' not found in indexed symbol or xref data; cannot provide definition/usage."
                    structured_nf = {
                        'answer': not_found_msg,
                        'plan': plan.intent,
                        'entities': plan.primary_entities,
                        'sources': [],
                        'notes': [f"Symbol {symbol} absent from retrieval results and enrichment lookup"],
                        'suggestions': [
                            f"Verify spelling / consider variants (e.g. hyphenated forms)",
                            "Ask: list symbols in PROGRAM <prog>",
                            "Broaden query or provide surrounding context"
                        ]
                    }
                    if retrieval_debug:
                        structured_nf['retrieval_debug'] = retrieval_debug
                    print(not_found_msg)
                    print('\n---\nJSON:\n' + json.dumps(structured_nf, indent=2))
                    return not_found_msg

        # If debugging retrieval, emit a concise summary before LLM call
        if retrieval_debug and not chat_mode:
            try:
                print("\n[Retrieval Debug Summary]")
                dist = retrieval_debug.get('program_distribution', {})
                if dist:
                    print('Program distribution:', ', '.join(f"{k}:{v}" for k,v in dist.items()))
                print('Fused top documents:')
                for entry in retrieval_debug.get('fusion', []):
                    print(f"  id={entry['id']} prog={entry.get('program_id')} vec_rank={entry['vec_rank']} txt_rank={entry['txt_rank']} fused={entry['fused_score']:.4f}")
                print('--- end retrieval debug ---\n')
            except Exception:
                pass
        # Assign citation tags even if empty (loop skipped if none)
        if chat_mode:
            prompt_loc = build_chat_prompt(user_q, passages_loc, history, max_history_turns=args.max_history)
        else:
            prompt_loc = build_prompt(user_q, passages_loc)
        # If no passages, short-circuit without LLM call to avoid bogus [P#]
        if not passages_loc:
            answer_loc = "Insufficient retrieved evidence."
            sources_loc: list[dict] = []
            if chat_mode:
                history.append({"role": "user", "content": user_q})
                history.append({"role": "assistant", "content": answer_loc})
                persist_history()
            else:
                if plan:
                    structured = {"answer": answer_loc, "sources": sources_loc, "plan": plan.intent, "entities": plan.primary_entities}
                    print(answer_loc)
                    print('\n---\nJSON:\n' + json.dumps(structured, indent=2))
                else:
                    print(answer_loc)
                    print('\n---\nJSON:\n' + json.dumps({"answer": answer_loc, "sources": sources_loc}, indent=2))
            return answer_loc
        answer_loc = call_chat(
            prompt_loc,
            azure_deployment=args.azure_deployment,
            openai_model=args.openai_model,
            azure_only=args.azure_only,
            api_version=args.api_version,
            stream=args.stream
        )
        sources_loc = [{"id": p.id, "tag": p.citation_tag, "path": p.source_path, "score": p.score} for p in passages_loc]
        if plan and not chat_mode:
            # Build structured answer skeleton
            expanded = expand(plan.intent, passages_loc, plan.primary_entities)
            structured = compose_answer(user_q, expanded)
            structured['plan'] = plan.intent
            structured['entities'] = plan.primary_entities
            if variant_match_note:
                structured.setdefault('notes', []).append(variant_match_note)
            if retrieval_debug:
                structured['retrieval_debug'] = retrieval_debug
            if missing_symbol_note_added:
                structured.setdefault('notes', []).append('Requested symbol not present in initial retrieval; enrichment attempted.')
        if chat_mode:
            history.append({"role": "user", "content": user_q})
            history.append({"role": "assistant", "content": answer_loc})
            if len(history) > args.max_history * 2:
                history[:] = history[-args.max_history*2:]
            persist_history()
        else:
            if plan:
                structured['answer_llm'] = answer_loc
                structured['sources'] = sources_loc
                print(answer_loc)
                print('\n---\nJSON:\n' + json.dumps(structured, indent=2))
            else:
                base_struct: dict[str, Any] = {"answer": answer_loc, "sources": sources_loc}
                if retrieval_debug:
                    base_struct['retrieval_debug'] = retrieval_debug
                print(answer_loc)
                print('\n---\nJSON:\n' + json.dumps(base_struct, indent=2))
        return answer_loc

    # Single-turn mode
    if not args.chat:
        run_one(args.question, False)
        return

    # Chat mode
    print('Entering multi-turn chat. Commands: /exit /history /save')
    if args.question:
        print('> ' + args.question)
        run_one(args.question, True)
    while True:
        try:
            user_in = input('You: ').strip()
        except (EOFError, KeyboardInterrupt):
            print('\nExiting chat.')
            break
        if user_in.lower() in ('/exit','/quit'):
            break
        if user_in.lower() == '/history':
            for h in history[-args.max_history*2:]:
                print(f"{h['role']}: {h['content'][:200]}")
            continue
        if user_in.lower() == '/save':
            persist_history(); print('History saved.'); continue
        start = time.time()
        ans = run_one(user_in, True)
        if ans and not args.stream:
            print(ans)
        if ans:
            print(f"[Latency: {time.time()-start:.2f}s]")
    persist_history()

if __name__ == '__main__':
    main()
