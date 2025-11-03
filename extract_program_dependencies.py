import os, json, argparse, logging
from pathlib import Path
from typing import Dict, Set
import requests

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)

LOCAL_SETTINGS = 'local.settings.json'

def autoload():
    p = Path(LOCAL_SETTINGS)
    if not p.exists():
        return
    try:
        data = json.loads(p.read_text(encoding='utf-8'))
        vals = data.get('Values', {}) or {}
        for k in ['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
        # normalize
        if 'AZURE_SEARCH_ENDPOINT' not in os.environ and 'SEARCH_ENDPOINT' in os.environ:
            os.environ['AZURE_SEARCH_ENDPOINT'] = os.environ['SEARCH_ENDPOINT']
        if 'AZURE_SEARCH_KEY' not in os.environ and 'SEARCH_KEY' in os.environ:
            os.environ['AZURE_SEARCH_KEY'] = os.environ['SEARCH_KEY']
    except Exception as ex:
        logger.warning(f"Failed autoload settings: {ex}")

def rest_query(index: str, body: Dict):
    ep = os.getenv('AZURE_SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY')
    if not (ep and key):
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    url = f"{ep.rstrip('/')}/indexes/{index}/docs/search?api-version=2024-05-01-preview"
    headers = {"Content-Type": "application/json", "api-key": key}
    r = requests.post(url, headers=headers, json=body, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Search {index} {r.status_code}: {r.text[:180]}")
    return r.json().get('value', [])

def gather(program: str):
    autoload()
    program = program.upper()
    # Index assumptions
    flow_index = 'cobol-flow-edges-v2'
    copybook_index = 'cobol-copybooks'
    symbols_index = 'cobol-symbols'

    callers: Set[str] = set()
    callees: Set[str] = set()
    copybooks: Set[str] = set()
    includes: Set[str] = set()
    externals: Set[str] = set()

    # Flow edges: gather where caller == program or callee == program
    # New v2 flow edges use caller_program_id / target_program_id and paragraph-level fields.
    # We'll treat target_program_id (if present) as an inter-program call; otherwise raw_target likely paragraph.
    for role, filt in [
        ('out', f"caller_program_id eq '{program}'"),
        ('in',  f"target_program_id eq '{program}'")
    ]:
        docs = rest_query(flow_index, {"search": "*", "filter": filt, "top": 2000})
        for d in docs:
            caller_prog = (d.get('caller_program_id') or '').upper()
            target_prog = (d.get('target_program_id') or '').upper()
            raw_target = (d.get('raw_target') or '').upper()
            if caller_prog == program and target_prog:
                # program-to-program call
                if target_prog != program:
                    callees.add(target_prog)
            if target_prog == program and caller_prog:
                if caller_prog != program:
                    callers.add(caller_prog)
            # Potential copybook/include not present in v2 edge docs; keep placeholders
            # If raw_target looks like a program (e.g., all alnum and length <= 8) but no target_program_id, skip for now
            # Future enhancement: heuristics.

    # Copybooks index: copybooks referencing program name in path or maybe program_id eq
    try:
        cb_docs = rest_query(copybook_index, {"search": program, "top": 200})
        for d in cb_docs:
            name = d.get('copybook_name') or d.get('name')
            if name:
                copybooks.add(name.upper())
    except Exception as ex:
        logger.info(f"Copybook scan skipped: {ex}")

    # Symbols index: look for external references named like program
    try:
        sym_docs = rest_query(symbols_index, {"search": program, "top": 200})
        for d in sym_docs:
            if d.get('symbol_type') == 'CALL':
                target = d.get('target_program') or d.get('value')
                if target:
                    callees.add(target.upper())
    except Exception as ex:
        logger.info(f"Symbols scan skipped: {ex}")

    # Remove self references
    callees.discard(program)
    callers.discard(program)

    return {
        'program': program,
        'calls': sorted(callees),
        'called_by': sorted(callers),
        'copybooks': sorted(copybooks),
        'includes': sorted(includes),
        'externals': sorted(externals)
    }

def main():
    ap = argparse.ArgumentParser(description='Extract COBOL program dependency graph slice')
    ap.add_argument('--program', required=True, help='Program name (e.g. LONPF2)')
    ap.add_argument('--json', action='store_true', help='Emit JSON only')
    args = ap.parse_args()
    data = gather(args.program)
    if args.json:
        print(json.dumps(data, indent=2))
        return
    print(f"Program: {data['program']}")
    print(f"Calls ({len(data['calls'])}): {', '.join(data['calls']) or '-'}")
    print(f"Called By ({len(data['called_by'])}): {', '.join(data['called_by']) or '-'}")
    print(f"Copybooks ({len(data['copybooks'])}): {', '.join(data['copybooks']) or '-'}")
    print(f"Includes ({len(data['includes'])}): {', '.join(data['includes']) or '-'}")
    print(f"Externals ({len(data['externals'])}): {', '.join(data['externals']) or '-'}")
    print('\nJSON:\n' + json.dumps(data, indent=2))

if __name__ == '__main__':
    main()
