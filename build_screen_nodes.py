"""Extract SCREEN SECTION artifacts into new_cobol_screen_nodes.

Approach (Phase 1 heuristic):
  1. Fetch COBOL source chunks (or program text) from existing index (e.g., new_code_chunks or file storage) selecting those that contain 'SCREEN SECTION'.
  2. For each program, locate SCREEN SECTION span by regex: /SCREEN\s+SECTION\./ until next division / PROCEDURE DIVISION / EOF.
  3. Parse field lines heuristically:
     - Pattern: level-number + name + PIC clause (optional) + OCCURS etc.
     - Capture: name, level, pic/raw, line offset relative to section start.
  4. Detect actions: lines with keywords like 'ACCEPT', 'DISPLAY', 'ENTER', 'F1', 'PF', 'PF\d+', 'FUNCTION KEY'
  5. Derive transitions: scan lines for PERFORM / CALL within section referencing other programs or paragraphs (coarse; may be empty in many systems).
  6. Construct summary_text: f"Screen {screen_name}: {field_count} fields; actions: {top_actions}".
  7. Upload documents (mergeOrUpload).

Later Enhancements:
  * Exact parser integration if structured parse available.
  * Add vector embedding field (screen_summary_vector) with semantic clustering.
  * Guard / dependency linkage to ui_paths sequences.
"""
from __future__ import annotations
import os, sys, re, json, argparse, time, datetime, requests, hashlib
from typing import List, Dict, Any

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
SCREEN_INDEX='new_cobol_screen_nodes'
CHUNK_INDEX='new_code_chunks'
CHUNK_SEARCH_API_VERSION=os.getenv('CODE_CHUNK_SEARCH_API_VERSION','2024-07-01')  # align with chunk ingestion script

FIELD_RE=re.compile(r'^(?P<lvl>\d{2})\s+([A-Z0-9-]+)\s+(?:PIC\s+[^.]+\.)',re.IGNORECASE)
SCREEN_START_RE=re.compile(r'SCREEN\s+SECTION\.',re.IGNORECASE)
DIVISION_RE=re.compile(r'PROCEDURE\s+DIVISION|DATA\s+DIVISION',re.IGNORECASE)
ACTION_TOKENS=['ACCEPT','DISPLAY','ENTER','PF','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12']
TRANSITION_TOKENS=['CALL','PERFORM']


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_chunks(ep,key,top=1000,phrase='SCREEN SECTION'):
    """Fetch code chunks containing the supplied phrase.

    Uses the correct field list from `new_code_chunks` (text, program_id, chunk_id).
    Falls back gracefully if the search errors (returns empty list).
    
    If top=0, fetches ALL matching chunks (no limit).
    """
    headers={'api-key':key,'Content-Type':'application/json'}
    url=f"{ep}/indexes/{CHUNK_INDEX}/docs/search?api-version={CHUNK_SEARCH_API_VERSION}"
    skip=0
    out=[]
    fetch_all = (top == 0)
    # phrase search: wrap in quotes if not already
    search_term=phrase if phrase.startswith('"') else f'"{phrase}"'
    payload={'search':search_term,'top':1000,'queryType':'simple','searchMode':'any','select':'chunk_id,program_id,text'}
    while True:
        payload['skip']=skip
        r=requests.post(url,headers=headers,json=payload)
        if r.status_code!=200:
            print('Fetch fail',r.status_code,r.text[:300])
            break
        batch=r.json().get('value',[])
        out.extend(batch)
        if len(batch)<payload['top']:
            break  # No more results
        if not fetch_all and len(out)>=top:
            break  # Reached limit
        skip+=payload['top']
    if not out:
        print('[INFO] No chunks matched phrase search; consider relaxing phrase or verifying index content.')
    return out if fetch_all else out[:top]


def extract_screen_sections(text:str)->List[str]:
    sections=[]
    for m in SCREEN_START_RE.finditer(text):
        start=m.end()
        remainder=text[start:]
        # find end
        end_match=DIVISION_RE.search(remainder)
        end=len(remainder) if not end_match else end_match.start()
        section=remainder[:end]
        sections.append(section)
    return sections


def parse_fields(section:str)->List[Dict[str,Any]]:
    """Parse screen fields, capturing VALUE clauses if present."""
    fields=[]
    for i,line in enumerate(section.splitlines()):
        line_strip=line.strip()
        m=FIELD_RE.match(line_strip)
        if m:
            name=line_strip.split()[1]
            # Extract VALUE clause if present
            value_clause = None
            value_match = re.search(r"VALUE\s+['\"]([^'\"]+)['\"]", line_strip, re.IGNORECASE)
            if value_match:
                value_clause = value_match.group(1)
            fields.append({'name':name,'line':i+1,'raw':line_strip,'value':value_clause})
    return fields


def extract_display_literals(section:str)->List[str]:
    """Extract literal text from DISPLAY statements."""
    literals=[]
    for line in section.splitlines():
        # Match DISPLAY 'text' or DISPLAY "text"
        display_matches = re.findall(r"DISPLAY\s+['\"]([^'\"]+)['\"]", line, re.IGNORECASE)
        literals.extend(display_matches)
    return literals


def extract_label_literals(section:str)->List[Dict[str,str]]:
    """Extract literal text from LABEL statements in SCREEN SECTION.
    
    LABEL statements contain menu text and other UI labels:
    Example: 03  LABEL LINE 05 COL 02 "1. ADVERTISING / SOLICITATION" LOW.
    
    COBOL format can have text on separate lines with lots of whitespace:
    03  LABEL LINE 12 COL 11
     "3. INQUIRIES"
                                                  LOW.
    
    Returns list of dicts with 'line', 'col', and 'text' keys.
    """
    labels=[]
    # Pattern matches LABEL LINE/COL followed by quoted text (possibly on next line)
    # Allow any amount of whitespace/newlines between COL and the quote
    label_pattern = re.compile(
        r'LABEL\s+LINE\s+(\d+)\s+COL\s+(\d+)\s*\n?\s*["\']([^"\']+)["\']',
        re.IGNORECASE | re.MULTILINE | re.DOTALL
    )
    for match in label_pattern.finditer(section):
        labels.append({
            'line': match.group(1),
            'col': match.group(2),
            'text': match.group(3).strip()
        })
    return labels


def detect_actions(section:str)->List[str]:
    acts=set()
    up=section.upper()
    for tok in ACTION_TOKENS:
        if tok in up:
            acts.add(tok)
    return sorted(acts)


def detect_transitions(section:str)->List[str]:
    trans=set()
    # naive: look for CALL PROGRAM-NAME or PERFORM PARA-NAME
    for line in section.splitlines():
        u=line.upper()
        if any(t in u for t in TRANSITION_TOKENS):
            # extract candidate tokens following CALL/PERFORM
            parts=re.split(r'[^A-Z0-9-]+',u)
            for i,p in enumerate(parts):
                if p in ('CALL','PERFORM') and i+1<len(parts):
                    nxt=parts[i+1]
                    if len(nxt)>=3 and nxt not in ('USING','THRU','THROUGH'):
                        trans.add(nxt)
    return sorted(trans)


def upload(ep,key,docs:List[Dict[str,Any]]):
    if not docs:
        print('No documents to upload.')
        return
    
    # Pre-upload visibility check for LPMENU screens
    lpmenu_screens = [d for d in docs if 'MASTER' in d.get('summary_text', '').upper() or 'DAILY PROCESSING' in d.get('summary_text', '')]
    if lpmenu_screens:
        print(f"[INFO] LPMENU screens prepared: {len(lpmenu_screens)}")
        for d in lpmenu_screens[:3]:
            print(f"   - {d.get('screen_id')} from program {d.get('program_id')[:16]}...")
    
    actions=[]
    url=f"{ep}/indexes/{SCREEN_INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    for d in docs:
        d['@search.action']='mergeOrUpload'
        actions.append(d)
    
    total_uploaded = 0
    total_failed = 0
    
    for i in range(0,len(actions),500):
        batch = actions[i:i+500]
        r=requests.post(url,headers=headers,json={'value':batch})
        
        if r.status_code == 207:
            # Partial success - some docs failed
            result = r.json()
            failed = [item for item in result.get('value', []) if not item.get('status')]
            total_failed += len(failed)
            total_uploaded += len(batch) - len(failed)
            print(f"⚠️  Partial success: {len(failed)} failed uploads out of {len(batch)} in this batch")
            for f in failed[:3]:
                print(f"   • Key: {f.get('key', 'N/A')} – Error: {f.get('errorMessage', 'Unknown')}")
        elif r.status_code in (200, 201):
            total_uploaded += len(batch)
        else:
            print(f'❌ Upload failed: {r.status_code} {r.text[:300]}')
            total_failed += len(batch)
            break
    
    if total_failed > 0:
        print(f'[WARNING] Uploaded {total_uploaded} screen nodes, {total_failed} failed')
    else:
        print(f'[SUCCESS] Successfully uploaded {total_uploaded} screen nodes')


def main():
    ap=argparse.ArgumentParser(description='Build screen nodes from SCREEN SECTION heuristics')
    ap.add_argument('--limit-chunks',type=int,default=1000,help='Max chunks to fetch (0 = all)')
    ap.add_argument('--phrase',default='SCREEN SECTION',help='Phrase to search for in code chunks')
    ap.add_argument('--include-copybooks',action='store_true',help='Also search for LABEL statements (copybooks)')
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    chunks=fetch_chunks(ep,key,top=args.limit_chunks,phrase=args.phrase)
    
    # Also fetch copybook chunks if requested
    if args.include_copybooks:
        copybook_chunks=fetch_chunks(ep,key,top=args.limit_chunks,phrase='LABEL LINE')
        print(f'Fetched copybook chunks: {len(copybook_chunks)} phrase=LABEL LINE')
        
        # Also fetch specific important menu copybooks by name (may not rank high in LABEL LINE search)
        important_copybooks = ['LPMENU_SCN.CPY', 'PGMENU_SCR1.CPY', 'FDMENU_SCN.CPY', 'ASMENU_SCN.CPY']
        for copybook_name in important_copybooks:
            specific_chunks = fetch_chunks(ep, key, top=50, phrase=copybook_name)
            if specific_chunks:
                print(f'  + Fetched {len(specific_chunks)} chunks for {copybook_name}')
                copybook_chunks.extend(specific_chunks)
        
        # Merge and deduplicate
        existing_ids = {ch['chunk_id'] for ch in chunks}
        for ch in copybook_chunks:
            if ch['chunk_id'] not in existing_ids:
                chunks.append(ch)
    
    print('Fetched candidate chunks:',len(chunks),'phrase=',args.phrase)
    
    # Group chunks by program_id to aggregate copybook screens that span multiple chunks
    from collections import defaultdict
    chunks_by_program = defaultdict(list)
    for ch in chunks:
        prog = (ch.get('program_id') or '').replace('\\', '/').upper()
        if prog:
            chunks_by_program[prog].append(ch)
    
    print(f'Grouped into {len(chunks_by_program)} programs')
    
    docs=[]
    ts=datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    per_program_counter: Dict[str,int] = {}
    seen_section_hash: set[str] = set()
    
    for prog, program_chunks in chunks_by_program.items():
        # Sort chunks by start_line to maintain order
        program_chunks.sort(key=lambda x: x.get('start_line', 0))
        
        # Aggregate content from all chunks for this program
        combined_content = '\n'.join(ch.get('text', '') for ch in program_chunks)
        
        # Check if content has SCREEN SECTION or LABEL LINE (copybooks)
        has_screen_section = 'SCREEN SECTION' in combined_content.upper()
        has_label_line = 'LABEL LINE' in combined_content.upper()
        
        if not has_screen_section and not has_label_line:
            continue
        
        # Extract sections
        if has_screen_section:
            sections=extract_screen_sections(combined_content)
        else:
            # Copybook: treat entire combined content as one section
            sections=[combined_content]
        
        if not sections:
            continue
        for sec in sections:
            h=hashlib.sha1(sec.encode('utf-8','ignore')).hexdigest()
            # Normalize key to prevent path separator causing false duplicates
            dup_key=f"{prog.lower()}:{h}"
            if dup_key in seen_section_hash:
                print(f"[SKIP] Duplicate section: {prog[:32]}... hash={h[:12]}...")
                continue  # skip duplicate section (overlapping chunks)
            seen_section_hash.add(dup_key)
            fields=parse_fields(sec)
            display_literals=extract_display_literals(sec)
            label_literals=extract_label_literals(sec)
            actions=detect_actions(sec)
            transitions=detect_transitions(sec)
            
            # Extract VALUE clauses from fields
            value_clauses = [f['value'] for f in fields if f.get('value')]
            
            per_program_counter[prog]=per_program_counter.get(prog,0)+1
            seq=per_program_counter[prog]
            screen_name=f"{prog}_SCREEN_{seq}"
            
            # Build rich, natural-language summary for better semantic search
            summary_parts = []
            
            # ALL screens get enhanced format for consistent semantic search
            summary_parts.append(f"Screen {screen_name} represents a COBOL user interface screen.")
            
            # Add user-facing description of menu options/labels
            label_texts = [lbl.get('text', '') for lbl in label_literals if lbl.get('text')] if label_literals else []
            display_texts = display_literals if display_literals else []
            value_texts = value_clauses if value_clauses else []
            
            # Combine all UI text elements
            all_ui_texts = label_texts + display_texts + value_texts
            
            # Filter out common non-actionable items (titles and prompts)
            filtered_ui_texts = []
            for text in all_ui_texts:
                text_upper = text.upper()
                text_stripped = text.strip()
                
                # Skip generic prompts like "ENTER SELECTION"
                if 'ENTER SELECTION' in text_upper:
                    continue
                
                # Skip menu titles (end with "MENU" or "MAINTENANCE" and don't start with menu numbers)
                # Menu choices start with numbers like "1.", "10.", "F6", etc.
                if (text_upper.endswith('MENU') or 'MAINTENANCE MENU' in text_upper) and not any(text_stripped.startswith(prefix) for prefix in ['1.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '10.', '11.', '12.', '13.', '14.', '15.', '16.', '17.', '18.', '19.', '20.', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12']):
                    continue
                
                filtered_ui_texts.append(text)
            
            # Sort menu items by their leading number/letter
            def sort_key(text):
                """Extract sort key from menu items like '1.', '10.', 'F6', etc."""
                import re
                text_stripped = text.strip()
                # Try to match leading number (e.g., "1.", "10.", "18.")
                match = re.match(r'^(\d+)\.', text_stripped)
                if match:
                    return (0, int(match.group(1)))  # Numeric items come first, sorted by number
                # Try to match F-key (e.g., "F6", "F7")
                match = re.match(r'^F(\d+)', text_stripped, re.IGNORECASE)
                if match:
                    return (1, int(match.group(1)))  # F-keys come after numbers, sorted by F-number
                # Everything else comes last, sorted alphabetically
                return (2, text_stripped.lower())
            
            filtered_ui_texts.sort(key=sort_key)
            
            if filtered_ui_texts:
                summary_parts.append("Users see the following menu options and interface elements on this screen:")
                # Use the original numbering from the menu items (don't add sequential numbers)
                # Increased limit to 50 to capture all menu items (some screens have 20+ items)
                for text in filtered_ui_texts[:50]:
                    summary_parts.append(f"  {text}")
                summary_parts.append("These are the text prompts, menu selections, and interface elements displayed to users when this screen appears.")
            else:
                summary_parts.append("This screen provides a user interface for COBOL application interaction.")
                summary_parts.append("The screen may contain data entry fields, navigation options, or system interface elements.")
            
            # Technical metadata (kept for hybrid search)
            tech_parts = [
                f"fields={len(fields)}",
                f"actions={len(actions)}",
                f"transitions={len(transitions)}"
            ]
            if value_clauses:
                tech_parts.append(f"value_clauses={len(value_clauses)}")
            if display_literals:
                tech_parts.append(f"display_literals={len(display_literals)}")
            if label_literals:
                tech_parts.append(f"label_literals={len(label_literals)}")
            
            summary_parts.append("Technical details: " + "; ".join(tech_parts))
            
            summary = "\n".join(summary_parts)
            # Azure Search document keys are safest when limited to alphanumerics/underscore/dash.
            # Remove colon and upper-case hex already fine. Use underscore separator.
            screen_id=f"{prog}_{seq}"
            doc={
                'screen_id': screen_id,
                'program_id': prog,
                'screen_name': screen_name,
                'field_count': len(fields),
                'action_count': len(actions),
                'transition_count': len(transitions),
                'fields_json': json.dumps(fields)[:32000],
                'actions_json': json.dumps(actions)[:12000],
                'transitions_json': json.dumps(transitions)[:12000],
                'display_literals_json': json.dumps(display_literals)[:32000],
                'value_clauses_json': json.dumps(value_clauses)[:32000],
                'label_literals_json': json.dumps(label_literals)[:32000],
                'raw_span_text': sec[:6000],
                'summary_text': summary,
                'generated_at': ts
            }
            docs.append(doc)
    if not docs:
        print('No screen sections discovered. (Consider verifying chunk index or phrase).')
        return
    if args.dry_run:
        print('Dry run only. Would upload',len(docs))
        print(json.dumps(docs[:2],indent=2)[:800])
        return
    upload(ep,key,docs)

if __name__=='__main__':
    main()
