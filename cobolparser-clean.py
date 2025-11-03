#!/usr/bin/env python3
"""
COBOL → JSONL extractor (Azure + Local)

Features (unified in both modes):
- Parses COBOL text (fixed/free), keeps exact line numbers
- Extracts: PROGRAM-ID, sections, paragraphs
- Extracts data items (level, PIC, USAGE, OCCURS, REDEFINES, VALUE) with qualified names
- Extracts procedure facts: PERFORM (incl. THRU), CALL (static/dynamic), MOVE, COMPUTE, READ ...
- Tags every fact with its containing paragraph
- Builds xrefs (read/write/param_in/param_out) with file path, program id, start/end columns
- Expands PERFORM THRU into flow edges (caller → every paragraph in range)
- Detects COPY statements
- Chunks for retrieval: paragraph chunks + level-01 data-group chunks
- Emits JSONL files under <prefix>/JSONL/ (Azure) or <dir>/JSONL/ (local):
  files.jsonl, paragraphs.jsonl, data_items.jsonl, procedure_facts.jsonl,
  xrefs.jsonl, calls.jsonl, chunks.jsonl, flow_edges.jsonl, copybooks.jsonl

Usage
-----
Local:
  python cobolparser-clean.py --local /path/to/folder

Azure Blob:
  export AZURE_STORAGE_CONNECTION_STRING="<connection string>"
  python cobolparser-clean.py --container aisearch --prefix S35-Source/
  # Outputs: aisearch/S35-Source/JSONL/*.jsonl

Options:
  --max-files N      Process at most N files (0 = no limit)

Notes
-----
- Writes outputs to <prefix>/JSONL/ in Azure (created if missing) or <dir>/JSONL locally.
- Safe to rerun; JSONL files are overwritten.
- Azure mode skips directories and files under /JSONL/ folders (case-insensitive).
- Only processes files with extensions: .cbl, .cob, .cobol, .cpy (case-insensitive).
"""
from __future__ import annotations
import argparse
import json
import os
import re
import sys
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple

# Optional Azure import only when needed
try:
    from azure.storage.blob import BlobServiceClient
except Exception:  # pragma: no cover
    BlobServiceClient = None  # type: ignore

# -------------------------------
# Dataclasses
# -------------------------------

@dataclass
class FileRec:
    file_id: str
    path: str
    program_id: Optional[str]
    fmt: str  # fixed | free
    procedure_using: Optional[List[str]] = None
    copybook: bool = False

@dataclass
class ParagraphRec:
    file_id: str
    name: str
    kind: str  # section | paragraph
    start_line: int
    end_line: int

@dataclass
class DataItemRec:
    file_id: str
    name: str
    qualified_name: str
    level: int
    section: str  # WORKING | LOCAL | LINKAGE | OTHER
    pic: Optional[str]
    usage: Optional[str]
    occurs_low: Optional[int]
    occurs_high: Optional[int]
    depends_on: Optional[str]
    redefines: Optional[str]
    renames: Optional[str]  # For level 66 items
    value: Optional[str]
    start_line: int
    end_line: int

@dataclass
class ProcFactRec:
    file_id: str
    kind: str  # perform | call | move | compute | read | accept
    line: int
    snippet: str
    para: Optional[str] = None
    thru: Optional[str] = None
    callee: Optional[str] = None
    callee_data_name: Optional[str] = None
    is_dynamic: Optional[bool] = None
    using_raw: Optional[str] = None
    target: Optional[str] = None
    source_raw: Optional[str] = None
    expr_raw: Optional[str] = None

@dataclass
class XrefRec:
    file_id: str
    qualified_name: str
    simple_name: str
    kind: str  # read | write | param
    line: int
    snippet: str
    direction: Optional[str] = None  # read | write | param_in | param_out
    path: Optional[str] = None
    program_id: Optional[str] = None
    start_col: Optional[int] = None  # 1-based
    end_col: Optional[int] = None    # 1-based inclusive

@dataclass
class ChunkRec:
    chunk_id: str
    file_id: str
    path: str
    program_id: Optional[str]
    scope: str  # paragraph | data-group
    name: str
    start_line: int
    end_line: int
    text: str

@dataclass
class FlowEdgeRec:
    file_id: str
    caller_para: str
    target_para: str
    line: int
    kind: str  # perform

@dataclass
class CopybookRec:
    file_id: str
    parent_path: str
    copybook_name: str
    line: int
    replacing_clause: Optional[str] = None

# -------------------------------
# Constants & Regexes
# -------------------------------

COBOL_EXTS = {".cbl", ".cob", ".cobol", ".cpy", ".CBL", ".COB", ".CPY"}

# Streaming JSONL flush size for memory management
CHUNK_FLUSH_SIZE = 20000  # Standard flush threshold expected by tests

_ALLOWED_KEY_CHARS = set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-=")

def sanitize_id(val: str) -> str:
    if not isinstance(val, str):
        val = str(val)
    return ''.join(c if c in _ALLOWED_KEY_CHARS else '-' for c in val)

RE_PROGRAM_ID = re.compile(r"^\s*PROGRAM-ID\.\s+([A-Z0-9\-]+)", re.IGNORECASE)
# Suspect file detection patterns (more flexible)
RE_PROGRAM_ID_DETECT = re.compile(r"^\s*PROGRAM-ID\s*[.:]", re.IGNORECASE)
RE_PROCEDURE_DIV_DETECT = re.compile(r"^\s*PROCEDURE\s+DIVISION\b", re.IGNORECASE)
RE_DATA_DIV = re.compile(r"^\s*DATA\s+DIVISION\.", re.IGNORECASE)
RE_PROC_DIV = re.compile(r"^\s*PROCEDURE\s+DIVISION(?:\s+USING\b.*)?\.", re.IGNORECASE)
RE_PROC_DIV_USING = re.compile(r"^\s*PROCEDURE\s+DIVISION\s+USING\s+(.*?)\.", re.IGNORECASE)
RE_STORAGE_SECTION = re.compile(r"^\s*(WORKING-STORAGE|LOCAL-STORAGE|LINKAGE)\s+SECTION\.", re.IGNORECASE)
RE_SECTION = re.compile(r"^\s*([A-Z0-9\-]+)-SECTION\.", re.IGNORECASE)
RE_PARAGRAPH = re.compile(r"^\s*([A-Z0-9\-]+)\.$", re.IGNORECASE)
RE_DATA_ITEM = re.compile(
    r"^\s*(01|02|03|04|05|06|07|08|09|10|66|77|88)\s+([A-Z0-9\-]+)"  # level + name (added 66, 88)
    r"(?:\s+REDEFINES\s+([A-Z0-9\-]+))?"
    r"(?:\s+RENAMES\s+([A-Z0-9\-]+(?:\s+THRU\s+[A-Z0-9\-]+)?))?"  # 66 RENAMES clause
    r"(?:\s+OCCURS\s+(\d+)(?:\s+TO\s+(\d+))?\s+TIMES(?:\s+DEPENDING\s+ON\s+([A-Z0-9\-]+))?)?"
    r"(?:\s+PIC\s+([A-Z0-9VXS\(\)\./\-\+]+))?"
    r"(?:\s+USAGE\s+([A-Z0-9\-]+))?"
    r"(?:\s+VALUE\s+(.+?))?\s*\.\s*$",
    re.IGNORECASE,
)
RE_PERFORM = re.compile(r"^\s*PERFORM\s+([A-Z0-9\-]+)(?:\s+THRU\s+([A-Z0-9\-]+))?\s*\.\s*$", re.IGNORECASE)
RE_CALL_LINE = re.compile(r"\bCALL\b", re.IGNORECASE)
RE_CALL_STATIC = re.compile(r"\bCALL\s+['\"]([^'\"]+)['\"]", re.IGNORECASE)
RE_CALL_DYNAMIC = re.compile(r"\bCALL\s+([A-Za-z][A-Za-z0-9\-]*)", re.IGNORECASE)
RE_MOVE = re.compile(r"^\s*MOVE\s+(.+?)\s+TO\s+([A-Z0-9\-]+)\s*\.\s*$", re.IGNORECASE)
RE_COMPUTE = re.compile(r"^\s*COMPUTE\s+([A-Z0-9\-]+)\s*=\s*(.+?)\s*\.\s*$", re.IGNORECASE)
RE_READ_INTO = re.compile(r"^\s*READ\s+([A-Z0-9\-]+)(?:\s+INTO\s+([A-Z0-9\-]+))?", re.IGNORECASE)
RE_ACCEPT = re.compile(r"^\s*ACCEPT\s+([A-Z0-9\-]+)", re.IGNORECASE)
RE_COPY = re.compile(r"^\s*COPY\s+(?:['\"]([^'\"]+)['\"]|([A-Z0-9\-]+))(?:\s+REPLACING\s+(.*?))?\s*\.\s*$", re.IGNORECASE)

COMMENT_COL7 = {"*", "/"}
CONT_MARK = "-"

# -------------------------------
# General helpers
# -------------------------------

def _canon_section(s: str) -> str:
    s = s.upper()
    if s == "WORKING-STORAGE":
        return "WORKING"
    if s == "LOCAL-STORAGE":
        return "LOCAL"
    return s  # LINKAGE or other

def detect_format(lines: List[str]) -> str:
    fixed_like = 0
    for ln in lines[:200]:
        if len(ln) >= 7:
            col7 = ln[6]
            if ln[:6].strip() and (col7 in COMMENT_COL7 or col7 == " " or col7 == CONT_MARK):
                fixed_like += 1
    return "fixed" if fixed_like > 20 else "free"

def strip_seq_and_cols(line: str, fmt: str) -> str:
    if fmt == "fixed":
        if len(line) >= 7:
            return line[6:72]
        return line
    return line

def is_comment_fixed(line: str) -> bool:
    return len(line) >= 7 and line[6] in COMMENT_COL7

def normalize_cobol(text: str) -> Tuple[List[str], List[str]]:
    raw_lines = text.splitlines()
    fmt = detect_format(raw_lines)
    norm_lines: List[str] = []
    for ln in raw_lines:
        if fmt == "fixed" and is_comment_fixed(ln):
            norm_lines.append("")
            continue
        norm = strip_seq_and_cols(ln, fmt).rstrip("\n\r")
        norm_lines.append(norm)
    return raw_lines, norm_lines

def split_statements(norm_lines: List[str]) -> List[Tuple[int, str]]:
    stmts: List[Tuple[int, str]] = []
    buf: List[str] = []
    for idx, line in enumerate(norm_lines, start=1):
        buf.append(line)
        if line.strip().endswith('.'):
            stmt = " ".join(x.rstrip() for x in buf).strip()
            stmts.append((idx, stmt))
            buf = []
    if buf:
        stmts.append((len(norm_lines), " ".join(x.rstrip() for x in buf).strip()))
    return stmts

def line_to_para_map(paras: List[ParagraphRec]) -> Dict[int, str]:
    m: Dict[int, str] = {}
    for p in paras:
        if p.kind != "paragraph":
            continue
        for ln in range(p.start_line, p.end_line + 1):
            m[ln] = p.name
    return m

def compute_column_positions(simple_name: str, snippet: str) -> Tuple[Optional[int], Optional[int]]:
    if not simple_name or not snippet:
        return None, None
    
    # First try standard word boundaries
    pat = r"\b" + re.escape(simple_name) + r"\b"
    m = re.search(pat, snippet, flags=re.IGNORECASE)
    if m:
        return m.start() + 1, m.end()
    
    # Fallback for COBOL identifiers with hyphens - use lookahead/lookbehind for non-alphanumeric-hyphen chars
    escaped_name = re.escape(simple_name)
    pat_cobol = r'(?<![A-Z0-9-])' + escaped_name + r'(?![A-Z0-9-])'
    m = re.search(pat_cobol, snippet, flags=re.IGNORECASE)
    if m:
        return m.start() + 1, m.end()
    
    return None, None

def parse_call(stmt: str) -> Tuple[Optional[str], Optional[str], Optional[bool]]:
    # static
    m = RE_CALL_STATIC.search(stmt)
    if m:
        return m.group(1).upper(), None, False
    # dynamic
    m = RE_CALL_DYNAMIC.search(stmt)
    if m:
        return None, m.group(1).upper(), True
    return None, None, None

def process_file_content(path_or_name: str, text: str, file_id: str) -> Tuple:
    """Process COBOL file content and return structured data"""
    fr, paras, items, facts, xrefs, chunks, edges, copies, raw = process_text(path_or_name, file_id, text)
    return fr, paras, items, facts, xrefs, chunks, edges, copies, raw

# -------------------------------
# Core processing (shared by local & azure)
# -------------------------------

def process_text(path: str, file_id: str, text: str) -> Tuple[FileRec, List[ParagraphRec], List[DataItemRec], List[ProcFactRec], List[XrefRec], List[ChunkRec], List[FlowEdgeRec], List[CopybookRec], List[str]]:
    raw_lines, norm_lines = normalize_cobol(text)
    fmt = detect_format(raw_lines)

    # Program ID and USING, plus division markers
    program_id: Optional[str] = None
    procedure_using: Optional[List[str]] = None
    data_div_start: Optional[int] = None
    proc_div_start: Optional[int] = None

    for i, line in enumerate(norm_lines, start=1):
        if program_id is None:
            m = RE_PROGRAM_ID.search(line)
            if m:
                program_id = m.group(1).upper()
        if data_div_start is None and RE_DATA_DIV.search(line):
            data_div_start = i
        if proc_div_start is None and RE_PROC_DIV.search(line):
            proc_div_start = i
            if procedure_using is None:
                um = RE_PROC_DIV_USING.search(line)
                if um:
                    using_raw = um.group(1)
                    procedure_using = [t.upper() for t in re.findall(r"[A-Z0-9\-]+", using_raw, re.IGNORECASE)] or None

    # Detect copybooks: .CPY extension or missing both PROGRAM-ID and PROCEDURE DIVISION
    is_copybook = (
        path.upper().endswith('.CPY') or 
        (program_id is None and proc_div_start is None)
    )
    
    if is_copybook:
        # For copybooks, program_id should be None (not fallback to filename)
        program_id = None
    elif not program_id:
        # fallback to file stem for regular programs
        program_id = os.path.splitext(os.path.basename(path))[0].upper()

    file_rec = FileRec(file_id=file_id, path=path, program_id=program_id, fmt=fmt, procedure_using=procedure_using, copybook=is_copybook)

    # Sections & paragraphs
    paragraphs: List[ParagraphRec] = []
    section_name = None
    section_start = None
    paragraph_stack: List[Tuple[str, int]] = []

    for i, line in enumerate(norm_lines, start=1):
        sm = RE_SECTION.match(line)
        if sm:
            if section_name is not None and section_start is not None:
                paragraphs.append(ParagraphRec(file_id, section_name, "section", section_start, i - 1))
            section_name = sm.group(1).upper()
            section_start = i
            if paragraph_stack:
                name, start = paragraph_stack.pop()
                paragraphs.append(ParagraphRec(file_id, name, "paragraph", start, i - 1))
            continue
        pm = RE_PARAGRAPH.match(line)
        if pm:
            if paragraph_stack:
                name, start = paragraph_stack.pop()
                paragraphs.append(ParagraphRec(file_id, name, "paragraph", start, i - 1))
            paragraph_stack.append((pm.group(1).upper(), i))

    if paragraph_stack:
        name, start = paragraph_stack.pop()
        paragraphs.append(ParagraphRec(file_id, name, "paragraph", start, len(norm_lines)))
    if section_name is not None and section_start is not None:
        paragraphs.append(ParagraphRec(file_id, section_name, "section", section_start, len(norm_lines)))

    line2para = line_to_para_map(paragraphs)

    # Data items
    data_items: List[DataItemRec] = []
    current_section = "OTHER"
    parents_by_level: Dict[int, str] = {}

    for i, line in enumerate(norm_lines, start=1):
        if RE_STORAGE_SECTION.match(line):
            current_section = _canon_section(RE_STORAGE_SECTION.match(line).group(1))
            parents_by_level = {}
            continue
        dm = RE_DATA_ITEM.match(line)
        if dm:
            level = int(dm.group(1))
            name = dm.group(2).upper()
            redefines = dm.group(3).upper() if dm.group(3) else None
            renames = dm.group(4) if dm.group(4) else None  # 66 RENAMES clause
            occurs_low = int(dm.group(5)) if dm.group(5) else None
            occurs_high = int(dm.group(6)) if dm.group(6) else None
            depends_on = dm.group(7).upper() if dm.group(7) else None
            pic = dm.group(8)
            usage = dm.group(9)
            value = dm.group(10)

            # parent chain by level
            for lvl in list(parents_by_level.keys()):
                if lvl >= level:
                    del parents_by_level[lvl]
            parent_q = parents_by_level.get(max(parents_by_level.keys(), default=0))
            qualified = f"{parent_q}.{name}" if parent_q else name
            parents_by_level[level] = qualified

            data_items.append(DataItemRec(
                file_id=file_id,
                name=name,
                qualified_name=qualified,
                level=level,
                section=current_section,
                pic=pic,
                usage=usage,
                occurs_low=occurs_low,
                occurs_high=occurs_high,
                depends_on=depends_on,
                redefines=redefines,
                renames=renames,
                value=value,
                start_line=i,
                end_line=i,
            ))

    # Quick maps
    qmap: Dict[str, List[str]] = {}
    for di in data_items:
        qmap.setdefault(di.name, []).append(di.qualified_name)

    linkage_names = {di.name for di in data_items if di.section == "LINKAGE"}
    linkage_qualified = {di.qualified_name for di in data_items if di.section == "LINKAGE"}
    using_params = set(procedure_using or [])

    def get_direction(simple: str, qualified: str, base_kind: str) -> str:
        is_linkage = (simple in linkage_names) or (qualified in linkage_qualified)
        is_using = simple in using_params
        if is_linkage or is_using:
            return "param_out" if base_kind == "write" else "param_in"
        return base_kind

    # Procedure facts + xrefs
    proc_facts: List[ProcFactRec] = []
    xrefs: List[XrefRec] = []

    def add_xref(qn: str, sn: str, kind: str, ln: int, snip: str, base_kind: str):
        direction = get_direction(sn, qn, base_kind)
        sc, ec = compute_column_positions(sn, snip)
        xrefs.append(XrefRec(
            file_id=file_id,
            qualified_name=qn,
            simple_name=sn,
            kind=kind,
            line=ln,
            snippet=snip,
            direction=direction,
            path=path,
            program_id=program_id,
            start_col=sc,
            end_col=ec,
        ))

    # Statements pass
    statements = split_statements(norm_lines)
    for end_line, stmt in statements:
        s = stmt.strip()
        if not s:
            continue
        # PERFORM
        m = RE_PERFORM.match(s)
        if m:
            proc_facts.append(ProcFactRec(
                file_id, "perform", end_line, s,
                para=line2para.get(end_line),
                thru=(m.group(2).upper() if m.group(2) else None)
            ))
            continue
        # CALL
        if RE_CALL_LINE.search(s):
            callee, callee_data_name, is_dynamic = parse_call(s)
            using_raw = None
            # Extract USING list if present
            m_using = re.search(r"\bUSING\s+(.+?)\.", s, flags=re.IGNORECASE)
            if m_using:
                using_raw = m_using.group(1).strip()
                for token in re.findall(r"[A-Z0-9\-]+", using_raw, flags=re.IGNORECASE):
                    t = token.upper()
                    if t in qmap:
                        for qn in qmap[t]:
                            add_xref(qn, t, "param", end_line, s, base_kind="param")
            proc_facts.append(ProcFactRec(
                file_id, "call", end_line, s,
                para=line2para.get(end_line),
                callee=callee,
                callee_data_name=callee_data_name,
                is_dynamic=is_dynamic,
                using_raw=using_raw,
            ))
            continue
        # MOVE
        m = RE_MOVE.match(s)
        if m:
            src = m.group(1).strip()
            tgt = m.group(2).upper()
            proc_facts.append(ProcFactRec(file_id, "move", end_line, s, para=line2para.get(end_line), target=tgt, source_raw=src))
            if tgt in qmap:
                for qn in qmap[tgt]:
                    add_xref(qn, tgt, "write", end_line, s, base_kind="write")
            for token in re.findall(r"[A-Z0-9\-]+", src, flags=re.IGNORECASE):
                t = token.upper()
                if t in qmap:
                    for qn in qmap[t]:
                        add_xref(qn, t, "read", end_line, s, base_kind="read")
            continue
        # COMPUTE
        m = RE_COMPUTE.match(s)
        if m:
            tgt = m.group(1).upper()
            expr = m.group(2).strip()
            proc_facts.append(ProcFactRec(file_id, "compute", end_line, s, para=line2para.get(end_line), target=tgt, expr_raw=expr))
            if tgt in qmap:
                for qn in qmap[tgt]:
                    add_xref(qn, tgt, "write", end_line, s, base_kind="write")
            for token in re.findall(r"[A-Z0-9\-]+", expr, flags=re.IGNORECASE):
                t = token.upper()
                if t in qmap:
                    for qn in qmap[t]:
                        add_xref(qn, t, "read", end_line, s, base_kind="read")
            continue
        # READ INTO
        m = RE_READ_INTO.match(s)
        if m:
            into = m.group(2).upper() if m.group(2) else None
            if into:
                proc_facts.append(ProcFactRec(file_id, "read", end_line, s, para=line2para.get(end_line), target=into))
                if into in qmap:
                    for qn in qmap[into]:
                        add_xref(qn, into, "write", end_line, s, base_kind="write")
            continue
        # ACCEPT
        m = RE_ACCEPT.match(s)
        if m:
            tgt = m.group(1).upper()
            proc_facts.append(ProcFactRec(file_id, "accept", end_line, s, para=line2para.get(end_line), target=tgt))
            if tgt in qmap:
                for qn in qmap[tgt]:
                    add_xref(qn, tgt, "write", end_line, s, base_kind="write")
            continue

    # Flow edges from PERFORM and PERFORM THRU
    flow_edges: List[FlowEdgeRec] = []
    ordered_paras = [p for p in paragraphs if p.kind == "paragraph"]
    para_index = {p.name: idx for idx, p in enumerate(ordered_paras)}
    for fact in proc_facts:
        if fact.kind != "perform" or not fact.para:
            continue
        start = fact.snippet
        m = RE_PERFORM.match(start)
        if not m:
            continue
        a = m.group(1).upper()
        b = m.group(2).upper() if m.group(2) else None
        if b and a in para_index and b in para_index:
            i0, i1 = para_index[a], para_index[b]
            if i0 <= i1:
                for i in range(i0, i1 + 1):
                    flow_edges.append(FlowEdgeRec(file_id, fact.para, ordered_paras[i].name, fact.line, "perform"))
        elif a in para_index:
            flow_edges.append(FlowEdgeRec(file_id, fact.para, a, fact.line, "perform"))

    # COPY statements
    copybooks: List[CopybookRec] = []
    for i, line in enumerate(norm_lines, start=1):
        cm = RE_COPY.match(line.strip())
        if cm:
            # Extract copybook name from either quoted path (group 1) or unquoted name (group 2)
            copybook_name = cm.group(1) or cm.group(2)
            # Extract just the filename from path if it contains slashes
            if '/' in copybook_name:
                copybook_name = copybook_name.split('/')[-1]
            # Remove .CPY extension if present
            if copybook_name.upper().endswith('.CPY'):
                copybook_name = copybook_name[:-4]
            
            copybooks.append(CopybookRec(
                file_id=file_id,
                parent_path=path,
                copybook_name=copybook_name.upper(),
                line=i,
                replacing_clause=(cm.group(3).strip() if cm.group(3) else None),
            ))

    # Chunks: paragraph + level-01 data groups
    chunks: List[ChunkRec] = []
    for p in ordered_paras:
        chunks.append(ChunkRec(
            chunk_id=f"{file_id}:{p.name}:{p.start_line}-{p.end_line}",
            file_id=file_id,
            path=path,
            program_id=program_id,
            scope="paragraph",
            name=p.name,
            start_line=p.start_line,
            end_line=p.end_line,
            text="\n".join(norm_lines[p.start_line - 1:p.end_line]),
        ))

    level01 = [di for di in data_items if di.level == 1]
    for idx, di in enumerate(level01):
        start = di.start_line
        if (idx + 1) < len(level01):
            # Not the last level-01 item
            end = level01[idx + 1].start_line - 1
        else:
            # Last level-01 item - extend until PROCEDURE DIVISION or end of file
            if proc_div_start is not None:
                end = proc_div_start - 1
            else:
                end = len(norm_lines)
        chunks.append(ChunkRec(
            chunk_id=f"{file_id}:{di.name}:{start}-{end}",
            file_id=file_id,
            path=path,
            program_id=program_id,
            scope="data-group",
            name=di.name,
            start_line=start,
            end_line=end,
            text="\n".join(norm_lines[start - 1:end]),
        ))

    return file_rec, paragraphs, data_items, proc_facts, xrefs, chunks, flow_edges, copybooks, raw_lines

# -------------------------------
# Local mode
# -------------------------------

def find_cobol_files(root_dir: str) -> List[str]:
    files: List[str] = []
    for dirpath, _, filenames in os.walk(root_dir):
        for fname in filenames:
            ext = os.path.splitext(fname)[1]
            if ext in COBOL_EXTS:
                files.append(os.path.join(dirpath, fname))
    return files

def read_file_text(path: str) -> str:
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        return f.read()

def write_jsonl_local(out_dir: str, filename: str, rows: List[dict]) -> None:
    os.makedirs(out_dir, exist_ok=True)
    out_path = os.path.join(out_dir, filename)
    with open(out_path, 'w', encoding='utf-8') as f:
        for r in rows:
            json_line = json.dumps(r, ensure_ascii=False)
            # Validate each line before writing
            try:
                json.loads(json_line)  # Validate it's valid JSON
            except Exception as e:
                print(f"❌ Corrupt JSONL line in {filename}: {json_line[:120]}... Error: {e}")
                raise
            f.write(json_line + "\n")
    print(f"Wrote {filename} -> {out_path} ({len(rows)} records)")

def append_jsonl_local(out_dir: str, filename: str, rows: List[dict]) -> None:
    """Append rows to existing JSONL file"""
    os.makedirs(out_dir, exist_ok=True)
    out_path = os.path.join(out_dir, filename)
    with open(out_path, 'a', encoding='utf-8') as f:
        for r in rows:
            json_line = json.dumps(r, ensure_ascii=False)
            # Validate each line before writing
            try:
                json.loads(json_line)  # Validate it's valid JSON
            except Exception as e:
                print(f"❌ Corrupt JSONL line in {filename}: {json_line[:120]}... Error: {e}")
                raise
            f.write(json_line + "\n")

def flush_if_needed(out_dir: str, filename: str, rows: List[dict], first_flush: bool = False) -> List[dict]:
    """Flush rows to JSONL if they exceed CHUNK_FLUSH_SIZE, return empty list if flushed"""
    if len(rows) >= CHUNK_FLUSH_SIZE:
        if first_flush:
            write_jsonl_local(out_dir, filename, rows)
        else:
            append_jsonl_local(out_dir, filename, rows)
        print(f"Flushed {len(rows)} records to {filename}")
        return []
    return rows

def append_jsonl_blob(container_client, blob_path: str, rows: List[dict]) -> None:
    """Append JSONL rows to a blob (download existing + append + reupload)"""
    try:
        # Try to download existing content
        blob_client = container_client.get_blob_client(blob_path)
        existing_content = blob_client.download_blob().readall().decode("utf-8", errors="replace")
        existing_lines = []
        if existing_content.strip():
            # Only include lines that are valid JSON to prevent corruption
            for line in existing_content.strip().split('\n'):
                if line.strip():
                    try:
                        json.loads(line)  # Validate it's valid JSON
                        existing_lines.append(line)
                    except:
                        # Skip invalid JSON lines to prevent corruption
                        pass
    except Exception:
        # Blob doesn't exist or is empty
        existing_lines = []
    
    # Append new rows
    new_lines = [json.dumps(r, ensure_ascii=False) for r in rows]
    all_lines = existing_lines + new_lines
    
    # Upload combined content with proper line ending
    payload = '\n'.join(all_lines) + '\n'
    
    # Validate JSONL format before writing
    for line in payload.splitlines():
        if line.strip():  # Skip empty lines
            try:
                json.loads(line)
            except Exception as e:
                print(f"❌ Corrupt JSONL line in {blob_path}: {line[:120]}... Error: {e}")
                raise
    
    blob_client = container_client.get_blob_client(blob_path)
    blob_client.upload_blob(payload.encode("utf-8"), overwrite=True)

def flush_if_needed_blob(container_client, out_prefix: str, filename: str, rows: List[dict]) -> List[dict]:
    """Flush rows to blob JSONL if they exceed CHUNK_FLUSH_SIZE, return empty list if flushed"""

    if len(rows) >= CHUNK_FLUSH_SIZE:
        blob_path = f"{out_prefix}/{filename}"
        append_jsonl_blob(container_client, blob_path, rows)
        print(f"🔄 Streaming flush: {len(rows)} records to {blob_path}")
        return []
    return rows

# --- Sharded flushing for very large datasets (e.g., procedure_facts) ---
FACTS_SHARD_MAX_BYTES = 80 * 1024 * 1024  # target ~80MB per shard (below 128MB limit)

def shard_upload_blob(container_client, out_prefix: str, base_name: str, shard_index: int, rows: List[dict]):
    shard_name = f"{base_name.rsplit('.',1)[0]}_{shard_index:04d}.jsonl"
    # Place procedure_facts shards under dedicated subfolder so data source prefix can target only facts
    if base_name == 'procedure_facts.jsonl':
        blob_path = f"{out_prefix}/procedure_facts/{shard_name}"
    else:
        blob_path = f"{out_prefix}/{shard_name}"
    payload_lines = [json.dumps(r, ensure_ascii=False) for r in rows]
    payload = "\n".join(payload_lines) + "\n"
    # quick validation
    for i,line in enumerate(payload_lines[:50]):
        try: json.loads(line)
        except Exception as e:
            print(f"❌ Invalid JSON in shard {shard_name} line {i+1}: {e}")
            raise
    container_client.get_blob_client(blob_path).upload_blob(payload.encode('utf-8'), overwrite=True)
    print(f"🟢 Shard written: {shard_name} ({len(rows)} rows, {len(payload)} bytes)")

def flush_sharded(collection_name: str, container_client, out_prefix: str, base_filename: str, rows: List[dict], shard_state: dict) -> List[dict]:
    """Accumulate rows; when size threshold exceeded write a shard and reset.
    shard_state tracks {'facts': {'index': int, 'rows_total': int}}
    """
    if collection_name not in shard_state:
        shard_state[collection_name] = {"index": 0, "current_size": 0, "rows_total": 0, "buffer": []}
    st = shard_state[collection_name]
    # Append incoming rows into shard buffer
    for r in rows:
        line = json.dumps(r, ensure_ascii=False)
        st['buffer'].append(r)
        st['current_size'] += len(line) + 1
    rows.clear()
    if st['current_size'] >= FACTS_SHARD_MAX_BYTES:
        shard_upload_blob(container_client, out_prefix, base_filename, st['index'], st['buffer'])
        st['rows_total'] += len(st['buffer'])
        st['index'] += 1
        st['buffer'] = []
        st['current_size'] = 0
    return rows

def run_local(root_dir: str) -> None:
    print(f"Scanning COBOL files under: {root_dir}")
    cobol_files = find_cobol_files(root_dir)
    print(f"Found {len(cobol_files)} files")

    files_out: List[dict] = []
    paras_out: List[dict] = []
    items_out: List[dict] = []
    facts_out: List[dict] = []
    xrefs_out: List[dict] = []
    chunks_out: List[dict] = []
    flow_edges_out: List[dict] = []
    copybooks_out: List[dict] = []

    jsonl_dir = os.path.join(root_dir, "JSONL")

    # Tracking for streaming flushes
    flush_counters = {
        "files": False, "paras": False, "items": False, "facts": False,
        "xrefs": False, "chunks": False, "flow_edges": False, "copybooks": False
    }

    i = 0
    for path in cobol_files:
        i += 1
        file_id = f"f{i:06d}"
        text = read_file_text(path)
        fr, paras, items, facts, xrefs, chunks, edges, copies, raw = process_text(path, file_id, text)
        
        # Log message based on file type
        if fr.copybook:
            print(f"{path} ({len(raw)} lines) — copybook detected")
        else:
            print(f"Processed {path}: paras={len(paras)} items={len(items)} facts={len(facts)} xrefs={len(xrefs)} chunks={len(chunks)}")

        files_out.append({
            "file_id": fr.file_id,
            "path": fr.path,
            "name": os.path.basename(fr.path),
            "program_id": fr.program_id,
            "lines": len(raw),
            "format": fr.fmt,
            "procedure_using": fr.procedure_using,
            "copybook": fr.copybook,
        })
        for p in paras:
            # Use sanitized paragraph id (Azure Search key cannot contain ':')
            paras_out.append({
                "para_id": sanitize_id(f"{file_id}:{p.name}"),
                "file_id": file_id,
                "name": p.name,
                "kind": p.kind,
                "start_line": p.start_line,
                "end_line": p.end_line,
            })
        for it in items:
            items_out.append({
                "item_id": sanitize_id(f"{file_id}:{it.qualified_name}"),
                "file_id": file_id,
                "path": path,
                "program_id": fr.program_id,
                "name": it.name,
                "qualified_name": it.qualified_name,
                "section": it.section,
                "level": it.level,
                "pic": it.pic,
                "usage": it.usage,
                "occurs_low": it.occurs_low,
                "occurs_high": it.occurs_high,
                "depends_on": it.depends_on,
                "redefines": it.redefines,
                "renames": it.renames,
                "value": it.value,
                "start_line": it.start_line,
                "end_line": it.end_line,
            })
        for f in facts:
            facts_out.append({
                "fact_id": sanitize_id(f"{file_id}:{f.line}:{f.kind}"),
                "file_id": file_id,
                "para": f.para,
                "line": f.line,
                "kind": f.kind,
                "snippet": f.snippet,
                "callee": f.callee,
                "callee_data_name": f.callee_data_name,
                "is_dynamic": f.is_dynamic,
                "using_raw": f.using_raw,
                "target": f.target,
                "source_raw": f.source_raw,
                "expr_raw": f.expr_raw,
            })
        for xr in xrefs:
            xrefs_out.append({
                "xref_id": sanitize_id(f"{file_id}:{xr.line}:{xr.simple_name}:{xr.kind}"),
                "file_id": file_id,
                "path": xr.path,
                "program_id": xr.program_id,
                "line": xr.line,
                "qualified_name": xr.qualified_name,
                "simple_name": xr.simple_name,
                "kind": xr.kind,
                "snippet": xr.snippet,
                "direction": xr.direction,
                "start_col": xr.start_col,
                "end_col": xr.end_col,
            })
        for ch in chunks:
            chunks_out.append({
                "chunk_id": sanitize_id(ch.chunk_id),
                "file_id": ch.file_id,
                "path": ch.path,
                "program_id": ch.program_id,
                "scope": ch.scope,
                "name": ch.name,
                "start_line": ch.start_line,
                "end_line": ch.end_line,
                "text": ch.text,
            })
        for e in edges:
            flow_edges_out.append({
                "edge_id": sanitize_id(f"{file_id}:{e.line}:{e.caller_para}:{e.target_para}"),
                "file_id": e.file_id,
                "caller_para": e.caller_para,
                "target_para": e.target_para,
                "line": e.line,
                "kind": e.kind,
            })
        for cb in copies:
            copybooks_out.append({
                "copybook_id": sanitize_id(f"{file_id}:{cb.line}:{cb.copybook_name}"),
                "file_id": cb.file_id,
                "parent_path": cb.parent_path,
                "copybook_name": cb.copybook_name,
                "line": cb.line,
                "replacing_clause": cb.replacing_clause,
            })

    # calls.jsonl derived from facts
    calls_out: List[dict] = []
    for rec in facts_out:
        if rec["kind"] != "call":
            continue
        if rec.get("is_dynamic") is False and rec.get("callee"):
            call_id = sanitize_id(f"{rec['file_id']}:{rec['line']}:{rec['callee']}")
            callee_program = rec["callee"]
            callee_data_name = None
        elif rec.get("is_dynamic") is True and rec.get("callee_data_name"):
            call_id = sanitize_id(f"{rec['file_id']}:{rec['line']}:{rec['callee_data_name']}")
            callee_program = None
            callee_data_name = rec["callee_data_name"]
        else:
            continue
        calls_out.append({
            "call_id": call_id,
            "file_id": rec["file_id"],
            "caller_para": rec["para"],
            "callee_program": callee_program,
            "callee_data_name": callee_data_name,
            "is_dynamic": rec.get("is_dynamic"),
            "line": rec["line"],
            "snippet": rec["snippet"],
        })

    # Write final JSONL files (handle any remaining data and calls)
    if files_out:
        if flush_counters["files"]:
            append_jsonl_local(jsonl_dir, "files.jsonl", files_out)
        else:
            write_jsonl_local(jsonl_dir, "files.jsonl", files_out)
    elif not flush_counters["files"]:
        write_jsonl_local(jsonl_dir, "files.jsonl", [])
        
    if paras_out:
        if flush_counters["paras"]:
            append_jsonl_local(jsonl_dir, "paragraphs.jsonl", paras_out)
        else:
            write_jsonl_local(jsonl_dir, "paragraphs.jsonl", paras_out)
    elif not flush_counters["paras"]:
        write_jsonl_local(jsonl_dir, "paragraphs.jsonl", [])
        
    if items_out:
        if flush_counters["items"]:
            append_jsonl_local(jsonl_dir, "data_items.jsonl", items_out)
        else:
            write_jsonl_local(jsonl_dir, "data_items.jsonl", items_out)
    elif not flush_counters["items"]:
        write_jsonl_local(jsonl_dir, "data_items.jsonl", [])
        
    if facts_out:
        if flush_counters["facts"]:
            append_jsonl_local(jsonl_dir, "procedure_facts.jsonl", facts_out)
        else:
            write_jsonl_local(jsonl_dir, "procedure_facts.jsonl", facts_out)
    elif not flush_counters["facts"]:
        write_jsonl_local(jsonl_dir, "procedure_facts.jsonl", [])
        
    if xrefs_out:
        if flush_counters["xrefs"]:
            append_jsonl_local(jsonl_dir, "xrefs.jsonl", xrefs_out)
        else:
            write_jsonl_local(jsonl_dir, "xrefs.jsonl", xrefs_out)
    elif not flush_counters["xrefs"]:
        write_jsonl_local(jsonl_dir, "xrefs.jsonl", [])
        
    write_jsonl_local(jsonl_dir, "calls.jsonl", calls_out)
    
    if chunks_out:
        if flush_counters["chunks"]:
            append_jsonl_local(jsonl_dir, "chunks.jsonl", chunks_out)
        else:
            write_jsonl_local(jsonl_dir, "chunks.jsonl", chunks_out)
    elif not flush_counters["chunks"]:
        write_jsonl_local(jsonl_dir, "chunks.jsonl", [])
        
    if flow_edges_out:
        if flush_counters["flow_edges"]:
            append_jsonl_local(jsonl_dir, "flow_edges.jsonl", flow_edges_out)
        else:
            write_jsonl_local(jsonl_dir, "flow_edges.jsonl", flow_edges_out)
    elif not flush_counters["flow_edges"]:
        write_jsonl_local(jsonl_dir, "flow_edges.jsonl", [])
        
    if copybooks_out:
        if flush_counters["copybooks"]:
            append_jsonl_local(jsonl_dir, "copybooks.jsonl", copybooks_out)
        else:
            write_jsonl_local(jsonl_dir, "copybooks.jsonl", copybooks_out)
    elif not flush_counters["copybooks"]:
        write_jsonl_local(jsonl_dir, "copybooks.jsonl", [])

    print(f"Done. Wrote JSONL to: {jsonl_dir}")

# -------------------------------
# Azure Blob mode
# -------------------------------

def read_local_settings() -> Optional[str]:
    """Read Azure Storage connection string from local.settings.json"""
    try:
        settings_path = os.path.join(os.path.dirname(__file__), "local.settings.json")
        if os.path.exists(settings_path):
            import json
            with open(settings_path, 'r') as f:
                settings = json.load(f)
                # Try multiple possible keys
                values = settings.get("Values", {})
                for key in ["DEPLOYMENT_STORAGE_CONNECTION_STRING", "AzureWebJobsStorage", "AZURE_STORAGE_CONNECTION_STRING"]:
                    if key in values and values[key]:
                        return values[key]
    except Exception as e:
        print(f"Warning: Could not read local.settings.json: {e}")
    return None

def get_blob_service(connection_string: Optional[str], account_url: Optional[str]):
    if BlobServiceClient is None:
        raise RuntimeError("azure-storage-blob is not installed. pip install azure-storage-blob")
    
    # Priority order: command line args, local.settings.json, environment variable
    if connection_string:
        return BlobServiceClient.from_connection_string(connection_string)
    if account_url:
        return BlobServiceClient(account_url=account_url)
    
    # Try local.settings.json
    local_conn_str = read_local_settings()
    if local_conn_str:
        print("Using connection string from local.settings.json")
        return BlobServiceClient.from_connection_string(local_conn_str)
    
    # Fall back to environment variable
    env = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
    if env:
        return BlobServiceClient.from_connection_string(env)
        
    raise RuntimeError("No Azure Storage connection found. Add to local.settings.json, set AZURE_STORAGE_CONNECTION_STRING, or pass --connection-string/--account-url")

def iter_cobol_blobs(cc, prefix: str) -> Iterable:
    prefix = prefix.lstrip("/") if prefix else ""
    for blob in cc.list_blobs(name_starts_with=prefix):
        name = blob.name
        if blob.size == 0:
            continue
        ext = os.path.splitext(name)[1]
        if ext in COBOL_EXTS:
            yield blob

def download_blob_text(cc, blob_name: str) -> str:
    return cc.get_blob_client(blob=blob_name).download_blob().readall().decode("utf-8", errors="replace")

def write_jsonl_to_blob(cc, prefix: str, filename: str, rows: List[dict]) -> None:
    out_path = f"{prefix.rstrip('/')}/JSONL/{filename}"

    bc = cc.get_blob_client(out_path)
    
    # Debug: Check what we're trying to serialize
    if filename == "chunks.jsonl" and len(rows) > 0:
        print(f"🔍 Debug: First chunk record keys: {list(rows[0].keys())}")
        text_field = rows[0].get('text', '')
        print(f"🔍 Debug: First chunk text length: {len(text_field)}")
        print(f"🔍 Debug: First chunk text preview: {repr(text_field[:100])}")
    
    payload = "\n".join(json.dumps(r, ensure_ascii=False) for r in rows) + "\n"
    
    # Validate JSONL format before writing
    print(f"🔍 Debug: Validating {len(payload.splitlines())} lines for {filename}")
    for i, line in enumerate(payload.splitlines()):
        if line.strip():  # Skip empty lines
            try:
                json.loads(line)
            except Exception as e:
                print(f"❌ Corrupt JSONL line {i+1} in {filename}: {line[:120]}... Error: {e}")
                print(f"🔍 Debug: Line {i+1} full content: {repr(line)}")
                raise
    
    bc.upload_blob(payload.encode("utf-8"), overwrite=True)
    print(f"Wrote {filename} -> {out_path} ({len(rows)} records)")

def run_blob_processing(container: str, prefix: str, connection_string: Optional[str], max_files: int = 0) -> None:
    """Process COBOL files directly from Azure Blob Storage"""

    # Use the existing get_blob_service function that handles local.settings.json
    bsc = get_blob_service(connection_string, None)
    cc = bsc.get_container_client(container)
    
    # Find COBOL files, excluding JSONL directories
    prefix = prefix.rstrip('/') + '/' if prefix and not prefix.endswith('/') else (prefix or '')
    cobol_blobs = []
    
    print(f"Scanning blobs in container '{container}' with prefix '{prefix}'...")
    
    for blob in cc.list_blobs(name_starts_with=prefix):
        # Skip directories and JSONL folders
        if blob.size == 0:  # Directory marker
            continue
        if '/jsonl/' in blob.name.lower():  # Skip JSONL folders (case-insensitive)
            continue
        
        # Check if it's a COBOL file
        ext = os.path.splitext(blob.name)[1].lower()
        if ext in {".cbl", ".cob", ".cobol", ".cpy"}:
            cobol_blobs.append(blob)
            if max_files > 0 and len(cobol_blobs) >= max_files:
                break
    
    print(f"Found {len(cobol_blobs)} COBOL files to process")
    if len(cobol_blobs) == 0:
        print("No COBOL files found. Exiting.")
        sys.exit(1)
    
    # Process each blob and accumulate results (with streaming)
    files_out: List[dict] = []
    paras_out: List[dict] = []
    items_out: List[dict] = []
    facts_out: List[dict] = []
    xrefs_out: List[dict] = []
    chunks_out: List[dict] = []
    flow_edges_out: List[dict] = []
    copybooks_out: List[dict] = []
    
    # Track suspect files for summary report
    suspect_files = []
    processed_count = 0
    
    for i, blob in enumerate(cobol_blobs, 1):
        file_id = f"f{i:06d}"
        
        try:
            # Download blob content
            blob_client = cc.get_blob_client(blob.name)
            text = blob_client.download_blob().readall().decode("utf-8", errors="replace")
            
            # Process using existing pipeline
            fr, paras, items, facts, xrefs, chunks, edges, copies, raw = process_file_content(blob.name, text, file_id)
            
            # Log message based on file type
            if fr.copybook:
                print(f"{blob.name} ({len(raw)} lines) — copybook detected")
            else:
                print(f"Processing {blob.name} → {file_id} ({len(raw)} lines)")
            
            # Check for suspect files using regex patterns (exclude copybooks)
            if not fr.copybook:
                program_id_count = sum(1 for line in raw if RE_PROGRAM_ID_DETECT.match(line.strip()))
                procedure_div_count = sum(1 for line in raw if RE_PROCEDURE_DIV_DETECT.match(line.strip()))
                
                if program_id_count == 0 or procedure_div_count == 0:
                    suspect_files.append({
                        "path": blob.name,
                        "file_id": file_id,
                        "program_id_count": program_id_count,
                        "procedure_div_count": procedure_div_count,
                        "lines": len(raw)
                    })
            
            # Accumulate file record
            files_out.append({
                "file_id": fr.file_id,
                "path": blob.name,
                "name": os.path.basename(blob.name),
                "program_id": fr.program_id,
                "lines": len(raw),
                "format": fr.fmt,
                "procedure_using": fr.procedure_using,
                "copybook": fr.copybook,
            })
            
            # Accumulate paragraphs
            for p in paras:
                paras_out.append({
                    "para_id": sanitize_id(f"{file_id}:{p.name}"),
                    "file_id": file_id,
                    "name": p.name,
                    "kind": p.kind,
                    "start_line": p.start_line,
                    "end_line": p.end_line,
                })
            
            # Accumulate data items
            for it in items:
                items_out.append({
                    "item_id": sanitize_id(f"{file_id}:{it.qualified_name}"),
                    "file_id": file_id,
                    "path": blob.name,
                    "program_id": fr.program_id,
                    "name": it.name,
                    "qualified_name": it.qualified_name,
                    "section": it.section,
                    "level": it.level,
                    "pic": it.pic,
                    "usage": it.usage,
                    "occurs_low": it.occurs_low,
                    "occurs_high": it.occurs_high,
                    "depends_on": it.depends_on,
                    "redefines": it.redefines,
                    "renames": it.renames,
                    "value": it.value,
                    "start_line": it.start_line,
                    "end_line": it.end_line,
                })
            
            # Accumulate procedure facts
            for f in facts:
                facts_out.append({
                    "fact_id": sanitize_id(f"{file_id}:{f.line}:{f.kind}"),
                    "file_id": file_id,
                    "para": f.para,
                    "line": f.line,
                    "kind": f.kind,
                    "snippet": f.snippet,
                    "callee": f.callee,
                    "callee_data_name": f.callee_data_name,
                    "is_dynamic": f.is_dynamic,
                    "using_raw": f.using_raw,
                    "target": f.target,
                    "source_raw": f.source_raw,
                    "expr_raw": f.expr_raw,
                })
            
            # Accumulate xrefs
            for xr in xrefs:
                xrefs_out.append({
                    "xref_id": sanitize_id(f"{file_id}:{xr.line}:{xr.simple_name}:{xr.kind}"),
                    "file_id": file_id,
                    "path": blob.name,
                    "program_id": fr.program_id,
                    "line": xr.line,
                    "qualified_name": xr.qualified_name,
                    "simple_name": xr.simple_name,
                    "kind": xr.kind,
                    "snippet": xr.snippet,
                    "direction": xr.direction,
                    "start_col": xr.start_col,
                    "end_col": xr.end_col,
                })
            
            # Accumulate chunks
            for ch in chunks:
                chunks_out.append({
                    "chunk_id": sanitize_id(ch.chunk_id),
                    "file_id": ch.file_id,
                    "path": blob.name,
                    "program_id": fr.program_id,
                    "scope": ch.scope,
                    "name": ch.name,
                    "start_line": ch.start_line,
                    "end_line": ch.end_line,
                    "text": ch.text,
                })
            
            # Accumulate flow edges
            for e in edges:
                flow_edges_out.append({
                    "edge_id": sanitize_id(f"{file_id}:{e.line}:{e.caller_para}:{e.target_para}"),
                    "file_id": e.file_id,
                    "caller_para": e.caller_para,
                    "target_para": e.target_para,
                    "line": e.line,
                    "kind": e.kind,
                })
            
            # Accumulate copybooks
            for cb in copies:
                copybooks_out.append({
                    "copybook_id": sanitize_id(f"{file_id}:{cb.line}:{cb.copybook_name}"),
                    "file_id": cb.file_id,
                    "parent_path": blob.name,
                    "copybook_name": cb.copybook_name,
                    "line": cb.line,
                    "replacing_clause": cb.replacing_clause,
                })
            
            processed_count += 1
            
            # Stream flush individual collections when they get large
            out_prefix = prefix.rstrip('/') + '/JSONL'
            files_out = flush_if_needed_blob(cc, out_prefix, "files.jsonl", files_out)
            paras_out = flush_if_needed_blob(cc, out_prefix, "paragraphs.jsonl", paras_out)
            items_out = flush_if_needed_blob(cc, out_prefix, "data_items.jsonl", items_out)
            # Sharded flush for large procedure facts to keep each blob under size cap
            shard_state = globals().setdefault('_FACTS_SHARD_STATE', {})
            if facts_out:
                facts_out = flush_sharded('facts', cc, out_prefix, 'procedure_facts.jsonl', facts_out, shard_state)
            xrefs_out = flush_if_needed_blob(cc, out_prefix, "xrefs.jsonl", xrefs_out)
            chunks_out = flush_if_needed_blob(cc, out_prefix, "chunks.jsonl", chunks_out)
            flow_edges_out = flush_if_needed_blob(cc, out_prefix, "flow_edges.jsonl", flow_edges_out)
            copybooks_out = flush_if_needed_blob(cc, out_prefix, "copybooks.jsonl", copybooks_out)
            
        except Exception as e:
            print(f"Error processing {blob.name}: {e}")
            continue
    
    if processed_count == 0:
        print("No files were successfully processed. Exiting.")
        sys.exit(1)
    
    # Derive calls from procedure facts
    calls_out: List[dict] = []
    for rec in facts_out:
        if rec["kind"] != "call":
            continue
        if rec.get("is_dynamic") is False and rec.get("callee"):
            # Static call
            call_id = sanitize_id(f"{rec['file_id']}:{rec['line']}:{rec['callee']}")
            calls_out.append({
                "call_id": call_id,
                "file_id": rec["file_id"],
                "caller_para": rec["para"],
                "callee_program": rec["callee"],
                "callee_data_name": None,
                "is_dynamic": False,
                "line": rec["line"],
                "snippet": rec["snippet"],
            })
        elif rec.get("is_dynamic") is True and rec.get("callee_data_name"):
            # Dynamic call
            call_id = sanitize_id(f"{rec['file_id']}:{rec['line']}:{rec['callee_data_name']}")
            calls_out.append({
                "call_id": call_id,
                "file_id": rec["file_id"],
                "caller_para": rec["para"],
                "callee_program": None,
                "callee_data_name": rec["callee_data_name"],
                "is_dynamic": True,
                "line": rec["line"],
                "snippet": rec["snippet"],
            })
    
    # Final flush: append any remaining records to blob storage
    out_prefix = prefix.rstrip('/') + '/JSONL'
    # Handle final facts shard buffer
    shard_state = globals().get('_FACTS_SHARD_STATE') or {}
    if 'facts' in shard_state:
        st = shard_state['facts']
        if st['buffer']:
            shard_upload_blob(cc, out_prefix, 'procedure_facts.jsonl', st['index'], st['buffer'])
            st['rows_total'] += len(st['buffer'])
            st['buffer'] = []
        # Write a manifest listing shards for traceability
        manifest = {
            'shards': st['index'] + 1,
            'rows_total': st['rows_total'],
            'base': 'procedure_facts',
        }
        # Write manifest inside the procedure_facts folder for cleanliness
        cc.get_blob_client(f"{out_prefix}/procedure_facts/procedure_facts_manifest.json").upload_blob(json.dumps(manifest, indent=2).encode('utf-8'), overwrite=True)
        # Also write an empty placeholder root file for backward compatibility (some tools expect procedure_facts.jsonl)
        try:
            placeholder_path = f"{out_prefix}/procedure_facts/procedure_facts.jsonl"
            # Always overwrite with empty to avoid leaving legacy oversized file
            cc.upload_blob(name=placeholder_path, data=b"", overwrite=True)
        except Exception as e:
            print(f"Warning: could not create placeholder procedure_facts.jsonl: {e}")

    final_files = [
        ("files.jsonl", files_out),
        ("paragraphs.jsonl", paras_out),
        ("data_items.jsonl", items_out),
        # procedure facts now sharded; skip single-file write
        ("xrefs.jsonl", xrefs_out),
        ("calls.jsonl", calls_out),
        ("chunks.jsonl", chunks_out),
        ("flow_edges.jsonl", flow_edges_out),
        ("copybooks.jsonl", copybooks_out),
    ]
    
    print(f"\nFinal flush to {container}/{out_prefix}/...")
    for filename, data in final_files:
        if data:  # Only write if there are remaining records
            try:

                # Use write_jsonl_to_blob instead of append_jsonl_blob to avoid corruption
                blob_path = f"{out_prefix}/{filename}"
                bc = cc.get_blob_client(blob_path)
                payload = "\n".join(json.dumps(r, ensure_ascii=False) for r in data) + "\n"
                

                # Validate before writing
                validation_errors = 0
                for i, line in enumerate(payload.splitlines()):
                    if line.strip():
                        try:
                            json.loads(line)
                        except Exception as e:
                            print(f"❌ Corrupt JSONL line {i+1} in {filename}: {line[:120]}... Error: {e}")
                            validation_errors += 1
                            if validation_errors > 3:  # Stop after a few errors
                                break
                
                if validation_errors > 0:
                    print(f"❌ Validation failed for {filename}, skipping upload")
                    continue
                
                bc.upload_blob(payload.encode("utf-8"), overwrite=True)
                print(f"Final flush: {filename} ({len(data)} records)")
                

            except Exception as e:
                print(f"Error writing {filename}: {e}")
    
    # Get final counts by reading back from blobs
    print(f"\n✅ Successfully processed {processed_count} COBOL files")
    print(f"📊 Final Summary:")
    for filename, _ in final_files:
        try:
            blob_client = cc.get_blob_client(f"{out_prefix}/{filename}")
            content = blob_client.download_blob().readall().decode("utf-8", errors="replace")
            record_count = len([line for line in content.split('\n') if line.strip()])
            print(f"   {filename}: {record_count} records")
        except Exception:
            print(f"   {filename}: 0 records")
    

    
    # Report suspect files
    if suspect_files:
        print(f"\n⚠️  Found {len(suspect_files)} suspect files:")
        for sf in suspect_files:
            issue_desc = []
            if sf["program_id_count"] == 0:
                issue_desc.append("no PROGRAM-ID")
            if sf["procedure_div_count"] == 0:
                issue_desc.append("no PROCEDURE DIVISION")
            issue_str = ", ".join(issue_desc)
            print(f"   {sf['path']} ({sf['lines']} lines) - {issue_str}")
    else:
        print("\n✅ All files passed basic structure checks")

def run_azure(container: str, prefix: str, connection_string: Optional[str], account_url: Optional[str], limit: Optional[int]) -> None:
    bsc = get_blob_service(connection_string, account_url)
    cc = bsc.get_container_client(container)

    blobs = list(iter_cobol_blobs(cc, prefix))
    if limit is not None:
        blobs = blobs[:limit]
    print(f"Processing {len(blobs)} COBOL files from {container}/{prefix}")

    files_out: List[dict] = []
    paras_out: List[dict] = []
    items_out: List[dict] = []
    facts_out: List[dict] = []
    xrefs_out: List[dict] = []
    chunks_out: List[dict] = []
    flow_edges_out: List[dict] = []
    copybooks_out: List[dict] = []

    i = 0
    for blob in blobs:
        i += 1
        file_id = f"f{i:06d}"
        text = download_blob_text(cc, blob.name)
        fr, paras, items, facts, xrefs, chunks, edges, copies, raw = process_text(blob.name, file_id, text)
        
        # Log message based on file type
        if fr.copybook:
            print(f"{blob.name} ({len(raw)} lines) — copybook detected")
        else:
            print(f"Processed {blob.name}: paras={len(paras)} items={len(items)} facts={len(facts)} xrefs={len(xrefs)} chunks={len(chunks)}")

        files_out.append({
            "file_id": fr.file_id,
            "path": fr.path,
            "name": os.path.basename(fr.path),
            "program_id": fr.program_id,
            "lines": len(raw),
            "format": fr.fmt,
            "procedure_using": fr.procedure_using,
            "copybook": fr.copybook,
        })
        for p in paras:
            paras_out.append({
                "para_id": sanitize_id(f"{file_id}:{p.name}"),
                "file_id": file_id,
                "name": p.name,
                "kind": p.kind,
                "start_line": p.start_line,
                "end_line": p.end_line,
            })
        for it in items:
            items_out.append({
                "item_id": f"{file_id}:{it.qualified_name}",
                "file_id": file_id,
                "path": blob.name,
                "program_id": fr.program_id,
                "name": it.name,
                "qualified_name": it.qualified_name,
                "section": it.section,
                "level": it.level,
                "pic": it.pic,
                "usage": it.usage,
                "occurs_low": it.occurs_low,
                "occurs_high": it.occurs_high,
                "depends_on": it.depends_on,
                "redefines": it.redefines,
                "renames": it.renames,
                "value": it.value,
                "start_line": it.start_line,
                "end_line": it.end_line,
            })
        for f in facts:
            facts_out.append({
                "fact_id": sanitize_id(f"{file_id}:{f.line}:{f.kind}"),
                "file_id": file_id,
                "para": f.para,
                "line": f.line,
                "kind": f.kind,
                "snippet": f.snippet,
                "callee": f.callee,
                "callee_data_name": f.callee_data_name,
                "is_dynamic": f.is_dynamic,
                "using_raw": f.using_raw,
                "target": f.target,
                "source_raw": f.source_raw,
                "expr_raw": f.expr_raw,
            })
        for xr in xrefs:
            xrefs_out.append({
                "xref_id": f"{file_id}:{xr.line}:{xr.simple_name}:{xr.kind}",
                "file_id": file_id,
                "path": xr.path,
                "program_id": xr.program_id,
                "line": xr.line,
                "qualified_name": xr.qualified_name,
                "simple_name": xr.simple_name,
                "kind": xr.kind,
                "snippet": xr.snippet,
                "direction": xr.direction,
                "start_col": xr.start_col,
                "end_col": xr.end_col,
            })
        for ch in chunks:
            chunks_out.append({
                "chunk_id": ch.chunk_id,
                "file_id": ch.file_id,
                "path": ch.path,
                "program_id": ch.program_id,
                "scope": ch.scope,
                "name": ch.name,
                "start_line": ch.start_line,
                "end_line": ch.end_line,
                "text": ch.text,
            })
        for e in edges:
            flow_edges_out.append({
                "edge_id": sanitize_id(f"{file_id}:{e.line}:{e.caller_para}:{e.target_para}"),
                "file_id": e.file_id,
                "caller_para": e.caller_para,
                "target_para": e.target_para,
                "line": e.line,
                "kind": e.kind,
            })
        for cb in copies:
            copybooks_out.append({
                "copybook_id": sanitize_id(f"{file_id}:{cb.line}:{cb.copybook_name}"),
                "file_id": cb.file_id,
                "parent_path": cb.parent_path,
                "copybook_name": cb.copybook_name,
                "line": cb.line,
                "replacing_clause": cb.replacing_clause,
            })

    # calls.jsonl derived from facts
    calls_out: List[dict] = []
    for rec in facts_out:
        if rec["kind"] != "call":
            continue
        if rec.get("is_dynamic") is False and rec.get("callee"):
            call_id = f"{rec['file_id']}:{rec['line']}:{rec['callee']}"
            callee_program = rec["callee"]
            callee_data_name = None
        elif rec.get("is_dynamic") is True and rec.get("callee_data_name"):
            call_id = f"{rec['file_id']}:{rec['line']}:{rec['callee_data_name']}"
            callee_program = None
            callee_data_name = rec["callee_data_name"]
        else:
            continue
        calls_out.append({
            "call_id": call_id,
            "file_id": rec["file_id"],
            "caller_para": rec["para"],
            "callee_program": callee_program,
            "callee_data_name": callee_data_name,
            "is_dynamic": rec.get("is_dynamic"),
            "line": rec["line"],
            "snippet": rec["snippet"],
        })

    # NOTE: Files already written by the final flush section above
    # Don't write again to avoid corruption
    print(f"Done. JSONL files written to: {prefix}/JSONL/ in container {container}")

# -------------------------------
# CLI
# -------------------------------

def parse_args():
    ap = argparse.ArgumentParser(description="COBOL → JSONL extractor (Azure + Local)")
    
    # Local mode
    ap.add_argument('--local', metavar='DIR', help='Process a local folder recursively')
    
    # Azure Blob mode
    ap.add_argument('--container', help='Azure container name (for blob processing)')
    ap.add_argument('--prefix', help='Azure prefix (folder) inside container (for blob processing)')
    ap.add_argument('--max-files', type=int, default=0, help='Max files to process (0 = no limit)')
    ap.add_argument('--connection-string', help='Azure Storage connection string (optional, else env AZURE_STORAGE_CONNECTION_STRING)')
    
    # Legacy compatibility
    ap.add_argument('--azure', action='store_true', help='(Legacy) Process from Azure Blob Storage')
    ap.add_argument('--account-url', help='(Legacy) Azure Storage account URL')
    ap.add_argument('--limit', type=int, help='(Legacy) Only process first N blobs (debug)')
    
    return ap.parse_args()

if __name__ == '__main__':
    args = parse_args()
    
    # Determine processing mode
    if args.container and args.prefix:
        # Azure Blob processing mode
        print("🔵 Azure Blob Processing Mode")
        print("=" * 50)
        run_blob_processing(args.container, args.prefix, args.connection_string, args.max_files)
    elif args.local:
        # Local file processing mode
        print("📁 Local File Processing Mode")
        print("=" * 50)
        run_local(args.local)
    elif args.azure:
        # Legacy Azure mode
        if not args.container or not args.prefix:
            raise SystemExit('--container and --prefix are required with --azure')
        print("🔵 Legacy Azure Processing Mode")
        print("=" * 50)
        run_azure(args.container, args.prefix, args.connection_string, args.account_url, args.limit)
    else:
        # No mode specified
        print("❌ Error: Must specify either --local DIR or --container CONTAINER --prefix PREFIX")
        print("\nUsage examples:")
        print("  Local mode:")
        print("    python cobolparser-clean.py --local /path/to/cobol/files")
        print("  Azure Blob mode:")
        print("    export AZURE_STORAGE_CONNECTION_STRING='...'")
        print("    python cobolparser-clean.py --container aisearch --prefix S35-Source/")
        sys.exit(1)
