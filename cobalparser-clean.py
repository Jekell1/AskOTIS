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
  python cobol_extractor_azure_jsonl.py --local /path/to/folder

Azure Blob:
  export AZURE_STORAGE_CONNECTION_STRING="<connection string>"
  python cobol_extractor_azure_jsonl.py --azure --container mycontainer --prefix path/in/container

Notes
-----
- Writes outputs to <prefix>/JSONL/ in Azure (created if missing) or <dir>/JSONL locally.
- Safe to rerun; JSONL files are overwritten.
"""
from __future__ import annotations
import argparse
import json
import os
import re
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

RE_PROGRAM_ID = re.compile(r"^\s*PROGRAM-ID\.\s+([A-Z0-9\-]+)", re.IGNORECASE)
RE_DATA_DIV = re.compile(r"^\s*DATA\s+DIVISION\.", re.IGNORECASE)
RE_PROC_DIV = re.compile(r"^\s*PROCEDURE\s+DIVISION(?:\s+USING\b.*)?\.", re.IGNORECASE)
RE_PROC_DIV_USING = re.compile(r"^\s*PROCEDURE\s+DIVISION\s+USING\s+(.*?)\.", re.IGNORECASE)
RE_STORAGE_SECTION = re.compile(r"^\s*(WORKING-STORAGE|LOCAL-STORAGE|LINKAGE)\s+SECTION\.", re.IGNORECASE)
RE_SECTION = re.compile(r"^\s*([A-Z0-9\-]+)-SECTION\.", re.IGNORECASE)
RE_PARAGRAPH = re.compile(r"^\s*([A-Z0-9\-]+)\.$", re.IGNORECASE)
RE_DATA_ITEM = re.compile(
    r"^\s*(01|02|03|04|05|06|07|08|09|10|77)\s+([A-Z0-9\-]+)"  # level + name
    r"(?:\s+REDEFINES\s+([A-Z0-9\-]+))?"
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
RE_COPY = re.compile(r"^\s*COPY\s+([A-Z0-9\-]+)(?:\s+REPLACING\s+(.*?))?\s*\.\s*$", re.IGNORECASE)

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
    pat = r"\b" + re.escape(simple_name) + r"\b"
    m = re.search(pat, snippet, flags=re.IGNORECASE)
    if not m:
        return None, None
    return m.start() + 1, m.end()

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

# -------------------------------
# Core processing (shared by local & azure)
# -------------------------------

def process_text(path: str, file_id: str, text: str) -> Tuple[FileRec, List[ParagraphRec], List[DataItemRec], List[ProcFactRec], List[XrefRec], List[ChunkRec], List[FlowEdgeRec], List[CopybookRec], List[str]]:
    raw_lines, norm_lines = normalize_cobol(text)
    fmt = detect_format(raw_lines)

    # Program ID and USING
    program_id: Optional[str] = None
    procedure_using: Optional[List[str]] = None

    for i, line in enumerate(norm_lines, start=1):
        if program_id is None:
            m = RE_PROGRAM_ID.search(line)
            if m:
                program_id = m.group(1).upper()
        if RE_PROC_DIV.search(line) and procedure_using is None:
            um = RE_PROC_DIV_USING.search(line)
            if um:
                using_raw = um.group(1)
                procedure_using = [t.upper() for t in re.findall(r"[A-Z0-9\-]+", using_raw, re.IGNORECASE)] or None

    if not program_id:
        # fallback to file stem
        program_id = os.path.splitext(os.path.basename(path))[0].upper()

    file_rec = FileRec(file_id=file_id, path=path, program_id=program_id, fmt=fmt, procedure_using=procedure_using)

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
            occurs_low = int(dm.group(4)) if dm.group(4) else None
            occurs_high = int(dm.group(5)) if dm.group(5) else None
            depends_on = dm.group(6).upper() if dm.group(6) else None
            pic = dm.group(7)
            usage = dm.group(8)
            value = dm.group(9)

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
            copybooks.append(CopybookRec(
                file_id=file_id,
                parent_path=path,
                copybook_name=cm.group(1).upper(),
                line=i,
                replacing_clause=(cm.group(2).strip() if cm.group(2) else None),
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
        end = (level01[idx + 1].start_line - 1) if (idx + 1) < len(level01) else di.end_line
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
            f.write(json.dumps(r, ensure_ascii=False) + "\n")
    print(f"Wrote {filename} -> {out_path}")

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

    i = 0
    for path in cobol_files:
        i += 1
        file_id = f"f{i:06d}"
        text = read_file_text(path)
        fr, paras, items, facts, xrefs, chunks, edges, copies, raw = process_text(path, file_id, text)
        print(f"Processed {path}: paras={len(paras)} items={len(items)} facts={len(facts)} xrefs={len(xrefs)} chunks={len(chunks)}")

        files_out.append({
            "file_id": fr.file_id,
            "path": fr.path,
            "name": os.path.basename(fr.path),
            "program_id": fr.program_id,
            "lines": len(raw),
            "format": fr.fmt,
            "procedure_using": fr.procedure_using,
        })
        for p in paras:
            paras_out.append({
                "para_id": f"{file_id}:{p.name}",
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
                "name": it.name,
                "qualified_name": it.qualified_name,
                "level": it.level,
                "section": it.section,
                "pic": it.pic,
                "usage": it.usage,
                "occurs_low": it.occurs_low,
                "occurs_high": it.occurs_high,
                "depends_on": it.depends_on,
                "redefines": it.redefines,
                "value": it.value,
                "start_line": it.start_line,
                "end_line": it.end_line,
            })
        for f in facts:
            facts_out.append({
                "fact_id": f"{file_id}:{f.line}:{f.kind}",
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
                "edge_id": f"{file_id}:{e.line}:{e.caller_para}:{e.target_para}",
                "file_id": e.file_id,
                "caller_para": e.caller_para,
                "target_para": e.target_para,
                "line": e.line,
                "kind": e.kind,
            })
        for cb in copies:
            copybooks_out.append({
                "copybook_id": f"{file_id}:{cb.line}:{cb.copybook_name}",
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

    # Write JSONL
    write_jsonl_local(jsonl_dir, "files.jsonl", files_out)
    write_jsonl_local(jsonl_dir, "paragraphs.jsonl", paras_out)
    write_jsonl_local(jsonl_dir, "data_items.jsonl", items_out)
    write_jsonl_local(jsonl_dir, "procedure_facts.jsonl", facts_out)
    write_jsonl_local(jsonl_dir, "xrefs.jsonl", xrefs_out)
    write_jsonl_local(jsonl_dir, "calls.jsonl", calls_out)
    write_jsonl_local(jsonl_dir, "chunks.jsonl", chunks_out)
    write_jsonl_local(jsonl_dir, "flow_edges.jsonl", flow_edges_out)
    write_jsonl_local(jsonl_dir, "copybooks.jsonl", copybooks_out)

    print(f"Done. Wrote JSONL to: {jsonl_dir}")

# -------------------------------
# Azure Blob mode
# -------------------------------

def get_blob_service(connection_string: Optional[str], account_url: Optional[str]):
    if BlobServiceClient is None:
        raise RuntimeError("azure-storage-blob is not installed. pip install azure-storage-blob")
    if connection_string:
        return BlobServiceClient.from_connection_string(connection_string)
    if account_url:
        return BlobServiceClient(account_url=account_url)
    env = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
    if not env:
        raise RuntimeError("Set AZURE_STORAGE_CONNECTION_STRING or pass --connection-string/--account-url")
    return BlobServiceClient.from_connection_string(env)

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
    payload = "\n".join(json.dumps(r, ensure_ascii=False) for r in rows)
    bc.upload_blob(payload.encode("utf-8"), overwrite=True)
    print(f"Wrote {filename} -> {out_path}")

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
        print(f"Processed {blob.name}: paras={len(paras)} items={len(items)} facts={len(facts)} xrefs={len(xrefs)} chunks={len(chunks)}")

        files_out.append({
            "file_id": fr.file_id,
            "path": fr.path,
            "name": os.path.basename(fr.path),
            "program_id": fr.program_id,
            "lines": len(raw),
            "format": fr.fmt,
            "procedure_using": fr.procedure_using,
        })
        for p in paras:
            paras_out.append({
                "para_id": f"{file_id}:{p.name}",
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
                "name": it.name,
                "qualified_name": it.qualified_name,
                "level": it.level,
                "section": it.section,
                "pic": it.pic,
                "usage": it.usage,
                "occurs_low": it.occurs_low,
                "occurs_high": it.occurs_high,
                "depends_on": it.depends_on,
                "redefines": it.redefines,
                "value": it.value,
                "start_line": it.start_line,
                "end_line": it.end_line,
            })
        for f in facts:
            facts_out.append({
                "fact_id": f"{file_id}:{f.line}:{f.kind}",
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
                "edge_id": f"{file_id}:{e.line}:{e.caller_para}:{e.target_para}",
                "file_id": e.file_id,
                "caller_para": e.caller_para,
                "target_para": e.target_para,
                "line": e.line,
                "kind": e.kind,
            })
        for cb in copies:
            copybooks_out.append({
                "copybook_id": f"{file_id}:{cb.line}:{cb.copybook_name}",
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

    # Write to Azure <prefix>/JSONL/
    jsonl_prefix = prefix.rstrip('/')
    write_jsonl_to_blob(cc, jsonl_prefix, "files.jsonl", files_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "paragraphs.jsonl", paras_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "data_items.jsonl", items_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "procedure_facts.jsonl", facts_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "xrefs.jsonl", xrefs_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "calls.jsonl", calls_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "chunks.jsonl", chunks_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "flow_edges.jsonl", flow_edges_out)
    write_jsonl_to_blob(cc, jsonl_prefix, "copybooks.jsonl", copybooks_out)

    print(f"Done. Wrote JSONL to: {jsonl_prefix}/JSONL/ in container {container}")

# -------------------------------
# CLI
# -------------------------------

def parse_args():
    ap = argparse.ArgumentParser(description="COBOL → JSONL extractor (Azure + Local)")
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--local', metavar='DIR', help='Process a local folder recursively')
    g.add_argument('--azure', action='store_true', help='Process from Azure Blob Storage')

    ap.add_argument('--container', help='Azure container name (required with --azure)')
    ap.add_argument('--prefix', help='Azure prefix (folder) inside container (required with --azure)')
    ap.add_argument('--connection-string', help='Azure Storage connection string (optional, else env)')
    ap.add_argument('--account-url', help='Azure Storage account URL (optional alternative)')
    ap.add_argument('--limit', type=int, help='Only process first N blobs (debug)')
    return ap.parse_args()

if __name__ == '__main__':
    args = parse_args()
    if args.local:
        run_local(args.local)
    else:
        if not args.container or not args.prefix:
            raise SystemExit('--container and --prefix are required with --azure')
        run_azure(args.container, args.prefix, args.connection_string, args.account_url, args.limit)
