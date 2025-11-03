
# Local file processing version of process_file
def process_file(file_path: str, file_id: str):
    text = read_file_text(file_path)
    raw_lines, norm_lines = normalize_cobol(text)
    fmt = detect_format(raw_lines)
    # Debug: print first 10 lines of the file
    print("  First 10 lines of file:")
    for idx, l in enumerate(raw_lines[:10], 1):
        print(f"    {idx:2}: {l.rstrip()}")
    print(f"  Detected format: {fmt}")

    # Debug: check for program id
    found_prog_id = False
    for i, line in enumerate(norm_lines, start=1):
        if RE_PROGRAM_ID.search(line):
            print(f"  Program ID matched on line {i}: {line.strip()}")
            found_prog_id = True
            break
    if not found_prog_id:
        print("  No PROGRAM-ID matched in file.")

    # Debug: check for paragraphs and data items
    para_count = 0
    dataitem_count = 0
    for i, line in enumerate(norm_lines, start=1):
        if RE_PARAGRAPH.match(line):
            print(f"  Paragraph matched on line {i}: {line.strip()}")
            para_count += 1
        if RE_DATA_ITEM.match(line):
            print(f"  Data item matched on line {i}: {line.strip()}")
            dataitem_count += 1
    if para_count == 0:
        print("  No paragraphs matched in file.")
    if dataitem_count == 0:
        print("  No data items matched in file.")

    # Program id and division boundaries
    program_id = None
    data_div_start = None
    proc_div_start = None
    procedure_using = None

    for i, line in enumerate(norm_lines, start=1):
        if RE_PROGRAM_ID.search(line) and program_id is None:
            m = RE_PROGRAM_ID.search(line)
            if m:
                program_id = m.group(1)
        if data_div_start is None and RE_DATA_DIV.search(line):
            data_div_start = i
        if proc_div_start is None and RE_PROC_DIV.search(line):
            proc_div_start = i
            # Check if it has USING clause
            using_match = RE_PROC_DIV_USING.search(line)
            if using_match:
                using_raw = using_match.group(1).strip()
                # Extract data names from USING clause
                procedure_using = []
                for token in re.findall(r"[A-Z0-9\-]+", using_raw, flags=re.IGNORECASE):
                    procedure_using.append(token.upper())

    file_rec = FileRec(file_id=file_id, path=file_path, program_id=program_id, fmt=fmt, procedure_using=procedure_using)

    # Paragraphs and sections
    paragraphs: List[ParagraphRec] = []
    paragraph_stack: List[Tuple[str, int]] = []
    section_name = None
    section_start = None
    for i, line in enumerate(norm_lines, start=1):
        sm = RE_SECTION.match(line)
        if sm:
            # close prior section
            if section_name is not None and section_start is not None:
                paragraphs.append(ParagraphRec(file_id, section_name, "section", section_start, i - 1))
            section_name = sm.group(1).upper()
            section_start = i
            # close any open paragraph
            if paragraph_stack:
                name, start = paragraph_stack.pop()
                paragraphs.append(ParagraphRec(file_id, name, "paragraph", start, i - 1))
            continue
        pm = RE_PARAGRAPH.match(line)
        if pm:
            # close prior paragraph
            if paragraph_stack:
                name, start = paragraph_stack.pop()
                paragraphs.append(ParagraphRec(file_id, name, "paragraph", start, i - 1))
            paragraph_stack.append((pm.group(1).upper(), i))

    # close any open sections or paragraphs
    if paragraph_stack:
        name, start = paragraph_stack.pop()
        paragraphs.append(ParagraphRec(file_id, name, "paragraph", start, len(norm_lines)))
    if section_name is not None and section_start is not None:
        paragraphs.append(ParagraphRec(file_id, section_name, "section", section_start, len(norm_lines)))

    # Data items (in DATA DIVISION if we found it)
    data_items: List[DataItemRec] = []
    current_section = "UNKNOWN"
    parents_by_level: Dict[int, str] = {}

    for i, line in enumerate(norm_lines, start=1):
        if RE_STORAGE_SECTION.match(line):
            current_section = RE_STORAGE_SECTION.match(line).group(1).upper()
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

            # parent chain by level numbers
            # remove deeper parents
            for lvl in list(parents_by_level.keys()):
                if lvl >= level:
                    del parents_by_level[lvl]
            parent_q = parents_by_level.get(max(parents_by_level.keys(), default=0), None)
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

    # Procedure facts (from statements)
    proc_facts: List[ProcFactRec] = []
    xrefs: List[XrefRec] = []

    # map simple name to possible qualified names for xref resolution
    qmap: Dict[str, List[str]] = {}
    for di in data_items:
        qmap.setdefault(di.name, []).append(di.qualified_name)
    
    # Create sets for tracking LINKAGE items and USING parameters for direction analysis
    linkage_items = set()
    using_params = set()
    for di in data_items:
        if di.section == "LINKAGE":
            linkage_items.add(di.name)
            linkage_items.add(di.qualified_name)
    
    if procedure_using:
        using_params.update(procedure_using)
    
    # Track which LINKAGE/USING items are written to (for param_out vs param_in)
    written_items = set()

    def get_direction(simple_name: str, qualified_name: str, operation_kind: str) -> str:
        """Determine the direction based on item type and operation"""
        # Check if it's a LINKAGE item or USING parameter
        is_linkage = simple_name in linkage_items or qualified_name in linkage_items
        is_using = simple_name in using_params
        
        if is_linkage or is_using:
            if operation_kind == "write":
                written_items.add(simple_name)
                written_items.add(qualified_name)
                return "param_out"
            else:  # read or param
                return "param_in"
        else:
            return operation_kind  # regular read/write

    def compute_column_positions(simple_name: str, snippet: str) -> Tuple[Optional[int], Optional[int]]:
        """Compute start and end column positions for an identifier occurrence within a snippet"""
        if not simple_name or not snippet:
            return None, None
        
        # Find the identifier in the snippet (case-insensitive)
        import re
        pattern = r'\b' + re.escape(simple_name) + r'\b'
        match = re.search(pattern, snippet, re.IGNORECASE)
        if match:
            start_col = match.start() + 1  # 1-based column numbering
            end_col = match.end()  # end is exclusive in match, so this gives us the position after the last character
            return start_col, end_col
        return None, None

    def create_xref(qualified_name: str, simple_name: str, kind: str, line: int, snippet: str, direction: str) -> XrefRec:
        """Create an XrefRec with all fields populated including path, program_id, and column positions"""
        start_col, end_col = compute_column_positions(simple_name, snippet)
        
        return XrefRec(
            file_id=file_id,
            qualified_name=qualified_name,
            simple_name=simple_name,
            kind=kind,
            line=line,
            snippet=snippet,
            direction=direction,
            path=file_path,
            program_id=program_id,
            start_col=start_col,
            end_col=end_col
        )

    statements = split_statements(norm_lines)
    for end_line, stmt in statements:
        s = stmt.strip()
        if not s:
            continue
        # PERFORM
        m = RE_PERFORM.match(s)
        if m:
            proc_facts.append(ProcFactRec(file_id, "perform", end_line, s, para=m.group(1).upper(), thru=(m.group(2).upper() if m.group(2) else None)))
            continue
        # CALL
        m = RE_CALL.match(s)
        if m:
            callee = m.group(1).upper()
            using_raw = (m.group(2) or "").strip()
            is_dynamic = False
            proc_facts.append(ProcFactRec(file_id, "call", end_line, s, callee=callee, is_dynamic=is_dynamic, using_raw=using_raw))
            if using_raw:
                for token in re.findall(r"[A-Z0-9\-]+", using_raw, flags=re.IGNORECASE):
                    t_up = token.upper()
                    if t_up in qmap:
                        for qn in qmap[t_up]:
                            direction = get_direction(t_up, qn, "param")
                            xrefs.append(create_xref(qn, t_up, "param", end_line, s, direction))
            continue
        # MOVE
        m = RE_MOVE.match(s)
        if m:
            src = m.group(1).strip()
            tgt = m.group(2).upper()
            proc_facts.append(ProcFactRec(file_id, "move", end_line, s, target=tgt, source_raw=src))
            if tgt in qmap:
                for qn in qmap[tgt]:
                    direction = get_direction(tgt, qn, "write")
                    xrefs.append(create_xref(qn, tgt, "write", end_line, s, direction))
            for token in re.findall(r"[A-Z0-9\-]+", src, flags=re.IGNORECASE):
                t_up = token.upper()
                if t_up in qmap:
                    for qn in qmap[t_up]:
                        direction = get_direction(t_up, qn, "read")
                        xrefs.append(create_xref(qn, t_up, "read", end_line, s, direction))
            continue
        # COMPUTE
        m = RE_COMPUTE.match(s)
        if m:
            tgt = m.group(1).upper()
            expr = m.group(2).strip()
            proc_facts.append(ProcFactRec(file_id, "compute", end_line, s, target=tgt, expr_raw=expr))
            if tgt in qmap:
                for qn in qmap[tgt]:
                    direction = get_direction(tgt, qn, "write")
                    xrefs.append(create_xref(qn, tgt, "write", end_line, s, direction))
            for token in re.findall(r"[A-Z0-9\-]+", expr, flags=re.IGNORECASE):
                t_up = token.upper()
                if t_up in qmap:
                    for qn in qmap[t_up]:
                        direction = get_direction(t_up, qn, "read")
                        xrefs.append(create_xref(qn, t_up, "read", end_line, s, direction))
            continue
        # READ INTO
        m = RE_READ_INTO.match(s)
        if m:
            into = m.group(2).upper() if m.group(2) else None
            if into:
                proc_facts.append(ProcFactRec(file_id, "read", end_line, s, target=into))
                if into in qmap:
                    for qn in qmap[into]:
                        direction = get_direction(into, qn, "write")
                        xrefs.append(create_xref(qn, into, "write", end_line, s, direction))
            continue
        # ACCEPT
        m = RE_ACCEPT.match(s)
        if m:
            tgt = m.group(1).upper()
            proc_facts.append(ProcFactRec(file_id, "accept", end_line, s, target=tgt))
            if tgt in qmap:
                for qn in qmap[tgt]:
                    direction = get_direction(tgt, qn, "write")
                    xrefs.append(create_xref(qn, tgt, "write", end_line, s, direction))
            continue

    # Chunks: paragraph chunks plus level 01 data groups
    chunks: List[ChunkRec] = []
    para_map: Dict[str, Tuple[int, int]] = {}
    for p in paragraphs:
        if p.kind == "paragraph":
            para_map[p.name] = (p.start_line, p.end_line)
            text_block = "\n".join(norm_lines[p.start_line - 1:p.end_line])
            chunks.append(ChunkRec(
                chunk_id=f"{file_id}:{p.name}:{p.start_line}-{p.end_line}",
                file_id=file_id,
                path=file_path,
                program_id=program_id,
                scope="paragraph",
                name=p.name,
                start_line=p.start_line,
                end_line=p.end_line,
                text=text_block,
            ))
    # Data group chunks for level 01
    for di in data_items:
        if di.level == 1:
            start = di.start_line
            end = start
            for dj in data_items:
                if dj.start_line > di.start_line and dj.level == 1:
                    end = dj.start_line - 1
                    break
            if end == start:
                end = di.end_line
            text_block = "\n".join(norm_lines[start - 1:end])
            chunks.append(ChunkRec(
                chunk_id=f"{file_id}:{di.name}:{start}-{end}",
                file_id=file_id,
                path=file_path,
                program_id=program_id,
                scope="data-group",
                name=di.name,
                start_line=start,
                end_line=end,
                text=text_block,
            ))

    return file_rec, paragraphs, data_items, proc_facts, xrefs, chunks
#!/usr/bin/env python3
"""
Azure COBOL Extractor to JSONL

Scans an Azure Blob Storage folder (including subfolders) for COBOL source files
(.cbl, .cob, .cobol), performs lightweight COBOL-aware extraction, and writes
JSONL outputs back to the same container under `<prefix>/JSONL/`:

- files.jsonl              One per COBOL file (path, program_id, format)
- paragraphs.jsonl         Paragraph and section spans (name, start_line, end_line)
- data_items.jsonl         Data item definitions from DATA DIVISION
- procedure_facts.jsonl    Facts from PROCEDURE DIVISION (PERFORM, CALL, MOVE, COMPUTE, READ, ACCEPT)
- xrefs.jsonl              Read/write/param occurrences for data items
- calls.jsonl              Caller to callee program edges
- chunks.jsonl             Paragraph and data-group text chunks for retrieval

Auth: uses AZURE_STORAGE_CONNECTION_STRING env var by default

Usage:
  export AZURE_STORAGE_CONNECTION_STRING="<connection string>"
  python cobol_to_jsonl_azure.py --container mycontainer --prefix path/in/container

Tip: You can run safely multiple times. Uploads overwrite the JSONL files.
"""
import argparse
import json
import os
import re
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple
from azure.storage.blob import BlobServiceClient


# Local helpers for file operations
def find_cobol_files(root_dir: str) -> list:
    cobol_files = []
    for dirpath, _, filenames in os.walk(root_dir):
        for fname in filenames:
            ext = os.path.splitext(fname)[1].lower()
            if ext in COBOL_EXTS:
                cobol_files.append(os.path.join(dirpath, fname))
    return cobol_files

def read_file_text(path: str) -> str:
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        return f.read()

def write_jsonl_local(jsonl_dir: str, filename: str, rows: list) -> None:
    out_path = os.path.join(jsonl_dir, filename)
    with open(out_path, 'w', encoding='utf-8') as f:
        for r in rows:
            f.write(json.dumps(r, ensure_ascii=False) + '\n')
    print(f"Wrote {filename} -> {out_path}")

# -------------------------------
# Helpers and data structures
# -------------------------------

COBOL_EXTS = {".cbl", ".cob", ".cobol", ".cpy"}

@dataclass
class FileRec:
    file_id: str
    path: str
    program_id: Optional[str]
    fmt: str  # "fixed" or "free" (best effort)
    procedure_using: Optional[List[str]] = None

@dataclass
class ParagraphRec:
    file_id: str
    name: str
    kind: str  # "paragraph" or "section"
    start_line: int
    end_line: int

@dataclass
class DataItemRec:
    file_id: str
    name: str
    qualified_name: str
    level: int
    section: str  # WORKING, LOCAL, LINKAGE, or UNKNOWN
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
    kind: str  # perform, call, move, compute, read, accept, param
    line: int
    snippet: str
    # Optional fields depending on kind
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
    kind: str  # read or write or call or param
    line: int
    snippet: str
    direction: Optional[str] = None  # read, write, param_in, param_out
    path: Optional[str] = None
    program_id: Optional[str] = None
    start_col: Optional[int] = None
    end_col: Optional[int] = None

@dataclass
class ChunkRec:
    chunk_id: str
    file_id: str
    path: str
    program_id: Optional[str]
    scope: str  # paragraph or data-group
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
    kind: str  # "perform"

@dataclass
class CopybookRec:
    file_id: str
    parent_path: str
    copybook_name: str
    line: int
    replacing_clause: Optional[str] = None

# Regex library (case-insensitive)
RE_IDENT_DIV = re.compile(r"^\s*IDENTIFICATION\s+DIVISION\.", re.IGNORECASE)
RE_PROGRAM_ID = re.compile(r"^\s*PROGRAM-ID\.\s+([A-Z0-9\-]+)", re.IGNORECASE)
RE_DATA_DIV = re.compile(r"^\s*DATA\s+DIVISION\.", re.IGNORECASE)
RE_PROC_DIV = re.compile(r"^\s*PROCEDURE\s+DIVISION(?:\s+USING\b.*)?\.", re.IGNORECASE)
RE_PROC_DIV_USING = re.compile(r"^\s*PROCEDURE\s+DIVISION\s+USING\s+(.*?)\.", re.IGNORECASE)
RE_STORAGE_SECTION = re.compile(r"^\s*(WORKING-STORAGE|LOCAL-STORAGE|LINKAGE)\s+SECTION\.", re.IGNORECASE)
RE_SECTION = re.compile(r"^\s*([A-Z0-9\-]+)-SECTION\.", re.IGNORECASE)
RE_PARAGRAPH = re.compile(r"^\s*([A-Z0-9\-]+)\.$", re.IGNORECASE)

# Data items (tolerant pattern)
RE_DATA_ITEM = re.compile(
    r"^\s*(01|02|03|04|05|10|77)\s+([A-Z0-9\-]+)"  # level and name
    r"(?:\s+REDEFINES\s+([A-Z0-9\-]+))?"           # redefines
    r"(?:\s+OCCURS\s+(\d+)(?:\s+TO\s+(\d+))?\s+TIMES(?:\s+DEPENDING\s+ON\s+([A-Z0-9\-]+))?)?"  # occurs
    r"(?:\s+PIC\s+([A-Z0-9VXS\(\)\.\+\/\-]+))?"  # pic - fixed to properly handle parentheses
    r"(?:\s+USAGE\s+([A-Z0-9\-]+))?"               # usage
    r"(?:\s+VALUE\s+(.+?))?\s*\.\s*$",
    re.IGNORECASE,
)

# Procedure facts
RE_PERFORM = re.compile(r"^\s*PERFORM\s+([A-Z0-9\-]+)(?:\s+THRU\s+([A-Z0-9\-]+))?\s*\.\s*$", re.IGNORECASE)
RE_CALL = re.compile(r"^\s*CALL\s+['\"]?([A-Z0-9\-]+)['\"]?\s*(?:USING\s+(.*?))?\s*\.\s*$", re.IGNORECASE)
RE_MOVE = re.compile(r"^\s*MOVE\s+(.+?)\s+TO\s+([A-Z0-9\-]+)\s*\.\s*$", re.IGNORECASE)
RE_COPY = re.compile(r"^\s*COPY\s+([A-Z0-9\-]+)(?:\s+REPLACING\s+(.*?))?\s*\.\s*$", re.IGNORECASE)
RE_COMPUTE = re.compile(r"^\s*COMPUTE\s+([A-Z0-9\-]+)\s*=\s*(.+?)\s*\.\s*$", re.IGNORECASE)
RE_READ_INTO = re.compile(r"^\s*READ\s+([A-Z0-9\-]+)(?:\s+INTO\s+([A-Z0-9\-]+))?", re.IGNORECASE)
RE_ACCEPT = re.compile(r"^\s*ACCEPT\s+([A-Z0-9\-]+)", re.IGNORECASE)

# Column handling markers (fixed format)
COMMENT_COL7 = {"*", "/"}  # column 7 comment markers in fixed format
CONT_MARK = "-"  # column 7 continuation marker for literals

# -------------------------------
# Azure Blob helpers
# -------------------------------

def load_azure_credentials():
    """Load Azure Storage credentials from local.settings.json"""
    settings_path = "c:/Users/jeff.childers/Documents/OTISCodeResearcher/local.settings.json"
    try:
        with open(settings_path, 'r') as f:
            settings = json.load(f)
            connection_string = settings.get('Values', {}).get('AzureWebJobsStorage')
            if not connection_string:
                raise ValueError("AzureWebJobsStorage not found in local.settings.json")
            return connection_string
    except FileNotFoundError:
        raise FileNotFoundError(f"Could not find local.settings.json at {settings_path}")
    except Exception as e:
        raise Exception(f"Error loading Azure credentials: {e}")

def iter_cobol_blobs(cc, prefix: str) -> Iterable:
    prefix = prefix.lstrip("/") if prefix else ""
    for blob in cc.list_blobs(name_starts_with=prefix):
        name = blob.name
        # Skip any files in S35JSON folders (check if S35JSON appears anywhere in the path)
        if "S35JSON" in name:
            continue
        # Only process files (not directories) with COBOL extensions
        if blob.size > 0:  # Ensure it's a file, not a directory marker
            ext = os.path.splitext(name)[1].lower()
            if ext in COBOL_EXTS:
                yield blob


def download_blob_text(cc, blob_name: str) -> str:
    blob_client = cc.get_blob_client(blob=blob_name)
    return blob_client.download_blob().readall().decode("utf-8", errors="replace")


def write_jsonl_to_blob(cc, prefix: str, filename: str, rows: List[dict]) -> None:
    out_path = f"{prefix.rstrip('/')}/{filename}"
    blob_client = cc.get_blob_client(blob=out_path)
    payload = "\n".join(json.dumps(r, ensure_ascii=False) for r in rows)
    blob_client.upload_blob(payload.encode("utf-8"), overwrite=True)
    print(f"Wrote {filename} -> {out_path}")

# -------------------------------
# COBOL normalization and parsing
# -------------------------------

def detect_format(lines: List[str]) -> str:
    """Best effort detect fixed vs free format."""
    # If many lines look like they have sequence numbers and a period in area B, assume fixed
    fixed_like = 0
    for ln in lines[:200]:
        if len(ln) >= 7:
            col7 = ln[6]
            if ln[:6].strip() and (col7 in COMMENT_COL7 or col7 == " " or col7 == CONT_MARK):
                fixed_like += 1
    return "fixed" if fixed_like > 20 else "free"


def strip_seq_and_cols(line: str, fmt: str) -> str:
    if fmt == "fixed":
        # Columns 1-6 sequence, 7 indicator, content starts at 8 (index 7)
        if len(line) >= 7:
            return line[6:72]  # keep up to 72 (area B). COBOL often uses 73-80 for IDs
        return line
    return line


def is_comment_fixed(line: str) -> bool:
    return len(line) >= 7 and line[6] in COMMENT_COL7


def normalize_cobol(text: str) -> Tuple[List[str], List[str]]:
    """Return (raw_lines, norm_lines). raw_lines are 1-based original lines.
    norm_lines are content after stripping fixed columns and trimming trailing newlines.
    We do not join continuation physically, but parse statement wise later.
    """
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
    """Split on periods that end COBOL statements. Returns list of (end_line_number, statement_text)."""
    stmts: List[Tuple[int, str]] = []
    buf: List[str] = []
    for idx, line in enumerate(norm_lines, start=1):
        # naive literal handling: this is a light extractor, so split by '.' at end of line primarily
        buf.append(line)
        if line.strip().endswith('.'):
            stmt = " ".join(x.rstrip() for x in buf).strip()
            stmts.append((idx, stmt))
            buf = []
    # leftover (no trailing period): still emit
    if buf:
        stmts.append((len(norm_lines), " ".join(x.rstrip() for x in buf).strip()))
    return stmts

# -------------------------------
# Main entry
# -------------------------------

def main():
    # Process COBOL files from Azure Blob Storage by default
    print("Processing COBOL files from Azure Blob Storage...")
    process_azure_cobol_files()
    return

def main_local():
    # Local processing (kept for reference)
    # Ask user for the full path to the directory
    dir_path = input("Enter the full path to the directory: ").strip()
    print(f"Scanning for COBOL files in: {dir_path}")
    cobol_files = find_cobol_files(dir_path)
    print(f"Found {len(cobol_files)} COBOL files:")
    for f in cobol_files:
        print(f"  {f}")

    jsonl_dir = os.path.join(dir_path, "JSONL")
    if not os.path.exists(jsonl_dir):
        os.makedirs(jsonl_dir)
        print(f"Created JSONL folder at: {jsonl_dir}")
    else:
        print(f"JSONL folder already exists at: {jsonl_dir}")


    files_out: List[dict] = []
    paras_out: List[dict] = []
    items_out: List[dict] = []
    facts_out: List[dict] = []
    xrefs_out: List[dict] = []
    chunks_out: List[dict] = []

    i = 0
    for file_path in cobol_files:
        i += 1
        file_id = f"f{i:06d}"
        print(f"\nProcessing {file_path} -> {file_id}")
        file_rec, paras, data_items, facts, xrefs, chunks = process_file(file_path, file_id)

        print(f"  Extracted: {len(paras)} paragraphs, {len(data_items)} data items, {len(facts)} facts, {len(xrefs)} xrefs, {len(chunks)} chunks")

        files_out.append({
            "file_id": file_rec.file_id,
            "path": file_rec.path,
            "program_id": file_rec.program_id,
            "format": file_rec.fmt,
            "procedure_using": file_rec.procedure_using,
        })
        for p in paras:
            paras_out.append({
                "file_id": p.file_id,
                "name": p.name,
                "kind": p.kind,
                "start_line": p.start_line,
                "end_line": p.end_line,
            })
        for di in data_items:
            items_out.append({
                "file_id": di.file_id,
                "name": di.name,
                "qualified_name": di.qualified_name,
                "level": di.level,
                "section": di.section,
                "pic": di.pic,
                "usage": di.usage,
                "occurs_low": di.occurs_low,
                "occurs_high": di.occurs_high,
                "depends_on": di.depends_on,
                "redefines": di.redefines,
                "value": di.value,
                "start_line": di.start_line,
                "end_line": di.end_line,
            })
        for f in facts:
            facts_out.append({
                "file_id": f.file_id,
                "kind": f.kind,
                "line": f.line,
                "snippet": f.snippet,
                "para": f.para,
                "thru": f.thru,
                "callee": f.callee,
                "is_dynamic": f.is_dynamic,
                "using_raw": f.using_raw,
                "target": f.target,
                "source_raw": f.source_raw,
                "expr_raw": f.expr_raw,
            })
        for xr in xrefs:
            xrefs_out.append({
                "file_id": xr.file_id,
                "qualified_name": xr.qualified_name,
                "simple_name": xr.simple_name,
                "kind": xr.kind,
                "line": xr.line,
                "snippet": xr.snippet,
                "direction": xr.direction,
                "path": xr.path,
                "program_id": xr.program_id,
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

    # Derive calls from facts
    calls_out: List[dict] = []
    for rec in facts_out:
        if rec.get("kind") == "call":
            calls_out.append({
                "call_id": f"{rec['file_id']}:{rec['line']}",
                "caller_file_id": rec["file_id"],
                "caller_path": next((f["path"] for f in files_out if f["file_id"] == rec["file_id"]), None),
                "caller_para": rec.get("para"),
                "callee_program": rec.get("callee"),
                "is_dynamic": rec.get("is_dynamic"),
                "using_raw": rec.get("using_raw"),
                "line": rec["line"],
            })

    # Write JSONL to local JSONL directory
    write_jsonl_local(jsonl_dir, "files.jsonl", files_out)
    write_jsonl_local(jsonl_dir, "paragraphs.jsonl", paras_out)
    write_jsonl_local(jsonl_dir, "data_items.jsonl", items_out)
    write_jsonl_local(jsonl_dir, "procedure_facts.jsonl", facts_out)
    write_jsonl_local(jsonl_dir, "xrefs.jsonl", xrefs_out)
    write_jsonl_local(jsonl_dir, "calls.jsonl", calls_out)
    write_jsonl_local(jsonl_dir, "chunks.jsonl", chunks_out)

    print("Done.")

def process_blob_content_full(blob_name: str, blob_content: str, file_id: str):
    """Process COBOL content from a blob and return structured data like process_file"""
    try:
        raw_lines, norm_lines = normalize_cobol(blob_content)
        fmt = detect_format(raw_lines)
        
        # Debug: print first 10 lines of the blob
        print(f"  First 10 lines of blob {blob_name}:")
        for idx, l in enumerate(raw_lines[:10], 1):
            print(f"    {idx:2}: {l.rstrip()}")
        print(f"  Detected format: {fmt}")

        # Find program ID
        found_prog_id = None
        for i, line in enumerate(norm_lines, start=1):
            if "PROGRAM-ID" in line:
                # Extract program name from PROGRAM-ID line
                parts = line.split()
                if len(parts) >= 2:
                    found_prog_id = parts[1].rstrip('.')
                print(f"  Found PROGRAM-ID at line {i}: {found_prog_id}")
                break
        
        if not found_prog_id:
            print(f"  Warning: No PROGRAM-ID found in {blob_name}")
            found_prog_id = os.path.basename(blob_name).replace('.CBL', '')

        # Parse PROCEDURE DIVISION USING parameters
        procedure_using = None
        for i, line in enumerate(norm_lines, start=1):
            if RE_PROC_DIV.search(line):
                # Check if it has USING clause
                using_match = RE_PROC_DIV_USING.search(line)
                if using_match:
                    using_raw = using_match.group(1).strip()
                    # Extract data names from USING clause
                    procedure_using = []
                    for token in re.findall(r"[A-Z0-9\-]+", using_raw, flags=re.IGNORECASE):
                        procedure_using.append(token.upper())
                break

        # Create file record
        file_rec = FileRec(
            file_id=file_id,
            path=blob_name,
            program_id=found_prog_id,
            fmt=fmt,
            procedure_using=procedure_using if procedure_using else None
        )

        # Call the existing parsing functions that process_file uses
        # Find program ID and sections
        prog_id_line = None
        for i, line in enumerate(norm_lines, start=1):
            if "PROGRAM-ID" in line:
                prog_id_line = i
                break

        # Data items parsing with enhanced section detection
        data_items = []
        current_section = "UNKNOWN"
        for i, line in enumerate(norm_lines, start=1):
            stripped = line.strip().upper()
            
            # Detect section changes
            if stripped.endswith("SECTION."):
                if "WORKING-STORAGE" in stripped:
                    current_section = "WORKING-STORAGE"
                elif "LINKAGE" in stripped:
                    current_section = "LINKAGE"
                elif "LOCAL-STORAGE" in stripped:
                    current_section = "LOCAL-STORAGE"
                elif "FILE" in stripped:
                    current_section = "FILE"
                else:
                    current_section = stripped
                continue
            
            # Simple data item detection (level numbers)
            original_line = line.strip()
            if original_line and original_line[0].isdigit():
                parts = original_line.split()
                if len(parts) >= 2:
                    level = parts[0]
                    name = parts[1]
                    
                    # Extract PIC clause if present
                    pic = None
                    if "PIC" in original_line.upper():
                        pic_match = re.search(r'PIC\s+([^\s.]+)', original_line, re.IGNORECASE)
                        if pic_match:
                            pic = pic_match.group(1)
                    
                    data_items.append(DataItemRec(
                        file_id=file_id,
                        name=name,
                        qualified_name=name,
                        level=level,
                        section=current_section,
                        pic=pic,
                        usage=None,
                        occurs_low=None,
                        occurs_high=None,
                        depends_on=None,
                        redefines=None,
                        value=None,
                        start_line=i,
                        end_line=i,
                    ))

        # Paragraph parsing - improved approach using regex
        paragraphs = []
        current_para = None
        
        for i, line in enumerate(norm_lines, start=1):
            stripped = line.strip()
            if not stripped or stripped.startswith('*'):
                continue
                
            # Check for section (ends with -SECTION.)
            if RE_SECTION.match(stripped):
                if current_para:
                    current_para.end_line = i - 1
                    paragraphs.append(current_para)
                
                section_name = RE_SECTION.match(stripped).group(1) + "-SECTION"
                current_para = ParagraphRec(
                    file_id=file_id,
                    name=section_name,
                    kind="section",
                    start_line=i,
                    end_line=i,
                )
                continue
            
            # Check for paragraph (simple name ending with .)
            if RE_PARAGRAPH.match(stripped):
                if current_para:
                    current_para.end_line = i - 1
                    paragraphs.append(current_para)
                
                para_name = RE_PARAGRAPH.match(stripped).group(1)
                current_para = ParagraphRec(
                    file_id=file_id,
                    name=para_name,
                    kind="paragraph",
                    start_line=i,
                    end_line=i,
                )
        
        if current_para:
            current_para.end_line = len(norm_lines)
            paragraphs.append(current_para)

        # Build fast lookup from line number to paragraph name
        def build_line_to_paragraph_lookup(paragraphs: List[ParagraphRec]) -> dict:
            """Build a fast lookup from line number to paragraph name using known paragraph spans."""
            line_to_para = {}
            for para in paragraphs:
                for line_num in range(para.start_line, para.end_line + 1):
                    line_to_para[line_num] = para.name
            return line_to_para
        
        line_to_para_lookup = build_line_to_paragraph_lookup(paragraphs)

        # Procedure facts - enhanced verb detection with proper paragraph tagging
        proc_facts = []
        for i, line in enumerate(norm_lines, start=1):
            stripped = line.strip()
            if stripped and not stripped.startswith('*'):
                # Look for COBOL verbs
                for verb in ['MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'IF', 'PERFORM', 'CALL', 'READ', 'WRITE', 'OPEN', 'CLOSE', 'ACCEPT', 'COMPUTE']:
                    if verb in stripped.upper():
                        # Get paragraph name for this line
                        para_name = line_to_para_lookup.get(i, None)
                        
                        # Extract additional info based on verb type
                        callee = None
                        callee_data_name = None
                        is_dynamic = None
                        target = None
                        
                        if verb == 'CALL':
                            # Check for static call (quoted literal) first
                            static_call_match = re.search(r'CALL\s+["\']([^"\']+)["\']', stripped, re.IGNORECASE)
                            if static_call_match:
                                # Static call - quoted literal
                                callee = static_call_match.group(1)
                                callee_data_name = None
                                is_dynamic = False
                            else:
                                # Check for dynamic call (unquoted data name)
                                dynamic_call_match = re.search(r'CALL\s+([A-Za-z][A-Za-z0-9\-]*)', stripped, re.IGNORECASE)
                                if dynamic_call_match:
                                    # Dynamic call - data name
                                    callee = None
                                    callee_data_name = dynamic_call_match.group(1).upper()
                                    is_dynamic = True
                        elif verb in ['MOVE', 'ADD', 'SUBTRACT']:
                            # Try to extract TO target
                            to_match = re.search(r'\sTO\s+(\w+)', stripped, re.IGNORECASE)
                            if to_match:
                                target = to_match.group(1)
                        
                        proc_facts.append(ProcFactRec(
                            file_id=file_id,
                            kind=verb.lower(),
                            line=i,
                            snippet=stripped[:100],  # Increased to 100 chars for better context
                            para=para_name,
                            callee=callee,
                            callee_data_name=callee_data_name,
                            is_dynamic=is_dynamic,
                            target=target,
                        ))
                        break

        # Cross-references with direction analysis
        xrefs = []
        
        # Create sets for tracking LINKAGE items and USING parameters for direction analysis
        linkage_items = set()
        using_params = set()
        for di in data_items:
            if di.section == "LINKAGE":
                linkage_items.add(di.name)
                linkage_items.add(di.qualified_name)
        
        if procedure_using:
            using_params.update(procedure_using)
        
        # Track which LINKAGE/USING items are written to (for param_out vs param_in)
        written_items = set()

        def get_direction(simple_name: str, qualified_name: str, operation_kind: str) -> str:
            """Determine the direction based on item type and operation"""
            # Check if it's a LINKAGE item or USING parameter
            is_linkage = simple_name in linkage_items or qualified_name in linkage_items
            is_using = simple_name in using_params
            
            if is_linkage or is_using:
                if operation_kind == "write":
                    written_items.add(simple_name)
                    written_items.add(qualified_name)
                    return "param_out"
                else:  # read or param
                    return "param_in"
            else:
                return operation_kind  # regular read/write
        
        def compute_column_positions(simple_name: str, snippet: str) -> Tuple[Optional[int], Optional[int]]:
            """Compute start_col and end_col for the first exact match of simple_name in snippet"""
            # Case-sensitive search for the simple_name in the snippet
            start_pos = snippet.find(simple_name)
            if start_pos == -1:
                return None, None
            
            # Convert to 1-based column positions
            start_col = start_pos + 1
            end_col = start_pos + len(simple_name)
            
            return start_col, end_col
        
        def create_xref(qualified_name: str, simple_name: str, kind: str, line: int, snippet: str, direction: str) -> XrefRec:
            """Create an XrefRec with all fields populated including path, program_id, and column positions"""
            start_col, end_col = compute_column_positions(simple_name, snippet)
            
            return XrefRec(
                file_id=file_id,
                qualified_name=qualified_name,
                simple_name=simple_name,
                kind=kind,
                line=line,
                snippet=snippet,
                direction=direction,
                path=blob_name,
                program_id=found_prog_id,
                start_col=start_col,
                end_col=end_col
            )
        
        # Build qmap for xref resolution
        qmap: Dict[str, List[str]] = {}
        for di in data_items:
            qmap.setdefault(di.name, []).append(di.qualified_name)
        
        # Parse statements to create proper xrefs
        statements = split_statements(norm_lines)
        for end_line, stmt in statements:
            s = stmt.strip()
            if not s:
                continue
            # CALL
            m = RE_CALL.match(s)
            if m:
                using_raw = (m.group(2) or "").strip()
                if using_raw:
                    for token in re.findall(r"[A-Z0-9\-]+", using_raw, flags=re.IGNORECASE):
                        t_up = token.upper()
                        if t_up in qmap:
                            for qn in qmap[t_up]:
                                direction = get_direction(t_up, qn, "param")
                                xrefs.append(create_xref(qn, t_up, "param", end_line, s, direction))
                continue
            # MOVE
            m = RE_MOVE.match(s)
            if m:
                src = m.group(1).strip()
                tgt = m.group(2).upper()
                if tgt in qmap:
                    for qn in qmap[tgt]:
                        direction = get_direction(tgt, qn, "write")
                        xrefs.append(create_xref(qn, tgt, "write", end_line, s, direction))
                for token in re.findall(r"[A-Z0-9\-]+", src, flags=re.IGNORECASE):
                    t_up = token.upper()
                    if t_up in qmap:
                        for qn in qmap[t_up]:
                            direction = get_direction(t_up, qn, "read")
                            xrefs.append(create_xref(qn, t_up, "read", end_line, s, direction))
                continue
            # COMPUTE
            m = RE_COMPUTE.match(s)
            if m:
                tgt = m.group(1).upper()
                expr = m.group(2).strip()
                if tgt in qmap:
                    for qn in qmap[tgt]:
                        direction = get_direction(tgt, qn, "write")
                        xrefs.append(create_xref(qn, tgt, "write", end_line, s, direction))
                for token in re.findall(r"[A-Z0-9\-]+", expr, flags=re.IGNORECASE):
                    t_up = token.upper()
                    if t_up in qmap:
                        for qn in qmap[t_up]:
                            direction = get_direction(t_up, qn, "read")
                            xrefs.append(create_xref(qn, t_up, "read", end_line, s, direction))

        # Flow edges - track PERFORM statement flows
        flow_edges = []
        
        # Build ordered list of paragraph names in file order with their spans
        para_order = []
        para_index_map = {}
        for i, para in enumerate(paragraphs):
            para_order.append(para.name)
            para_index_map[para.name] = i
        
        # Process PERFORM statements to create flow edges
        for i, line in enumerate(norm_lines, start=1):
            stripped = line.strip().upper()
            if stripped and not stripped.startswith('*'):
                # Get current paragraph for this line
                current_para = line_to_para_lookup.get(i, None)
                if not current_para:
                    continue
                
                # Check for PERFORM statements
                perform_match = RE_PERFORM.match(stripped)
                if perform_match:
                    para_name = perform_match.group(1)
                    thru_name = perform_match.group(2) if perform_match.group(2) else None
                    
                    if thru_name:
                        # PERFORM A THRU B - create edges from caller to every paragraph A through B inclusive
                        if para_name in para_index_map and thru_name in para_index_map:
                            start_idx = para_index_map[para_name]
                            end_idx = para_index_map[thru_name]
                            
                            # Ensure we go from start to end (handle cases where B comes before A)
                            if start_idx <= end_idx:
                                for idx in range(start_idx, end_idx + 1):
                                    target_para = para_order[idx]
                                    flow_edges.append(FlowEdgeRec(
                                        file_id=file_id,
                                        caller_para=current_para,
                                        target_para=target_para,
                                        line=i,
                                        kind="perform"
                                    ))
                    else:
                        # PERFORM PARA - create single edge from caller to target
                        if para_name in para_index_map:
                            flow_edges.append(FlowEdgeRec(
                                file_id=file_id,
                                caller_para=current_para,
                                target_para=para_name,
                                line=i,
                                kind="perform"
                            ))

        # Copybook processing - track COPY statements
        copybooks = []
        for i, line in enumerate(norm_lines, start=1):
            stripped = line.strip().upper()
            if stripped and not stripped.startswith('*'):
                # Check for COPY statements
                copy_match = RE_COPY.match(stripped)
                if copy_match:
                    copybook_name = copy_match.group(1)
                    replacing_clause = copy_match.group(2) if copy_match.group(2) else None
                    
                    copybooks.append(CopybookRec(
                        file_id=file_id,
                        parent_path=blob_name,
                        copybook_name=copybook_name,
                        line=i,
                        replacing_clause=replacing_clause
                    ))

        # Chunks - split into logical sections with COPY token awareness
        chunks = []
        chunk_size = 50  # lines per chunk
        
        # Build a set of line numbers that contain COPY statements for quick lookup
        copy_lines = set(cb.line for cb in copybooks)
        
        for start in range(0, len(raw_lines), chunk_size):
            end = min(start + chunk_size, len(raw_lines))
            chunk_text = '\n'.join(raw_lines[start:end])
            
            # Check if this chunk contains any COPY lines
            chunk_line_range = range(start + 1, end + 1)
            contains_copy = any(line_num in copy_lines for line_num in chunk_line_range)
            
            # If chunk contains COPY, the text already includes it unchanged
            # We just need to ensure the COPY text is preserved in chunk_text
            chunks.append(ChunkRec(
                chunk_id=f"{file_id}:chunk_{start + 1}",
                file_id=file_id,
                path=blob_name,
                program_id=found_prog_id,
                scope="program",
                name=f"lines_{start + 1}_{end}",
                start_line=start + 1,
                end_line=end,
                text=chunk_text,
            ))

        # Full modern tuple (9 values)
        full_tuple = (file_rec, paragraphs, data_items, proc_facts, xrefs, chunks, flow_edges, copybooks, raw_lines)
        # Backward-compat shim: some legacy tests expect 7 values (without flow_edges, copybooks)
        # Detect by inspecting caller stack could be overkill; instead offer attribute for tests to opt-in.
        if os.environ.get("COBOLPARSER_LEGACY7") == "1":
            return file_rec, paragraphs, data_items, proc_facts, xrefs, chunks, raw_lines
        return full_tuple
        
    except Exception as e:
        print(f"  Error processing blob content {blob_name}: {e}")
        return None, [], [], [], [], [], [], [], []

def process_azure_cobol_files():
    """Process .CBL files from Azure Blob Storage in the aisearch container's S35-source folder"""
    # Load Azure credentials
    try:
        connection_string = load_azure_credentials()
    except Exception as e:
        print(f"Error loading Azure credentials: {e}")
        return
    
    try:
        # Create blob service client
        blob_service_client = BlobServiceClient.from_connection_string(connection_string)
        container_name = "aisearch"
        
        # Get container client
        container_client = blob_service_client.get_container_client(container_name)
        
        # Process COBOL files (.CBL, .CPY) in S35-Source folder, ignoring S35JSON
        prefix = "S35-Source/"
        processed_count = 0
        
        print(f"Processing COBOL files (.CBL, .CPY) in {container_name}/{prefix} (ignoring S35JSON)...")
        
        # First, let's see what files are available
        all_files = list(container_client.list_blobs(name_starts_with=prefix))
        print(f"Found {len(all_files)} total files/folders in {prefix}")
        
        cobol_blobs = list(iter_cobol_blobs(container_client, prefix))
        print(f"Found {len(cobol_blobs)} COBOL files to process")
        
        # Output will be written to S35-Source/JSONL/ in the blob container
        output_prefix = "S35-Source/JSONL/"
        print(f"Output will be written to: {container_name}/{output_prefix}")
        
        # Initialize output collections
        files_out: List[dict] = []
        paras_out: List[dict] = []
        items_out: List[dict] = []
        facts_out: List[dict] = []
        xrefs_out: List[dict] = []
        chunks_out: List[dict] = []
        flow_edges_out: List[dict] = []
        copybooks_out: List[dict] = []
        
        # Process only first 5 files for testing (change to cobol_blobs to process all)
        test_blobs = cobol_blobs[:5]
        print(f"Processing first {len(test_blobs)} files for testing...")
        
        i = 0
        for blob in test_blobs:
            i += 1
            file_id = f"f{i:06d}"
            print(f"\nProcessing {blob.name} -> {file_id}")
            
            try:
                # Download blob content
                blob_content = download_blob_text(container_client, blob.name)
                if blob_content:
                    # Process the COBOL file content using the full pipeline
                    file_rec, paras, data_items, proc_facts, xrefs, chunks, flow_edges, copybooks, raw_lines = process_blob_content_full(blob.name, blob_content, file_id)
                    
                    if file_rec:
                        print(f"  Extracted: {len(paras)} paragraphs, {len(data_items)} data items, {len(proc_facts)} facts, {len(xrefs)} xrefs, {len(chunks)} chunks, {len(flow_edges)} flow edges, {len(copybooks)} copybooks")
                        
                        # Add to output collections
                        files_out.append({
                            "file_id": file_rec.file_id,
                            "path": file_rec.path,
                            "name": os.path.basename(file_rec.path),
                            "program_id": file_rec.program_id,
                            "lines": len(raw_lines),
                            "format": file_rec.fmt,
                            "procedure_using": file_rec.procedure_using,
                        })
                        
                        # Add paragraphs
                        for p in paras:
                            paras_out.append({
                                "para_id": f"{file_id}:{p.name}",
                                "file_id": file_id,
                                "name": p.name,
                                "kind": p.kind,
                                "start_line": p.start_line,
                                "end_line": p.end_line,
                            })
                        
                        # Add data items
                        for item in data_items:
                            items_out.append({
                                "item_id": f"{file_id}:{item.name}",
                                "file_id": file_id,
                                "name": item.name,
                                "qualified_name": item.qualified_name,
                                "level": item.level,
                                "section": item.section,
                                "pic": item.pic,
                                "usage": item.usage,
                                "occurs_low": item.occurs_low,
                                "occurs_high": item.occurs_high,
                                "depends_on": item.depends_on,
                                "redefines": item.redefines,
                                "value": item.value,
                                "start_line": item.start_line,
                                "end_line": item.end_line,
                            })
                        
                        # Add procedure facts with para field populated
                        for fact in proc_facts:
                            facts_out.append({
                                "fact_id": f"{file_id}:{fact.line}",
                                "file_id": file_id,
                                "para": fact.para,  # This will now be populated with the correct paragraph
                                "line": fact.line,
                                "kind": fact.kind,
                                "snippet": fact.snippet,
                                "callee": fact.callee,
                                "callee_data_name": fact.callee_data_name,
                                "is_dynamic": fact.is_dynamic,
                                "target": fact.target,
                            })
                        
                        # Add xrefs
                        for xref in xrefs:
                            xrefs_out.append({
                                "xref_id": f"{file_id}:{xref.line}:{xref.simple_name}",
                                "file_id": file_id,
                                "path": xref.path,
                                "program_id": xref.program_id,
                                "line": xref.line,
                                "qualified_name": xref.qualified_name,
                                "simple_name": xref.simple_name,
                                "kind": xref.kind,
                                "snippet": xref.snippet,
                                "direction": xref.direction,
                                "start_col": xref.start_col,
                                "end_col": xref.end_col,
                            })
                        
                        # Add chunks
                        for chunk in chunks:
                            chunks_out.append({
                                "chunk_id": chunk.chunk_id,
                                "file_id": chunk.file_id,
                                "path": chunk.path,
                                "program_id": chunk.program_id,
                                "scope": chunk.scope,
                                "name": chunk.name,
                                "start_line": chunk.start_line,
                                "end_line": chunk.end_line,
                                "text": chunk.text,
                            })
                        
                        # Add flow edges
                        for flow_edge in flow_edges:
                            flow_edges_out.append({
                                "edge_id": f"{file_id}:{flow_edge.line}:{flow_edge.caller_para}:{flow_edge.target_para}",
                                "file_id": flow_edge.file_id,
                                "caller_para": flow_edge.caller_para,
                                "target_para": flow_edge.target_para,
                                "line": flow_edge.line,
                                "kind": flow_edge.kind,
                            })
                        
                        # Add copybooks
                        for copybook in copybooks:
                            copybooks_out.append({
                                "copybook_id": f"{file_id}:{copybook.line}:{copybook.copybook_name}",
                                "file_id": copybook.file_id,
                                "parent_path": copybook.parent_path,
                                "copybook_name": copybook.copybook_name,
                                "line": copybook.line,
                                "replacing_clause": copybook.replacing_clause,
                            })
                        
                        processed_count += 1
                        print(f"Successfully processed: {blob.name}")
                    else:
                        print(f"Failed to process: {blob.name}")
                else:
                    print(f"Could not download content for: {blob.name}")
            except Exception as e:
                print(f"Error processing {blob.name}: {str(e)}")
        
        # Derive calls_out from facts_out for CALL statements
        calls_out = []
        for fact in facts_out:
            if fact["kind"] == "call":
                # Create call_id based on static vs dynamic
                if fact["is_dynamic"] == False and fact["callee"]:
                    # Static call
                    call_id = f"{fact['file_id']}:{fact['line']}:{fact['callee']}"
                    callee_program = fact["callee"]
                    callee_data_name = None
                elif fact["is_dynamic"] == True and fact["callee_data_name"]:
                    # Dynamic call
                    call_id = f"{fact['file_id']}:{fact['line']}:{fact['callee_data_name']}"
                    callee_program = None
                    callee_data_name = fact["callee_data_name"]
                else:
                    # Skip malformed call facts
                    continue
                
                calls_out.append({
                    "call_id": call_id,
                    "file_id": fact["file_id"],
                    "caller_para": fact["para"],  # Use the para from the fact record
                    "callee_program": callee_program,
                    "callee_data_name": callee_data_name,
                    "is_dynamic": fact["is_dynamic"],
                    "line": fact["line"],
                    "snippet": fact["snippet"],
                })

        # Write JSONL files to blob storage
        print(f"\nWriting output files to {container_name}/{output_prefix}...")
        write_jsonl_to_blob(container_client, output_prefix, "files.jsonl", files_out)
        write_jsonl_to_blob(container_client, output_prefix, "paragraphs.jsonl", paras_out)
        write_jsonl_to_blob(container_client, output_prefix, "data_items.jsonl", items_out)
        write_jsonl_to_blob(container_client, output_prefix, "procedure_facts.jsonl", facts_out)
        write_jsonl_to_blob(container_client, output_prefix, "calls.jsonl", calls_out)
        write_jsonl_to_blob(container_client, output_prefix, "xrefs.jsonl", xrefs_out)
        write_jsonl_to_blob(container_client, output_prefix, "chunks.jsonl", chunks_out)
        write_jsonl_to_blob(container_client, output_prefix, "flow_edges.jsonl", flow_edges_out)
        write_jsonl_to_blob(container_client, output_prefix, "copybooks.jsonl", copybooks_out)
        
        print(f"Processing complete. Processed {processed_count} files.")
        print(f"Output files written to: {container_name}/{output_prefix}")
    
    except Exception as e:
        print(f"Error connecting to Azure Blob Storage: {str(e)}")
        return

if __name__ == "__main__":
    # Uncomment the line below to process Azure blob files instead
    # process_azure_cobol_files()
    main()
