"""Utilities for stable cross-index identifiers.

All functions are pure + deterministic.
"""
from __future__ import annotations
import hashlib

def normalize_program_id(pid: str | None) -> str:
    if not pid:
        return ''
    return pid.strip().upper()

def _sha1(text: str) -> str:
    return hashlib.sha1(text.encode('utf-8')).hexdigest()

def make_scoped_symbol_id(program_id: str, symbol_name: str) -> str:
    return _sha1(f"{normalize_program_id(program_id)}|{symbol_name.strip().upper()}")

def make_global_symbol_id(symbol_name: str) -> str:
    return _sha1(symbol_name.strip().upper())

def make_data_item_symbol_id(program_id: str, path: str) -> str:
    # Path already encodes hierarchical uniqueness in program scope.
    return _sha1(f"{normalize_program_id(program_id)}|{path.strip().upper()}")

def make_ref_id(program_id: str, line: int, symbol_name: str, kind: str) -> str:
    return _sha1(f"{normalize_program_id(program_id)}|{line}|{symbol_name.strip().upper()}|{kind.upper()}")
