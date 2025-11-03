#!/usr/bin/env python3
"""
Analyze LPMENU source code to find what program/transaction is called for option 1.
"""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag import OTISRAG
import re

def main():
    rag = OTISRAG()
    
    # Get LPMENU source code
    client = SearchClient(
        endpoint=rag.config.search_endpoint,
        index_name='code_new',
        credential=AzureKeyCredential(rag.config.search_key)
    )
    
    result = list(client.search('file_name:LPMENU', top=1))
    if not result:
        print("LPMENU not found in code index")
        return
    
    doc = result[0]
    content = doc.get('content', '')
    lines = content.split('\n')
    
    print("=" * 80)
    print("SEARCHING LPMENU FOR MENU OPTION 1 ROUTING")
    print("=" * 80)
    print()
    
    # Look for various patterns that might indicate menu routing
    patterns = [
        (r'WHEN.*[\'"]?1[\'"]?', 'WHEN clause with 1'),
        (r'IF.*SELECTION.*=.*1', 'IF selection = 1'),
        (r'DAILY', 'Contains DAILY'),
        (r'OPTION.*1', 'Option 1 reference'),
        (r'EVALUATE.*SELECTION', 'EVALUATE selection'),
        (r'TRANID|TRAN-ID|TRANSACTION', 'Transaction ID'),
    ]
    
    for pattern_str, description in patterns:
        print(f"\n--- Pattern: {description} ({pattern_str}) ---")
        pattern = re.compile(pattern_str, re.IGNORECASE)
        found = False
        for i, line in enumerate(lines, 1):
            if pattern.search(line):
                found = True
                # Print context (3 lines before and after)
                start = max(0, i - 4)
                end = min(len(lines), i + 3)
                for j in range(start, end):
                    marker = ' >>> ' if j == i - 1 else '     '
                    print(f"{marker}{j+1:4d}: {lines[j]}")
                print()
        if not found:
            print("  (no matches)")
    
    # Also look for any CALL or XCTL statements
    print("\n" + "=" * 80)
    print("ALL CALL/XCTL STATEMENTS IN LPMENU")
    print("=" * 80)
    print()
    
    call_pattern = re.compile(r'(CALL|XCTL|LINK)\s+[\'"]?(\w+)[\'"]?', re.IGNORECASE)
    for i, line in enumerate(lines, 1):
        match = call_pattern.search(line)
        if match:
            print(f"{i:4d}: {line.strip()}")
            if match.group(2):
                print(f"       -> Calls: {match.group(2)}")

if __name__ == '__main__':
    main()
