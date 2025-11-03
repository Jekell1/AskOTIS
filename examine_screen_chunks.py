"""
Examine the actual content of chunks to understand the SCREEN SECTION issue.
"""
import os
import requests
from secrets_loader import load_secrets

def examine_screen_chunks():
    """Examine actual chunk content to understand the issue."""
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    print("🔍 EXAMINING SCREEN SECTION CHUNKS")
    print("=" * 60)
    
    # Get chunks with "SCREEN SECTION"
    chunks_url = f"{search_endpoint}/indexes/new_code_chunks/docs/search?api-version=2023-11-01"
    
    search_body = {
        "search": '"SCREEN SECTION"',
        "top": 3,
        "queryType": "simple",
        "searchMode": "any",
        "select": "chunk_id,program_id,text"
    }
    
    chunks_response = requests.post(chunks_url, headers=headers, json=search_body)
    
    if chunks_response.status_code == 200:
        chunks_data = chunks_response.json()
        chunks = chunks_data.get('value', [])
        
        for i, chunk in enumerate(chunks):
            print(f"\n📄 CHUNK {i+1} FULL CONTENT:")
            print(f"Program: {chunk.get('program_id')}")
            print(f"Chunk ID: {chunk.get('chunk_id')}")
            print("-" * 80)
            
            text = chunk.get('text', '')
            
            # Show context around "SCREEN SECTION"
            lines = text.split('\n')
            screen_section_line = -1
            
            for j, line in enumerate(lines):
                if 'SCREEN SECTION' in line.upper():
                    screen_section_line = j
                    break
            
            if screen_section_line >= 0:
                # Show 10 lines before and 20 lines after SCREEN SECTION
                start = max(0, screen_section_line - 10)
                end = min(len(lines), screen_section_line + 20)
                
                print(f"Context around SCREEN SECTION (lines {start+1}-{end}):")
                for k in range(start, end):
                    marker = ">>> " if k == screen_section_line else "    "
                    print(f"{marker}{k+1:3d}: {lines[k]}")
            
            print("\n" + "=" * 80)
    
    # Also search for actual COBOL field definitions
    print(f"\n🔍 SEARCHING FOR ACTUAL FIELD DEFINITIONS:")
    
    # Look for PIC clauses
    pic_search_body = {
        "search": "PIC",
        "top": 3,
        "select": "chunk_id,program_id,text"
    }
    
    pic_response = requests.post(chunks_url, headers=headers, json=pic_search_body)
    
    if pic_response.status_code == 200:
        pic_data = pic_response.json()
        pic_chunks = pic_data.get('value', [])
        
        print(f"Found {len(pic_chunks)} chunks with 'PIC'")
        
        for i, chunk in enumerate(pic_chunks[:2]):
            text = chunk.get('text', '')
            lines = text.split('\n')
            
            print(f"\nChunk {i+1} with PIC clauses:")
            for j, line in enumerate(lines):
                if 'PIC' in line.upper() and line.strip():
                    print(f"  {j+1:3d}: {line.strip()}")

if __name__ == "__main__":
    examine_screen_chunks()