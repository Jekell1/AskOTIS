#!/usr/bin/env python3
"""
Analyze current screen nodes to understand patterns for expansion
"""

import os
import requests
from secrets_loader import load_secrets

def analyze_current_screen_nodes():
    """Analyze current screen nodes to understand patterns."""
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    url = f'{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01'
    
    # Get examples of current good records
    body = {'search': '*', 'top': 10, 'select': 'screen_name,screen_type,field_count,summary_text,program_id,fields_json'}
    response = requests.post(url, headers=headers, json=body)
    
    if response.status_code == 200:
        results = response.json().get('value', [])
        print('=== CURRENT SCREEN NODE EXAMPLES ===')
        for i, record in enumerate(results, 1):
            screen_name = record.get('screen_name', 'N/A')
            screen_type = record.get('screen_type', 'N/A') 
            field_count = record.get('field_count', 0)
            program_id = record.get('program_id', 'N/A')
            summary = record.get('summary_text', 'N/A')
            
            print(f'{i}. {screen_name} ({screen_type})')
            print(f'   Fields: {field_count}, Program: {program_id[:16]}...')
            print(f'   Summary: {summary}')
            print()
        
        # Analyze patterns
        print('=== PATTERN ANALYSIS ===')
        screen_types = {}
        field_counts = {}
        
        for record in results:
            stype = record.get('screen_type', 'unknown')
            fcount = record.get('field_count', 0)
            
            screen_types[stype] = screen_types.get(stype, 0) + 1
            field_counts[fcount] = field_counts.get(fcount, 0) + 1
        
        print('Screen Types:')
        for stype, count in screen_types.items():
            print(f'  {stype}: {count}')
            
        print('\nField Counts:')
        for fcount, count in sorted(field_counts.items()):
            print(f'  {fcount} fields: {count} screens')
    
    else:
        print(f'Error: {response.status_code}')

if __name__ == "__main__":
    analyze_current_screen_nodes()