#!/usr/bin/env python3
"""
Analyze the expanded screen nodes dataset
"""

import os
import requests
from secrets_loader import load_secrets

def analyze_expanded_dataset():
    """Analyze the expanded screen nodes dataset."""
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    url = f'{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01'
    
    print("📊 EXPANDED SCREEN NODES ANALYSIS")
    print("=" * 50)
    
    # Get total counts
    total_body = {'search': '*', 'top': 0, 'count': True}
    response = requests.post(url, headers=headers, json=total_body)
    total_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    # Count by screen type
    form_body = {'search': '*', 'filter': 'screen_type eq \'form\'', 'top': 0, 'count': True}
    form_response = requests.post(url, headers=headers, json=form_body)
    form_count = form_response.json().get('@odata.count', 0) if form_response.status_code == 200 else 0
    
    report_body = {'search': '*', 'filter': 'screen_type eq \'report\'', 'top': 0, 'count': True}
    report_response = requests.post(url, headers=headers, json=report_body)
    report_count = report_response.json().get('@odata.count', 0) if report_response.status_code == 200 else 0
    
    print(f"📊 DATASET OVERVIEW:")
    print(f"   Total screens: {total_count}")
    print(f"   Form screens: {form_count}")
    print(f"   Report screens: {report_count}")
    print(f"   Other screens: {total_count - form_count - report_count}")
    
    # Get samples of new screens (without vectors)
    print(f"\n📝 NEW SCREEN EXAMPLES (without embeddings yet):")
    new_screens_body = {
        'search': '*', 
        'filter': 'has_vector eq false',
        'top': 10, 
        'select': 'screen_name,screen_type,field_count,summary_text,program_id'
    }
    response = requests.post(url, headers=headers, json=new_screens_body)
    
    if response.status_code == 200:
        results = response.json().get('value', [])
        for i, record in enumerate(results, 1):
            screen_name = record.get('screen_name', 'N/A')
            screen_type = record.get('screen_type', 'N/A')
            field_count = record.get('field_count', 0)
            program_id = record.get('program_id', 'N/A')
            summary = record.get('summary_text', 'N/A')
            
            print(f"   {i}. {screen_name} ({screen_type})")
            print(f"      Fields: {field_count}, Program: {program_id[:16]}...")
            print(f"      Summary: {summary}")
            print()
    
    # Analyze field count distribution
    print(f"📊 FIELD COUNT DISTRIBUTION:")
    field_counts = {}
    
    for field_count in range(1, 21):  # Check up to 20 fields
        count_body = {
            'search': '*', 
            'filter': f'field_count eq {field_count}',
            'top': 0, 
            'count': True
        }
        response = requests.post(url, headers=headers, json=count_body)
        count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
        
        if count > 0:
            field_counts[field_count] = count
    
    for field_count, screen_count in sorted(field_counts.items()):
        print(f"   {field_count} fields: {screen_count} screens")
    
    # Find screens with the most fields
    print(f"\n🏆 SCREENS WITH MOST FIELDS:")
    complex_screens_body = {
        'search': '*',
        'orderby': 'field_count desc',
        'top': 5,
        'select': 'screen_name,screen_type,field_count,summary_text'
    }
    response = requests.post(url, headers=headers, json=complex_screens_body)
    
    if response.status_code == 200:
        results = response.json().get('value', [])
        for i, record in enumerate(results, 1):
            screen_name = record.get('screen_name', 'N/A')
            screen_type = record.get('screen_type', 'N/A')
            field_count = record.get('field_count', 0)
            summary = record.get('summary_text', 'N/A')
            
            print(f"   {i}. {screen_name} ({screen_type}) - {field_count} fields")
            print(f"      {summary}")
    
    # Check for interactive screens
    print(f"\n🖥️ INTERACTIVE SCREENS:")
    interactive_body = {
        'search': '*',
        'filter': 'contains(screen_name, \'INTERACTIVE\')',
        'top': 5,
        'select': 'screen_name,field_count,summary_text'
    }
    response = requests.post(url, headers=headers, json=interactive_body)
    
    if response.status_code == 200:
        results = response.json().get('value', [])
        print(f"   Found {len(results)} interactive screens")
        for record in results:
            screen_name = record.get('screen_name', 'N/A')
            field_count = record.get('field_count', 0)
            summary = record.get('summary_text', 'N/A')
            print(f"   - {screen_name}: {summary}")

if __name__ == "__main__":
    analyze_expanded_dataset()