#!/usr/bin/env python3
"""
Investigate Screen Nodes Record Count

This script investigates why NEW_COBOL_SCREEN_NODES only has 1,000 records
by examining the actual data content and structure.
"""

import requests
import os
import json
from secrets_loader import load_secrets

def load_config():
    """Load configuration using the working pattern."""
    load_secrets()
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
    }

def investigate_screen_nodes(config):
    """Investigate the screen nodes index in detail."""
    print("🔍 INVESTIGATING NEW_COBOL_SCREEN_NODES")
    print("=" * 60)
    
    index_name = "new_cobol_screen_nodes"
    search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    # Get total count
    count_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
    try:
        total_response = requests.get(count_url, headers={'api-key': config['search_key']}, timeout=30)
        total_count = int(total_response.text) if total_response.status_code == 200 else 0
        print(f"📊 Total records: {total_count:,}")
    except Exception as e:
        print(f"❌ Error getting count: {e}")
        return
    
    # Get sample records to understand the structure
    sample_body = {
        'search': '*',
        'top': 10,  # Get first 10 records
        'orderby': 'search.score() desc'
    }
    
    try:
        sample_response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=sample_body,
            timeout=30
        )
        
        if sample_response.status_code == 200:
            sample_data = sample_response.json()
            docs = sample_data.get('value', [])
            
            print(f"\n📋 SAMPLE RECORDS ANALYSIS (showing {len(docs)} records)")
            print("-" * 60)
            
            if docs:
                # Show structure of first record
                first_doc = docs[0]
                print(f"🏗️  RECORD STRUCTURE:")
                for key, value in first_doc.items():
                    if key.startswith('@'):
                        continue
                    value_preview = str(value)[:100] + "..." if len(str(value)) > 100 else str(value)
                    print(f"   {key:20}: {value_preview}")
                
                print(f"\n📄 SAMPLE RECORD CONTENTS:")
                print("-" * 60)
                
                # Show details of first few records
                for i, doc in enumerate(docs[:5], 1):
                    print(f"\n📄 RECORD {i}:")
                    
                    # Show key identifying fields
                    for field in ['id', 'program_id', 'screen_name', 'node_id', 'node_type', 'summary_text']:
                        if field in doc and doc[field]:
                            value = str(doc[field])[:150] + "..." if len(str(doc[field])) > 150 else str(doc[field])
                            print(f"   {field:15}: {value}")
                
                # Analyze field population
                print(f"\n📊 FIELD POPULATION ANALYSIS:")
                print("-" * 60)
                
                field_counts = {}
                for doc in docs:
                    for field, value in doc.items():
                        if field.startswith('@'):
                            continue
                        if field not in field_counts:
                            field_counts[field] = 0
                        if value and str(value).strip():
                            field_counts[field] += 1
                
                for field, count in sorted(field_counts.items()):
                    percentage = (count / len(docs)) * 100
                    print(f"   {field:20}: {count:2}/{len(docs)} ({percentage:5.1f}%)")
                
            else:
                print("❌ No sample records found")
        
        else:
            print(f"❌ Error getting samples: {sample_response.status_code}")
            
    except Exception as e:
        print(f"❌ Error getting sample data: {e}")
    
    # Check if this might be a subset/filtered view
    print(f"\n🔍 INVESTIGATING POTENTIAL CAUSES:")
    print("-" * 60)
    
    # Check for unique programs
    programs_body = {
        'search': '*',
        'facets': ['program_id'],
        'top': 0
    }
    
    try:
        programs_response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=programs_body,
            timeout=30
        )
        
        if programs_response.status_code == 200:
            programs_data = programs_response.json()
            facets = programs_data.get('@search.facets', {})
            program_facets = facets.get('program_id', [])
            
            print(f"📁 Unique programs with screen nodes: {len(program_facets)}")
            
            if program_facets:
                print(f"📋 Top programs by screen node count:")
                # Sort by count descending
                sorted_programs = sorted(program_facets, key=lambda x: x['count'], reverse=True)
                for i, prog in enumerate(sorted_programs[:10], 1):
                    print(f"   {i:2}. {prog['value']:15}: {prog['count']:3} screen nodes")
    
    except Exception as e:
        print(f"⚠️  Could not analyze program distribution: {e}")
    
    # Compare with UI paths to see if there's a relationship
    print(f"\n🔗 COMPARISON WITH UI_PATHS:")
    print("-" * 60)
    
    ui_paths_count_url = f"{config['search_endpoint']}/indexes/new_cobol_ui_paths/docs/$count?api-version=2023-11-01"
    try:
        ui_response = requests.get(ui_paths_count_url, headers={'api-key': config['search_key']}, timeout=30)
        ui_count = int(ui_response.text) if ui_response.status_code == 200 else 0
        print(f"📊 UI Paths records: {ui_count:,}")
        print(f"📊 Screen Nodes records: {total_count:,}")
        print(f"📊 Ratio: {ui_count/total_count:.1f} UI paths per screen node" if total_count > 0 else "N/A")
    except Exception as e:
        print(f"⚠️  Could not compare with UI paths: {e}")

def investigate_data_source_patterns(config):
    """Check if the 1,000 limit suggests this is a sample or subset."""
    print(f"\n🔍 INVESTIGATING DATA PATTERNS:")
    print("-" * 60)
    
    index_name = "new_cobol_screen_nodes"
    search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    # Check if records are distributed across different programs
    distribution_body = {
        'search': '*',
        'top': 1000,  # Get all records to analyze distribution
        'select': 'program_id,id'
    }
    
    try:
        response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=distribution_body,
            timeout=60
        )
        
        if response.status_code == 200:
            data = response.json()
            docs = data.get('value', [])
            
            print(f"📊 Analyzing distribution of {len(docs)} records...")
            
            # Count by program
            program_counts = {}
            for doc in docs:
                prog_id = doc.get('program_id', 'UNKNOWN')
                program_counts[prog_id] = program_counts.get(prog_id, 0) + 1
            
            print(f"📁 Records distributed across {len(program_counts)} programs")
            
            if len(program_counts) > 0:
                # Show distribution
                sorted_progs = sorted(program_counts.items(), key=lambda x: x[1], reverse=True)
                print(f"📊 Top programs by screen node count:")
                for i, (prog, count) in enumerate(sorted_progs[:15], 1):
                    print(f"   {i:2}. {prog:15}: {count:3} nodes")
                
                # Check if this looks like a sample
                max_count = max(program_counts.values())
                avg_count = sum(program_counts.values()) / len(program_counts)
                
                print(f"\n📈 DISTRIBUTION ANALYSIS:")
                print(f"   Programs with screen nodes: {len(program_counts)}")
                print(f"   Average nodes per program: {avg_count:.1f}")
                print(f"   Maximum nodes in one program: {max_count}")
                print(f"   Total screen nodes: {sum(program_counts.values())}")
                
                # Check if 1,000 is a round number suggesting a limit
                if sum(program_counts.values()) == 1000:
                    print(f"\n🚨 ANALYSIS: Exactly 1,000 records suggests this might be:")
                    print(f"   - A sample/preview dataset")
                    print(f"   - A limit imposed during data ingestion")
                    print(f"   - A subset for testing purposes")
                    print(f"   - The actual complete dataset if COBOL programs have limited UI screens")
    
    except Exception as e:
        print(f"❌ Error analyzing distribution: {e}")

def main():
    """Main investigation function."""
    print("🔍 INVESTIGATING SCREEN NODES RECORD COUNT")
    print("=" * 80)
    print("Why does NEW_COBOL_SCREEN_NODES only have 1,000 records?")
    
    config = load_config()
    
    if not config['search_endpoint'] or not config['search_key']:
        print("❌ Missing Azure Search configuration")
        return
    
    investigate_screen_nodes(config)
    investigate_data_source_patterns(config)
    
    print(f"\n🎯 CONCLUSIONS:")
    print("-" * 60)
    print("Possible explanations for 1,000 records:")
    print("1. Sample/subset: Exactly 1,000 suggests a deliberate limit")
    print("2. UI screens are rare: COBOL programs may have limited screen definitions")
    print("3. Ingestion limit: Data pipeline may have imposed a 1,000 record limit")
    print("4. Filtered data: Only certain types of screen nodes were included")
    print("5. Test dataset: This might be a preview/test version of the full data")

if __name__ == "__main__":
    main()