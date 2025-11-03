#!/usr/bin/env python3
"""
FINAL STATUS TRACKER
Provides real-time status updates while marathon processing runs.
"""

import requests
import os
import time
from typing import Dict
from secrets_loader import load_secrets

def load_config():
    """Load configuration."""
    load_secrets()
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
    }

def get_priority_1_status(config: Dict) -> Dict:
    """Get Priority 1 completion status."""
    indexes = [
        {'name': 'new_cobol_program_meta', 'target': 9678},
        {'name': 'new_cobol_program_flows', 'target': 9677},
        {'name': 'new_cobol_program_deps', 'target': 9687},
        {'name': 'new_cobol_screen_nodes', 'target': 10560},
        {'name': 'new_cobol_ui_paths', 'target': 10155}
    ]
    
    results = {}
    total_with_vectors = 0
    total_target = 0
    
    for index_info in indexes:
        index_name = index_info['name']
        target = index_info['target']
        
        # Get records with vectors
        search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        vector_body = {
            'search': '*',
            'filter': 'has_vector eq true',
            'top': 0,
            'count': True
        }
        
        try:
            vector_response = requests.post(
                search_url,
                headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
                json=vector_body,
                timeout=30
            )
            
            vector_count = 0
            if vector_response.status_code == 200:
                vector_count = vector_response.json().get('@odata.count', 0)
        except:
            vector_count = 0
        
        coverage = (vector_count / target * 100) if target > 0 else 0
        
        results[index_name] = {
            'with_vectors': vector_count,
            'target': target,
            'remaining': target - vector_count,
            'coverage': coverage
        }
        
        total_with_vectors += vector_count
        total_target += target
    
    overall_coverage = (total_with_vectors / total_target * 100) if total_target > 0 else 0
    
    return {
        'indexes': results,
        'total_with_vectors': total_with_vectors,
        'total_target': total_target,
        'total_remaining': total_target - total_with_vectors,
        'overall_coverage': overall_coverage
    }

def track_marathon_progress():
    """Track progress while marathon processor runs."""
    config = load_config()
    
    print("📊 MARATHON PROGRESS TRACKER")
    print("=" * 60)
    print("Tracking Priority 1 progress while marathon processor runs...")
    print("Press Ctrl+C to stop tracking\n")
    
    check_count = 0
    last_total = 0
    
    try:
        while True:
            check_count += 1
            status = get_priority_1_status(config)
            
            print(f"📈 CHECK #{check_count} - {time.strftime('%H:%M:%S')}")
            print("-" * 40)
            
            # Show individual indexes
            for index_name, data in status['indexes'].items():
                short_name = index_name.replace('new_cobol_', '').replace('_', ' ').title()
                
                if data['coverage'] >= 100:
                    icon = "✅"
                elif data['coverage'] >= 75:
                    icon = "🟢"
                elif data['coverage'] >= 50:
                    icon = "🟡"
                elif data['coverage'] >= 25:
                    icon = "🟠"
                else:
                    icon = "🔴"
                
                print(f"{icon} {short_name:15} | {data['with_vectors']:>5,}/{data['target']:>5,} ({data['coverage']:>5.1f}%)")
            
            # Overall status
            current_total = status['total_with_vectors']
            change = current_total - last_total
            change_indicator = f" (+{change:,})" if change > 0 else ""
            
            print(f"\n📊 PRIORITY 1 OVERALL:")
            print(f"   Total: {current_total:,}/{status['total_target']:,} ({status['overall_coverage']:.1f}%){change_indicator}")
            print(f"   Remaining: {status['total_remaining']:,} records")
            
            if status['overall_coverage'] >= 90:
                print(f"🎉 APPROACHING COMPLETION! 90%+ achieved!")
            
            last_total = current_total
            
            print(f"\n⏱️  Next check in 60 seconds...\n")
            time.sleep(60)
            
    except KeyboardInterrupt:
        print(f"\n📊 Tracking stopped. Final status:")
        final_status = get_priority_1_status(config)
        print(f"   Priority 1: {final_status['overall_coverage']:.1f}% complete")
        print(f"   Remaining: {final_status['total_remaining']:,} records")

if __name__ == '__main__':
    track_marathon_progress()