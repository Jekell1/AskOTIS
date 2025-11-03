"""
Monitor the comprehensive COBOL source file enhancement progress.
Shows real-time stats and current status.
"""

import json
import requests
import time
from datetime import datetime

def monitor_enhancement_progress():
    """Monitor the enhancement progress by checking the index."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print("📊 MONITORING COMPREHENSIVE ENHANCEMENT PROGRESS")
    print("=" * 60)
    
    while True:
        try:
            # Get total document count
            total_url = f"{endpoint}/indexes/new_cobol_calls/docs?api-version=2024-07-01&$count=true&$top=0"
            total_response = requests.get(total_url, headers=headers)
            
            if total_response.status_code == 200:
                total_data = total_response.json()
                total_docs = total_data.get('@odata.count', 0)
            else:
                total_docs = 0
            
            # Get enhanced records count
            enhanced_url = f"{endpoint}/indexes/new_cobol_calls/docs?api-version=2024-07-01&$count=true&$top=0&$filter=enhanced_data eq true"
            enhanced_response = requests.get(enhanced_url, headers=headers)
            
            if enhanced_response.status_code == 200:
                enhanced_data = enhanced_response.json()
                enhanced_docs = enhanced_data.get('@odata.count', 0)
            else:
                enhanced_docs = 0
            
            # Get facets for enhanced data
            facet_url = f"{endpoint}/indexes/new_cobol_calls/docs/search?api-version=2024-07-01"
            facet_body = {
                "search": "*",
                "filter": "enhanced_data eq true",
                "facets": ["category", "reference_type"],
                "top": 0
            }
            
            facet_response = requests.post(facet_url, headers=headers, json=facet_body)
            
            categories = {}
            ref_types = {}
            
            if facet_response.status_code == 200:
                facet_data = facet_response.json()
                facets = facet_data.get('@search.facets', {})
                
                if 'category' in facets:
                    categories = {f.get('value', 'Unknown'): f.get('count', 0) for f in facets['category']}
                
                if 'reference_type' in facets:
                    ref_types = {f.get('value', 'Unknown'): f.get('count', 0) for f in facets['reference_type'][:10]}
            
            # Display current status
            timestamp = datetime.now().strftime('%H:%M:%S')
            print(f"\r[{timestamp}] Total docs: {total_docs:,} | Enhanced: {enhanced_docs:,} | Growth: +{enhanced_docs - 361:,}", end='', flush=True)
            
            # Show detailed stats every 30 seconds
            if int(time.time()) % 30 == 0:
                print(f"\n\n📈 DETAILED PROGRESS [{timestamp}]")
                print(f"   Total index documents: {total_docs:,}")
                print(f"   Enhanced records: {enhanced_docs:,}")
                print(f"   Growth since start: +{enhanced_docs - 361:,}")
                
                if categories:
                    print(f"\n   📂 Categories:")
                    for category, count in sorted(categories.items(), key=lambda x: x[1], reverse=True):
                        print(f"      • {category}: {count:,}")
                
                if ref_types:
                    print(f"\n   🔍 Top Reference Types:")
                    for ref_type, count in sorted(ref_types.items(), key=lambda x: x[1], reverse=True)[:5]:
                        print(f"      • {ref_type}: {count:,}")
                
                print(f"\n" + "=" * 60)
            
            time.sleep(2)  # Update every 2 seconds
            
        except KeyboardInterrupt:
            print(f"\n\n✅ Monitoring stopped.")
            break
        except Exception as e:
            print(f"\n❌ Error monitoring: {e}")
            time.sleep(5)

def show_final_summary():
    """Show final summary after enhancement completes."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print(f"\n🎯 FINAL ENHANCEMENT SUMMARY")
    print("=" * 50)
    
    # Get enhanced records by caller program
    caller_search_url = f"{endpoint}/indexes/new_cobol_calls/docs/search?api-version=2024-07-01"
    caller_body = {
        "search": "*",
        "filter": "enhanced_data eq true",
        "facets": ["caller_program"],
        "top": 0
    }
    
    caller_response = requests.post(caller_search_url, headers=headers, json=caller_body)
    
    if caller_response.status_code == 200:
        data = caller_response.json()
        facets = data.get('@search.facets', {})
        
        if 'caller_program' in facets:
            programs = facets['caller_program']
            print(f"📋 Enhanced programs: {len(programs):,}")
            
            # Show programs with most references
            top_programs = sorted(programs, key=lambda x: x.get('count', 0), reverse=True)[:10]
            print(f"\n🔝 Top 10 programs by external references:")
            for i, prog in enumerate(top_programs, 1):
                name = prog.get('value', 'Unknown')
                count = prog.get('count', 0)
                print(f"   {i:2d}. {name}: {count} references")

def main():
    """Main monitoring function."""
    try:
        monitor_enhancement_progress()
    finally:
        show_final_summary()

if __name__ == "__main__":
    main()