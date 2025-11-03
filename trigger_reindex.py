#!/usr/bin/env python3
"""
Trigger reindexing of the COBOL search index to fix duplicate line number issues
"""
import requests
import json
import time

def trigger_reindex():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = 'ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef'
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== COBOL INDEX REINDEXING ===')
    
    # First, let's check the current indexer status
    indexers_to_run = ['cobol-indexer', 'cobol-jsonl-indexer']
    
    for indexer_name in indexers_to_run:
        print(f'\n--- Processing {indexer_name} ---')
        
        # Get current status
        status_response = requests.get(
            f'{search_endpoint}/indexers/{indexer_name}?api-version=2024-07-01',
            headers=headers
        )
        
        if status_response.status_code == 200:
            indexer_info = status_response.json()
            current_status = indexer_info.get('status', {}).get('lastResult', {}).get('status', 'unknown')
            print(f'Current status: {current_status}')
            
            # Reset the indexer to clear any previous state
            print(f'Resetting {indexer_name}...')
            reset_response = requests.post(
                f'{search_endpoint}/indexers/{indexer_name}/reset?api-version=2024-07-01',
                headers=headers
            )
            
            if reset_response.status_code in [200, 204]:
                print(f'✅ Reset successful')
                
                # Now run the indexer
                print(f'Starting reindex for {indexer_name}...')
                run_response = requests.post(
                    f'{search_endpoint}/indexers/{indexer_name}/run?api-version=2024-07-01',
                    headers=headers
                )
                
                if run_response.status_code in [200, 202]:
                    print(f'✅ Reindex started successfully')
                else:
                    print(f'❌ Failed to start reindex: {run_response.status_code} - {run_response.text}')
            else:
                print(f'❌ Failed to reset indexer: {reset_response.status_code} - {reset_response.text}')
        else:
            print(f'❌ Failed to get indexer status: {status_response.status_code} - {status_response.text}')
    
    print(f'\n=== MONITORING REINDEX PROGRESS ===')
    print(f'Checking indexer status every 30 seconds...')
    
    # Monitor progress for a few minutes
    max_checks = 10  # Check for up to 5 minutes
    for check in range(max_checks):
        print(f'\n--- Check {check + 1}/{max_checks} ---')
        
        all_complete = True
        for indexer_name in indexers_to_run:
            status_response = requests.get(
                f'{search_endpoint}/indexers/{indexer_name}/status?api-version=2024-07-01',
                headers=headers
            )
            
            if status_response.status_code == 200:
                status_info = status_response.json()
                last_result = status_info.get('lastResult', {})
                status = last_result.get('status', 'unknown')
                item_count = last_result.get('itemCount', 0)
                error_count = last_result.get('errorCount', 0)
                
                print(f'{indexer_name}: {status} (items: {item_count}, errors: {error_count})')
                
                if status not in ['success', 'transientFailure', 'persistentFailure']:
                    all_complete = False
            else:
                print(f'{indexer_name}: Unable to get status')
                all_complete = False
        
        if all_complete:
            print(f'\n✅ All indexers completed!')
            break
        
        if check < max_checks - 1:  # Don't sleep on the last check
            time.sleep(30)
    
    # Final status check
    print(f'\n=== FINAL STATUS ===')
    for indexer_name in indexers_to_run:
        status_response = requests.get(
            f'{search_endpoint}/indexers/{indexer_name}/status?api-version=2024-07-01',
            headers=headers
        )
        
        if status_response.status_code == 200:
            status_info = status_response.json()
            last_result = status_info.get('lastResult', {})
            status = last_result.get('status', 'unknown')
            item_count = last_result.get('itemCount', 0)
            error_count = last_result.get('errorCount', 0)
            
            print(f'{indexer_name}: {status}')
            print(f'  Items processed: {item_count:,}')
            print(f'  Errors: {error_count}')
            
            if error_count > 0:
                errors = last_result.get('errors', [])
                print(f'  Recent errors:')
                for error in errors[:3]:  # Show first 3 errors
                    print(f'    - {error.get("errorMessage", "Unknown error")}')
    
    print(f'\n=== NEXT STEPS ===')
    print(f'1. Run test_cobol_search.py to verify the index is working')
    print(f'2. Run investigate_duplicates.py to check if line duplicates are fixed')
    print(f'3. Test the RAG chatbot with simple_cobol_rag.py')

if __name__ == "__main__":
    print("This will trigger a full reindex of your COBOL search data.")
    print("This process may take several minutes and will temporarily affect search performance.")
    
    confirm = input("Do you want to proceed? (yes/no): ").lower().strip()
    
    if confirm in ['yes', 'y']:
        trigger_reindex()
    else:
        print("Reindexing cancelled.")
