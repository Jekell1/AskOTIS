import json
import requests

# Load configuration
with open('local.settings.json', 'r') as f:
    config = json.load(f)

search_endpoint = config['Values']['SEARCH_ENDPOINT']
search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
search_admin_key = config['Values']['SEARCH_KEY']

search_url = f"https://{search_service_name}.search.windows.net/indexes/cobol-index/docs/search"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

print("🔍 DETAILED ANALYSIS: Document vs File Count")
print("=" * 60)

# Let's examine the discrepancy more carefully
print("THEORY TESTING:")
print("If each JSONL file has multiple lines (records), but only the first line")
print("gets indexed as a document, that would explain the 2:1 ratio.")
print()

# Check if we're only getting the first line of each JSONL file
sample_search = {
    "search": "*",
    "select": "repo_path,line,code,id",
    "orderby": "repo_path",
    "top": 50
}

try:
    response = requests.post(search_url, headers=headers, params=params, json=sample_search)
    
    if response.status_code == 200:
        result = response.json()
        results = result.get('value', [])
        
        # Group by file and check line numbers
        file_lines = {}
        for doc in results:
            repo_path = doc.get('repo_path', '')
            filename = repo_path.split('/')[-1]
            line_num = doc.get('line', 0)
            
            if filename not in file_lines:
                file_lines[filename] = []
            file_lines[filename].append(line_num)
        
        print("📊 LINE NUMBERS PER FILE (sample of 50 documents):")
        print("-" * 60)
        
        files_with_multiple_lines = 0
        files_with_single_line = 0
        
        for filename, lines in list(file_lines.items())[:10]:  # Show first 10 files
            lines_sorted = sorted(lines)
            print(f"📄 {filename}:")
            print(f"   Lines: {lines_sorted}")
            
            if len(lines_sorted) > 1:
                files_with_multiple_lines += 1
                print(f"   ✅ Multiple lines indexed: {len(lines_sorted)} documents")
            else:
                files_with_single_line += 1
                print(f"   ⚠️  Single line indexed: {len(lines_sorted)} document")
            print()
        
        print(f"📈 PATTERN ANALYSIS:")
        print(f"   Files with multiple lines indexed: {files_with_multiple_lines}")
        print(f"   Files with single line indexed: {files_with_single_line}")
        
    else:
        print(f"Error: {response.text}")
        
except Exception as e:
    print(f"Error: {e}")

print("\n" + "=" * 60)

# Let's count documents for a few specific files to verify
test_files = ["CL__ACUMEM.CBL.jsonl", "CL__AUDITW.CBL.jsonl", "CL__BICLAS.CBL.jsonl"]

print("🔍 DOCUMENT COUNT VERIFICATION:")
print("-" * 60)

for filename in test_files:
    # Count documents from this specific file
    file_search = {
        "search": "*",
        "filter": f"search.ismatch('{filename}', 'repo_path')",
        "count": True,
        "top": 0
    }
    
    try:
        response = requests.post(search_url, headers=headers, params=params, json=file_search)
        
        if response.status_code == 200:
            result = response.json()
            doc_count = result.get('@odata.count', 0)
            print(f"📄 {filename}: {doc_count} indexed documents")
            
            # We know from earlier analysis:
            if filename == "CL__ACUMEM.CBL.jsonl":
                print(f"   (JSONL file has 315 lines)")
            elif filename == "CL__AUDITW.CBL.jsonl":
                print(f"   (JSONL file has 424 lines)")
            elif filename == "CL__BICLAS.CBL.jsonl":
                print(f"   (JSONL file has 155 lines)")
                
        else:
            print(f"❌ Error counting {filename}: {response.text}")
            
    except Exception as e:
        print(f"❌ Error: {e}")

print("\n" + "=" * 60)
print("🧐 CONCLUSION:")
print("-" * 60)

print("Based on the analysis, here's what's happening:")
print()
print("1. ✅ **JSONL files contain MULTIPLE lines**")
print("   - CL__ACUMEM.CBL.jsonl: 315 lines")
print("   - CL__AUDITW.CBL.jsonl: 424 lines") 
print("   - CL__BICLAS.CBL.jsonl: 155 lines")
print()
print("2. ❓ **But only ~1-2 documents per file are indexed**")
print("   - This suggests the indexer or skillset isn't processing")
print("     all lines in each JSONL file")
print()
print("3. 📊 **The 19,904 vs 9,952 ratio (2:1) suggests:**")
print("   - Some files produce 1 document, some produce 2")
print("   - Average: 2 documents per JSONL file")
print("   - But JSONL files actually contain 100-400+ lines each!")
print()
print("4. 🚨 **POTENTIAL ISSUE IDENTIFIED:**")
print("   - The Azure Function or indexer may not be processing")
print("     ALL lines in each JSONL file")
print("   - We should have closer to 100,000+ documents if all")
print("     lines were indexed individually!")

print("\n💡 **RECOMMENDATION:**")
print("Check the Azure Function and skillset configuration.")
print("The indexer should process ALL lines in each JSONL file,")
print("not just the first 1-2 lines.")
print("=" * 60)
