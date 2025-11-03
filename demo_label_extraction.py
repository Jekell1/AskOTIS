"""Quick test to demonstrate LABEL extraction is working."""
import os, json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

load_settings()
ep=os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
key=os.getenv('AZURE_SEARCH_KEY')
client=SearchClient(endpoint=ep,index_name='new_cobol_screen_nodes',credential=AzureKeyCredential(key))

print("=" * 70)
print("SCREEN NODES INDEX - LABEL EXTRACTION TEST")
print("=" * 70)

# Get screens with LABEL statements
results=client.search(
    search_text='DISPLAY ENTER SELECT',
    search_fields=['label_literals_json'],
    select=['screen_id','program_id','label_literals_json'],
    top=5
)

found=0
for doc in results:
    if doc.get('label_literals_json'):
        labels = json.loads(doc['label_literals_json'])
        if labels:
            found+=1
            print(f"\n✅ {doc['program_id']} - {doc['screen_id']}")
            print(f"   Found {len(labels)} LABEL statements")
            for i, label in enumerate(labels[:3], 1):
                text = label['text'][:60]
                print(f"   {i}. Line {label['line']}, Col {label['col']}: \"{text}\"")

print(f"\n{'=' * 70}")
print(f"RESULT: Found {found} screens with LABEL statements")
print(f"{'=' * 70}")

if found > 0:
    print("\n✅ LABEL extraction is WORKING for programs")
    print("⚠️  Menu programs (OPMENU, PGMENU, etc.) not found because")
    print("   their SCREEN SECTION is in copybooks (*.CPY files)")
    print("   which are not yet in the code_chunks index.")
else:
    print("\n⚠️  No LABEL statements found in current screen_nodes")

print("\n" + "=" * 70)
print("NEXT STEP: Add copybooks to chunking pipeline")
print("=" * 70)
print("1. Find chunking script (likely in ingest/ or root)")
print("2. Add pattern: *.CPY files")
print("3. Re-run chunking for LIBWI, LIBSP directories")
print("4. Re-run: python build_screen_nodes.py")
print("5. Menu text will then be searchable via RAG")
print("=" * 70)
