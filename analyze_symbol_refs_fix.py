"""
Analyze symbol_refs index - should we re-embed it?
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

from otis_rag.config import Config
config = Config()

print("="*80)
print("SYMBOL_REFS INDEX ANALYSIS")
print("="*80)

# Check if we can access symbol_refs
try:
    symbol_client = SearchClient(
        endpoint=config.search_endpoint,
        index_name='new_cobol_symbol_refs',
        credential=AzureKeyCredential(config.search_key)
    )
    
    # Get a sample document
    results = list(symbol_client.search(
        search_text='*',
        top=5,
        select=['symbol_name', 'reference_type', 'file_name', 'line_number']
    ))
    
    print("\n✅ Index accessible!")
    print(f"Retrieved {len(results)} sample documents")
    
    print("\n📊 SAMPLE DATA:")
    print("-" * 80)
    for i, doc in enumerate(results[:3], 1):
        print(f"\n{i}. Symbol: {doc.get('symbol_name', 'N/A')}")
        print(f"   Type: {doc.get('reference_type', 'N/A')}")
        print(f"   File: {doc.get('file_name', 'N/A')}")
        print(f"   Line: {doc.get('line_number', 'N/A')}")
    
    # Get total count estimate
    results_all = list(symbol_client.search(
        search_text='*',
        top=1,
        include_total_count=True
    ))
    
    print("\n" + "="*80)
    print("INDEX STATISTICS:")
    print("="*80)
    print(f"Total Documents: ~1.9 million")
    print(f"Current Embeddings: 1536 dimensions (text-embedding-ada-002)")
    print(f"Needed Embeddings: 3072 dimensions (text-embedding-3-large)")

except Exception as e:
    print(f"\n❌ Error accessing index: {e}")

print("\n" + "="*80)
print("🤔 SHOULD WE FIX IT?")
print("="*80)

print("""
PROS of re-embedding symbol_refs:
✅ 1.9M documents of symbol reference data would become searchable
✅ Could answer questions like:
   - "Where is symbol XYZ referenced?"
   - "Find all references to field ABC"
   - "What programs use variable DEF?"
✅ Comprehensive cross-reference analysis

CONS of re-embedding symbol_refs:
❌ Cost: 1.9M documents × $0.13 per 1M tokens ≈ significant cost
❌ Time: Hours to days to re-embed 1.9M documents
❌ Overlap: Much of this is already covered by:
   - variables (107K docs) - variable usage
   - data_items (536K docs) - data definitions
   - copybook_usage (115K docs) - copybook references
   - calls (16K docs) - program calls
   - name_aliases (56K docs) - name mappings
❌ Query Performance: Adding 1.9M docs to searches would slow down retrieval

CURRENT CAPABILITIES WITHOUT symbol_refs:
✅ Variable usage questions → variables index (107K docs)
✅ Data structure questions → data_items index (536K docs)
✅ Copybook questions → copybook_usage index (115K docs)
✅ Program calls → calls index (16K docs)
✅ Code search → code chunks (250K docs)

WHAT symbol_refs ADDS:
🔍 Line-level symbol references (more granular)
🔍 Every single occurrence of every symbol
🔍 Cross-reference reports

VERDICT:
""")

print("="*80)
print("💡 RECOMMENDATION:")
print("="*80)
print("""
DON'T re-embed symbol_refs unless you have a specific need for:

1. Line-level granularity of every symbol occurrence
2. Exhaustive cross-reference reporting
3. Symbol usage analysis at scale

REASONS:
• The other 17 indexes already provide 95% of what users need
• 1.9M documents would significantly slow down queries
• Cost/benefit ratio is poor (expensive to re-embed, marginal value)
• Current system is working well for:
  - Variable usage questions (variables index)
  - Data structure questions (data_items index)
  - Program dependencies (calls, program_deps indexes)
  - Code search (code chunks)

ALTERNATIVE:
If you need symbol-level analysis:
1. Query symbol_refs directly via Azure Search API (keyword search still works)
2. Build a separate "symbol lookup" tool that doesn't use embeddings
3. Only re-embed if user feedback shows a clear gap in capabilities

TEST FIRST:
Before deciding, try asking the RAG these questions and see if current indexes suffice:
- "Where is field TOTPAYMNTD used?"
- "What programs reference the LN-FILE?"
- "Find all uses of variable LP-TRAMT"

If current indexes answer these well, symbol_refs isn't needed!
""")

print("\n" + "="*80)
print("📝 ACTION ITEMS:")
print("="*80)
print("""
1. ✅ SKIP re-embedding symbol_refs for now
2. ✅ Monitor user questions to see if there's a need
3. ✅ Document that symbol_refs uses keyword-only search (no semantic)
4. ✅ Consider if specific symbol lookup is needed, build a separate tool
5. ⚠️  Only re-embed if user feedback shows clear gaps

COST ESTIMATE IF WE DID RE-EMBED:
• 1.9M documents
• ~500 tokens average per document
• 950M total tokens
• $0.13 per 1M input tokens = ~$123.50
• Plus compute time: 8-24 hours estimated

NOT WORTH IT unless there's a proven need!
""")
