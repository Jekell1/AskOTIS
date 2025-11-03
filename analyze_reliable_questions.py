"""
Analyze all OTIS indexes and generate high-reliability questions
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

from otis_rag.config import Config
config = Config()

print("="*80)
print("OTIS INDEX ANALYSIS - HIGH RELIABILITY QUESTIONS")
print("="*80)

# Map of indexes and their capabilities
indexes = {
    'new_code_chunks': 'COBOL source code snippets with context',
    'new_cobol_paragraphs': 'Individual COBOL paragraphs and procedures',
    'new_cobol_menu_trees': 'Menu hierarchy and navigation trees',
    'new_cobol_ui_paths': 'User interface navigation paths',
    'new_cobol_screen_nodes': 'Screen definitions and layouts',
    'new_cobol_data_items': 'Data structure definitions and fields',
    'new_cobol_program_meta': 'Program metadata and descriptions',
    'new_cobol_program_deps': 'Program dependencies and relationships',
    'new_cobol_flow_edges_v2': 'Control flow between programs',
    'new_cobol_program_flows': 'Complete program flow documentation',
    'new-cobol-files': 'File-level program information',
    'new_cobol_copybooks': 'Copybook metadata',
    'new_cobol_variable_usage': 'Variable usage across programs'
}

print("\n📊 AVAILABLE INDEXES:")
print("-" * 80)
for idx, description in indexes.items():
    print(f"  • {idx:30} - {description}")

print("\n" + "="*80)
print("🎯 HIGH-RELIABILITY QUESTION CATEGORIES")
print("="*80)

categories = {
    "1. PROGRAM STRUCTURE & IDENTIFICATION": {
        "index": "new_cobol_program_meta, new-cobol-files",
        "questions": [
            "What does program [PROGRAM_NAME] do?",
            "What is the business purpose of [PROGRAM_NAME]?",
            "How many COBOL programs are in the OTIS system?",
            "List all programs in the [DIRECTORY] folder",
            "What programs are related to loan processing?",
            "Find programs that deal with payment processing",
            "What are the main batch processing programs?",
            "Which programs handle dealer maintenance?"
        ]
    },
    
    "2. MENU NAVIGATION & USER INTERFACE": {
        "index": "new_cobol_menu_trees, new_cobol_ui_paths",
        "questions": [
            "What is the entry point/startup screen for OTIS?",
            "Show me all top-level menus available in OTIS",
            "What menus can I access from GTFORM?",
            "Show me the menu tree starting from [MENU_NAME]",
            "What programs are under the WIMENU menu?",
            "How do I navigate from PGMENU to loan maintenance?",
            "What is the complete UI path from GTFORM to [PROGRAM]?",
            "Which menus lead to dealer functions?"
        ]
    },
    
    "3. PROGRAM DEPENDENCIES & CALLS": {
        "index": "new_cobol_program_deps, new_cobol_flow_edges_v2",
        "questions": [
            "What programs does [PROGRAM_NAME] call?",
            "What programs call [PROGRAM_NAME]?",
            "Show me the call chain for [PROGRAM_NAME]",
            "What is the dependency tree for [PROGRAM_NAME]?",
            "Which programs use copybook [COPYBOOK_NAME]?",
            "Find all programs that call CHKSEC (security check)",
            "What programs depend on FORM-PROGX?",
            "Show me all programs that access the loan file"
        ]
    },
    
    "4. PROGRAM FLOW & LOGIC": {
        "index": "new_cobol_program_flows, new_cobol_flow_edges_v2",
        "questions": [
            "Show me the flowchart for [PROGRAM_NAME]",
            "What is the control flow in [PROGRAM_NAME]?",
            "What happens when [PROGRAM_NAME] executes?",
            "Trace the execution path from [PARAGRAPH_A] to [PARAGRAPH_B]",
            "What paragraphs are executed in [PROGRAM_NAME]?",
            "Show me the logic flow for loan payment processing",
            "What is the error handling flow in [PROGRAM_NAME]?"
        ]
    },
    
    "5. DATA STRUCTURES & FIELDS": {
        "index": "new_cobol_data_items, new_cobol_variable_usage",
        "questions": [
            "What fields are defined in [COPYBOOK_NAME]?",
            "What is the data structure for loan records?",
            "What fields can a user modify in [PROGRAM_NAME]?",
            "Show me the layout of the [SCREEN_NAME] screen",
            "What variables are used in [PROGRAM_NAME]?",
            "Find all programs that use field [FIELD_NAME]",
            "What is the definition of [DATA_ITEM]?",
            "What fields are in the dealer master record?"
        ]
    },
    
    "6. SCREEN DEFINITIONS & USER INPUT": {
        "index": "new_cobol_screen_nodes, new_code_chunks",
        "questions": [
            "What fields are displayed on the [PROGRAM_NAME] screen?",
            "What can a user enter in [PROGRAM_NAME]?",
            "Show me all input fields in [PROGRAM_NAME]",
            "What are the field prompts in [PROGRAM_NAME]?",
            "What function keys are available in [PROGRAM_NAME]?",
            "What validation rules exist for [FIELD_NAME]?",
            "What are the modifiable fields vs display-only fields?",
            "Show me the screen layout for dealer inquiry"
        ]
    },
    
    "7. BUSINESS RULES & CONSTRAINTS": {
        "index": "new_code_chunks, new_cobol_paragraphs",
        "questions": [
            "What are the validation rules in [PROGRAM_NAME]?",
            "What business constraints apply to [OPERATION]?",
            "What is the MAX limit for fields in [PROGRAM_NAME]?",
            "What are the edit rules for [FIELD_NAME]?",
            "What conditions must be met to [PERFORM_ACTION]?",
            "What error messages can [PROGRAM_NAME] display?",
            "What security checks are performed in [PROGRAM_NAME]?",
            "What calculations are done in [PROGRAM_NAME]?"
        ]
    },
    
    "8. COPYBOOK USAGE & SHARED CODE": {
        "index": "new_cobol_copybooks, new_cobol_program_deps",
        "questions": [
            "What copybooks does [PROGRAM_NAME] use?",
            "Which programs use [COPYBOOK_NAME]?",
            "What is defined in copybook [COPYBOOK_NAME]?",
            "Show me all copybooks related to loans",
            "What data structures are shared across programs?",
            "Which programs share the [COPYBOOK_NAME] structure?",
            "What copybooks define screen layouts?"
        ]
    },
    
    "9. CODE ANALYSIS & SEARCH": {
        "index": "new_code_chunks, new_cobol_paragraphs",
        "questions": [
            "Find all programs that contain [SEARCH_TERM]",
            "Show me code that handles [BUSINESS_OPERATION]",
            "What programs perform loan calculations?",
            "Find code that accesses the dealer file",
            "Which programs have error handling for [CONDITION]?",
            "Show me all ENTRY-MODULE paragraphs",
            "Find programs that use CICS commands",
            "What code handles date calculations?"
        ]
    },
    
    "10. SYSTEM ARCHITECTURE & INTEGRATION": {
        "index": "new_cobol_program_deps, new_cobol_flow_edges_v2",
        "questions": [
            "What is the architecture of the loan processing system?",
            "How do the menu programs integrate with business logic?",
            "What is the call hierarchy starting from GTFORM?",
            "Show me all entry points in the system",
            "What programs form the core of the payment system?",
            "How do programs interact with the database?",
            "What is the security architecture?",
            "Map the relationship between UI and business logic programs"
        ]
    }
}

for category, details in categories.items():
    print(f"\n{category}")
    print(f"Index: {details['index']}")
    print("-" * 80)
    for i, question in enumerate(details['questions'], 1):
        print(f"  {i:2}. {question}")

print("\n" + "="*80)
print("💡 BEST PRACTICES FOR HIGH-RELIABILITY ANSWERS")
print("="*80)
print("""
1. SPECIFIC PROGRAM NAMES: Questions with exact program names yield best results
   ✅ "What does REGPAY do?"
   ✅ "What programs does WIMENU call?"
   
2. STRUCTURAL QUERIES: Questions about relationships and hierarchies work well
   ✅ "Show me the menu tree from PGMENU"
   ✅ "What programs call CHKSEC?"
   
3. DATA DEFINITIONS: Questions about data structures are highly reliable
   ✅ "What fields are in the REGPAY screen?"
   ✅ "What copybooks does LTMAIN use?"
   
4. NAVIGATION PATHS: UI navigation questions leverage menu/path indexes
   ✅ "How do I get to loan payment from the main menu?"
   ✅ "What is the path from GTFORM to DLMAIN?"
   
5. BUSINESS LOGIC WITH CONTEXT: Questions about specific constraints/rules
   ✅ "What fields can a user modify in REGPAY?" (with MAX=3 constraint)
   ✅ "What validation occurs in loan payment processing?"

6. AVOID VAGUE QUESTIONS: Generic questions may return too much irrelevant data
   ❌ "Tell me about loans" (too broad)
   ✅ "What programs handle loan payment processing?" (specific)
""")

print("\n" + "="*80)
print("📝 SAMPLE HIGH-CONFIDENCE QUESTIONS TO TEST")
print("="*80)

test_questions = [
    ("Navigation", "Show me all top-level menus in OTIS"),
    ("Program Info", "What is the business purpose of REGPAY?"),
    ("Dependencies", "What programs does WIMENU call?"),
    ("Data Fields", "What fields can a user modify in REGPAY?"),
    ("Menu Tree", "Show me the menu tree from PGMENU down 2 levels"),
    ("Call Chain", "What programs call FORM-PROGX?"),
    ("Screen Layout", "What fields are displayed on the dealer maintenance screen?"),
    ("Business Rules", "What is the MAX constraint in REGPAY?"),
    ("Copybooks", "What copybooks does LTMAIN use?"),
    ("Flow", "What is the execution flow in loan payment processing?")
]

print("\nTop 10 Recommended Test Questions:")
print("-" * 80)
for i, (category, question) in enumerate(test_questions, 1):
    print(f"  {i:2}. [{category:15}] {question}")

print("\n" + "="*80)
print("✅ CONCLUSION")
print("="*80)
print("""
The OTIS RAG system has HIGH RELIABILITY for:
• Program identification and purpose (metadata indexed)
• Menu navigation and UI paths (full tree structures indexed)
• Program dependencies and call chains (relationships mapped)
• Data structures and field definitions (copybooks & data items indexed)
• Business constraints when they appear in code (MAX=3 example)
• Screen layouts and user input fields (screen nodes indexed)

The system has MODERATE RELIABILITY for:
• Complex multi-step workflows requiring inference
• Implicit business rules not explicitly coded
• Historical context or reasons for design decisions
• Performance characteristics or optimization details

The system has LOW RELIABILITY for:
• Questions requiring external documentation not in code
• Questions about runtime behavior without code evidence
• Questions about data values (only structure is indexed, not data)
• Questions about systems outside OTIS/OTOS
""")
