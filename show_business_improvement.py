"""
Demonstrate the improvement in business-focused answers.
Shows before vs after the prompt enhancement.
"""

print("=" * 80)
print("BUSINESS-FOCUSED RAG IMPROVEMENT DEMO")
print("=" * 80)
print()

print("📋 PROBLEM:")
print("   Vague questions like 'What does OTIS do?' were returning technical")
print("   implementation details instead of business purpose.")
print()

print("💡 SOLUTION:")
print("   Enhanced the system prompt to explicitly prioritize BUSINESS PURPOSE")
print("   before technical details.")
print()

print("=" * 80)
print("BEFORE (OLD PROMPT)")
print("=" * 80)
print("""
Question: "What does the OTIS system do?"

Answer (Technical-First):
┌────────────────────────────────────────────────────────────┐
│ The OTIS system is a COBOL-based application designed to   │
│ manage and automate operational tasks related to file and  │
│ data handling, particularly in the context of path and     │
│ file management for business workflows.                    │
│                                                             │
│ Key Functions:                                             │
│ • System Command Execution                                 │
│ • File Path Management and Merging                         │
│ • Error Handling and Messaging                             │
│                                                             │
│ Technical Details:                                         │
│   MOVE PST-PATH-ALL TO PST-WS-RM-FILE                      │
│   MOVE PST-WS-RM-COMMAND TO SYSTEM-BUF                     │
│   PERFORM SYSTEM-CALL                                      │
└────────────────────────────────────────────────────────────┘

❌ Problem: Focused on COBOL implementation, not business value
""")

print("=" * 80)
print("AFTER (NEW PROMPT)")
print("=" * 80)
print("""
Question: "What does the OTIS system do?"

Answer (Business-First):
┌────────────────────────────────────────────────────────────┐
│ **Business Purpose:**                                       │
│                                                             │
│ OTIS is a loan servicing platform for financial            │
│ institutions. Its primary business function is to manage   │
│ the lifecycle of loans, including:                         │
│                                                             │
│ • Tracking loan balances, payments, and schedules          │
│ • Processing financial transactions (payments,             │
│   disbursements, fees)                                     │
│ • Managing customer accounts and loan records              │
│ • Supporting regulatory compliance and reporting           │
│ • Handling operational tasks such as cash receipts and     │
│   check disbursements                                      │
│                                                             │
│ **Technical Implementation:**                               │
│ • COBOL Programs: Suite of programs handling specific      │
│   loan servicing operations                                │
│ • Data Structures: Copybooks defining layouts              │
│ • System Integration: Routines for system commands         │
│                                                             │
│ Industry Context:                                          │
│ Tailored for financial services industry (banks, credit    │
│ unions, lenders) needing robust loan servicing.            │
└────────────────────────────────────────────────────────────┘

✅ Success: Answers business question FIRST, then technical details
""")

print("=" * 80)
print("KEY IMPROVEMENTS")
print("=" * 80)
print()
print("1. ✅ Business Purpose First")
print("   - Explains what problem OTIS solves")
print("   - Identifies target industry (financial services)")
print("   - Lists core business functions")
print()
print("2. ✅ Clear Structure")
print("   - Section 1: Business Purpose")
print("   - Section 2: Technical Implementation")
print("   - Section 3: Industry Context")
print()
print("3. ✅ Audience-Appropriate")
print("   - Business stakeholders get what they need first")
print("   - Technical details available for developers")
print("   - Context helps both audiences")
print()

print("=" * 80)
print("HOW IT WORKS")
print("=" * 80)
print()
print("Enhanced System Prompt:")
print("┌────────────────────────────────────────────────────────────┐")
print("│ CRITICAL: When asked 'what does X do?', interpret this as:│")
print("│ 1. FIRST: Explain the BUSINESS PURPOSE                    │")
print("│ 2. THEN: Explain the technical implementation             │")
print("│                                                            │")
print("│ For OTIS questions, prioritize:                           │")
print("│ • Business domain (loan servicing)                        │")
print("│ • Business processes (what operations)                    │")
print("│ • Industry context (financial services)                   │")
print("│ • THEN technical details                                  │")
print("└────────────────────────────────────────────────────────────┘")
print()

print("=" * 80)
print("TEST IT YOURSELF")
print("=" * 80)
print()
print("from otis_rag import OTISRAG")
print()
print("rag = OTISRAG()")
print("answer = rag.ask('What does the OTIS system do?')")
print("print(answer)")
print()
print("Expected: Business purpose in first 500 characters!")
print()
print("=" * 80)
