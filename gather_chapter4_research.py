#!/usr/bin/env python3
"""
Chapter 4: Mapping the Legacy Codebase - Research Data Gathering
Asks 50 targeted questions to understand OTIS inventory, relationships, and business rules
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    # Chapter 4: Mapping the Legacy Codebase - 50 Questions
    questions = [
        # Section 4.1: Inventory of Programs, Copybooks, and Files (Q1-Q12)
        "What is the total count of COBOL programs in the OTIS system? List the top 20 programs by size or complexity with their line counts and primary purposes.",
        
        "What are all the different program categories or types in OTIS (batch, online, utility, report)? Provide counts and examples for each category.",
        
        "What is the complete inventory of copybooks in the system? How many copybooks exist and what are the top 15 most widely used copybooks with their usage counts?",
        
        "What are the different copybook categories (working storage, file definitions, SQL, screens, parameters)? Provide counts and key examples for each category.",
        
        "What data files does the OTIS system use? List all major data files with their organization type (indexed, sequential, relative) and primary access programs.",
        
        "What SQL tables and databases does OTIS access? List all SQL-related copybooks and the tables they reference with their purposes.",
        
        "What screen files or screen definitions exist in the system? How are screen layouts organized and stored?",
        
        "What report programs exist in OTIS? List major report generators with their outputs and scheduling.",
        
        "What utility programs exist for data maintenance, conversion, or system administration? List key utilities with their functions.",
        
        "What are the naming conventions used across programs, copybooks, and files? Are there different prefixes or patterns that indicate program type or subsystem?",
        
        "What is the total estimated lines of code in the OTIS system? Break down by programs vs copybooks vs other artifacts.",
        
        "What documentation files, comments standards, or metadata exist to describe the codebase inventory?",
        
        # Section 4.2: Call Chains and Program Relationships (Q13-Q25)
        "What are the complete call chain patterns in OTIS? Show the deepest call chains with full paths from entry point to leaf programs.",
        
        "What programs serve as entry points for users (menus, main programs)? List all 30+ entry programs with their subsystem areas.",
        
        "What programs are called most frequently across the system? Provide a ranked list of top 20 most-called programs with caller counts.",
        
        "What are the core utility programs that multiple subsystems depend on? List programs like SYSTEM, SETENV, etc. with their roles and caller lists.",
        
        "What are the program-to-program call relationships? Show examples of major call patterns (menu→maintenance→utility, batch→processor→file handler).",
        
        "What programs call external systems or APIs? List programs that interface with external databases, services, or systems.",
        
        "What are the circular dependencies or recursive call patterns? Identify any programs that have complex interdependencies.",
        
        "What programs are never called by other programs (potential dead code or entry points)? List standalone programs.",
        
        "What programs have the highest fan-out (call many other programs)? What programs have highest fan-in (called by many)?",
        
        "How are copybooks shared across programs? Show copybook dependency patterns - which copybooks are included together frequently?",
        
        "What are the subsystem boundaries based on call patterns? Can you identify distinct modules like Loan Processing, Collections, Payments, Reports?",
        
        "What batch job chains exist? Show sequences of batch programs that must run in specific orders.",
        
        "What programs share common data files? Show file-based coupling between programs.",
        
        # Section 4.3: Data Structures and File Formats (Q26-Q37)
        "What are the key data structures defined in copybooks? List the 20 most important record layouts with their field counts and purposes.",
        
        "What are the indexed file structures? Show the primary keys, alternate keys, and record layouts for major indexed files.",
        
        "What are the SQL table structures? Show table definitions including primary keys, foreign keys, and field types from SQL copybooks.",
        
        "What are the data type patterns used across the system? List common PICTURE clauses (PIC 9, PIC X, PIC S9, etc.) and their typical uses.",
        
        "What are the common field naming conventions? Show patterns like prefixes, suffixes that indicate data type or purpose.",
        
        "What COMP/COMP-3 (binary/packed decimal) fields exist for performance optimization? Where are computational fields used?",
        
        "What are the date and time field formats used throughout the system? Show all date representations (YYMMDD, CCYYMMDD, Julian, etc.).",
        
        "What are the currency and financial field formats? Show how monetary values, interest rates, and percentages are stored.",
        
        "What REDEFINES clauses exist for complex data structures or unions? Show examples of overlapping field definitions.",
        
        "What OCCURS (array) structures exist? Show table-like data structures within records.",
        
        "What are the file record lengths? List major files with their record sizes and any variable-length record handling.",
        
        "What data validation is embedded in field definitions (VALUE clauses, 88-levels, conditionals)?",
        
        # Section 4.4: Identifying Business Rules and Validation Logic (Q38-Q50)
        "What validation routines exist for data entry and processing? List common validation patterns across programs.",
        
        "What business rules are coded in EVALUATE/IF statements? Show examples of decision logic for loan processing, payments, or approvals.",
        
        "What are the validation copybooks that contain reusable business rules? List copybooks with validation logic like FERRORS.CPY.",
        
        "What error handling patterns exist? Show how errors are detected, logged, and communicated to users.",
        
        "What calculation routines exist for financial operations (interest, payments, fees, penalties)? Show key calculation sections.",
        
        "What are the business rules for loan approval, disbursement, and payoff? Show the logic flow for core loan operations.",
        
        "What validation rules exist for customer data (SSN, addresses, phone numbers, dates of birth)?",
        
        "What business rules govern batch processing cutoffs, posting dates, and effective dates?",
        
        "What audit trail and compliance rules are implemented? Show logging, tracking, and regulatory compliance logic.",
        
        "What authorization and security rules exist? Show access control logic based on user roles, branch restrictions, or transaction limits.",
        
        "What data integrity rules are enforced (referential integrity, mandatory fields, field ranges)?",
        
        "What transaction rollback or compensation logic exists for error recovery?",
        
        "What are the business rules documented in comments, help text, or field descriptions? Extract embedded business knowledge from code comments.",
    ]
    
    print("=" * 80)
    print("CHAPTER 4 RESEARCH: MAPPING THE LEGACY CODEBASE")
    print("=" * 80)
    print(f"Total questions to process: {len(questions)}")
    print("=" * 80)
    print()
    
    results = []
    start_time = time.time()
    
    for i, question in enumerate(questions, 1):
        print(f"\n[Question {i}/{len(questions)}]")
        print(f"Q: {question}")
        print("-" * 80)
        
        q_start = time.time()
        result = rag.ask_with_timing(question, verbose=False)
        answer = result.get("answer", "")
        context_docs = result.get("context_docs", [])
        timing_data = result.get("timing", {})
        q_elapsed = timing_data.get("total_seconds", 0)
        
        print(f"A: {answer[:200]}..." if len(answer) > 200 else f"A: {answer}")
        print(f"⏱️  Query time: {q_elapsed:.2f}s")
        
        results.append({
            "question_number": i,
            "question": question,
            "answer": answer,
            "sources_count": len(context_docs),
            "query_time_seconds": round(q_elapsed, 2)
        })
        
        # Save progress after each question
        with open("chapter4_mapping_research.json", "w", encoding="utf-8") as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
    
    total_time = time.time() - start_time
    
    print("\n" + "=" * 80)
    print("RESEARCH COMPLETE!")
    print("=" * 80)
    print(f"Total questions answered: {len(results)}")
    print(f"Total time: {total_time:.2f} seconds ({total_time/60:.2f} minutes)")
    print(f"Average time per question: {total_time/len(results):.2f} seconds")
    print(f"Results saved to: chapter4_mapping_research.json")
    print("=" * 80)

if __name__ == "__main__":
    main()
