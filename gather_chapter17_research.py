#!/usr/bin/env python3
"""
Chapter 17: Common Pitfalls and How to Avoid Them
Research script to identify typical mistakes, challenges, and mitigation strategies
when converting COBOL systems like OTIS to modern C# architectures.
"""

import json
from pathlib import Path
from otis_rag import OTISRAG

# Initialize RAG system
print("Initializing OTISRAG for Chapter 17 research (Common Pitfalls)...")
rag = OTISRAG()

# Chapter 17: Common Pitfalls and How to Avoid Them
questions = [
    # Section 17.1: Data Loss and Precision Issues (Q1-17)
    "What are common data loss scenarios when converting COBOL COMP-3 (packed decimal) fields to C# decimal or double types?",
    "How can precision be lost when converting COBOL PIC 9(15)V99 fields to C# types, and what best practices prevent this?",
    "What issues arise from converting COBOL REDEFINES clauses to C# unions or different data types, and how do you detect these early?",
    "How can character encoding problems (EBCDIC to ASCII/Unicode) cause data corruption, and what validation catches these issues?",
    "What risks exist when converting COBOL occurs-depending-on (ODO) arrays to C# collections, particularly with boundary conditions?",
    
    "How can implicit truncation in COBOL (e.g., moving 1234 into PIC 999) cause silent data loss, and how do you preserve this behavior or detect it in C#?",
    "What issues arise from COBOL's alphanumeric comparisons (trailing spaces) versus C# string comparisons, and how do you handle these consistently?",
    "How can date conversion errors (Julian dates, century windowing) lead to incorrect calculations, and what testing strategies catch these?",
    "What pitfalls exist when converting COBOL file sorting and merging logic to SQL ORDER BY or LINQ, especially with complex sort keys?",
    "How can missing null handling (COBOL has no nulls) cause runtime exceptions in C#, and what patterns make null handling robust?",
    
    "What data loss can occur during migration from COBOL sequential/VSAM files to relational databases (field lengths, data types)?",
    "How do you detect and prevent data quality issues (orphaned records, referential integrity violations) when migrating legacy data?",
    "What risks exist when converting COBOL binary fields (COMP, COMP-4) to C# integer types, especially regarding byte order and signedness?",
    "How can rounding differences between COBOL and C# decimal arithmetic cause discrepancies in financial calculations?",
    "What issues arise from COBOL's implicit type conversions (numeric to alphanumeric, vice versa) when implemented in strongly-typed C#?",
    "How do you validate that no data is lost during the conversion process, and what reconciliation reports provide confidence?",
    "What backup and rollback strategies protect against catastrophic data loss during cutover from COBOL to C# systems?",
    
    # Section 17.2: Misinterpreting Business Rules (Q18-34)
    "What are common mistakes when interpreting undocumented business logic embedded in COBOL code, and how do you discover the true requirements?",
    "How can edge cases handled implicitly by COBOL (e.g., specific error codes, overflow behavior) be missed in C# implementations?",
    "What risks exist when assuming COBOL's sequential processing logic can be easily parallelized in C#, and what testing reveals these issues?",
    "How can COBOL's use of global variables and file-level state lead to incorrect refactoring in C# (e.g., assuming stateless operations)?",
    
    "What issues arise from misunderstanding COBOL EVALUATE (case) statements with fall-through behavior or complex conditions when converting to C# switch or if-else?",
    "How can incorrect interpretation of COBOL PERFORM statements (loops, subroutines) lead to logic errors in C# methods?",
    "What pitfalls exist when business rules are encoded in JCL (job control language) rather than COBOL, and how do you ensure these are captured?",
    "How can assumptions about COBOL program execution order (batch job sequences) lead to bugs when converted to API-based architecture?",
    "What risks exist when converting COBOL report generation logic to modern APIs without understanding the full formatting and totaling requirements?",
    
    "How do you identify and preserve business rules hidden in copybook structures (field positions, implied relationships) when modeling C# classes?",
    "What issues arise from converting COBOL condition-names (88-levels) to C# enums or constants without understanding their usage context?",
    "How can misinterpreting COBOL's implicit date logic (fiscal years, billing cycles) lead to incorrect C# implementations?",
    "What risks exist when simplifying complex COBOL nested IF statements into C# without fully testing all code paths?",
    "How do you ensure that COBOL programs' error handling (specific SQLCODE or file status checks) is replicated accurately in C# exception handling?",
    
    "What strategies help uncover implicit business rules through stakeholder interviews, code analysis, and legacy system observation?",
    "How can regression testing against COBOL outputs help detect business rule misinterpretations in C# implementations?",
    "What documentation practices help capture discovered business rules to prevent future misinterpretation?",
    
    # Section 17.3: Performance Bottlenecks and Scalability Issues (Q35-50)
    "What performance anti-patterns from COBOL (sequential file scans) can cause severe bottlenecks when naively converted to database queries in C#?",
    "How can N+1 query problems arise when converting COBOL file processing to Entity Framework, and what eager loading strategies prevent this?",
    "What scalability issues occur when converting COBOL batch programs to synchronous API calls without considering timeouts and resource limits?",
    "How can inefficient LINQ queries (caused by translating COBOL logic directly) degrade C# API performance, and what optimization techniques help?",
    
    "What memory issues arise when loading entire COBOL file equivalents into memory in C# (large datasets, unbounded collections)?",
    "How can missing indexes on database tables (converted from COBOL files) cause query performance problems, and how do you identify these early?",
    "What issues arise from not implementing pagination when converting COBOL batch reports to API endpoints that return large result sets?",
    "How can synchronous I/O operations (mimicking COBOL's blocking reads/writes) limit C# API throughput, and when should async/await be used?",
    
    "What concurrency issues can occur in C# that didn't exist in single-threaded COBOL programs (race conditions, deadlocks)?",
    "How do you identify and resolve contention on shared resources (database connections, file locks) when multiple C# API instances replace single COBOL jobs?",
    "What caching strategies help C# APIs achieve better performance than COBOL batch processing for frequently accessed data?",
    "How can improper transaction scoping (too broad or too narrow) cause performance problems or data consistency issues in C# implementations?",
    
    "What monitoring and profiling tools help identify performance bottlenecks in converted C# code (Application Insights, profilers, query analyzers)?",
    "How do you establish performance benchmarks from COBOL systems to measure whether C# implementations meet or exceed expectations?",
    "What load testing strategies reveal scalability issues in C# APIs before production deployment?",
    "How can API rate limiting and throttling be implemented to prevent overload while maintaining acceptable performance for legitimate users?"
]

# Output file
output_file = Path("chapter17_pitfalls_research.json")

# Load existing results if file exists
results = []
if output_file.exists():
    print(f"Loading existing results from {output_file}...")
    with open(output_file, 'r', encoding='utf-8') as f:
        results = json.load(f)
    print(f"Found {len(results)} existing Q&A pairs.")

# Determine starting point
start_idx = len(results)
if start_idx >= len(questions):
    print("All questions already answered!")
else:
    print(f"\nStarting research from question {start_idx + 1}/{len(questions)}...\n")
    
    for i in range(start_idx, len(questions)):
        question = questions[i]
        print(f"\n{'='*80}")
        print(f"Question {i+1}/{len(questions)}")
        print(f"{'='*80}")
        print(f"Q: {question}")
        print("\nQuerying RAG system...")
        
        try:
            response = rag.ask_with_timing(question)
            
            result = {
                "question_number": i + 1,
                "question": question,
                "answer": response['answer'],
                "sources_count": response['timing'].get('num_documents', 0),
                "query_time_seconds": response['timing']
            }
            
            results.append(result)
            
            print(f"✓ Answer received ({result['sources_count']} sources, {result['query_time_seconds']['total_seconds']:.2f}s)")
            
            # Save incrementally
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            print(f"✓ Saved to {output_file}")
            
        except Exception as e:
            print(f"✗ Error processing question {i+1}: {e}")
            break

print(f"\n{'='*80}")
print("Chapter 17 Research Complete!")
print(f"Total Q&A pairs: {len(results)}")
print(f"Output file: {output_file}")
print(f"{'='*80}")
