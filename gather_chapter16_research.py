#!/usr/bin/env python3
"""
Chapter 16: Real-World Conversion Examples
Research script to gather information for case studies of actual OTIS COBOL conversions.
Focuses on practical examples: copybook utilities, workflow conversions, and specific program APIs.
"""

import json
from pathlib import Path
from otis_rag import OTISRAG

# Initialize RAG system
print("Initializing OTISRAG for Chapter 16 research (Case Studies)...")
rag = OTISRAG()

# Chapter 16: Real-World Conversion Examples
questions = [
    # Section 16.1: Case Study: Converting TIMEIO.CPY Utility (Q1-17)
    "What is TIMEIO.CPY in the OTIS COBOL codebase, what functionality does it provide, and why is it a good candidate for conversion to a C# utility?",
    "What date and time manipulation operations does TIMEIO.CPY perform, and how are these typically used by COBOL programs in OTIS?",
    "How does COBOL handle date arithmetic in TIMEIO.CPY (e.g., REDEFINES, COMP-3 fields), and what modern C# equivalents exist (DateTime, DateTimeOffset, TimeSpan)?",
    "What are the key differences between COBOL's date representations (Julian dates, packed decimal) and C#'s DateTime structures when converting TIMEIO.CPY?",
    "How would you design a C# TimeUtility class to replace TIMEIO.CPY functionality, including method signatures and error handling?",
    "What unit tests would you create for the C# TimeUtility to ensure it matches TIMEIO.CPY behavior exactly (leap years, century handling, edge cases)?",
    
    "How does TIMEIO.CPY handle time zone conversions or daylight saving time, and how would you implement this in C# using TimeZoneInfo?",
    "What performance considerations exist when converting high-frequency date/time operations from COBOL to C#, especially for batch processing?",
    "How would you handle date validation errors in the C# version (e.g., invalid dates like February 30th) compared to COBOL's approach?",
    "What documentation should accompany the C# TimeUtility to help developers migrate from TIMEIO.CPY calls?",
    "How would you version the C# TimeUtility API to support gradual migration from COBOL without breaking existing dependencies?",
    
    "What integration testing strategy would verify that C# TimeUtility produces identical outputs to TIMEIO.CPY for a large sample of dates?",
    "How do you handle century windowing (2-digit year conversion) in C# when migrating from COBOL's TIMEIO.CPY logic?",
    "What common pitfalls exist when converting date/time utilities from COBOL to C#, particularly around implicit conversions and rounding?",
    "How would you expose the C# TimeUtility as a REST API for use by other systems, and what would the endpoint design look like?",
    "What metrics or logging should the C# TimeUtility include to monitor usage patterns and detect issues after deployment?",
    "How would you handle backward compatibility if TIMEIO.CPY behavior changes are discovered after C# conversion is complete?",
    
    # Section 16.2: Case Study: Loan Payment Workflow Conversion (Q18-34)
    "What are the key steps in a typical loan payment workflow in the OTIS COBOL system (from payment receipt to balance update)?",
    "Which COBOL programs are involved in loan payment processing, and how do they interact (sequential execution, file passing, shared data areas)?",
    "What data structures and file formats are used in the COBOL loan payment workflow (VSAM files, copybooks, record layouts)?",
    "How would you model the loan payment workflow as a C# API, breaking monolithic COBOL programs into microservices or endpoints?",
    
    "What validation rules exist in the COBOL payment processing (payment amount, account status, late fees), and how would you implement these in C#?",
    "How does the COBOL payment workflow handle transactions and rollback in case of errors, and what modern equivalents exist in C# (EF transactions, distributed transactions)?",
    "What business rules around interest calculation, principal allocation, and escrow handling exist in COBOL, and how do you preserve these exactly in C#?",
    "How would you convert COBOL's sequential file processing (reading payment batch files) to C# with database operations or message queues?",
    "What error handling patterns exist in the COBOL payment workflow (return codes, error files), and how do you modernize these in C# (exceptions, logging, retry policies)?",
    
    "How do you test the C# payment workflow against the COBOL version to ensure identical business logic (regression testing, data comparison)?",
    "What performance optimizations are possible in C# that weren't feasible in COBOL for payment processing (parallel processing, async operations, caching)?",
    "How would you expose the payment workflow as REST APIs (POST /payments, GET /payments/{id}, GET /accounts/{id}/payment-history)?",
    "What security considerations exist for the payment API (authentication, authorization, PCI compliance, audit logging)?",
    "How would you handle idempotency in the C# payment API (preventing duplicate payment processing from retries)?",
    "What monitoring and alerting should be implemented for the payment workflow (failed payments, unusual patterns, system health)?",
    
    "How do you coordinate the cutover from COBOL payment processing to C# in a live production environment without disrupting operations?",
    "What rollback strategy exists if critical issues are discovered in the C# payment workflow after deployment?",
    
    # Section 16.3: Case Study: Converting MBUNR1H.CBL to Branch Totals API (Q35-50)
    "What is MBUNR1H.CBL in the OTIS COBOL system, what business function does it serve, and what makes it a good candidate for API conversion?",
    "What inputs does MBUNR1H.CBL accept (files, parameters, control cards), and what outputs does it produce (reports, files, database updates)?",
    "What are the key data structures (copybooks, working storage) used by MBUNR1H.CBL, and how would you model these in C# classes?",
    
    "How does MBUNR1H.CBL aggregate or summarize data (branch totals, account summaries), and what SQL or LINQ queries would replicate this in C#?",
    "What business logic exists in MBUNR1H.CBL for calculating totals, applying filters, or handling exceptions, and how do you preserve this in C#?",
    "How would you design REST API endpoints to replace MBUNR1H.CBL functionality (GET /branches/{id}/totals, POST /reports/branch-summary)?",
    "What query parameters should the Branch Totals API support (date ranges, account types, branch hierarchies) to match COBOL flexibility?",
    "How would you optimize the C# implementation for performance compared to the COBOL batch process (indexing, caching, pagination)?",
    
    "What error handling exists in MBUNR1H.CBL (data validation, file not found, calculation errors), and how do you modernize this in the C# API (HTTP status codes, error responses)?",
    "How do you test the C# Branch Totals API against MBUNR1H.CBL outputs to ensure accuracy (comparing report files, validating calculations)?",
    "What authorization model should the Branch Totals API implement (role-based access, branch-level permissions, data filtering by user)?",
    "How would you version the Branch Totals API to support multiple consumers with different requirements (v1 for legacy compatibility, v2 for enhanced features)?",
    "What response format should the API use (JSON, XML, CSV) to accommodate different consumer needs (dashboards, reports, integrations)?",
    
    "How do you document the Branch Totals API (Swagger/OpenAPI) to help consumers understand available endpoints, parameters, and response formats?",
    "What caching strategy would improve API performance for frequently requested branch totals (Redis, in-memory cache, ETag/conditional requests)?",
    "How would you implement rate limiting or throttling on the Branch Totals API to prevent abuse or overload?"
]

# Output file
output_file = Path("chapter16_case_studies_research.json")

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
print("Chapter 16 Research Complete!")
print(f"Total Q&A pairs: {len(results)}")
print(f"Output file: {output_file}")
print(f"{'='*80}")
