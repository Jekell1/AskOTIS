#!/usr/bin/env python3
"""
Appendices Research Script
Gather information for comprehensive appendices including:
- Glossary of Terms
- Reference Tables (COBOL to C# Mapping)
- Sample Code Listings
"""

import json
from pathlib import Path
from otis_rag import OTISRAG

# Initialize RAG system
print("Initializing OTISRAG for Appendices research...")
rag = OTISRAG()

# Appendices: 50 questions covering all three sections
questions = [
    # Appendix A: Glossary of Terms (Q1-20)
    "What are the key COBOL-specific terms (PIC, COMP, COMP-3, REDEFINES, ODO, etc.) that need glossary entries for developers learning modernization?",
    "What mainframe and batch processing terms (JCL, JES, CICS, TSO, VSAM, etc.) are essential for understanding the legacy OTIS environment?",
    "What modernization and architectural terms (microservices, API gateway, RESTful, GraphQL, containers, orchestration) are key for the target C# system?",
    "What data migration and ETL terms (extraction, transformation, loading, reconciliation, cutover) are important for understanding the conversion process?",
    "What testing terminology (unit testing, integration testing, regression testing, UAT, smoke testing, load testing) is essential for quality assurance?",
    
    "What DevOps and deployment terms (CI/CD, pipeline, blue-green deployment, canary release, feature flags) are relevant to modernization?",
    "What C# and .NET-specific terms (namespaces, LINQ, async/await, Entity Framework, dependency injection) need explanation?",
    "What database terms (normalization, indexes, stored procedures, migrations, ORM) are important for data architecture discussions?",
    "What security terms (authentication, authorization, OAuth, JWT, encryption, secrets management) are relevant to API development?",
    "What performance and scalability terms (caching, load balancing, horizontal scaling, throughput, latency) need definition?",
    
    "What COBOL file organization terms (sequential, indexed, relative, KSDS, ESDS, RRDS) need glossary entries?",
    "What programming paradigm terms (procedural, object-oriented, functional, declarative) help explain the COBOL-to-C# shift?",
    "What agile and project management terms (sprint, backlog, user story, technical debt, MVP) are relevant to modernization projects?",
    "What cloud computing terms (IaaS, PaaS, SaaS, serverless, multi-tenancy) apply to modern deployment strategies?",
    "What API design terms (endpoint, resource, payload, idempotency, versioning, rate limiting) are essential for API development?",
    
    "What monitoring and observability terms (telemetry, metrics, logs, traces, APM, alerting) are important for production systems?",
    "What data type and encoding terms (EBCDIC, ASCII, Unicode, UTF-8, packed decimal, binary) need explanation for conversion?",
    "What transaction processing terms (ACID, consistency, isolation, rollback, commit) are relevant to business logic preservation?",
    "What legacy system terms (technical debt, brownfield, greenfield, strangler pattern, big bang) help frame modernization approaches?",
    "What compliance and regulatory terms (audit trail, data retention, PCI-DSS, GDPR, SOX) are relevant to financial system modernization?",
    
    # Appendix B: Reference Tables - COBOL to C# Mapping (Q21-35)
    "What are the direct mappings from COBOL data types (PIC 9, PIC X, PIC S9 COMP, PIC S9 COMP-3) to C# types (int, string, decimal)?",
    "How do COBOL numeric picture clauses (PIC 9(5), PIC 9(7)V99, PIC S9(9) COMP-3) map to C# numeric types with appropriate precision?",
    "What are the mappings for COBOL alphanumeric and edited picture clauses (PIC X(n), PIC Z(n), PIC $(n).99) to C# string formatting?",
    "How do COBOL file organization types (SEQUENTIAL, INDEXED, RELATIVE, VSAM KSDS/ESDS/RRDS) map to modern C# data access patterns?",
    "What are the C# equivalents for COBOL verbs (MOVE, ADD, COMPUTE, IF, PERFORM, EVALUATE, CALL)?",
    
    "How do COBOL condition names (88-level entries) map to C# enums, constants, or boolean properties?",
    "What are the C# patterns for COBOL PERFORM structures (PERFORM UNTIL, PERFORM VARYING, PERFORM TIMES)?",
    "How do COBOL copybook COPY statements map to C# using directives, namespaces, and shared class libraries?",
    "What are the mappings for COBOL file operations (OPEN, READ, WRITE, CLOSE, REWRITE, DELETE) to C# file I/O or database operations?",
    "How do COBOL error handling patterns (FILE STATUS, SQLCODE, ON SIZE ERROR) map to C# exception handling and validation?",
    
    "What are the C# equivalents for COBOL string operations (STRING, UNSTRING, INSPECT) using StringBuilder, LINQ, Regex?",
    "How do COBOL table handling operations (OCCURS, SEARCH, SEARCH ALL) map to C# arrays, lists, LINQ queries, and dictionaries?",
    "What are the mappings for COBOL sorting operations (SORT, MERGE, INPUT/OUTPUT PROCEDURE) to C# LINQ OrderBy or database operations?",
    "How do COBOL paragraph and section structures map to C# methods, functions, and class organization?",
    "What are the C# equivalents for COBOL date/time operations (ACCEPT DATE, ACCEPT TIME, date arithmetic) using DateTime and TimeSpan?",
    
    # Appendix C: Sample Code Listings (Q36-50)
    "What is a representative COBOL program from OTIS (e.g., simple calculation or validation) and its complete C# equivalent showing modern patterns?",
    "What does a typical COBOL copybook look like in OTIS, and what is the equivalent C# class definition with properties and data annotations?",
    "What is an example of COBOL sequential file processing (read loop with validation) and the equivalent C# code using StreamReader or database query?",
    "What does COBOL COMP-3 (packed decimal) field handling look like, and what is the C# code for reading/writing these fields?",
    "What is an example of COBOL REDEFINES usage for data structure variants, and the C# equivalent using unions, inheritance, or discriminated types?",
    
    "What does a COBOL PERFORM loop with varying index look like, and what is the idiomatic C# equivalent using for/foreach/LINQ?",
    "What is an example of COBOL condition-name (88-level) usage, and the C# equivalent using enums or named constants?",
    "What does COBOL error handling with FILE STATUS or ON SIZE ERROR look like, and what is the C# equivalent using try-catch and validation?",
    "What is an example of COBOL string manipulation (STRING/UNSTRING), and the modern C# equivalent using string methods or LINQ?",
    "What does COBOL table searching (SEARCH ALL) look like, and what is the C# equivalent using LINQ, dictionaries, or binary search?",
    
    "What is an example of COBOL SORT operation with input/output procedures, and the C# equivalent using LINQ OrderBy or database operations?",
    "What does a COBOL batch program with transaction processing look like, and what is the C# API equivalent with proper transaction handling?",
    "What is an example of COBOL date/time manipulation, and the modern C# equivalent using DateTime, TimeSpan, and date formatting?",
    "What does COBOL JCL for running a batch program look like, and what is the modern equivalent (PowerShell, Docker, Kubernetes job)?",
    "What is an example of COBOL database access (embedded SQL or CICS), and the modern C# equivalent using Entity Framework or Dapper?"
]

# Output file
output_file = Path("appendices_research.json")

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
print("Appendices Research Complete!")
print(f"Total Q&A pairs: {len(results)}")
print(f"Output file: {output_file}")
print(f"{'='*80}")
