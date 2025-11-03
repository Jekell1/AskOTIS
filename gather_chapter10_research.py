"""
Chapter 10: API Modeling and Design
Research script to gather data on translating COBOL programs to API endpoints,
mapping data structures to DTOs, and designing CRUD operations and workflows.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 10.1: Translating COBOL Programs to API Endpoints (Q1-18)
        "What are the key principles for identifying which COBOL programs in the OTIS system should become REST API endpoints versus internal service methods?",
        "How do we map COBOL program entry points (CALL statements, screen programs) to API resource endpoints following REST conventions?",
        "What strategy should we use to convert COBOL transaction programs (e.g., APIPAY, REFUPD) into HTTP POST/PUT operations?",
        "How can we identify COBOL programs that perform read-only operations and should become HTTP GET endpoints?",
        "What approach should we take to handle COBOL programs that perform multiple operations (read, validate, update) in a single API design?",
        "How do we design API endpoints for COBOL batch programs that process large datasets or perform bulk operations?",
        "What patterns should we use to represent COBOL screen navigation flows (menu systems, multi-step processes) as API workflows?",
        "How can we convert COBOL file I/O operations (READ, WRITE, REWRITE, DELETE) to RESTful CRUD operations?",
        "What strategy should we use for handling COBOL PERFORM statements and paragraph calls in API method design?",
        "How do we map COBOL copybooks used across multiple programs to shared API resources or microservices?",
        "What approach should we take to design API endpoints for COBOL programs that interact with screens (SCRN1, PGMENU, GTFORM)?",
        "How can we identify and group related COBOL programs into cohesive API controllers or service boundaries?",
        "What patterns should we use to handle COBOL program-to-program calls (CALL statements) in a microservices or API architecture?",
        "How do we design API versioning strategy to support incremental migration from COBOL to C# while maintaining backward compatibility?",
        "What approach should we take for API authentication and authorization based on COBOL security models (user IDs, access levels)?",
        "How can we map COBOL transaction boundaries (COMMIT, ROLLBACK logic) to API transaction scopes and database transactions?",
        "What strategy should we use to handle COBOL error handling (FILE STATUS, error codes) in API error responses (HTTP status codes, error payloads)?",
        "How do we design API rate limiting and throttling to replace COBOL queue management or concurrency controls?",
        
        # Section 10.2: Mapping Data Structures to DTOs and Models (Q19-33)
        "What are the best practices for mapping COBOL data structures (FD entries, 01 levels, group items) to C# DTOs and domain models?",
        "How should we convert COBOL PICTURE clauses (numeric, alphanumeric, edited fields) to appropriate C# data types (int, decimal, string)?",
        "What strategy should we use to handle COBOL REDEFINES clauses when designing C# DTOs (union types, inheritance, separate models)?",
        "How do we map COBOL OCCURS clauses (arrays, tables) to C# collections (List<T>, arrays, dictionaries)?",
        "What approach should we take for converting COBOL hierarchical data structures (nested group items) to nested C# objects or flattened DTOs?",
        "How can we handle COBOL COMP-3 (packed decimal) fields in C# DTOs, ensuring precision and performance?",
        "What patterns should we use to represent COBOL sign conventions (SIGN LEADING/TRAILING) in C# numeric types?",
        "How do we design DTOs for COBOL copybooks that are used across multiple programs (shared data definitions)?",
        "What strategy should we use to handle COBOL date formats (YYMMDD, CCYYMMDD) and convert them to C# DateTime or DateOnly types?",
        "How can we map COBOL conditional fields (88-level condition names) to C# enums or boolean properties?",
        "What approach should we take for handling COBOL filler fields and padding in DTOs (include, exclude, or document)?",
        "How do we design validation attributes for DTOs based on COBOL field definitions (length, range, format)?",
        "What patterns should we use to separate API DTOs from domain models to maintain clean architecture boundaries?",
        "How can we implement AutoMapper or similar tools to map between COBOL-derived DTOs and C# domain entities?",
        "What strategy should we use for API request/response DTOs versus internal data models to optimize API contracts?",
        
        # Section 10.3: Designing CRUD Operations and Business Workflows (Q34-50)
        "What RESTful conventions should we follow when designing CRUD operations for OTIS business entities (loans, payments, accounts)?",
        "How do we design POST endpoints for creating new records based on COBOL file WRITE operations?",
        "What strategy should we use for designing PUT versus PATCH endpoints when converting COBOL REWRITE operations?",
        "How can we design GET endpoints that support filtering, sorting, and pagination for COBOL sequential file reads?",
        "What approach should we take for designing DELETE endpoints that handle COBOL file DELETE operations and referential integrity?",
        "How do we handle complex business workflows (multi-step COBOL processes) in API design using orchestration or workflow patterns?",
        "What patterns should we use to represent COBOL validation logic (field edits, business rules) in API request validation?",
        "How can we design API endpoints that support both synchronous operations (immediate response) and asynchronous processing (COBOL batch equivalents)?",
        "What strategy should we use for implementing idempotency in API operations to handle retry scenarios common in legacy systems?",
        "How do we design API error responses that provide meaningful feedback comparable to COBOL error messages and FILE STATUS codes?",
        "What approach should we take for designing bulk operations APIs that replace COBOL batch processing programs?",
        "How can we implement API transaction management to ensure consistency across multiple operations (similar to COBOL commit units)?",
        "What patterns should we use for implementing optimistic or pessimistic locking in APIs to handle concurrent updates?",
        "How do we design APIs to support partial updates and field-level modifications without requiring full object replacement?",
        "What strategy should we use for implementing audit logging in APIs to track changes similar to COBOL audit trails?",
        "How can we design search and query APIs that replace COBOL indexed file access and alternate key lookups?",
        "What approach should we take for implementing business workflow state machines in APIs based on COBOL process flows?"
    ]
    
    results = []
    
    print(f"Starting Chapter 10 research: {len(questions)} questions")
    print("=" * 60)
    
    for i, question in enumerate(questions, 1):
        print(f"\nProcessing Q{i}/50: {question[:80]}...")
        try:
            result = rag.ask_with_timing(question)
            
            qa_pair = {
                "question_number": i,
                "question": question,
                "answer": result["answer"],
                "sources_count": len(result.get("context_docs", [])),
                "query_time_seconds": result["timing"]
            }
            
            results.append(qa_pair)
            
            # Save incrementally
            with open("chapter10_api_design_research.json", "w", encoding="utf-8") as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            
            print(f"✓ Completed in {result['timing']:.2f}s (Sources: {qa_pair['sources_count']})")
            
        except Exception as e:
            print(f"✗ Error on Q{i}: {e}")
            continue
    
    print("\n" + "=" * 60)
    print(f"✅ Research complete! {len(results)}/50 questions answered")
    print(f"Results saved to: chapter10_api_design_research.json")
    
    # Calculate statistics
    if results:
        times = [r["query_time_seconds"] if isinstance(r["query_time_seconds"], (int, float)) 
                 else r["query_time_seconds"].get("total_seconds", 0) for r in results]
        total_time = sum(times)
        avg_time = total_time / len(results)
        print(f"\n📊 Statistics:")
        print(f"   Total time: {total_time/60:.2f} minutes")
        print(f"   Average per question: {avg_time:.2f} seconds")
        print(f"   Fastest: {min(times):.2f}s")
        print(f"   Slowest: {max(times):.2f}s")

if __name__ == "__main__":
    main()
