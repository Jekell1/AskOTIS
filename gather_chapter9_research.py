"""
Chapter 9: Refactoring for Optimization
Research script to gather data on identifying inefficient patterns,
modernizing data access, and improving performance and maintainability.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 9.1: Identifying Inefficient Patterns (Q1-18)
        "What are the most common inefficient coding patterns found in the OTIS COBOL codebase, such as deeply nested IF statements, and how do they impact performance and maintainability?",
        "How can we identify and measure the performance impact of nested IF-ELSE structures in COBOL programs, and what metrics should we use to prioritize refactoring efforts?",
        "What are the typical code smells in COBOL that indicate opportunities for refactoring, such as duplicated code, long methods, or tightly coupled modules?",
        "How prevalent are sequential file I/O operations in the OTIS codebase, and what performance bottlenecks do they create compared to indexed or direct access methods?",
        "What techniques can be used to detect COBOL programs that perform excessive file opens, closes, and rewrites within loops or nested structures?",
        "How can we identify COBOL paragraphs or sections that are called repeatedly in a single transaction and could benefit from optimization or caching?",
        "What are the performance implications of COBOL PERFORM statements with varying iterations, and how can we refactor these to use more efficient control structures in C#?",
        "How do we identify and measure the impact of redundant data movements (MOVE statements) in COBOL programs that could be eliminated during conversion?",
        "What patterns in COBOL indicate inefficient memory usage, such as oversized working storage areas or redundant data structures?",
        "How can we detect COBOL programs that perform string manipulation inefficiently (e.g., INSPECT, STRING, UNSTRING) and would benefit from C# string optimization?",
        "What are the signs of poorly optimized COBOL batch processing programs, and how can we identify candidates for parallel processing in C#?",
        "How do we identify COBOL programs with inefficient sorting or merging operations that could be improved with modern algorithms and data structures?",
        "What techniques can we use to detect COBOL programs that perform redundant validation checks or business logic that should be centralized?",
        "How can we identify COBOL copybooks that are overused or contain unused data definitions, leading to memory waste and code bloat?",
        "What are the indicators of poor error handling in COBOL programs (e.g., generic error codes, missing validations) that need improvement in the C# implementation?",
        "How do we detect COBOL programs with hardcoded values, magic numbers, or business rules that should be externalized to configuration or database tables?",
        "What patterns indicate that COBOL programs are doing too much in a single unit (lack of separation of concerns) and need to be decomposed into multiple services?",
        "How can we identify COBOL programs that use inefficient database access patterns, such as cursor-based processing or row-by-row updates instead of set-based operations?",
        
        # Section 9.2: Modernizing Data Access (Q19-33)
        "What is the current data access architecture in the OTIS system, including the types of files used (VSAM, flat files, DB2) and their access patterns?",
        "How can we map COBOL file structures (FD entries, record layouts) to modern relational database schemas with proper normalization?",
        "What strategies should we use to migrate from sequential flat files to relational database tables while preserving data integrity and business logic?",
        "How do we handle the conversion of COBOL indexed files (KSDS) to SQL database tables with appropriate primary keys and indexes?",
        "What approach should we take to convert COBOL alternate indexes and secondary key structures to database indexes and foreign key relationships?",
        "How can we identify opportunities to introduce database transactions and ACID properties to replace COBOL file commit/rollback logic?",
        "What patterns should we use for data access in the new C# application, such as Repository pattern, Unit of Work, or Entity Framework?",
        "How do we handle the migration of COBOL file I/O error handling to database exception handling and retry logic in C#?",
        "What strategies can we use to introduce connection pooling and optimize database connections compared to COBOL file handles?",
        "How should we design the database schema to support the performance requirements of the new API-driven architecture?",
        "What techniques can we use to optimize SQL queries generated from COBOL file access patterns, including query tuning and indexing strategies?",
        "How do we handle the conversion of COBOL multi-file processing (e.g., SORT-MERGE operations) to efficient database joins and aggregations?",
        "What approach should we take to implement caching strategies (in-memory, distributed cache) to reduce database load and improve API performance?",
        "How can we leverage database features like stored procedures, views, and functions to encapsulate complex business logic previously embedded in COBOL?",
        "What strategies should we use to handle data migration from legacy files to the new database, including data cleansing and validation?",
        
        # Section 9.3: Improving Performance and Maintainability (Q34-50)
        "What are the key performance metrics we should establish for the new C# application to ensure it meets or exceeds the legacy COBOL system performance?",
        "How can we introduce asynchronous processing in C# to improve responsiveness compared to synchronous COBOL batch processing?",
        "What strategies should we use to implement parallel processing in C# for operations that are currently sequential in COBOL?",
        "How do we design the C# API architecture to support horizontal scaling and load balancing for high-volume transaction processing?",
        "What caching strategies (application-level, distributed cache, CDN) should we implement to optimize frequently accessed data?",
        "How can we leverage modern C# language features (LINQ, async/await, generics) to write more efficient and maintainable code than the COBOL equivalent?",
        "What design patterns (Factory, Strategy, Dependency Injection) should we implement to improve code maintainability and testability?",
        "How do we structure the C# solution to promote separation of concerns, modularity, and adherence to SOLID principles?",
        "What logging and monitoring strategies should we implement to track performance, identify bottlenecks, and support troubleshooting in production?",
        "How can we introduce comprehensive unit testing, integration testing, and performance testing to ensure the refactored code meets quality standards?",
        "What code review and static analysis tools should we use to maintain code quality and consistency across the C# codebase?",
        "How do we document the refactored C# code and APIs to ensure maintainability and knowledge transfer?",
        "What strategies should we use to handle technical debt and prioritize refactoring efforts based on business value and risk?",
        "How can we implement continuous integration and continuous deployment (CI/CD) pipelines to support rapid iteration and deployment?",
        "What strategies should we use to version the APIs and manage backward compatibility during the transition period?",
        "How do we implement feature flags or A/B testing to gradually roll out optimizations and new features?",
        "What performance testing and load testing strategies should we implement to validate that the refactored system can handle production workloads?"
    ]
    
    results = []
    
    print(f"Starting Chapter 9 research: {len(questions)} questions")
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
            with open("chapter9_refactoring_research.json", "w", encoding="utf-8") as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            
            print(f"✓ Completed in {result['timing']:.2f}s (Sources: {qa_pair['sources_count']})")
            
        except Exception as e:
            print(f"✗ Error on Q{i}: {e}")
            continue
    
    print("\n" + "=" * 60)
    print(f"✅ Research complete! {len(results)}/50 questions answered")
    print(f"Results saved to: chapter9_refactoring_research.json")
    
    # Calculate statistics
    if results:
        times = [r["query_time_seconds"] for r in results]
        total_time = sum(times)
        avg_time = total_time / len(results)
        print(f"\n📊 Statistics:")
        print(f"   Total time: {total_time/60:.2f} minutes")
        print(f"   Average per question: {avg_time:.2f} seconds")
        print(f"   Fastest: {min(times):.2f}s")
        print(f"   Slowest: {max(times):.2f}s")

if __name__ == "__main__":
    main()
