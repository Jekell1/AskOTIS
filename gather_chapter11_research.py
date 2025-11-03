"""
Chapter 11: Implementing the C# APIs
Research script to gather data on setting up project structure,
implementing business logic, integrating with data stores, and error handling.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 11.1: Setting Up the Project Structure (Q1-13)
        "What is the recommended project structure for a C# API solution converting COBOL programs, including layers for API, business logic, data access, and shared utilities?",
        "How should we organize the C# solution to separate concerns between different OTIS modules (loans, payments, accounts, screens)?",
        "What naming conventions and folder structures should we follow for controllers, services, repositories, and DTOs in the C# API project?",
        "How do we set up dependency injection in ASP.NET Core to support the converted COBOL business logic and data access layers?",
        "What configuration management approach should we use for environment-specific settings (connection strings, API keys, business rules) replacing COBOL configuration?",
        "How should we structure middleware for cross-cutting concerns like logging, authentication, exception handling, and request/response transformation?",
        "What approach should we take for organizing shared libraries, utilities, and common code that will be used across multiple API endpoints?",
        "How do we set up the project for both synchronous API endpoints and asynchronous background processing (replacing COBOL batch jobs)?",
        "What build and deployment pipeline configuration should we establish for the C# API project (CI/CD, Docker, Kubernetes)?",
        "How should we structure unit test, integration test, and API test projects to ensure comprehensive test coverage?",
        "What documentation tools and approaches should we implement for API documentation (Swagger/OpenAPI, README, code comments)?",
        "How do we organize database migration scripts and seed data to support development, testing, and production environments?",
        "What versioning strategy should we implement in the project structure to support multiple API versions during the transition?",
        
        # Section 11.2: Implementing Business Logic (Q14-26)
        "What patterns should we use to implement COBOL business rules in C# while maintaining separation of concerns (Service Layer, Domain Model)?",
        "How do we convert COBOL paragraph-based logic to C# methods and classes with appropriate abstraction and encapsulation?",
        "What approach should we take for implementing complex validation logic from COBOL programs in C# (FluentValidation, Data Annotations)?",
        "How should we handle COBOL PERFORM statements and program flow control in C# service methods?",
        "What patterns should we use for implementing transaction processing logic from COBOL programs (Unit of Work, Transaction Scope)?",
        "How do we implement calculation and business rule engines in C# to replace COBOL COMPUTE statements and complex formulas?",
        "What approach should we take for handling COBOL copybook-based shared business logic in C# (shared services, extension methods)?",
        "How should we implement state management for multi-step processes that span multiple COBOL programs in a stateless API architecture?",
        "What patterns should we use for implementing workflow orchestration replacing COBOL screen flows and program chains?",
        "How do we handle COBOL file status checking and error branching logic in C# business logic layers?",
        "What approach should we take for implementing business rule externalization (rules engine, decision tables) to improve maintainability?",
        "How should we implement audit logging and business event tracking in the C# business logic layer?",
        "What patterns should we use for implementing domain events and event sourcing if applicable to the OTIS business domain?",
        
        # Section 11.3: Integrating with Modern Data Stores (Q27-38)
        "What approach should we take for implementing Entity Framework Core or Dapper for data access, replacing COBOL file I/O?",
        "How do we design and implement the repository pattern for data access, abstracting database operations from business logic?",
        "What strategy should we use for implementing database connection management, connection pooling, and resilience (retry policies)?",
        "How should we handle database transactions in C# APIs to ensure ACID properties similar to COBOL commit/rollback logic?",
        "What approach should we take for implementing optimistic concurrency control using row versioning or timestamps?",
        "How do we implement efficient bulk operations in C# to replace COBOL batch file processing?",
        "What patterns should we use for implementing database migrations and schema evolution as the system evolves?",
        "How should we implement read/write splitting or CQRS patterns if query performance requires optimization?",
        "What approach should we take for implementing caching strategies (Redis, MemoryCache) at the data access layer?",
        "How do we handle database-specific features (stored procedures, functions) while maintaining database portability?",
        "What strategy should we use for implementing data access auditing and change tracking?",
        "How should we implement pagination, filtering, and sorting at the data access layer for large result sets?",
        
        # Section 11.4: Error Handling and Validation (Q39-50)
        "What comprehensive error handling strategy should we implement in C# APIs to replace COBOL error handling patterns?",
        "How should we design exception hierarchies and custom exceptions for different error scenarios (business errors, validation errors, system errors)?",
        "What approach should we take for implementing global exception handling middleware in ASP.NET Core?",
        "How do we implement validation at multiple layers (DTO validation, business rule validation, database constraints)?",
        "What patterns should we use for returning consistent error responses from APIs (Problem Details, custom error models)?",
        "How should we implement logging and monitoring for errors and exceptions (Application Insights, Serilog, ELK stack)?",
        "What approach should we take for implementing retry logic and circuit breaker patterns for resilience?",
        "How do we implement request validation including input sanitization to prevent security vulnerabilities?",
        "What strategy should we use for implementing business validation rules that can be easily maintained and tested?",
        "How should we handle partial failures in operations that involve multiple steps or external systems?",
        "What approach should we take for implementing correlation IDs and distributed tracing for debugging across services?",
        "How do we implement health checks and readiness probes for the C# APIs in containerized environments?"
    ]
    
    results = []
    
    print(f"Starting Chapter 11 research: {len(questions)} questions")
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
            with open("chapter11_implementing_apis_research.json", "w", encoding="utf-8") as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            
            print(f"✓ Completed in {result['timing']:.2f}s (Sources: {qa_pair['sources_count']})")
            
        except Exception as e:
            print(f"✗ Error on Q{i}: {e}")
            continue
    
    print("\n" + "=" * 60)
    print(f"✅ Research complete! {len(results)}/50 questions answered")
    print(f"Results saved to: chapter11_implementing_apis_research.json")
    
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
