"""
Chapter 6 Research: Choosing the Right Tools and Technologies
Gathers comprehensive research through 50 targeted questions
"""

import json
import time
from datetime import datetime
from otis_rag import OTISRAG

def main():
    print("=" * 80)
    print("CHAPTER 6 RESEARCH: CHOOSING THE RIGHT TOOLS AND TECHNOLOGIES")
    print("=" * 80)
    
    # Initialize RAG system
    rag = OTISRAG()
    
    # Define 50 research questions across 3 major sections
    questions = [
        # Section 6.1: Why C# and .NET? (Questions 1-20)
        "Why is C# and .NET a suitable choice for modernizing the OTIS COBOL application? What advantages does C# offer over COBOL?",
        "What specific features of C# make it ideal for converting business logic from COBOL?",
        "How does the .NET ecosystem support enterprise application development compared to mainframe COBOL?",
        "What are the language similarities between COBOL and C# that could ease the transition?",
        "What are the key differences between COBOL and C# that developers need to understand?",
        "How does C#'s object-oriented programming model compare to COBOL's procedural approach?",
        "What memory management differences exist between COBOL and C# (garbage collection vs manual)?",
        "How do exception handling patterns in C# compare to COBOL error handling?",
        "What are the benefits of C#'s type system compared to COBOL's PICTURE clauses?",
        "How can COBOL PERFORM statements be translated to C# control structures?",
        "What C# features support the complex business rules found in the OTIS system?",
        "How does .NET's runtime environment compare to COBOL's mainframe runtime?",
        "What tooling advantages does Visual Studio provide for C# development vs COBOL IDEs?",
        "How do debugging capabilities in C# compare to COBOL debugging?",
        "What unit testing frameworks are available for C# that weren't available for COBOL?",
        "How does C#'s LINQ feature simplify data access compared to COBOL file handling?",
        "What concurrency and async programming patterns in C# improve upon COBOL?",
        "How do C# interfaces and generics provide better code reusability than COBOL copybooks?",
        "What cross-platform capabilities does .NET Core/.NET 5+ offer for the modernized system?",
        "How does the C# and .NET community support compare to COBOL resources?",
        
        # Section 6.2: API Frameworks (ASP.NET Core, REST, GraphQL) (Questions 21-35)
        "Why is ASP.NET Core the recommended framework for building the OTIS API layer?",
        "What are the advantages of ASP.NET Core for building RESTful APIs?",
        "How does ASP.NET Core's middleware pipeline support API concerns like authentication and logging?",
        "What built-in features of ASP.NET Core support API versioning?",
        "How does ASP.NET Core's dependency injection container improve code maintainability?",
        "What are the benefits of using ASP.NET Core Minimal APIs vs Controller-based APIs?",
        "How does ASP.NET Core support OpenAPI/Swagger documentation generation?",
        "What validation frameworks in ASP.NET Core help ensure API data integrity?",
        "How does ASP.NET Core handle API rate limiting and throttling?",
        "What security features does ASP.NET Core provide for API authentication (JWT, OAuth2)?",
        "How can ASP.NET Core support both REST and SOAP endpoints for backward compatibility?",
        "What are the pros and cons of REST vs GraphQL for the OTIS loan servicing domain?",
        "How would GraphQL benefit complex, nested queries in the OTIS system?",
        "What REST API design patterns (HATEOAS, Richardson Maturity Model) should be applied?",
        "How should REST endpoints be structured for OTIS resources (loans, payments, customers)?",
        
        # Section 6.2 continued: API Frameworks (Questions 36-40)
        "What API gateway solutions (Azure API Management, Ocelot, YARP) should be considered?",
        "How can ASP.NET Core support gRPC for high-performance internal service communication?",
        "What message serialization formats (JSON, XML, Protobuf) are best for different API scenarios?",
        "How does ASP.NET Core support real-time communication (SignalR, WebSockets)?",
        "What API monitoring and observability tools integrate with ASP.NET Core?",
        
        # Section 6.3: Data Storage Options (SQL, NoSQL, Cloud) (Questions 41-50)
        "What SQL database options (SQL Server, PostgreSQL, Azure SQL) are suitable for OTIS data migration?",
        "How should the choice between SQL Server and PostgreSQL be evaluated for OTIS?",
        "What are the advantages of using Azure SQL Database for a cloud-native OTIS system?",
        "How can Entity Framework Core simplify data access compared to COBOL file I/O?",
        "What NoSQL databases (MongoDB, Cosmos DB, Redis) could complement the SQL database?",
        "When should NoSQL be used vs relational databases in the OTIS architecture?",
        "How can Redis be used for caching and session management in the modernized system?",
        "What document database features (MongoDB, Cosmos DB) could benefit unstructured OTIS data?",
        "How should the data migration strategy from COBOL flat files and VSAM to SQL be planned?",
        "What cloud storage options (Azure Blob Storage, S3) should be used for file storage and archival?"
    ]
    
    print(f"Total questions to process: {len(questions)}")
    print()
    
    results = []
    start_time = time.time()
    
    for i, question in enumerate(questions, 1):
        print(f"[Question {i}/{len(questions)}]")
        print(f"Q: {question}")
        print("-" * 80)
        
        try:
            # Ask the question with timing
            q_start = time.time()
            result = rag.ask_with_timing(question, verbose=False)
            answer = result.get("answer", "")
            context_docs = result.get("context_docs", [])
            timing_data = result.get("timing", {})
            q_elapsed = timing_data.get("total_seconds", 0)
            
            # Store result
            research_result = {
                "question_number": i,
                "question": question,
                "answer": answer,
                "sources_count": len(context_docs),
                "query_time_seconds": round(q_elapsed, 2)
            }
            results.append(research_result)
            
            # Show preview
            preview = answer[:200] + "..." if len(answer) > 200 else answer
            print(f"A: {preview}")
            print(f"⏱️  Query time: {q_elapsed:.2f}s")
            print()
            
            # Save after each question (for crash recovery)
            with open('chapter6_tools_technologies_research.json', 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
                
        except Exception as e:
            print(f"❌ Error processing question {i}: {e}")
            print()
            continue
    
    end_time = time.time()
    total_time = end_time - start_time
    
    # Final save
    with open('chapter6_tools_technologies_research.json', 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
    
    # Print summary
    print("=" * 80)
    print("RESEARCH COMPLETE!")
    print("=" * 80)
    print(f"Total questions answered: {len(results)}")
    print(f"Total time: {total_time:.2f} seconds ({total_time/60:.2f} minutes)")
    print(f"Average time per question: {total_time/len(results):.2f} seconds")
    print(f"Results saved to: chapter6_tools_technologies_research.json")
    print("=" * 80)

if __name__ == "__main__":
    main()
