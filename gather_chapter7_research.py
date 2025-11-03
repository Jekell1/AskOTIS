"""
Chapter 7: Designing the Target Architecture
Research script to gather data from OTIS RAG system for 50 questions.

Topics:
- Layered Architecture (Q1-15)
- Microservices vs Modular Monolith (Q16-25)
- API Design Patterns (Q26-40)
- Security Considerations (Q41-50)
"""

import json
import time
from otis_rag import OTISRAG

# Initialize RAG system
rag = OTISRAG()

# Define 50 research questions
questions = [
    # Section 7.1: Layered Architecture (Q1-15)
    "What is layered architecture and how does it apply to converting OTIS to C#?",
    "How should the presentation layer be designed for the modernized OTIS system?",
    "What responsibilities belong in the business logic layer vs the data layer?",
    "How can Domain-Driven Design (DDD) principles guide the layered architecture?",
    "What design patterns (Repository, Service, Factory) support a layered architecture in C#?",
    "How should the data access layer abstract database operations in the OTIS C# system?",
    "What are the benefits of separating concerns across presentation, business, and data layers?",
    "How can dependency injection support loose coupling between layers?",
    "What cross-cutting concerns (logging, validation, caching) span multiple layers?",
    "How should DTOs (Data Transfer Objects) be used between layers?",
    "What role does the Application Service layer play in mediating between presentation and domain?",
    "How can CQRS (Command Query Responsibility Segregation) enhance the layered architecture?",
    "What are the trade-offs between strict layering vs relaxed layering?",
    "How should transaction boundaries be managed across layers?",
    "What testing strategies work best for each layer (unit, integration, end-to-end)?",
    
    # Section 7.2: Microservices vs Modular Monolith (Q16-25)
    "What are the key differences between microservices and a modular monolith?",
    "Should OTIS be converted to microservices or a modular monolith, and why?",
    "What bounded contexts in the OTIS domain could become separate microservices?",
    "How would a modular monolith with well-defined modules benefit the OTIS migration?",
    "What are the operational complexities of microservices (deployment, monitoring, debugging)?",
    "How can a modular monolith evolve into microservices later if needed?",
    "What communication patterns (synchronous HTTP, async messaging) fit microservices?",
    "How should data be managed in microservices (database per service vs shared database)?",
    "What are the performance implications of microservices vs a monolith for OTIS?",
    "How can service boundaries be defined to minimize coupling in a microservices architecture?",
    
    # Section 7.3: API Design Patterns (Q26-40)
    "What are the essential principles of RESTful API design for the OTIS system?",
    "How should API endpoints be versioned to maintain backward compatibility?",
    "What HTTP status codes should be used consistently across OTIS APIs?",
    "How can API pagination, filtering, and sorting be implemented effectively?",
    "What role does HATEOAS play in making APIs discoverable and self-describing?",
    "How should API error responses be structured for clarity and consistency?",
    "What are the best practices for API request and response payload design?",
    "How can API rate limiting and throttling protect the OTIS system from abuse?",
    "What caching strategies (ETags, Cache-Control headers) improve API performance?",
    "How should long-running operations be handled in APIs (async patterns, polling, webhooks)?",
    "What documentation standards (OpenAPI/Swagger) should be used for OTIS APIs?",
    "How can API gateways centralize cross-cutting concerns (authentication, logging, rate limiting)?",
    "What are the differences between REST, GraphQL, and gRPC for OTIS API design?",
    "How should bulk operations and batch processing be exposed through APIs?",
    "What idempotency patterns ensure safe retries of API requests?",
    
    # Section 7.4: Security Considerations (Q41-50)
    "What authentication mechanisms (JWT, OAuth2, OpenID Connect) should secure OTIS APIs?",
    "How should authorization and role-based access control (RBAC) be implemented?",
    "What are the best practices for securing sensitive data in transit and at rest?",
    "How can API keys, tokens, and secrets be managed securely in the OTIS system?",
    "What OWASP Top 10 vulnerabilities must be addressed in the API design?",
    "How should input validation and sanitization prevent injection attacks?",
    "What are the security implications of CORS (Cross-Origin Resource Sharing) for OTIS APIs?",
    "How can rate limiting and DDoS protection secure the API layer?",
    "What audit logging and compliance requirements apply to OTIS financial data?",
    "How should security testing (penetration testing, vulnerability scanning) be integrated into development?"
]

print(f"Starting research for Chapter 7: Designing the Target Architecture")
print(f"Total questions: {len(questions)}")
print("=" * 80)

results = []

for i, question in enumerate(questions, 1):
    print(f"\n[Question {i}/{len(questions)}]")
    print(f"Q: {question}")
    print("-" * 80)
    
    start_time = time.time()
    
    try:
        # Query RAG system
        result = rag.ask_with_timing(question, verbose=False)
        
        # Extract answer and metadata
        answer = result.get("answer", "")
        context_docs = result.get("context_docs", [])
        timing_data = result.get("timing", {})
        query_time = timing_data.get("total_seconds", time.time() - start_time)
        
        # Store result
        results.append({
            "question_number": i,
            "question": question,
            "answer": answer,
            "sources_count": len(context_docs),
            "query_time_seconds": round(query_time, 2)
        })
        
        # Show preview
        preview = answer[:200] + "..." if len(answer) > 200 else answer
        print(f"A: {preview}")
        print(f"⏱️  Query time: {query_time:.2f}s")
        
        # Save incrementally
        with open("chapter7_target_architecture_research.json", "w", encoding="utf-8") as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
            
    except Exception as e:
        print(f"❌ Error processing question {i}: {str(e)}")
        # Continue with next question even if one fails
        continue

print("\n" + "=" * 80)
print("RESEARCH COMPLETE!")
print("=" * 80)
print(f"Total questions answered: {len(results)}")
print(f"Total time: {sum(r['query_time_seconds'] for r in results):.2f} seconds ({sum(r['query_time_seconds'] for r in results)/60:.2f} minutes)")
print(f"Average time per question: {sum(r['query_time_seconds'] for r in results)/len(results):.2f} seconds")
print(f"Results saved to: chapter7_target_architecture_research.json")
print("=" * 80)
