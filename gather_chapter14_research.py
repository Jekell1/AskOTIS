#!/usr/bin/env python3
"""
Chapter 14 Research: Testing and Quality Assurance
Gathers answers to 50 questions about unit/integration testing, regression testing,
and user acceptance testing for the modernized C# API system.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 14.1: Unit and Integration Testing for APIs (Q1-17)
        "What unit testing frameworks are best suited for testing C# APIs (xUnit, NUnit, MSTest)?",
        "How should we structure unit tests for business logic services converted from COBOL?",
        "What mocking frameworks (Moq, NSubstitute) work well for isolating dependencies in API tests?",
        "How can we achieve high code coverage for critical business logic without over-testing?",
        "What integration testing strategies validate API endpoints end-to-end?",
        "How should we test database interactions (repository layer) without hitting production databases?",
        "What tools can generate test data that mimics production COBOL data patterns?",
        "How can we implement contract testing to ensure API stability across versions?",
        "What strategies exist for testing asynchronous operations and background jobs in C# APIs?",
        "How should we test error handling, validation, and exception scenarios in APIs?",
        "What role does test-driven development (TDD) play in the COBOL-to-C# conversion process?",
        "How can we automate integration tests to run in CI/CD pipelines?",
        "What tools (Postman, REST Client, Swagger) help with API endpoint testing?",
        "How should we test authentication and authorization logic in the new APIs?",
        "What strategies exist for performance testing individual API endpoints (load, stress testing)?",
        "How can we use snapshot testing to validate API response structures?",
        "What documentation should accompany unit and integration tests for maintainability?",
        
        # Section 14.2: Regression Testing Against Legacy Outputs (Q18-34)
        "How can we capture baseline outputs from the legacy COBOL system for comparison?",
        "What strategies exist for comparing COBOL program outputs with new C# API outputs?",
        "How should we handle acceptable differences (formatting, precision) vs. actual defects?",
        "What tools can automate regression testing by comparing files, reports, or database states?",
        "How can we extract test cases from existing COBOL programs or JCL procedures?",
        "What role does data-driven testing play in validating migration accuracy?",
        "How should we test edge cases and error conditions that may not be well-documented in legacy systems?",
        "What strategies exist for parallel running (old and new systems) during regression testing?",
        "How can we validate that complex business calculations produce identical results in C# vs. COBOL?",
        "What logging or tracing should be in place to troubleshoot regression test failures?",
        "How should we handle temporal differences (dates, timestamps) when comparing outputs?",
        "What criteria determine when regression testing is 'complete' and the new system is ready?",
        "How can we use synthetic transactions to validate end-to-end business processes?",
        "What strategies exist for regression testing batch jobs converted from COBOL to C#?",
        "How should we test data migration accuracy as part of regression testing?",
        "What documentation tracks known differences or workarounds between old and new systems?",
        "How can we leverage COBOL test data to seed C# API test scenarios?",
        
        # Section 14.3: User Acceptance Testing (Q35-50)
        "What is the role of user acceptance testing (UAT) in validating the modernized system?",
        "How should we involve business stakeholders in UAT planning and execution?",
        "What test scenarios should be prioritized for UAT (critical business processes, high-risk areas)?",
        "How can we create realistic UAT environments that mimic production without exposing sensitive data?",
        "What tools or frameworks facilitate UAT (test management systems, bug tracking)?",
        "How should we train end-users on the new front end and API-driven workflows?",
        "What acceptance criteria define success for UAT (performance, usability, functionality)?",
        "How can we capture user feedback during UAT and incorporate it into the final release?",
        "What strategies exist for conducting UAT with geographically distributed users?",
        "How should we handle defects discovered during UAT (severity, priority, triage)?",
        "What sign-off process confirms that UAT is complete and the system is production-ready?",
        "How can we use UAT to validate non-functional requirements (performance, security, accessibility)?",
        "What role does exploratory testing play in UAT vs. scripted test cases?",
        "How should we test the new front end's usability and user experience during UAT?",
        "What strategies exist for load testing the system with realistic user concurrency during UAT?",
        "How can we ensure UAT covers all critical user roles and permissions in the new system?",
    ]
    
    results = []
    
    print(f"\n{'='*60}")
    print(f"CHAPTER 14 RESEARCH: Testing and Quality Assurance")
    print(f"{'='*60}")
    print(f"Total questions to process: {len(questions)}\n")
    
    for i, question in enumerate(questions, 1):
        print(f"\n[{i}/{len(questions)}] Researching: {question[:80]}...")
        
        start_time = time.time()
        response = rag.ask_with_timing(question)
        elapsed = time.time() - start_time
        
        result = {
            "question_number": i,
            "question": question,
            "answer": response['answer'],
            "sources_count": response['timing'].get('num_documents', 0),
            "query_time_seconds": response['timing']
        }
        results.append(result)
        
        # Save incrementally
        with open("chapter14_testing_qa_research.json", "w", encoding="utf-8") as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"✓ Answer received ({result['sources_count']} sources, {elapsed:.2f}s)")
    
    print(f"\n{'='*60}")
    print(f"✅ CHAPTER 14 RESEARCH COMPLETE!")
    print(f"{'='*60}")
    print(f"Results saved to: chapter14_testing_qa_research.json")
    print(f"Total questions answered: {len(results)}")

if __name__ == "__main__":
    main()
