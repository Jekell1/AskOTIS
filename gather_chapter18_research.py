#!/usr/bin/env python3
"""
Chapter 18: Maintaining and Evolving the New System
Research script to explore strategies for continuous improvement, adding new features,
and planning the eventual sunset of legacy COBOL systems after modernization.
"""

import json
from pathlib import Path
from otis_rag import OTISRAG

# Initialize RAG system
print("Initializing OTISRAG for Chapter 18 research (System Maintenance & Evolution)...")
rag = OTISRAG()

# Chapter 18: Maintaining and Evolving the New System
questions = [
    # Section 18.1: Continuous Improvement of the C# Codebase (Q1-17)
    "What continuous improvement practices should be established for the modernized C# OTIS system (code reviews, refactoring, technical debt management)?",
    "How do you measure code quality metrics for the C# system (code coverage, complexity, maintainability index) and establish improvement goals?",
    "What refactoring opportunities commonly emerge after initial C# conversion, and how do you prioritize these improvements?",
    "How should technical debt be tracked and managed in the modernized system (debt register, impact assessment, remediation planning)?",
    "What automated code quality tools (SonarQube, ReSharper, analyzers) should be integrated into the CI/CD pipeline to catch issues early?",
    
    "How do you establish coding standards and best practices for the team maintaining the C# system (style guides, architectural patterns)?",
    "What mechanisms ensure knowledge sharing and prevent silos in teams maintaining the modernized C# APIs and front end?",
    "How should performance monitoring inform continuous improvement efforts (identifying slow endpoints, optimizing hot paths)?",
    "What strategies help reduce duplication and improve code reuse across the C# API modules?",
    "How do you balance continuous improvement with new feature development to avoid stagnation or over-engineering?",
    
    "What role does automated testing play in enabling safe refactoring and continuous improvement of the C# codebase?",
    "How should database schema evolution be managed as the C# system grows (migrations, backward compatibility, versioning)?",
    "What dependency management practices prevent security vulnerabilities and technical obsolescence in the C# system?",
    "How do you identify and address performance regressions that emerge as the C# system evolves over time?",
    "What feedback loops (user surveys, support tickets, monitoring alerts) should drive continuous improvement priorities?",
    "How should architectural evolution be governed (ADRs, review boards) to maintain consistency while allowing innovation?",
    "What strategies help modernize the C# system incrementally rather than letting it accumulate technical debt like the legacy COBOL did?",
    
    # Section 18.2: Adding New Features to the Modernized System (Q18-34)
    "What process should govern how new features are designed and implemented in the modernized C# system (requirements gathering, design reviews, implementation)?",
    "How do you ensure new features follow established architectural patterns and don't introduce inconsistencies into the C# system?",
    "What strategies help estimate the effort required to add new features compared to the legacy COBOL environment?",
    "How should API versioning be managed when adding new features that might break existing consumers?",
    
    "What testing strategies ensure new features don't introduce regressions or break existing functionality in the C# system?",
    "How do you balance speed of feature delivery with quality and maintainability in the modernized environment?",
    "What role does feature flagging play in rolling out new capabilities gradually and managing risk in the C# system?",
    "How should database schema changes be coordinated when adding new features (new tables, columns, indexes)?",
    "What documentation standards ensure new features are well-understood by developers, operations, and users?",
    
    "How do you leverage modern capabilities (cloud services, AI/ML, real-time processing) when adding features that weren't possible in COBOL?",
    "What strategies help avoid feature bloat and keep the C# system focused on core business value?",
    "How should cross-functional collaboration (developers, designers, product managers, users) be structured for feature development?",
    "What mechanisms ensure regulatory compliance and security are considered early when designing new features?",
    "How do you measure the success of new features after deployment (usage metrics, user satisfaction, business impact)?",
    
    "What strategies help preserve the stability and reliability of the C# system while adding new capabilities?",
    "How should third-party integrations (payment processors, credit bureaus, reporting services) be managed when adding new features?",
    "What lessons from the COBOL-to-C# conversion inform how new features should be designed and implemented?",
    
    # Section 18.3: Planning for Legacy System Sunset (Q35-50)
    "What criteria indicate the COBOL legacy system is ready to be decommissioned (functionality parity, stability, user acceptance)?",
    "How do you plan a phased sunset of the COBOL system to minimize risk and ensure smooth transition?",
    "What contingency plans should exist in case critical issues are discovered in the C# system after COBOL decommissioning?",
    "How do you handle the final cutover from COBOL to C# for remaining batch processes or integrations?",
    
    "What documentation and knowledge capture is needed before decommissioning the COBOL system (historical decisions, data formats, integration points)?",
    "How do you manage dependencies where external systems still expect COBOL-style outputs or interfaces during the sunset period?",
    "What data archival strategies ensure historical COBOL data remains accessible after system decommissioning?",
    "How do you handle licensing, infrastructure, and staffing changes as the COBOL system is sunset?",
    "What communication plan keeps stakeholders informed about the timeline and impact of COBOL system decommissioning?",
    
    "How do you validate that all COBOL functionality has been successfully migrated before final decommissioning?",
    "What metrics and monitoring confirm the C# system is fully capable of replacing all COBOL operations?",
    "How should the final COBOL decommissioning be coordinated with business operations (timing, backup plans, validation)?",
    "What training and change management help users and IT staff complete the transition away from COBOL?",
    "How do you celebrate and communicate the successful completion of the modernization project to build organizational confidence?",
    
    "What post-sunset review should be conducted to capture lessons learned from the entire COBOL-to-C# conversion?",
    "How do you ensure institutional knowledge about the original COBOL system is preserved for future reference even after decommissioning?"
]

# Output file
output_file = Path("chapter18_maintenance_research.json")

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
print("Chapter 18 Research Complete!")
print(f"Total Q&A pairs: {len(results)}")
print(f"Output file: {output_file}")
print(f"{'='*80}")
