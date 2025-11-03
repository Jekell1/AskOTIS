#!/usr/bin/env python3
"""
Gather research data for Chapter 1: Introduction
50 questions about the book's purpose, OTIS overview, modernization rationale, and target audience
"""

import json
from datetime import datetime
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Purpose of the Book (Q1-10)
        "What is the primary goal of this book about modernizing the OTIS COBOL application to C# with API architecture?",
        "Who will benefit most from reading this book about OTIS modernization to C# APIs?",
        "What specific modernization challenges does this book address for legacy COBOL systems like OTIS?",
        "How does this book guide readers through the technical transformation from COBOL to C# APIs?",
        "What makes this modernization guide unique compared to general COBOL-to-C# conversion resources?",
        "What are the expected outcomes for readers who follow this book's modernization approach?",
        "How does this book balance technical implementation details with strategic planning guidance?",
        "What prerequisites should readers have before starting this OTIS modernization journey?",
        "How does this book address both code conversion and architectural transformation?",
        "What is the intended scope of modernization covered in this book for OTIS?",
        
        # Overview of OTIS/OTOS (Q11-25)
        "What is the OTIS (OTOS) system and what business domain does it serve?",
        "What are the main functional areas and modules in the OTIS COBOL application?",
        "What programming languages and technologies are used in the current OTIS system?",
        "How large is the OTIS codebase in terms of programs, lines of code, and data structures?",
        "What databases and data storage systems does OTIS currently use?",
        "What are the primary input and output mechanisms in the OTIS system?",
        "How does OTIS handle batch processing versus online transaction processing?",
        "What are the key copybooks and data structures that define OTIS business entities?",
        "What role do JCL (Job Control Language) and job scheduling play in OTIS operations?",
        "What are the most complex or critical programs in the OTIS system?",
        "How does OTIS integrate with external systems or databases?",
        "What reporting and analytics capabilities exist in the current OTIS system?",
        "What security and access control mechanisms are implemented in OTIS?",
        "How does OTIS handle error logging, auditing, and compliance requirements?",
        "What is the typical user workflow when interacting with OTIS screens and functions?",
        
        # Why Modernize? (Q26-40)
        "What are the main business drivers for modernizing OTIS from COBOL to C# APIs?",
        "What technical limitations of the current OTIS COBOL system necessitate modernization?",
        "How do maintenance costs and developer availability impact the need to modernize OTIS?",
        "What new business capabilities could be enabled by modernizing OTIS to an API architecture?",
        "How does the current OTIS architecture limit scalability and performance?",
        "What risks does the organization face by continuing to run OTIS on legacy technology?",
        "How would modernizing OTIS improve user experience and accessibility?",
        "What competitive advantages could be gained by modernizing OTIS?",
        "How does modernization address compliance and regulatory requirements?",
        "What opportunities exist for cloud migration and modern deployment models after modernization?",
        "How would API-based architecture improve integration with other systems?",
        "What benefits would a modern front-end provide to OTIS users?",
        "How does modernization support mobile and remote access requirements?",
        "What cost savings or ROI could be achieved through OTIS modernization?",
        "How does modernization improve data analytics and business intelligence capabilities?",
        
        # Target Audience (Q41-50)
        "Who are the primary technical readers this book is written for?",
        "What experience level should readers have with COBOL systems?",
        "What C# and .NET knowledge is expected from readers?",
        "How does this book serve architects designing modernization strategies?",
        "What value does this book provide to project managers overseeing modernization efforts?",
        "How can business stakeholders use this book to understand modernization implications?",
        "What guidance does this book provide for developers performing the actual code conversion?",
        "How does this book help QA and testing teams validate the modernized system?",
        "What insights can operations and infrastructure teams gain from this book?",
        "How can this book serve as a reference for organizations considering similar COBOL modernization projects?"
    ]
    
    research_data = []
    
    for i, question in enumerate(questions, 1):
        print(f"\n{'='*80}")
        print(f"Question {i}/50: {question}")
        print('='*80)
        
        result = rag.ask_with_timing(question)
        
        # Extract timing data correctly
        timing_data = result.get("timing", {})
        total_seconds = timing_data.get("total_seconds", 0)
        
        research_data.append({
            "question_number": i,
            "question": question,
            "answer": result.get("answer", ""),
            "query_time_seconds": total_seconds,
            "sources_found": len(result.get("context_docs", [])),
            "timestamp": datetime.now().isoformat()
        })
        
        print(f"\n✓ Query time: {total_seconds:.2f}s | Sources: {len(result.get('context_docs', []))}")
        print(f"Answer preview: {result.get('answer', '')[:200]}...")
    
    # Save research data
    output_file = "chapter1_introduction_research.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(research_data, f, indent=2, ensure_ascii=False)
    
    total_time = sum(q['query_time_seconds'] for q in research_data)
    avg_time = total_time / len(research_data)
    
    print(f"\n{'='*80}")
    print("✅ CHAPTER 1 RESEARCH COMPLETE!")
    print(f"{'='*80}")
    print(f"Total questions: {len(research_data)}")
    print(f"Total time: {total_time/60:.2f} minutes")
    print(f"Average per question: {avg_time:.2f} seconds")
    print(f"Output saved to: {output_file}")
    print('='*80)

if __name__ == "__main__":
    main()
