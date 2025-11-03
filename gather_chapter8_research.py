"""
Chapter 8: Automated and Manual Code Analysis
Research script to gather data from OTIS RAG system for 50 questions.

Topics:
- Parsing COBOL Code (Q1-15)
- Extracting Business Logic and Data Structures (Q16-25)
- Tools for COBOL-to-C# Conversion (Q26-40)
- Handling Copybooks and Common Routines (Q41-50)
"""

import json
import time
from otis_rag import OTISRAG

# Initialize RAG system
rag = OTISRAG()

# Define 50 research questions
questions = [
    # Section 8.1: Parsing COBOL Code (Q1-15)
    "What are the key challenges in parsing COBOL code for conversion to C#?",
    "How can COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE) be parsed and mapped to C# structures?",
    "What techniques can extract COBOL program metadata (program names, dates, authors)?",
    "How should COBOL data items in the DATA DIVISION be parsed for type conversion?",
    "What parsing strategies handle COBOL hierarchical data structures (level numbers 01-49)?",
    "How can COBOL PICTURE clauses be interpreted and converted to C# data types?",
    "What are the best practices for parsing COBOL WORKING-STORAGE SECTION variables?",
    "How should COBOL file descriptions (FD entries) be parsed for database schema generation?",
    "What techniques parse COBOL PROCEDURE DIVISION logic into C# methods?",
    "How can COBOL paragraph and section structures be identified during parsing?",
    "What parsing approaches handle COBOL conditional statements (IF-THEN-ELSE, EVALUATE)?",
    "How should COBOL PERFORM statements and loop constructs be parsed?",
    "What techniques parse COBOL CALL statements for dependency analysis?",
    "How can COBOL SQL statements (EXEC SQL) be extracted during parsing?",
    "What error handling strategies are needed when parsing legacy COBOL code?",
    
    # Section 8.2: Extracting Business Logic and Data Structures (Q16-25)
    "How can business rules embedded in COBOL PROCEDURE DIVISION be extracted?",
    "What techniques identify reusable business logic in COBOL programs?",
    "How should COBOL data validation logic be extracted and modernized for C#?",
    "What approaches extract calculation logic from COBOL COMPUTE statements?",
    "How can COBOL copybooks be analyzed to extract shared data structures?",
    "What techniques identify COBOL programs that should become C# services or APIs?",
    "How should COBOL transaction processing logic be extracted for API design?",
    "What methods extract data transformation logic from COBOL MOVE and STRING statements?",
    "How can COBOL screen handling logic be separated from business logic?",
    "What strategies extract domain entities and value objects from COBOL data structures?",
    
    # Section 8.3: Tools for COBOL-to-C# Conversion (Q26-40)
    "What automated tools are available for COBOL-to-C# code conversion?",
    "How can static analysis tools aid in understanding COBOL codebase structure?",
    "What role do AST (Abstract Syntax Tree) parsers play in COBOL conversion?",
    "How can tools like ANTLR be used to build custom COBOL parsers?",
    "What benefits do commercial COBOL conversion tools offer over manual conversion?",
    "How can code generation tools automate C# class creation from COBOL data structures?",
    "What tools can analyze COBOL program dependencies and call hierarchies?",
    "How should database schema reverse-engineering tools be used for COBOL file migration?",
    "What role do AI/ML-based code conversion tools play in modernizing COBOL?",
    "How can version control and diff tools track conversion progress?",
    "What testing tools verify functional equivalence between COBOL and C# implementations?",
    "How can code quality analysis tools ensure converted C# code meets standards?",
    "What IDE features (Visual Studio, VS Code) support COBOL-to-C# conversion workflows?",
    "How should documentation generation tools create API docs from converted code?",
    "What project management tools track conversion tasks and dependencies?",
    
    # Section 8.4: Handling Copybooks and Common Routines (Q41-50)
    "What strategies handle COBOL copybooks during conversion to C# shared libraries?",
    "How should copybook dependencies be analyzed across the OTIS codebase?",
    "What approaches convert COBOL copybook data structures to C# DTOs or POCOs?",
    "How can common COBOL utility routines be identified for C# library creation?",
    "What techniques handle COBOL copybook REPLACING and parameterization?",
    "How should COBOL copybooks with nested structures be converted to C# classes?",
    "What strategies ensure consistency when multiple programs use the same copybook?",
    "How can COBOL common error-handling routines be converted to C# exception patterns?",
    "What approaches handle COBOL copybooks that define screen layouts?",
    "How should versioning be managed when converting shared COBOL copybooks to C# libraries?"
]

print(f"Starting research for Chapter 8: Automated and Manual Code Analysis")
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
        with open("chapter8_code_analysis_research.json", "w", encoding="utf-8") as f:
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
print(f"Results saved to: chapter8_code_analysis_research.json")
print("=" * 80)
