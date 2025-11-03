"""
Gather comprehensive information for Chapter 3: Technical Architecture of OTIS
Asks 50 targeted questions to the RAG system to collect detailed technical data.
"""
import json
import time
from otis_rag import OTISRAG

def gather_technical_architecture_info():
    """Query RAG system with 50 comprehensive questions about OTIS technical architecture."""
    
    # Initialize RAG system
    rag = OTISRAG()
    
    # 50 comprehensive questions about technical architecture
    questions = [
        # COBOL Program Structure (10 questions)
        "What is the typical structure of a COBOL program in OTIS? What are the main divisions?",
        "How are COBOL programs organized in OTIS? What naming conventions are used?",
        "What are the common program types in OTIS (batch, online, utility)?",
        "How do COBOL programs handle error processing and exception handling in OTIS?",
        "What are the key sections in the PROCEDURE DIVISION of OTIS programs?",
        "How are subroutines and paragraphs organized in OTIS COBOL programs?",
        "What are the common WORKING-STORAGE patterns used in OTIS programs?",
        "How do OTIS programs use the LINKAGE SECTION for parameter passing?",
        "What are the typical program entry points and exit points in OTIS?",
        "How are COBOL programs compiled and deployed in the OTIS system?",
        
        # Data Flow and File Dependencies (10 questions)
        "What are the main data files used in OTIS? What are their purposes?",
        "How does data flow between different COBOL programs in OTIS?",
        "What indexed files are used in OTIS and what data do they contain?",
        "How does OTIS handle file I/O operations (READ, WRITE, REWRITE, DELETE)?",
        "What is the relationship between programs and the files they access?",
        "How does OTIS use SQL to access database tables?",
        "What are the key data structures passed between programs?",
        "How does OTIS handle transaction processing and data consistency?",
        "What file locking mechanisms are used in OTIS?",
        "How does OTIS handle batch file processing vs online file access?",
        
        # Menu and Screen Navigation (10 questions)
        "What is the main menu structure in OTIS? What is the entry point?",
        "How does menu navigation work in OTIS? What are the key menu programs?",
        "What is LPMENU and what role does it play in the system?",
        "How do users navigate between different screens in OTIS?",
        "What function keys are used for navigation (F1, F7, etc.)?",
        "How are screens defined and displayed in OTIS?",
        "What is the relationship between menu programs and business logic programs?",
        "How does OTIS handle user input validation on screens?",
        "What screen copybooks are used to define UI layouts?",
        "How does the system route users from menus to specific functions?",
        
        # Key Copybooks and Their Purposes (15 questions)
        "What is TIMEIO.CPY and what does it do?",
        "What are the most important copybooks in OTIS and their purposes?",
        "What is GBFSPR.CPY and what data structures does it define?",
        "What copybooks define the main loan account structures?",
        "What copybooks are used for payment processing?",
        "What copybooks define screen layouts and menu structures?",
        "What copybooks contain business rules and validation logic?",
        "What copybooks are used for date and time handling?",
        "What copybooks define transaction codes and types?",
        "How are copybooks organized and categorized in OTIS?",
        "What copybooks are used for file definitions (FD sections)?",
        "What copybooks contain SQL table definitions?",
        "What copybooks are used for error codes and messages?",
        "What copybooks define common working storage structures?",
        "What copybooks are shared across many programs vs specialized?",
        
        # Program Dependencies and Call Chains (10 questions)
        "What are the most frequently called programs in OTIS?",
        "How deep are typical call chains in OTIS programs?",
        "What programs call what other programs? Give examples of call hierarchies.",
        "What are the core utility programs that many programs depend on?",
        "How does OTIS use CALL statements to invoke other programs?",
        "What parameters are typically passed between calling programs?",
        "What programs have the most dependencies on copybooks?",
        "How are program dependencies documented in OTIS?",
        "What circular dependencies or complex relationships exist?",
        "What programs are entry points that users interact with directly?",
        
        # Additional Technical Details (5 questions)
        "What are the performance considerations in OTIS program design?",
        "How does OTIS handle concurrent access and multi-user scenarios?",
        "What logging and debugging facilities are built into OTIS programs?",
        "How does OTIS handle batch processing schedules and dependencies?",
        "What are the key technical constraints and limitations of the current architecture?"
    ]
    
    results = {}
    total_questions = len(questions)
    
    print(f"Starting information gathering for Chapter 3...")
    print(f"Total questions to process: {total_questions}\n")
    print("=" * 80)
    
    for i, question in enumerate(questions, 1):
        print(f"\n[{i}/{total_questions}] Question: {question}")
        print("-" * 80)
        
        start_time = time.time()
        
        try:
            # Ask the question using ask_with_timing which returns dict with answer, sources, etc.
            result = rag.ask_with_timing(question, verbose=False)
            
            # Extract data from result
            answer = result.get("answer", "")
            context_docs = result.get("context_docs", [])
            timing_data = result.get("timing", {})
            elapsed = timing_data.get("total_seconds", 0)
            
            # Store the result
            results[f"Q{i}"] = {
                "question": question,
                "answer": answer,
                "sources_count": len(context_docs),
                "timing_seconds": elapsed
            }
            
            # Print preview of answer
            answer_preview = answer[:200] + "..." if len(answer) > 200 else answer
            print(f"Answer preview: {answer_preview}")
            print(f"Sources used: {len(context_docs)}")
            print(f"Time: {elapsed:.2f}s")
            
        except Exception as e:
            print(f"ERROR: {str(e)}")
            results[f"Q{i}"] = {
                "question": question,
                "answer": f"Error: {str(e)}",
                "sources_count": 0,
                "timing_seconds": 0
            }
        
        # Small delay to avoid overwhelming the system
        time.sleep(0.5)
    
    print("\n" + "=" * 80)
    print("Information gathering complete!")
    print(f"Total questions answered: {len(results)}")
    
    # Save results to JSON file
    output_file = "chapter3_technical_architecture_research.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
    
    print(f"\nResults saved to: {output_file}")
    
    # Print summary statistics
    total_time = sum(r["timing_seconds"] for r in results.values())
    avg_time = total_time / len(results)
    print(f"\nSummary:")
    print(f"  Total time: {total_time:.2f}s")
    print(f"  Average time per question: {avg_time:.2f}s")
    print(f"  Total sources retrieved: {sum(r['sources_count'] for r in results.values())}")
    
    return results

if __name__ == '__main__':
    results = gather_technical_architecture_info()
