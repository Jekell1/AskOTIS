"""
Chapter 12: Building the New Front End
Research script to gather data on front end technology choices,
consuming APIs, and UI/UX modernization.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 12.1: Front End Technology Choices (Q1-17)
        "What are the key considerations for choosing front end technologies (React, Angular, Vue, Blazor) to replace COBOL screen-based interfaces?",
        "How should we evaluate web vs. desktop vs. mobile front end approaches based on OTIS user requirements and business needs?",
        "What are the trade-offs between Single Page Applications (SPA) and server-side rendered applications for the OTIS front end?",
        "How do we assess progressive web app (PWA) capabilities for providing offline functionality similar to terminal-based COBOL applications?",
        "What front end framework features are most important for replicating COBOL screen navigation patterns and data entry forms?",
        "How should we evaluate component libraries (Material-UI, Ant Design, Bootstrap) for rapid UI development?",
        "What state management solutions (Redux, MobX, Vuex, Context API) are appropriate for managing complex application state replacing COBOL working storage?",
        "How do we evaluate TypeScript vs JavaScript for type safety when working with APIs converted from COBOL?",
        "What build tools and bundlers (Webpack, Vite, Rollup) should we use for optimizing front end performance?",
        "How should we approach responsive design to support multiple device types when COBOL only supported fixed terminals?",
        "What accessibility (WCAG) standards should we implement to ensure the new front end is accessible to all users?",
        "How do we evaluate front end testing frameworks (Jest, Cypress, Playwright) for ensuring UI quality?",
        "What internationalization (i18n) and localization (l10n) capabilities should we build in from the start?",
        "How should we approach UI theme customization and branding in the new front end?",
        "What front end performance monitoring and analytics tools should we integrate?",
        "How do we evaluate micro-frontend architectures if we need to support gradual migration from COBOL screens?",
        "What browser compatibility requirements should we support, and how does this influence technology choices?",
        
        # Section 12.2: Consuming APIs (Q18-33)
        "What patterns should we use for making HTTP requests to C# APIs from the front end (Fetch API, Axios, React Query)?",
        "How should we implement API client code generation from OpenAPI specifications to ensure type safety?",
        "What strategies should we use for handling API authentication and token management in the front end (JWT, OAuth)?",
        "How do we implement refresh token logic and session management in the front end application?",
        "What approach should we take for handling API errors and displaying user-friendly error messages?",
        "How should we implement loading states, progress indicators, and skeleton screens for better user experience during API calls?",
        "What patterns should we use for implementing optimistic updates and rollback on API failures?",
        "How do we implement request cancellation and debouncing to prevent unnecessary API calls?",
        "What caching strategies should we use on the front end to reduce API calls and improve performance?",
        "How should we handle API versioning in the front end to support gradual migration?",
        "What approach should we take for implementing real-time updates (WebSockets, SignalR, Server-Sent Events) if needed?",
        "How do we implement pagination, infinite scroll, and virtual scrolling for large data sets from APIs?",
        "What patterns should we use for handling file uploads and downloads through APIs?",
        "How should we implement API request retries and circuit breaker patterns on the front end?",
        "What approach should we take for implementing offline capabilities and sync when connection is restored?",
        "How do we implement proper CORS handling and security headers when consuming APIs?",
        
        # Section 12.3: UI/UX Modernization (Q34-50)
        "How do we analyze COBOL screen flows and interactions to design modern, intuitive UI workflows?",
        "What UX research methods should we use to understand user needs and pain points with the current COBOL system?",
        "How should we approach converting COBOL function key navigation to modern UI interactions (buttons, menus, shortcuts)?",
        "What design patterns should we use for data entry forms to replace COBOL screen layouts while improving usability?",
        "How do we implement form validation feedback that is clearer and more helpful than COBOL error messages?",
        "What approach should we take for implementing search and filtering capabilities that go beyond COBOL indexed file lookups?",
        "How should we design navigation menus and breadcrumbs to replace COBOL menu hierarchies?",
        "What patterns should we use for implementing dashboard and summary views that weren't possible in character-based COBOL screens?",
        "How do we implement data visualization (charts, graphs) to provide insights not available in COBOL reports?",
        "What approach should we take for implementing drag-and-drop, multi-select, and other modern interaction patterns?",
        "How should we design mobile-first responsive layouts that adapt gracefully to different screen sizes?",
        "What patterns should we use for implementing wizards and multi-step processes replacing COBOL screen sequences?",
        "How do we implement contextual help and tooltips to reduce user training time compared to COBOL function key help?",
        "What approach should we take for implementing keyboard shortcuts for power users familiar with COBOL function keys?",
        "How should we design confirmation dialogs and destructive action warnings improving on COBOL confirmations?",
        "What patterns should we use for implementing bulk operations and batch actions in the UI?",
        "How do we implement user preferences and customization options (themes, layout, default values)?"
    ]
    
    results = []
    
    print(f"Starting Chapter 12 research: {len(questions)} questions")
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
            with open("chapter12_frontend_research.json", "w", encoding="utf-8") as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            
            print(f"✓ Completed in {result['timing']:.2f}s (Sources: {qa_pair['sources_count']})")
            
        except Exception as e:
            print(f"✗ Error on Q{i}: {e}")
            continue
    
    print("\n" + "=" * 60)
    print(f"✅ Research complete! {len(results)}/50 questions answered")
    print(f"Results saved to: chapter12_frontend_research.json")
    
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
