"""Command-line interface for OTIS RAG."""

import argparse
from .rag import OTISRAG


def main():
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="OTIS RAG - Ask questions about COBOL source code"
    )
    parser.add_argument(
        'question',
        nargs='*',
        help='Question to ask (or use interactive mode if omitted)'
    )
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Show routing and retrieval details'
    )
    parser.add_argument(
        '--config',
        default='local.settings.json',
        help='Path to configuration file'
    )
    
    args = parser.parse_args()
    
    # Initialize RAG
    try:
        rag = OTISRAG(config_file=args.config)
        print("✅ OTIS RAG initialized successfully\n")
    except Exception as e:
        print(f"❌ Failed to initialize: {e}")
        return 1
    
    # Interactive mode if no question provided
    if not args.question:
        return interactive_mode(rag, args.verbose)
    
    # Single question mode
    question = ' '.join(args.question)
    answer = rag.ask(question, verbose=args.verbose)
    print(f"\n💬 Question: {question}")
    print(f"\n🤖 Answer:\n{answer}\n")
    
    return 0


def interactive_mode(rag: OTISRAG, verbose: bool):
    """Interactive Q&A mode."""
    print("="*60)
    print("  OTIS RAG - Interactive Mode")
    print("="*60)
    print("\nCommands:")
    print("  • Type your question and press Enter")
    print("  • 'clear' - Clear conversation memory")
    print("  • 'stats' - Show system statistics")
    print("  • 'quit' or 'exit' - Exit")
    print("\n" + "="*60 + "\n")
    
    while True:
        try:
            question = input("💬 You: ").strip()
            
            if not question:
                continue
            
            # Commands
            if question.lower() in ['quit', 'exit', 'q']:
                print("\n👋 Goodbye!\n")
                break
            
            elif question.lower() == 'clear':
                rag.clear_memory()
                print("🗑️  Conversation memory cleared\n")
                continue
            
            elif question.lower() == 'stats':
                stats = rag.get_stats()
                print(f"\n📊 Stats:")
                print(f"  • Conversation turns: {stats['conversation_turns']}/{stats['max_turns']}")
                print(f"  • Indexes available: {stats['indexes_available']}")
                print(f"  • Last query: {stats['last_query'][:50]}...\n")
                continue
            
            # Ask question
            print()  # Blank line before answer
            answer = rag.ask(question, verbose=verbose)
            print(f"🤖 Assistant:\n{answer}\n")
            print("-"*60 + "\n")
        
        except KeyboardInterrupt:
            print("\n\n👋 Goodbye!\n")
            break
        except Exception as e:
            print(f"\n❌ Error: {e}\n")
    
    return 0


if __name__ == '__main__':
    exit(main())
