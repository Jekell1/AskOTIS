"""
Phase 1 Implementation: FREE Prompt Engineering Improvements

This script implements Phase 1 improvements to the OTIS RAG system:
1. Enhanced graph traversal prompts
2. Historical context emphasis
3. Cross-reference analysis using variable_usage
4. Improved comment surfacing with multi-pass retrieval

Cost: $0 (prompt engineering only)
Expected Improvement: 15-25% better answers for challenging questions
Timeline: 0-2 weeks
"""

import os
import sys
from pathlib import Path

# Phase 1 improvements to implement
PHASE1_IMPROVEMENTS = {
    "graph_traversal": {
        "title": "Enhanced Graph Traversal Prompts",
        "description": "Better use of flow_edges to trace multi-program workflows",
        "file": "otis_rag/prompts.py",
        "changes": [
            "Add workflow traversal instructions",
            "Emphasize following CALLS edges for multi-hop analysis",
            "Include conditional logic from code context"
        ],
        "example": """
        When asked about multi-program workflows:
        1. First, retrieve all CALLS edges from the starting program
        2. For each called program, retrieve ITS calls (2nd hop)
        3. Assemble the sequence with conditions from code
        4. Present as: "User action → Screen X → Program A (validates) → 
           Program B (if type=RP, processes refund) → Program C (updates ledger)"
        """
    },
    
    "historical_context": {
        "title": "Historical Context Emphasis",
        "description": "Better surfacing of DATE-WRITTEN, comments, and design rationale",
        "file": "otis_rag/prompts.py",
        "changes": [
            "Add instructions to check DATE-WRITTEN field",
            "Emphasize looking for comments with WHY/REASON/PURPOSE",
            "Suggest checking older programs for historical patterns"
        ],
        "example": """
        When asked about WHY a design decision was made:
        1. Check program DATE-WRITTEN field for age context
        2. Look for comments containing "REASON:", "WHY:", "PURPOSE:", "NOTE:"
        3. Compare with similar programs from same era
        4. Note if pattern appears in multiple old programs (suggests standard practice)
        5. Acknowledge when historical context is unclear vs definitive
        """
    },
    
    "cross_reference": {
        "title": "Cross-Reference Analysis",
        "description": "Use variable_usage index to trace data flow",
        "file": "otis_rag/prompts.py",
        "changes": [
            "Add instructions for tracing variable usage across programs",
            "Emphasize checking which programs READ/WRITE same files",
            "Include data flow analysis in answers"
        ],
        "example": """
        When asked about data flow or dependencies:
        1. Query variable_usage for the target variable/file
        2. Identify all programs that READ it (consumers)
        3. Identify all programs that WRITE it (producers)
        4. Map the flow: Writer → File → Reader
        5. Present as: "APIPAY writes to PAYMENT-FILE (fields X, Y, Z), 
           which is read by REFUPD and LONPW9 for validation and reporting"
        """
    },
    
    "comment_surfacing": {
        "title": "Improved Comment Surfacing",
        "description": "Multi-pass retrieval to find relevant comments",
        "file": "otis_rag/prompts.py",
        "changes": [
            "Add instructions for searching comments index specifically",
            "Suggest querying with multiple phrasings",
            "Emphasize paragraph-level comments for business logic"
        ],
        "example": """
        When asked about business rules or logic:
        1. First pass: Query code_comments for the program
        2. Second pass: Query paragraphs for specific logic sections
        3. Look for comments near key statements (IF, EVALUATE, PERFORM)
        4. Present comments in context with code they explain
        5. Note: "The comment at line X explains: [comment text]"
        """
    }
}


def print_header(text):
    """Print a formatted header."""
    print("\n" + "=" * 80)
    print(text.center(80))
    print("=" * 80 + "\n")


def print_improvement(key, details):
    """Print details of an improvement."""
    print(f"📝 {details['title']}")
    print(f"   Description: {details['description']}")
    print(f"   Target file: {details['file']}")
    print(f"\n   Changes to implement:")
    for change in details['changes']:
        print(f"   - {change}")
    print(f"\n   Example instructions:")
    print(details['example'])
    print("-" * 80 + "\n")


def check_prompts_file():
    """Check if prompts.py exists and show its current structure."""
    prompts_path = Path("otis_rag/prompts.py")
    
    if not prompts_path.exists():
        print(f"⚠️  File not found: {prompts_path}")
        print(f"   We'll need to create a new prompts module.")
        return False
    
    print(f"✅ Found existing file: {prompts_path}")
    
    # Check file size
    file_size = prompts_path.stat().st_size
    print(f"   File size: {file_size:,} bytes")
    
    # Count lines
    with open(prompts_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        print(f"   Lines: {len(lines)}")
    
    return True


def show_implementation_plan():
    """Show the step-by-step implementation plan."""
    print_header("PHASE 1 IMPLEMENTATION PLAN")
    
    print("🎯 GOAL: Improve RAG answers by 15-25% through better prompts")
    print("💰 COST: $0 (no new data, embeddings, or API costs)")
    print("⏱️  TIME: 0-2 weeks\n")
    
    print("=" * 80)
    print("IMPROVEMENTS TO IMPLEMENT")
    print("=" * 80 + "\n")
    
    for key, details in PHASE1_IMPROVEMENTS.items():
        print_improvement(key, details)
    
    print("=" * 80)
    print("IMPLEMENTATION STEPS")
    print("=" * 80 + "\n")
    
    steps = [
        {
            "step": 1,
            "title": "Review current prompts.py structure",
            "actions": [
                "Check if file exists",
                "Identify where system prompts are defined",
                "Look for existing workflow/context instructions"
            ]
        },
        {
            "step": 2,
            "title": "Add graph traversal instructions",
            "actions": [
                "Create detailed workflow analysis prompt",
                "Add multi-hop traversal guidelines",
                "Include conditional logic extraction"
            ]
        },
        {
            "step": 3,
            "title": "Add historical context emphasis",
            "actions": [
                "Add DATE-WRITTEN checking instructions",
                "Emphasize comment keyword search",
                "Add historical pattern recognition guidance"
            ]
        },
        {
            "step": 4,
            "title": "Add cross-reference analysis",
            "actions": [
                "Add variable_usage query instructions",
                "Include data flow mapping guidance",
                "Add producer-consumer analysis"
            ]
        },
        {
            "step": 5,
            "title": "Add comment surfacing improvements",
            "actions": [
                "Add multi-pass retrieval strategy",
                "Emphasize paragraph-level comments",
                "Include context-aware comment presentation"
            ]
        },
        {
            "step": 6,
            "title": "Test with sample questions",
            "actions": [
                "Test complex workflow questions",
                "Test historical/design decision questions",
                "Test implicit business rule questions",
                "Test performance/optimization questions"
            ]
        },
        {
            "step": 7,
            "title": "Measure improvement",
            "actions": [
                "Compare answers before/after",
                "Calculate improvement percentage",
                "Document which question types improved most",
                "Collect user feedback"
            ]
        }
    ]
    
    for step_info in steps:
        print(f"Step {step_info['step']}: {step_info['title']}")
        for action in step_info['actions']:
            print(f"  • {action}")
        print()
    
    print("=" * 80)
    print("NEXT ACTIONS")
    print("=" * 80 + "\n")
    
    print("1. Review current otis_rag/prompts.py structure")
    print("2. Create enhanced prompt templates")
    print("3. Update system prompts with Phase 1 improvements")
    print("4. Test with challenging questions")
    print("5. Measure improvement vs baseline")
    print()


def main():
    """Main entry point."""
    print_header("PHASE 1: FREE PROMPT IMPROVEMENTS")
    
    # Check if prompts file exists
    has_prompts = check_prompts_file()
    print()
    
    # Show implementation plan
    show_implementation_plan()
    
    print("=" * 80)
    print("READY TO PROCEED?")
    print("=" * 80 + "\n")
    
    print("Next steps:")
    print("1. Review the implementation plan above")
    print("2. We'll examine otis_rag/prompts.py")
    print("3. We'll create enhanced prompt templates")
    print("4. We'll test improvements with sample questions")
    print()
    print("Would you like to proceed? (This is FREE - no costs involved)")
    print()


if __name__ == "__main__":
    main()
