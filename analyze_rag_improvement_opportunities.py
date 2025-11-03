"""
Analyze how to improve RAG answers for challenging question types.

These question types currently have lower reliability:
1. Complex multi-step workflows requiring inference
2. Implicit business rules not explicitly coded
3. Historical context or reasons for design decisions
4. Performance characteristics or optimization details
"""

from typing import Dict, List
import json

def analyze_improvement_strategies():
    """Analyze strategies to improve answers for challenging questions."""
    
    improvements = {
        "1_complex_workflows": {
            "challenge": "Complex multi-step workflows requiring inference",
            "current_limitations": [
                "flow_edges (385K) captures direct program-to-program calls",
                "ui_paths (2K) captures screen navigation sequences",
                "Missing: Cross-program transaction flows spanning multiple systems",
                "Missing: Conditional flow logic (IF payment type = X THEN call Y)",
                "Missing: Loop structures across multiple programs"
            ],
            "data_sources_available": {
                "code_comments": "APIPAY shows historical workflow comments ('LOGIC WAS TAKEN FROM REFUPD')",
                "flow_edges": "385K edges with kind/subkind (CALLS, PERFORMS, SEQUENCE)",
                "calls_index": "16K call records with caller/callee relationships",
                "program_flows": "9.7K flow documents describing program logic",
                "screen_nodes": "1.6K nodes showing screen transitions"
            },
            "improvement_strategies": [
                {
                    "strategy": "Enhanced Flow Documents with Workflow Narratives",
                    "description": "Generate natural language descriptions of multi-program workflows",
                    "implementation": [
                        "Analyze call chains: Program A → B → C → D",
                        "Extract conditional logic from COBOL: IF WS-TRANS-TYPE = 'RP' PERFORM PROCESS-REFUND",
                        "Generate workflow narrative: 'When processing a refund (RP), system calls APIPAY which validates payment, then REFUPD updates balances, then LONPW9 generates notices'",
                        "Store in new workflow_descriptions index with embeddings"
                    ],
                    "cost_estimate": "~$50-100 (generate descriptions for ~1000 complex workflows)",
                    "roi": "HIGH - directly answers 'walk me through' questions"
                },
                {
                    "strategy": "Transaction Type Taxonomy",
                    "description": "Build comprehensive mapping of transaction codes to workflows",
                    "implementation": [
                        "Extract all transaction codes from programs (RP, PL, P2, RE, etc.)",
                        "Map each code to: programs involved, screens touched, data updated",
                        "Store as structured documents: {tx_code: 'RP', workflow: [...], description: '...'}",
                        "Index with embeddings for semantic search"
                    ],
                    "cost_estimate": "~$20-30 (if ~500 unique transaction types)",
                    "roi": "MEDIUM-HIGH - answers 'what happens when user enters RP?'"
                },
                {
                    "strategy": "Enhanced Graph Traversal Prompts",
                    "description": "Improve RAG prompts to better utilize existing flow_edges",
                    "implementation": [
                        "Current: RAG retrieves edges but doesn't chain them well",
                        "Improved: Multi-hop retrieval - get Program A calls, then get calls from those programs",
                        "Add reasoning step: 'Based on these edges, infer the sequence...'",
                        "Use chain-of-thought prompting for workflow assembly"
                    ],
                    "cost_estimate": "$0 - prompt engineering only",
                    "roi": "MEDIUM - better use of existing data"
                }
            ],
            "recommended_action": "START WITH: Enhanced graph traversal prompts (no cost), THEN: Transaction taxonomy ($20-30), CONSIDER: Workflow narratives if budget allows ($50-100)"
        },
        
        "2_implicit_business_rules": {
            "challenge": "Implicit business rules not explicitly coded",
            "current_limitations": [
                "Business rules embedded in nested IFs, complex EVALUATE statements",
                "Validation rules scattered across multiple copybooks",
                "Constants defined but purpose unclear (e.g., WS-MAX-AMOUNT PIC 9(7) VALUE 9999999)",
                "Cross-field dependencies (if field A = X then field B must be Y)"
            ],
            "data_sources_available": {
                "code_comments": "Some programs have business rule comments",
                "data_items": "536K data definitions with some VALUE clauses",
                "variables": "107K variable usage records",
                "paragraphs": "225K paragraph records (may contain validation logic)"
            },
            "improvement_strategies": [
                {
                    "strategy": "Business Rule Extraction Pipeline",
                    "description": "Parse COBOL to extract validation rules as structured documents",
                    "implementation": [
                        "Pattern detection: IF statements with validation logic",
                        "Extract: Field name, condition, action, error message",
                        "Example: 'IF WS-AMOUNT > 9999999 MOVE 'AMOUNT TOO LARGE' TO ERROR-MSG'",
                        "Generate rule: {field: 'WS-AMOUNT', constraint: 'max_value', limit: 9999999, message: '...'}",
                        "Store in business_rules index with semantic embeddings"
                    ],
                    "cost_estimate": "~$40-60 (embed ~10K extracted rules with descriptions)",
                    "roi": "HIGH - directly answers 'what are validation rules for X?'"
                },
                {
                    "strategy": "Cross-Reference Analysis",
                    "description": "Identify implicit dependencies between fields/programs",
                    "implementation": [
                        "Analyze variable_usage (107K records) for temporal patterns",
                        "Find: Field A set in paragraph X, then field B checked in paragraph Y",
                        "Generate dependency: 'Field B validation depends on field A being set first'",
                        "Use existing data_items + variables indexes"
                    ],
                    "cost_estimate": "$0 - analysis of existing data",
                    "roi": "MEDIUM - better answers to 'why does this validation exist?'"
                },
                {
                    "strategy": "Comment Mining for Business Context",
                    "description": "Extract business explanations from comments",
                    "implementation": [
                        "Scan all code comments for business terms (customer, payment, balance, etc.)",
                        "Associate comments with nearby code structures",
                        "Example: Comment 'LOGIC WAS TAKEN FROM REFUPD' → associate with APIPAY paragraphs",
                        "Already indexed in code/code_new text field, but enhance retrieval prompts"
                    ],
                    "cost_estimate": "$0 - improve prompts to surface comments",
                    "roi": "LOW-MEDIUM - works for well-commented code only"
                }
            ],
            "recommended_action": "START WITH: Business rule extraction ($40-60 for high ROI), THEN: Cross-reference analysis ($0), ENHANCE: Comment-focused retrieval prompts"
        },
        
        "3_historical_context": {
            "challenge": "Historical context or reasons for design decisions",
            "current_limitations": [
                "Comments like 'LOGIC WAS TAKEN FROM REFUPD' exist but not emphasized",
                "DATE-WRITTEN field exists (APIPAY: 08/16/2023) but not searchable",
                "Change logs in comments not extracted (e.g., '2024.0206 BAH #1641')",
                "No connection to external docs (requirements, design docs, tickets)"
            ],
            "data_sources_available": {
                "code_comments": "Historical comments exist (APIPAY example shows this works)",
                "program_meta": "9.7K programs with metadata (could add date fields)",
                "files_index": "10K files with basic metadata"
            },
            "improvement_strategies": [
                {
                    "strategy": "Historical Comment Extraction",
                    "description": "Extract and enhance historical comments as separate documents",
                    "implementation": [
                        "Scan comments for keywords: 'DATE-WRITTEN', 'TAKEN FROM', 'MODIFIED', 'BAH #', 'CHANGE LOG'",
                        "Extract change history: {date: '2024.0206', author: 'BAH', ticket: '#1641', program: 'APIPAY'}",
                        "Extract lineage: {program: 'APIPAY', derived_from: ['REFUPD', 'LONPW9', 'OTHUPD', 'LONPWA']}",
                        "Store in program_history index with embeddings",
                        "Link to related programs for 'family tree' queries"
                    ],
                    "cost_estimate": "~$30-50 (extract and embed history for ~5K programs with good comments)",
                    "roi": "MEDIUM-HIGH - answers 'why was this program created?', 'what changed?'"
                },
                {
                    "strategy": "Program Lineage Graph",
                    "description": "Build graph of program evolution (X derived from Y, replaced by Z)",
                    "implementation": [
                        "Parse 'LOGIC TAKEN FROM' comments to build lineage",
                        "Analyze similar program names (LONPW9 → LONPWA = variants?)",
                        "Store as graph edges: {source: 'REFUPD', derived: 'APIPAY', relationship: 'logic_source'}",
                        "Add to flow_edges index with kind='LINEAGE'"
                    ],
                    "cost_estimate": "~$10-20 (minimal new embeddings, mostly analysis)",
                    "roi": "MEDIUM - answers 'what programs are related?', 'evolution of X?'"
                },
                {
                    "strategy": "Enhanced Prompt for Historical Questions",
                    "description": "Train RAG to recognize and emphasize historical clues",
                    "implementation": [
                        "Add examples to system prompt: 'When asked about history, look for DATE-WRITTEN, TAKEN FROM, CHANGE LOG'",
                        "Boost relevance of comments with dates/ticket numbers",
                        "Multi-pass retrieval: First get program, then specifically search its comments"
                    ],
                    "cost_estimate": "$0 - prompt engineering",
                    "roi": "LOW-MEDIUM - works for programs with good comments (like APIPAY)"
                }
            ],
            "recommended_action": "START WITH: Enhanced prompts for historical context ($0), THEN: Historical comment extraction ($30-50), CONSIDER: Program lineage graph ($10-20)"
        },
        
        "4_performance_characteristics": {
            "challenge": "Performance characteristics or optimization details",
            "current_limitations": [
                "No execution statistics (how long does program X take?)",
                "No resource usage data (memory, CPU, I/O)",
                "Can't identify bottlenecks without runtime data",
                "Optimization opportunities not documented"
            ],
            "data_sources_available": {
                "code_structure": "Can analyze loops, nested calls, file operations",
                "flow_edges": "385K edges show call patterns (many calls = potential bottleneck)",
                "paragraphs": "225K paragraphs (can identify large/complex paragraphs)"
            },
            "improvement_strategies": [
                {
                    "strategy": "Static Code Analysis for Performance",
                    "description": "Analyze code structure to identify potential performance issues",
                    "implementation": [
                        "Count loop depths (PERFORM VARYING nested 3+ levels = warning)",
                        "Identify file operations in loops (READ inside PERFORM VARYING)",
                        "Find programs with many calls (APIPAY calls 7+ programs = coordination overhead)",
                        "Detect sequential scans (READ ... NEXT without KEY)",
                        "Store as performance_insights: {program: 'X', issue: 'nested_loops', severity: 'medium'}"
                    ],
                    "cost_estimate": "~$20-30 (analyze 10K programs, embed insights)",
                    "roi": "MEDIUM - answers 'what are performance risks in X?'"
                },
                {
                    "strategy": "Complexity Metrics",
                    "description": "Calculate and index cyclomatic complexity, lines of code, etc.",
                    "implementation": [
                        "Add to program_meta: {program: 'X', loc: 5000, complexity: 45, file_ops: 12}",
                        "Already have paragraph count, call count - enhance these",
                        "Generate descriptions: 'APIPAY is medium complexity (285 LOC, 7 calls, 3 file operations)'",
                        "Embed these descriptions for semantic search"
                    ],
                    "cost_estimate": "~$10-15 (embed complexity summaries for 10K programs)",
                    "roi": "LOW-MEDIUM - answers 'how complex is X?', useful for prioritization"
                },
                {
                    "strategy": "Runtime Data Integration (Future)",
                    "description": "If runtime logs/APM data available, index it",
                    "implementation": [
                        "Parse CICS/mainframe logs for execution times",
                        "Extract: {program: 'X', avg_duration_ms: 450, call_count_per_day: 5000}",
                        "Index with embeddings for semantic queries",
                        "Correlate with code structure (slow programs with nested loops)"
                    ],
                    "cost_estimate": "~$50-100 (if logs available and need parsing)",
                    "roi": "HIGH - but requires external data source"
                },
                {
                    "strategy": "Optimization Pattern Library",
                    "description": "Document known optimization patterns in COBOL",
                    "implementation": [
                        "Create knowledge base: 'Use SEARCH ALL instead of sequential SEARCH'",
                        "Store optimization patterns with examples",
                        "RAG can suggest: 'This code uses sequential search, consider SEARCH ALL'",
                        "Small index (~100 patterns), curated manually"
                    ],
                    "cost_estimate": "~$5-10 (embed small pattern library)",
                    "roi": "LOW - generic advice, not specific to codebase"
                }
            ],
            "recommended_action": "START WITH: Static code analysis ($20-30 for good insights), THEN: Complexity metrics ($10-15), WAIT ON: Runtime data (requires external source), SKIP: Generic patterns (low value)"
        }
    }
    
    # Overall recommendations
    recommendations = {
        "immediate_actions_free": [
            "Enhanced graph traversal prompts (complex workflows)",
            "Historical context prompts (emphasize DATE-WRITTEN, TAKEN FROM)",
            "Cross-reference analysis using existing data (implicit rules)",
            "Better comment surfacing for business context"
        ],
        "high_roi_investments": [
            {
                "investment": "Business Rule Extraction",
                "cost": "$40-60",
                "benefit": "Directly answers validation/constraint questions",
                "priority": "HIGH"
            },
            {
                "investment": "Enhanced Workflow Narratives",
                "cost": "$50-100",
                "benefit": "Answers complex multi-step transaction questions",
                "priority": "HIGH"
            },
            {
                "investment": "Historical Comment Extraction",
                "cost": "$30-50",
                "benefit": "Answers 'why' and 'when' questions",
                "priority": "MEDIUM-HIGH"
            },
            {
                "investment": "Static Performance Analysis",
                "cost": "$20-30",
                "benefit": "Identifies potential bottlenecks and risks",
                "priority": "MEDIUM"
            },
            {
                "investment": "Transaction Type Taxonomy",
                "cost": "$20-30",
                "benefit": "Maps user actions to system workflows",
                "priority": "MEDIUM"
            }
        ],
        "total_cost_high_priority": "$120-210 for major improvements",
        "phased_approach": {
            "phase_1_free": "Prompt engineering (0-2 weeks, $0)",
            "phase_2_quick_wins": "Transaction taxonomy + complexity metrics ($30-45, 2-4 weeks)",
            "phase_3_major_value": "Business rules + workflow narratives ($90-160, 4-8 weeks)",
            "phase_4_historical": "Historical extraction + lineage ($40-70, 2-4 weeks)"
        }
    }
    
    return improvements, recommendations


def main():
    print("=" * 80)
    print("RAG IMPROVEMENT OPPORTUNITIES ANALYSIS")
    print("=" * 80)
    print()
    
    improvements, recommendations = analyze_improvement_strategies()
    
    # Print each challenge area
    for key, data in improvements.items():
        print(f"\n{'=' * 80}")
        print(f"CHALLENGE: {data['challenge']}")
        print(f"{'=' * 80}\n")
        
        print("Current Limitations:")
        for limitation in data['current_limitations']:
            print(f"  ❌ {limitation}")
        
        print("\nData Sources Available:")
        for source, desc in data['data_sources_available'].items():
            print(f"  📊 {source}: {desc}")
        
        print(f"\nImprovement Strategies ({len(data['improvement_strategies'])} options):")
        for i, strategy in enumerate(data['improvement_strategies'], 1):
            print(f"\n  Strategy {i}: {strategy['strategy']}")
            print(f"  Description: {strategy['description']}")
            print(f"  Cost: {strategy['cost_estimate']}")
            print(f"  ROI: {strategy['roi']}")
            if 'implementation' in strategy:
                print(f"  Implementation:")
                for step in strategy['implementation'][:3]:  # Show first 3 steps
                    print(f"    • {step}")
        
        print(f"\n  ✅ RECOMMENDED: {data['recommended_action']}")
    
    # Print overall recommendations
    print(f"\n\n{'=' * 80}")
    print("OVERALL RECOMMENDATIONS")
    print(f"{'=' * 80}\n")
    
    print("Immediate Actions (FREE - Prompt Engineering):")
    for action in recommendations['immediate_actions_free']:
        print(f"  🎯 {action}")
    
    print("\n\nHigh-ROI Investments:")
    for inv in recommendations['high_roi_investments']:
        print(f"\n  💰 {inv['investment']}")
        print(f"     Cost: {inv['cost']}")
        print(f"     Benefit: {inv['benefit']}")
        print(f"     Priority: {inv['priority']}")
    
    print(f"\n\nTotal Cost for High-Priority Improvements: {recommendations['total_cost_high_priority']}")
    
    print("\n\nPhased Rollout Plan:")
    for phase, desc in recommendations['phased_approach'].items():
        print(f"  📅 {phase.replace('_', ' ').title()}: {desc}")
    
    print("\n\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print("""
The current RAG system excels at structural/navigational questions but struggles with:
1. Multi-step workflows → Need workflow narratives or better graph traversal
2. Implicit business rules → Need rule extraction from COBOL logic
3. Historical context → Need to surface existing comments better
4. Performance → Need static analysis of code complexity

QUICK WINS (FREE):
• Enhance prompts to better traverse flow_edges for multi-program workflows
• Emphasize historical comments (DATE-WRITTEN, TAKEN FROM, CHANGE LOG)
• Use existing 107K variable_usage records for cross-field dependencies

BEST INVESTMENTS ($120-210 total):
• Business rule extraction ($40-60) - HIGH value for validation questions
• Workflow narratives ($50-100) - HIGH value for transaction flow questions
• Historical extraction ($30-50) - MEDIUM value for 'why' questions
• Static performance analysis ($20-30) - MEDIUM value for risk assessment

The data exists (comments, flows, code structure) - we need to:
1. Extract it into dedicated indexes (rules, workflows, history)
2. Generate natural language descriptions
3. Embed for semantic search
4. Enhance prompts to utilize it effectively
    """)
    print("=" * 80)


if __name__ == "__main__":
    main()
