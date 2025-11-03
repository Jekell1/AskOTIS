#!/usr/bin/env python3
"""
Gather research data for Chapter 2: Business Domain Analysis
50 questions about loan servicing, financial operations, business processes, user roles, and workflows
"""

import json
from datetime import datetime
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Loan Servicing and Financial Operations in OTIS (Q1-20)
        "What are the core loan servicing functions performed by the OTIS system?",
        "What types of loans does OTIS handle (consumer, commercial, mortgage, etc.)?",
        "How does OTIS manage the complete loan lifecycle from origination to payoff?",
        "What payment processing capabilities does OTIS provide for loan accounts?",
        "How does OTIS handle interest calculation and accrual for different loan types?",
        "What escrow account management functions does OTIS support?",
        "How does OTIS process late fees, penalties, and default scenarios?",
        "What financial reporting and regulatory compliance features exist in OTIS?",
        "How does OTIS manage loan modifications, refinancing, and restructuring?",
        "What collections and delinquency management processes does OTIS support?",
        "How does OTIS handle loan payoff calculations and final settlements?",
        "What credit bureau reporting and integration capabilities does OTIS have?",
        "How does OTIS manage insurance tracking and forced-placed insurance?",
        "What tax-related functions does OTIS perform (1098 reporting, property taxes)?",
        "How does OTIS handle charge-offs, write-offs, and loss mitigation?",
        "What investor accounting and remittance functions does OTIS support?",
        "How does OTIS manage automated clearing house (ACH) and electronic payment processing?",
        "What general ledger integration and accounting functions does OTIS provide?",
        "How does OTIS handle multi-currency or international loan servicing if applicable?",
        "What financial analytics and portfolio management capabilities exist in OTIS?",
        
        # Key Business Processes Supported (Q21-35)
        "What are the main end-to-end business processes supported by OTIS?",
        "How does OTIS handle the daily payment posting and application process?",
        "What batch processing cycles does OTIS run (daily, monthly, year-end)?",
        "How does OTIS manage customer statement generation and delivery?",
        "What are the key month-end and year-end closing processes in OTIS?",
        "How does OTIS support audit trails and compliance reporting requirements?",
        "What exception handling and research processes exist in OTIS?",
        "How does OTIS manage document imaging and correspondence tracking?",
        "What workflow processes exist for loan boarding and setup in OTIS?",
        "How does OTIS handle payment reversals, adjustments, and corrections?",
        "What are the business rules engines or decision logic frameworks in OTIS?",
        "How does OTIS support disaster recovery and business continuity processes?",
        "What data migration and conversion processes has OTIS undergone historically?",
        "How does OTIS manage rate changes and payment recalculations?",
        "What quality control and reconciliation processes are built into OTIS?",
        
        # User Roles and Workflows (Q36-50)
        "What are the primary user roles that interact with the OTIS system?",
        "What functions do loan servicing representatives perform in OTIS daily?",
        "How do customer service agents use OTIS to respond to customer inquiries?",
        "What capabilities do collection specialists have in OTIS for delinquent accounts?",
        "How do accounting and finance users interact with OTIS for financial reporting?",
        "What administrative and system configuration functions exist in OTIS?",
        "How do managers and supervisors use OTIS for oversight and approvals?",
        "What reporting and analytics access do business analysts have in OTIS?",
        "How do compliance officers use OTIS to monitor regulatory adherence?",
        "What audit and review capabilities exist for internal and external auditors?",
        "What is the typical workflow for processing a customer payment in OTIS?",
        "How do users navigate between OTIS screens and modules in daily operations?",
        "What security and access controls govern different user roles in OTIS?",
        "How do users handle exception cases and escalations in OTIS workflows?",
        "What training and support materials exist for OTIS users across different roles?"
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
    output_file = "chapter2_business_domain_research.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(research_data, f, indent=2, ensure_ascii=False)
    
    total_time = sum(q['query_time_seconds'] for q in research_data)
    avg_time = total_time / len(research_data)
    
    print(f"\n{'='*80}")
    print("✅ CHAPTER 2 RESEARCH COMPLETE!")
    print(f"{'='*80}")
    print(f"Total questions: {len(research_data)}")
    print(f"Total time: {total_time/60:.2f} minutes")
    print(f"Average per question: {avg_time:.2f} seconds")
    print(f"Output saved to: {output_file}")
    print('='*80)

if __name__ == "__main__":
    main()
