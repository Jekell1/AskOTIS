#!/usr/bin/env python3
"""
Chapter 13 Research: Data Migration Strategies
Gathers answers to 50 questions about extracting, transforming, and loading data
from COBOL legacy systems to modern C# architecture.
"""

import json
import time
from otis_rag import OTISRAG

def main():
    rag = OTISRAG()
    
    questions = [
        # Section 13.1: Extracting Data from Legacy Files (Q1-17)
        "What types of data files does the OTIS COBOL system use (VSAM, sequential, indexed) and how are they organized?",
        "How can we inventory all the data structures, copybooks, and file definitions used in the OTIS COBOL programs?",
        "What are the best tools and techniques for extracting data from COBOL VSAM files to modern formats like CSV or JSON?",
        "How should we handle COBOL packed decimal (COMP-3) and binary data types during extraction?",
        "What strategies exist for extracting data incrementally vs. doing a full bulk extraction?",
        "How can we preserve relationships between different COBOL files during extraction (e.g., master-detail relationships)?",
        "What are the risks of data loss or corruption during the extraction phase, and how can we mitigate them?",
        "How should we handle REDEFINES clauses in COBOL copybooks when extracting data?",
        "What documentation should we create during the extraction phase to map legacy file structures to new schemas?",
        "How can we validate that extracted data matches the source files (checksums, record counts, etc.)?",
        "What role do COBOL copybooks play in understanding the structure of extracted data?",
        "How should we handle multi-format files where record layouts vary based on record type indicators?",
        "What are best practices for extracting data from COBOL indexed files with alternate keys?",
        "How can we extract embedded business rules or validation logic from COBOL data definitions?",
        "What strategies exist for extracting data from COBOL programs that use SORT or MERGE operations?",
        "How should we handle temporal data (dates, timestamps) when extracting from COBOL files?",
        "What security considerations exist when extracting sensitive financial data from legacy systems?",
        
        # Section 13.2: Data Cleansing and Transformation (Q18-34)
        "What common data quality issues exist in legacy COBOL systems (duplicates, nulls, inconsistencies)?",
        "How can we identify and resolve data quality problems before loading into the new system?",
        "What transformation rules are needed to convert COBOL data types to C# / SQL Server data types?",
        "How should we handle COBOL 'spaces' or 'zeros' as indicators of null or missing values?",
        "What strategies exist for normalizing denormalized COBOL file structures during transformation?",
        "How can we transform hierarchical COBOL data structures into relational database schemas?",
        "What ETL (Extract, Transform, Load) tools are best suited for COBOL-to-C# data migration?",
        "How should we handle data referential integrity issues discovered during cleansing?",
        "What role does data profiling play in understanding legacy data before transformation?",
        "How can we implement data validation rules during the transformation phase?",
        "What strategies exist for handling encoding issues (EBCDIC to ASCII) during transformation?",
        "How should we transform COBOL date formats (YYMMDD, YYYYMMDD) to modern date types?",
        "What approaches work best for deduplicating records during data transformation?",
        "How can we implement business rule transformations (e.g., status code mappings) during migration?",
        "What testing frameworks or tools can validate transformation logic before production migration?",
        "How should we handle orphaned records or broken relationships during data cleansing?",
        "What documentation should track all transformation rules applied during data migration?",
        
        # Section 13.3: Loading Data into New Systems (Q35-50)
        "What strategies exist for loading large volumes of data into SQL Server or other modern databases?",
        "How can we use bulk loading techniques (SQL Server BULK INSERT, BCP) to optimize load performance?",
        "What are the trade-offs between ETL tools (SSIS, Azure Data Factory) vs. custom C# load programs?",
        "How should we handle transaction boundaries when loading data into the new system?",
        "What rollback strategies should be in place if data loading fails midway?",
        "How can we implement idempotent load operations to support retry logic?",
        "What indexing strategies optimize load performance (load first, then create indexes)?",
        "How should we handle foreign key constraints during the data loading phase?",
        "What validation checks should run after data loading to ensure completeness and accuracy?",
        "How can we implement parallel loading to speed up the migration process?",
        "What logging and auditing should track the data loading process for troubleshooting?",
        "How should we handle slowly changing dimensions (SCD) if historical data needs versioning?",
        "What strategies exist for loading data into microservices vs. monolithic databases?",
        "How can we implement data reconciliation reports comparing source vs. target after loading?",
        "What cutover strategies minimize downtime during the final data migration?",
        "How should we handle incremental loads vs. full reloads during testing and production cutover?",
    ]
    
    results = []
    
    print(f"\n{'='*60}")
    print(f"CHAPTER 13 RESEARCH: Data Migration Strategies")
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
        with open("chapter13_data_migration_research.json", "w", encoding="utf-8") as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"✓ Answer received ({result['sources_count']} sources, {elapsed:.2f}s)")
    
    print(f"\n{'='*60}")
    print(f"✅ CHAPTER 13 RESEARCH COMPLETE!")
    print(f"{'='*60}")
    print(f"Results saved to: chapter13_data_migration_research.json")
    print(f"Total questions answered: {len(results)}")

if __name__ == "__main__":
    main()
