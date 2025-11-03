#!/usr/bin/env python3
"""
Generate business-level descriptions for COBOL files using LLM.
Updates the 'summary' field in new-cobol-files index with meaningful descriptions.
"""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from openai import AzureOpenAI
import json
import time
from typing import Dict, List
import logging

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class FileDescriptionGenerator:
    """Generate business descriptions for COBOL files using LLM."""
    
    def __init__(self):
        """Initialize with Azure credentials."""
        with open('local.settings.json', 'r') as f:
            config = json.load(f)['Values']
        
        self.search_client = SearchClient(
            endpoint=config['SEARCH_ENDPOINT'],
            index_name='new-cobol-files',
            credential=AzureKeyCredential(config['SEARCH_KEY'])
        )
        
        self.openai_client = AzureOpenAI(
            api_key=config['AZURE_OPENAI_KEY'],
            api_version="2024-08-01-preview",
            azure_endpoint=config['AZURE_OPENAI_ENDPOINT']
        )
        
        self.chat_deployment = config.get('AZURE_OPENAI_DEPLOYMENT', 'gpt-4.1')
        
    def generate_description(self, file_record: Dict) -> str:
        """Generate a business-level description for a COBOL file."""
        
        path = file_record.get('path', '')
        program_id = file_record.get('programId', '')
        kind = file_record.get('kind', 'program')
        content = file_record.get('content', '')
        copybooks = file_record.get('copybooksUsed', [])
        has_sql = file_record.get('hasSQL', False)
        has_cics = file_record.get('hasCICS', False)
        has_screens = file_record.get('hasScreens', False)
        
        # Build context for LLM
        context_parts = []
        context_parts.append(f"File: {path}")
        context_parts.append(f"Program ID: {program_id}")
        context_parts.append(f"Type: {kind}")
        
        features = []
        if has_sql:
            features.append("uses SQL database operations")
        if has_cics:
            features.append("uses CICS transaction processing")
        if has_screens:
            features.append("has screen/UI definitions")
        if copybooks:
            features.append(f"uses {len(copybooks)} copybooks")
        
        if features:
            context_parts.append(f"Features: {', '.join(features)}")
        
        # Truncate content to first 3000 chars (enough for context)
        code_sample = content[:3000] if content else ""
        
        prompt = f"""Analyze this COBOL {kind} and provide a concise 1-2 sentence business description of what it does.

{chr(10).join(context_parts)}

Code (first 3000 chars):
```cobol
{code_sample}
```

Provide a clear description focusing on:
1. The business purpose (what it accomplishes for users/business)
2. Key operations or data it handles

Format: Just 1-2 sentences, no preamble. Start with "This {kind}..." or "Handles..."

Example good descriptions:
- "This program processes daily loan payments, updates account balances, and generates payment confirmation reports for the loan servicing system."
- "Handles customer account inquiries by retrieving account details from the database and displaying balance, transaction history, and payment schedules."
- "This copybook defines the data structure for loan master records including borrower information, loan amounts, interest rates, and payment schedules."
"""
        
        try:
            response = self.openai_client.chat.completions.create(
                model=self.chat_deployment,
                messages=[
                    {"role": "system", "content": "You are a COBOL code analyst who creates concise business descriptions. Focus on business purpose, not technical implementation details."},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.3,  # Low for consistency
                max_tokens=200
            )
            
            description = response.choices[0].message.content.strip()
            return description
            
        except Exception as e:
            logger.error(f"Failed to generate description for {program_id}: {e}")
            return None
    
    def update_file_description(self, file_id: str, description: str) -> bool:
        """Update a file record with new description."""
        try:
            # Merge update - only update the summary field
            self.search_client.merge_documents([{
                'id': file_id,
                'summary': description
            }])
            return True
        except Exception as e:
            logger.error(f"Failed to update {file_id}: {e}")
            return False
    
    def process_batch(self, batch: List[Dict], start_idx: int) -> tuple:
        """Process a batch of files."""
        success_count = 0
        fail_count = 0
        
        for i, record in enumerate(batch, start=start_idx):
            file_id = record.get('id')
            program_id = record.get('programId', 'Unknown')
            
            logger.info(f"[{i}] Processing {program_id} ({file_id})...")
            
            # Generate description
            description = self.generate_description(record)
            
            if description:
                # Update in index
                if self.update_file_description(file_id, description):
                    logger.info(f"  ✓ Updated: {description[:100]}...")
                    success_count += 1
                else:
                    logger.error(f"  ✗ Failed to update")
                    fail_count += 1
            else:
                logger.error(f"  ✗ Failed to generate description")
                fail_count += 1
            
            # Rate limiting - avoid overwhelming OpenAI API
            time.sleep(0.5)
        
        return success_count, fail_count
    
    def run(self, batch_size: int = 10, max_records: int = None, resume_from: int = 0):
        """Run the description generation process."""
        logger.info("=" * 80)
        logger.info("STARTING FILE DESCRIPTION GENERATION")
        logger.info("=" * 80)
        logger.info(f"Batch size: {batch_size}")
        if max_records:
            logger.info(f"Max records: {max_records}")
        if resume_from:
            logger.info(f"Resuming from: {resume_from}")
        logger.info("")
        
        # Get all file records
        all_files = list(self.search_client.search(
            '',
            select=['id', 'path', 'programId', 'kind', 'content', 'copybooksUsed', 
                    'hasSQL', 'hasCICS', 'hasScreens', 'summary'],
            top=10000  # Get all files
        ))
        
        logger.info(f"Found {len(all_files)} files in index")
        
        # Apply limits
        if resume_from:
            all_files = all_files[resume_from:]
        if max_records:
            all_files = all_files[:max_records]
        
        logger.info(f"Processing {len(all_files)} files")
        logger.info("")
        
        total_success = 0
        total_fail = 0
        
        # Process in batches
        for i in range(0, len(all_files), batch_size):
            batch = all_files[i:i + batch_size]
            batch_num = i // batch_size + 1
            total_batches = (len(all_files) + batch_size - 1) // batch_size
            
            logger.info(f"BATCH {batch_num}/{total_batches}")
            logger.info("-" * 80)
            
            success, fail = self.process_batch(batch, resume_from + i + 1)
            total_success += success
            total_fail += fail
            
            logger.info(f"Batch complete: {success} success, {fail} failed")
            logger.info("")
        
        # Final stats
        logger.info("=" * 80)
        logger.info("GENERATION COMPLETE")
        logger.info("=" * 80)
        logger.info(f"Total processed: {len(all_files)}")
        logger.info(f"Success: {total_success}")
        logger.info(f"Failed: {total_fail}")
        logger.info(f"Success rate: {100 * total_success / len(all_files):.1f}%")
        logger.info("=" * 80)


def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate business descriptions for COBOL files')
    parser.add_argument('--batch-size', type=int, default=10, help='Number of files to process per batch')
    parser.add_argument('--max-records', type=int, help='Maximum number of files to process (for testing)')
    parser.add_argument('--resume-from', type=int, default=0, help='Resume from this file index')
    
    args = parser.parse_args()
    
    generator = FileDescriptionGenerator()
    generator.run(
        batch_size=args.batch_size,
        max_records=args.max_records,
        resume_from=args.resume_from
    )


if __name__ == '__main__':
    main()
