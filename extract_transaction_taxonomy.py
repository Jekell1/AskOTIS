"""
Extract Transaction Type Taxonomy from OTIS COBOL Programs

This script identifies transaction codes and builds a comprehensive mapping of:
- Transaction codes (RP, PL, P2, RE, etc.)
- Programs involved in each transaction
- Screens used
- Data files touched
- Business purpose/description

Stores results for indexing with embeddings.
"""

import os
import re
from pathlib import Path
from collections import defaultdict
import json
from typing import Dict, List, Set, Tuple
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

# Azure Search connection
SEARCH_ENDPOINT = os.getenv("AZURE_SEARCH_ENDPOINT")
SEARCH_KEY = os.getenv("AZURE_SEARCH_KEY")

# Common transaction code patterns in OTIS
TX_CODE_PATTERNS = [
    # Direct value assignments
    r"(?:WS-|LS-)?TRAN(?:S)?(?:-)?(?:CODE|TYPE|ID)\s+(?:PIC|PICTURE)\s+[X9A]+(?:\([0-9]+\))?\s+VALUE\s+['\"]([A-Z0-9]{2})['\"]",
    # IF statements checking transaction codes
    r"IF\s+(?:WS-|LS-)?TRAN(?:S)?(?:-)?(?:CODE|TYPE|ID)\s*=\s*['\"]([A-Z0-9]{2})['\"]",
    # EVALUATE statements
    r"WHEN\s+['\"]([A-Z0-9]{2})['\"]",
    # Comments describing transaction codes
    r"\*.*(?:TRANS(?:ACTION)?|TX|TRAN)\s+(?:CODE|TYPE)[\s:]+([A-Z0-9]{2})\b",
]

# Menu transaction code mappings (from GTFORM and menu programs)
KNOWN_MENU_CODES = {
    "WI": {"menu": "WIMENU", "description": "Work Item Menu"},
    "SP": {"menu": "SPMENU", "description": "Special Processing Menu"},
    "PG": {"menu": "PGMENU", "description": "Program Menu"},
    "BP": {"menu": "BPMENU", "description": "Batch Processing Menu"},
    "SC": {"menu": "SCMENU", "description": "Screen Menu"},
    "OP": {"menu": "OPMENU", "description": "Operations Menu"},
    "TW": {"menu": "TWMENU", "description": "Two-Way Menu"},
    "AS": {"menu": "ASMENU", "description": "Administration Menu"},
    "UP": {"menu": "UPMENU", "description": "Update Menu"},
    "FD": {"menu": "FDMENU", "description": "Fund Menu"},
    "GL": {"menu": "GLMENU", "description": "General Ledger Menu"},
    "EO": {"menu": "EOMENU", "description": "End of Month Menu"},
    "CL": {"menu": "CLMENU", "description": "Client Menu"},
    "CO": {"menu": "COMENU", "description": "Company Menu"},
    "FX": {"menu": "FXMENU", "description": "Fix Menu"},
    "IR": {"menu": "IRMENU", "description": "IR Menu"},
    "SE": {"menu": "SEMENU", "description": "SE Menu"},
    "LP": {"menu": "LPMENU", "description": "Loan Processing Menu"},
}


class TransactionExtractor:
    def __init__(self, cobol_src_dir: str = "cobol_src"):
        self.cobol_src_dir = Path(cobol_src_dir)
        self.transactions: Dict[str, Dict] = {}
        self.program_transactions: Dict[str, List[str]] = defaultdict(list)
        
    def extract_transaction_codes_from_file(self, filepath: Path) -> Set[str]:
        """Extract transaction codes from a single COBOL file."""
        codes = set()
        
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                
                # Try each pattern
                for pattern in TX_CODE_PATTERNS:
                    matches = re.finditer(pattern, content, re.IGNORECASE | re.MULTILINE)
                    for match in matches:
                        code = match.group(1).upper()
                        # Filter valid 2-char codes
                        if len(code) == 2 and code.isalnum():
                            codes.add(code)
                            
        except Exception as e:
            print(f"Error reading {filepath}: {e}")
            
        return codes
    
    def analyze_program_for_transaction(self, filepath: Path, tx_code: str) -> Dict:
        """Analyze a program file to extract transaction workflow details."""
        program_name = filepath.stem
        details = {
            "program": program_name,
            "calls": [],
            "screens": [],
            "file_operations": [],
            "description_hints": []
        }
        
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
                
                # Look for CALL statements
                call_pattern = r"CALL\s+['\"]([A-Z0-9]+)['\"]"
                for match in re.finditer(call_pattern, content, re.IGNORECASE):
                    called_prog = match.group(1).upper()
                    if called_prog not in details["calls"]:
                        details["calls"].append(called_prog)
                
                # Look for screen names (MAP names in CICS)
                screen_pattern = r"(?:MAP|MAPSET)\s*\(\s*['\"]([A-Z0-9]+)['\"]"
                for match in re.finditer(screen_pattern, content, re.IGNORECASE):
                    screen = match.group(1).upper()
                    if screen not in details["screens"]:
                        details["screens"].append(screen)
                
                # Look for file operations
                file_pattern = r"(?:READ|WRITE|REWRITE|DELETE)\s+([A-Z0-9\-]+)"
                for match in re.finditer(file_pattern, content, re.IGNORECASE):
                    file_name = match.group(1).upper()
                    if not file_name.startswith("WS-") and not file_name.startswith("LS-"):
                        if file_name not in details["file_operations"]:
                            details["file_operations"].append(file_name)
                
                # Extract description hints from comments near transaction code
                for i, line in enumerate(lines):
                    if tx_code in line:
                        # Look at surrounding comments (within 5 lines)
                        for offset in range(-5, 6):
                            idx = i + offset
                            if 0 <= idx < len(lines):
                                comment_line = lines[idx].strip()
                                if comment_line.startswith('*') and len(comment_line) > 10:
                                    # Clean up comment
                                    hint = comment_line[1:].strip()
                                    if hint and len(hint) > 15:  # Meaningful comments
                                        details["description_hints"].append(hint)
                                        
        except Exception as e:
            print(f"Error analyzing {filepath}: {e}")
            
        return details
    
    def scan_all_programs(self):
        """Scan all COBOL programs for transaction codes."""
        print("Scanning COBOL programs for transaction codes...")
        
        all_cobol_files = list(self.cobol_src_dir.rglob("*.CBL"))
        print(f"Found {len(all_cobol_files)} COBOL files")
        
        for filepath in all_cobol_files:
            codes = self.extract_transaction_codes_from_file(filepath)
            program_name = filepath.stem
            
            for code in codes:
                self.program_transactions[program_name].append(code)
                
                if code not in self.transactions:
                    self.transactions[code] = {
                        "tx_code": code,
                        "programs": [],
                        "entry_menu": KNOWN_MENU_CODES.get(code, {}).get("menu"),
                        "menu_description": KNOWN_MENU_CODES.get(code, {}).get("description"),
                        "workflow_programs": [],
                        "screens": [],
                        "files": [],
                        "description": ""
                    }
                
                if program_name not in self.transactions[code]["programs"]:
                    self.transactions[code]["programs"].append(program_name)
        
        print(f"Found {len(self.transactions)} unique transaction codes")
        print(f"Transaction codes in {len(self.program_transactions)} programs")
        
    def enrich_transactions_with_workflow(self):
        """Enrich transaction data with workflow details from programs."""
        print("\nEnriching transactions with workflow details...")
        
        for tx_code, tx_data in self.transactions.items():
            all_calls = []
            all_screens = []
            all_files = []
            all_descriptions = []
            
            # Analyze each program associated with this transaction
            for program_name in tx_data["programs"][:5]:  # Limit to first 5 to avoid explosion
                # Find the program file
                matching_files = list(self.cobol_src_dir.rglob(f"{program_name}.CBL"))
                if matching_files:
                    details = self.analyze_program_for_transaction(matching_files[0], tx_code)
                    all_calls.extend(details["calls"])
                    all_screens.extend(details["screens"])
                    all_files.extend(details["file_operations"])
                    all_descriptions.extend(details["description_hints"])
            
            # Deduplicate and store
            tx_data["workflow_programs"] = list(set(all_calls))
            tx_data["screens"] = list(set(all_screens))
            tx_data["files"] = list(set(all_files))
            
            # Generate description from hints
            if all_descriptions:
                # Take the longest, most descriptive comment
                tx_data["description"] = max(all_descriptions, key=len)[:500]
            
            print(f"  {tx_code}: {len(tx_data['programs'])} programs, "
                  f"{len(tx_data['workflow_programs'])} calls, "
                  f"{len(tx_data['screens'])} screens, "
                  f"{len(tx_data['files'])} files")
    
    def generate_natural_language_descriptions(self):
        """Generate natural language descriptions for each transaction."""
        print("\nGenerating natural language descriptions...")
        
        for tx_code, tx_data in self.transactions.items():
            parts = []
            
            # Start with transaction code
            parts.append(f"Transaction code '{tx_code}'")
            
            # Add menu context if known
            if tx_data.get("menu_description"):
                parts.append(f"accessed via {tx_data['menu_description']}")
            
            # Add program context
            if tx_data["programs"]:
                prog_list = ", ".join(tx_data["programs"][:3])
                if len(tx_data["programs"]) > 3:
                    prog_list += f" and {len(tx_data['programs']) - 3} more"
                parts.append(f"is implemented in programs: {prog_list}")
            
            # Add workflow context
            if tx_data["workflow_programs"]:
                workflow_list = ", ".join(tx_data["workflow_programs"][:5])
                if len(tx_data["workflow_programs"]) > 5:
                    workflow_list += f" and {len(tx_data['workflow_programs']) - 5} more"
                parts.append(f"The workflow involves calling: {workflow_list}")
            
            # Add screen context
            if tx_data["screens"]:
                screen_list = ", ".join(tx_data["screens"][:3])
                parts.append(f"Uses screens: {screen_list}")
            
            # Add file context
            if tx_data["files"]:
                file_list = ", ".join(tx_data["files"][:3])
                if len(tx_data["files"]) > 3:
                    file_list += f" and {len(tx_data['files']) - 3} more files"
                parts.append(f"Accesses data files: {file_list}")
            
            # Add extracted description if available
            if tx_data.get("description"):
                parts.append(f"Purpose: {tx_data['description']}")
            
            tx_data["narrative"] = ". ".join(parts) + "."
    
    def save_taxonomy(self, output_file: str = "transaction_taxonomy.json"):
        """Save the transaction taxonomy to a JSON file."""
        output_path = Path(output_file)
        
        # Convert to list for easier indexing
        taxonomy_list = []
        for tx_code, tx_data in sorted(self.transactions.items()):
            doc = {
                "id": f"TX_{tx_code}",
                "tx_code": tx_code,
                "entry_menu": tx_data.get("entry_menu"),
                "menu_description": tx_data.get("menu_description"),
                "programs": tx_data["programs"],
                "workflow_programs": tx_data["workflow_programs"],
                "screens": tx_data["screens"],
                "files": tx_data["files"],
                "description": tx_data.get("description", ""),
                "narrative": tx_data.get("narrative", ""),
                "program_count": len(tx_data["programs"]),
                "workflow_depth": len(tx_data["workflow_programs"]),
            }
            taxonomy_list.append(doc)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(taxonomy_list, f, indent=2, ensure_ascii=False)
        
        print(f"\nSaved {len(taxonomy_list)} transaction definitions to {output_path}")
        return taxonomy_list
    
    def generate_statistics(self):
        """Generate statistics about the transaction taxonomy."""
        print("\n" + "=" * 80)
        print("TRANSACTION TAXONOMY STATISTICS")
        print("=" * 80)
        
        print(f"\nTotal unique transaction codes: {len(self.transactions)}")
        print(f"Total programs with transaction codes: {len(self.program_transactions)}")
        
        # Transaction codes by menu
        menu_counts = defaultdict(int)
        for tx_data in self.transactions.values():
            menu = tx_data.get("entry_menu", "Unknown")
            menu_counts[menu] += 1
        
        print("\nTransaction codes by menu:")
        for menu, count in sorted(menu_counts.items(), key=lambda x: -x[1]):
            print(f"  {menu}: {count} codes")
        
        # Top transaction codes by program count
        print("\nTop 10 transaction codes by number of programs:")
        sorted_txs = sorted(self.transactions.items(), 
                          key=lambda x: len(x[1]["programs"]), 
                          reverse=True)
        for tx_code, tx_data in sorted_txs[:10]:
            print(f"  {tx_code}: {len(tx_data['programs'])} programs, "
                  f"{len(tx_data['workflow_programs'])} workflow calls")
        
        # Sample narratives
        print("\nSample transaction narratives:")
        for tx_code, tx_data in list(sorted_txs[:3]):
            print(f"\n  {tx_code}:")
            print(f"    {tx_data.get('narrative', 'No narrative generated')[:300]}...")


def main():
    print("=" * 80)
    print("TRANSACTION TYPE TAXONOMY EXTRACTOR")
    print("=" * 80)
    
    extractor = TransactionExtractor()
    
    # Step 1: Scan all programs
    extractor.scan_all_programs()
    
    # Step 2: Enrich with workflow details
    extractor.enrich_transactions_with_workflow()
    
    # Step 3: Generate natural language descriptions
    extractor.generate_natural_language_descriptions()
    
    # Step 4: Generate statistics
    extractor.generate_statistics()
    
    # Step 5: Save taxonomy
    taxonomy = extractor.save_taxonomy()
    
    print("\n" + "=" * 80)
    print("NEXT STEPS")
    print("=" * 80)
    print("""
1. Review transaction_taxonomy.json to verify extracted data
2. Run build_transaction_taxonomy_index.py to create Azure Search index
3. Run embed_transaction_taxonomy.py to add embeddings
4. Test with questions like:
   - "What happens when user enters transaction code RP?"
   - "Show me the workflow for transaction PL"
   - "Which programs are involved in transaction WI?"
    """)


if __name__ == "__main__":
    main()
