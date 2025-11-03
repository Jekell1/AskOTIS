"""Ground truth analysis: Trace RP execution flow to find actual copybooks used.

Steps:
1. Parse all COPY statements -> get all 288 copybooks with their data items
2. Find RP entry conditions (IF LP-TRCD = "RP")
3. Trace execution through PERFORMed paragraphs
4. Track which data items are referenced
5. Map data items back to defining copybooks
6. Return unique copybooks in RP flow
"""

import re
from pathlib import Path
from collections import defaultdict

class COBOLFlowAnalyzer:
    def __init__(self, source_file):
        self.source_file = source_file
        self.lines = source_file.read_text(encoding='utf-8', errors='ignore').splitlines()
        
        # Data structures
        self.copybooks = {}  # copybook_name -> {line_num, library_path}
        self.copybook_data_items = defaultdict(set)  # copybook -> set of data item names
        self.paragraphs = {}  # paragraph_name -> (start_line, end_line)
        self.paragraph_performs = defaultdict(set)  # paragraph -> set of paragraphs it PERFORMs
        self.paragraph_data_refs = defaultdict(set)  # paragraph -> set of data items referenced
        
    def parse_copy_statements(self):
        """Parse all COPY statements."""
        copy_pattern = re.compile(r'^\s*COPY\s+"([^"]+)"', re.IGNORECASE)
        
        for i, line in enumerate(self.lines, 1):
            match = copy_pattern.match(line)
            if match:
                full_path = match.group(1)
                # Extract library and filename
                if '/' in full_path:
                    library, filename = full_path.rsplit('/', 1)
                else:
                    library = ''
                    filename = full_path
                
                if not filename.endswith('.CPY'):
                    filename += '.CPY'
                
                self.copybooks[filename] = {
                    'line': i,
                    'library': library,
                    'full_path': full_path
                }
        
        print(f"✅ Parsed {len(self.copybooks)} COPY statements")
        return self.copybooks
    
    def parse_paragraphs(self):
        """Parse all paragraph labels and their line ranges."""
        para_pattern = re.compile(r'^([A-Z0-9][A-Z0-9-]{0,62})\.\s*$', re.IGNORECASE)
        
        current_para = None
        para_start = 0
        
        for i, line in enumerate(self.lines):
            stripped = line.strip()
            match = para_pattern.match(stripped)
            
            if match:
                # Save previous paragraph
                if current_para:
                    self.paragraphs[current_para] = (para_start, i)
                
                # Start new paragraph
                current_para = match.group(1).upper()
                para_start = i
        
        # Save last paragraph
        if current_para:
            self.paragraphs[current_para] = (para_start, len(self.lines))
        
        print(f"✅ Parsed {len(self.paragraphs)} paragraphs")
        return self.paragraphs
    
    def find_rp_entry_points(self):
        """Find all lines where RP logic is triggered."""
        rp_entries = []
        
        # Pattern: IF LP-TRCD = "RP"
        rp_pattern = re.compile(r'IF\s+.*LP-TRCD.*=.*["\']RP["\']', re.IGNORECASE)
        
        for i, line in enumerate(self.lines, 1):
            if rp_pattern.search(line):
                # Find which paragraph this is in
                para_name = self._find_paragraph_for_line(i)
                rp_entries.append({
                    'line': i,
                    'text': line.strip(),
                    'paragraph': para_name
                })
        
        print(f"✅ Found {len(rp_entries)} RP entry points")
        for entry in rp_entries[:5]:
            print(f"   Line {entry['line']:5d} in {entry['paragraph']}: {entry['text'][:60]}")
        
        return rp_entries
    
    def _find_paragraph_for_line(self, line_num):
        """Find which paragraph contains the given line number."""
        for para_name, (start, end) in self.paragraphs.items():
            if start <= line_num < end:
                return para_name
        return None
    
    def build_perform_graph(self):
        """Build graph of which paragraphs PERFORM which other paragraphs."""
        perform_pattern = re.compile(r'PERFORM\s+([A-Z0-9][A-Z0-9-]{0,62})', re.IGNORECASE)
        
        for para_name, (start, end) in self.paragraphs.items():
            para_lines = self.lines[start:end]
            
            for line in para_lines:
                for match in perform_pattern.finditer(line):
                    target_para = match.group(1).upper()
                    if target_para in self.paragraphs:
                        self.paragraph_performs[para_name].add(target_para)
        
        total_edges = sum(len(targets) for targets in self.paragraph_performs.values())
        print(f"✅ Built PERFORM graph: {total_edges} edges")
        return self.paragraph_performs
    
    def trace_execution_from_paragraphs(self, start_paragraphs, max_depth=10):
        """Trace execution flow from starting paragraphs using BFS."""
        visited = set()
        queue = [(p, 0) for p in start_paragraphs]
        execution_trace = []
        
        while queue:
            para, depth = queue.pop(0)
            
            if para in visited or depth > max_depth:
                continue
            
            visited.add(para)
            execution_trace.append((para, depth))
            
            # Add paragraphs this one PERFORMs
            for target in self.paragraph_performs.get(para, []):
                if target not in visited:
                    queue.append((target, depth + 1))
        
        print(f"✅ Traced execution: {len(execution_trace)} paragraphs reachable")
        return execution_trace
    
    def extract_data_references(self, paragraph_name):
        """Extract data item references from a paragraph (heuristic)."""
        if paragraph_name not in self.paragraphs:
            return set()
        
        start, end = self.paragraphs[paragraph_name]
        para_lines = self.lines[start:end]
        
        data_refs = set()
        
        # Simple heuristic: find COBOL data names (words with hyphens in statements)
        # This is NOT perfect but gives us a rough idea
        data_name_pattern = re.compile(r'\b([A-Z][A-Z0-9-]{1,62})\b')
        
        for line in para_lines:
            # Skip comments
            if line.strip().startswith('*'):
                continue
            
            # Find potential data names
            for match in data_name_pattern.finditer(line):
                name = match.group(1)
                # Filter out COBOL keywords and paragraph names
                if name not in self.paragraphs and not self._is_cobol_keyword(name):
                    data_refs.add(name)
        
        return data_refs
    
    def _is_cobol_keyword(self, word):
        """Check if word is a COBOL keyword."""
        keywords = {
            'PERFORM', 'IF', 'THEN', 'ELSE', 'END-IF', 'MOVE', 'TO', 'FROM',
            'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
            'READ', 'WRITE', 'REWRITE', 'DELETE', 'START', 'OPEN', 'CLOSE',
            'DISPLAY', 'ACCEPT', 'STOP', 'EXIT', 'GO', 'GOTO',
            'EVALUATE', 'WHEN', 'END-EVALUATE', 'CONTINUE',
            'AND', 'OR', 'NOT', 'EQUAL', 'GREATER', 'LESS',
            'FILE', 'SECTION', 'DIVISION'
        }
        return word.upper() in keywords
    
    def map_data_to_copybooks(self):
        """Map data items to their defining copybooks (requires parsing copybook contents)."""
        # This requires reading each copybook file to see what data items it defines
        # For now, we'll use a heuristic based on naming conventions
        
        print("\n⚠️  Note: Full data item mapping requires parsing copybook source files")
        print("   Using naming convention heuristics instead...")
        
        # Heuristic: Data items starting with copybook prefix likely come from that copybook
        # Example: LP-TRCD likely comes from LP*.CPY copybooks
        
        return {}  # Would need copybook file parsing to implement fully

def main():
    print("="*80)
    print("GROUND TRUTH: RP COPYBOOK USAGE ANALYSIS")
    print("="*80)
    
    # Find LONPF2
    cobol_src = Path("cobol_src")
    lonpf2_file = None
    
    for f in cobol_src.rglob("LONPF2.CBL"):
        lonpf2_file = f
        break
    
    if not lonpf2_file:
        print("❌ LONPF2.CBL not found")
        return
    
    print(f"\n📄 Analyzing: {lonpf2_file}\n")
    
    # Initialize analyzer
    analyzer = COBOLFlowAnalyzer(lonpf2_file)
    
    # Step 1: Parse COPY statements
    print("STEP 1: Parse COPY statements")
    print("-" * 80)
    copybooks = analyzer.parse_copy_statements()
    
    # Step 2: Parse paragraphs
    print(f"\nSTEP 2: Parse paragraphs")
    print("-" * 80)
    paragraphs = analyzer.parse_paragraphs()
    
    # Step 3: Find RP entry points
    print(f"\nSTEP 3: Find RP entry conditions")
    print("-" * 80)
    rp_entries = analyzer.find_rp_entry_points()
    
    # Step 4: Build PERFORM graph
    print(f"\nSTEP 4: Build PERFORM graph")
    print("-" * 80)
    perform_graph = analyzer.build_perform_graph()
    
    # Step 5: Trace execution from RP entry paragraphs
    print(f"\nSTEP 5: Trace execution from RP entry points")
    print("-" * 80)
    rp_paragraphs = set(entry['paragraph'] for entry in rp_entries if entry['paragraph'])
    print(f"Starting from {len(rp_paragraphs)} RP entry paragraphs:")
    for para in sorted(rp_paragraphs):
        print(f"   - {para}")
    
    execution_trace = analyzer.trace_execution_from_paragraphs(rp_paragraphs, max_depth=10)
    print(f"\nExecution trace covers {len(execution_trace)} paragraphs")
    
    # Step 6: Extract data references from traced paragraphs
    print(f"\nSTEP 6: Extract data references from RP execution flow")
    print("-" * 80)
    all_data_refs = set()
    
    for para, depth in execution_trace[:20]:  # Sample first 20 for display
        refs = analyzer.extract_data_references(para)
        all_data_refs.update(refs)
    
    # Get all data refs (not just sample)
    for para, depth in execution_trace:
        refs = analyzer.extract_data_references(para)
        all_data_refs.update(refs)
    
    print(f"Found {len(all_data_refs)} unique data item references in RP flow")
    print(f"Sample data items: {sorted(list(all_data_refs))[:20]}")
    
    # Step 7: Map to copybooks (heuristic)
    print(f"\nSTEP 7: Map data items to copybooks")
    print("-" * 80)
    print("⚠️  This step requires parsing copybook source files to see what data items")
    print("   each copybook defines. Current implementation uses naming heuristics.")
    
    # Heuristic: Look for copybook prefixes in data item names
    copybook_usage_estimate = set()
    
    for data_item in all_data_refs:
        # Check if data item name matches copybook name pattern
        for copybook_name in copybooks.keys():
            # Extract prefix from copybook (e.g., "LP" from "LPFSWK.CPY")
            prefix = copybook_name.split('.')[0][:2]
            if data_item.startswith(prefix):
                copybook_usage_estimate.add(copybook_name)
                break
    
    print(f"\nEstimated copybooks used in RP flow (based on naming): {len(copybook_usage_estimate)}")
    
    # Results
    print(f"\n{'='*80}")
    print("RESULTS")
    print(f"{'='*80}")
    print(f"Total COPY statements in LONPF2:     {len(copybooks)}")
    print(f"RP entry points found:                {len(rp_entries)}")
    print(f"Paragraphs in RP execution flow:      {len(execution_trace)}")
    print(f"Data items referenced in RP flow:     {len(all_data_refs)}")
    print(f"Copybooks estimated (by naming):      {len(copybook_usage_estimate)}")
    print(f"\nRAG system answer:                    178 copybooks")
    print(f"Previous expectation:                 28 copybooks")
    
    print(f"\n{'='*80}")
    print("LIMITATIONS & NEXT STEPS")
    print(f"{'='*80}")
    print("""
This analysis provides the framework but has limitations:

1. ✅ COPY statements parsed correctly (288 total)
2. ✅ RP entry points identified (IF LP-TRCD = "RP")
3. ✅ Execution flow traced through PERFORM graph
4. ⚠️  Data item references extracted (heuristic, not complete)
5. ❌ Data item -> copybook mapping requires parsing copybook files

To get the TRUE ground truth:
- Parse each copybook file to extract its data item definitions
- Build complete data dictionary: data_item -> copybook
- Cross-reference data items used in RP flow with dictionary
- Return exact set of copybooks

Alternative: Use existing index data (data_items index) if available.
""")
    
    # Show sample of RP execution trace
    print(f"\nSample RP execution trace (first 15 paragraphs):")
    for para, depth in execution_trace[:15]:
        indent = "  " * depth
        print(f"{indent}{para} (depth {depth})")

if __name__ == "__main__":
    main()
