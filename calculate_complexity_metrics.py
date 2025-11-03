"""
Calculate Complexity Metrics for OTIS COBOL Programs

This script calculates various complexity metrics:
- Lines of Code (LOC)
- Cyclomatic Complexity (decision points)
- Number of paragraphs
- Number of CALL statements
- Number of PERFORM statements
- File operation count
- Maximum nesting depth
- Halstead complexity (if feasible)

Enhances the existing program_meta index with these metrics.
"""

import os
import re
from pathlib import Path
from typing import Dict, List, Tuple
import json
from collections import defaultdict
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

# Azure Search connection
SEARCH_ENDPOINT = os.getenv("AZURE_SEARCH_ENDPOINT")
SEARCH_KEY = os.getenv("AZURE_SEARCH_KEY")
INDEX_NAME = "new_cobol_programs"


class ComplexityAnalyzer:
    def __init__(self, cobol_src_dir: str = "cobol_src"):
        self.cobol_src_dir = Path(cobol_src_dir)
        self.metrics = {}
        
    def count_lines_of_code(self, content: str) -> Dict[str, int]:
        """Count various types of lines."""
        lines = content.split('\n')
        
        total_lines = len(lines)
        blank_lines = sum(1 for line in lines if not line.strip())
        comment_lines = sum(1 for line in lines if line.strip().startswith('*'))
        code_lines = total_lines - blank_lines - comment_lines
        
        return {
            "total_lines": total_lines,
            "code_lines": code_lines,
            "comment_lines": comment_lines,
            "blank_lines": blank_lines
        }
    
    def calculate_cyclomatic_complexity(self, content: str) -> int:
        """
        Calculate cyclomatic complexity based on decision points.
        Decision points: IF, EVALUATE, PERFORM UNTIL, PERFORM VARYING, etc.
        """
        decision_keywords = [
            r'\bIF\b',
            r'\bEVALUATE\b',
            r'\bWHEN\b',
            r'\bPERFORM\s+UNTIL\b',
            r'\bPERFORM\s+VARYING\b',
            r'\bSEARCH\b',
        ]
        
        complexity = 1  # Base complexity
        
        for pattern in decision_keywords:
            matches = re.findall(pattern, content, re.IGNORECASE)
            complexity += len(matches)
        
        # Each WHEN in EVALUATE adds a path
        when_matches = re.findall(r'\bWHEN\b', content, re.IGNORECASE)
        # Subtract base WHEN count since EVALUATE already counted
        evaluate_count = len(re.findall(r'\bEVALUATE\b', content, re.IGNORECASE))
        
        return complexity
    
    def count_paragraphs(self, content: str) -> int:
        """Count number of paragraphs in PROCEDURE DIVISION."""
        # Paragraph names typically start in column 8 and end with period
        # Pattern: starts at beginning of line, alphanumeric with hyphens, ends with period
        paragraph_pattern = r'^[A-Z][A-Z0-9\-]*\.$'
        
        lines = content.split('\n')
        paragraph_count = 0
        in_procedure_division = False
        
        for line in lines:
            if 'PROCEDURE DIVISION' in line.upper():
                in_procedure_division = True
                continue
            
            if in_procedure_division:
                # Check if line looks like a paragraph name
                stripped = line.strip()
                if stripped and re.match(paragraph_pattern, stripped):
                    paragraph_count += 1
        
        return paragraph_count
    
    def count_calls(self, content: str) -> int:
        """Count number of CALL statements."""
        call_pattern = r'\bCALL\s+["\'][A-Z0-9]+["\']'
        return len(re.findall(call_pattern, content, re.IGNORECASE))
    
    def count_performs(self, content: str) -> int:
        """Count number of PERFORM statements."""
        perform_pattern = r'\bPERFORM\s+[A-Z][A-Z0-9\-]*'
        return len(re.findall(perform_pattern, content, re.IGNORECASE))
    
    def count_file_operations(self, content: str) -> Dict[str, int]:
        """Count file I/O operations."""
        operations = {
            "read": len(re.findall(r'\bREAD\b', content, re.IGNORECASE)),
            "write": len(re.findall(r'\bWRITE\b', content, re.IGNORECASE)),
            "rewrite": len(re.findall(r'\bREWRITE\b', content, re.IGNORECASE)),
            "delete": len(re.findall(r'\bDELETE\b', content, re.IGNORECASE)),
        }
        operations["total"] = sum(operations.values())
        return operations
    
    def estimate_max_nesting_depth(self, content: str) -> int:
        """Estimate maximum nesting depth of control structures."""
        lines = content.split('\n')
        max_depth = 0
        current_depth = 0
        
        # Track nesting by IF/EVALUATE/PERFORM
        for line in lines:
            upper_line = line.upper().strip()
            
            # Increase depth
            if (upper_line.startswith('IF ') or 
                upper_line.startswith('EVALUATE ') or
                'PERFORM UNTIL' in upper_line or
                'PERFORM VARYING' in upper_line):
                current_depth += 1
                max_depth = max(max_depth, current_depth)
            
            # Decrease depth
            elif upper_line.startswith('END-IF') or upper_line.startswith('END-EVALUATE'):
                current_depth = max(0, current_depth - 1)
        
        return max_depth
    
    def categorize_complexity(self, complexity: int, loc: int) -> str:
        """Categorize program complexity."""
        # Based on cyclomatic complexity and LOC
        if complexity <= 10 and loc <= 200:
            return "simple"
        elif complexity <= 20 and loc <= 500:
            return "moderate"
        elif complexity <= 50 and loc <= 1000:
            return "medium"
        elif complexity <= 100 and loc <= 2000:
            return "complex"
        else:
            return "very_complex"
    
    def analyze_program(self, filepath: Path) -> Dict:
        """Analyze a single program file for all metrics."""
        program_name = filepath.stem
        
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            loc_metrics = self.count_lines_of_code(content)
            cyclomatic = self.calculate_cyclomatic_complexity(content)
            paragraphs = self.count_paragraphs(content)
            calls = self.count_calls(content)
            performs = self.count_performs(content)
            file_ops = self.count_file_operations(content)
            max_nesting = self.estimate_max_nesting_depth(content)
            
            complexity_category = self.categorize_complexity(
                cyclomatic, 
                loc_metrics["code_lines"]
            )
            
            metrics = {
                "program": program_name,
                "file_path": str(filepath.relative_to(self.cobol_src_dir.parent)),
                "total_lines": loc_metrics["total_lines"],
                "code_lines": loc_metrics["code_lines"],
                "comment_lines": loc_metrics["comment_lines"],
                "blank_lines": loc_metrics["blank_lines"],
                "cyclomatic_complexity": cyclomatic,
                "paragraph_count": paragraphs,
                "call_count": calls,
                "perform_count": performs,
                "file_read_count": file_ops["read"],
                "file_write_count": file_ops["write"],
                "file_total_io": file_ops["total"],
                "max_nesting_depth": max_nesting,
                "complexity_category": complexity_category,
            }
            
            # Generate natural language description
            metrics["complexity_description"] = self.generate_description(metrics)
            
            return metrics
            
        except Exception as e:
            print(f"Error analyzing {filepath}: {e}")
            return None
    
    def generate_description(self, metrics: Dict) -> str:
        """Generate natural language description of complexity."""
        parts = []
        
        prog = metrics["program"]
        loc = metrics["code_lines"]
        complexity = metrics["cyclomatic_complexity"]
        category = metrics["complexity_category"]
        
        # Overall assessment
        category_desc = {
            "simple": "a simple program",
            "moderate": "a moderately complex program",
            "medium": "a medium complexity program",
            "complex": "a complex program",
            "very_complex": "a very complex program"
        }
        parts.append(f"{prog} is {category_desc.get(category, 'a program')}")
        
        # Size
        parts.append(f"with {loc} lines of code")
        
        # Complexity
        if complexity < 10:
            parts.append("and low cyclomatic complexity")
        elif complexity < 20:
            parts.append("and moderate cyclomatic complexity")
        elif complexity < 50:
            parts.append("and high cyclomatic complexity")
        else:
            parts.append("and very high cyclomatic complexity")
        
        # Structure details
        details = []
        if metrics["paragraph_count"] > 0:
            details.append(f"{metrics['paragraph_count']} paragraphs")
        if metrics["call_count"] > 0:
            details.append(f"{metrics['call_count']} external calls")
        if metrics["perform_count"] > 10:
            details.append(f"{metrics['perform_count']} PERFORM statements")
        if metrics["file_total_io"] > 0:
            details.append(f"{metrics['file_total_io']} file operations")
        
        if details:
            parts.append(f"It contains {', '.join(details)}")
        
        # Nesting warning
        if metrics["max_nesting_depth"] >= 4:
            parts.append(f"The code has deep nesting (max depth: {metrics['max_nesting_depth']}) which may impact maintainability")
        
        # File I/O in complexity context
        if metrics["file_total_io"] > 5 and complexity > 20:
            parts.append("The combination of high complexity and multiple file operations may impact performance")
        
        return ". ".join(parts) + "."
    
    def analyze_all_programs(self, limit: int = None):
        """Analyze all COBOL programs."""
        print("Scanning for COBOL programs...")
        all_files = list(self.cobol_src_dir.rglob("*.CBL"))
        
        if limit:
            all_files = all_files[:limit]
            print(f"Analyzing {limit} programs (limited)...")
        else:
            print(f"Analyzing {len(all_files)} programs...")
        
        results = []
        for i, filepath in enumerate(all_files, 1):
            if i % 100 == 0:
                print(f"  Processed {i}/{len(all_files)} programs...")
            
            metrics = self.analyze_program(filepath)
            if metrics:
                results.append(metrics)
                self.metrics[metrics["program"]] = metrics
        
        print(f"\nCompleted analysis of {len(results)} programs")
        return results
    
    def save_metrics(self, output_file: str = "program_complexity_metrics.json"):
        """Save metrics to JSON file."""
        output_path = Path(output_file)
        
        metrics_list = list(self.metrics.values())
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(metrics_list, f, indent=2, ensure_ascii=False)
        
        print(f"\nSaved {len(metrics_list)} program metrics to {output_path}")
        return metrics_list
    
    def generate_statistics(self):
        """Generate statistics about complexity metrics."""
        print("\n" + "=" * 80)
        print("COMPLEXITY METRICS STATISTICS")
        print("=" * 80)
        
        if not self.metrics:
            print("No metrics to analyze")
            return
        
        metrics_list = list(self.metrics.values())
        
        # Category distribution
        category_counts = defaultdict(int)
        for m in metrics_list:
            category_counts[m["complexity_category"]] += 1
        
        print("\nComplexity Distribution:")
        for category in ["simple", "moderate", "medium", "complex", "very_complex"]:
            count = category_counts.get(category, 0)
            pct = (count / len(metrics_list) * 100) if metrics_list else 0
            print(f"  {category:15s}: {count:4d} ({pct:5.1f}%)")
        
        # Average metrics
        avg_loc = sum(m["code_lines"] for m in metrics_list) / len(metrics_list)
        avg_complexity = sum(m["cyclomatic_complexity"] for m in metrics_list) / len(metrics_list)
        avg_paragraphs = sum(m["paragraph_count"] for m in metrics_list) / len(metrics_list)
        
        print(f"\nAverage Metrics:")
        print(f"  Lines of Code: {avg_loc:.0f}")
        print(f"  Cyclomatic Complexity: {avg_complexity:.1f}")
        print(f"  Paragraphs: {avg_paragraphs:.1f}")
        
        # Top 10 most complex programs
        print("\nTop 10 Most Complex Programs (by cyclomatic complexity):")
        sorted_by_complexity = sorted(metrics_list, 
                                     key=lambda x: x["cyclomatic_complexity"], 
                                     reverse=True)
        for i, m in enumerate(sorted_by_complexity[:10], 1):
            print(f"  {i:2d}. {m['program']:12s}: complexity={m['cyclomatic_complexity']:3d}, "
                  f"LOC={m['code_lines']:4d}, category={m['complexity_category']}")
        
        # Top 10 largest programs
        print("\nTop 10 Largest Programs (by lines of code):")
        sorted_by_loc = sorted(metrics_list, 
                              key=lambda x: x["code_lines"], 
                              reverse=True)
        for i, m in enumerate(sorted_by_loc[:10], 1):
            print(f"  {i:2d}. {m['program']:12s}: LOC={m['code_lines']:4d}, "
                  f"complexity={m['cyclomatic_complexity']:3d}, paragraphs={m['paragraph_count']:3d}")
        
        # Programs with deep nesting
        deep_nesting = [m for m in metrics_list if m["max_nesting_depth"] >= 5]
        if deep_nesting:
            print(f"\nPrograms with Deep Nesting (>= 5 levels): {len(deep_nesting)}")
            for m in sorted(deep_nesting, key=lambda x: x["max_nesting_depth"], reverse=True)[:5]:
                print(f"  {m['program']:12s}: max depth={m['max_nesting_depth']}, "
                      f"complexity={m['cyclomatic_complexity']}")
        
        # Sample descriptions
        print("\nSample Complexity Descriptions:")
        for m in sorted_by_complexity[:3]:
            print(f"\n  {m['program']}:")
            print(f"    {m['complexity_description']}")


def main():
    print("=" * 80)
    print("PROGRAM COMPLEXITY METRICS ANALYZER")
    print("=" * 80)
    
    analyzer = ComplexityAnalyzer()
    
    # Analyze all programs (or set limit for testing)
    import sys
    limit = None
    if len(sys.argv) > 1 and sys.argv[1] == "--test":
        limit = 100
        print("\nTEST MODE: Analyzing only 100 programs")
    
    analyzer.analyze_all_programs(limit=limit)
    
    # Generate statistics
    analyzer.generate_statistics()
    
    # Save metrics
    analyzer.save_metrics()
    
    print("\n" + "=" * 80)
    print("NEXT STEPS")
    print("=" * 80)
    print("""
1. Review program_complexity_metrics.json to verify metrics
2. Run update_program_meta_with_complexity.py to add to Azure Search
3. Test with questions like:
   - "Which programs are most complex?"
   - "Show me simple programs that do X"
   - "What are the performance risks in program Y?"
   - "Find programs with high cyclomatic complexity"
    """)


if __name__ == "__main__":
    main()
