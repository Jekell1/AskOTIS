# COBOL Copybook Analyzer

A comprehensive C# console application for analyzing COBOL files, identifying referenced sections, and constructing copybooks containing only the sections that are actually used in the code.

## Features

### Core Analysis
- **Section Identification**: Automatically identifies all sections, paragraphs, and data items in COBOL files
- **Reference Tracking**: Finds all `PERFORM` and `GO TO` statements to track section usage
- **Copy Statement Detection**: Identifies all `COPY` statements and embedded copybooks
- **Smart Filtering**: Distinguishes between referenced and unreferenced code sections

### Advanced Features
- **Inlined Copybook Support**: Handles COBOL files with embedded/inlined copybooks
- **Copybook Extraction**: Extracts individual copybook files from inlined COBOL files
- **File Comparison**: Compares original and inlined COBOL files to identify structural differences
- **Syntax Validation**: Basic COBOL syntax validation for common issues
- **Comprehensive Reporting**: Generates detailed analysis reports and statistics

## Usage

### Command Line Interface

```bash
# Analyze a COBOL file
CobolCopybookAnalyzer analyze <cobol-file> [--output-dir <dir>]

# Compare original and inlined files
CobolCopybookAnalyzer compare <original-file> <inlined-file> [--output-dir <dir>]

# Extract copybooks from inlined file
CobolCopybookAnalyzer extract <inlined-file> [--output-dir <dir>]

# Validate COBOL syntax
CobolCopybookAnalyzer validate <cobol-file>
```

### Batch File Runner

For convenience, use the provided batch file:

```cmd
# Analyze APIPAY.CBL
analyze.bat analyze APIPAY.CBL

# Compare original and inlined versions
analyze.bat compare APIPAY.CBL APIPAY_Inlined.CBL

# Extract copybooks from inlined file
analyze.bat extract APIPAY_Inlined.CBL copybooks

# Validate syntax
analyze.bat validate APIPAY.CBL
```

## Examples

### Example 1: Basic Analysis
```cmd
analyze.bat analyze c:\cobol\APIPAY.CBL
```

**Output Files:**
- `APIPAY_ReferencedSections.cpy` - Copybook with only referenced sections
- `APIPAY_Analysis.txt` - Detailed analysis report
- `APIPAY_Stats.txt` - Summary statistics

### Example 2: Inlined File Processing
```cmd
analyze.bat extract APIPAY_Inlined.CBL extracted_copybooks
```

**Output:**
- Individual `.cpy` files for each embedded copybook
- Analysis of section references across all embedded content

### Example 3: File Comparison
```cmd
analyze.bat compare APIPAY.CBL APIPAY_Inlined.CBL reports
```

**Output:**
- `COBOL_Comparison_Report.txt` - Structural differences report

## Output Files

### Referenced Sections Copybook (`.cpy`)
Contains only the sections that are actually referenced in the code:
```cobol
      * GENERATED COPYBOOK - REFERENCED SECTIONS ONLY
      * GENERATED ON: 2025-06-25 10:30:15
      * SOURCE ANALYSIS:
      *   MAIN-PROGRAM (SECTION) - Referenced 1 times
      *   CREATE-LOG (SECTION) - Referenced 15 times
      *   PAYMENT-POSTING (SECTION) - Referenced 3 times
      
      * SECTION: MAIN-PROGRAM (SECTION)
      * REFERENCED FROM LINES: 340
      *----------------------------------------------------------------------
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
           ...
```

### Analysis Report (`.txt`)
Comprehensive analysis including:
- Summary statistics
- All COPY statements found
- Referenced sections with line numbers
- Unreferenced sections
- Reference frequency analysis

### Summary Statistics (`.txt`)
High-level metrics:
- Total sections count
- Section type breakdown
- Reference statistics
- Copy statement analysis

## Architecture

### Core Classes

#### `CobolSection`
Represents a COBOL section, paragraph, or data item:
```csharp
public class CobolSection
{
    public string Name { get; set; }
    public string Type { get; set; }           // SECTION, PARAGRAPH, DATA-01, etc.
    public int StartLine { get; set; }
    public int EndLine { get; set; }
    public List<string> Content { get; set; }
    public bool IsReferenced { get; set; }
    public List<int> ReferencedFromLines { get; set; }
}
```

#### `CobolAnalyzer`
Main analysis engine:
- Pattern matching for COBOL constructs
- Reference tracking via PERFORM/GO TO statements
- Section content extraction
- Report generation

#### `EnhancedCobolAnalyzer`
Extended analyzer with:
- Embedded copybook extraction
- Inlined file processing
- Advanced comparison features

### Regular Expression Patterns

The analyzer uses sophisticated regex patterns to identify:
- Sections: `^\s*([A-Z0-9-]+)\s+SECTION\s*\.`
- Paragraphs: `^\s*([A-Z0-9-]+)\s*\.`
- PERFORM statements: `PERFORM\s+([A-Z0-9-]+)`
- COPY statements: `COPY\s+["']([^"']+)["']`
- Data items: `^\s*(\d{2})\s+([A-Z0-9-]+)`

## Building and Running

### Requirements
- .NET 6.0 or later
- Windows (batch files) or cross-platform (direct dotnet commands)

### Build
```cmd
dotnet build --configuration Release
```

### Run
```cmd
dotnet run --project CobolCopybookAnalyzer.csproj -- analyze APIPAY.CBL
```

## Use Cases

### Legacy System Modernization
- Identify dead code sections for removal
- Extract only necessary copybooks for migration
- Understand code dependencies and references

### Code Quality Analysis
- Find unreferenced sections that can be removed
- Validate COBOL syntax for common issues
- Generate documentation of code structure

### Copybook Management
- Extract copybooks from monolithic inlined files
- Create focused copybooks with only used sections
- Compare different versions of COBOL files

### Documentation Generation
- Create comprehensive analysis reports
- Generate section reference maps
- Produce statistics for project metrics

## Sample Output

```
COBOL Copybook Analyzer
=======================

Analyzing COBOL file: APIPAY.CBL
Found 156 sections/paragraphs
Found 89 COPY statements
Found 23 referenced sections

ANALYSIS COMPLETE:
  Total sections found: 156
  Referenced sections: 23
  Unreferenced sections: 133
  COPY statements: 89

OUTPUT FILES:
  Copybook: APIPAY_ReferencedSections.cpy
  Report: APIPAY_Analysis.txt

TOP REFERENCED SECTIONS:
  CREATE-LOG - 15 references
  READ-BR-FILE - 8 references
  PAYMENT-POSTING - 3 references
  END-ROUTINE - 2 references
  MAIN-PROGRAM - 1 references
```

## Configuration

The analyzer can be extended with custom patterns and rules by modifying the regex patterns in the `CobolAnalyzer` class. Key configuration points:

- **Section Patterns**: Modify `_sectionPattern` for custom section identification
- **Reference Patterns**: Add new patterns to `FindReferences()` method
- **Output Formats**: Customize report generation in the various `Generate*()` methods

## Error Handling

The application includes comprehensive error handling for:
- Missing input files
- Invalid COBOL syntax
- File system permissions
- Large file processing

Common error scenarios are logged with helpful messages to assist in troubleshooting.
