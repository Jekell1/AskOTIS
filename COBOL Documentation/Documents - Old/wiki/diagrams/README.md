# LONPFB Program Flow Diagrams

This directory contains Mermaid diagram files for the LONPFB (Loan Payment Function - Rebate) program documentation.

## Diagram Files

### 1. LONPFB_Main_Flow.mmd
**Purpose:** Main rebate processing flow  
**Description:** Shows the complete processing logic from start to finish, including validation, account checking, balance calculations, and final posting.

**Key Components:**
- Input validation
- Account existence verification
- Status and balance checking
- Rebate calculation and adjustment
- Account balance updates
- GL entry creation
- Audit trail logging

### 2. LONPFB_Validation_Flow.mmd
**Purpose:** Input parameter validation process  
**Description:** Detailed validation flow for all input parameters before processing begins.

**Key Components:**
- Payment type verification (RP)
- Account number format validation
- Rebate amount validation
- Date validation
- Authorization checking

### 3. LONPFB_Error_Handling_Flow.mmd
**Purpose:** Error detection and handling process  
**Description:** Shows how different types of errors are categorized, logged, and handled.

**Key Components:**
- Error type classification
- Error message formatting
- Logging procedures
- Status code assignment
- Error reporting

### 4. LONPFB_Data_Flow_Architecture.mmd
**Purpose:** Data flow and system architecture  
**Description:** Illustrates how data moves through processing modules and interacts with file systems.

**Key Components:**
- Module interactions
- File system access patterns
- Data transformation flow
- Input/output relationships

## Usage

These Mermaid files can be:

1. **Viewed in VS Code** with the Mermaid Preview extension
2. **Rendered online** at [mermaid.live](https://mermaid.live/)
3. **Integrated into documentation** as diagrams
4. **Version controlled** as text files for easy maintenance

## Viewing Instructions

### In VS Code:
1. Install the "Mermaid Preview" extension
2. Open any .mmd file
3. Use Ctrl+Shift+P and search for "Mermaid: Preview"

### Online:
1. Copy the content of any .mmd file
2. Go to https://mermaid.live/
3. Paste the content to view the rendered diagram

### In Documentation:
The HTML documentation files reference these diagrams with direct links, allowing for separate maintenance while keeping documentation comprehensive.

## Maintenance

When updating program logic:
1. Update the corresponding .mmd file
2. Test the diagram syntax online or in VS Code
3. Update any references in the documentation
4. Commit changes to version control

## File Relationships

```
LONPFB Documentation/
├── wiki/
│   ├── LONPFB_Documentation.html (references diagrams)
│   └── diagrams/
│       ├── README.md (this file)
│       ├── LONPFB_Main_Flow.mmd
│       ├── LONPFB_Validation_Flow.mmd
│       ├── LONPFB_Error_Handling_Flow.mmd
│       └── LONPFB_Data_Flow_Architecture.mmd
```

## Standards

All Mermaid diagrams follow these conventions:
- Use descriptive node labels
- Include decision points with clear Yes/No paths
- Use consistent styling and flow direction
- Include error handling paths
- Document key processing steps

For questions or updates to these diagrams, refer to the main LONPFB documentation or contact the development team.
