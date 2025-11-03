# OPEN-GI1-FILE

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** August 5, 2025  
**Program ID:** OPEN-GI1-FILE  
**Date Written:** See source comments

## Table of Contents
1. [Program Overview](#program-overview)
2. [Transaction Types Supported](#transaction-types-supported)
3. [Input Parameters](#input-parameters)
4. [Output Fields](#output-fields)
5. [Program Flow Diagrams](#program-flow-diagrams)
6. [Batch or Sequential Process Timeline](#batch-or-sequential-process-timeline)
7. [Paragraph-Level Flow Explanation](#paragraph-level-flow-explanation)
8. [Data Flow Mapping](#data-flow-mapping)
9. [Referenced Programs](#referenced-programs)
10. [Error Handling Flow](#error-handling-flow)
11. [Error Handling and Validation](#error-handling-and-validation)
12. [Common Error Conditions](#common-error-conditions)
13. [Technical Implementation](#technical-implementation)
14. [Integration Points](#integration-points)
15. [File Dependencies](#file-dependencies)
16. [Call Graph of PERFORMed Paragraphs](#call-graph-of-performed-paragraphs)

---

## Program Overview
`OPEN-GI1-FILE` opens the GI1 general inquiry file, which is used for general inquiry and reporting operations in the APIPAY batch process. This file is required for reading and updating inquiry data.

## Transaction Types Supported
- Not transaction-based; supports all operations requiring general inquiry file access.

## Input Parameters
- None directly; uses system configuration for file path.

## Output Fields
- None directly; makes the GI1 file available for subsequent operations.

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Assign GI1 file path]
    B --> C[Open GI1 file]
    C --> D[Check file status]
    D --> E[Return to caller]
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title OPEN-GI1-FILE Timeline
section File Operations
Assign Path      :done, a1, 2025-08-05, 1m
Open File        :done, a2, after a1, 1m
Check Status     :done, a3, after a2, 1m
```

## Paragraph-Level Flow Explanation
- Assigns the file path for the GI1 general inquiry file.
- Opens the file using COBOL's SELECT/ASSIGN and OPEN statements.
- Checks the file status code to ensure the file is available.
- Returns control to the calling routine.

## Data Flow Mapping
```mermaid
graph TD
    Input[System Config] --> Path[GI1 File Path]
    Path --> Open[OPEN Statement]
    Open --> Status[File Status]
```

## Referenced Programs
- None

## Error Handling Flow
- If the file cannot be opened, sets a file status code and returns an error to the caller.

## Error Handling and Validation
- Validates file status after OPEN.
- Handles file not found or access denied errors.

## Common Error Conditions
- File not found
- Access denied
- File already open

## Technical Implementation
- Uses COBOL SELECT/ASSIGN and OPEN statements.
- File status checked after OPEN.

## Integration Points
- Used by APIPAY and other routines requiring general inquiry data.

## File Dependencies
- GI1 general inquiry file (see source for path)

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    OPEN-GI1-FILE --> [Return to caller]
```
