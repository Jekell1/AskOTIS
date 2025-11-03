# CONVERT-MMDDYY-TO-YYYYMMDD Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** August 4, 2025  
**Program ID:** CONVERT-MMDDYY-TO-YYYYMMDD  
**Date Written:** See source comments

## Table of Contents
- [Program Overview](#program-overview)
- [Transaction Types Supported](#transaction-types-supported)
- [Input Parameters](#input-parameters)
- [Output Fields](#output-fields)
- [Program Flow Diagrams](#program-flow-diagrams)
- [Batch or Sequential Process Timeline](#batch-or-sequential-process-timeline)
- [Paragraph-Level Flow Explanation](#paragraph-level-flow-explanation)
- [Data Flow Mapping](#data-flow-mapping)
- [Referenced Programs](#referenced-programs)
- [Error Handling Flow](#error-handling-flow)
- [Error Handling and Validation](#error-handling-and-validation)
- [Common Error Conditions](#common-error-conditions)
- [Technical Implementation](#technical-implementation)
- [Integration Points](#integration-points)
- [File Dependencies](#file-dependencies)
- [Call Graph of PERFORMed Paragraphs](#call-graph-of-performed-paragraphs)

## Program Overview
This routine converts dates from MMDDYY format to YYYYMMDD format, supporting legacy data migration and standardization.

## Transaction Types Supported
- Date format conversion (legacy to standard)

## Input Parameters
- `LEGACY-DATE` (MMDDYY): Legacy date input

## Output Fields
- `STD-DATE` (YYYYMMDD): Standardized date output

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Validate MMDDYY]
    B --> C[Convert to YYYYMMDD]
    C --> D[Output STD-DATE]
    D --> E[End]
```

### Detailed Flow
```mermaid
graph TD
    Start([Start]) --> Validate[Validate MMDDYY]
    Validate --> Convert[Convert to YYYYMMDD]
    Convert --> Output[Output STD-DATE]
    Output --> End([End])
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title CONVERT-MMDDYY-TO-YYYYMMDD Batch Timeline
section Initialization
Validate Input :a1, 2025-08-04, 1d
section Processing
Convert Date :a2, after a1, 1d
section Result
Output STD-DATE :a3, after a2, 1d
```

## Paragraph-Level Flow Explanation
- **VALIDATE-MMDDYY**: Checks input for correct format and range.
- **CONVERT-DATE**: Performs conversion logic, handling century windowing.
- **OUTPUT-RESULT**: Returns standardized date.

## Data Flow Mapping
```mermaid
graph TD
    LEGACY_DATE --> VALIDATE_MMDDYY --> CONVERT_DATE --> STD_DATE
```

## Referenced Programs
- None

## Error Handling Flow
- Returns error if input is invalid or out of range.

## Error Handling and Validation
- Validates input format and range.
- Handles non-numeric and out-of-bounds values.

## Common Error Conditions
- Invalid date input
- Out-of-range year

## Technical Implementation
- Uses working-storage fields for date manipulation.
- No external file I/O.

## Integration Points
- Used by routines requiring date standardization.

## File Dependencies
- No external files; uses internal paragraphs.

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    MAIN --> VALIDATE-MMDDYY --> CONVERT-DATE --> OUTPUT-RESULT
```
