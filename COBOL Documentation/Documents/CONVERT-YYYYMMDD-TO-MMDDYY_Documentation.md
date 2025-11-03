# CONVERT-YYYYMMDD-TO-MMDDYY Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** August 4, 2025  
**Program ID:** CONVERT-YYYYMMDD-TO-MMDDYY  
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
This routine converts dates from YYYYMMDD format to MMDDYY format, supporting legacy system compatibility and reporting.

## Transaction Types Supported
- Date format conversion (standard to legacy)

## Input Parameters
- `STD-DATE` (YYYYMMDD): Standardized date input

## Output Fields
- `LEGACY-DATE` (MMDDYY): Legacy date output

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Validate YYYYMMDD]
    B --> C[Convert to MMDDYY]
    C --> D[Output LEGACY-DATE]
    D --> E[End]
```

### Detailed Flow
```mermaid
graph TD
    Start([Start]) --> Validate[Validate YYYYMMDD]
    Validate --> Convert[Convert to MMDDYY]
    Convert --> Output[Output LEGACY-DATE]
    Output --> End([End])
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title CONVERT-YYYYMMDD-TO-MMDDYY Batch Timeline
section Initialization
Validate Input :a1, 2025-08-04, 1d
section Processing
Convert Date :a2, after a1, 1d
section Result
Output LEGACY-DATE :a3, after a2, 1d
```

## Paragraph-Level Flow Explanation
- **VALIDATE-YYYYMMDD**: Checks input for correct format and range.
- **CONVERT-DATE**: Performs conversion logic, handling century windowing.
- **OUTPUT-RESULT**: Returns legacy date.

## Data Flow Mapping
```mermaid
graph TD
    STD_DATE --> VALIDATE_YYYYMMDD --> CONVERT_DATE --> LEGACY_DATE
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
- Used by routines requiring legacy date output.

## File Dependencies
- No external files; uses internal paragraphs.

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    MAIN --> VALIDATE-YYYYMMDD --> CONVERT-DATE --> OUTPUT-RESULT
```
