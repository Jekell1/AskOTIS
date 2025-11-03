# C-DATE-TEST Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-DATE-TEST  
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
C-DATE-TEST validates a date in CCYYMMDD format. If the date is valid, it returns the same date; otherwise, it returns 0. It uses Julian conversion to check validity.

## Transaction Types Supported
- Date validation

## Input Parameters
- `NUM-DATE`: Date to validate (CCYYMMDD)

## Output Fields
- `NUM-DATE`: Validated date (CCYYMMDD or 0)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Move NUM-DATE to WS-NDTE]
    B --> C[Check Month/Day Validity]
    C --> D{Valid?}
    D -- Yes --> E[Convert to Julian and Back]
    D -- No --> F[Set NUM-DATE to 0]
    E --> G[Compare with Original]
    G -- Match --> H[Return NUM-DATE]
    G -- No Match --> F
    F --> I[End]
    H --> I
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-DATE-TEST Detailed Flow
graph TD
    Start([Start])
    MoveNDTE["Move NUM-DATE to WS-NDTE"]
    CheckValid["Check month/day validity"]
    Valid["If valid"]
    ToJulian["Convert to Julian"]
    BackToDate["Convert back to date"]
    Compare["Compare with original"]
    SetZero["Set NUM-DATE to 0"]
    End([End])

    Start --> MoveNDTE --> CheckValid --> Valid
    Valid -- Yes --> ToJulian --> BackToDate --> Compare
    Compare -- Match --> End
    Compare -- No Match --> SetZero --> End
    Valid -- No --> SetZero --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title C-DATE-TEST Batch Timeline
    section Initialization
    Move NUM-DATE to WS-NDTE :done, a1, 2025-07-31, 1d
    section Validation
    Check Validity :a2, after a1, 1d
    Convert to Julian :a3, after a2, 1d
    Convert Back :a4, after a3, 1d
    Compare :a5, after a4, 1d
    Set Zero :a6, after a5, 1d
```

## Paragraph-Level Flow Explanation
- **C-DATE-TEST**: Moves NUM-DATE to WS-NDTE, checks validity, converts to Julian and back, compares, and sets NUM-DATE to 0 if invalid.

## Data Flow Mapping
```mermaid
graph TD
    NUM_DATE --> WS_NDTE
    WS_NDTE --> P_DATE_IN
    P_DATE_IN --> P_JULIAN_DATE
    P_JULIAN_DATE --> P_CJUL_JULIAN_DATE
    P_CJUL_JULIAN_DATE --> P_CJUL_DATE_OUT
    P_CJUL_DATE_OUT --> NUM_DATE
```

## Referenced Programs
- C-CALL-JUL (internal)
- C-CALL-CJUL (internal)

## Error Handling Flow
- Sets NUM-DATE to 0 if invalid

## Error Handling and Validation
- Checks month and day validity
- Ensures round-trip conversion matches

## Common Error Conditions
- Invalid month or day
- Non-matching conversion

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring date validation

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-DATE-TEST --> C-CALL-JUL
    C-DATE-TEST --> C-CALL-CJUL
```
