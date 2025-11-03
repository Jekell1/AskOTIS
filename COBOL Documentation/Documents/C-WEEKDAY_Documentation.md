# C-WEEKDAY Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-WEEKDAY  
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
C-WEEKDAY computes the day of the week for a given date, returning a value from 1 (Sunday) to 7 (Saturday). It uses Julian conversion for calculation.

## Transaction Types Supported
- Day of week calculation

## Input Parameters
- `NUM-DATE`: Date to evaluate (CCYYMMDD)

## Output Fields
- `DAY--OF-WEEK`: 1 (Sunday) to 7 (Saturday)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Move NUM-DATE to P-DATE-IN]
    B --> C[PERFORM C-CALL-JUL]
    C --> D[Add 1 to Julian]
    D --> E[Divide by 7]
    E --> F[Set DAY--OF-WEEK]
    F --> G[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-WEEKDAY Detailed Flow
graph TD
    Start([Start])
    MoveDate["Move NUM-DATE to P-DATE-IN"]
    ToJulian["PERFORM C-CALL-JUL"]
    AddOne["Add 1 to Julian"]
    Divide7["Divide by 7"]
    SetDay["Set DAY--OF-WEEK"]
    End([End])

    Start --> MoveDate --> ToJulian --> AddOne --> Divide7 --> SetDay --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title C-WEEKDAY Batch Timeline
    section Initialization
    Move NUM-DATE to P-DATE-IN :done, a1, 2025-07-31, 1d
    section Calculation
    Convert to Julian :a2, after a1, 1d
    Add 1 :a3, after a2, 1d
    Divide by 7 :a4, after a3, 1d
    Set DAY--OF-WEEK :a5, after a4, 1d
```

## Paragraph-Level Flow Explanation
- **C-WEEKDAY**: Moves NUM-DATE to P-DATE-IN, converts to Julian, adds 1, divides by 7, sets DAY--OF-WEEK.
- **C-CALL-JUL**: Converts Gregorian date to Julian.

## Data Flow Mapping
```mermaid
graph TD
    NUM_DATE --> P_DATE_IN --> P_JULIAN_DATE --> WS_JUL_WK --> DAY_OF_WEEK
```

## Referenced Programs
- C-CALL-JUL (internal)

## Error Handling Flow
- Handles invalid dates by returning error value

## Error Handling and Validation
- Validates input date

## Common Error Conditions
- Invalid input date

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring day of week

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-WEEKDAY --> C-CALL-JUL
```
