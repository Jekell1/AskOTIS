# C-TIM999-998 Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-TIM999-998  
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
C-TIM999-998 computes elapsed time between two dates using a custom 998/999-day year convention. It is used for specialized financial calculations where standard 365/360/367-day logic does not apply. The routine adjusts months and days, handles negative differences, and normalizes date order for accurate results.

## Transaction Types Supported
- Elapsed time calculation (998/999-day year)

## Input Parameters
- `WS-DATE1-CCYY`, `WS-DATE1-MM`, `WS-DATE1-DD`: Start date
- `WS-DATE2-CCYY`, `WS-DATE2-MM`, `WS-DATE2-DD`: End date
- `TA-YRTYPE`: Year type (should be 998 or 999 for this routine)

## Output Fields
- `WS-XMONTHS`: Elapsed months
- `WS-XDAYS`: Elapsed days
- `WS-XREM`: Remainder days

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[PERFORM C-TIMBEG]
    B --> C[Compute Elapsed Months]
    C --> D[Adjust for Negative Days]
    D --> E[Increment Date by Months]
    E --> F[Convert Dates to Julian]
    F --> G[Compute Elapsed Days]
    G --> H[PERFORM C-TIMEND]
    H --> I[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-TIM999-998 Detailed Flow
graph TD
    Start([Start])
    InitVars["PERFORM C-TIMBEG"]
    CalcMonths["Compute WS-XMONTHS"]
    NegDayAdj["Adjust for Negative Days"]
    SetWork["Set WS-UP-WORK"]
    MoveDate["Move Date2 to NDTE"]
    IncrMonths["PERFORM C-INCR-MONTHS"]
    MoveBack["Move NDTE back to Date2"]
    ToJulian["PERFORM C-CALL-JUL"]
    CalcDays["Compute WS-XDAYS"]
    Timend["PERFORM C-TIMEND"]
    End([End])

    Start --> InitVars --> CalcMonths --> NegDayAdj --> SetWork --> MoveDate --> IncrMonths --> MoveBack --> ToJulian --> CalcDays --> Timend --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
    title C-TIM999-998 Batch Timeline
    section Initialization
    C-TIMBEG :done, a1, 2025-07-31, 1d
    section Calculation
    Compute Months :a2, after a1, 1d
    Adjust for Negative Days :a3, after a2, 1d
    Increment Date :a4, after a3, 1d
    Convert to Julian :a5, after a4, 1d
    Compute Days :a6, after a5, 1d
    C-TIMEND :a7, after a6, 1d
```

## Paragraph-Level Flow Explanation
- **C-TIM999-998**: Main routine for 998/999-day elapsed time calculation. Calls C-TIMBEG, computes months, adjusts for negative days, increments date, converts to Julian, computes days, and calls C-TIMEND.
- **C-TIMBEG**: Initializes and normalizes date order, converts dates to Julian.
- **C-INCR-MONTHS**: Increments a date by a number of months, adjusting for month-end.
- **C-CALL-JUL**: Converts a date to Julian format.
- **C-TIMEND**: Finalizes elapsed time, sets correct sign, and computes remainders.

## Data Flow Mapping
```mermaid
graph TD
    WS-DATE1-CCYY --> WS-XMONTHS
    WS-DATE1-MM --> WS-XMONTHS
    WS-DATE1-DD --> WS-XDAYS
    WS-DATE2-CCYY --> WS-XMONTHS
    WS-DATE2-MM --> WS-XMONTHS
    WS-DATE2-DD --> WS-XDAYS
    WS-XMONTHS --> WS-XDAYS
    WS-XDAYS --> WS-XREM
```

## Referenced Programs
- C-TIMBEG (internal)
- C-INCR-MONTHS (internal)
- C-CALL-JUL (internal)
- C-TIMEND (internal)

## Error Handling Flow
- Handles negative day differences by adjusting months
- Normalizes date order if start date is after end date

## Error Handling and Validation
- Validates input date fields
- Ensures correct sign for elapsed time
- Handles edge cases for month and day boundaries

## Common Error Conditions
- Negative day difference
- Incorrect date order
- Invalid input fields

## Technical Implementation
- Uses working-storage fields for all calculations
- No external file I/O in this routine
- Relies on internal subroutines for date normalization and conversion

## Integration Points
- Used by batch and online processes requiring 998/999-day elapsed time calculations

## File Dependencies
- No external files; uses internal paragraphs and working-storage

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-TIM999-998 --> C-TIMBEG
    C-TIM999-998 --> C-INCR-MONTHS
    C-TIM999-998 --> C-CALL-JUL
    C-TIM999-998 --> C-TIMEND
```
