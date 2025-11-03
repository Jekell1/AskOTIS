# C-INCR-MONTHS Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-INCR-MONTHS  
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
C-INCR-MONTHS increments a date by a specified number of months, adjusting for month and year boundaries. It ensures the resulting date is valid and normalized, handling leap years and month-end logic.

## Transaction Types Supported
- Date increment by months

## Input Parameters
- `NDTE-DATE`: Date to increment (CCYYMMDD)
- `WS-UP-WORK`: Number of months to increment

## Output Fields
- `NDTE-DATE`: Incremented date (CCYYMMDD)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Calculate Years/Months]
    B --> C[Adjust for Month Overflow]
    C --> D[Adjust for Day Overflow]
    D --> E[Call New Date]
    E --> F[Update NDTE-DATE]
    F --> G[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-INCR-MONTHS Detailed Flow
graph TD
    Start([Start])
    CalcYM["Divide months into years/months"]
    AddToDate["Add to NDTE-MM/NDTE-CCYY"]
    MonthUnder["If month < 1, adjust"]
    MonthOver["If month > 12, adjust"]
    DayOverflow["If day > 99, set to 99"]
    MoveFields["Move fields to NDTE"]
    CallNewDate["PERFORM C-CALL-NEWDATE"]
    UpdateFields["Update NDTE fields"]
    End([End])

    Start --> CalcYM --> AddToDate --> MonthUnder --> MonthOver --> DayOverflow --> MoveFields --> CallNewDate --> UpdateFields --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title C-INCR-MONTHS Batch Timeline
    section Initialization
    Calculate_Years_Months :done, a1, 2025-07-31, 1d
    section Calculation
    Adjust_for_Month_Overflow :a2, after a1, 1d
    Adjust_for_Day_Overflow :a3, after a2, 1d
    Call_New_Date :a4, after a3, 1d
    Update_NDTE_Fields :a5, after a4, 1d
```

## Paragraph-Level Flow Explanation
- **C-INCR-MONTHS**: Divides months into years/months, adjusts for overflow, and updates NDTE fields.
- **C-CALL-NEWDATE**: Ensures the resulting date is valid.

## Data Flow Mapping
```mermaid
graph TD
    NDTE-DATE --> WS-NDTE-MM-S
    WS-UP-WORK --> WS-NDTE-MM-S
    WS-NDTE-MM-S --> NDTE-MM
    WS-NDTE-CCYY-S --> NDTE-CCYY
    NDTE-MM --> NDTE-DATE
```

## Referenced Programs
- C-CALL-NEWDATE (internal)

## Error Handling Flow
- Adjusts for invalid months/days
- Ensures valid date output

## Error Handling and Validation
- Validates month and day fields
- Handles overflow/underflow

## Common Error Conditions
- Month < 1 or > 12
- Day > 99

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring month arithmetic

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-INCR-MONTHS --> C-CALL-NEWDATE
```
