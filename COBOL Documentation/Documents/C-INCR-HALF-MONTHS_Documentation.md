# C-INCR-HALF-MONTHS Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-INCR-HALF-MONTHS  
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
C-INCR-HALF-MONTHS increments a date by a number of half-months. It uses C-INCR-MONTHS for full months and adjusts days for half-month increments, handling both positive and negative increments.

## Transaction Types Supported
- Date increment by half-months

## Input Parameters
- `NDTE-DATE`: Date to increment (CCYYMMDD)
- `WS-UP-WORK`: Number of half-months to increment

## Output Fields
- `NDTE-DATE`: Incremented date (CCYYMMDD)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Divide Half-Months]
    B --> C[PERFORM C-INCR-MONTHS]
    C --> D[Adjust Days for Half-Month]
    D --> E[Update NDTE Fields]
    E --> F[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-INCR-HALF-MONTHS Detailed Flow
graph TD
    Start([Start])
    SaveDay["Save NDTE-DD-S"]
    Divide["Divide WS-UP-WORK by 2"]
    IncrMonths["PERFORM C-INCR-MONTHS"]
    Mod2["Check remainder"]
    IfMod2["If remainder > 0"]
    DayAdj["Adjust days for half-month"]
    UpdateFields["Update NDTE fields"]
    End([End])

    Start --> SaveDay --> Divide --> IncrMonths --> Mod2
    Mod2 -- Yes --> IfMod2 --> DayAdj --> UpdateFields --> End
    Mod2 -- No --> UpdateFields --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
    title C-INCR-HALF-MONTHS Batch Timeline
    section Initialization
    Save Day :done, a1, 2025-07-31, 1d
    section Calculation
    Divide Half-Months :a2, after a1, 1d
    Increment Months :a3, after a2, 1d
    Adjust Days :a4, after a3, 1d
    Update NDTE Fields :a5, after a4, 1d
```

## Paragraph-Level Flow Explanation
- **C-INCR-HALF-MONTHS**: Divides half-months, calls C-INCR-MONTHS, and adjusts days for half-month increments.
- **C-INCR-MONTHS**: Handles full month increments.

## Data Flow Mapping
```mermaid
graph TD
    NDTE_DATE --> WS_NDTE_DD_S
    WS_UP_WORK --> WS_DIV_RESULT
    WS_DIV_RESULT --> C_INCR_MONTHS
    WS_MOD_2 --> Day_Adjustment
    NDTE_DD --> NDTE_DATE
```

## Referenced Programs
- C-INCR-MONTHS (internal)

## Error Handling Flow
- Handles positive and negative increments
- Adjusts for day overflow/underflow

## Error Handling and Validation
- Validates day fields after adjustment

## Common Error Conditions
- Invalid input date
- Day out of range after adjustment

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring half-month arithmetic

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-INCR-HALF-MONTHS --> C-INCR-MONTHS
```
