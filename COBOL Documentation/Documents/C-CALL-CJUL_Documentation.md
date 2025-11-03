# C-CALL-CJUL Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-CALL-CJUL  
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
C-CALL-CJUL converts a Julian date to a Gregorian calendar date (CCYYMMDD format). It handles leap years and month boundaries.

## Transaction Types Supported
- Julian to Gregorian date conversion

## Input Parameters
- `P-CJUL-JULIAN-DATE`: Julian date (number of days since 12/31/1899)

## Output Fields
- `P-CJUL-DATE-OUT`: Gregorian date (CCYYMMDD)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Move Julian to CJUL-IN]
    B --> C[Calculate Years]
    C --> D[Calculate Days]
    D --> E[Adjust for Leap Year]
    E --> F[Calculate Month]
    F --> G[Set Output Fields]
    G --> H[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-CALL-CJUL Detailed Flow
graph TD
    Start([Start])
    MoveJulian["Move Julian to CJUL-IN"]
    CalcYears["Calculate years"]
    CalcDays["Calculate days"]
    LeapAdj["Adjust for leap year"]
    CalcMonth["Calculate month"]
    SetFields["Set output fields"]
    End([End])

    Start --> MoveJulian --> CalcYears --> CalcDays --> LeapAdj --> CalcMonth --> SetFields --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title C-CALL-CJUL Batch Timeline
    section Initialization
    Move Julian to CJUL-IN :done, a1, 2025-07-31, 1d
    section Calculation
    Calculate Years :a2, after a1, 1d
    Calculate Days :a3, after a2, 1d
    Adjust for Leap Year :a4, after a3, 1d
    Calculate Month :a5, after a4, 1d
    Set Output Fields :a6, after a5, 1d
```

## Paragraph-Level Flow Explanation
- **C-CALL-CJUL**: Moves Julian date to input, calculates years, days, adjusts for leap year, calculates month, sets output fields.

## Data Flow Mapping
```mermaid
graph TD
    P_CJUL_JULIAN_DATE --> CJUL_IN --> WS_CJUL_OUT_CCYY --> WS_CJUL_OUT_DD --> WS_CJUL_OUT_MM --> P_CJUL_DATE_OUT
```

## Referenced Programs
- None (self-contained)

## Error Handling Flow
- Handles invalid Julian dates

## Error Handling and Validation
- Validates input range

## Common Error Conditions
- Julian date out of range

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring Julian to Gregorian conversion

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-CALL-CJUL
```
