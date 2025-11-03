# C-INCR-DAYS Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-INCR-DAYS  
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
C-INCR-DAYS increments a date by a specified number of days. It handles date normalization, leap years, and updates the date fields accordingly. Used for date arithmetic in financial and batch processing routines.

## Transaction Types Supported
- Date increment by days

## Input Parameters
- `NDTE-DATE`: Date to increment (CCYYMMDD)
- `WS-UP-WORK`: Number of days to increment

## Output Fields
- `NDTE-DATE`: Incremented date (CCYYMMDD)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Save Current Dates]
    B --> C[Move NDTE-DATE to NUM-DATE]
    C --> D[Convert to Julian]
    D --> E[Add Days]
    E --> F[Convert Back to Date]
    F --> G[Restore Dates]
    G --> H[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-INCR-DAYS Detailed Flow
graph TD
    Start([Start])
    SaveVars["Save NUM-DATE, SYS-DATE"]
    MoveNDTE["Move NDTE-DATE to NUM-DATE"]
    ToJulian["PERFORM C-CALL-JUL"]
    AddDays["Add WS-UP-WORK to Julian"]
    ToDate["PERFORM C-CALL-CJUL"]
    MoveBack["Move result to NDTE-DATE"]
    RestoreVars["Restore NUM-DATE, SYS-DATE"]
    End([End])

    Start --> SaveVars --> MoveNDTE --> ToJulian --> AddDays --> ToDate --> MoveBack --> RestoreVars --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
    title C-INCR-DAYS Batch Timeline
    section Initialization
    Save Variables :done, a1, 2025-07-31, 1d
    section Calculation
    Convert to Julian :a2, after a1, 1d
    Add Days :a3, after a2, 1d
    Convert Back to Date :a4, after a3, 1d
    Restore Variables :a5, after a4, 1d
```

## Paragraph-Level Flow Explanation
- **C-INCR-DAYS**: Saves current date variables, converts the input date to Julian, adds the increment, converts back, and restores variables.
- **C-CALL-JUL**: Converts Gregorian date to Julian.
- **C-CALL-CJUL**: Converts Julian date back to Gregorian.

## Data Flow Mapping
```mermaid
graph TD
    NDTE-DATE --> NUM-DATE --> P-DATE-IN --> P-JULIAN-DATE --> WS-JUL-WK
    WS-JUL-WK --> P-CJUL-JULIAN-DATE --> P-CJUL-DATE-OUT --> NDTE-DATE
```

## Referenced Programs
- C-CALL-JUL (internal)
- C-CALL-CJUL (internal)

## Error Handling Flow
- Handles invalid dates by restoring original values

## Error Handling and Validation
- Validates date fields before and after increment

## Common Error Conditions
- Invalid input date
- Overflow in date calculation

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring date arithmetic

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-INCR-DAYS --> C-CALL-JUL
    C-INCR-DAYS --> C-CALL-CJUL
```
