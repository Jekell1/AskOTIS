# C-INCREMENT-PERIODS Program Documentation

---
Location: APIPAY_Inlined.CBL
Generated on: 2025-07-31
Program ID: C-INCREMENT-PERIODS
Written: Extract the author and date from the source code comments if available. If not present, indicate "See source comments" or leave blank.
---

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
C-INCREMENT-PERIODS increments a date by a specified number of periods (months, days, years, etc.) based on input codes and frequency. It supports multiple period types and calls the appropriate increment routine.

## Transaction Types Supported
- Date increment by period (month, day, year, week, half-month, bi-week)

## Input Parameters
- `DATER-UNITPER-CD`: Period code (M, D, Y, S, W, B)
- `DATER-UNITPER-FREQ`: Frequency
- `NDTE-DATE`: Date to increment
- `NDTE-HOLD`: Increment value

## Output Fields
- `NDTE-DATE`: Incremented date

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Move Input Fields]
    B --> C[Compute Increment Value]
    C --> D[Evaluate Period Code]
    D --> E[PERFORM Appropriate Increment Routine]
    E --> F[Move Results to Output]
    F --> G[PERFORM C-CALL-NEWDATE]
    G --> H[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-INCREMENT-PERIODS Detailed Flow
graph TD
    Start([Start])
    MoveFields["Move NDTE fields to working storage"]
    CalcWork["Compute WS-UP-WORK"]
    EvalUnit{Evaluate DATER-UNITPER-CD}
    Months["PERFORM C-INCR-MONTHS"]
    HalfMonths["PERFORM C-INCR-HALF-MONTHS"]
    Days["PERFORM C-INCR-DAYS"]
    SetFields["Move results back to NDTE fields"]
    CallNewDate["PERFORM C-CALL-NEWDATE"]
    End([End])

    Start --> MoveFields --> CalcWork --> EvalUnit
    EvalUnit -- "M" --> Months --> SetFields
    EvalUnit -- "S" --> HalfMonths --> SetFields
    EvalUnit -- "W" --> Days --> SetFields
    EvalUnit -- "B" --> Days --> SetFields
    EvalUnit -- "D" --> Days --> SetFields
    EvalUnit -- "Y" --> Months --> SetFields
    SetFields --> CallNewDate --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
    title C-INCREMENT-PERIODS Batch Timeline
    section Initialization
    Move Input Fields :done, a1, 2025-07-31, 1d
    section Calculation
    Compute Increment Value :a2, after a1, 1d
    Evaluate Period Code :a3, after a2, 1d
    PERFORM Increment Routine :a4, after a3, 1d
    Move Results to Output :a5, after a4, 1d
    PERFORM C-CALL-NEWDATE :a6, after a5, 1d
```

## Paragraph-Level Flow Explanation
- **Move Input Fields**: Loads input fields into working variables.
- **Compute Increment Value**: Calculates increment value.
- **Evaluate Period Code**: Determines which increment routine to call.
- **PERFORM Appropriate Increment Routine**: Calls the correct routine (months, days, etc.).
- **Move Results to Output**: Moves incremented date to output.
- **PERFORM C-CALL-NEWDATE**: Finalizes the incremented date.

## Data Flow Mapping
```mermaid
graph TD
    DATER-UNITPER-CD --> NDTE-DATE
    DATER-UNITPER-FREQ --> NDTE-DATE
    NDTE-DATE --> NDTE-DATE
    NDTE-HOLD --> NDTE-DATE
```

## Referenced Programs
- C-INCR-MONTHS (internal)
- C-INCR-HALF-MONTHS (internal)
- C-INCR-DAYS (internal)
- C-CALL-NEWDATE (internal)

## Error Handling Flow
- Validates period code and frequency
- Handles invalid input

## Error Handling and Validation
- Validates input fields
- Ensures correct increment logic

## Common Error Conditions
- Invalid period code
- Incorrect increment value

## Technical Implementation
- Uses working-storage fields
- No external file I/O
- Key algorithms: period increment logic

## Integration Points
- Used by other date calculation routines

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-INCREMENT-PERIODS --> C-INCR-MONTHS
    C-INCREMENT-PERIODS --> C-INCR-HALF-MONTHS
    C-INCREMENT-PERIODS --> C-INCR-DAYS
    C-INCREMENT-PERIODS --> C-CALL-NEWDATE
```
