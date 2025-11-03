# C-ELAPSED-TIME Program Documentation

---
Location: APIPAY_Inlined.CBL
Generated on: 2025-07-31
Program ID: C-ELAPSED-TIME
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
C-ELAPSED-TIME computes elapsed time between two dates, supporting multiple year types and unit period codes. It handles leap years, zero dates, and outputs elapsed months, days, and results.

## Transaction Types Supported
- Elapsed time calculation (multiple year types)

## Input Parameters
- `NUM-DATE`, `SYS-DATE`: Input dates
- `DATER-UNITPER-CD`, `DATER-UNITPER-FREQ`: Unit period code and frequency

## Output Fields
- `ELAPSED-DAYS`, `ELAPSED-MONTHS`, `ELAPSED-REM`, `ELAPSED-RESULTS`: Elapsed values and result code

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Move Input Dates]
    B --> C[Check for Zero Dates]
    C --> D[PERFORM C-TIMALL]
    D --> E[PERFORM C-TIM-RESULTS]
    E --> F[Output Results]
    F --> G[Unit Period Calculation]
    G --> H[PERFORM C-TIMALL]
    H --> I[Output Unit Period Results]
    I --> J[End]
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-ELAPSED-TIME Detailed Flow
graph TD
    Start([Start])
    InitVars["Initialize variables"]
    ReadInput["Read input records"]
    ProcessLoop{More records?}
    Validate["Validate record"]
    Calc["Calculate elapsed time"]
    WriteOutput["Write output record"]
    Error["Handle errors"]
    End([End])

    Start --> InitVars --> ReadInput --> ProcessLoop
    ProcessLoop -- Yes --> Validate --> Calc --> WriteOutput --> ReadInput
    ProcessLoop -- No --> End
    Validate -- Error --> Error --> ReadInput
```

## Batch or Sequential Process Timeline
```mermaid
gantt
    title C-ELAPSED-TIME Batch Timeline
    section Initialization
    Move Input Dates :done, a1, 2025-07-31, 1d
    section Calculation
    Check for Zero Dates :a2, after a1, 1d
    C-TIMALL :a3, after a2, 1d
    C-TIM-RESULTS :a4, after a3, 1d
    Output Results :a5, after a4, 1d
    Unit Period Calculation :a6, after a5, 1d
    C-TIMALL :a7, after a6, 1d
    Output Unit Period Results :a8, after a7, 1d
```

## Paragraph-Level Flow Explanation
- **Move Input Dates**: Loads input dates into working variables.
- **Check for Zero Dates**: Handles special case for zero dates.
- **PERFORM C-TIMALL**: Calculates elapsed time based on year type.
- **PERFORM C-TIM-RESULTS**: Computes result code.
- **Output Results**: Moves results to output fields.
- **Unit Period Calculation**: Handles unit period code/frequency logic.

## Data Flow Mapping
```mermaid
graph TD
    NUM-DATE --> WS-DATE1
    SYS-DATE --> WS-DATE2
    WS-DATE1 --> ELAPSED-DAYS
    WS-DATE2 --> ELAPSED-DAYS
    DATER-UNITPER-CD --> ELAPSED-REM
    DATER-UNITPER-FREQ --> ELAPSED-REM
```

## Referenced Programs
- C-TIMALL (internal)
- C-TIM-RESULTS (internal)

## Error Handling Flow
- Handles zero dates
- Validates input fields

## Error Handling and Validation
- Validates input date fields
- Ensures correct year type logic

## Common Error Conditions
- Zero or invalid date
- Incorrect year type

## Technical Implementation
- Uses working-storage fields
- No external file I/O
- Key algorithms: elapsed time calculation, unit period logic

## Integration Points
- Used by other date calculation routines

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-ELAPSED-TIME --> C-TIMALL
    C-ELAPSED-TIME --> C-TIM-RESULTS
```
