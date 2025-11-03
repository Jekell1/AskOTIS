# TIM360 COBOL Program Design Document

**Location:** `./APIPAY_Inlined.CBL`
**Generated on:** 2025-07-24

---

## Table of Contents
1. [Program Overview](#program-overview)
2. [Transaction Types Supported](#transaction-types-supported)
3. [Input Parameters](#input-parameters)
4. [Output Fields](#output-fields)
5. [Program Flow Diagrams](#program-flow-diagrams)
    - [High-Level Flow](#high-level-flow)
    - [Detailed Flows](#detailed-flows)
6. [Batch or Sequential Process Timeline](#batch-or-sequential-process-timeline)
7. [Paragraph-Level Flow Explanation](#paragraph-level-flow-explanation)
8. [Data Flow Mapping](#data-flow-mapping)
9. [Referenced Programs](#referenced-programs)
10. [Error Handling Flow](#error-handling-flow)
11. [Error Handling and Validation](#error-handling-and-validation)
12. [Common Error Conditions](#common-error-conditions)
13. [Technical Implementation](#technical-implementation)
14. [Integration Points](#integration-points)
15. [File Dependencies](#file-dependencies)
16. [Call Graph of PERFORMed Paragraphs](#call-graph-of-performed-paragraphs)

---

## Program Overview
TIM360 is a COBOL routine for calculating elapsed time between two dates using the 30/360 day-count convention. It is used in financial, actuarial, and loan calculations where a 360-day year (12 months of 30 days) is required. TIM360 is implemented as a section in `APIPAY_Inlined.CBL` and delegates most logic to shared routines.

## Transaction Types Supported
- Elapsed time calculation between two dates using the 30/360 convention
- Used for interest, loan, and actuarial computations
- Supports negative elapsed time (start date after end date)

## Input Parameters
- `NUM-DATE` (Start date, format: YYYYMMDD)
- `SYS-DATE` (End date, format: YYYYMMDD)
- (Internally set) `ELAPSED-YRTYPE` = 360

## Output Fields
- `ELAPSED-DAYS`: Total days elapsed (30/360 logic)
- `ELAPSED-MONTHS`: Number of full months elapsed
- `ELAPSED-REM`: Remaining days after full months
- `ELAPSED-RESULT`: Comparison result (e.g., LT, GT, EQ)

## Program Flow Diagrams
### High-Level Flow
```mermaid
flowchart TD
    Start([Start: Call TIM360]) --> SetYrtype[Set ELAPSED-YRTYPE = 360]
    SetYrtype --> PerformTimall[PERFORM TIMALL]
    PerformTimall --> CheckZero{NUM-DATE or SYS-DATE = 0?}
    CheckZero -- Yes --> SetZero[Set outputs to 0, exit]
    CheckZero -- No --> SaveState[Save NDTE-DATE, set unit period to M/1]
    SaveState --> SetAction[Set action code to 'E']
    SetAction --> PerformDater[PERFORM DATER-ROUTINE]
    PerformDater --> RestoreState[Restore NDTE-DATE]
    RestoreState --> CallElapsed[In DATER-ROUTINE: Call C-ELAPSED-TIME]
    CallElapsed --> SetupWS[Set up working storage]
    SetupWS --> PerformCtimall[PERFORM C-TIMALL]
    PerformCtimall --> CheckYrtype2{TA-YRTYPE = 360?}
    CheckYrtype2 -- Yes --> PerformCtim360[PERFORM C-TIM360-358]
    PerformCtim360 --> AdjustDays[Adjust days > 30 to 30]
    AdjustDays --> LeapYear[Handle leap year for Feb]
    LeapYear --> ComputeElapsed[Compute elapsed: years*360 + months*30 + days]
    ComputeElapsed --> PerformCtimend[PERFORM C-TIMEND]
    PerformCtimend --> Finalize[Set sign, finalize outputs]
    Finalize --> Return[Return results]
```


#### Detailed Mermaid Diagrams

**TIM360 Detailed Flow**  
<sub>Shows the full step-by-step logic for the TIM360 routine, including all major decision points and calls.</sub>
```mermaid
flowchart TD
    Start([Start: Call TIM360]) --> SetYrtype[Set ELAPSED-YRTYPE = 360]
    SetYrtype --> PerformTimall[PERFORM TIMALL]
    PerformTimall --> CheckZero{NUM-DATE or SYS-DATE = 0?}
    CheckZero -- Yes --> SetZero[Set outputs to 0, exit]
    CheckZero -- No --> SaveState[Save NDTE-DATE, set unit period to M/1]
    SaveState --> SetAction[Set action code to 'E']
    SetAction --> PerformDater[PERFORM DATER-ROUTINE]
    PerformDater --> RestoreState[Restore NDTE-DATE]
    RestoreState --> CallElapsed[In DATER-ROUTINE: Call C-ELAPSED-TIME]
    CallElapsed --> SetupWS[Set up working storage]
    SetupWS --> PerformCtimall[PERFORM C-TIMALL]
    PerformCtimall --> CheckYrtype2{TA-YRTYPE = 360?}
    CheckYrtype2 -- Yes --> PerformCtim360[PERFORM C-TIM360-358]
    PerformCtim360 --> AdjustDays[Adjust days > 30 to 30]
    AdjustDays --> LeapYear[Handle leap year for Feb]
    LeapYear --> ComputeElapsed[Compute elapsed time based on years, months, and days]
    ComputeElapsed --> PerformCtimend[PERFORM C-TIMEND]
    PerformCtimend --> Finalize[Set sign, finalize outputs]
    Finalize --> Return[Return results]
```

**C-TIM360-358 Logic**  
<sub>Details the 30/360 calculation logic, including day/month adjustments and leap year handling.</sub>
```mermaid
flowchart TD
    Begin([Begin C-TIM360-358]) --> TimBeg[PERFORM C-TIMBEG]
    TimBeg --> CheckD1[IF WS-DATE1-DD > 30]
    CheckD1 -- Yes --> SetD1[Set WS-DATE1-DD = 30]
    CheckD1 -- No --> CheckD2
    SetD1 --> CheckD2[IF WS-DATE2-DD > 30]
    CheckD2 -- Yes --> SetD2[Set WS-DATE2-DD = 30]
    CheckD2 -- No --> LeapYear[Handle leap year for Feb]
    SetD2 --> LeapYear
    LeapYear --> Compute[Compute elapsed]
    Compute --> TimEnd[PERFORM C-TIMEND]
    TimEnd --> End([End C-TIM360-358])
```

**TIMALL Driver Flow**  
<sub>Illustrates the driver logic for all time calculations, including input validation and routine setup.</sub>
```mermaid
flowchart TD
    Begin([Begin TIMALL]) --> CheckZero{NUM-DATE = 0 or SYS-DATE = 0?}
    CheckZero -- Yes --> SetZero[Set outputs to 0, exit]
    CheckZero -- No --> SaveState[Save NDTE-DATE, HOLD-UNITPER-INFO]
    SaveState --> SetUnit[Set DATER-UNITPER-CD = 'M', FREQ = 1]
    SetUnit --> SetAction[Set DATER-ACTION-CODE = 'E']
    SetAction --> PerformDater[PERFORM DATER-ROUTINE]
    PerformDater --> RestoreState[Restore NDTE-DATE, HOLD-UNITPER-INFO]
    RestoreState --> End([End TIMALL])
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title TIM360 Batch/Sequential Timeline
section TIM360
TIM360 Call           :a1, 2025-07-24, 1d
TIMALL Driver         :a2, after a1, 1d
DATER-ROUTINE         :a3, after a2, 1d
C-ELAPSED-TIME        :a4, after a3, 1d
C-TIMALL              :a5, after a4, 1d
C-TIM360-358          :a6, after a5, 1d
C-TIMEND              :a7, after a6, 1d
```

## Paragraph-Level Flow Explanation
- **TIM360 SECTION**: Entry point. Sets year type to 360 and delegates to TIMALL.
- **TIMALL SECTION**: Driver for all time calculations. Handles input validation, sets up unit period, and calls DATER-ROUTINE.
- **DATER-ROUTINE**: Core date calculation logic. Calls C-ELAPSED-TIME.
- **C-ELAPSED-TIME**: Sets up working storage, delegates to C-TIMALL.
- **C-TIMALL**: Dispatches to the correct year-type routine (C-TIM360-358 for 360).
- **C-TIM360-358**: Implements 30/360 logic. Adjusts days, handles leap years, computes elapsed time.
- **C-TIMEND**: Finalizes sign and output fields.

## Data Flow Mapping
```mermaid
flowchart LR
    NUM-DATE & SYS-DATE --> TIM360
    TIM360 --> TIMALL
    TIMALL --> DATER-ROUTINE
    DATER-ROUTINE --> C-ELAPSED-TIME
    C-ELAPSED-TIME --> C-TIMALL
    C-TIMALL --> C-TIM360-358
    C-TIM360-358 --> C-TIMEND
    C-TIMEND --> ELAPSED-DAYS & ELAPSED-MONTHS & ELAPSED-REM & ELAPSED-RESULT
```

## Referenced Programs
- `DATER-ROUTINE` (internal, not a separate program)
- `C-ELAPSED-TIME` (internal)
- `C-TIMALL` (internal)
- `C-TIM360-358` (internal)
- `C-TIMEND` (internal)
- `C-LEAP-YEAR-TEST` (internal)
- `C-CALL-JUL` (internal)

## Error Handling Flow
- If either input date is zero, all outputs are set to zero and the routine exits.
- If invalid date formats are detected, the routine defaults to safe values.

## Error Handling and Validation
- Input validation for zero or invalid dates
- Leap year logic for February
- Negative elapsed time supported

## Common Error Conditions
- Input date is zero or invalid
- February 29th on non-leap years
- Start date after end date (results are negative)

## Technical Implementation
- **Data Structures:** Uses working-storage fields for dates, elapsed values, and temporary variables.
- **File Handling:** No direct file I/O in TIM360; relies on in-memory data.
- **Key Algorithms:** 30/360 day-count, leap year detection, sign handling for negative intervals.

## Integration Points
- Used by other routines in APIPAY_Inlined.CBL for timing calculations
- Can be called by external programs for 30/360 logic

## File Dependencies
- No external files; all logic is internal to APIPAY_Inlined.CBL
- Uses internal copybooks and working-storage

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    TIM360 --> TIMALL
    TIMALL --> DATER-ROUTINE
    DATER-ROUTINE --> C-ELAPSED-TIME
    C-ELAPSED-TIME --> C-TIMALL
    C-TIMALL --> C-TIM360-358
    C-TIM360-358 --> C-TIMEND
```

---

**See also:**
- [DATER-ROUTINE Documentation](DATER-ROUTINE_Documentation.md)
- [C-TIM360-358 Detailed Flow](Diagrams/C-TIM360-358_flow.mmd)
- [TIMALL Driver Flow](Diagrams/TIMALL_flow.mmd)
- [Main APIPAY Documentation](APIPAY_Documentation.md)
