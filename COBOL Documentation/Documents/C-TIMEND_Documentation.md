# C-TIMEND Documentation

**generated_on:** 2025-07-31
**Location:** .\APIPAY_Inlined.CBL

## Table of Contents
- [Program Overview](#program-overview)
- [Transaction Types Supported](#transaction-types-supported)
- [Input Parameters](#input-parameters)
- [Output Fields](#output-fields)
- [Program Flow Diagram](#program-flow-diagram)
- [Batch Timeline](#batch-timeline)
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
C-TIMEND is a COBOL routine that sets the correct sign on elapsed time calculations and adjusts date and month values based on business rules. It handles special cases for different year types and reverses dates if required.

## Transaction Types Supported
- Elapsed time calculation
- Date and month normalization

## Input Parameters
- WS-XSIGN
- WS-XDAYS
- WS-XMONTHS
- WS-XREM
- TA-YRTYPE
- WS-JUL-1
- WS-JUL-2
- WS-DATE1
- WS-DATE2

## Output Fields
- WS-XDAYS
- WS-XMONTHS
- WS-XREM
- WS-DATE1
- WS-DATE2
- WS-JUL-1
- WS-JUL-2

## Program Flow Diagram
```mermaid
flowchart TD
    Start --> ComputeDays
    ComputeDays --> YearTypeCheck
    YearTypeCheck -- "998" --> ComputeMonths
    YearTypeCheck -- "365" --> ComputeJulian
    YearTypeCheck -- "Other" --> DivideDays
    ComputeMonths --> ComputeRemainder
    DivideDays --> ComputeRemainder
    ComputeJulian --> End
    ComputeRemainder --> End
    End["End"]
    Start["Start"]
    ComputeDays["COMPUTE WS-XDAYS = WS-XSIGN * WS-XDAYS"]
    YearTypeCheck["Check TA-YRTYPE"]
    ComputeMonths["COMPUTE WS-XMONTHS = WS-XMONTHS * WS-XSIGN"]
    ComputeRemainder["COMPUTE WS-XREM"]
    DivideDays["DIVIDE WS-XDAYS BY 30"]
    ComputeJulian["COMPUTE WS-XDAYS for TA-YRTYPE=365"]
```

## Batch Timeline
```mermaid
gantt
    title C-TIMEND Batch Timeline
    section Elapsed Time
    C-TIMEND :active, a1, 2025-07-31, 1d
    section Processing
    Date/Month Adjustment :after a1, 1d
```

## Paragraph-Level Flow Explanation
- **C-TIMEND SECTION:**
    - Computes elapsed days with sign.
    - Handles special year types (998, 365) for business logic.
    - Adjusts months and remainder days.
    - Reverses dates and Julian values if sign is negative.

## Data Flow Mapping
```mermaid
flowchart LR
    WS_XSIGN["WS-XSIGN"] --> WS_XDAYS["WS-XDAYS"]
    TA_YRTYPE["TA-YRTYPE"] --> YearType["Year Type Check"]
    WS_XDAYS --> WS_XMONTHS["WS-XMONTHS"]
    WS_XMONTHS --> WS_XREM["WS-XREM"]
    WS_JUL_1["WS-JUL-1"] --> Compare["Compare Julian"]
    WS_JUL_2["WS-JUL-2"] --> Compare
    Compare --> WS_XSIGN
```

## Referenced Programs
- None (internal logic only)

## Error Handling Flow
- Handles invalid year types and negative signs.
- Reverses dates if sign is negative.

## Error Handling and Validation
- No explicit error handling; relies on valid input data.

## Common Error Conditions
- Invalid year type
- Negative sign on elapsed days

## Technical Implementation
- Uses COMPUTE and DIVIDE for date math.
- Handles business rules for year types.

## Integration Points
- Used by elapsed time routines and batch processing.

## File Dependencies
- APIPAY_Inlined.CBL

## Call Graph of PERFORMed Paragraphs
```mermaid
flowchart TD
    MAIN --> C-TIMEND
```

---
### Diagrams
- [Program Flow Diagram](../Diagrams/C-TIMEND_Flow.mmd)
- [Batch Timeline Diagram](../Diagrams/C-TIMEND_Batch.mmd)
- [Data Flow Diagram](../Diagrams/C-TIMEND_DataFlow.mmd)
- [Call Graph Diagram](../Diagrams/C-TIMEND_CallGraph.mmd)

Click the links above to view the diagrams.
