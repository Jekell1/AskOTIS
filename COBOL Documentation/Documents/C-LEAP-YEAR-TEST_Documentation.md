# C-LEAP-YEAR-TEST Program Documentation

**Location:** APIPAY/APIPAY_Inlined.CBL  
**Generated on:** July 31, 2025  
**Program ID:** C-LEAP-YEAR-TEST  
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
C-LEAP-YEAR-TEST determines if a given year is a leap year. It sets a flag to indicate true or false, following standard leap year rules.

## Transaction Types Supported
- Leap year determination

## Input Parameters
- `LEAP-YEAR-CCYY`: Year to test (4 digits)

## Output Fields
- `LEAP-YEAR-FG`: 'T' (true) or 'F' (false)

## Program Flow Diagrams
### High-Level Flow
```mermaid
graph TD
    A[Start] --> B[Divide by 4]
    B --> C{Remainder 0?}
    C -- No --> D[Set F]
    C -- Yes --> E[Divide by 100]
    E --> F{Remainder 0?}
    F -- No --> G[Set T]
    F -- Yes --> H[Divide by 400]
    H --> I{Remainder 0?}
    I -- No --> J[Set F]
    I -- Yes --> K[Set T]
    D --> L[End]
    G --> L
    J --> L
    K --> L
```

### Detailed Flow
```mermaid
%% Mermaid Flowchart: C-LEAP-YEAR-TEST Detailed Flow
graph TD
    Start([Start])
    Div4["Divide by 4"]
    Rem4["Remainder 0?"]
    SetF["Set F"]
    Div100["Divide by 100"]
    Rem100["Remainder 0?"]
    SetT["Set T"]
    Div400["Divide by 400"]
    Rem400["Remainder 0?"]
    SetF2["Set F"]
    SetT2["Set T"]
    End([End])

    Start --> Div4 --> Rem4
    Rem4 -- No --> SetF --> End
    Rem4 -- Yes --> Div100 --> Rem100
    Rem100 -- No --> SetT --> End
    Rem100 -- Yes --> Div400 --> Rem400
    Rem400 -- No --> SetF2 --> End
    Rem400 -- Yes --> SetT2 --> End
```

## Batch or Sequential Process Timeline
```mermaid
gantt
title C-LEAP-YEAR-TEST Batch Timeline
    section Initialization
    Divide by 4 :done, a1, 2025-07-31, 1d
    section Calculation
    Divide by 100 :a2, after a1, 1d
    Divide by 400 :a3, after a2, 1d
    Set Flag :a4, after a3, 1d
```

## Paragraph-Level Flow Explanation
- **C-LEAP-YEAR-TEST**: Divides year by 4, 100, and 400, sets flag based on remainders.

## Data Flow Mapping
```mermaid
graph TD
    LEAP_YEAR_CCYY --> C_LEAP_YEAR_TEST --> LEAP_YEAR_FG
```

## Referenced Programs
- None (self-contained)

## Error Handling Flow
- Handles non-numeric or invalid years

## Error Handling and Validation
- Ensures year is 4 digits

## Common Error Conditions
- Year not divisible by 4
- Year not 4 digits

## Technical Implementation
- Uses working-storage fields
- No external file I/O

## Integration Points
- Used by routines requiring leap year logic

## File Dependencies
- No external files; uses internal paragraphs

## Call Graph of PERFORMed Paragraphs
```mermaid
graph TD
    C-LEAP-YEAR-TEST
```
