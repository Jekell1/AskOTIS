# C-TIMBEG and C-TIMEND - Date Processing Initialization and Finalization

**Location:** .\C-TIMBEG-TIMEND.cpy  
**Generated on:** July 22, 2025  
**Program ID:** C-TIMBEG and C-TIMEND  
**Date Written:** Legacy system (pre-2000)

## Table of Contents

1. [Program Overview](#program-overview)
2. [Transaction Types Supported](#transaction-types-supported)
3. [Input Parameters](#input-parameters)
4. [Output Fields](#output-fields)
5. [Program Flow Diagrams](#program-flow-diagrams)
6. [Batch Processing Timeline](#batch-processing-timeline)
7. [Paragraph-Level Flow Explanation](#paragraph-level-flow-explanation)
8. [Data Flow Mapping](#data-flow-mapping)
9. [Referenced Programs](#referenced-programs)
10. [Error Handling and Validation](#error-handling-and-validation)
11. [Technical Implementation](#technical-implementation)
12. [Integration Points](#integration-points)
13. [File Dependencies](#file-dependencies)
14. [Call Graph of PERFORMed Paragraphs](#call-graph-of-performed-paragraphs)

---

## Overview
`C-TIMBEG` and `C-TIMEND` are essential utility sections that serve as the initialization and finalization components for all elapsed time calculation routines in the APIPAY system. These utilities work together to ensure consistent date handling, proper date order, and standardized calculation results across all time-related functions.

## Purpose
These utilities serve multiple critical purposes:

### C-TIMBEG (Time Calculation Begin)
- Initialize working storage for time calculations
- Save original date values for restoration
- Convert dates to Julian format for easier calculation
- Ensure proper date ordering (swap if needed)
- Set the sign flag for positive or negative elapsed time

### C-TIMEND (Time Calculation End)
- Apply the correct sign to elapsed time results
- Calculate months and remainder days based on year type
- Handle specific year type adjustments (365, 998, etc.)
- Restore original date order if needed
- Standardize output format for all time calculation routines

## Usage

### C-TIMBEG
**Input:**
- `WS-DATE1`: First date (typically start date)
- `WS-DATE2`: Second date (typically end date)

**Output:**
- `WS-JUL-1`: Julian date for first date
- `WS-JUL-2`: Julian date for second date
- `WS-XSIGN`: Sign indicator (-1 if dates were swapped, 1 otherwise)

### C-TIMEND
**Input:**
- `WS-XDAYS`: Raw calculated days
- `WS-XSIGN`: Sign indicator
- `TA-YRTYPE`: Year type code (365, 998, etc.)
- `WS-JUL-1`, `WS-JUL-2`: Julian dates

**Output:**
- `WS-XMONTHS`: Calculated elapsed months
- `WS-XDAYS`: Finalized elapsed days (with sign)
- `WS-XREM`: Remainder days

### Typical Usage Pattern
```cobol
PERFORM C-TIMBEG.
* Specific time calculation code here
PERFORM C-TIMEND.
```

## Technical Details

### C-TIMBEG Algorithm
1. Saves original input dates
2. Converts both dates to Julian format
3. Compares Julian dates
4. If first date is later than second date:
   - Swaps the dates and Julian values
   - Sets sign indicator to -1
5. Otherwise sets sign indicator to 1
6. Restores original input dates

### C-TIMEND Algorithm
1. Applies the sign to the calculated days
2. Processes based on year type:
   - For YRTYPE 998 (Elapsed Unit Periods):
     - Applies sign to months
     - Calculates remainder as days - (months × 30)
   - For other year types:
     - Divides days by 30 to get months and remainder
3. For YRTYPE 365:
   - Recalculates days as the difference between Julian dates
4. If dates were swapped (negative sign):
   - Restores original date order

## Special Year Type Handling

The system supports multiple year types, each with specific calculation methods:
- **YRTYPE 365**: Actual calendar days
- **YRTYPE 998**: Elapsed Unit Periods (30-day periods)
- **Default**: Standard 30-day month calculations

## Dependencies
- `C-CALL-JUL`: Used to convert dates to Julian format

## Integration Points
These utilities are used by all elapsed time calculation routines:
- `C-TIM360-358`: 360/358-day calculations
- `C-TIM361`: 361-day calculations
- `C-TIM362`: 362-day calculations
- `C-TIM365-367`: 365/367-day calculations
- `C-TIM999-998`: Truth in Lending/Elapsed Unit Period calculations
- `C-TIMALL`: All time calculations

## Notes
- The utilities ensure consistent behavior across different time calculation methods
- C-TIMBEG was designed to handle cases where dates might be provided in reverse order
- C-TIMEND contains special handling for year type 998 added by JTG on 05/02/97
