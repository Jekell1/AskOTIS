# LONPF7 - Loan Processing Transaction Posting System

**Location:** .\APIPAY_Inlined.CBL  
**Generated on:** July 22, 2025  
**Program ID:** LONPF7  
**Date Written:** February 17, 1984

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

## Program Overview

**LONPF7** is a comprehensive COBOL loan processing program specifically designed for **Transaction Posting** in a loan servicing system. It serves as **Program 7** in the loan processing suite and handles multiple transaction types including late charges, interest adjustments, waivers, write-offs, and other loan-related transactions.

### Key Characteristics

- **Execution Mode:** Interactive screen-based processing with batch capabilities
- **Primary Function:** Advanced transaction posting with complex balance calculations
- **Transaction Focus:** Handles AL, AI, AP, WL, WI, WO, OT, O2, W2, OP, and RE transaction codes
- **Integration:** Part of the larger LONPF family of loan processing programs
- **Screen Interface:** Uses VDU (Video Display Unit) forms for user interaction
- **Security:** Implements password protection for sensitive transaction types

### Business Purpose

LONPF7 serves critical functions in loan servicing by:

1. **Late Charge Management:** Calculates and applies late charges using configurable formulas
2. **Interest Processing:** Handles additional interest calculations and adjustments
3. **Waiver Processing:** Allows authorized waivers of late charges and interest
4. **Write-off Management:** Processes write-offs for uncollectible amounts
5. **Other Transaction Processing:** Handles miscellaneous charges and adjustments
6. **Balance Distribution:** Sophisticated payment application logic across multiple balance types
7. **Audit Trail:** Maintains comprehensive transaction logs for compliance
8. **GL Integration:** Provides general ledger distribution capabilities

### Historical Context

The program has evolved significantly since 1984 with major enhancements including:
- Password security implementation for sensitive transactions (1995)
- Separate password requirements for different transaction types (1996)
- Weekly payment processing support (1996)
- Cash drawer password integration (1999)
- Revolving credit support (1999-2001)
- Interbranch payment processing (1999-2002)
- Enhanced late charge logic for judgments (2004)
- Batch posting capabilities for RE and O2 transactions (2016-2017)
- Global file integration and modernization (2018-2024)

---

## Transaction Types Supported

LONPF7 supports eleven primary transaction types with sophisticated business logic:

### Late Charge and Interest Transactions
| Code | Description | Purpose | Password Required |
|------|-------------|---------|------------------|
| **AL** | Assess Late Charge | Calculates and applies late charges using configured formulas | No |
| **AI** | Additional Interest | Adds additional interest to interest-bearing loans | No |

### Waiver Transactions
| Code | Description | Purpose | Password Required |
|------|-------------|---------|------------------|
| **WL** | Waive Late Charge | Reduces or eliminates late charge balances | Yes |
| **WI** | Waive Interest | Reduces or eliminates interest balances | Yes |

### Write-off Transactions
| Code | Description | Purpose | Password Required |
|------|-------------|---------|------------------|
| **WO** | Write-off Other | Writes off uncollectible other charges | Yes |
| **W2** | Write-off Type 2 | Writes off uncollectible type 2 other charges | Yes |

### Applied Payment Transactions
| Code | Description | Purpose | Password Required |
|------|-------------|---------|------------------|
| **AP** | Applied Payment | Distributes payments across late charges, interest, and other | Yes |

### Other Charge Transactions
| Code | Description | Purpose | Password Required |
|------|-------------|---------|------------------|
| **OT** | Other Transaction | Processes miscellaneous charges and adjustments | Yes |
| **O2** | Other Type 2 | Processes type 2 other charges | No |

### Special Combination Transactions
| Code | Description | Purpose | Password Required |
|------|-------------|---------|------------------|
| **OP** | Other Payment | Automatic combination of OT and AP logic | No |
| **RE** | Repossession Expense | Adds repossession-related expenses | No |

### Transaction Processing Rules

1. **Password Protection:** WL, WI, WO, W2, OT, and AP transactions require password verification
2. **Interbranch Support:** Special password handling for interbranch postings
3. **Balance Validation:** All transactions validate against available balances
4. **Revolving Credit:** Special handling for revolving credit accounts
5. **Zero Amount Restrictions:** Most transactions do not allow zero amounts
6. **GL Distribution:** Automatic general ledger entry creation

---

## Input Parameters

### Primary Input Structure
The program receives input through the standard LONPF parameter structure:

```cobol
FORM-PATHNAME     - VDU form path for screen display
EXIT-PATHNAME     - Return path after processing
PROG-BUF          - Program control buffer
UPDATE-BUF        - Transaction update buffer
```

### Transaction Input Fields
| Field | Type | Description |
|-------|------|-------------|
| **LP-TRCD** | PIC XX | Transaction code (AL, AI, AP, etc.) |
| **LP-TRAMT** | PIC S9(7)V99 | Transaction amount |
| **LP-PAYDATE** | PIC 9(8) | Payment/transaction date |
| **LP-APLC** | PIC S9(7)V99 | Applied late charge amount |
| **LP-APINT** | PIC S9(7)V99 | Applied interest amount |
| **LP-APOTH** | PIC S9(7)V99 | Applied other charge amount |
| **LP-APOT2** | PIC S9(7)V99 | Applied other type 2 amount |
| **LP-APCUR** | PIC S9(7)V99 | Applied current balance amount |

### Batch Payment Input
For batch processing integration:
```cobol
HOLD-BP-TRCD      - Batch payment transaction code
PATH-MACHINE      - Interbranch posting indicator
```

### Screen Input Fields
| Field | Format | Description |
|-------|--------|-------------|
| **Account Number** | 9(6) | Target loan account |
| **Transaction Code** | XX | Transaction type code |
| **Amount** | Z(5).99- | Transaction amount |
| **Applied Amounts** | Z(4).99- | Distribution amounts |
| **GL Numbers** | 9(11) | General ledger account numbers |
| **Payment Date** | MM/DD/YY | Transaction date |

---

## Output Fields

### Balance Update Fields
| Field | Type | Description |
|-------|------|-------------|
| **LP-CURBAL** | PIC S9(7)V99 | Updated current balance |
| **LP-LCBAL** | PIC S9(7)V99 | Updated late charge balance |
| **LP-INTBAL** | PIC S9(7)V99 | Updated interest balance |
| **LP-OTHBAL** | PIC S9(7)V99 | Updated other charge balance |
| **LP-OT2BAL** | PIC S9(7)V99 | Updated other type 2 balance |
| **LP-PDTH-DATE** | PIC 9(8) | Updated paid through date |
| **LP-LCPDTH-DATE** | PIC 9(8) | Late charge paid through date |

### Transaction Results
| Field | Type | Description |
|-------|------|-------------|
| **LP-NOLCHG** | PIC X | No late charge flag |
| **LP-IBPC** | PIC X | Interest bearing/principal card indicator |
| **LCAP-PARTIALS** | PIC S9(7)V99 | Partial payment amounts |
| **LCAP-LCPAID** | PIC S9(7)V99 | Late charge paid amount |

### Screen Display Output
The program displays formatted transaction screens showing:
- Current balance information
- Applied amount distributions
- Transaction details and confirmations
- Error messages and validation results
- GL distribution windows

### File Updates
| File | Purpose |
|------|---------|
| **LP-FILE** | Primary loan record updates |
| **LX-FILE** | Transaction history updates |
| **LXE-FILE** | Earnings record updates |
| **LTI-FILE** | Transaction index updates |
| **LTP-FILE** | Transaction posting file |
| **WK-FILE** | Work file for temporary calculations |

---

## Program Flow Diagrams

### High-Level Processing Flow
![High-Level Processing Flow](Diagrams/LONPF7_Main_Flow.mmd)

### Transaction Processing Flow
![Transaction Processing Flow](Diagrams/LONPF7_Transaction_Processing.mmd)

### Balance Calculation Flow
![Balance Calculation Flow](Diagrams/LONPF7_Balance_Calculation.mmd)

### Error Handling Flow
![Error Handling Flow](Diagrams/LONPF7_Error_Handling.mmd)

---

## Batch Processing Timeline

### Processing Timeline
![Processing Timeline](Diagrams/LONPF7_Batch_Timeline.mmd)

---

## Paragraph-Level Flow Explanation

### Main Program Control Flow

**MAIN-PROGRAM SECTION**
- **Purpose:** Primary program controller and initialization
- **Flow:** SQL connection → environment setup → batch/interactive processing → cleanup
- **Key Actions:** File opening, parameter validation, branch record loading

**BATCH-PAYMENT-ROUTINE SECTION**
- **Purpose:** Handles automated batch payments from external systems
- **Flow:** Balance setup → paid through calculation → transaction type processing
- **Key Actions:** 
  - Sets LP-OTHBAL for OT transactions
  - Adds amounts to LP-INTDUE and LP-OTHBAL for RE transactions
  - Handles O2 transactions with LP-APINTOWE and LP-OT2BAL updates

**SELECTION-MODULE SECTION**
- **Purpose:** Interactive user interface and transaction routing
- **Flow:** Initialize → amount entry → display → accept input → process
- **Key Actions:** Route control, screen management, user interaction validation

### Transaction Processing Flow

**INITIALIZE-ROUTINE SECTION**
- **Purpose:** Validates transaction types and handles password verification
- **Flow:** Balance setup → transaction validation → password checking
- **Key Actions:**
  - Validates AI transactions for interest-bearing loans only
  - Checks late charge formula for AL transactions
  - Performs password verification for WL, WI, WO, W2, OT, and AP transactions

**ENTRY-LINE-CALCULATIONS SECTION**
- **Purpose:** Core business logic for balance calculations
- **Flow:** Transaction type analysis → formula application → balance updates
- **Key Actions:**
  - AL: Late charge calculation using LATE-CHARGE-APPLY routine
  - AI: Interest balance updates for interest-bearing loans
  - Computes LP-APCUR = LP-TRAMT - LP-APINT - LP-APOTH - LP-APOT2 - LP-APLC

**AMOUNT-ENTRY-ROUTINES SECTION**
- **Purpose:** Interactive amount entry with validation
- **Flow:** Transaction amount → applied other → applied interest/LC → verification
- **Key Actions:**
  - Validates transaction amounts against business rules
  - Handles negative amounts for write-offs and waivers
  - Distributes payments across multiple balance types

### Display and Interface Management

**DISPLAY-ENTRY-LINE SECTION**
- **Purpose:** Formats and displays transaction entry screens
- **Flow:** Setup display buffers → format amounts → display screen
- **Key Actions:** Screen formatting, balance display, field positioning

**ACCEPT-ENTRY SECTION**
- **Purpose:** Handles user input and function key processing
- **Flow:** Accept input → process function keys → validate entries
- **Key Actions:** F-key processing, data validation, navigation control

### Password and Security Management

**VERIFY-PASSWORD SECTION**
- **Purpose:** Validates user passwords for restricted transactions
- **Flow:** Password prompt → validation → authorization check
- **Key Actions:** Password verification, security enforcement

**PASSWORD-MUST-EXIST SECTION**
- **Purpose:** Ensures required passwords exist for transaction types
- **Flow:** Check password configuration → validate existence
- **Key Actions:** Password configuration validation

### Balance Calculation Sections

**SET-BAL-IBPC-LPREC SECTION**
- **Purpose:** Initializes balance fields from loan record
- **Flow:** Copy balances → set IBPC test → validate loan type
- **Key Actions:**
  - LP-OTHBAL = LN-OTHBAL
  - LP-OT2BAL = LN-OT2BAL
  - LP-INTBAL = LN-INTBAL
  - LP-LCBAL = LN-LCBAL
  - LP-CURBAL = LN-CURBAL

### Utility and Support Sections

**CODE-SCAN SECTION**
- **Purpose:** Handles payoff code scanning and validation
- **Flow:** Code entry → validation → processing
- **Key Actions:** Payoff code processing, validation checks

**MISCELLANEOUS-ROUTINES SECTION**
- **Purpose:** Various utility functions and calculations
- **Flow:** Multiple utility functions for common operations
- **Key Actions:** Date calculations, amount formatting, field conversions

---

## Data Flow Mapping

### Input Data Sources
```mermaid
graph LR
    A[APIPAY Batch Input] --> B[LONPF7 PROG-BUF]
    C[Interactive User Input] --> B
    D[LP-FILE Loan Data] --> E[Balance Processing]
    B --> E
    F[SP-FILE State Parameters] --> E
    G[BR-FILE Branch Data] --> E
    H[GB-FILE Global Data] --> E
```

### Key Data Transformations

| Source Field | Transformation | Target Field | Purpose |
|-------------|----------------|--------------|---------|
| LN-OTHBAL | Direct copy | LP-OTHBAL | Initialize other balance |
| LN-INTBAL | Direct copy | LP-INTBAL | Initialize interest balance |
| LN-LCBAL | Direct copy | LP-LCBAL | Initialize late charge balance |
| LP-TRAMT | Formula calculation | LP-APCUR | Calculate applied current amount |
| LCAP-APP | Add to LP-APLC | LP-APLC | Apply calculated late charges |
| INDU-INTEREST | Add to LP-INTBAL | LP-INTBAL | Add additional interest |

### Data Flow Through Processing Steps

**Step 1: Initialization**
- PROG-BUF → Field validation → Working storage
- Loan record → Balance initialization → LP-record fields
- State parameters → Business rule setup

**Step 2: Transaction Setup**
- Transaction code → Transaction type determination
- Amount validation → LP-TRAMT field setup
- Password verification → Security authorization

**Step 3: Balance Calculations**
- Late charge formulas → LCAP calculations → LP-APLC updates
- Interest calculations → INDU-INTEREST → LP-INTBAL updates
- Applied amounts → Distribution logic → Multiple balance updates

**Step 4: Final Processing**
- Current balance calculations → LP-CURBAL updates
- Paid through calculations → LP-PDTH-DATE updates
- GL distribution → General ledger entries

**Step 5: File Updates**
- LP-record updates → LP-FILE write
- Transaction history → LX-FILE write
- Earnings updates → LXE-FILE write

---

## Referenced Programs

### Called Programs
| Program | Purpose | Integration Point |
|---------|---------|------------------|
| **LIBGB/GLENT** | General ledger entry | Called for GL distribution |
| **LIBLP/LCAS** | Late charge assessment | Called for late charge calculations |
| **LIBGB/GBWK** | Global work routines | Utility functions |

### Calling Programs
| Program | Purpose | Relationship |
|---------|---------|-------------|
| **APIPAY** | Batch payment processing | Calls LONPF7 for "OT", "RE", "O2" transactions |
| **LONPF Menu** | Interactive menu system | Direct user access to LONPF7 |
| **LONPF2** | Payment update processor | Coordination for complex transactions |

### Copybooks and Libraries
| Copybook | Purpose |
|----------|---------|
| **LIBLP/LP01LN.CPY** | Loan record layout |
| **LIBLP/LP01LP.CPY** | Loan processing record layout |
| **LIBLP/LP01LX.CPY** | Transaction history layout |
| **LIBLP/LP01LXE.CPY** | Earnings record layout |
| **LIBLP/LP01SP.CPY** | State parameter layout |
| **LIBGB/GB01BR.CPY** | Branch record layout |
| **LIBGB/GB01GB.CPY** | Global record layout |
| **LIBLP/LP01CA.CPY** | Cash advance record layout |
| **LIBLP/LP01CD.CPY** | Cash drawer record layout |

---

## Error Handling and Validation

### Input Validation
| Validation Type | Error Condition | Error Message | Recovery Action |
|----------------|-----------------|---------------|----------------|
| **State Parameter** | SP record not found | "INVALID STATE PARAMETER RECORD" | Exit program |
| **Branch Record** | BR record not found | "INVALID BRANCH RECORD" | Exit program |
| **Global Record** | GB record not found | "INVALID GLOBAL RECORD" | Exit program |
| **Transaction Code** | Invalid/unsupported code | Transaction-specific message | Return to input |

### Business Rule Validation
| Rule | Condition | Action |
|------|-----------|--------|
| **Interest Bearing Only** | AI on principal card loan | Display error, reject transaction |
| **Late Charge Formula** | AL with no LC formula | Display error, reject transaction |
| **Password Required** | Restricted transaction without password | Prompt for password or reject |
| **Zero Amount** | Zero transaction amount | Display error, request valid amount |
| **Balance Limits** | Amount exceeds available balance | Display balance exceeded message |

### Error Recovery Mechanisms
1. **Screen Redisplay:** Invalid inputs redisplay entry screen with error message
2. **Transaction Rollback:** Failed transactions maintain original balances
3. **Password Retry:** Failed password attempts allow retry with limit
4. **Route Control:** Error conditions set route codes for proper navigation

### Common Error Conditions
| Error | Cause | Resolution |
|-------|-------|------------|
| **Balance Exceeded** | Transaction amount > available balance | Reduce amount or verify balance |
| **Password Failure** | Incorrect or missing password | Verify credentials or contact supervisor |
| **Formula Missing** | Late charge formula not configured | Contact system administrator |
| **File Lock** | Concurrent access to loan record | Retry or abort transaction |

---

## Technical Implementation

### Data Structures

**Primary Working Storage Fields**
```cobol
01  ELE                    PIC 99             VALUE 0.
01  TOTEARN                PIC S9(6)V99 COMP  VALUE 0.
01  UNEARNED               PIC S9(6)V99 COMP  VALUE 0.
01  LN-WK                  PIC 9(6)     COMP  VALUE 0.
01  PRIN-WORKER            PIC S9(5)V99 COMP  VALUE 0.
01  GL-WORK                PIC S9(7)V99 COMP  VALUE 0.
01  TEST-AMT               PIC S9(7)V99 COMP  VALUE 0.
01  DISP-REV-BAL           PIC X              VALUE "Y".
01  IB-RE-TRCD             PIC XX.
```

**Transaction Buffer Structure**
```cobol
01  LN-BUF              VALUE SPACES.
    03  L-010           PIC 9(6).          /* Account Number */
    03  L-020           PIC XX.            /* Transaction Code */
    03  L-030           PIC X(5).          /* Date Field */
    03  L-040           PIC Z(5).99-.      /* Amount Field */
    03  L-050           PIC Z(4).99-.      /* Applied LC */
    03  L-060           PIC Z(4).99-.      /* Applied Interest */
    03  L-070           PIC Z(5).99-.      /* Balance Field */
    03  L-080           PIC 9(4).          /* Term/Date Field */
    03  L-090           PIC Z(5).99-.      /* Other Balance */
    03  L-100           PIC Z(4).99-.      /* Applied Other */
    03  L-110           PIC Z(5)9.99.      /* Total Field */
    03  L-120           PIC X.             /* Status Flag */
```

### File Handling

**File Access Patterns**
- **LP-FILE:** Random access by account number for loan records
- **LX-FILE:** Sequential append for transaction history
- **LXE-FILE:** Direct access for earnings updates
- **SP-FILE:** Read-only access for state parameters
- **BR-FILE:** Read-only access for branch configuration
- **GB-FILE:** Read-only access for global parameters
- **WK-FILE:** Temporary work file for complex calculations

**Database Operations**
```cobol
PERFORM SQL-CONNECT.              /* Database connection */
PERFORM OPEN-LP1-FILE.           /* Open loan file */
PERFORM READ-LP1-FILE.           /* Read loan record */
PERFORM UPDATE-LP1-FILE.         /* Update loan record */
PERFORM CLOSE-LP1-FILE.          /* Close loan file */
```

### Key Algorithms

**Late Charge Calculation Algorithm**
1. Validate late charge formula exists
2. Load payment date and first payment date
3. Calculate late charge assessment period
4. Apply configured late charge formula
5. Update late charge balance and paid through date

**Balance Distribution Algorithm**
1. Apply payments to late charges first (LP-APLC)
2. Apply remainder to interest (LP-APINT)
3. Apply remainder to other charges (LP-APOTH/LP-APOT2)
4. Calculate applied current balance (LP-APCUR)
5. Update total current balance

**Password Verification Algorithm**
1. Determine transaction type and interbranch status
2. Select appropriate password sequence
3. Prompt user for password
4. Validate against stored password
5. Set authorization flag or reject transaction

---

## Integration Points

### APIPAY Integration
- **Trigger:** APIPAY calls LONPF7 for "OT", "RE", and "O2" transaction codes
- **Data Exchange:** Transaction data passed via HOLD-BP-TRCD variables
- **Processing Mode:** Batch mode with no user interaction
- **Return Values:** Success/failure status in ERRCD field

### LONPF2 Integration
- **Purpose:** Final update processing and GL distribution
- **Trigger:** All successful LONPF7 transactions
- **Data Flow:** Updated balance information and transaction details
- **Coordination:** Ensures transactional integrity across systems

### Screen Interface Integration
- **VDU Forms:** Uses "LONPF" form family for user interface
- **Menu System:** Integrated with loan processing menu structure
- **Help System:** Context-sensitive help via F1 key
- **Navigation:** Standard function key navigation (F1-F12)

### General Ledger Integration
- **GL Distribution:** Automatic GL entry creation for transactions
- **Branch-specific:** Uses owning branch for interbranch transactions
- **Account Types:** Supports multiple GL account configurations
- **Audit Trail:** Maintains GL reference numbers for reconciliation

### Security Integration
- **Password System:** Integrated with branch security system
- **Cash Drawer:** Password protection for cash drawer access
- **Interbranch:** Special security handling for interbranch transactions
- **Audit:** All security events logged for compliance

---

## File Dependencies

### Input Files
| File | Purpose | Access Mode | Key Field |
|------|---------|-------------|-----------|
| **LP-FILE** | Loan master records | Random | LN-ACNO |
| **SP-FILE** | State parameters | Random | SP-STATE |
| **BR-FILE** | Branch configuration | Random | BR-NO |
| **GB-FILE** | Global parameters | Random | GB-BRNO |
| **CA-FILE** | Cash advance records | Random | CA-ACNO |
| **CD-FILE** | Cash drawer records | Random | CD-BRNO |

### Output Files
| File | Purpose | Access Mode | Update Type |
|------|---------|-------------|-------------|
| **LP-FILE** | Updated loan records | Random | In-place update |
| **LX-FILE** | Transaction history | Sequential | Append |
| **LXE-FILE** | Earnings records | Random | Update/Add |
| **LTI-FILE** | Transaction index | Sequential | Append |
| **LTP-FILE** | Transaction posting | Sequential | Append |

### Temporary Files
| File | Purpose | Scope |
|------|---------|-------|
| **WK-FILE** | Working calculations | Program execution |
| **Screen buffers** | VDU screen management | Session |

### External Datasets
- **GL Interface:** General ledger posting files
- **Security Files:** Password and authorization files
- **Audit Files:** Transaction audit trails
- **Configuration:** System parameter files

---

## Call Graph of PERFORMed Paragraphs

```mermaid
graph TD
    A[MAIN-PROGRAM] --> B[SQL-CONNECT]
    A --> C[GET-GPENV]
    A --> D[OPEN-SP1-FILE]
    A --> E[READ-SP1-FILE]
    A --> F[OPEN-BR-FILE]
    A --> G[READ-BR-FILE]
    A --> H[OPEN-GB-FILE]
    A --> I[READ-GB-FILE]
    A --> J{Batch Payment?}
    
    J -->|Yes| K[BATCH-PAYMENT-ROUTINE]
    J -->|No| L[SELECTION-MODULE]
    
    K --> K1[SET-BAL-IBPC-LPREC]
    K --> K2[PAID-THRU-CALCULATION]
    
    L --> L1[INITIALIZE-ROUTINE]
    L --> L2[AMOUNT-ENTRY-ROUTINES]
    L --> L3[ENTRY-LINE-CALCULATIONS]
    L --> L4[DISPLAY-ENTRY-LINE]
    L --> L5[ACCEPT-ENTRY]
    
    L1 --> M[VERIFY-PASSWORD]
    L1 --> N[SET-BAL-IBPC-LPREC]
    
    L2 --> O[TRANS-AMT-ENTRY]
    L2 --> P[APPLIED-OTH-ENTRY]
    L2 --> Q[APPLIED-INT-LC-ENTRY]
    
    L3 --> R[LATE-CHARGE-APPLY]
    
    L5 --> S[DISPLAY-ENTRY-DOWN]
    L5 --> T[DISPLAY-ENTRY-UP]
    L5 --> U[CODE-SCAN]
    
    M --> V[PASSWORD-MUST-EXIST]
    
    subgraph "File Operations"
        W[OPEN-LP1-FILE]
        X[READ-LP1-FILE]
        Y[UPDATE-LP1-FILE]
        Z[CLOSE-LP1-FILE]
    end
    
    subgraph "Screen Operations"
        AA[SAVE-SCREEN]
        BB[RESTORE-SCREEN]
        CC[VDU-CONVERT-FIELD]
        DD[SETUP-LINE]
    end
    
    R --> W
    R --> X
    L4 --> AA
    L5 --> BB
    L --> CC
    L --> DD
```

---

This comprehensive documentation provides a complete technical reference for the LONPF7 program, suitable for developers, analysts, and system administrators who need to understand, maintain, or integrate with this sophisticated loan processing system. The documentation covers all aspects from high-level business purpose to detailed technical implementation, making it accessible to readers with varying levels of COBOL and loan processing system knowledge.
