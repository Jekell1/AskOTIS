# Modernizing OTIS: From COBOL to C# - A Complete Guide

## Chapter 3: Technical Architecture of OTIS

The technical architecture of OTIS represents decades of evolutionary development in the COBOL/mainframe paradigm. Understanding this architecture in detail is essential for planning an effective modernization strategy. This chapter provides a comprehensive technical deep-dive into OTIS's program structure, data management, user interface patterns, and the critical copybooks that form the backbone of the system.

---

### COBOL Program Structure

#### The Four-Division Architecture

Every COBOL program in OTIS follows the classic four-division structure mandated by the COBOL language standard. This structured approach, while verbose by modern standards, provides clear separation of concerns and has served the mainframe community well for over five decades.

**1. IDENTIFICATION DIVISION**

The IDENTIFICATION DIVISION serves as the program's metadata header, containing:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. APIPAY.
AUTHOR. SYSTEM DEVELOPMENT TEAM.
DATE-WRITTEN. 1995-03-15.
DATE-COMPILED. 2024-10-15.
```

**Key Elements:**
- **PROGRAM-ID**: The unique identifier for the program (e.g., APIPAY, LPMENU, REGPAY). This name is used in CALL statements throughout the system.
- **AUTHOR**: Documentation of the original development team.
- **DATE-WRITTEN / DATE-COMPILED**: Timestamps indicating original creation and last compilation.

In OTIS, program names follow specific conventions:
- **LP** prefix: Loan Processing functions (LPMENU, LPCPMU, LPSMNU)
- **OP** prefix: Operations/processing functions (OPMENU)
- **PG** prefix: Program/utility functions (PGMENU)
- **REG** prefix: Registration/specific transactions (REGPAY)
- **API** prefix: Application Programming Interface utilities (APIPAY)

**2. ENVIRONMENT DIVISION**

The ENVIRONMENT DIVISION defines the program's interaction with external resources—primarily files:

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. IBM-370.
OBJECT-COMPUTER. IBM-370.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT PAYMENT-FILE ASSIGN TO 'PAYMENT.DAT'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS PAYMENT-ID
        FILE STATUS IS WS-FILE-STATUS.
        
    SELECT LOAN-FILE ASSIGN TO 'LOANMAST.DAT'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS LOAN-ACCOUNT-NO
        ALTERNATE RECORD KEY IS BORROWER-SSN
        FILE STATUS IS WS-LOAN-STATUS.
```

**Critical Concepts:**
- **SELECT statements**: Link logical file names to physical datasets
- **ORGANIZATION**: Defines file structure (INDEXED, SEQUENTIAL, RELATIVE)
- **ACCESS MODE**: Specifies how records are retrieved (SEQUENTIAL, RANDOM, DYNAMIC)
- **RECORD KEY**: Primary key for indexed file access
- **ALTERNATE RECORD KEY**: Secondary indexes for alternate access paths
- **FILE STATUS**: Two-byte status code for error handling

In OTIS, common indexed files include:
- **BRFILE**: Branch/location master file
- **PAYMENT-FILE**: Payment transaction records
- **LOANMAST**: Loan account master records
- **DEALER-FILE**: Dealer/merchant information
- **ADDON-FILE**: Add-on product records

**3. DATA DIVISION**

The DATA DIVISION is where OTIS programs define all data structures. This division has three critical sections:

**FILE SECTION (FD)**

Defines the structure of records read from or written to external files:

```cobol
DATA DIVISION.
FILE SECTION.
FD  PAYMENT-FILE.
01  PAYMENT-RECORD.
    COPY PAYMENTREC.
    
FD  LOAN-FILE.
01  LOAN-RECORD.
    COPY GBFSPR.
```

Note the extensive use of **COPY statements**—OTIS programs rarely define record structures inline. Instead, they reference copybooks (`.CPY` files) that contain standardized definitions shared across multiple programs. This promotes consistency but creates tight coupling.

**WORKING-STORAGE SECTION**

Contains variables and data structures that exist for the program's execution duration:

```cobol
WORKING-STORAGE SECTION.
01  WS-PROGRAM-NAME               PIC X(8) VALUE 'APIPAY'.
01  WS-FILE-STATUS                PIC XX VALUE SPACES.
01  WS-RETURN-CODE                PIC 9(4) COMP VALUE ZERO.
01  WS-ERROR-MESSAGE              PIC X(80) VALUE SPACES.

01  WS-DATE-FIELDS.
    05  WS-CURRENT-DATE           PIC 9(8).
    05  WS-CURRENT-TIME           PIC 9(6).
    05  WS-JULIAN-DATE            PIC 9(7).
    
01  WS-CALCULATION-FIELDS.
    05  WS-PRINCIPAL-AMOUNT       PIC 9(11)V99 COMP-3.
    05  WS-INTEREST-AMOUNT        PIC 9(9)V99 COMP-3.
    05  WS-PAYMENT-AMOUNT         PIC 9(11)V99 COMP-3.
    05  WS-REMAINING-BALANCE      PIC 9(11)V99 COMP-3.

COPY TIMEIO.
COPY LP01TRC.
COPY RESCIND.
```

**Common Patterns in OTIS WORKING-STORAGE:**
- **WS-** prefix for program-specific variables
- **COMP-3** (packed decimal) for numeric calculations to save space and improve performance
- **PIC X** for alphanumeric fields
- **PIC 9** for numeric fields with **V** indicating implied decimal point
- **VALUE** clauses for initialization
- Extensive copybook inclusion for shared structures

**LINKAGE SECTION**

Defines parameters passed from calling programs:

```cobol
LINKAGE SECTION.
01  LS-LOAN-ACCOUNT-NO            PIC X(12).
01  LS-TRANSACTION-CODE           PIC XX.
01  LS-AMOUNT                     PIC 9(11)V99 COMP-3.
01  LS-RETURN-CODE                PIC 9(4) COMP.
01  LS-ERROR-MESSAGE              PIC X(80).
```

When a program calls APIPAY, it might use:

```cobol
CALL 'APIPAY' USING LOAN-ACCOUNT-NO
                    TRANSACTION-CODE
                    PAYMENT-AMOUNT
                    RETURN-CODE
                    ERROR-MESSAGE.
```

The parameters in the USING clause map positionally to the LINKAGE SECTION items. This is a critical integration point—parameter mismatches cause runtime errors.

**4. PROCEDURE DIVISION**

The PROCEDURE DIVISION contains the executable logic, organized into paragraphs and sections:

```cobol
PROCEDURE DIVISION.
0000-MAIN-PROCESSING.
    PERFORM 1000-INITIALIZE.
    PERFORM 2000-PROCESS-TRANSACTION
        UNTIL END-OF-FILE OR ERROR-OCCURRED.
    PERFORM 9000-CLEANUP.
    STOP RUN.

1000-INITIALIZE.
    OPEN INPUT LOAN-FILE.
    OPEN I-O PAYMENT-FILE.
    MOVE 'N' TO WS-EOF-FLAG.
    MOVE ZERO TO WS-RECORD-COUNT.
    ACCEPT WS-CURRENT-DATE FROM DATE.
    PERFORM 1100-VALIDATE-PARAMETERS.

1100-VALIDATE-PARAMETERS.
    IF LS-LOAN-ACCOUNT-NO = SPACES
        MOVE 'ERROR: LOAN ACCOUNT REQUIRED' TO LS-ERROR-MESSAGE
        MOVE 8001 TO LS-RETURN-CODE
        GO TO 9000-CLEANUP
    END-IF.

2000-PROCESS-TRANSACTION.
    PERFORM 2100-READ-LOAN-RECORD.
    IF LOAN-FOUND
        PERFORM 2200-VALIDATE-TRANSACTION
        IF VALID-TRANSACTION
            PERFORM 2300-POST-PAYMENT
            PERFORM 2400-UPDATE-LOAN-RECORD
        END-IF
    ELSE
        PERFORM 2900-HANDLE-LOAN-NOT-FOUND
    END-IF.

2100-READ-LOAN-RECORD.
    MOVE LS-LOAN-ACCOUNT-NO TO LOAN-ACCOUNT-NO.
    READ LOAN-FILE
        INVALID KEY
            MOVE 'N' TO WS-LOAN-FOUND-FLAG
        NOT INVALID KEY
            MOVE 'Y' TO WS-LOAN-FOUND-FLAG
    END-READ.

2200-VALIDATE-TRANSACTION.
    EVALUATE LS-TRANSACTION-CODE
        WHEN '01'
            PERFORM 2210-VALIDATE-PAYMENT
        WHEN '02'
            PERFORM 2220-VALIDATE-REFUND
        WHEN '03'
            PERFORM 2230-VALIDATE-ADJUSTMENT
        WHEN OTHER
            MOVE 'INVALID TRANSACTION CODE' TO LS-ERROR-MESSAGE
            MOVE 8002 TO LS-RETURN-CODE
    END-EVALUATE.

9000-CLEANUP.
    CLOSE LOAN-FILE.
    CLOSE PAYMENT-FILE.
```

**Structural Patterns in OTIS Programs:**

1. **Paragraph Naming Conventions**:
   - **0000-series**: Main control logic
   - **1000-series**: Initialization and setup
   - **2000-series**: Core business processing
   - **3000-series**: Calculation routines
   - **8000-series**: Error handling
   - **9000-series**: Cleanup and termination

2. **PERFORM Statements**: OTIS uses PERFORM extensively for subroutine calls:
   - `PERFORM paragraph-name` - Execute once
   - `PERFORM paragraph-name UNTIL condition` - Loop execution
   - `PERFORM paragraph-name THROUGH ending-paragraph` - Range execution
   - `PERFORM paragraph-name n TIMES` - Fixed iteration

3. **Error Handling Patterns**:
   - FILE STATUS checking after every I/O operation
   - Return codes passed back to callers
   - Error messages in 80-character strings
   - GO TO statements for error exits (considered poor practice in modern development but common in COBOL)

4. **Call Chains**: OTIS programs frequently call other programs:
   ```cobol
   CALL 'TIMEIO' USING WS-DATE-FIELDS WS-RETURN-CODE.
   CALL 'RESCIND' USING LOAN-RECORD REBATE-AMOUNT WS-RETURN-CODE.
   ```

#### Program Types in OTIS

**1. Online Transaction Programs**
- **Purpose**: Handle real-time user interactions
- **Characteristics**: 
  - Short execution time (< 5 seconds ideal)
  - Interactive screen I/O
  - Random file access
  - Frequent program calls
- **Examples**: APIPAY (payment posting), REGPAY (payment registration), LONPF2 (loan inquiry)

**2. Batch Processing Programs**
- **Purpose**: Bulk data processing, typically overnight
- **Characteristics**:
  - Long execution time (minutes to hours)
  - Sequential file processing
  - High-volume record processing
  - Report generation
- **Examples**: EOINIT (end-of-day initialization), YINIT (year-end processing), batch report generators

**3. Menu/Navigation Programs**
- **Purpose**: Provide user interface navigation
- **Characteristics**:
  - Display screens and accept user input
  - Route to appropriate business logic programs
  - Minimal business logic
- **Examples**: LPMENU (Loan Processing Menu), OPMENU (Operations Menu), PGMENU (Program Menu)

**4. Utility Programs**
- **Purpose**: Provide reusable services
- **Characteristics**:
  - No direct user interaction
  - Called by many other programs
  - Stateless operation
- **Examples**: TIMEIO (date/time utilities), validation routines, calculation routines

---

### Data Flow and File Dependencies

Understanding how data moves through OTIS is critical for modernization planning. The system exhibits a **file-centric architecture** where programs communicate primarily through shared files rather than direct parameter passing.

#### Core Data Files

**1. LOANMAST (Loan Master File)**
- **Type**: ISAM (Indexed Sequential Access Method)
- **Primary Key**: Loan Account Number (12 characters)
- **Alternate Keys**: Borrower SSN, Loan Officer ID
- **Record Structure**: Defined by GBFSPR.CPY copybook
- **Purpose**: Central repository for all loan account information
- **Usage Pattern**: Accessed by nearly every loan-related program for read and update operations

**Key Fields:**
```cobol
01  LOAN-MASTER-RECORD.
    05  LOAN-ACCOUNT-NO           PIC X(12).
    05  BORROWER-NAME             PIC X(40).
    05  BORROWER-SSN              PIC 9(9).
    05  LOAN-TYPE-CODE            PIC XX.
    05  ORIGINAL-AMOUNT           PIC 9(11)V99 COMP-3.
    05  CURRENT-BALANCE           PIC 9(11)V99 COMP-3.
    05  INTEREST-RATE             PIC 9V9(4) COMP-3.
    05  PAYMENT-AMOUNT            PIC 9(9)V99 COMP-3.
    05  NEXT-DUE-DATE             PIC 9(8).
    05  DAYS-DELINQUENT           PIC 9(3).
    05  STATUS-CODE               PIC XX.
    05  LOAN-OFFICER-ID           PIC X(8).
    05  BRANCH-CODE               PIC X(4).
```

**2. PAYMENT-FILE (Payment Transaction File)**
- **Type**: ISAM
- **Primary Key**: Payment ID (composite: Account + Date + Sequence)
- **Record Structure**: Defined by PAYMENTREC.CPY
- **Purpose**: Records all payment transactions
- **Usage**: Written by payment posting programs, read by payment inquiry and reporting programs

**3. BRFILE (Branch Master File)**
- **Type**: ISAM
- **Primary Key**: Branch Code
- **Record Structure**: Branch information copybook
- **Purpose**: Branch/location master data
- **Usage**: Referenced for branch validation, routing, reporting

**4. DEALER-FILE (Dealer Master File)**
- **Type**: ISAM
- **Primary Key**: Dealer Number
- **Purpose**: Dealer/merchant information for indirect loans
- **Usage**: Dealer maintenance, disbursement processing

**5. ADDON-FILE (Add-on Product File)**
- **Type**: ISAM
- **Primary Key**: Add-on Code
- **Purpose**: Optional product offerings (insurance, warranties, etc.)
- **Usage**: Add-on maintenance, loan origination

#### Data Flow Patterns

**Pattern 1: Direct File Access**

Most OTIS programs access files directly:

```cobol
* Program reads loan record, updates balance, writes back
READ LOAN-FILE
    KEY IS LOAN-ACCOUNT-NO
    INVALID KEY
        PERFORM ERROR-HANDLING
END-READ.

COMPUTE CURRENT-BALANCE = CURRENT-BALANCE - PAYMENT-AMOUNT.

REWRITE LOAN-RECORD
    INVALID KEY
        PERFORM UPDATE-ERROR
END-REWRITE.
```

**Pattern 2: Work File Processing**

Batch programs often use work files for intermediate results:

```
1. Read LOANMAST sequentially
2. Apply business logic
3. Write results to WORK-FILE
4. Sort WORK-FILE
5. Read sorted WORK-FILE
6. Generate report or update master file
```

**Pattern 3: Transaction Logging**

Critical updates write to transaction logs for audit and recovery:

```
1. Read LOAN-RECORD
2. Calculate changes
3. Write BEFORE-IMAGE to AUDIT-LOG
4. Update LOAN-RECORD
5. Write AFTER-IMAGE to AUDIT-LOG
6. Write TRANSACTION-RECORD to TRANSACTION-FILE
```

**Pattern 4: Parameter-Based Communication**

Programs pass data through CALL parameters:

```cobol
* Calling program
MOVE LOAN-ACCOUNT-NO TO WS-ACCOUNT.
MOVE PAYMENT-AMOUNT TO WS-AMOUNT.
CALL 'APIPAY' USING WS-ACCOUNT
                    WS-TRANSACTION-CODE
                    WS-AMOUNT
                    WS-RETURN-CODE
                    WS-ERROR-MSG.
IF WS-RETURN-CODE NOT = ZERO
    PERFORM DISPLAY-ERROR
END-IF.
```

#### File I/O Operations in OTIS

**READ Operations**:
```cobol
* Random READ by key
READ LOAN-FILE
    KEY IS LOAN-ACCOUNT-NO
    INVALID KEY
        MOVE 'RECORD NOT FOUND' TO ERROR-MSG
    NOT INVALID KEY
        PERFORM PROCESS-LOAN-RECORD
END-READ.

* Sequential READ
READ LOAN-FILE NEXT RECORD
    AT END
        MOVE 'Y' TO EOF-FLAG
    NOT AT END
        PERFORM PROCESS-RECORD
END-READ.
```

**WRITE Operations**:
```cobol
* Write new record
WRITE PAYMENT-RECORD
    INVALID KEY
        MOVE 'DUPLICATE KEY' TO ERROR-MSG
    NOT INVALID KEY
        ADD 1 TO RECORD-COUNT
END-WRITE.
```

**REWRITE Operations** (Update existing record):
```cobol
* Must have previously READ the record
REWRITE LOAN-RECORD
    INVALID KEY
        MOVE 'UPDATE FAILED' TO ERROR-MSG
END-REWRITE.
```

**DELETE Operations**:
```cobol
* Must have previously READ the record
DELETE LOAN-FILE RECORD
    INVALID KEY
        MOVE 'DELETE FAILED' TO ERROR-MSG
END-DELETE.
```

#### Transaction Processing and Data Consistency

OTIS employs several strategies for maintaining data consistency:

**1. File Locking**
- **Automatic locking**: When a program opens a file with I-O or EXTEND mode, the file system provides record-level locking
- **Explicit locking**: Some programs use special file status codes to detect lock conflicts
- **Deadlock potential**: Programs opening multiple files in different orders can deadlock

**2. Two-Phase Updates**
```cobol
* Phase 1: Validate all changes
PERFORM VALIDATE-PAYMENT.
IF VALIDATION-FAILED
    GO TO ROLLBACK
END-IF.

* Phase 2: Apply all changes
PERFORM UPDATE-LOAN-BALANCE.
PERFORM WRITE-PAYMENT-RECORD.
PERFORM UPDATE-HISTORY.
```

**3. Compensating Transactions**
- OTIS includes reversal programs that undo previous transactions
- Example: APIPAY-REVERSAL reverses a payment posted by APIPAY

**4. Audit Trails**
Every critical update writes audit records:
```cobol
MOVE 'BEFORE' TO AUDIT-TYPE.
MOVE LOAN-RECORD TO AUDIT-BEFORE-IMAGE.
WRITE AUDIT-RECORD.

PERFORM UPDATE-LOAN-RECORD.

MOVE 'AFTER' TO AUDIT-TYPE.
MOVE LOAN-RECORD TO AUDIT-AFTER-IMAGE.
WRITE AUDIT-RECORD.
```

#### Batch vs. Online File Access

**Online Access Characteristics:**
- **Random I/O**: Direct access by key
- **Short transactions**: Complete in seconds
- **Concurrent access**: Multiple users accessing same files
- **Record locking**: Prevents update conflicts
- **Frequent commits**: Each transaction commits independently

**Batch Access Characteristics:**
- **Sequential I/O**: Process records in order
- **Long-running**: Hours of processing
- **Exclusive access**: Typically no concurrent online access during batch
- **Bulk updates**: Thousands to millions of records
- **Delayed commit**: Commit at end of batch or checkpoints

**Batch Windows**: OTIS has defined batch windows when online users are locked out:
- **Daily batch**: Midnight to 2 AM (end-of-day processing, payment posting, interest accrual)
- **Monthly batch**: Last day of month, 10 PM to 4 AM (month-end closing, statement generation)
- **Yearly batch**: December 31, 6 PM to January 1, 8 AM (year-end closing, tax reporting)

---

### Menu and Screen Navigation (Mainframe UI)

The OTIS user interface reflects the constraints and conventions of mainframe terminal systems. Understanding this navigation model is essential for designing modern replacement interfaces that preserve user workflows.

#### Master Menu Structure

OTIS employs a **hierarchical menu system** where users navigate through progressively more specific menus to reach desired functions.

**Master Menu (Entry Point)**:
```
================================================================================
                    OTIS LOAN SERVICING SYSTEM
                         MASTER MENU
================================================================================

    1.  LOAN PROCESSING
    2.  DEALER/ADDON MAINTENANCE
    3.  BATCH FUNCTIONS
    4.  SPECIAL PROCEDURES
    5.  GENERAL LEDGER
    6.  ACCOUNTS PAYABLE
    7.  COLLECTION REPORTS
    8.  SYSTEM ADMINISTRATION
    9.  EXIT

    ENTER SELECTION (1-9): ___

    F1=MASTER MENU  F7=PREVIOUS MENU  F12=EXIT
================================================================================
```

**Implementation**: The master menu is displayed by a dedicated program (often named MASTER, MAINMENU, or similar) that:
1. Displays the screen
2. Accepts user input
3. Validates the selection
4. Calls the appropriate submenu or function program
5. Returns control when the called program exits

#### Loan Processing Menu (LPMENU)

Selecting option 1 from the Master Menu displays:

```
================================================================================
                    LOAN PROCESSING MENU
================================================================================

    1.  LOAN INQUIRY
    2.  LOAN MAINTENANCE
    3.  PAYMENT POSTING
    4.  LOAN PAYOFF
    5.  LOAN RENEWAL
    6.  CLOSING BALANCE
    7.  LEDGER CARD PRINT
    8.  LOAN CHARGES
    9.  VARIABLE LOAN INQUIRY
    10. FORM REQUESTS
    11. [Additional functions]

    ENTER SELECTION: ___
    LOAN ACCOUNT: ____________

    F1=MASTER MENU  F7=BACK  F12=EXIT
================================================================================
```

**LPMENU Program Characteristics:**
- **Program Name**: LPMENU
- **Type**: Menu navigation program
- **Key Logic**:
  ```cobol
  ACCEPT MENU-SCREEN.
  EVALUATE SELECTION
      WHEN '1'
          CALL 'LONPF2' USING LOAN-ACCOUNT-NO RETURN-CODE
      WHEN '2'
          CALL 'LNMAINT' USING LOAN-ACCOUNT-NO RETURN-CODE
      WHEN '3'
          CALL 'APIPAY' USING LOAN-ACCOUNT-NO RETURN-CODE
      WHEN '4'
          CALL 'PAYOFF' USING LOAN-ACCOUNT-NO RETURN-CODE
      * ... additional options
      WHEN 'X'
          GO TO EXIT-PROGRAM
      WHEN OTHER
          MOVE 'INVALID SELECTION' TO ERROR-MESSAGE
          PERFORM DISPLAY-ERROR
  END-EVALUATE.
  ```

#### Function Key Navigation

OTIS implements standard function key conventions:

| Function Key | Purpose | Typical Behavior |
|--------------|---------|------------------|
| **F1** | Return to Master Menu | Immediate return to top-level menu regardless of depth |
| **F7** | Previous Menu/Screen | Navigate back one level |
| **F12** | Exit System | Log out of OTIS (may require confirmation) |
| **Enter** | Submit/Continue | Process input and move to next screen |
| **PF Keys** | Program-specific | Varies by program (print, search, validate, etc.) |

**Implementation in COBOL:**
```cobol
ACCEPT FUNCTION-SCREEN.

EVALUATE TRUE
    WHEN F1-PRESSED
        CALL 'MASTER'
    WHEN F7-PRESSED
        MOVE 'Y' TO GO-BACK-FLAG
        GO TO EXIT-PARAGRAPH
    WHEN F12-PRESSED
        PERFORM CONFIRM-EXIT
    WHEN ENTER-PRESSED
        PERFORM PROCESS-INPUT
    WHEN OTHER
        MOVE 'INVALID KEY' TO ERROR-MSG
END-EVALUATE.
```

#### Screen Definition and Display

OTIS screens are defined using copybooks that contain screen layouts:

**Screen Copybook Example (LPMNUI.CPY - Loan Processing Menu Input Screen)**:
```cobol
01  LOAN-PROCESSING-MENU-SCREEN.
    05  FILLER                    PIC X(24) VALUE '  LOAN PROCESSING MENU  '.
    05  FILLER                    PIC X(55) VALUE SPACES.
    05  SELECTION-FIELD.
        10  FILLER                PIC X(20) VALUE 'ENTER SELECTION: '.
        10  SELECTION-INPUT       PIC XX.
    05  LOAN-ACCOUNT-FIELD.
        10  FILLER                PIC X(20) VALUE 'LOAN ACCOUNT: '.
        10  LOAN-ACCOUNT-INPUT    PIC X(12).
    05  ERROR-MESSAGE-LINE.
        10  FILLER                PIC X(15) VALUE 'ERROR: '.
        10  ERROR-TEXT            PIC X(65).
    05  FUNCTION-KEY-LINE         PIC X(80) 
        VALUE 'F1=MASTER MENU  F7=BACK  F12=EXIT'.
```

**Display Logic:**
```cobol
PROCEDURE DIVISION.
DISPLAY-MENU.
    DISPLAY LOAN-PROCESSING-MENU-SCREEN.
    ACCEPT LOAN-PROCESSING-MENU-SCREEN.
    
    IF SELECTION-INPUT = '3'
        MOVE LOAN-ACCOUNT-INPUT TO WS-ACCOUNT-NO
        CALL 'APIPAY' USING WS-ACCOUNT-NO
                           WS-RETURN-CODE
    END-IF.
```

#### Input Validation on Screens

OTIS programs validate user input at multiple levels:

**Field-Level Validation:**
```cobol
VALIDATE-LOAN-ACCOUNT.
    IF LOAN-ACCOUNT-INPUT = SPACES
        MOVE 'LOAN ACCOUNT REQUIRED' TO ERROR-TEXT
        MOVE 'Y' TO ERROR-FLAG
        GO TO REDISPLAY-SCREEN
    END-IF.
    
    IF LOAN-ACCOUNT-INPUT NOT NUMERIC
        MOVE 'LOAN ACCOUNT MUST BE NUMERIC' TO ERROR-TEXT
        MOVE 'Y' TO ERROR-FLAG
        GO TO REDISPLAY-SCREEN
    END-IF.
```

**Business Rule Validation:**
```cobol
VALIDATE-BUSINESS-RULES.
    * Check if loan account exists
    READ LOAN-FILE
        KEY IS LOAN-ACCOUNT-NO
        INVALID KEY
            MOVE 'LOAN ACCOUNT NOT FOUND' TO ERROR-TEXT
            MOVE 'Y' TO ERROR-FLAG
    END-READ.
    
    * Check if loan is active
    IF LOAN-STATUS NOT = 'ACTIVE'
        MOVE 'LOAN IS NOT ACTIVE' TO ERROR-TEXT
        MOVE 'Y' TO ERROR-FLAG
    END-IF.
```

#### Menu-to-Function Routing

The relationship between menu programs and business logic programs follows a clear pattern:

1. **Menu program displays options**
2. **User selects an option and enters required data** (e.g., loan account number)
3. **Menu program validates the selection and required fields**
4. **Menu program calls the appropriate business logic program**, passing parameters
5. **Business logic program executes**, displays its own screens if needed
6. **Business logic program returns control** to menu program with status code
7. **Menu program displays success/error message**
8. **Menu redisplays**, awaiting next user action

**Example Flow for Payment Posting:**
```
User at Master Menu
    ↓
Selects "1" → Calls LPMENU
    ↓
LPMENU displays Loan Processing options
    ↓
User enters "3" (Payment Posting) and loan account "123456789012"
    ↓
LPMENU calls: CALL 'APIPAY' USING '123456789012' WS-RETURN-CODE
    ↓
APIPAY executes:
    - Displays payment entry screen
    - Accepts payment amount, date, method
    - Validates payment
    - Posts payment to loan account
    - Updates payment file
    - Returns to LPMENU with success code
    ↓
LPMENU displays "PAYMENT POSTED SUCCESSFULLY"
    ↓
LPMENU redisplays menu for next transaction
```

---

### Key Copybooks and Their Purposes

Copybooks are the lifeblood of OTIS. These shared code modules promote consistency but also create tight coupling and can complicate understanding of program behavior. A single program might include 10-20 copybooks, and understanding what each provides is essential.

#### TIMEIO.CPY – Standardized Time Routines

**Purpose**: Provides date and time utility functions used across the entire OTIS system.

**Contents**:
```cobol
      *****************************************************************
      * TIMEIO.CPY - DATE AND TIME UTILITY ROUTINES
      *****************************************************************
       01  TIMEIO-FIELDS.
           05  TIMEIO-CURRENT-DATE.
               10  TIMEIO-YEAR           PIC 9(4).
               10  TIMEIO-MONTH          PIC 99.
               10  TIMEIO-DAY            PIC 99.
           05  TIMEIO-CURRENT-TIME.
               10  TIMEIO-HOURS          PIC 99.
               10  TIMEIO-MINUTES        PIC 99.
               10  TIMEIO-SECONDS        PIC 99.
           05  TIMEIO-JULIAN-DATE        PIC 9(7).
           05  TIMEIO-DAYS-BETWEEN       PIC 9(5) COMP-3.
           05  TIMEIO-RETURN-CODE        PIC 9(4) COMP VALUE ZERO.
           
      *****************************************************************
      * CALL TIMEIO USING TIMEIO-FUNCTION TIMEIO-FIELDS TIMEIO-RETURN-CODE
      * 
      * FUNCTION CODES:
      *   '01' = GET CURRENT DATE AND TIME
      *   '02' = CONVERT DATE TO JULIAN
      *   '03' = CONVERT JULIAN TO DATE
      *   '04' = CALCULATE DAYS BETWEEN TWO DATES
      *   '05' = ADD DAYS TO DATE
      *   '06' = VALIDATE DATE
      *****************************************************************
```

**Usage Example**:
```cobol
* In calling program:
COPY TIMEIO.

PROCEDURE DIVISION.
    * Get current date and time
    MOVE '01' TO TIMEIO-FUNCTION.
    CALL 'TIMEIO' USING TIMEIO-FUNCTION
                       TIMEIO-FIELDS
                       TIMEIO-RETURN-CODE.
    
    * Calculate days delinquent
    MOVE '04' TO TIMEIO-FUNCTION.
    MOVE LOAN-DUE-DATE TO TIMEIO-START-DATE.
    MOVE TIMEIO-CURRENT-DATE TO TIMEIO-END-DATE.
    CALL 'TIMEIO' USING TIMEIO-FUNCTION
                       TIMEIO-FIELDS
                       TIMEIO-RETURN-CODE.
    MOVE TIMEIO-DAYS-BETWEEN TO DAYS-DELINQUENT.
```

**Why This Matters for Modernization:**
- TIMEIO is called by hundreds of programs
- Date logic is centralized, making it a prime candidate for replacement with a C# DateTime utility class
- All date calculations flow through this utility, so replacing it correctly is critical

#### GBFSPR.CPY – General Business File Structure (Loan Master Record)

**Purpose**: Defines the structure of the primary loan account master record. This is arguably the single most important copybook in OTIS—it defines the core entity around which the entire system revolves.

**Abbreviated Structure** (actual copybook is 500+ lines):
```cobol
      *****************************************************************
      * GBFSPR.CPY - LOAN MASTER RECORD STRUCTURE
      *****************************************************************
       01  LOAN-MASTER-RECORD.
           05  LMR-ACCOUNT-NUMBER        PIC X(12).
           05  LMR-ACCOUNT-TYPE          PIC XX.
               88  CONSUMER-LOAN         VALUE 'CL'.
               88  AUTO-LOAN             VALUE 'AL'.
               88  PERSONAL-LOAN         VALUE 'PL'.
               88  MORTGAGE              VALUE 'MG'.
           05  LMR-BORROWER-INFO.
               10  LMR-BORROWER-NAME     PIC X(40).
               10  LMR-BORROWER-SSN      PIC 9(9).
               10  LMR-BORROWER-DOB      PIC 9(8).
               10  LMR-BORROWER-ADDRESS.
                   15  LMR-ADDRESS-LINE1 PIC X(40).
                   15  LMR-ADDRESS-LINE2 PIC X(40).
                   15  LMR-CITY          PIC X(30).
                   15  LMR-STATE         PIC XX.
                   15  LMR-ZIP           PIC X(10).
           05  LMR-LOAN-DETAILS.
               10  LMR-ORIGINAL-AMOUNT   PIC 9(11)V99 COMP-3.
               10  LMR-CURRENT-BALANCE   PIC 9(11)V99 COMP-3.
               10  LMR-INTEREST-RATE     PIC 9V9(4) COMP-3.
               10  LMR-TERM-MONTHS       PIC 9(3).
               10  LMR-PAYMENT-AMOUNT    PIC 9(9)V99 COMP-3.
               10  LMR-PAYMENT-DUE-DAY   PIC 99.
               10  LMR-NEXT-DUE-DATE     PIC 9(8).
               10  LMR-LAST-PAYMENT-DATE PIC 9(8).
               10  LMR-LAST-PAYMENT-AMT  PIC 9(9)V99 COMP-3.
           05  LMR-STATUS-INFO.
               10  LMR-STATUS-CODE       PIC XX.
                   88  ACTIVE            VALUE 'AC'.
                   88  PAID-OFF          VALUE 'PO'.
                   88  CHARGED-OFF       VALUE 'CO'.
                   88  IN-BANKRUPTCY     VALUE 'BK'.
               10  LMR-DAYS-DELINQUENT   PIC 9(3).
               10  LMR-DELINQUENT-AMOUNT PIC 9(9)V99 COMP-3.
           05  LMR-BUSINESS-INFO.
               10  LMR-BRANCH-CODE       PIC X(4).
               10  LMR-LOAN-OFFICER-ID   PIC X(8).
               10  LMR-PRODUCT-CODE      PIC X(4).
               10  LMR-DEALER-NUMBER     PIC X(8).
           05  LMR-DATES.
               10  LMR-ORIGINATION-DATE  PIC 9(8).
               10  LMR-MATURITY-DATE     PIC 9(8).
               10  LMR-LAST-UPDATE-DATE  PIC 9(8).
               10  LMR-LAST-UPDATE-TIME  PIC 9(6).
               10  LMR-LAST-UPDATE-USER  PIC X(8).
```

**Usage**: Virtually every loan-related program includes GBFSPR.CPY:
```cobol
DATA DIVISION.
FILE SECTION.
FD  LOAN-MASTER-FILE.
01  LOAN-FILE-RECORD.
    COPY GBFSPR.

WORKING-STORAGE SECTION.
01  WS-LOAN-RECORD.
    COPY GBFSPR.
```

**Modernization Consideration:**
This copybook maps directly to a C# domain model:
```csharp
public class LoanMasterRecord
{
    public string AccountNumber { get; set; }
    public string AccountType { get; set; }
    public BorrowerInfo Borrower { get; set; }
    public LoanDetails LoanDetails { get; set; }
    public StatusInfo Status { get; set; }
    public BusinessInfo Business { get; set; }
    public DateInfo Dates { get; set; }
}
```

#### LP01TRC.CPY – Loan Processing Transaction Codes

**Purpose**: Defines standardized transaction codes used throughout the loan processing subsystem.

**Structure**:
```cobol
      *****************************************************************
      * LP01TRC.CPY - TRANSACTION CODE DEFINITIONS
      *****************************************************************
       01  TRANSACTION-CODES.
           05  TC-PAYMENT-CODES.
               10  TC-REGULAR-PAYMENT    PIC XX VALUE '01'.
               10  TC-PARTIAL-PAYMENT    PIC XX VALUE '02'.
               10  TC-PAYOFF             PIC XX VALUE '03'.
               10  TC-LATE-CHARGE        PIC XX VALUE '04'.
               10  TC-NSF-FEE            PIC XX VALUE '05'.
           05  TC-ADJUSTMENT-CODES.
               10  TC-PRINCIPAL-ADJ      PIC XX VALUE '10'.
               10  TC-INTEREST-ADJ       PIC XX VALUE '11'.
               10  TC-FEE-ADJUSTMENT     PIC XX VALUE '12'.
           05  TC-DISBURSEMENT-CODES.
               10  TC-INITIAL-DISB       PIC XX VALUE '20'.
               10  TC-ADDITIONAL-DISB    PIC XX VALUE '21'.
               10  TC-DEALER-PAYMENT     PIC XX VALUE '22'.
           05  TC-REFUND-CODES.
               10  TC-OVERPAYMENT-REFUND PIC XX VALUE '30'.
               10  TC-REBATE-REFUND      PIC XX VALUE '31'.
               10  TC-INSURANCE-REFUND   PIC XX VALUE '32'.
           05  TC-TRANSFER-CODES.
               10  TC-BRANCH-TRANSFER    PIC XX VALUE '40'.
               10  TC-ACCOUNT-TRANSFER   PIC XX VALUE '41'.
               10  TC-CLASS-TRANSFER     PIC XX VALUE '42'.
```

**Usage**:
```cobol
COPY LP01TRC.

PROCEDURE DIVISION.
    EVALUATE TRANSACTION-CODE
        WHEN TC-REGULAR-PAYMENT
            PERFORM POST-REGULAR-PAYMENT
        WHEN TC-PARTIAL-PAYMENT
            PERFORM POST-PARTIAL-PAYMENT
        WHEN TC-PAYOFF
            PERFORM PROCESS-PAYOFF
        WHEN OTHER
            MOVE 'INVALID TRANSACTION CODE' TO ERROR-MSG
    END-EVALUATE.
```

#### RESCIND.CPY / RESCINDW.CPY – Rebate and Rescission Calculations

**Purpose**: Defines data structures and calculation routines for computing rebates (interest refunds) when loans are paid off early, and for handling regulatory rescission periods (right to cancel).

**Structure (RESCIND.CPY)**:
```cobol
      *****************************************************************
      * RESCIND.CPY - REBATE CALCULATION STRUCTURES
      *****************************************************************
       01  REBATE-CALCULATION-AREA.
           05  REBATE-LOAN-AMOUNT        PIC 9(11)V99 COMP-3.
           05  REBATE-ORIGINAL-TERM      PIC 9(3).
           05  REBATE-MONTHS-PAID        PIC 9(3).
           05  REBATE-INTEREST-RATE      PIC 9V9(4) COMP-3.
           05  REBATE-METHOD-CODE        PIC X.
               88  RULE-OF-78            VALUE '1'.
               88  ACTUARIAL             VALUE '2'.
               88  PRO-RATA              VALUE '3'.
           05  REBATE-AMOUNT             PIC 9(9)V99 COMP-3.
           05  REBATE-RETURN-CODE        PIC 9(4) COMP.
```

**Calculation Program Call**:
```cobol
COPY RESCIND.

PROCEDURE DIVISION.
CALCULATE-EARLY-PAYOFF-REBATE.
    MOVE LMR-ORIGINAL-AMOUNT TO REBATE-LOAN-AMOUNT.
    MOVE LMR-TERM-MONTHS TO REBATE-ORIGINAL-TERM.
    COMPUTE REBATE-MONTHS-PAID = (LMR-ORIGINATION-DATE - CURRENT-DATE) / 30.
    MOVE LMR-INTEREST-RATE TO REBATE-INTEREST-RATE.
    MOVE '1' TO REBATE-METHOD-CODE.  * Rule of 78
    
    CALL 'RESCIND' USING REBATE-CALCULATION-AREA REBATE-RETURN-CODE.
    
    IF REBATE-RETURN-CODE = ZERO
        SUBTRACT REBATE-AMOUNT FROM PAYOFF-AMOUNT
    ELSE
        PERFORM REBATE-ERROR-HANDLING
    END-IF.
```

**Business Logic Note**: The rebate calculation is complex and highly regulated. Errors here could result in regulatory violations and financial losses. During modernization, this logic must be carefully validated against test cases.

#### LPMENU.CPY / LPMNUI.CPY – Loan Processing Menu Screen Definitions

**Purpose**: Defines the screen layout for the Loan Processing Menu.

**LPMENU.CPY** (Output screen structure):
```cobol
      *****************************************************************
      * LPMENU.CPY - LOAN PROCESSING MENU OUTPUT SCREEN
      *****************************************************************
       01  LOAN-PROCESSING-MENU-OUTPUT.
           05  LPM-TITLE-LINE.
               10  FILLER                PIC X(25) VALUE SPACES.
               10  FILLER                PIC X(30) 
                   VALUE '    LOAN PROCESSING MENU    '.
               10  FILLER                PIC X(25) VALUE SPACES.
           05  LPM-OPTION-LINES.
               10  LPM-OPTION-01         PIC X(80) 
                   VALUE '    1.  LOAN INQUIRY'.
               10  LPM-OPTION-02         PIC X(80)
                   VALUE '    2.  LOAN MAINTENANCE'.
               10  LPM-OPTION-03         PIC X(80)
                   VALUE '    3.  PAYMENT POSTING'.
               * ... additional options ...
           05  LPM-PROMPT-LINE.
               10  FILLER                PIC X(25) VALUE SPACES.
               10  FILLER                PIC X(20) VALUE 'ENTER SELECTION: '.
               10  LPM-SELECTION-FIELD   PIC XX.
           05  LPM-ACCOUNT-PROMPT.
               10  FILLER                PIC X(25) VALUE SPACES.
               10  FILLER                PIC X(20) VALUE 'LOAN ACCOUNT: '.
               10  LPM-ACCOUNT-FIELD     PIC X(12).
           05  LPM-ERROR-LINE.
               10  FILLER                PIC X(8) VALUE 'ERROR: '.
               10  LPM-ERROR-TEXT        PIC X(72).
           05  LPM-FUNCTION-KEY-LINE     PIC X(80)
               VALUE 'F1=MASTER MENU  F7=BACK  F12=EXIT'.
```

**LPMNUI.CPY** (Input screen structure):
```cobol
      *****************************************************************
      * LPMNUI.CPY - LOAN PROCESSING MENU INPUT SCREEN
      *****************************************************************
       01  LOAN-PROCESSING-MENU-INPUT.
           05  LPMI-SELECTION            PIC XX.
           05  LPMI-LOAN-ACCOUNT         PIC X(12).
           05  LPMI-FUNCTION-KEY         PIC X(2).
```

**Usage in Program**:
```cobol
COPY LPMENU.
COPY LPMNUI.

PROCEDURE DIVISION.
DISPLAY-MENU.
    DISPLAY LOAN-PROCESSING-MENU-OUTPUT.
    ACCEPT LOAN-PROCESSING-MENU-INPUT.
    
    MOVE LPMI-SELECTION TO WS-USER-SELECTION.
    MOVE LPMI-LOAN-ACCOUNT TO WS-LOAN-ACCOUNT.
    
    PERFORM PROCESS-SELECTION.
```

#### Additional Critical Copybooks

**PASSWDW.CPY – Password and Security Structures**
- Defines user authentication data
- Password encryption structures
- Security permission flags
- Access control lists

**EOINIT.CPY – End-of-Day Initialization Structures**
- Batch processing control fields
- Cutoff dates and times
- Processing flags for various batch jobs

**GBFDPR.CPY – Daily Processing Record**
- Daily transaction summary data
- Accumulation fields for daily totals
- Balance tracking for reconciliation

**File Definition Copybooks (FD sections)**:
- Individual copybooks for each major file's FD structure
- Ensures consistent file definitions across all programs accessing the file

#### Copybook Organization and Categorization

OTIS copybooks fall into several categories:

**1. Data Structure Copybooks**
- Define record layouts for files
- Examples: GBFSPR.CPY (loan master), PAYMENTREC.CPY (payment records)
- Used in: FILE SECTION and WORKING-STORAGE SECTION

**2. Screen Copybooks**
- Define screen layouts for terminal I/O
- Examples: LPMENU.CPY, PGMENU.CPY
- Used in: WORKING-STORAGE SECTION

**3. Code Definition Copybooks**
- Define constants, transaction codes, status codes
- Examples: LP01TRC.CPY (transaction codes), STATCODES.CPY (status codes)
- Used in: WORKING-STORAGE SECTION

**4. Utility Copybooks**
- Define parameter structures for utility programs
- Examples: TIMEIO.CPY, RESCIND.CPY
- Used in: WORKING-STORAGE SECTION and LINKAGE SECTION

**5. SQL Copybooks**
- Define host variable structures for SQL operations
- Examples: DCLGEN output from DB2
- Used in: WORKING-STORAGE SECTION

#### Shared vs. Specialized Copybooks

**Widely Shared Copybooks** (used by 50+ programs):
- GBFSPR.CPY (loan master record)
- TIMEIO.CPY (date/time utilities)
- LP01TRC.CPY (transaction codes)
- LPMENU.CPY (loan processing menu)
- PAYMENTREC.CPY (payment record)

**Specialized Copybooks** (used by < 10 programs):
- Specific report layouts
- Specialized calculation structures
- Niche business logic copybooks

**Modernization Strategy Implications:**
- Widely shared copybooks should become core domain models in C#
- Specialized copybooks may be folded into specific classes rather than separate modules
- Screen copybooks translate to View Models or DTO classes
- Code definition copybooks become enums or constants classes

---

### Program Dependencies and Call Hierarchies

Understanding which programs call which other programs is critical for impact analysis, testing, and migration sequencing.

#### Most Frequently Called Programs

**TIMEIO** – Date/Time Utility
- **Called by**: 100+ programs
- **Purpose**: Centralized date and time operations
- **Criticality**: HIGH - virtually every program depends on it
- **Modernization Priority**: Early - replace first with C# DateTime utility class

**Validation Utilities**
- **Called by**: 80+ programs
- **Purpose**: Input validation (SSN, account numbers, dates)
- **Criticality**: HIGH - data quality depends on these
- **Modernization Priority**: Early - create validation framework

**File Access Utilities**
- **Called by**: 60+ programs
- **Purpose**: Standardized file I/O operations
- **Criticality**: MEDIUM - improves consistency but not strictly required
- **Modernization Priority**: Mid - can be replaced with repository pattern

#### Call Chain Depth Analysis

**Shallow Call Chains** (Depth 1-2):
- Menu programs → Business logic programs
- Example: LPMENU → APIPAY
- **Modernization**: Straightforward to replicate in APIs

**Medium Call Chains** (Depth 3-4):
- Menu → Business logic → Utility → Sub-utility
- Example: LPMENU → APIPAY → RESCIND → TIMEIO
- **Modernization**: Requires careful orchestration layer design

**Deep Call Chains** (Depth 5+):
- Complex transactions with multiple layers
- Example: Batch programs calling business logic calling validation calling calculation calling utility
- **Modernization**: Prime candidates for refactoring into simpler structures

#### Example Call Hierarchies

**Payment Posting Hierarchy:**
```
LPMENU (Loan Processing Menu)
  ├─→ APIPAY (Payment Posting)
  │     ├─→ TIMEIO (Get current date)
  │     ├─→ VALACCT (Validate account)
  │     │     └─→ CKDIGIT (Check digit validation)
  │     ├─→ RESCIND (Calculate rebate if early payoff)
  │     │     └─→ TIMEIO (Date calculations)
  │     ├─→ POSTPMT (Post payment to ledger)
  │     │     └─→ GLUPDT (Update GL accounts)
  │     └─→ PRTRCPT (Print receipt)
```

**Batch End-of-Day Hierarchy:**
```
EOINIT (End-of-Day Initialization)
  ├─→ DATEVAL (Validate business date)
  │     └─→ TIMEIO (Date validation)
  ├─→ INTRCAL (Interest Calculation)
  │     ├─→ TIMEIO (Days in period)
  │     └─→ RATEUTIL (Rate lookup)
  ├─→ DELINQ (Delinquency Processing)
  │     ├─→ TIMEIO (Days past due)
  │     ├─→ NOTICEGEN (Generate delinquency notices)
  │     └─→ FEECALC (Late fees)
  │           └─→ GLUPDT (Post fees to GL)
  └─→ REPGEN (Report Generation)
        └─→ SORTUTIL (Sort reports)
```

#### Core Utility Programs

**1. TIMEIO**
- **Function**: Date and time operations
- **Dependencies**: None (leaf node)
- **Dependents**: 100+ programs
- **Complexity**: Low
- **Modernization**: Simple utility class replacement

**2. VALACCT**
- **Function**: Account number validation
- **Dependencies**: CKDIGIT (check digit algorithm)
- **Dependents**: 80+ programs
- **Complexity**: Low
- **Modernization**: Validation attribute or fluent validation

**3. RESCIND**
- **Function**: Rebate calculations
- **Dependencies**: TIMEIO
- **Dependents**: 25+ programs dealing with payoffs
- **Complexity**: High (complex financial calculations)
- **Modernization**: Requires careful unit testing against known scenarios

**4. GLUPDT**
- **Function**: General ledger update
- **Dependencies**: None
- **Dependents**: 50+ programs posting financial transactions
- **Complexity**: Medium
- **Modernization**: Integration with modern accounting system required

#### Parameter Passing Patterns

**Pattern 1: Simple Scalar Parameters**
```cobol
CALL 'TIMEIO' USING WS-DATE-FIELD WS-RETURN-CODE.
```

**Pattern 2: Copybook Structures**
```cobol
CALL 'VALACCT' USING LOAN-MASTER-RECORD WS-RETURN-CODE.
```

**Pattern 3: Multiple IN/OUT Parameters**
```cobol
CALL 'RESCIND' USING REBATE-INPUT-AREA
                    REBATE-OUTPUT-AREA
                    REBATE-RETURN-CODE
                    REBATE-ERROR-MESSAGE.
```

**Pattern 4: Reference vs. Content**
```cobol
* By reference (default) - changes visible to caller
CALL 'UPDATE' USING LOAN-RECORD.

* By content - changes not visible to caller
CALL 'CALC' USING BY CONTENT LOAN-AMOUNT BY REFERENCE RESULT.
```

#### Programs with High Copybook Dependencies

**APIPAY** (Payment Posting Program):
- Includes 15+ copybooks
- GBFSPR.CPY (loan record)
- PAYMENTREC.CPY (payment record)
- LP01TRC.CPY (transaction codes)
- TIMEIO.CPY (date utilities)
- RESCIND.CPY (rebate calculation)
- STATCODES.CPY (status codes)
- Plus numerous screen and utility copybooks

**LONPF2** (Loan Inquiry Program):
- Includes 12+ copybooks
- All the display-related copybooks for loan details
- File definition copybooks for multiple files (loan, payment, history)

#### Circular Dependencies

**Risk Area:** OTIS generally avoids circular dependencies at the program level (Program A calls Program B which calls Program A), but they can occur at the copybook level.

**Copybook Circular Reference Example:**
```
LOANREC.CPY includes STATUSREC.CPY
STATUSREC.CPY includes HISTORYREF.CPY
HISTORYREF.CPY includes LOANREC.CPY
```

These circular copybook dependencies can cause compilation issues and must be carefully analyzed during modernization.

#### Entry Point Programs (Direct User Interaction)

**Menu Programs:**
- MASTER (Master Menu)
- LPMENU (Loan Processing Menu)
- OPMENU (Operations Menu)
- PGMENU (Program Menu)

**Direct Transaction Programs:**
- Programs callable directly by users without going through menus
- Less common in OTIS but exist for power users

**Batch Entry Points:**
- EOINIT (End-of-Day)
- MONTHEND (Month-End Processing)
- YEAREND (Year-End Processing)
- Report generation programs

---

### Performance Considerations in OTIS Program Design

Performance in COBOL mainframe applications differs significantly from modern multi-tier architectures. OTIS programs were designed with specific performance constraints and optimization techniques.

#### Key Performance Factors

**1. File I/O Optimization**
- **Random vs. Sequential**: Sequential access is 10-100x faster than random
- **Buffering**: File systems buffer reads/writes for performance
- **Index Usage**: Proper key selection critical for indexed file performance

**Optimization Techniques:**
```cobol
* BAD: Random reads in loop
PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1000
    MOVE ACCOUNT-TABLE(I) TO LOAN-ACCOUNT-NO
    READ LOAN-FILE KEY IS LOAN-ACCOUNT-NO
    PERFORM PROCESS-LOAN
END-PERFORM.

* GOOD: Sort accounts, open sequential, read in order
SORT ACCOUNT-FILE ASCENDING KEY ACCOUNT-NO.
OPEN INPUT LOAN-FILE.
READ LOAN-FILE NEXT RECORD AT END...
PERFORM UNTIL EOF
    PERFORM PROCESS-LOAN
    READ LOAN-FILE NEXT RECORD AT END...
END-PERFORM.
```

**2. COMP-3 (Packed Decimal) for Calculations**
- **COMP-3** stores two digits per byte, more efficient than display format
- Arithmetic operations faster in COMP-3
- Used extensively for financial calculations

**3. Table Lookups**
```cobol
* LINEAR SEARCH - slow for large tables
SEARCH table-name
    AT END...
    WHEN condition...
END-SEARCH.

* BINARY SEARCH - requires sorted table, much faster
SEARCH ALL table-name
    AT END...
    WHEN table-key = search-value...
END-SEARCH.
```

**4. Program Size**
- Smaller programs load faster
- But too small = excessive CALL overhead
- OTIS balances with medium-sized programs (500-2000 lines)

#### Concurrent Access and Multi-User Scenarios

**Record Locking:**
```cobol
* Automatic record locking when file opened in I-O mode
OPEN I-O LOAN-FILE.

* Read locks the record
READ LOAN-FILE KEY IS LOAN-ACCOUNT-NO.

* Update and release lock
REWRITE LOAN-RECORD.
```

**Lock Conflicts:**
```cobol
READ LOAN-FILE KEY IS LOAN-ACCOUNT-NO
    INVALID KEY
        PERFORM RECORD-NOT-FOUND
    NOT INVALID KEY
        IF FILE-STATUS = '9D'  * Record locked
            PERFORM RETRY-LOGIC
        END-IF
END-READ.
```

**Deadlock Avoidance:**
- Programs should open files in consistent order
- Keep locks as short as possible
- Implement retry logic with timeouts

#### Logging and Debugging Facilities

**Trace Logging:**
```cobol
WRITE-LOG.
    MOVE FUNCTION CURRENT-DATE TO LOG-TIMESTAMP.
    MOVE WS-PROGRAM-NAME TO LOG-PROGRAM.
    MOVE WS-PARAGRAPH-NAME TO LOG-LOCATION.
    MOVE WS-MESSAGE TO LOG-TEXT.
    WRITE LOG-RECORD FROM LOG-ENTRY.
```

**Error Logging:**
```cobol
LOG-ERROR.
    MOVE 'ERROR' TO LOG-SEVERITY.
    MOVE WS-ERROR-CODE TO LOG-ERROR-CODE.
    MOVE WS-ERROR-MESSAGE TO LOG-ERROR-TEXT.
    MOVE LOAN-ACCOUNT-NO TO LOG-ACCOUNT.
    WRITE LOG-RECORD FROM ERROR-LOG-ENTRY.
```

**Debug Displays** (removed in production):
```cobol
DISPLAY 'DEBUG: ACCOUNT=' LOAN-ACCOUNT-NO.
DISPLAY 'DEBUG: BALANCE=' CURRENT-BALANCE.
DISPLAY 'DEBUG: STATUS=' WS-FILE-STATUS.
```

#### Batch Processing Schedules and Dependencies

**Daily Batch Window** (Midnight - 2 AM):
1. **EOINIT** - End-of-day initialization
2. **INTCALC** - Interest accrual
3. **DELINQ** - Delinquency processing
4. **STMTGEN** - Statement generation (if applicable)
5. **REPORTS** - Daily reports
6. **CLEANUP** - Purge temporary files

**Dependencies:**
- Step 2 depends on completion of Step 1
- Step 3 depends on Step 2
- Failures cascade - if Step 2 fails, Steps 3-6 cannot run

**Checkpoint/Restart:**
```cobol
* Write checkpoint records periodically
IF RECORD-COUNT = 1000
    MOVE CURRENT-KEY TO CHECKPOINT-KEY
    WRITE CHECKPOINT-RECORD
    MOVE ZERO TO RECORD-COUNT
END-IF.

* Restart logic
IF RESTART-FLAG = 'Y'
    READ CHECKPOINT-FILE
    MOVE CHECKPOINT-KEY TO START-KEY
    START LOAN-FILE KEY >= START-KEY
END-IF.
```

---

### Technical Constraints and Limitations

Understanding the constraints of the current architecture is essential for justifying modernization and designing the target state.

#### Architectural Limitations

**1. Monolithic Structure**
- All code runs in single address space
- No microservices isolation
- Difficult to scale individual components

**2. Synchronous Processing**
- No asynchronous operations
- Long-running operations block users
- Batch windows required for heavy processing

**3. Limited Error Handling**
- No exception handling (pre-COBOL 2002)
- Error handling via IF statements and GOTOs
- Difficult to ensure all error paths covered

**4. File-Based Integration**
- Programs communicate via shared files
- No APIs or message queues
- Tight coupling through file structures

**5. Character-Based UI**
- Fixed-format screens
- No graphics, limited formatting
- Not suitable for modern user expectations

#### Technical Debt Areas

**1. Code Duplication**
- Similar logic repeated across programs
- Copy-paste reuse rather than subroutines
- Increases maintenance burden

**2. Magic Numbers and Codes**
- Hardcoded transaction codes
- Status codes scattered throughout programs
- Difficult to understand without documentation

**3. Complex Conditionals**
- Deeply nested IFs (10+ levels in some programs)
- Multiple GOTOs creating spaghetti logic
- Hard to test and maintain

**4. Lack of Automated Testing**
- No unit testing framework
- Testing is manual and time-consuming
- Regression testing difficult

**5. Documentation Gaps**
- Inline comments sparse or outdated
- External documentation not kept in sync with code
- Tribal knowledge required to understand system

#### Modernization Implications

**What Must Be Preserved:**
- Business logic and calculations (especially financial/regulatory)
- Data integrity and audit trail capabilities
- Transaction atomicity and consistency
- Security and access control models

**What Can Be Improved:**
- User interface (modern web/mobile)
- Architecture (microservices, APIs)
- Data storage (relational/NoSQL databases)
- Integration capabilities (REST APIs, message queues)
- DevOps and deployment (CI/CD, automated testing)
- Performance and scalability (cloud elasticity)

---

### Summary

The technical architecture of OTIS reflects decades of mainframe COBOL development practices. Its strengths include:
- **Proven stability** and reliability
- **Clear program structure** mandated by COBOL divisions
- **Centralized business logic** in copybooks
- **Efficient file I/O** for batch processing

Its weaknesses for modern environments include:
- **Monolithic architecture** difficult to scale and evolve
- **Character-based UI** unsuitable for modern users
- **File-based integration** creates tight coupling
- **Limited concurrency** and asynchronous capabilities
- **Maintenance challenges** due to code complexity and documentation gaps

Understanding this architecture in depth—the program structures, data flows, menu navigation, and critical copybooks—is essential for planning a successful modernization that preserves business value while transforming technical capabilities.

In the next chapter, we will examine specific COBOL code patterns and how they translate to modern C# idioms, providing concrete examples of the transformation process.

---

*In Chapter 4, we'll dive into detailed code-level analysis, examining specific COBOL constructs and their C# equivalents, preparing you for the actual translation work ahead.*
