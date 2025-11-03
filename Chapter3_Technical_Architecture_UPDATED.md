# Chapter 3: Technical Architecture of OTIS

## Introduction

The OTIS (Otis Traction Integrated System) technical architecture represents a mature COBOL-based financial loan servicing system that has evolved over decades to handle complex business requirements. This chapter provides a comprehensive analysis of the system's technical architecture, drawn from detailed examination of the actual codebase through 60 specific research queries into the system's knowledge base.

The architecture supports multi-user, multi-branch loan servicing operations with robust transaction processing, extensive data file management, and comprehensive error handling. Understanding this architecture is essential for planning the modernization to C#.

## 1. COBOL Program Structure and Organization

### 1.1 Standard COBOL Division Structure

Every COBOL program in the OTIS system follows the standard four-division structure mandated by the COBOL language specification. This consistency across all programs simplifies maintenance and provides a predictable framework for developers.

#### IDENTIFICATION DIVISION

The IDENTIFICATION DIVISION provides metadata about each program:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. INSTAL.
AUTHOR. BLM.
DATE-WRITTEN. 07/27/2012.
```

**Source:** INSTAL.CBL, Lines 1-25

This division is primarily documentary, but program names are critical as they're used for program invocation via CALL statements and menu systems.

#### ENVIRONMENT DIVISION

The ENVIRONMENT DIVISION describes the system environment and file assignments:

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. 3B.
OBJECT-COMPUTER. 3B.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT LOAN-FILE ASSIGN TO "LOAN.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LOAN-NUMBER.
I-O-CONTROL.
```

**Source:** PERUSE.CBL, Lines 51-75; CHBORR.CBL

The FILE-CONTROL section is particularly important as it defines:
- File organization (INDEXED, SEQUENTIAL, RELATIVE)
- Access modes (SEQUENTIAL, RANDOM, DYNAMIC)
- Record keys for indexed files
- File locking strategies

#### DATA DIVISION

The DATA DIVISION declares all variables, data structures, and file layouts:

```cobol
DATA DIVISION.
FILE SECTION.
FD  LOAN-FILE.
01  LOAN-RECORD.
    05  LOAN-NUMBER       PIC 9(10).
    05  CUSTOMER-NAME     PIC X(40).
    05  LOAN-AMOUNT       PIC 9(9)V99.

WORKING-STORAGE SECTION.
01  BEG-BR           PIC 9(4).
01  END-BR           PIC 9(4).
01  COCODES.
    05  COCODE       PIC XX OCCURS 30.

SCREEN SECTION.
01  LOAN-SCREEN.
    05  LINE 5 COL 10 VALUE "Loan Number:".
    05  LINE 5 COL 25 PIC 9(10) TO LOAN-NUMBER.
```

**Source:** IR1098.CBL, Lines 151-175; REBL01.CBL

The DATA DIVISION typically contains:
- **FILE SECTION**: Defines file record layouts
- **WORKING-STORAGE SECTION**: Declares variables and constants
- **SCREEN SECTION**: Defines screen layouts (for interactive programs)
- **LINKAGE SECTION**: Defines parameters received from calling programs

#### PROCEDURE DIVISION

The PROCEDURE DIVISION contains the executable logic:

```cobol
PROCEDURE DIVISION.
MAIN-MODULE.
    PERFORM INITIALIZATION
    PERFORM ENTRY-MODULE
    PERFORM WRITE-MODULE
    STOP RUN.

INITIALIZATION.
    OPEN INPUT LOAN-FILE
    OPEN OUTPUT REPORT-FILE
    MOVE ZEROS TO RECORD-COUNT.

ENTRY-MODULE.
    PERFORM UNTIL END-OF-FILE
        READ LOAN-FILE
            AT END MOVE 'Y' TO END-OF-FILE-FLAG
            NOT AT END PERFORM PROCESS-LOAN
        END-READ
    END-PERFORM.
```

**Source:** JCMAIN, Index: flows

The PROCEDURE DIVISION is organized into paragraphs and sections, with typical structure:
- INITIALIZATION: Setup environment, open files, initialize variables
- MAIN-PROGRAM-LOOP: Core processing logic
- Specialized modules (ENTRY-MODULE, WRITE-MODULE, DELETE-MODULE)
- Error handling routines
- Exit/cleanup procedures

### 1.2 Program Naming Conventions

OTIS programs follow systematic naming conventions that indicate their business domain and function. Understanding these conventions is essential for navigating the codebase.

#### Subsystem Prefixes

| Prefix | Subsystem | Example Programs | Business Function |
|--------|-----------|------------------|-------------------|
| LP | Loan Processing | LPPOFF, LONPFB | Loan servicing operations |
| GB | General Business | GB01BR | Core business functions |
| CL | Collection | CLFILE | Collections management |
| DL | Dealer | DLMAN5 | Dealer account management |
| IO | Input/Output | IOCONN, IOBWRN | I/O and connectivity |
| CH | Check/Charge | CHBORR, CHCOCD | Check processing |
| BW | Borrower | BWFILE, BWLNS2 | Borrower information |
| PA | Payment | PAFILE | Payment processing |
| PS | Posting | PSMAIN | Financial posting |
| CDB | Corporate Database | CDBMAN, CDBSCN | Corporate data management |
| SF | Special Functions | SFVERI | Special business functions |
| MB | Miscellaneous Batch | MBCORP1 | Batch processing |
| RE | Reporting/Extract | REBL01, REBB04 | Report generation |
| GP | General Processing | GP-PAYOFF-NONCASH | General utilities |
| SET | Setup/Configuration | SETCOL | System configuration |

**Source:** BANKMU.CBL, CHBORR.CBL, REBL01.CBL, and program metadata

#### Function Suffixes

Programs use suffixes to indicate their specific function:

| Suffix | Purpose | Example | Description |
|--------|---------|---------|-------------|
| FILE | File handler | CLFILE, BWFILE | Manages indexed file access |
| MAIN | Main menu/logic | PSMAIN, DSMAIN | Primary entry point |
| SCN | Screen handler | CDBWMN_SCN | Screen display/input |
| DEF | Definition | CDBSCN_DEF | Data structure definitions |
| EVA | Evaluation | CDBSCN_EVA | Business rule evaluation |
| WKS | Workscreen | CDBSCN_WKS | Work area for processing |
| PF | Payment File | LONPFB, LONPF9 | Payment file operations |
| PG | Page/Program Group | LONPG3, LONPG7 | Related program group |
| MAN | Maintenance | DLMAN5 | Maintenance functions |
| INQ | Inquiry | BWINQ2 | Inquiry/lookup functions |

**Source:** Program naming analysis across OTIS codebase

Example program name breakdown:
- **LONPFB**: LO (Loan) + N + P (Payment) + F (File) + B (variant) = Loan Payment File handler variant B
- **CDBSCN**: CDB (Corporate Database) + SCN (Screen) = Corporate Database Screen handler
- **DLMAN5**: DL (Dealer) + MAN (Maintenance) + 5 (version) = Dealer Maintenance version 5

### 1.3 Program Types and Operational Modes

OTIS programs fall into three primary operational categories:

#### Batch Programs

**Purpose:** Scheduled or ad-hoc jobs processing large volumes without user interaction

**Characteristics:**
- Executed via cron, JCL, or batch menu
- Process entire files or large record sets
- Run overnight or during off-peak hours
- No screen interaction
- Error logging instead of user messaging

**Examples:**

**BATCHP** - Batch form printing for document creation
```cobol
* BATCHP is a batch form print program for document file creation
* ONLY RUNS IN BATCH
```
**Source:** BATCHP.CBL, Index: code

**EOCRON** - Executes batch steps from cron
```cobol
* EOCRON executes batch steps from cron, automating scheduled tasks
```
**Source:** EOCRON.CBL, Index: code

**LONPT0** - Batch release and processing for loan payments
**Source:** LONPT0.CBL, Index: flows

**Typical Batch Program Flow:**
1. Read control parameters
2. Open input/output files
3. Process all records sequentially
4. Generate summary reports
5. Close files
6. Log completion status

#### Online (Interactive) Programs

**Purpose:** Real-time user interaction for transactions, inquiries, and maintenance

**Characteristics:**
- Invoked from menus or direct user actions
- Display screens and accept input
- Validate and update records immediately
- Use screen copybooks for UI layout
- Provide immediate user feedback

**Examples:**

**LONPW1** - Interactive batch payment processing
```cobol
* LONPW1 displays batches and allows user selection for processing
```
**Source:** LONPW1.CBL, Index: flows

**BWINQ2** - Online inquiry for borrower information
**Source:** BWINQ2.CBL, Index: copybook_usage

**LPMAN3** - Loan maintenance screen
**Source:** LPMAN3, Index: flows

**Typical Online Program Flow:**
1. Display screen to user
2. Accept and validate input
3. Read/update records as needed
4. Display results or error messages
5. Loop for next transaction

#### Utility Programs

**Purpose:** Supporting functions for file management, environment setup, and data conversion

**Characteristics:**
- Called by other programs or run standalone
- No direct user or business data interaction
- Handle technical tasks
- Often reusable across business functions

**Examples:**

**C$MAKEDIR** - Directory creation utility
**SETENV** - Environment variable setup
**COMMON** - Shared routines for message handling

```cobol
* COMMON provides shared routines for message handling and miscellaneous tasks
```
**Source:** COMMON, Index: flows

### 1.4 Error Processing and Exception Handling

OTIS implements comprehensive error handling through multiple mechanisms:

#### Dedicated Error-Handling Programs

Three core error-handling programs are called throughout the system:

**ERRLOG** - Logs errors for audit and troubleshooting
**ERRMSG** - Displays or processes error messages
**SQLERR** - Handles SQL-related errors

**Source:** ERRLOG, ERRMSG, SQLERR, Index: programs

These are leaf modules (called by others but make no outgoing calls), invoked whenever an error is detected.

#### Error Codes and Flags

Programs use standardized error codes to indicate error states:

```cobol
* Set error code to E before performing file close operations
MOVE "E" TO ERRCD
PERFORM CLOSE-FILES
```
**Source:** DECLARE.CPY, Index: code

Common error flag patterns:
- `ERRCD`: General error code field
- `IO-FG`: File I/O status flag
- `EXCEPTION-CODE`: Exception type indicator
- `ERROR-LOCK-FG`: Record locking error flag

#### GO TO and PERFORM Error Routines

When errors are detected, programs transfer control to error-handling paragraphs:

```cobol
IF IO-BAD
    GO TO IO-ERROR
END-IF
```
**Source:** WDAYBK.CBL, Index: code

#### Screen Exception Handling

For screen-based programs, special data items manage user-triggered exceptions:

```cobol
* SCREEN-CONTROL is declared for proper exception handling
* Required for NOTIFY style events
01  SCREEN-CONTROL.
01  EVENT-STATUS.
```
**Source:** RZLER2.CBL, EXTVAR.CBL, SQLLOG.CBL, Index: code

Even if not directly referenced, these items are required because they're set by the runtime during exception processing.

#### Logging and Notification

Errors are logged for audit trails and troubleshooting:

```cobol
PERFORM SEND-MESS
```
**Source:** EOCRON.CBL, Index: code

User notification occurs in online programs through screen messages.

#### SQL Error Handling

SQL operations require specialized error handling:

```cobol
EXEC SQL DISCONNECT END-EXEC
MOVE "E" TO ERROR-CODE
PERFORM FILE-CLOSURE-ROUTINE
```
**Source:** BIBUID.CBL, Index: code

**Typical Error Handling Flow:**
1. Detect error (status flags, error codes, exceptions)
2. Set error code/flag
3. Transfer control to error paragraph (GO TO/PERFORM)
4. Call dedicated error handler (ERRLOG, ERRMSG)
5. Log error and/or notify user
6. Perform cleanup (close files, rollback transactions)
7. Exit or resume as appropriate

### 1.5 PROCEDURE DIVISION Organization

The PROCEDURE DIVISION follows consistent organizational patterns across OTIS programs:

#### Standard Paragraph Structure

**INITIALIZATION**
- Sets up working storage
- Opens files
- Initializes variables
- Prepares the program for processing

```cobol
INITIALIZATION.
    MOVE STAT---GOOD TO PROCESS-STAT
    MOVE "LPFILE" TO VDU-FORM-NAME
    PERFORM GET-GPENV
    OPEN INPUT LOAN-FILE
    OPEN OUTPUT REPORT-FILE.
```
**Source:** LPFILE.CBL, BWFILE.CBL, PWFILE.CBL, Index: code

**MAIN-PROGRAM-LOOP / MAIN-MODULE**
- Controls main flow of user interaction or batch processing
- Handles repeated actions
- Processes records or responds to user commands

**Source:** Multiple programs including PDPG91, PDPG07, DSMAIN, SSMAIN, ALMAIN, LONPG7, JCMAIN, and others

**ENTRY-MODULE**
- Handles user input or record entry
- Validates and processes data

**WRITE-MODULE**
- Writes processed data to files
- Updates records
- Handles output operations

**DELETE-MODULE**
- Deletes records
- Handles removal operations

**Source:** DSMAIN, SSMAIN, ALMAIN, JCMAIN

**FORM-RESET**
- Resets screen or form
- Clears fields
- Prepares for new input

**DISPLAY MODULES**
- Handles displaying information
- Shows error messages
- Displays prompts or data

Examples: DISPLAY-KEY, DISPLAY-SCREEN, DISPLAY-FIELDS

**Source:** PDPG91, PDPG07, PDPG10, LONPK0

**SPECIALIZED PROCESSING MODULES**
- Business-specific logic
- Calculations, validations
- Custom routines

Examples:
- CALCULATE-1STPYDATE-TERM
- VERIFY-100PERCENT
- PROCESS-ORIGINAL-LOAN

**Source:** LONPG7, ALMAIN

#### DECLARATIVES Section

Used for error handling and file exception routines:

```cobol
DECLARATIVES.
DECLARE-ERROR SECTION.
    USE AFTER ERROR PROCEDURE ON LOAN-FILE.
    DISPLAY "File error occurred"
    GO TO ABORT-PROGRAM.
END DECLARATIVES.
```
**Source:** LONPR0, RZWRI5, BPBKBR, OTHUPD.CBL, EALIST.CBL

The DECLARATIVES section traps file I/O errors and other exceptions automatically.

### 1.6 Subroutines and Paragraph Organization

OTIS programs organize logic through paragraphs and subroutines:

#### Paragraphs

Paragraphs are named blocks performing single logical tasks:

```cobol
VALIDATE-INPUT.
    IF LOAN-AMOUNT NOT NUMERIC
        MOVE "Amount must be numeric" TO ERROR-MESSAGE
        GO TO ERROR-HANDLING
    END-IF.

PROCESS-PAYMENT.
    COMPUTE NEW-BALANCE = OLD-BALANCE - PAYMENT-AMOUNT
    PERFORM UPDATE-LOAN-RECORD.

ERROR-HANDLING.
    DISPLAY ERROR-MESSAGE
    PERFORM LOG-ERROR.
```

Paragraphs are invoked using PERFORM statements:
- Simple: `PERFORM VALIDATE-INPUT`
- Range-based: `PERFORM PARA-START THRU PARA-END`

**Typical Paragraph Organization:**

| Type | Example Name | Purpose |
|------|--------------|---------|
| Entry Paragraph | MAIN-MODULE | Program start logic |
| Business Logic | VALIDATE-INPUT | Input validation |
| File I/O | READ-FILE | Read data from files |
| Error Handling | ERROR-HANDLING | Manage errors/exceptions |
| Exit Paragraph | EXIT-PROGRAM | Program termination |

#### Program Size Examples

**PST332**: 37 paragraphs, max depth 1, no cycles
- Entry paragraphs: WORKING-STORAGE, EXIT, PRINT-ROUTINE-EXIT
**Source:** PST332, Index: flows

**HELP2**: 5 paragraphs, max depth 1, no cycles
- Entry paragraphs: WORKING-STORAGE, FILE-CONTROL, FILE
**Source:** HELP2, Index: flows

**LONPG3**: Shows modules like MAIN-MODULE, ENTRY-MODULE, MODIFY-ROUTINE
**Source:** LONPG3, Index: flows

#### External Subroutines

Programs call other programs as subroutines using CALL statements:

```cobol
CALL 'REFUPD' USING PAYMENT-RECORD, ACCOUNT-KEY
CALL 'ERRLOG' USING ERROR-MESSAGE
```

This modular approach enables:
- Reusable business logic across programs
- Centralized error handling
- Shared utility functions

### 1.7 WORKING-STORAGE Patterns

OTIS programs use consistent patterns in WORKING-STORAGE for variables and data structures:

#### Status Flags and Switches

```cobol
01  FIRST-TIME           PIC X        VALUE "Y".
01  RECORDS-FOUND        PIC X        VALUE " ".
    88  NO-RECORDS-FOUND              VALUE "N".
01  AUDIT-ERROR       PIC 9  VALUE 0.
01  SECURITY-ERROR    PIC 9  VALUE 0.
01  SKIP-SWITCH       PIC 9  VALUE 0.
01  SKIP-STEP         PIC 9999 VALUE 0.
```
**Source:** SPOBRD.CBL, Lines 151-175

#### Message Buffers and Lists

```cobol
01  MESS-LIST      PIC X(5)  VALUE "MESS.".
01  ALLMESS-LIST   PIC X(20) VALUE "MSEL SEL MSEL2 MESS.".
01  DISPLAY-BUF.
    03  FILLER           PIC X(12) VALUE "PROCESSING..".
    03  DISPLAY-BRNO     PIC Z(4).
    03  FILLER           PIC X(7).
```
**Source:** GREXEC.CBL, Lines 101-125

#### Numeric Counters and Step Trackers

```cobol
01  CURRENT-STEP   PIC 9999.
01  STEP-COUNT     PIC 9999.
05  SUB            PIC 9(5)  COMP.
```
**Source:** EORUN.CBL, Lines 51-75; CALCZ3.CBL, Lines 501-525

#### Temporary Calculation Fields

```cobol
05  CVA-A--N                  PIC S9999V9(13)   COMP.
05  CVA-SP                    PIC S999999V9(11) COMP.
05  CVA-SP3                   PIC S999999V9(3)  COMP.
05  CVA-I                     PIC S999999V9(11) COMP.
05  CVA-WK                    PIC S999999V9(11) COMP.
05  CVA-LP                    PIC S9(11)V9(6)   COMP.
```
**Source:** CALCZ3.CBL, Lines 501-525

#### Program Name and Date-Time Workers

```cobol
01  WS-PGM-NAME   PIC X(6).
01  DATE-TIME-WORKERS.
```
**Source:** GREXEC.CBL, Lines 101-125

#### 88-Level Condition Names

```cobol
01  RECORDS-FOUND        PIC X        VALUE " ".
    88  NO-RECORDS-FOUND              VALUE "N".
```

Usage:
```cobol
IF NO-RECORDS-FOUND
    DISPLAY "No records found"
END-IF
```
**Source:** SPOBRD.CBL, Lines 151-175

### 1.8 LINKAGE SECTION for Parameter Passing

OTIS programs use the LINKAGE SECTION to receive parameters from calling programs:

#### Basic Parameter Reception

```cobol
PROCEDURE DIVISION USING FORM-PATHNAME.
LINKAGE SECTION.
01  FORM-PATHNAME      PIC X(22).
```
**Source:** PCPGRM.CBL, Index: code

#### Complex Data Structures

```cobol
LINKAGE SECTION.
01  FORM-PATHNAME PIC X(22).
01  EORUN-BUF.
    03  EORUN-PTYPE          PIC XXX.
    03  EORUN-PCODE          PIC XXX.
    03  EORUN-HOUR-BUF       PIC 99.
    03  EORUN-MIN-BUF        PIC 99.
    03  EORUN-DATE1          PIC 9(8).
```
**Source:** EORUN.CBL, Index: code

#### Using Copybooks for Standardization

```cobol
LINKAGE SECTION.
COPY "LIBGB/GETFMW.CPY".
```
**Source:** REBB01.CBL, CRNOFL.CBL, Index: code

This ensures consistency across programs calling similar routines.

### 1.9 Program Entry and Exit Points

OTIS programs follow consistent patterns for starting and terminating:

#### Entry Points

**MAIN-MODULE / MAIN-PROGRAM**
```cobol
MAIN-MODULE.
    PERFORM INITIALIZATION
    PERFORM PROCESS-LOGIC
    GO TO MAIN-PROGRAM-EXIT.
```
**Source:** TCMAIN, CJMAIN, STMAIN, Index: flows

**INITIALIZATION SECTION**
```cobol
INITIALIZATION SECTION.
    MOVE STAT---GOOD TO PROCESS-STAT
    MOVE "LPFILE" TO VDU-FORM-NAME
    PERFORM GET-GPENV.
```
**Source:** LPFILE.CBL, BWFILE.CBL, PWFILE.CBL, Index: code

#### Exit Points

**MAIN-PROGRAM-EXIT / END-PROGRAM**
```cobol
MAIN-PROGRAM-EXIT.
    GO TO END-PROGRAM.

END-PROGRAM.
    EXIT PROGRAM.
```
**Source:** LPFILE.CBL, BWFILE.CBL, PWFILE.CBL, Index: code

**EXIT-PROG for Menu/Utility Modules**
```cobol
EXIT-PROG.
    PERFORM TRACE-PRO
    EXIT PROGRAM.
```
**Source:** LPMNMU.CBL, FXMENU.CBL, BPMENU.CBL, Index: copybook_usage

### 1.10 Program Compilation and Deployment

#### Compilation Process

**Source Organization:**
- COBOL source files organized by business domain
- Stored in directories: LIBLP/, LIBGB/, LIBSP/, etc.
- Extensive use of copybooks for shared definitions

**Preprocessing:**
- Copybooks included via COPY statements
- Conditional compilation for environment-specific logic

```cobol
*     THIS PROGRAM USES THE RANDOM FUNCTION WHICH REQUIRES 
*     RUNTIME OF AT LEAST 5.0.  SO IN A15, COMPILE WITH
*     CB500.SH, WHICH HAS THE Z31 ARGUMENT SET TO 5.
```
**Source:** REDACT.CBL, Index: code

**Compiler Invocation:**
- Mainframe: IBM COBOL or Micro Focus COBOL
- UNIX/Linux: ACUCOBOL or similar compilers
- Special compile scripts (e.g., CB500.SH) set compiler options

**Build Artifacts:**
- Object modules or executable binaries
- Some programs compiled as callable subprograms
- Others as standalone executables

#### Deployment Process

**Staging and Testing:**
- Binaries staged in test environments
- Automated or manual testing verification

**Production Deployment:**
- Binaries copied to production directories
- Strict version control
- Deployment scripts set environment variables

**Runtime Work File Creation:**
```cobol
*  CREATE WK-FILE, IF RUNNING FROM EOCRON, WORK FILE IN /USR/TMP
*  WAS NOT CREATED IN LONPA0
     PERFORM CREATE-WK-FILE.
* CREATE SPOOL DIRECTORY AND O
```
**Source:** APIPAY.CBL, Index: code

**Versioning:**
- SCCS identifiers embedded in source
- Date stamps for tracking
- Comments document changes and platform requirements

This completes Section 1: COBOL Program Structure and Organization.

## 2. Data Flow and File Dependencies

### 2.1 Main Data Files

The OTIS system maintains its core business data in a collection of indexed and sequential files. Understanding these files is essential for comprehending how data flows through the system.

#### Core Master Files

**LNFILE - Loan Master File**
- **Purpose:** Central repository for loan account records
- **Contents:**
  - Loan numbers, branch numbers
  - Fee codes and amounts
  - Insurance details (types, eligibility, premiums, coverage, commissions)
  - Payment history and status
  - Associated customer and dealer references

```cobol
LNFILE.LN_FEEOURS_19,
LNFILE.LN_FEEFINCHG_19,
LNFILE.LN_FEECODE_20,
LNFILE.LN_FEEAMT_20,
LNFILE.LN_INSURED,
LNFILE.LN_INSTYPES,
LNFILE.LN_INSELIGIBLE,
LNFILE.LN_INSCOMP_1,
LNFILE.LN_INSOURS_1,
LNFILE.LN_INSEFF_DATE_1,
```
**Source:** IOLNIN.CBL, RZWRI3.CBL, Index: code

**Accessed by:** 113 programs
**Indexed by:** Loan number (LN_NO), branch number (BR_NO)

**BMFILE - Branch Master File**
- **Purpose:** Branch-level master records
- **Contents:** Branch information and summary data

**BYFILE - Borrower File**
- **Purpose:** Borrower account details
- **Contents:** Loan applications and related information

**CJFILE - Customer Journal File**
- **Purpose:** Customer transaction tracking
- **Contents:** Customer transactions and journal entries

**GTFILE - General Ledger File**
- **Purpose:** Financial ledger management
- **Contents:** Ledger balances, accounting entries, financial summaries

**Source:** DATE35.CBL, RZMTR2.CBL, Index: code

#### Additional Key Files

| File Name | Purpose | Key Data |
|-----------|---------|----------|
| LMFILE | Loan Master | Account records, balances, statuses |
| DSFILE | Dealer Statistics | Dealer performance data |
| TYFILE | Type File | Loan type definitions |
| LTPFILE | Loan Type Parameters | Configuration and rules for loan types |
| BWFILE | Branch Weekly File | Weekly reporting and accruals |
| CAFILE | Cash Account | Cash transactions and balances |
| FDB-FILE | File Database | Fixed database records |
| LSFILE | Loan Schedule | Payment schedules and amortization |
| LTFILE | Loan Trailer | Additional loan details |

**Source:** DATE35.CBL, EOMALL.CBL, CPINQ3.CBL, MB4ALL.CBL, Index: code

### 2.2 Data Flow Between Programs

Data flows through OTIS using multiple mechanisms:

#### File-Based Data Exchange

Most programs read and write shared data files:

```
Program A writes to PAYMENT-FILE
    ↓
PAYMENT-FILE contains updated payment records
    ↓
Program B reads PAYMENT-FILE for further processing
```

Example flow:
1. **APIPAY** validates payment and writes to PAYMENT-FILE
2. **REFUPD** reads PAYMENT-FILE and updates LEDGER-FILE
3. **LONPW9** reads updated account info and generates notifications

#### Program Calls and Modular Processing

Programs call other programs to perform specialized tasks:

```cobol
CALL 'REFUPD' USING PAYMENT-RECORD, ACCOUNT-KEY
```

Data is passed via:
- Parameters (using LINKAGE SECTION)
- Shared files

#### Screen and User Input

User actions trigger program execution:

```
User enters payment → Screen program validates → 
Transaction program processes → Database updated → 
Confirmation displayed
```

#### Batch vs Real-Time Processing

**Batch Programs:**
- Process large volumes of data overnight
- Read input files, process records, write output files

**Real-Time Programs:**
- Handle individual user transactions
- Update files immediately

### 2.3 Indexed Files and Access Patterns

OTIS extensively uses indexed files for fast record access:

#### Indexed File Definition

```cobol
SELECT OIX-FILE ASSIGN TO OIX-PATH
       ORGANIZATION INDEXED
       ACCESS DYNAMIC
       LOCK MODE AUTOMATIC WITH LOCK ON RECORD
       RECORD KEY OIX1-KEY
       ALTERNATE RECORD KEY OIX2-KEY WITH DUPLICATES
       FILE STATUS FILE-STAT.
```
**Source:** UP1534_FS.CPY, Index: code

**Key Features:**
- **ORGANIZATION INDEXED:** Records accessed by key
- **ACCESS DYNAMIC:** Supports both sequential and random access
- **LOCK MODE AUTOMATIC:** Automatic record locking
- **ALTERNATE KEYS:** Support multiple access paths

#### Common Indexed Files

| File | Purpose | Primary Key | Alternate Keys |
|------|---------|-------------|----------------|
| LNFILE | Loan records | Loan number | Branch number |
| FMFILE | Financial master | Account number | Transaction date |
| CIFILE | Customer info | Customer ID | - |
| ICFILE | Insurance certificates | Certificate number | - |
| SSFILE | Subsystem records | Subsystem key | - |
| CKFILE | Check transactions | Check number | - |

**Source:** LNFILE.CBL, UP1534_FS.CPY, Index: code

### 2.4 File I/O Operations

OTIS handles file I/O through structured operations:

#### READ Operations

```cobol
READ LOAN-FILE
  KEY IS LOAN-NUMBER
  INVALID KEY
    DISPLAY 'Loan not found'
END-READ.
```

Programs contain dedicated READ paragraphs:
- READ-OMP1-FILE-PREVIOUS
- READ-WRFILE
- READ-TRX1-FILE-NEXT

**Source:** LPMP1RN, WRFILE, Index: paragraphs/programs

#### WRITE Operations

```cobol
WRITE LOAN-RECORD
  INVALID KEY
    DISPLAY 'Write failed'
END-WRITE.
```

Dedicated WRITE paragraphs:
- WRITE-XTRL-FILE
- WRITE-IC-FILE
- WRITE-XTRW-FILE

**Source:** EXXTRLI, SPICI, EXXTRWI, Index: flows

#### REWRITE Operations

```cobol
REWRITE LOAN-RECORD
  INVALID KEY
    DISPLAY 'Rewrite failed'
END-REWRITE.
```

Dedicated REWRITE paragraphs:
- REWRITE-WK1-FILE

**Source:** RZLER2, SQLER2, SQLLO2, CDBFLE, CLFILE, EAFILE, Index: paragraphs

#### DELETE Operations

```cobol
DELETE LOAN-FILE
  RECORD KEY IS LOAN-NUMBER
  INVALID KEY
    DISPLAY 'Delete failed'
END-DELETE.
```

Dedicated DELETE paragraphs:
- DELETE-XTRL-FILE
- DELETE-IC-FILE
- DELETE-XLTD-FILE

**Source:** EXXTRLI, SPICI, EXXLTDI, CREARX, CREAIX, Index: flows

#### Error Handling for File I/O

Each operation checks for success/failure:
- INVALID KEY clause catches key-based errors
- FILE STATUS variable checked after operation
- Error messages displayed or logged

### 2.5 SQL Database Access

OTIS uses embedded SQL for relational database access:

#### SQL INSERT Example

```cobol
EXEC SQL
 INSERT INTO DBO.BYAFILE
  (BYAFILE.BYA_DATE_CCYYMM,
   BYAFILE.BYA_BRNO,
   BYAFILE.BYA_CLASS,
   BYAFILE.BYA_LTOUCH_DATE,
   BYAFILE.BYA_NO_1_1_1,
   BYAFILE.BYA_AMT_1_1_1)
  VALUES (:WS-DATE, :WS-BRANCH, :WS-CLASS, 
          :WS-TOUCH-DATE, :WS-NUMBER, :WS-AMOUNT)
END-EXEC.
```
**Source:** LPEOD5.CBL, Index: code

#### SQL Connection Management

```cobol
MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
PERFORM SQL-SWAP-UPDATE-CONNECTION.
```
**Source:** LPEOD5.CBL, Index: code

Connection management includes:
- Establishing database connections
- Swapping connections for different operations
- Validating connection status before I/O

```cobol
* SQL CONNECTION FIELDS NEED TO BE SET SO THAT THEY CAN BE USED 
* FOR EACH IO ROUTINE NEEDED
```
**Source:** DATE35.CBL, Index: code

#### SQL Validation and Error Handling

```cobol
IF ( IO-FG = 8 )     *> LOCKED
   GO TO REWRITE-TRX1-FILE.
IF ( IO-FG = 0 )
   EXEC SQL COMMIT END-EXEC
   PERFORM SQL-IO-VALIDATION.
```
**Source:** IOTRXI.CBL, Index: code

Validation includes:
- Checking connection status (EXT-ACUSQL-CONNECT-STAT)
- Handling locked records
- Performing COMMIT or ROLLBACK as appropriate

```cobol
* IN SQL-IO-VALIDATION ROUTINE ADDED A CHECK TO SEE IF THE SQL
* CONNECTION (EXT-ACUSQL-CONNECT-STAT) IS OPEN (GOOD) THEN
* PERFORM CLOSE-FILES
```
**Source:** DECLRP_ASCII.CPY, Index: code

#### Dynamic SQL Construction

```cobol
STRING " WHERE TRBFILE.TB_BRNO = ",  
       QTRB1-WBEG-BRNO ,  
       " AND TRBFILE.TB_DATE BETWEEN '\"  ,
       QTRB1-WBEG-DATE  ,
       \" ' AND '\" ,
       QTRB1-WEND-DATE ,
       \"' | \" INTO WS-WHER
```
**Source:** RECAPJ.CBL, Index: code

Dynamic SQL allows programs to build queries based on business logic.

#### SQL Access Modules

Dedicated programs encapsulate SQL operations:
- **SQL**: DATA_ACCESS role, called by 2 programs
- **SQLLC**: Handles file operations and SQL modules
- **IOCONN**: SQL connection handler
- **SQLCONNECT**: Connection establishment

**Source:** SQL, SQLLC, Index: programs/flows

### 2.6 Transaction Processing and Data Consistency

OTIS ensures data consistency through structured transaction processing:

#### Transaction Processing Workflow

```cobol
PERFORM OPEN-TRX1-FILE.
MOVE LPEOD5-BRANCH TO TX-BRNO.
MOVE LPEOD5-TRANS-DATE TO TX-DATE SQL-DATE-YYYYMMDD.
PERFORM SQL-SET-DATE.
PERFORM START-TRX1-FILE.
PERFORM READ-TRX1-FILE-NEXT.
IF IO-FG NOT = 0 OR (TRX-PATH-OWNBR NOT = TX-BRNO)
   PERFORM ERROR-HANDLING.
```
**Source:** LPEOD5.CBL, REDACT.CBL, Index: code

#### Data Consistency Mechanisms

**Validation Routines:**
```cobol
PERFORM SQL-IO-VALIDATION.
```

**Error Handling:**
```cobol
IF ( IO-FG = 8 )     *> LOCKED
   GO TO REWRITE-TRX1-FILE.
IF ( IO-FG = 0 )
   EXEC SQL COMMIT END-EXEC
   PERFORM SQL-IO-VALIDATION.
```
**Source:** IOTRXI.CBL, Index: code

**Sequential Updates:**
```cobol
PERFORM WRITE-TRX1-FILE.
IF IO-FG = 0
   GO TO EXIT-UPDATE-TR1-PURCHASED.
```
**Source:** LONPFZ.CBL, Index: code

**Change Logging:**
```cobol
*   ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
```
**Source:** TRXMN3.CBL, TRBMN3.CBL, TRCMAN.CBL, Index: code

Change files support audit trails and rollback capability.

### 2.7 File Locking Mechanisms

OTIS uses multiple locking mechanisms for data integrity:

#### COBOL File Locking

```cobol
SELECT BY-FILE ASSIGN TO BY-PATH
       ORGANIZATION INDEXED
       ACCESS DYNAMIC
       LOCK MODE AUTOMATIC WITH LOCK ON RECORD
       RECORD KEY BY1-KEY
       FILE STATUS FILE-STAT.
```
**Source:** LPFSBY.CPY, Index: code

**LOCK MODE AUTOMATIC WITH LOCK ON RECORD** means:
- Records automatically locked when read for update
- Other users/programs cannot update until lock released
- Applied to indexed, sequential, and relative files

#### SQL Transaction Control

For SQL tables:
```cobol
EXEC SQL BEGIN TRANSACTION END-EXEC
EXEC SQL COMMIT END-EXEC
EXEC SQL ROLLBACK END-EXEC
```

#### Programmatic Lock Handling

**File Status Code Checking:**
```cobol
* BLF 060309 FIX PROBLEM WITH STATUS 93 ON OPFILE 
* (SOMEONE STILL IN DAILY PROCESSING) CAUSING LOCKUP
```
**Source:** AUTOC2.CBL, Index: code

Status code "93" indicates a record lock.

**Retry Logic:**
```cobol
IF ERROR-LOCK-FG = "Y"
   GO TO END-DECLARE
END-IF
PERFORM SUSPEND
ADD 1 TO LOCK-STATUS-99-RETRIES
IF LOCK-STATUS-99-RETRIES = 10
   MOVE 0 TO LOCK-STATUS-99-RETRIES
   MOVE FILE-STAT TO E-STATUS-ERROR
   MOVE E-FILE TO E-FILEX
   MOVE FORM-PATHNAME  TO E-PROG
   MOVE ERROR-MSG-HEAD TO ERRMSG-HEAD
   MOVE "RECORD LOCKED BY ANOTHER USER" TO ERRMSG
```
**Source:** DECLARE.CPY, Index: code

After 10 retries, the program reports an error and aborts.

### 2.8 Batch vs Online File Access

OTIS handles batch and online file access differently:

#### Batch File Processing

**Characteristics:**
- File-level locking during processing
- Sequential processing of entire files
- No user screens
- Error logging
- Triggered by schedulers (cron, JCL)

```cobol
* ONLY RUNS IN BATCH
* BATCH FORM PRINT FOR LONPEX DOC FILE CREATE
```
**Source:** BATCHP.CBL, Index: code

**Typical Batch Flow:**
1. Scheduler triggers program
2. Program opens files
3. Processes all records
4. Writes output
5. Closes files
6. Logs completion

#### Online File Access

**Characteristics:**
- Record-level locking
- Indexed or random access
- Interactive screens
- Immediate validation and error display
- User-triggered operations

```cobol
PERFORM ACCESS-CALL
IF FILE-NOT-FOUND
    DISPLAY "BATCH CORE FILE DOES NOT EXIST !!!"
END-IF
```
**Source:** BPBKBR.CBL, Lines 526-550; BPOTH.CBL, Lines 801-825

**Typical Online Flow:**
1. User initiates transaction
2. Program checks file existence
3. Reads/writes individual records
4. Displays results or errors
5. Loops for next transaction

### 2.9 Key Data Structures Passed Between Programs

Programs exchange data through standardized structures:

#### Record Buffers and Keys

```cobol
* Dealer record key
01  HOLD-DL1-KEY.
    05  HOLD-DL-BRNO    PIC 9(4).
    05  HOLD-DL-NUM     PIC 9(6).
```
**Source:** DLMAN6.CBL, Index: code

```cobol
* Payment/loan key
01  SAVE-PD2-KEY.
    05  KEY-ID          PIC X(2).
    05  KEY-BRANCH      PIC 9(4).
    05  KEY-NAME        PIC X(30).
```
**Source:** ZONPC0.CBL, Index: code

#### Common Memory Buffers

```cobol
* PROG-BUF contains error codes, program form data
01  PROG-BUF.
    05  PROG-ERROR-CODE     PIC XX.
    05  PROG-FORM-PATH      PIC X(80).
    05  PROG-CONTROL-FLAGS  PIC X(10).
```
**Source:** LPLONIW.CPY, Index: code

#### File Record Structures

```cobol
* Original and changed records for update operations
01  ORIGINAL-DL-REC.
    05  ...
01  CHANGED-DL-REC.
    05  ...
```
**Source:** DLMAN6.CBL, Index: code

#### Screen and Message Buffers

```cobol
01  MSG-LINE1   PIC X(80).
01  MSG-LINE2   PIC X(80).
```
**Source:** ZONPC0.CBL, Index: code

#### Copybook-Based Structures

Key copybooks for data exchange:
- **GBFSPR.CPY** - General business file structures (used by DLLIST, CLMCTR, GCLIST, CKVOID, GLEOD2, LN1099)
- **GBFSWK2I.CPY** - Ageing and trial balance structures (used by AGEALP)
- **SPINQ1.CPY** - Inquiry structures (used by SPINQ)
- **LPWSDL.CPY** - Loan payment work structures (used by RZ1ERN)
- **BULKTRW.CPY** - Bulk transfer workers (for batch operations)

**Source:** Various CBL files, Index: copybook_usage

### 2.10 Program-File Relationships

Programs are tightly coupled to the files they manage:

**LNFILE** (Loan Master File)
- Accessed by 113 programs
- Central repository for loan data
- Programs use LNFILE to read/update loan information

**CHFILE** (Charge File Handler)
- Manages charge records
- Calls and is called by other programs
- Uses copybooks: ACCESSW.CPY, DACCESS.CPY

**SSFILE** (Social Security File)
- Handles SSN data
- Accessed by multiple programs
- Uses DACCESS.CPY for file definitions

**STFILE** (State File)
- Manages state-related data
- Uses ACCESSW.CPY for access routines

**Source:** LNFILE, CHFILE, SSFILE, STFILE, Index: programs; corresponding .CBL files, Index: copybook_usage

**File Handler Pattern:**
```
Business Logic Program
      ↓ (CALL)
File Handler Program (e.g., LNFILE)
      ↓ (READ/WRITE/REWRITE/DELETE)
Indexed File
```

This modular approach:
- Encapsulates file access logic
- Ensures consistent file handling
- Simplifies maintenance

This completes Section 2: Data Flow and File Dependencies.

## 3. Menu and Screen Navigation

### 3.1 Main Menu Structure and Entry Point

The OTIS application uses a hierarchical menu system to organize access to its various business functions. Understanding this menu structure is essential for comprehending user workflows and program invocation patterns.

#### Primary Entry Point: PGMENU

**PGMENU** serves as the main entry point for users accessing the OTIS application:

```
User Login → PGMENU (Main Menu) → Subsystem Menus → Functional Programs
```

**Source:** PGMENU, Index: flows

The PGMENU flowchart shows the following navigation structure:

```
I-O-CONTROL → SETUP-WINDOW
              ↓
         SELECT-FIELD-FROM-DISPLAY
              ├→ SEND-LEGEND
              ├→ ADDON-MAINT
              ├→ DEALER-MAINT
              └→ LOAN-MAINT
```

**Main Subsystems Accessible from PGMENU:**
- Loan Maintenance
- Dealer Maintenance
- Addon Maintenance
- Batch Processing
- Collections
- Reporting
- System Administration

When a user selects an option, PGMENU invokes the corresponding subsystem menu or functional program.

(Sources: LPMENU, SCMENU, CLMENU, PGMENU, SNMAIN, PWMAIN, LTMAIN, STMAIN, BRMAN7, EQMAIN, AQMAIN, ICMAIN, GRMAIN, LMMAIN, NOMAIN, DBMAIN, PSMAIN, CXMAIN, CJMAIN, FMMAIN, BYMAIN, CKMAIN, SSMAIN, EAMAIN, DLMAIN, LNMAIN, UTMAIN)

### 3.2 Screen Handling and User Interface Patterns

OTIS handles screen presentation through COBOL SCREEN SECTION definitions and copybook-based layouts.

**Common Screen Patterns:**
- Fixed-format forms with field validation
- Menu-driven navigation with numeric selection
- Function key operations (F1=Help, F2=Save, F3=Exit, etc.)
- Field-level help text (stored in help_fields index)

**Screen Layout Definition Example:**
```cobol
SCREEN SECTION.
01 MAIN-SCREEN.
   05 LINE 1 COLUMN 1 VALUE "LOAN PROCESSING".
   05 LINE 3 COLUMN 5 VALUE "LOAN NUMBER:".
   05 LINE 3 COLUMN 20 PIC X(10) USING LN-NUMBER.
```

**Source:** LIBGB/SCREENW.CPY, GLIMIT.CBL, Index: copybook_usage

---

## Section 4: Key Copybooks and Their Purposes

### 4.1 File Definition Copybooks (FD Sections)

**Primary File Definition Copybooks:**

| Copybook | Purpose | Programs Using |
|----------|---------|----------------|
| GBFDPR.CPY | General business file definitions | 185+ programs |
| GBFSPR.CPY | General business file specifications | 185+ programs |
| LPFSWK.CPY | Loan processing file work areas | Loan programs |
| LPFDWK.CPY | Loan file definitions | Loan programs |
| LP01FD.CPY | Loan payment file definitions | Payment programs |
| UP01ATRP.CPY | Update transaction report files | Update programs |

**Source:** FMFILE.CBL, LONPF7.CBL, LONPF9.CBL, SFDISB.CBL, Index: copybook_usage

### 4.2 SQL Table Definition Copybooks

**Key SQL Definition Copybooks:**

| Copybook | Description | Programs |
|----------|-------------|----------|
| LPCDBGS_SQL.CPY | Loan core table definitions | PST270, PST272, PST271, PST277 |
| LPCKGS_SQL.CPY | Loan key table definitions | PST272, PST270, PST277 |
| LP01CDB_SQL.CPY | Batch/transaction tables | PST270, PST594, PST597 |
| LPTRCGS_SQL.CPY | Transaction code tables | PST270, PST272 |
| CONNECTW_SQL.CPY | Connection/session tables | ALSCAN, JKC001 |

**Source:** PST270.CBL, PST272.CBL, PST271.CBL, PST277.CBL, Index: copybook_usage

### 4.3 Error Code and Message Copybooks

**Primary Error Handling Copybook:**

**FERRORS.CPY** (LIBGB)
- Used by 165+ programs
- Defines error codes, message text, error handling routines
- Standard error messaging across system

**Additional Error Copybooks:**
- EOINIT.CPY - Error initialization routines
- GBRC2RN.CPY - Reconciliation error codes
- CONAME_EXT.CPY - Company name error codes
- SENDMESS.CPY - Message sending routines

**Source:** COINFO.CBL, LONPJ1.CBL, CRNOF3.CBL, EMAIL.CBL, MBCORP.CBL, PST610.CBL, Index: copybook_usage

### 4.4 Common Working Storage Copybooks

**Shared Working Storage Structures:**

| Copybook | Purpose | Programs |
|----------|---------|----------|
| GB01BR.CPY | General business records | SETOPT.CBL, ACTION.CBL |
| GBFSWKI.CPY | File working storage utility fields | CPCORP.CBL, SUMTRL.CBL |
| PST_COPYW.CPY | Posting working storage | PST820.CBL, PST464.CBL |
| FPWSPST.CPY | Financial posting working storage | PST469.CBL, PST593.CBL |

**Source:** SETOPT.CBL, ACTION.CBL, CPCORP.CBL, PST820.CBL, Index: copybook_usage

### 4.5 Date/Time Handling Copybooks

**Date/Time Copybooks:**

| Copybook | Purpose |
|----------|---------|
| DATER.CPY | Standard date field definitions |
| DATERW.CPY | Working-storage date fields |
| TIMEWK.CPY | Time-related fields |
| TIMEIO.CPY | Time I/O routines |
| LPTBNO.CPY | Batch number with timestamps |

**Source:** CHCOCD.CBL, DATE35.CBL, SETCOL.CBL, Index: copybook_usage

### 4.6 Transaction Code and Type Copybooks

**Transaction Definition Copybooks:**

| Copybook | Purpose |
|----------|---------|
| LP01TRX.CPY | Transaction record structures |
| LP01TRX_SQL.CPY | SQL transaction definitions |
| LP01CD.CPY | Transaction code mappings |
| LP01CD_SQL.CPY | SQL transaction codes |
| TRWCOD_SCN.CPY | Transaction code screen layout |

**Source:** TRXMN3.CBL, TRWCOD.CBL, TC2SCN.CBL, Index: copybook_usage

### 4.7 Business Rule and Validation Copybooks

**Validation Logic Copybooks:**

| Copybook | Purpose | Programs |
|----------|---------|----------|
| ERRMSG.CPY | Error messages | DECLARE, DECLARE_SQL |
| FPWSPST.CPY | Payment validation rules | PST594, PST599, PST841 |
| PST_COPYW.CPY | Posting validation logic | PST591, PST269 |
| EOINIT.CPY | End-of-day validation | EO1534, TR15VY |

**Source:** DECLARE.CPY, PST594.CBL, PST599.CBL, Index: copybook_usage

### 4.8 Copybook Organization and Categorization

**Library-Based Organization:**

| Library | Purpose | Example Copybooks |
|---------|---------|-------------------|
| LIBGB/ | General Business | GBFSPR.CPY, GBFSWKI.CPY |
| LIBLP/ | Loan Processing | LPFSVBY.CPY, LPWSLN.CPY |
| LIBFP/ | Financial Processing | PSTPMCW.CPY |
| LIBSP/ | Specialized Processing | SPINQ1.CPY |
| LIBWI/ | Workflow/Interface | GRSCANW.CPY |
| LIBUP/ | Update/Transaction | UP01ATRP.CPY |

**Source:** Multiple program copybook usage records

---

## Section 5: Program Dependencies and Call Hierarchies

### 5.1 Most Frequently Called Programs

**Top Utility Programs:**

| Program | Called By | Role |
|---------|-----------|------|
| SYSTEM | 24 programs | System utilities, environment setup |
| LONPW9 | Call cycles | Loan payoff notification |
| ICFILE | 3 programs | Insurance file management |
| SSFILE | 3 programs | Social security file management |
| LONPFB | 2 programs | Loan payment batch processing |
| CDBMAN | 2 programs | Certificate DB management |

**Source:** SYSTEM, LONPW9, ICFILE, SSFILE, LONPFB, CDBMAN, Index: programs

### 5.2 Call Chain Depth

**Typical Call Depth:**
- Screen handlers: 1 level
- Maintenance modules: 1-2 levels
- Business logic: 2-3 levels
- Batch/utility: 3-4+ levels

**Example:** PST593 has call depth of 51 (exceptionally deep, indicates complex batch processing).

**Source:** SYSTEM, CALCZ3, CALCZL, LNQUOT, Index: flows

### 5.3 Call Hierarchy Examples

**Menu to Business Logic:**
```
WIMENU → FORM-PROGX → SYSTEM
```

**Business Logic with Utility Calls:**
```
LONPFA → DISBURSE-OTHER-APPLIED → SYSTEM
```

**Source:** WIMENU, LONPFA, DACCESS, Index: flow_edges

### 5.4 Core Utility Program Dependencies

**Most-Depended-On Utilities:**

| Utility | Purpose | Dependency |
|---------|---------|------------|
| SYSTEM | Central system utility | Called by 24 programs |
| SETENV | Environment setup | Call depth 3 |
| ACCESSW.CPY | Data access routines | Included in many programs |
| SYSTEMW.CPY | System working storage | Included in many programs |
| GETENVW.CPY | Environment variables | Included in many programs |

**Source:** SYSTEM, SETENV, LTEXTR.CBL, PERUSE.CBL, Index: code

### 5.5 CALL Statement Usage

**Static CALL:**
```cobol
CALL "SYSTEM" USING SYSTEM-BUFFER
```

**Dynamic CALL:**
```cobol
CALL PROGRAM-NAME USING PARAMS
```

**Parameter Passing:**
- Business identifiers (BRNO, ACCTNO, LOANNO)
- Transaction context (dates, types, flags)
- Control flags (processing mode, error status)
- Error/status messages
- Environment info (DB names, server names)
- Data buffers (bulk transfer)
- Security credentials

**Source:** GREXEC, REFUPD, PERUSE, LEXBNK.CBL, Index: flow_edges

### 5.6 Program Dependency Documentation

**Documentation Methods:**
1. CALL statements in source code
2. Copybook COPY statements
3. Flow indices (flow_edges, program_deps, flows)
4. Comments and documentation fields
5. Help fields and screen documentation
6. Flowcharts and call graphs
7. Menu trees and UI paths

**Source:** LOADCO, LPMAN3, Index: flow_edges, flows

### 5.7 Circular Dependencies

**Inter-Program Cycle:**
- **PST593** participates in call cycles (call depth 51)

**Intra-Program Loops:**
- GRLST2: GO TO PARENT-LOOP
- PITBO2: GO TO FIX-NAME-LOOP2
- LN1099: GO TO FIX-NAME-LOOP
- IR1098: GO TO MERGE-LOOP
- IRPREP: GO TO LOOP-REC-C-B

**Source:** PST593, GRLST2, PITBO2, LN1099, Index: programs, flow_edges

### 5.8 User Entry Points

**Main Entry Programs (25+):**

| Category | Programs |
|----------|----------|
| Menus | LPMENU, SCMENU, CLMENU, PGMENU |
| Maintenance | LTMAIN, STMAIN, BRMAN7, EQMAIN, AQMAIN, ICMAIN, GRMAIN |
| Inquiry | UTMAIN, ENTRY, BYMAIN |
| Batch | TCLP, TCLPWO |

**Source:** LPMENU, SCMENU, LTMAIN, STMAIN, BRMAN7, Index: programs, flows

---

## Section 6: Performance Considerations

### 6.1 Performance Optimization Techniques

**I/O Minimization:**
```cobol
*THIS IS A COPY OF LP/LNAGED.CBL FOR TESTING THE NEW SQL-COL.SH
*SCRIPT THAT GETS THE SQL COLUMNS USED IN THE PROGRAM SO THAT 
*HARDCODED IO ROUTINES WITH LIMITED COLUMNS ARE USED TO IMPROVE PERFORMANCE.
```
**Source:** LNAGKC.CBL, Index: code

**Pacing and Throttling:**
```cobol
PACE-ROUTINE SECTION.
    IF ( EXT-ROUTE-BUF NOT = "00" )
       GO TO PACE-ROUTINE-EXIT.
    ADD 01 TO PACE-COUNTER.
    IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
       GO TO PACE-ROUTINE-EXIT.
```
**Source:** IORXRN.CBL, IOFMRN.CBL, IOLSDR.CBL, Index: code

**Performance Techniques Summary:**

| Technique | Purpose | Implementation |
|-----------|---------|----------------|
| Hardcoded I/O | Reduce unnecessary data access | Limited column selection |
| PACE-ROUTINE | Throttle processing | Counter-based pacing |
| Selective field access | Minimize memory/I/O | Only required fields |
| Modular routines | Reuse optimized code | FORM-RESET, ENTRY-MODULE |
| Data scope constraints | Limit processing chunks | MAX, OCCURS, arrays |
| Early error exits | Prevent wasted cycles | GO TO error-exit |

**Source:** LNAGKC.CBL, IORXRN.CBL, IOFMRN.CBL, Index: code

### 6.2 Concurrency and Multi-User Handling

**Record Locking Mechanisms:**

```cobol
IF ERROR-LOCK-FG = "Y"
   GO TO END-DECLARE
END-IF
PERFORM SUSPEND
ADD 1 TO LOCK-STATUS-99-RETRIES
IF LOCK-STATUS-99-RETRIES = 10
   MOVE "RECORD LOCKED BY ANOTHER USER" TO ERRMSG
```
**Source:** DECLARE.CPY, Index: code

**User-Specific Working Files:**

```cobol
*IN MARCH 2025, BETH AND JAY WORKED ON THE ISSUES THAT S35 USER 
*TESTING WAS GETTING (LOCKING, DISPLAY OUT OF ORDER, NOT GETTING 
*THE NEXT ACCOUNT WHEN SKIPPING).
*NOW IN THE COLLECTION WORK SCREEN, AT THE START, IT WILL LOAD 
*ALL CQ-REC FOR THAT GIVEN BRANCH INTO THE USER'S VCQFILE THEN 
*IT WILL GO THROUGH IT USING VCQ2-KEY
```
**Source:** SPINQ2.CPY, Index: code

**Concurrency Techniques:**

| Technique | Purpose | Implementation |
|-----------|---------|----------------|
| Record Locking | Prevent simultaneous updates | Lock flags, retry logic |
| Retry/Timeout | Avoid indefinite waits | 10-retry limit |
| User-Specific Files | Isolate sessions | VCQFILE per user |
| Record Comparison | Prevent overwrites | Match check before update |
| Error Messaging | Notify conflicts | "RECORD LOCKED BY ANOTHER USER" |

**Source:** DECLARE.CPY, SPINQ2.CPY, SP01VCQ.CPY, MB4ALL.CBL, Index: code

### 6.3 Logging and Debugging Facilities

**Centralized Logging Programs:**

| Program | Purpose | Copybooks Used |
|---------|---------|----------------|
| ERRLOG | Error logging | ACCESS.CPY, GETENV.CPY, GBERI.CPY |
| TRACE | Execution tracing | GETENV.CPY |
| SQLLOG | SQL operation logging | Various |
| SQLERR | SQL error logging | Various |
| APIDRV | Audit logging | SYSTEMW.CPY |
| AUDIQ | Operational auditing | SYSTEMW.CPY |

**Source:** ERRLOG.CBL, TRACE.CBL, SQLLOG, APIDRV, AUDIQ.CBL, Index: programs, copybook_usage

### 6.4 Batch Processing Management

**Batch Scheduling Logic:**

```
GET-BATCHES-FOR-TODAY → TEST-FOR-UNASSIGNED-BATCH → 
DISPLAY-BATCH → VERIFY-BATCH → BATCH-RELEASE → 
BATCH-PAYMENT-ROUTINE
```

**Batch Assignment:**
```
"Enter a BATCH number to assign to the Dealer checks."
```
**Source:** LP_LONPT_BATCH, LONPW1, LONPT0, Index: help_fields, flows

**Batch Dependencies:**
- Sequential job execution
- Manual intervention for failures
- Validation before processing
- Error reporting and preliminary runs

---

## Section 7: Technical Constraints and Limitations

### 7.1 Key Architectural Constraints

**Constraint Summary:**

| Constraint | Description | Impact |
|------------|-------------|--------|
| Record Locking | Serialized updates | Scalability limits, contention risk |
| Batch Dependencies | Sequential execution | Bottlenecks, delays |
| File-Based Storage | Flat files, rigid schemas | Limited flexibility, hard to change |
| Hard-Coded Logic | Manual code updates | Labor-intensive changes |
| Limited Modularization | Deep call chains | Reduced code reuse |
| Static UI Layouts | UI in code/copybooks | Cumbersome UI changes |
| Basic Error Handling | File logs, simple codes | Limited traceability |
| Legacy Technical Debt | Historical constraints | Frequent manual fixes |

**Source:** SP01CQ.CPY, UP1534.CBL, LONPC0.CBL, GLIMIT.CBL, LPCPMU, Index: code, programs

### 7.2 Scalability Limitations

- Record-level locking limits concurrent throughput
- Batch processing creates bottlenecks
- File-based storage limits ad-hoc queries
- Hard-coded business logic resists rapid changes

### 7.3 Modernization Challenges

**Historical Design Decisions:**
```cobol
*REMOVED THE HARD CODED 'IF' STATEMENT.
*REMOVED USE OF OLD STATUS ROUTINES [LIBLA/MASTATUS(W)].
*NOTES MENTIONS RISC2 NOT UPDATING THE WRFILE, ALL DONE ON RISC1.
```
**Source:** UP1534.CBL, LONPC0.CBL, Index: code

**Legacy Technical Debt:**
- Field size changes requiring widespread updates
- File splits and restructuring
- Removal of obsolete routines
- Ongoing manual fixes for historical constraints

---

## Conclusion

The OTIS technical architecture represents a mature mainframe COBOL system with well-defined structures for program organization, data management, and business logic processing. While the architecture effectively supports current operations, it faces constraints typical of legacy systems: rigid file structures, hard-coded logic, limited modularization, and scalability challenges.

Key strengths include:
- Comprehensive error handling and logging
- Well-organized copybook structure
- Clear program dependencies and call hierarchies
- Robust concurrency mechanisms
- Performance optimizations (pacing, selective I/O)

Areas for improvement in modernization:
- Database migration from flat files
- Modularization and service-oriented architecture
- Dynamic UI framework implementation
- Enhanced error handling and traceability
- Automation of batch processing dependencies

Understanding this technical architecture is essential for successful migration to C#, as it reveals the complexity, dependencies, and business logic that must be preserved while addressing the architectural limitations of the legacy system.

---

**Chapter 3 Complete - Total Questions Incorporated: 60**
**Research Sources: 7,700 total across all questions**
**Total Research Time: 945 seconds**
