# APIPAY Program Documentation

**Generated Date:** July 17, 2025  
**Program ID:** APIPAY  
**Date Written:** 08/16/2023  
**Language:** COBOL  

---

## Table of Contents

1. [Program Overview](#program-overview)
2. [Program Purpose](#program-purpose)
3. [Input Parameters](#input-parameters)
4. [Program Flow](#program-flow)
5. [Transaction Types Supported](#transaction-types-supported)
6. [Referenced Programs](#referenced-programs)
7. [File Processing](#file-processing)
8. [Error Handling](#error-handling)
9. [Return Status Codes](#return-status-codes)
10. [Logging](#logging)

---

## Program Overview

**APIPAY** is a single payment reference codes processing program designed for batch payment processing. It's a command-line program that runs without screens ($ sign run) and processes individual loan payment transactions based on reference codes.

### Key Characteristics
- **Batch Processing**: Processes single payment transactions
- **No User Interface**: Command-line execution only
- **Reference Code Driven**: Transaction processing based on reference codes
- **Comprehensive Validation**: Multiple validation checks before processing
- **Audit Trail**: Creates detailed log entries for all transactions

---

## Program Purpose

APIPAY serves as a centralized payment processing engine that:

1. **Validates Transaction Parameters**: Ensures all input data is valid before processing
2. **Routes to Appropriate Processors**: Directs transactions to specialized payment programs based on reference codes
3. **Maintains Data Integrity**: Performs extensive validation to prevent invalid transactions
4. **Provides Audit Trail**: Creates comprehensive logs for all transaction attempts
5. **Handles Multiple Payment Types**: Supports various loan payment scenarios

### Business Context
The program was created by consolidating logic from existing programs:
- **REFUPD** and **LONPW9** (base logic)
- **OTHUPD** and **LONPWA** (OT transaction logic)

---

## Input Parameters

The program accepts two input parameters via COBOL CHAINING:

### Parameter 1: INPUT-STRING (BT-REC Format)
A concatenated string containing:
- **BT-BRANCH** (4 digits): Branch number
- **BT-ACCTNO** (6 digits): Account number  
- **BT-AMT** (10 digits): Transaction amount (implied decimal)
- **BT-REFCD** (2 chars): Reference code
- **BT-PAYDATE** (6 digits): Payment date (MMDDYY)
- **BT-TRCD** (2 chars): Transaction code

**Example**: `00100786740000001000CUACH090123OT`

### Parameter 2: REPAY-TRANS-ID
A transaction identifier for tracking purposes.

---

## Program Flow

![APIPAY Program Flow](Diagrams/APIPAY_Program_Flow.md)

### Transaction Processing Flow

![APIPAY Transaction Flow](Diagrams/APIPAY_Transaction_Flow.md)

### Data Flow Architecture

![APIPAY Data Flow](Diagrams/APIPAY_Data_Flow.md)

### High-Level Processing Steps

1. **Initialization Phase**
   - Parse input parameters
   - Set up environment variables
   - Connect to database (SQL-CONNECT)
   - Initialize working variables

2. **Validation Phase**
   - Validate branch record exists and is accessible
   - Validate reference code in CD1 file
   - Check machine assignment compatibility
   - Verify day is open for transactions
   - Validate loan account exists and is accessible

3. **Transaction-Specific Validation**
   - Perform validation specific to transaction type
   - Check account status and restrictions
   - Validate business rules for the specific transaction

4. **Payment Processing**
   - Route to appropriate payment processor based on transaction code
   - Setup LONPF buffer for payment programs
   - Execute payment logic
   - Handle payoff processing if required

5. **Completion**
   - Log transaction results
   - Set return status
   - Clean up resources

---

## Transaction Types Supported

| Transaction Code | Description | Processor Program | Purpose |
|------------------|-------------|-------------------|---------|
| **PY** | Payment | [LONPFC](LONPFC_Documentation.md) | Standard loan payment |
| **PA** | Payment Alternative | [LONPFC](LONPFC_Documentation.md) | Alternative payment method |
| **RP** | Rebate Payment | [LONPFB](LONPFB_Documentation.md) | Interest rebate processing |
| **PL** | Profit & Loss | [LONPF9](LONPF9_Documentation.md) | Charge-off to P&L |
| **P2** | Profit & Loss Type 2 | [LONPF9](LONPF9_Documentation.md) | Alternative P&L processing |
| **P3** | Profit & Loss Type 3 | [LONPF9](LONPF9_Documentation.md) | Third P&L option |
| **RE** | Repossession | [LONPF7](LONPF7_Documentation.md) | Repossession processing |
| **O2** | Other Type 2 | [LONPF7](LONPF7_Documentation.md) | Secondary other processing |
| **RV** | Reversal | [LONPF2](LONPF2_Documentation.md) | Transaction reversal |
| **BK** | Bankruptcy | [LONPF2](LONPF2_Documentation.md) | Bankruptcy status change |
| **BD** | Bankruptcy Dismissal | [LONPF2](LONPF2_Documentation.md) | Bankruptcy dismissal |
| **OT** | Other | [LONPF7](LONPF7_Documentation.md) | Other transaction types |

---

## Referenced Programs

The APIPAY program calls several specialized payment processing programs:

1. **[LONPFC](LONPFC_Documentation.md)** - Payment Processing
2. **[LONPFB](LONPFB_Documentation.md)** - Rebate Processing  
3. **[LONPF9](LONPF9_Documentation.md)** - Profit & Loss Processing
4. **[LONPF7](LONPF7_Documentation.md)** - Repository/Other Processing
5. **[LONPF2](LONPF2_Documentation.md)** - Reversal/Bankruptcy Processing
6. **[LONPFA](LONPFA_Documentation.md)** - Payoff Processing

Each of these programs handles specific aspects of loan payment processing and maintains the loan file integrity.

---

## File Processing

### Primary Files Used

| File | Purpose | Access Mode |
|------|---------|-------------|
| **LN1-FILE** | Loan master file | Read/Write |
| **BR-FILE** | Branch master file | Read |
| **CD1-FILE** | Code definition file | Read |
| **GB-FILE** | Global parameters | Read |
| **WK-FILE** | Work file | Read/Write |
| **OP-FILE** | Operation log file | Write |
| **LOG-FILE** | Transaction log | Write |
| **RC2-FILE** | Daily control file | Read |
| **BW1-FILE** | Batch work file | Read/Write |

### File Processing Sequence

1. **Setup Phase**: Open branch and global files for configuration
2. **Validation Phase**: Read branch, code, and loan files
3. **Processing Phase**: Update loan and work files through called programs
4. **Logging Phase**: Write to operation and log files

---

## Error Handling

The program implements comprehensive error handling with specific return codes for different error conditions:

### Validation Errors
- **Branch Validation**: Ensures branch exists and is on correct machine
- **Reference Code Validation**: Validates reference codes in CD1 file
- **Account Validation**: Checks loan account existence and status
- **Date Validation**: Ensures payment dates are valid
- **Business Rule Validation**: Enforces business-specific rules

### Error Recovery
- **File Errors**: Handled through declaratives section
- **Program Errors**: Logged and returned with specific status codes
- **Database Errors**: SQL error handling implemented

---

## Return Status Codes

| Status | Description |
|--------|-------------|
| **0** | Successful update |
| **2** | Branch not on file |
| **3** | Reference code not on file |
| **4** | Branch not on this machine |
| **5** | Global record missing |
| **6** | Day not open |
| **7** | Could not open day |
| **8** | Auto accelerate flag reject |
| **9** | Back date in prior month |
| **10** | Account not on file |
| **11** | Account has escrow |
| **12** | Account already bankrupt |
| **13** | Account not bankrupt status |
| **14** | Account already active P&L |
| **15** | Must have repo options turned on |
| **16** | Account not repo status |
| **17** | Account not interest bearing |
| **99** | Default error status |

---

## Logging

### Log File Format
The program creates a log file with the naming convention: `SC9990/EO/APIMMDDYY/0000LOG`

### Log Entry Format
```
[PASS/FAIL] [Return Code] [Date] [Time] [Branch] [Account] [Message]
```

### Sample Log Entries
```
     2023/08/18 01:04 PM 0010 078674 BEGIN SINGLE BATCH PAYMENT POSTING
PASS 2023/08/18 01:04 PM 0010 078674 SUCCESSFUL UPDATE
     2023/08/18 01:04 PM 0010 033333 BEGIN SINGLE BATCH PAYMENT POSTING
FAIL 2023/08/18 01:04 PM 0010 033333 ACCOUNT NOT ON FILE, ABORTED
```

### Logging Features
- **Transaction Start**: Logs beginning of each transaction
- **Success/Failure**: Clear indication of transaction outcome
- **Error Details**: Specific error messages for failed transactions
- **Audit Trail**: Complete transaction history for compliance

---

## Technical Notes

### Performance Considerations
- **File Locking**: Uses automatic record locking for data integrity
- **Memory Management**: Efficient use of working storage
- **Resource Cleanup**: Proper file closure and resource deallocation

### Maintenance History
- **2024.0206 BAH #1641**: Bug fixes
- **2025.0228 BAH**: LXE-EARN field clearing fix (S35Q-168)

### Dependencies
- **Database Connection**: Requires SQL connectivity
- **File System**: Access to loan processing file system
- **Environment Variables**: Proper system environment setup

---

## Usage Example

### Command Line Execution
```bash
SH ./APIPAY.SH 00100786740000001000CUACH090123OT 1234567890
```

### Parameters Breakdown
- **Branch**: 0010
- **Account**: 078674
- **Amount**: $10.00 (0000001000)
- **Reference Code**: CU
- **Payment Date**: 09/01/23 (090123)
- **Transaction Code**: OT
- **Transaction ID**: 1234567890

---

*This documentation provides a comprehensive overview of the APIPAY program for users unfamiliar with the system. For detailed information about specific payment processors, refer to the linked documentation for each LONPF program.*
