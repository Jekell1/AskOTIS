# Chapter 2: Business Domain Analysis
## Overview
This chapter provides a comprehensive analysis of the business domain served by OTIS, including loan servicing operations, financial processes, key business workflows, and the various user roles that interact with the system daily.
## 2.1 Loan Servicing and Financial Operations in OTIS
### What are the core loan servicing functions performed by the OTIS system?

The OTIS system (Online Transaction Information System) is designed to support comprehensive loan servicing for financial institutions. Its core functions span the entire lifecycle of a loan, from origination to payoff, including maintenance, reporting, and compliance. Here’s a summary of the main loan servicing functions performed by OTIS, based on the retrieved context:

---

**BUSINESS PURPOSE:**
OTIS automates and manages all key activities required to service loans, ensuring accuracy, regulatory compliance, and efficient customer support.

**CORE LOAN SERVICING FUNCTIONS:**

1. **Loan Origination and Booking**
   - Creation of new loan records, including capturing borrower details, loan terms, and initial disbursement.
   - Example: Programs like APILNS and LNAPPROVAL handle loan creation and approval workflows.
   - Source: [[FILE:APILNS|APILNS]], [[FILE:LNAPPROV|LNAPPROV]]AL (Index: flows)

2. **Loan Maintenance and Changes**
   - Updating loan information (e.g., address, payment schedule, interest rates).
   - Processing modifications such as payment rescheduling, term changes, or status updates.
   - Example: LONPF3 and LONPS4 manage loan changes and maintenance routines.
   - Source: [[FILE:LONPF3|LONPF3]], [[FILE:LONPS4|LONPS4]] (Index: flows)

3. **Payment Processing**
   - Accepting and posting payments, including regular, partial, and payoff transactions.
   - Calculating outstanding balances, interest accruals, and late charges.
   - Example: Programs like GP-PAYOFF-NONCASH and LONPG0 handle payment posting and payoff calculations.
   - Source: [[FILE:GP-PAYOF|GP-PAYOF]]F-NONCASH, LONPG0 (Index: programs, code)

4. **Delinquency and Collections Management**
   - Identifying delinquent accounts and managing collection activities.
   - Generating collection letters, reports, and follow-up actions.
   - Example: CPINQ0 supports delinquency inquiry and collector assignment.
   - Source: [[FILE:CPINQ0|CPINQ0]] (Index: flows)

5. **Escrow and Add-on Management**
   - Managing escrow accounts for taxes, insurance, and other add-ons.
   - Tracking and updating escrow balances and disbursements.
   - Example: ESCROW program manages escrow-related transactions.
   - Source: [[FILE:ESCROW|ESCROW]] (Index: programs)

6. **Reporting and Compliance**
   - Generating regulatory, management, and operational reports (e.g., aged trial balances, payment histories).
   - Ensuring compliance with federal and state regulations (e.g., payment limits, disclosure requirements).
   - Example: SUMMBY and FXBYTY aggregate and report loan data.
   - Source: [[FILE:SUMMBY|SUMMBY]], [[FILE:FXBYTY|FXBYTY]] (Index: flows)

7. **Dealer and Add-on Maintenance**
   - Supporting dealer relationships and managing add-on products (e.g., insurance, warranties).
   - Example: Programs like DLPOFF and ADMAIN handle dealer and add-on maintenance.
   - Source: [[FILE:DLPOFF|DLPOFF]] (Index: variables), ADMAIN (inferred from context)

8. **Data Access and Integration**
   - Reading and updating loan data from various files and databases.
   - Integrating with external systems for data exchange and batch processing.
   - Example: SQLLC_START_LC6, SQLLC_READ_LC8 provide data access routines.
   - Source: [[FILE:SQLLC|SQLLC]]_START_LC6, SQLLC_READ_LC8 (Index: programs)

---

**TECHNICAL IMPLEMENTATION:**
- OTIS is structured as a modular COBOL application, with each function handled by dedicated programs.
- Programs interact via well-defined call chains, passing data through copybooks and files.
- Data integrity is maintained through validation routines and error handling.
- User interfaces (screens) guide operators through transaction entry, maintenance, and reporting.

---

**VISUAL REPRESENTATION:**

Here's a visual diagram of the OTIS system's core loan servicing functions:

<svg width="900" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="24" font-weight="bold" fill="#1a1a1a">OTIS Loan Servicing Functions</text>
  
  <!-- Main OTIS System Box -->
  <rect x="350" y="70" width="200" height="60" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="12"/>
  <text x="450" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="white">OTIS System</text>
  
  <!-- Function Boxes -->
  <rect x="50" y="180" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="10"/>
  <text x="140" y="210" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Loan Origination</text>
  
  <rect x="250" y="180" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="10"/>
  <text x="340" y="210" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Loan Maintenance</text>
  
  <rect x="450" y="180" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="10"/>
  <text x="540" y="210" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Payment Processing</text>
  
  <rect x="650" y="180" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="10"/>
  <text x="740" y="210" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Collections</text>
  
  <rect x="150" y="300" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="10"/>
  <text x="240" y="330" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Escrow/Add-ons</text>
  
  <rect x="370" y="300" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="10"/>
  <text x="460" y="330" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Reporting</text>
  
  <rect x="590" y="300" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="10"/>
  <text x="680" y="330" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Dealer/Add-on Maint</text>
  
  <rect x="370" y="420" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="10"/>
  <text x="460" y="450" text-anchor="middle" font-size="14" font-weight="bold" fill="#15803d">Data Access/Integration</text>
  
  <!-- Arrows from OTIS System to functions -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="450" y1="130" x2="140" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="340" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="540" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="740" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="240" y2="300" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="460" y2="300" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="680" y2="300" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="130" x2="460" y2="420" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
OTIS provides a full suite of loan servicing capabilities, including origination, maintenance, payment processing, collections, escrow/add-on management, reporting, dealer/add-on maintenance, and robust data access/integration. Each function is supported by specialized COBOL programs and workflows, ensuring efficient and compliant loan management.

**Sources:**
- APILNS, LNAPPROVAL, LONPF3, LONPS4, GP-PAYOFF-NONCASH, LONPG0, CPINQ0, ESCROW, SUMMBY, FXBYTY, DLPOFF, SQLLC_START_LC6, SQLLC_READ_LC8 (Index: programs, flows, code)

### What types of loans does OTIS handle (consumer, commercial, mortgage, etc.)?

OTIS (Online Transaction Information System) is designed to handle a wide variety of loan types for financial institutions. Based on the retrieved context and program/copybook references, OTIS supports the following loan categories:

**Loan Types Handled by OTIS:**

1. **Consumer Loans**
   - Includes direct personal loans, installment loans, and revolving credit.
   - Evidence: References to "CONSUMER DECEASED" and ECOA codes for individual/joint liability (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code).

2. **Commercial Loans**
   - Business/commercial loans are explicitly mentioned.
   - Evidence: "W = BUSINESS/COMMERCIAL" in ECOA code definitions (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code).

3. **Mortgage Loans**
   - Covers first and second mortgages, including origination and principal calculations.
   - Evidence: Comments such as "CHARGED ON DIRECT LOANS, FIRST MORTGAGES, AND SECOND MORTGAGES" (Source: [[FILE:CALCZL.CBL|CALCZL.CBL]], Index: code).

4. **Line of Credit Loans**
   - Includes open-ended credit lines and revolving credit accounts.
   - Evidence: "LINE OF CREDIT = HIGHEST BALANCE EVER ATTAINED" (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code).

5. **Installment Loans**
   - Fixed-term loans with scheduled payments.
   - Evidence: "INSTALLMENT = ORIGINAL AMT OF LOAN" (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code).

6. **Open Loans**
   - Likely refers to open-ended credit products.
   - Evidence: "OPEN = HIGHEST BALANCE EVER ATTAINED" (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code).

7. **Revolving Loans**
   - Credit cards and other revolving credit arrangements.
   - Evidence: "REVOLVING = HIGHEST BALANCE EVER ATTAINED" (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code).

8. **Special State-Specific Loans**
   - North Carolina, Tennessee, Illinois, and others are referenced, indicating support for state-specific loan products.
   - Evidence: "DIRECT LOANS (NON SALES FINANCE) NORTH CAROLINA, (SCFRMLA 23)" (Source: [[FILE:NCSC23.CBL|NCSC23.CBL]], Index: code).

9. **Other Specialized Loans**
   - References to "BANKRUPTCY", "GIFTNET", and "DISBURSED" suggest handling of loans with special conditions or features.
   - Evidence: Data items such as ":QLN-BNKRPT-CHAPTER", ":QLN-GIFTNET", ":QLN-PROC-DISBURSED" (Source: [[FILE:ASOPEN.CBL|ASOPEN.CBL]], [[FILE:CQRUN.CBL|CQRUN.CBL]], [[FILE:IOLNIN.CBL|IOLNIN.CBL]], Index: code).

---

**Summary Table: Loan Types Supported**

| Loan Type           | Description/Notes                                      | Source Reference                |
|---------------------|--------------------------------------------------------|---------------------------------|
| Consumer            | Personal, installment, revolving credit                | [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], [[FILE:CALCZL.CBL|CALCZL.CBL]]      |
| Commercial          | Business loans                                         | [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]                  |
| Mortgage            | First/second mortgages, direct loans                   | [[FILE:CALCZL.CBL|CALCZL.CBL]], [[FILE:NCSC23.CBL|NCSC23.CBL]]          |
| Line of Credit      | Open-ended credit lines                                | [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]                  |
| Installment         | Fixed-term, scheduled payments                         | [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]                  |
| Open                | Open-ended credit products                             | [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]                  |
| Revolving           | Credit cards, revolving accounts                       | [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]                  |
| State-Specific      | NC, TN, IL, etc. direct loans                         | [[FILE:NCSC23.CBL|NCSC23.CBL]]                      |
| Specialized/Other   | Bankruptcy, gift, disbursed, special conditions        | [[FILE:ASOPEN.CBL|ASOPEN.CBL]], [[FILE:CQRUN.CBL|CQRUN.CBL]], [[FILE:IOLNIN.CBL|IOLNIN.CBL]] |

---

**Source Citations:**
- "W = BUSINESS/COMMERCIAL" and other ECOA codes (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code)
- "CHARGED ON DIRECT LOANS, FIRST MORTGAGES, AND SECOND MORTGAGES" (Source: [[FILE:CALCZL.CBL|CALCZL.CBL]], Index: code)
- "DIRECT LOANS (NON SALES FINANCE) NORTH CAROLINA, (SCFRMLA 23)" (Source: [[FILE:NCSC23.CBL|NCSC23.CBL]], Index: code)
- Data items for bankruptcy, gift, and disbursed loans (Source: [[FILE:ASOPEN.CBL|ASOPEN.CBL]], [[FILE:CQRUN.CBL|CQRUN.CBL]], [[FILE:IOLNIN.CBL|IOLNIN.CBL]], Index: code)

**Conclusion:**  
OTIS is a multi-purpose loan servicing system supporting consumer, commercial, mortgage, line of credit, installment, open/revolving, state-specific, and specialized loans. This breadth allows financial institutions to manage a wide range of lending products within a unified platform.

### How does OTIS manage the complete loan lifecycle from origination to payoff?

OTIS (Online Transaction Information System) manages the complete loan lifecycle by orchestrating a series of specialized COBOL programs and routines that handle each major phase of a loan’s existence—from origination, through servicing, to final payoff. Here’s how OTIS covers each stage:

---

### 1. **Loan Origination**
- **Business Purpose:** Capture new loan applications, validate applicant data, calculate terms, and book loans into the system.
- **Technical Implementation:**
  - Programs like **APILNS** handle loan creation, calling routines for calculations and restrictions (e.g., **CALCZL**, **LNVERI**).
  - Origination involves creating log and data files for audit and compliance ([Source: [[FILE:APIDRV.CBL|APIDRV.CBL]], Index: code]).
  - Data is validated, and if successful, the loan is added to the master files.

---

### 2. **Loan Servicing**
- **Business Purpose:** Manage ongoing loan activities such as payment processing, interest accrual, account maintenance, and customer inquiries.
- **Technical Implementation:**
  - **Payment Processing:** Programs like **LONPFA**, **LONPFB**, and **LONPF2** handle payment posting, updating balances, and recalculating due dates.
  - **Account Maintenance:** Programs such as **LONPS4** and **RESCHD** manage changes to loan terms, rescheduling, and modifications.
  - **Inquiry and Reporting:** Inquiry programs (e.g., **CPINQ0**, **LONPF0**) allow users to view loan status, payment history, and account details.
  - **Interest and Fee Calculations:** Specialized routines calculate interest accruals, late fees, and other charges.

---

### 3. **Loan Payoff**
- **Business Purpose:** Calculate final payoff amounts, process loan closure, and handle rebates or refunds.
- **Technical Implementation:**
  - **Payoff Calculation:** Programs like **PAYOFF**, **LPPOFF**, and **WIPOFF** compute the exact payoff amount, including principal, accrued interest, and any applicable rebates ([Source: [[FILE:LPPOFF.CBL|LPPOFF.CBL]], Index: code]; [Source: [[FILE:PAYOFF.CBL|PAYOFF.CBL]], Index: copybook_usage]).
  - **Final Processing:** These programs update the loan status to closed, generate payoff statements, and ensure all financial records are updated.
  - **Rebate Handling:** Routines within **LPPOFF** and related programs calculate and apply insurance or interest rebates as required ([Source: [[FILE:LPPOFF.CBL|LPPOFF.CBL]], Index: code]).

---

### 4. **Post-Payoff and Archival**
- **Business Purpose:** Ensure all records are finalized, compliance is met, and data is archived for future reference.
- **Technical Implementation:**
  - Programs update master files, generate audit logs, and may trigger archival routines.
  - Reporting programs summarize closed loans for regulatory and business reporting.

---

## **Workflow Narrative Example**

**Typical Loan Lifecycle in OTIS:**
1. **Origination:**  
   - User enters loan application → **APILNS** validates and books the loan.
2. **Servicing:**  
   - Customer makes payments → **LONPFA/LONPFB** post payments, update balances.
   - Account changes (e.g., reschedule) → **RESCHD** processes modifications.
   - Inquiries → **CPINQ0** displays loan/account status.
3. **Payoff:**  
   - Customer requests payoff → **PAYOFF/LPPOFF** calculate final amount.
   - Payment received → Loan marked as paid off, rebates processed.
4. **Closure:**  
   - Records updated, reports generated, loan archived.

---

## **Key Programs and Their Roles**

| Stage         | Key Programs      | Description                                                                 |
|---------------|------------------|-----------------------------------------------------------------------------|
| Origination   | APILNS           | Loan creation, validation, and booking                                      |
| Servicing     | LONPFA, LONPFB   | Payment posting, balance updates                                            |
|               | LONPS4, RESCHD   | Account maintenance, rescheduling, modifications                            |
|               | CPINQ0, LONPF0   | Loan/account inquiry and reporting                                          |
| Payoff        | PAYOFF, LPPOFF, WIPOFF | Payoff calculation, closure, rebate/refund processing                  |
| Post-Payoff   | Reporting programs| Audit, compliance, and archival                                            |

---

## **Visual Representation: OTIS Loan Lifecycle Workflow**

Here's a visual representation:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Loan Lifecycle Management</text>
  
  <!-- Origination -->
  <rect x="50" y="80" width="180" height="80" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="10"/>
  <text x="140" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#065f46">Origination</text>
  <text x="140" y="130" text-anchor="middle" font-size="12" fill="#065f46">APILNS</text>
  
  <!-- Servicing -->
  <rect x="270" y="80" width="250" height="80" fill="#e0e7ff" stroke="#4338ca" stroke-width="3" rx="10"/>
  <text x="395" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#3730a3">Servicing</text>
  <text x="395" y="130" text-anchor="middle" font-size="12" fill="#3730a3">LONPFA, LONPFB, LONPS4, RESCHD, CPINQ0</text>
  
  <!-- Payoff -->
  <rect x="550" y="80" width="140" height="80" fill="#f59e0b" stroke="#b45309" stroke-width="3" rx="10"/>
  <text x="620" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#92400e">Payoff</text>
  <text x="620" y="130" text-anchor="middle" font-size="12" fill="#92400e">PAYOFF, LPPOFF, WIPOFF</text>
  
  <!-- Post-Payoff -->
  <rect x="730" y="80" width="120" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="10"/>
  <text x="790" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#166534">Post-Payoff</text>
  <text x="790" y="130" text-anchor="middle" font-size="12" fill="#166534">Reporting, Archival</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="120" x2="270" y2="120" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="520" y1="120" x2="550" y2="120" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="120" x2="730" y2="120" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- Stage labels -->
  <text x="140" y="180" text-anchor="middle" font-size="12" fill="#065f46">Application, Validation, Booking</text>
  <text x="395" y="180" text-anchor="middle" font-size="12" fill="#3730a3">Payments, Maintenance, Inquiry</text>
  <text x="620" y="180" text-anchor="middle" font-size="12" fill="#92400e">Payoff Calculation, Closure</text>
  <text x="790" y="180" text-anchor="middle" font-size="12" fill="#166534">Audit, Compliance, Archive</text>
</svg>

---

## **Source References**
- Loan origination and validation: [Source: [[FILE:APIDRV.CBL|APIDRV.CBL]], Index: code], [Source: [[FILE:APILNS|APILNS]], [[FILE:I|I]]ndex: flows]
- Payment processing and servicing: [Source: [[FILE:LONPFA|LONPFA]], [[FILE:LONPFB|LONPFB]], [[FILE:LONPS4|LONPS4]], [[FILE:RESCHD|RESCHD]], [[FILE:CPINQ0|CPINQ0]], [[FILE:I|I]]ndex: flows]
- Payoff calculation and closure: [Source: [[FILE:PAYOFF|PAYOFF]], [[FILE:LPPOFF|LPPOFF]], [[FILE:WIPOFF|WIPOFF]], [[FILE:I|I]]ndex: programs, flows, code]
- Rebate and final processing: [Source: [[FILE:LPPOFF.CBL|LPPOFF.CBL]], Index: code]
- Reporting and archival: [Source: [[FILE:CLAST2.CBL|CLAST2.CBL]], Index: code]

---

**Summary:**  
OTIS manages the loan lifecycle by coordinating specialized programs for each phase—origination, servicing, payoff, and closure—ensuring accurate processing, compliance, and robust record-keeping throughout the life of every loan.

### What payment processing capabilities does OTIS provide for loan accounts?

OTIS provides robust payment processing capabilities for loan accounts, supporting a wide range of payment-related operations throughout the loan lifecycle. Here’s a summary of its payment processing functions:

**BUSINESS PURPOSE:**
- Accepts and processes payments from borrowers for various loan types (consumer, commercial, mortgage, etc.).
- Handles regular scheduled payments, partial payments, payoffs, and refunds.
- Supports batch and individual payment entry, including automated and manual posting.
- Validates payment amounts, dates, and account status before posting.
- Updates loan balances, payment histories, and generates notifications/receipts.
- Integrates with external systems for ACH, check, and cash payments.
- Manages exceptions, duplicate payments, and special cases (e.g., automotive credit, world accounts).

**TECHNICAL IMPLEMENTATION:**
- Payment processing is distributed across several specialized COBOL programs and routines, including APIPAY, LONPF9, LONPFC, LPPOFF, REPAYE, and others.
- Key features include:
  - **Payment Entry & Validation:** Programs like APIPAY validate payment amounts against account balances and regulatory limits (e.g., SEC max for ACH).
  - **Batch Payment Handling:** LONPF9 and LONPFC manage batch payment routines, including selection modules and exception handling.
  - **Payoff Processing:** LPPOFF and GP-PAYOFF-NONCASH handle full loan payoffs, calculating final amounts, rebates, and updating loan status.
  - **Refunds & Reversals:** APIPAY and related routines process refunds and reversal transactions, ensuring accurate ledger updates.
  - **File Creation & Reporting:** REPAYE creates ASCII loan and payment files for open accounts, supporting external reporting and reconciliation.
  - **Exception Management:** LONPW9 and related modules handle duplicate payments, special account types, and error logging.
  - **Integration:** Payment routines interact with files such as PAYMENT-FILE, LEDGER-FILE, and HISTORY-FILE, ensuring persistent and auditable records.

**EXAMPLES FROM SOURCE:**
- **APIPAY:** "Validates payment amount, checks account status, calls REFUPD to process, calls LONPW9 for notification."  
  (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows)
- **LONPF9:** "Handles batch payment routine, selection module, exception management."  
  (Source: [[FILE:LONPF9|LONPF9]], [[FILE:I|I]]ndex: flows)
- **LPPOFF:** "Payoff loan routine, calculates rebates, sets interest/late charges, updates loan status."  
  (Source: [[FILE:LPPOFF|LPPOFF]], [[FILE:I|I]]ndex: flows)
- **REPAYE:** "Creates ASCII loan and payment files for all open accounts, excludes rescinded/discharged loans."  
  (Source: [[FILE:REPAYE.CBL|REPAYE.CBL]], Index: code)
- **LONPW9:** "Handles duplicate payment exceptions, special customer logic, batch payment rules."  
  (Source: [[FILE:LONPW9.CBL|LONPW9.CBL]], Index: code)

**PAYMENT PROCESSING WORKFLOW:**
1. **Payment Entry:** User enters payment via screen; APIPAY validates and posts.
2. **Batch Processing:** LONPF9/LONPFC process batches, update accounts, handle exceptions.
3. **Payoff Calculation:** LPPOFF computes payoff, updates loan, generates receipts.
4. **File Creation:** REPAYE generates payment files for reporting.
5. **Notification:** LONPW9 sends confirmation, logs errors/exceptions.

**Here's a visual representation:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Loan Payment Processing Architecture</text>
  
  <!-- Presentation Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Screens: Payment Entry, Batch Payment, Payoff Request</text>
  
  <!-- Business Logic Layer -->
  <rect x="50" y="180" width="800" height="120" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="205" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="200" y="235" text-anchor="middle" font-size="14" fill="#1e40af">APIPAY</text>
  <text x="350" y="235" text-anchor="middle" font-size="14" fill="#1e40af">LONPF9</text>
  <text x="500" y="235" text-anchor="middle" font-size="14" fill="#1e40af">LONPFC</text>
  <text x="650" y="235" text-anchor="middle" font-size="14" fill="#1e40af">LPPOFF</text>
  <text x="800" y="235" text-anchor="middle" font-size="14" fill="#1e40af">REPAYE</text>
  <text x="450" y="260" text-anchor="middle" font-size="12" fill="#3b82f6">Validation, Batch Processing, Payoff, Refunds, Exception Handling</text>
  
  <!-- Data Layer -->
  <rect x="50" y="330" width="800" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="355" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="250" y="380" text-anchor="middle" font-size="14" fill="#15803d">PAYMENT-FILE</text>
  <text x="450" y="380" text-anchor="middle" font-size="14" fill="#15803d">LEDGER-FILE</text>
  <text x="650" y="380" text-anchor="middle" font-size="14" fill="#15803d">HISTORY-FILE</text>
  <text x="450" y="400" text-anchor="middle" font-size="12" fill="#22c55e">Persistent Storage & Reporting</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="450" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="300" x2="450" y2="330" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**Summary:**  
OTIS supports comprehensive payment processing for loan accounts, including entry, validation, batch handling, payoffs, refunds, exception management, and reporting—integrating business rules and technical routines to ensure accurate, auditable, and flexible payment operations.

**Sources:**
- APIPAY, Index: flows
- LONPF9, Index: flows
- LPPOFF, Index: flows
- [[FILE:REPAYE.CBL|REPAYE.CBL]], Index: code
- [[FILE:LONPW9.CBL|LONPW9.CBL]], Index: code

### How does OTIS handle interest calculation and accrual for different loan types?

OTIS (Online Transaction Information System) handles interest calculation and accrual for different loan types using a sophisticated set of COBOL programs and copybooks that implement multiple interest formulas, accrual methods, and business rules tailored to each loan product. Here’s how the system manages this process:

---

**BUSINESS PURPOSE:**
- Accurately calculate interest charges and accrued interest for a wide variety of loan types (consumer, commercial, mortgage, revolving, etc.).
- Support regulatory requirements and institution-specific rules for interest assessment, rebates, and payoff scenarios.
- Ensure correct earnings recognition for both standard and special loan products.

---

**TECHNICAL IMPLEMENTATION:**

### 1. **Interest Calculation Formulas**
OTIS supports numerous interest calculation methods, each suited to different loan types:

- **Actuarial (Rule of 78, True Rebate, Proration):**
  - Used for consumer loans, installment loans, and some commercial products.
  - Example from [[FILE:LPAERN.CPY|LPAERN.CPY]]:
    ```
    * 58  (58) -  RULE 78    CASH LIQUIDATION IF PAID AHEAD OR
    *                           TIME (ACCRUAL) IF DELINQUENCY
    * 59  (59) -  REBATE     TRUE REBATE METHOD
    * 60       -  ACTUARIAL    (MERCURY)
    * 62       -  ACTUARIAL  PRORATION OF COMPONENT TO THE TOTAL YIELD
    ```
    (Source: [[FILE:LPAERN.CPY|LPAERN.CPY]], Index: code)

- **Simple Interest:**
  - Used for certain commercial and real estate loans.
  - Example logic:
    ```
    IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
       MOVE WKS-RATE TO IWA-RATE
       MOVE WKS-ANNUITY TO IWA-LOAN-ANNUITY
    ELSE
       PERFORM XBA-CALCULATE-SIMPLE-INTEREST
    ```
    (Source: [[FILE:CALCZL.CBL|CALCZL.CBL]], Lines 3776-3800)

- **Amortized Interest (Rate Type 'Z'):**
  - Special handling for real estate loans, computes interest on a 30-day method.
    ```
    * ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
    * INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
    * ARE DONE SIMPLE
    ```
    (Source: [[FILE:LPAPRS.CPY|LPAPRS.CPY]], Lines 51-75)

- **Special Cases (Judgment, Acceleration, Reduction):**
  - When loans are in judgment, accelerated, or reduced status, the system uses the date of the event to determine the correct interest assessed.
    ```
    * ADDED LOGIC TO USE THE CORRECT INTEREST ASSESSED THROUGH
    * DATE WHEN THE LOAN IS EXPIRED AND IN JUDGMENT,
    * ACCELERATED OR REDUCTION STATUS.
    ```
    (Source: [[FILE:LPRATE.CPY|LPRATE.CPY]], Lines 76-100)

### 2. **Accrual Logic**
- **Monthly/Periodic Accruals:**
  - Interest is accrued based on the loan’s schedule and payment history.
  - Special logic for paid-ahead, delinquent, or midstream purchase scenarios.
    ```
    * THE 1ST ACCRUAL WILL BE TAKEN AT THE END OF THE MONTH THE
    * SAC PERIOD EXPIRES.
    ```
    (Source: [[FILE:LPAERN.CPY|LPAERN.CPY]], Lines 226-250)

- **Handling of Add-ons and Upfront Charges:**
  - Upfront service charges and add-on interest are handled distinctly, especially for Interest Bearing (IB) loans.
    ```
    * NEED TO TAKE UPFRONT SERVICE CHARGE FOR IB'S
    UPD-INTYTD.
       IF SP-1098FRMLA = " " OR "B"
          COMPUTE LN-YTDINTPD = LN-ACCERN(1) + LN-ACCERN(2)
                              + LN-ACCERN(3) + LN-ACCERN(4)
    ```
    (Source: [[FILE:LONPB2.CBL|LONPB2.CBL]], Lines 926-950)

- **Maximum Accrual Days:**
  - For IB loans, the system enforces a maximum accrual period.
    ```
    * IB COPUTES A MAX ACCRUAL BASED ON SP-MAX-IB-ACR-DAYS. IF THIS IS 0,
    *   ITS DONE WITH THIS LOAN.
    ```
    (Source: [[FILE:RZ3DAY.CBL|RZ3DAY.CBL]], Lines 26-50)

### 3. **Program and Copybook Structure**
- **Core Programs:**
  - CALCZL: Central calculation engine for loan rates and payments.
  - LPAERN: Handles earnings and accrual logic for various formulas.
  - LPRATE: Manages rate calculations and special status handling.
  - LPPOFF/LFPPO: Handles payoff calculations, including interest refund/due for amortized loans.

- **Key Copybooks:**
  - [[FILE:LPAERN.CPY|LPAERN.CPY]] – Defines earnings formulas and accrual logic.
  - [[FILE:LPRATE.CPY|LPRATE.CPY]] – Contains rate calculation rules and status handling.
  - [[FILE:LPAPRS.CPY|LPAPRS.CPY]] – Manages rate types and special calculation methods.
  - [[FILE:LPCERN.CPY|LPCERN.CPY]] – Details actuarial and time-based interest calculations.

### 4. **Conditional Logic and Business Rules**
- The system uses IF/EVALUATE statements to select the correct formula and accrual method based on:
  - Loan type (consumer, commercial, mortgage, revolving, etc.)
  - Status (active, paid ahead, delinquent, judgment, accelerated, reduced)
  - Product-specific parameters (rate type, formula code, max accrual days)

---

**SUMMARY TABLE: Interest Calculation Methods by Loan Type**

| Loan Type         | Formula/Method         | Key Programs/Copybooks         | Special Handling                |
|-------------------|-----------------------|-------------------------------|---------------------------------|
| Consumer Loans    | Rule of 78, True Rebate, Actuarial | LPAERN, LPRATE, [[FILE:LPCERN.CPY|LPCERN.CPY]] | Paid ahead, delinquency logic   |
| Commercial Loans  | Simple Interest, Actuarial        | CALCZL, LPRATE, [[FILE:LPAPRS.CPY|LPAPRS.CPY]]  | Upfront charges, max accrual    |
| Mortgage Loans    | Amortized ('Z' Rate Type), Simple | LPPOFF, LFPPO, [[FILE:LPAPRS.CPY|LPAPRS.CPY]]   | 30-day method, payoff logic     |
| Revolving Credit  | Special IB formulas, Max accrual  | LPAERN, RZ3DAY, [[FILE:LONPB2.CBL|LONPB2.CBL]]  | Max IB accrual days enforced    |

---

**EXAMPLE CODE SNIPPET:**
```cobol
IF SP-ERNFRMLA = 58
   PERFORM RULE-78-CALCULATION
ELSE IF SP-ERNFRMLA = 59
   PERFORM TRUE-REBATE-CALCULATION
ELSE IF SP-ERNFRMLA = 60 OR 62
   PERFORM ACTUARIAL-CALCULATION
...
IF LN-LOANTYPE = "M" AND SP-CAL-RATETYPE = "Z"
   CALL 'LFPPO' USING PAYOFF-DATA
```
(Source: [[FILE:LPAERN.CPY|LPAERN.CPY]], [[FILE:LPAPRS.CPY|LPAPRS.CPY]], [[FILE:LFPPO.CBL|LFPPO.CBL]])

---

**CONCLUSION:**
OTIS’s interest calculation and accrual logic is highly configurable and supports a broad spectrum of loan products. It selects the appropriate formula and accrual method based on loan type, status, and business rules, ensuring accurate financial processing for each account.

---

**Here's a visual representation:**

<svg width="900" height="520" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Interest Calculation & Accrual Flow</text>
  
  <!-- Loan Types -->
  <rect x="40" y="70" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="130" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Consumer Loans</text>
  <rect x="260" y="70" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="350" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Commercial Loans</text>
  <rect x="480" y="70" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="570" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Mortgage Loans</text>
  <rect x="700" y="70" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="790" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Revolving Credit</text>
  
  <!-- Calculation Methods -->
  <rect x="130" y="170" width="140" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="200" y="200" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Rule of 78</text>
  <rect x="350" y="170" width="140" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="420" y="200" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Actuarial</text>
  <rect x="570" y="170" width="140" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="640" y="200" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Simple Interest</text>
  <rect x="790" y="170" width="140" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="860" y="200" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Amortized</text>
  
  <!-- Programs -->
  <rect x="200" y="270" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="260" y="300" text-anchor="middle" font-size="13" font-weight="bold" fill="white">LPAERN</text>
  <rect x="350" y="270" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="410" y="300" text-anchor="middle" font-size="13" font-weight="bold" fill="white">LPRATE</text>
  <rect x="500" y="270" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="560" y="300" text-anchor="middle" font-size="13" font-weight="bold" fill="white">CALCZL</text>
  <rect x="650" y="270" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="710" y="300" text-anchor="middle" font-size="13" font-weight="bold" fill="white">LPPOFF/LFPPO</text>
  
  <!-- Data Layer -->
  <rect x="350" y="370" width="200" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="400" text-anchor="middle" font-size="14" font-weight="bold" fill="#15803d">Loan Account Files</text>
  <text x="450" y="420" text-anchor="middle" font-size="12" fill="#22c55e">Accrued Interest, Payment History</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <!-- Loan Types to Calculation Methods -->
  <line x1="130" y1="130" x2="200" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="130" x2="420" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="570" y1="130" x2="640" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="790" y1="130" x2="860" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <!-- Calculation Methods to Programs -->
  <line x1="200" y1="230" x2="260" y2="270" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="420" y1="230" x2="410" y2="270" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="640" y1="230" x2="560" y2="270" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="860" y1="230" x2="710" y2="270" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <!-- Programs to Data Layer -->
  <line x1="260" y1="330" x2="450" y2="370" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="410" y1="330" x2="450" y2="370" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="560" y1="330" x2="450" y2="370" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="710" y1="330" x2="450" y2="370" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Source Citations:**
- [[FILE:LPAERN.CPY|LPAERN.CPY]], Index: code (interest formulas and accrual logic)
- [[FILE:LPRATE.CPY|LPRATE.CPY]], Index: code (rate calculation and status handling)
- [[FILE:LPAPRS.CPY|LPAPRS.CPY]], Index: code (rate type 'Z' for amortized interest)
- [[FILE:CALCZL.CBL|CALCZL.CBL]], Lines 3776-3800 (simple interest calculation)
- [[FILE:LONPB2.CBL|LONPB2.CBL]], Lines 926-950 (upfront charges and IB accrual)
- [[FILE:RZ3DAY.CBL|RZ3DAY.CBL]], Lines 26-50 (max accrual days for IB loans)
- [[FILE:LFPPO.CBL|LFPPO.CBL]], Lines 1-25 (payoff interest calculation for amortized loans)

If you need details on a specific loan type or formula, let me know!

### What escrow account management functions does OTIS support?

OTIS supports comprehensive escrow account management functions for loan accounts, enabling financial institutions to handle all aspects of escrow throughout the loan lifecycle. Here’s a summary of the key escrow management capabilities provided by OTIS:

---

**BUSINESS PURPOSE:**
- **Escrow Collection:** OTIS tracks and collects escrow payments from borrowers as part of regular loan payments. These funds are set aside for specific purposes such as property taxes, insurance, and mortgage insurance premiums.
- **Escrow Disbursement:** The system manages disbursement of escrow funds to pay third-party obligations (e.g., tax authorities, insurance companies) when due.
- **Escrow Balance Tracking:** OTIS maintains detailed records of escrow balances, scheduled escrow amounts, amounts collected, and amounts disbursed for each loan account.
- **Escrow Reversal & Adjustment:** Supports reversal of escrow transactions (e.g., when payments are reversed or corrected), and adjustment of escrow balances when necessary.
- **Prepaid Escrow Handling:** Manages prepaid escrow transactions, allowing for early or additional escrow payments and updating account records accordingly.
- **Escrow Status & Flags:** Tracks escrow status flags (active/inactive), outstanding scheduled escrow, and changes to escrow status based on account activity.
- **Error Handling & Messaging:** Provides error messages and prompts when escrow records are missing, balances are insufficient, or transactions are invalid (e.g., attempting to post escrow transactions to accounts without escrow).

---

**TECHNICAL IMPLEMENTATION:**

OTIS implements these functions using a combination of COBOL programs and data structures:

- **Key Programs:**
  - **ESCROW:** Central program for escrow management (Source: [[FILE:ESCROW|ESCROW]], [[FILE:I|I]]ndex: programs)
  - **LONPF2, LONPFC, LONPF1, LONPQ1, LONPFE:** Loan processing programs with embedded escrow logic

- **Core Data Structures:**
  - **ES-ESCROW-SCHEDULE ([[FILE:LP01ES.CPY|LP01ES.CPY]]):** Tracks scheduled escrow payments, effective dates, first payment dates, amounts, terms, and collected amounts (Source: [[FILE:LP01ES.CPY|LP01ES.CPY]], Index: code)
  - **ES-BALANCE:** Current escrow balance for the account
  - **ES-STAT-TABLE:** Tracks prepaid, collected, and disbursed escrow amounts for YTD, last YTD, and loan-to-date (Source: [[FILE:LP01ES.CPY|LP01ES.CPY]], Index: code)

- **Main Functions:**
  - **Opening/Reading/Writing Escrow Records:**
    - Programs perform OPEN-ES1-FILE, READ-ES1-FILE, REWRITE-ES1-FILE, and CLOSE-ES1-FILE to manage escrow data (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Index: code)
  - **Applying Payments to Escrow:**
    - Adds payment amounts to escrow balance and updates collected amounts (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9626-9650)
  - **Disbursing Escrow Funds:**
    - Subtracts disbursed amounts from escrow balance and updates disbursed counters (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9726-9750)
  - **Escrow Reversal:**
    - Handles reversal of escrow transactions when needed (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9651-9675)
  - **Prepaid Escrow Processing:**
    - Manages prepaid escrow transactions and updates account records (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9751-9775)
  - **Outstanding Escrow Calculation:**
    - Computes outstanding scheduled escrow and flags accounts for changes if necessary (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9676-9700)
  - **Validation & Error Messaging:**
    - Checks for missing escrow records, invalid transactions, and provides user feedback (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9626-9650; [[FILE:LONPF1.CBL|LONPF1.CBL]], Lines 2251-2275)

- **Escrow Status Flags:**
  - **LN-ESCROW-FG:** Indicates if escrow is active ("Y") or inactive ("I")
  - **CHANGE-ESCROW-FG:** Set when outstanding escrow is detected on inactive accounts

- **User Interaction:**
  - Displays escrow information on screens, prompts for acceptance or override of escrow amounts, and requires password verification for overrides (Source: [[FILE:LONPFC.CBL|LONPFC.CBL]], Lines 4301-4325)

---

**EXAMPLE CODE SNIPPETS:**

- **Applying Payment to Escrow:**
  ```cobol
  ADD LP-APPLIED-TO-ESCROW TO ES-BALANCE.
  ADD LP-APPLIED-TO-ESCROW TO ES-COLLECTED(1)
                             ES-COLLECTED(3).
  ```

- **Disbursing Escrow Funds:**
  ```cobol
  SUBTRACT LP-TRAMT FROM ES-BALANCE.
  ADD LP-TRAMT TO ES-DISBURSED-TAXES(1)
                 ES-DISBURSED-TAXES(3).
  ```

- **Calculating Outstanding Escrow:**
  ```cobol
  COMPUTE ESCROW-OUTSTANDING = ESCROW-OUTSTANDING +
     ES-SCHD-ESCROW(SUB) - ES-SCHD-COLLECTED(SUB)
  ```

- **Error Handling:**
  ```cobol
  IF IO-BAD
     MOVE "ERROR: ESCROW RECORD NOT FOUND FOR ACCOUNT!" TO MESS
     PERFORM SEND-MESS
  ```

---

**SOURCE REFERENCES:**
- ESCROW program (Source: [[FILE:ESCROW|ESCROW]], [[FILE:I|I]]ndex: programs)
- [[FILE:LONPF2.CBL|LONPF2.CBL]] (Lines 9626-9700, 9726-9775) – Escrow application, disbursement, reversal, prepaid handling
- [[FILE:LP01ES.CPY|LP01ES.CPY]] (Lines 26-68) – Escrow schedule and status table definitions
- [[FILE:LONPF1.CBL|LONPF1.CBL]] (Lines 2251-2275, 2426-2450) – Escrow transaction validation and error messaging
- [[FILE:LONPFC.CBL|LONPFC.CBL]] (Lines 4301-4325) – User acceptance and override of escrow amounts

---

**VISUAL REPRESENTATION:**

Here's a visual diagram of the escrow management flow in OTIS:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Escrow Account Management Flow</text>
  
  <!-- Loan Payment -->
  <rect x="50" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Loan Payment</text>
  <text x="140" y="128" text-anchor="middle" fill="white" font-size="12">Borrower submits payment</text>
  
  <!-- Escrow Application -->
  <rect x="270" y="80" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Apply to Escrow</text>
  <text x="360" y="128" text-anchor="middle" fill="white" font-size="12">Update ES-BALANCE, ES-COLLECTED</text>
  
  <!-- Escrow Disbursement -->
  <rect x="490" y="80" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Disburse Escrow</text>
  <text x="580" y="128" text-anchor="middle" fill="white" font-size="12">Pay taxes/insurance</text>
  
  <!-- Escrow Reversal -->
  <rect x="710" y="80" width="140" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="780" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Reversal</text>
  <text x="780" y="128" text-anchor="middle" fill="white" font-size="12">Correct/undo transaction</text>
  
  <!-- Prepaid Escrow -->
  <rect x="270" y="180" width="180" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="360" y="210" text-anchor="middle" fill="#1e40af" font-size="16" font-weight="bold">Prepaid Escrow</text>
  <text x="360" y="228" text-anchor="middle" fill="#1e40af" font-size="12">Early/additional payments</text>
  
  <!-- Escrow Status -->
  <rect x="490" y="180" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="580" y="210" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Escrow Status</text>
  <text x="580" y="228" text-anchor="middle" fill="#15803d" font-size="12">Active/inactive, outstanding</text>
  
  <!-- Error Handling -->
  <rect x="710" y="180" width="140" height="60" fill="#fca5a5" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="780" y="210" text-anchor="middle" fill="#dc2626" font-size="16" font-weight="bold">Error Handling</text>
  <text x="780" y="228" text-anchor="middle" fill="#dc2626" font-size="12">Missing/invalid records</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="670" y1="110" x2="710" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="140" x2="360" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="140" x2="580" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="780" y1="140" x2="780" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
OTIS provides end-to-end escrow account management for loans, including collection, disbursement, reversal, prepaid handling, status tracking, and robust error handling. These functions are tightly integrated into the loan processing workflow, ensuring compliance and accuracy for all escrow-related activities.

If you need details on a specific escrow function or want to see the code for a particular process, let me know!

### How does OTIS process late fees, penalties, and default scenarios?

OTIS processes late fees, penalties, and default scenarios using a coordinated set of COBOL routines and copybooks that implement complex business rules for loan servicing. Here’s a breakdown of how these functions are handled:

---

**BUSINESS PURPOSE**

- **Late Fees:** OTIS automatically assesses late charges when payments are overdue, using formulas tailored to loan type, payment history, and regulatory requirements.
- **Penalties:** Additional charges or adjustments are applied for specific delinquency or default conditions, such as exceeding grace periods or failing to meet contractual obligations.
- **Default Scenarios:** The system tracks loans that enter default status, recalculates amounts owed, and applies appropriate penalties and late charges, ensuring compliance and accurate accounting.

---

**TECHNICAL IMPLEMENTATION**

### 1. **Late Fee Assessment**

- **Routine:** The main logic for late charge calculation is in the copybook `[[FILE:LPLCHG.CPY|LPLCHG.CPY]]` and the apply routine `[[FILE:LPLCAP.CPY|LPLCAP.CPY]]`.
- **Process:**
  - The system checks if a payment is late by comparing payment dates to due dates and grace periods.
  - If late, it calculates the late charge using formulas based on loan type (`SP-LCTYPE`), amount overdue, and maintenance fees.
  - Example logic (Source: [[FILE:LPLCHG.CPY|LPLCHG.CPY]], Lines 176-200):
    ```cobol
    IF SP-LCTYPE = "O"
       IF LCHG-LATE > (LN-REGPYAMT - LN-MAINTFEE)
          COMPUTE LCHG-LATE = LN-REGPYAMT - LN-MAINTFEE.

    IF SP-LCTYPE = "N"
       COMPUTE LCHG-TRUNC = LCHG-LATE - LN-MAINTFEE
       COMPUTE LCHG-CHARGE ROUNDED = LCHG-TRUNC * SP-LCRATE /100.
    ```
    - This shows different formulas for different loan types.

- **Assessment Checks:** Before applying a late charge, the system performs checks:
  - Is the payment past maturity? (Source: [[FILE:LPLCAP.CPY|LPLCAP.CPY]], Lines 751-775)
  - Has the late charge already been brought up to date?
  - Is the amount overdue less than the delinquency factor?

### 2. **Penalty Application**

- **Routine:** Penalties are managed in routines like `[[FILE:PENALT.CBL|PENALT.CBL]]` (uses `[[FILE:GETFMW.CPY|GETFMW.CPY]]` for penalty formulas).
- **Process:**
  - Penalties may be assessed for specific events, such as default or repeated late payments.
  - The system computes penalty amounts and applies them to the loan balance.

### 3. **Default Handling**

- **Routine:** Default scenarios are processed in routines such as `[[FILE:LPPOFF.CBL|LPPOFF.CBL]]` and `[[FILE:LPLCAP.CPY|LPLCAP.CPY]]`.
- **Process:**
  - When a loan enters default, the system recalculates all amounts due, including principal, interest, late charges, and penalties.
  - Example logic (Source: [[FILE:LPPOFF.CBL|LPPOFF.CBL]], Lines 401-425):
    ```cobol
    IF IBPC-FG = "P" AND IBPC-EXPIRED-FLAG = " "
       PERFORM PAYOFF-SET-INT-CHARGES
       PERFORM PAYOFF-SET-LATE-CHARGES.
    ```
    - This shows how late charges and interest are recalculated for payoffs in default scenarios.

- **Late Charge Calculation in Payoff:** (Source: [[FILE:LPPOFF.CBL|LPPOFF.CBL]], Lines 676-700)
    ```cobol
    PAYOFF-SET-LATE-CHARGES SECTION.
       MOVE POFF-PAYDATE TO LCAP-PAYDATE.
       MOVE LN-1STPYDATE TO LCAP-1STPYDATE.
       MOVE "AL" TO LCAP-LPTRCD.
    ```
    - The system sets up the context for late charge calculation during payoff.

### 4. **Business Rule Documentation**

- The routines include extensive comments documenting regulatory and business policy changes, such as:
  - "ASSESS A LATE CHARGE EVEN IF ACCOUNT IS PAID AHEAD" (Source: [[FILE:LPLCAP.CPY|LPLCAP.CPY]], Lines 126-150)
  - "CAN'T TAKE LATE CHARGE ON MONIES PAID WITHIN GRACE DAYS OF DUE DAY" (Source: [[FILE:LONPW9.CBL|LONPW9.CBL]], Lines 2601-2625)

---

**KEY PROGRAMS AND COPYBOOKS INVOLVED**

| Program/Copybook      | Description                                                        |
|-----------------------|--------------------------------------------------------------------|
| [[FILE:LPLCAP.CPY|LPLCAP.CPY]]            | Late charge apply routine: determines late charge, deficiency, etc.|
| [[FILE:LPLCHG.CPY|LPLCHG.CPY]]            | Late charge calculation formulas for different loan types.          |
| [[FILE:LPPOFF.CBL|LPPOFF.CBL]]            | Payoff routine: recalculates charges and penalties at payoff.      |
| [[FILE:PENALT.CBL|PENALT.CBL]]            | Penalty calculation and application logic.                         |
| [[FILE:GETFMW.CPY|GETFMW.CPY]]            | Penalty formula definitions.                                       |

---

**PROCESS FLOW SUMMARY**

1. **Payment Due Date Reached**
2. **System Checks for Delinquency**
   - If payment is late, triggers late charge calculation.
3. **Late Charge Calculation**
   - Uses formulas based on loan type and amount overdue.
4. **Penalty Assessment**
   - Applies additional penalties if default or other conditions met.
5. **Default Scenario**
   - Recalculates all amounts due, including late charges and penalties.
6. **Updates Loan Account**
   - Adjusts balances, records charges, and updates paid-thru dates.

---

**Here's a visual representation:**

<svg width="900" height="520" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Late Fee, Penalty, and Default Processing</text>
  
  <!-- Step 1: Payment Due -->
  <rect x="60" y="70" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Payment Due Date</text>
  
  <!-- Step 2: Delinquency Check -->
  <rect x="280" y="70" width="180" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="370" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Delinquency Check</text>
  
  <!-- Step 3: Late Charge Calculation -->
  <rect x="500" y="70" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="590" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Late Charge Calculation</text>
  
  <!-- Step 4: Penalty Assessment -->
  <rect x="720" y="70" width="120" height="60" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="780" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">Penalty Assessment</text>
  
  <!-- Step 5: Default Scenario -->
  <rect x="280" y="180" width="180" height="60" fill="#fee2e2" stroke="#ef4444" stroke-width="3" rx="8"/>
  <text x="370" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#b91c1c">Default Scenario</text>
  
  <!-- Step 6: Account Update -->
  <rect x="500" y="180" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="590" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Account Update</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="240" y1="100" x2="280" y2="100" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="460" y1="100" x2="500" y2="100" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="680" y1="100" x2="720" y2="100" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="780" y1="130" x2="370" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="460" y1="210" x2="500" y2="210" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**SOURCE REFERENCES:**
- [[FILE:LPLCAP.CPY|LPLCAP.CPY]] – Late charge apply logic and business rules (Source: [[FILE:LPLCAP.CPY|LPLCAP.CPY]], Index: code)
- [[FILE:LPLCHG.CPY|LPLCHG.CPY]] – Late charge calculation formulas (Source: [[FILE:LPLCHG.CPY|LPLCHG.CPY]], Index: code)
- [[FILE:LPPOFF.CBL|LPPOFF.CBL]] – Payoff and default scenario handling (Source: [[FILE:LPPOFF.CBL|LPPOFF.CBL]], Index: code)
- [[FILE:PENALT.CBL|PENALT.CBL]] – Penalty calculation logic (Source: [[FILE:PENALT.CBL|PENALT.CBL]], Index: copybook_usage)
- [[FILE:GETFMW.CPY|GETFMW.CPY]] – Penalty formula definitions (Source: [[FILE:PENALT.CBL|PENALT.CBL]], Index: copybook_usage)

---

**Summary:**  
OTIS uses specialized routines to assess late fees, apply penalties, and handle default scenarios for loan accounts. The system ensures charges are calculated accurately according to business rules, loan type, and regulatory requirements, updating loan records and maintaining compliance throughout the process.

### What financial reporting and regulatory compliance features exist in OTIS?

OTIS (Online Transaction Information System) provides robust financial reporting and regulatory compliance features to support the needs of financial institutions. These features are implemented through a combination of COBOL programs, copybooks, and business logic designed to meet industry standards and legal requirements.

**Financial Reporting Features in OTIS:**

- **Branch and Loan Detail Reports:**  
  Programs like **DBREPT** generate detailed reports on loan activity, branch totals, and grand totals, supporting internal financial analysis and audit trails.  
  (Source: [[FILE:DBREPT|DBREPT]], [[FILE:I|I]]ndex: flows)

- **Accrual and Interest Reporting:**  
  Specialized screens and reports (e.g., those using **[[FILE:BRMAN3_SCN.CPY|BRMAN3_SCN.CPY]]**) provide options for accrual methods, full-month calculations, and interest allocation, ensuring accurate financial statements.  
  (Source: [[FILE:BRMAN3_SCN.CPY|BRMAN3_SCN.CPY]], Index: code)

- **Rebate and Unearned Income Reports:**  
  Programs such as **REBATE** and copybooks like **[[FILE:REBATE.CPY|REBATE.CPY]]** are used to calculate and report rebates, unearned insurance, and related financial metrics, which are critical for compliance and profitability analysis.  
  (Source: [[FILE:REBATE|REBATE]], [[FILE:I|I]]ndex: flows; [[FILE:REBATE.CPY|REBATE.CPY]], Index: copybook_usage)

- **Comparative File Analysis:**  
  The **TYFILE** program compares different versions of financial files (e.g., S35 vs. A15 TYFILE) to identify discrepancies, supporting reconciliation and audit processes.  
  (Source: [[FILE:TYFILE.CBL|TYFILE.CBL]], Index: code)

- **General Ledger and Trial Balance Reports:**  
  Programs and copybooks (e.g., **[[FILE:GBGR1RN.CPY|GBGR1RN.CPY]]**) support the generation of trial balances, general ledger extracts, and other accounting reports required for financial oversight.  
  (Source: [[FILE:MBALR6H.CBL|MBALR6H.CBL]], Index: copybook_usage)

**Regulatory Compliance Features in OTIS:**

- **Credit Bureau Reporting:**  
  The **CHTRWF** program is dedicated to credit bureau reporting, ensuring that loan and account data is transmitted in compliance with FCRA and FCBA regulations.  
  (Source: [[FILE:CHTRWF.CBL|CHTRWF.CBL]], Index: code)

- **Compliance Condition Codes:**  
  Copybooks like **[[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]** define compliance condition codes (e.g., XA, XB, XC, XD, XE) that are required for legal compliance with consumer protection laws. These codes are used to flag accounts for specific regulatory statuses.  
  (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code)

- **Audit Trails and Change Logs:**  
  Programs such as **JOURNL** maintain audit logs of financial transactions, supporting regulatory requirements for traceability and accountability.  
  (Source: [[FILE:JOURNL|JOURNL]], [[FILE:I|I]]ndex: variables)

- **Insurance and Rebate Compliance:**  
  Programs like **FEUNR1** and related copybooks ensure that unearned insurance and rebate calculations meet regulatory standards, including proper GL entries and reporting for compliance audits.  
  (Source: [[FILE:FEUNR1.CBL|FEUNR1.CBL]], Index: code)

- **Password and Access Controls:**  
  Security programs (e.g., **PASSWD**) enforce password requirements and access controls, which are essential for compliance with data protection regulations.  
  (Source: [[FILE:PASSWD|PASSWD]], [[FILE:I|I]]ndex: flows)

**Example of Compliance Condition Codes (from [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]):**
- XA: Account closed at consumer's request
- XB: Account info disputed by consumer (FCRA)
- XC: Completed investigation of FCRA dispute, customer disagrees
- XD: Account closed at consumer's request and in dispute under FCRA
- XE: Account closed/consumer request/dispute

**Summary Table of Key Programs and Copybooks:**

| Program/Copybook         | Description / Compliance Role                                  |
|--------------------------|---------------------------------------------------------------|
| DBREPT                   | Branch/loan reporting, totals, audit support                  |
| REBATE                   | Rebate and unearned income reporting                          |
| TYFILE                   | File comparison for reconciliation/audit                      |
| [[FILE:GBGR1RN.CPY|GBGR1RN.CPY]]              | General ledger/trial balance reporting                        |
| CHTRWF                   | Credit bureau reporting (FCRA/FCBA compliance)                |
| [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]           | Compliance condition codes for regulatory reporting           |
| JOURNL                   | Audit trail and transaction logging                           |
| FEUNR1                   | Unearned insurance reporting, GL compliance                   |
| PASSWD                   | Password/access control for data protection                   |

**Business Impact:**
- Ensures accurate financial statements and internal controls
- Supports legal and regulatory reporting requirements (FCRA, FCBA, insurance regulations)
- Provides audit trails and reconciliation tools for compliance audits
- Enables secure, traceable financial operations

---

**Here's a visual representation of how OTIS supports financial reporting and regulatory compliance:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Financial Reporting & Compliance Architecture</text>
  
  <!-- Reporting Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Reporting Layer</text>
  <text x="450" y="125" text-anchor="middle" font-size="14" fill="#4338ca">DBREPT, REBATE, TYFILE, [[FILE:GBGR1RN.CPY|GBGR1RN.CPY]]</text>
  
  <!-- Compliance Layer -->
  <rect x="50" y="180" width="800" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="210" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Compliance Layer</text>
  <text x="450" y="235" text-anchor="middle" font-size="14" fill="#1e40af">CHTRWF, [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], FEUNR1, JOURNL, PASSWD</text>
  
  <!-- Data Layer -->
  <rect x="50" y="290" width="800" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="320" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="450" y="345" text-anchor="middle" font-size="14" fill="#15803d">Loan Files, GL Files, Compliance Codes, Audit Logs</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="450" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="260" x2="450" y2="290" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**Sources:**
- DBREPT, Index: flows, programs, copybook_usage
- REBATE, Index: flows
- [[FILE:TYFILE.CBL|TYFILE.CBL]], Index: code
- [[FILE:GBGR1RN.CPY|GBGR1RN.CPY]], Index: copybook_usage
- [[FILE:CHTRWF.CBL|CHTRWF.CBL]], Index: code
- [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code
- JOURNL, Index: variables
- [[FILE:FEUNR1.CBL|FEUNR1.CBL]], Index: code
- PASSWD, Index: flows

If you need details on a specific report, compliance code, or regulatory logic, let me know!

### How does OTIS manage loan modifications, refinancing, and restructuring?

OTIS manages loan modifications, refinancing, and restructuring through a set of specialized COBOL programs and routines that guide users through the process, enforce business rules, and update all relevant loan records. Here’s a detailed breakdown of how these functions are implemented:

---

**BUSINESS PURPOSE**

- **Loan Modifications:** OTIS allows users to change loan terms (e.g., interest rate, payment schedule, maturity date) in response to borrower requests or financial hardship.
- **Refinancing:** The system supports payoff and re-issuance of loans, updating principal, interest, and other terms as required.
- **Restructuring:** OTIS can adjust loans due to default, regulatory requirements, or negotiated settlements, including changes to payment plans, interest rates, and fees.

---

**TECHNICAL IMPLEMENTATION**

### 1. **User Workflow**

- Users initiate modifications via dedicated screens (e.g., RESCHD, LONPS4, LONPF3).
- The system prompts for new terms, validates input, and enforces constraints (such as allowable changes and regulatory limits).

### 2. **Core Programs and Flow**

- **RESCHD (Reschedule/Restructure):**
  - Handles entry of new loan terms, payment schedules, and restructuring details.
  - Flow includes modules for resetting forms, entering keys, updating files, and scanning borrower loans.
  - **Source:** RESCHD, Index: flows

- **LONPS4 (Loan Change Processing):**
  - Manages changes to loan records, including promised payments and actions.
  - Flow includes routines for modifying messages, promised amounts, and actions.
  - **Source:** LONPS4, Index: flows

- **LONPF3 (Payment File Modification):**
  - Supports changes to payment files and loan records.
  - Flow includes routines for displaying transaction records, restoring screens, and processing loan changes.
  - **Source:** LONPF3, Index: flows

- **LPPOFF (Payoff and Refinancing):**
  - Handles loan payoff calculations, rebate processing, and creation of new loan records for refinancing.
  - Includes routines for setting charges, calculating rebates, and updating loan status.
  - **Source:** LPPOFF, Index: flows

### 3. **Business Rule Enforcement**

- **Validation:** Each program enforces business rules such as:
  - Only certain fields can be modified (e.g., interest rate, payment amount).
  - Regulatory checks (e.g., maximum allowable interest rate, required disclosures).
  - Restrictions on changes (e.g., cannot modify loan date or proceeds after origination).
  - **Example:** In LONPG5, code comments specify that changes to SRCD, LOANDATE, and PROCEEDS are not allowed during modification.
    - *Source: [[FILE:LONPG5.CBL|LONPG5.CBL]], Lines 451-475*

- **Element Specifications:** Programs use MAX and SPEC-TABLE constructs to limit the number and type of modifications per transaction.
  - *Example:* CHDUAL, CHORIG, CHDEBT all use a MAX value to restrict how many elements (fields) can be changed in a single operation.
  - *Source: [[FILE:CHDUAL.CBL|CHDUAL.CBL]], Lines 126-150; [[FILE:CHORIG.CBL|CHORIG.CBL]], Lines 101-125; [[FILE:CHDEBT.CBL|CHDEBT.CBL]], Lines 101-125*

### 4. **File and Data Updates**

- Upon successful validation, the system updates loan master files, payment schedules, and audit logs.
- All changes are tracked for compliance and reporting.

### 5. **Audit and Compliance**

- Modifications trigger updates to audit trails and compliance reports, ensuring regulatory requirements are met.
- Programs like LONPF3 and LPPOFF integrate with reporting modules to reflect changes in financial statements.

---

**EXAMPLE WORKFLOW NARRATIVE**

1. **User selects “Loan Modification” from the OTIS menu.**
2. **RESCHD screen appears:** User enters new terms (e.g., payment amount, due date).
3. **System validates input:** Only allowed fields can be changed; regulatory checks are performed.
4. **LONPS4 processes changes:** Updates loan record, recalculates payment schedule, logs changes.
5. **LPPOFF handles refinancing:** If loan is paid off and reissued, calculates payoff, creates new loan record.
6. **Audit trail updated:** All changes are logged for compliance.
7. **Reporting modules reflect changes:** Financial and regulatory reports are updated.

---

**KEY PROGRAMS INVOLVED**

| Program   | Purpose                                              |
|-----------|-----------------------------------------------------|
| RESCHD    | Loan rescheduling/restructuring entry and validation|
| LONPS4    | Loan change processing (promises, actions)          |
| LONPF3    | Payment file and loan record modification           |
| LPPOFF    | Payoff, refinancing, and new loan creation          |
| CHDUAL    | Dual loan adjustment logic                          |
| CHORIG    | Loan origination change logic                       |
| CHDEBT    | Debt modification logic                             |

---

**SOURCE REFERENCES**

- RESCHD, Index: flows
- LONPS4, Index: flows
- LONPF3, Index: flows
- LPPOFF, Index: flows
- [[FILE:LONPG5.CBL|LONPG5.CBL]], Lines 451-475
- [[FILE:CHDUAL.CBL|CHDUAL.CBL]], Lines 126-150
- [[FILE:CHORIG.CBL|CHORIG.CBL]], Lines 101-125
- [[FILE:CHDEBT.CBL|CHDEBT.CBL]], Lines 101-125

---

**Here's a visual representation:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Loan Modification & Restructuring Flow</text>
  
  <!-- User Action -->
  <rect x="50" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">User Initiates</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="12">Modification/Refinance</text>
  
  <!-- RESCHD -->
  <rect x="270" y="80" width="140" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="340" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">RESCHD</text>
  <text x="340" y="130" text-anchor="middle" fill="white" font-size="12">Entry/Validation</text>
  
  <!-- LONPS4 -->
  <rect x="440" y="80" width="140" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="510" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">LONPS4</text>
  <text x="510" y="130" text-anchor="middle" fill="white" font-size="12">Change Processing</text>
  
  <!-- LONPF3 -->
  <rect x="610" y="80" width="140" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="680" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">LONPF3</text>
  <text x="680" y="130" text-anchor="middle" fill="white" font-size="12">File Modification</text>
  
  <!-- LPPOFF -->
  <rect x="780" y="80" width="100" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="830" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">LPPOFF</text>
  <text x="830" y="130" text-anchor="middle" fill="white" font-size="12">Payoff/Refinance</text>
  
  <!-- Audit/Reporting -->
  <rect x="340" y="220" width="220" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="450" y="250" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Audit & Reporting</text>
  <text x="450" y="270" text-anchor="middle" fill="white" font-size="12">Compliance/Financials</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="410" y1="110" x2="440" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="110" x2="610" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="750" y1="110" x2="780" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="340" y1="140" x2="450" y2="220" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="510" y1="140" x2="450" y2="220" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="680" y1="140" x2="450" y2="220" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="830" y1="140" x2="450" y2="220" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
OTIS provides a structured, rule-driven process for loan modifications, refinancing, and restructuring, ensuring compliance, auditability, and accurate record-keeping. The process is managed by dedicated COBOL programs that guide users, validate changes, and update all necessary files and reports.

### What collections and delinquency management processes does OTIS support?

OTIS supports a comprehensive set of collections and delinquency management processes designed for loan servicing and financial institutions. These processes are implemented through specialized COBOL programs, copybooks, and business logic that automate the tracking, reporting, and resolution of delinquent accounts.

### Collections and Delinquency Management Processes in OTIS

#### 1. **Delinquency Calculation and Tracking**
- **Arrearage and Delinquency Computation:**  
  OTIS calculates arrearages and delinquency status for each loan account using routines that consider payment schedules, current balances, and contractual terms.
  - Example:  
    - **LPTDEL** and **LPCDEL** programs and copybooks contain logic to compute account arrearage and set delinquency flags.  
      - Comments show ongoing enhancements to accurately produce arrearage and handle maturity checks.  
      - (Source: [[FILE:LPTDEL.CPY|LPTDEL.CPY]], [[FILE:LPCDEL.CPY|LPCDEL.CPY]], Index: code)
  - **Past Due Days Calculation:**  
    - **LPPDUE** computes the number of days an account is past due, supporting aging analysis for collections.  
      - (Source: [[FILE:LPPDUE|LPPDUE]], [[FILE:I|I]]ndex: flows)

#### 2. **Collections Workflow and Reporting**
- **Collections Assist Screens:**  
  - **COLAST** provides a user interface for collections staff to view and manage delinquent accounts, integrating with inquiry and payment posting workflows.  
    - (Source: [[FILE:COLAST.CBL|COLAST.CBL]], Index: code)
- **Collection Letter Generation:**  
  - **COLLET** automates the creation of collection letters for delinquent accounts, including date range checks and totals routines.  
    - (Source: [[FILE:COLLET.CBL|COLLET.CBL]], Index: code)
- **Delinquency Reports:**  
  - **COLRPT** and **COLRP2** generate detailed reports on delinquent accounts, including worked account flags and collector activity tracking.  
    - (Source: [[FILE:COLRPT.CBL|COLRPT.CBL]], Index: code)

#### 3. **Business Rule Enforcement**
- **Allowable Delinquency Policy Validation:**  
  - OTIS enforces maximum allowable delinquency policies, blocking actions when thresholds are exceeded.
    - Example:  
      - **LONPF1** validates against maximums and sets error messages if limits are breached.  
      - (Source: [[FILE:LONPF1.CBL|LONPF1.CBL]], Index: code)
- **Contractual vs. Actual Delinquency:**  
  - **LPCDEL** distinguishes between contractual delinquency (based on payment schedule) and actual arrearage, ensuring accurate status assignment.  
    - (Source: [[FILE:LPCDEL.CPY|LPCDEL.CPY]], Index: code)

#### 4. **Special Handling Scenarios**
- **Bankruptcy and Non-Accrual Accounts:**  
  - Programs like **PROGRP** adjust delinquency calculations for bankrupt accounts, ensuring correct reporting and compliance.  
    - (Source: [[FILE:PROGRP.CBL|PROGRP.CBL]], Index: code)
- **Bulk Purchase and Repo Handling:**  
  - Logic in **LONIF1** manages delinquency updates for bulk purchase transactions and repossessed accounts, reversing out certain balances as needed.  
    - (Source: [[FILE:LONIF1.CBL|LONIF1.CBL]], Index: code)

#### 5. **Dealer and Addon Delinquency**
- **Dealer Delinquency Management:**  
  - Programs such as **RZMALL** and **BWVDEL** handle delinquency tracking for dealer accounts, including special routines for non-accrual and outstanding dealer stats.  
    - (Source: [[FILE:RZMALL.CBL|RZMALL.CBL]], BWVDEL, Index: code/flows)
- **Addon Contract Delinquency:**  
  - **LPDLCL** and related routines manage delinquency for addon contracts, ensuring all components of a loan are tracked.  
    - (Source: [[FILE:LPDLCL|LPDLCL]], [[FILE:I|I]]ndex: flows)

#### 6. **Integration with Other Loan Servicing Functions**
- **Interaction with Payment Posting and Inquiries:**  
  - Collections routines are integrated with payment posting (e.g., F11 from posting date triggers collections assist screens) and account inquiry modules for seamless workflow.  
    - (Source: [[FILE:COLAST.CBL|COLAST.CBL]], Index: code)

---

### Visual Representation: Collections & Delinquency Management Architecture

Here's a visual representation of the key components and their relationships:

<svg width="900" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Collections & Delinquency Management</text>
  
  <!-- Collections Workflow Layer -->
  <rect x="50" y="70" width="800" height="100" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Collections Workflow</text>
  <text x="200" y="130" text-anchor="middle" font-size="14" fill="#4338ca">COLAST (Assist Screen)</text>
  <text x="450" y="130" text-anchor="middle" font-size="14" fill="#4338ca">COLLET (Letter Generation)</text>
  <text x="700" y="130" text-anchor="middle" font-size="14" fill="#4338ca">COLRPT/COLRP2 (Reporting)</text>
  
  <!-- Delinquency Calculation Layer -->
  <rect x="50" y="200" width="800" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="225" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Delinquency Calculation</text>
  <text x="200" y="260" text-anchor="middle" font-size="14" fill="#1e40af">LPTDEL / LPCDEL (Arrearage)</text>
  <text x="450" y="260" text-anchor="middle" font-size="14" fill="#1e40af">LPPDUE (Past Due Days)</text>
  <text x="700" y="260" text-anchor="middle" font-size="14" fill="#1e40af">LONPF1 (Policy Validation)</text>
  
  <!-- Special Handling Layer -->
  <rect x="50" y="330" width="800" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="355" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Special Handling</text>
  <text x="200" y="390" text-anchor="middle" font-size="14" fill="#15803d">PROGRP (Bankruptcy)</text>
  <text x="450" y="390" text-anchor="middle" font-size="14" fill="#15803d">LONIF1 (Bulk/Repo)</text>
  <text x="700" y="390" text-anchor="middle" font-size="14" fill="#15803d">RZMALL/BWVDEL (Dealer)</text>
  
  <!-- Arrows between layers -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="170" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="300" x2="450" y2="330" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

### Source Citations

- [[FILE:LPTDEL.CPY|LPTDEL.CPY]], [[FILE:LPCDEL.CPY|LPCDEL.CPY]] – Arrearage and delinquency computation logic (Source: [[FILE:LPTDEL.CPY|LPTDEL.CPY]], [[FILE:LPCDEL.CPY|LPCDEL.CPY]], Index: code)
- [[FILE:COLAST.CBL|COLAST.CBL]] – Collections assist screen integration (Source: [[FILE:COLAST.CBL|COLAST.CBL]], Index: code)
- [[FILE:COLLET.CBL|COLLET.CBL]] – Collection letter generation routines (Source: [[FILE:COLLET.CBL|COLLET.CBL]], Index: code)
- [[FILE:COLRPT.CBL|COLRPT.CBL]] – Delinquency reporting and collector activity tracking (Source: [[FILE:COLRPT.CBL|COLRPT.CBL]], Index: code)
- [[FILE:LONPF1.CBL|LONPF1.CBL]] – Allowable delinquency policy enforcement (Source: [[FILE:LONPF1.CBL|LONPF1.CBL]], Index: code)
- [[FILE:PROGRP.CBL|PROGRP.CBL]] – Bankruptcy handling in delinquency calculations (Source: [[FILE:PROGRP.CBL|PROGRP.CBL]], Index: code)
- [[FILE:RZMALL.CBL|RZMALL.CBL]], BWVDEL – Dealer delinquency management (Source: [[FILE:RZMALL.CBL|RZMALL.CBL]], BWVDEL, Index: code/flows)
- LPPDUE – Past due days calculation (Source: [[FILE:LPPDUE|LPPDUE]], [[FILE:I|I]]ndex: flows)
- LPDLCL – Addon contract delinquency management (Source: [[FILE:LPDLCL|LPDLCL]], [[FILE:I|I]]ndex: flows)

---

**Summary:**  
OTIS automates collections and delinquency management through calculation routines, reporting modules, business rule enforcement, and special handling for complex scenarios (bankruptcy, bulk, dealer, addon contracts). This ensures accurate tracking, regulatory compliance, and efficient workflow for collections staff.

### How does OTIS handle loan payoff calculations and final settlements?

OTIS handles loan payoff calculations and final settlements through a coordinated set of COBOL programs, copybooks, and routines that ensure accurate computation of payoff amounts, proper application of rebates, fees, and interest, and compliance with business rules for final loan closure.

---

**BUSINESS PURPOSE:**
- Accurately determine the amount required to pay off a loan, including principal, accrued interest, late charges, and any applicable rebates or fees.
- Support final settlements by updating all related records, generating payoff statements, and ensuring regulatory compliance.
- Provide users (loan officers, customer service reps) with tools to process payoffs, validate calculations, and finalize loan accounts.

---

**TECHNICAL IMPLEMENTATION:**

### 1. **Core Programs Involved**
- **PAYOFF**: Main program for calculating loan payoff amounts.
- **LPPOFF**: Handles detailed payoff routines, including overrides and rebate calculations.
- **GP-PAYOFF-NONCASH**: Manages non-cash payoff scenarios.
- **LOAN**: Provides loan data for calculations.

(Source: [[FILE:PAYOFF|PAYOFF]], [[FILE:LPPOFF|LPPOFF]], [[FILE:GP-PAYOF|GP-PAYOF]]F-NONCASH, LOAN, Index: programs/flows)

---

### 2. **Calculation Workflow**

**Step-by-step process:**
1. **Initiation**: User requests a payoff quote or initiates a final settlement.
2. **Data Gathering**: The system retrieves loan details (principal, interest, fees, payment history).
3. **Payoff Calculation**:
   - **Principal Balance**: Current outstanding principal.
   - **Accrued Interest**: Interest calculated up to the payoff date.
   - **Late Charges & Fees**: Any outstanding charges are added.
   - **Rebates**: Applicable rebates (e.g., unearned insurance, finance charges) are computed and subtracted.
   - **Overrides**: Special business rules or overrides (e.g., regulatory adjustments) are applied.
4. **Final Amount**: The net payoff amount is computed, ensuring all components are included.
5. **Settlement**: Upon payment, the system updates loan records, closes the account, and generates final statements.

**Example Calculation Logic:**
```cobol
COMPUTE NET-POFF-PRIN =
   LN-CURBAL - LN-OT2BAL + LN-OTHBAL - TOT-POFF-REBATES.
```
(Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Lines 1001-1025)

---

### 3. **Key Routines and Paragraphs**
- **PAYOFF-LOAN-ROUTINE**: Central routine for calculating payoff.
- **REB-LN-DF-REBATE**: Calculates rebates and applies them to the payoff.
- **PAYOFF-SET-INT-CHARGES / PAYOFF-SET-LATE-CHARGES**: Add interest and late charges.
- **GB-PATH-OVERRIDE**: Applies business rule overrides.

(Source: [[FILE:LPPOFF|LPPOFF]], [[FILE:I|I]]ndex: flows)

---

### 4. **Copybooks Used**
PAYOFF uses a comprehensive set of copybooks to structure data and enforce business rules. Key examples include:

| Copybook Name           | Description                                                      |
|-------------------------|------------------------------------------------------------------|
| LIBGB/[[FILE:DATER.CPY|DATER.CPY]]         | Date handling routines                                           |
| LIBLP/[[FILE:LPLCAP.CPY|LPLCAP.CPY]]        | Loan capital structure definitions                               |
| LIBLP/[[FILE:LPAPRZ.CPY|LPAPRZ.CPY]]        | APR and rebate calculations                                      |
| LIBGB/[[FILE:REBATE.CPY|REBATE.CPY]]        | Rebate computation logic                                         |
| LIBLP/[[FILE:SETCTBL.CPY|SETCTBL.CPY]]       | Settlement table definitions                                     |
| LIBLP/[[FILE:LPPOF2.CPY|LPPOF2.CPY]]        | Payoff calculation routines                                      |
| LIBLP/[[FILE:LPLCHG.CPY|LPLCHG.CPY]]        | Late charge calculations                                         |
| LIBGB/[[FILE:DECLARE_SQL.CPY|DECLARE_SQL.CPY]]   | SQL declarations for database access                             |
| LIBLP/[[FILE:LPSPGS_SQL.CPY|LPSPGS_SQL.CPY]]    | Payment schedule SQL routines                                    |
| LIBLP/[[FILE:LPLOANW.CPY|LPLOANW.CPY]]       | Loan working storage                                             |
| LIBLP/[[FILE:LPPOFF.CPY|LPPOFF.CPY]]        | Payoff calculation logic                                         |
| LIBLP/[[FILE:LPITRM.CPY|LPITRM.CPY]]        | Interest term calculations                                       |
| LIBLP/[[FILE:LPRATE.CPY|LPRATE.CPY]]        | Rate calculations                                                |
| LIBLP/[[FILE:LPLCAS.CPY|LPLCAS.CPY]]        | Loan case definitions                                            |
| LIBLP/[[FILE:ADDONSPR.CPY|ADDONSPR.CPY]]      | Add-on insurance rebate logic                                    |
| LIBLP/[[FILE:LPAVCR.CPY|LPAVCR.CPY]]        | Available credit calculations                                    |
| LIBLP/[[FILE:LPCEPP.CPY|LPCEPP.CPY]]        | Early payoff penalty logic                                       |
| LIBLP/[[FILE:LPUPWK.CPY|LPUPWK.CPY]]        | Payment update working storage                                   |
| LIBGB/[[FILE:GPENV.CPY|GPENV.CPY]]         | Environment setup for payoff routines                            |
| LIBLP/[[FILE:LPTDEL.CPY|LPTDEL.CPY]]        | Dealer payoff logic                                              |
| LIBLP/[[FILE:LPLTIGS_SQL.CPY|LPLTIGS_SQL.CPY]]   | Loan trigger SQL routines                                        |
| LIBLP/[[FILE:LPLPGS_SQL.CPY|LPLPGS_SQL.CPY]]    | Loan payment SQL routines                                        |
| LIBLP/[[FILE:LPLNGS_SQL.CPY|LPLNGS_SQL.CPY]]    | Loan general SQL routines                                        |
| LIBLP/[[FILE:LPBWGS_SQL.CPY|LPBWGS_SQL.CPY]]    | Loan batch write SQL routines                                    |
| LIBGB/[[FILE:GBBRGS_SQL.CPY|GBBRGS_SQL.CPY]]    | General business rules SQL routines                              |
| LIBGB/[[FILE:GBGBGS_SQL.CPY|GBGBGS_SQL.CPY]]    | General business SQL routines                                    |
| LIBGB/[[FILE:GBBRRN.CPY|GBBRRN.CPY]]        | Business rule reference numbers                                  |
| LIBGB/[[FILE:GBGBRN.CPY|GBGBRN.CPY]]        | General business reference numbers                               |
| LIBLP/[[FILE:LPLTP1RN.CPY|LPLTP1RN.CPY]]      | Loan type reference numbers                                      |
| LIBLP/[[FILE:LPBW1RN.CPY|LPBW1RN.CPY]]       | Loan batch write reference numbers                               |
| LIBLP/[[FILE:LPPOFFW.CPY|LPPOFFW.CPY]]       | Payoff working storage                                           |
| LIBLP/[[FILE:LPUPERW.CPY|LPUPERW.CPY]]       | Payment update error working storage                             |
| LIBLP/[[FILE:LPIBPC.CPY|LPIBPC.CPY]]        | Insurance and benefit premium calculations                       |
| LIBLP/[[FILE:LPIBPCW.CPY|LPIBPCW.CPY]]       | Insurance and benefit premium working storage                    |
| LIBLP/[[FILE:LPITRMW.CPY|LPITRMW.CPY]]       | Interest term working storage                                    |
| LIBLP/[[FILE:LPAMTS.CPY|LPAMTS.CPY]]        | Loan amounts calculations                                        |
| LIBLP/[[FILE:LPAMTSW.CPY|LPAMTSW.CPY]]       | Loan amounts working storage                                     |
| LIBLP/[[FILE:LPAPRZW.CPY|LPAPRZW.CPY]]       | APR and rebate working storage                                   |
| LIBLP/[[FILE:LPLCASW.CPY|LPLCASW.CPY]]       | Loan case working storage                                        |
| LIBLP/[[FILE:SETCTBLW.CPY|SETCTBLW.CPY]]      | Settlement table working storage                                 |
| LIBGB/[[FILE:REBATEW.CPY|REBATEW.CPY]]       | Rebate working storage                                           |
| LIBGB/[[FILE:SPPDTHW.CPY|SPPDTHW.CPY]]       | Special payoff death handling working storage                    |
| LIBGB/[[FILE:GETFMW.CPY|GETFMW.CPY]]        | Get financial management working storage                         |
| LIBGB/[[FILE:FERRORS.CPY|FERRORS.CPY]]       | Financial error codes                                            |
| LIBLP/[[FILE:ADDONSPRW.CPY|ADDONSPRW.CPY]]     | Add-on insurance rebate working storage                          |
| LIBGB/[[FILE:GB01BR.CPY|GB01BR.CPY]]        | General business reference (working storage)                     |
| LIBGB/[[FILE:GB01BR_SQL.CPY|GB01BR_SQL.CPY]]    | General business reference SQL (working storage)                 |
| LIBLP/[[FILE:LPLCAPW.CPY|LPLCAPW.CPY]]       | Loan capital working storage                                     |
| LIBLP/[[FILE:LPLOANW.CPY|LPLOANW.CPY]]       | Loan working storage                                             |
| LIBLP/[[FILE:LPPOFFW.CPY|LPPOFFW.CPY]]       | Payoff working storage                                           |
| LIBLP/[[FILE:LPRATEW.CPY|LPRATEW.CPY]]       | Rate working storage                                             |
| LIBLP/[[FILE:LPITRMW.CPY|LPITRMW.CPY]]       | Interest term working storage                                    |
| LIBLP/[[FILE:LPUPWKW.CPY|LPUPWKW.CPY]]       | Payment update working storage                                   |

(Source: [[FILE:PAYOFF.CBL|PAYOFF.CBL]], Index: copybook_usage)

---

### 5. **Business Rule Enforcement**
- **Rebate Calculations**: Ensures unearned finance charges and insurance are properly rebated.
- **Interest and Fee Handling**: Accrues interest up to payoff date, adds late charges, and applies any penalties.
- **Overrides and Adjustments**: Handles special cases (e.g., regulatory overrides, double-dip reversal as noted in comments).
- **Final Settlement**: Updates loan status, closes account, and generates payoff statements.

**Example business rule comment:**
> "THE PAYOFF COLLECTED INTEREST FROM LAST PAY... THIS 'DOUBLE DIP' IS REVERSED OUT OF GL ON THE NEXT MONTH VIA THE STAT AMOUNT BUT THE ZERO BALANCE ACCOUNT DOES NOT GO THRU THE ACCRUAL AND THE AEFILE WOULD REFLECT THE 'DOUBLE DIP'."  
(Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 626-650)

---

### 6. **Error Handling and Compliance**
- Validates all calculations for accuracy.
- Logs errors and exceptions using dedicated error copybooks (e.g., [[FILE:FERRORS.CPY|FERRORS.CPY]]).
- Ensures compliance with federal and state regulations for loan settlements.

---

### 7. **Call Chain and Program Relationships**

Here's a visual representation:

<svg width="1200" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="600" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">Loan Payoff Calculation & Settlement Call Chain</text>
  
  <!-- Main program at top center -->
  <rect x="520" y="60" width="160" height="80" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="600" y="95" text-anchor="middle" fill="white" font-size="18" font-weight="bold">PAYOFF</text>
  <text x="600" y="118" text-anchor="middle" fill="white" font-size="13">Main Program</text>
  
  <!-- Row 1: Programs 1-4 -->
  <rect x="50" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="120" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LPPOFF</text>
  <text x="120" y="228" text-anchor="middle" fill="white" font-size="11">Payoff Routine</text>
  
  <rect x="230" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="300" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">GP-PAYOFF-NONCASH</text>
  <text x="300" y="228" text-anchor="middle" fill="white" font-size="11">Noncash Payoff</text>
  
  <rect x="410" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="480" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LOAN</text>
  <text x="480" y="228" text-anchor="middle" fill="white" font-size="11">Loan Data</text>
  
  <rect x="590" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="660" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">REBATE</text>
  <text x="660" y="228" text-anchor="middle" fill="white" font-size="11">Rebate Calculation</text>
  
  <!-- Row 2: Programs 5-8 -->
  <rect x="770" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="840" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LPSCHD</text>
  <text x="840" y="228" text-anchor="middle" fill="white" font-size="11">Schedule Calc</text>
  
  <rect x="950" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="1020" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">FAPOFF</text>
  <text x="1020" y="228" text-anchor="middle" fill="white" font-size="11">Final Payoff</text>
  
  <!-- Arrows from main to all called programs -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="600" y1="140" x2="120" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="300" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="480" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="660" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="840" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="1020" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**SUMMARY:**
- OTIS uses the PAYOFF program and related routines to calculate loan payoff amounts, apply all relevant business rules, and finalize settlements.
- The process includes principal, interest, fees, rebates, and overrides, with robust error handling and compliance checks.
- Data structures and logic are defined in a comprehensive set of copybooks, ensuring consistency and accuracy across all payoff scenarios.

**Sources:**
- PAYOFF, LPPOFF, GP-PAYOFF-NONCASH, LOAN, FAPOFF, LPSCHD (Index: programs, flows)
- [[FILE:PAYOFF.CBL|PAYOFF.CBL]] (Index: copybook_usage, code)
- [[FILE:APIPAY.CBL|APIPAY.CBL]] (Lines 1001-1025)
- [[FILE:LONPF2.CBL|LONPF2.CBL]] (Lines 626-650)
- [[FILE:REBATE.CPY|REBATE.CPY]] (Lines 3476-3500)

### What credit bureau reporting and integration capabilities does OTIS have?

OTIS provides robust credit bureau reporting and integration capabilities to support financial institutions in managing borrower credit data, regulatory compliance, and automated data exchange with major bureaus. These capabilities are implemented through dedicated COBOL programs, specialized routines, and structured data flows.

**BUSINESS PURPOSE:**
- Automates the reporting of borrower credit information to bureaus (Equifax, Experian, TransUnion)
- Supports regulatory compliance for credit reporting (e.g., Metro2 format)
- Enables periodic uploads/updates of credit scores and borrower data
- Integrates bureau responses and updates into borrower and loan records
- Facilitates credit score updates for new accounts and ongoing servicing

**TECHNICAL IMPLEMENTATION:**

1. **Credit Bureau Reporting Programs:**
   - **CHTRWF**: Handles credit bureau reporting, specifically triggered from the collections menu (CLMENU). According to program comments, it is dedicated to credit bureau reporting and only called from CLMENU.
     - *Source: [[FILE:CHTRWF.CBL|CHTRWF.CBL]], Index: code*
   - **WRLDCR**: Uploads credit scores into borrower records and updates loan credit scores for new accounts. It processes input files from external sources (e.g., /USR/DATA/R#/SP/OCORE.Y) and updates both borrower and loan files.
     - *Source: [[FILE:WRLDCR.CBL|WRLDCR.CBL]], Index: code*
   - **METRO2**: Implements Metro2 credit bureau reporting format, including logic for formatting account numbers, handling address segments, and bankruptcy reporting. It uses specialized copybooks for Metro2 data structures and includes historical notes about compliance with bureau requirements.
     - *Source: [[FILE:METRO2.CBL|METRO2.CBL]], Index: code, copybook_usage*

2. **Integration with Bureau Data:**
   - Programs read and write files containing credit scores, credit limits, and bureau-specific fields (e.g., BW_CREDIT_SCORE, BW_CRED_SCORE_1-4, BW_BNKRPT_SCORE_1-4).
     - *Source: [[FILE:IOBWRN.CBL|IOBWRN.CBL]], Index: code*
   - Data structures are designed to accommodate multiple bureau scores and identifiers, supporting integration with all major bureaus.

3. **Copybooks Supporting Bureau Integration:**
   - **LIBSP/[[FILE:SP01TRWINFO.CPY|SP01TRWINFO.CPY]]**: Defines TransUnion/Metro2 reporting fields.
   - **LIBGB/[[FILE:REBATE.CPY|REBATE.CPY]]** and **LIBGB/[[FILE:REBATEW.CPY|REBATEW.CPY]]**: Support rebate and credit score data structures.
   - **[[FILE:TWINFO.CBL|TWINFO.CBL]]**: Specifies bureau program identifiers and reporter information for Equifax, Experian, and TransUnion.
     - *Source: [[FILE:METRO2.CBL|METRO2.CBL]], Index: copybook_usage; [[FILE:TWINFO.CBL|TWINFO.CBL]], Index: code*

4. **Menu Integration:**
   - Credit bureau reporting is accessible via the collections menu (CLMENU), allowing users to trigger reporting routines directly from the UI.
     - *Source: [[FILE:CHTRWF.CBL|CHTRWF.CBL]], Index: code*

5. **Automated Data Exchange:**
   - Programs like WRLDCR and METRO2 automate the upload and formatting of credit data, ensuring compatibility with bureau requirements (e.g., right-justified, zero-filled account numbers).
   - Historical comments document changes made to comply with bureau specifications and regulatory updates.
     - *Source: [[FILE:METRO2.CBL|METRO2.CBL]], Index: code*

**KEY PROGRAMS AND FILES INVOLVED:**

| Program   | Purpose/Role                                              |
|-----------|----------------------------------------------------------|
| CHTRWF    | Credit bureau reporting (triggered from CLMENU)          |
| WRLDCR    | Uploads credit scores, updates borrower/loan records     |
| METRO2    | Metro2 format reporting, compliance logic                |
| CLMENU    | Collections menu, entry point for bureau reporting       |

| Copybook/File                | Description                                      |
|------------------------------|--------------------------------------------------|
| LIBSP/[[FILE:SP01TRWINFO.CPY|SP01TRWINFO.CPY]]        | TransUnion/Metro2 reporting fields               |
| LIBGB/[[FILE:REBATE.CPY|REBATE.CPY]], [[FILE:REBATEW.CPY|REBATEW.CPY]]| Rebate/credit score data structures              |
| [[FILE:TWINFO.CBL|TWINFO.CBL]]                   | Bureau identifiers and reporter info             |
| BWFILE (BW_CREDIT_SCORE, etc)| Borrower credit score fields                     |

**DATA FLOW OVERVIEW:**
- Borrower and loan data are extracted and formatted according to bureau requirements.
- Credit scores and related fields are updated in borrower/loan files.
- Data is uploaded to bureaus via automated routines.
- Bureau responses (scores, updates) are integrated back into OTIS records.

---

Here's a visual representation of the credit bureau reporting and integration architecture in OTIS:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Credit Bureau Reporting & Integration</text>
  
  <!-- Presentation Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="450" y="125" text-anchor="middle" font-size="14" fill="#4338ca">Collections Menu (CLMENU), User Triggers</text>
  
  <!-- Business Logic Layer -->
  <rect x="50" y="180" width="800" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="210" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="200" y="240" text-anchor="middle" font-size="14" fill="#1e40af">CHTRWF (Reporting)</text>
  <text x="450" y="240" text-anchor="middle" font-size="14" fill="#1e40af">WRLDCR (Score Upload)</text>
  <text x="700" y="240" text-anchor="middle" font-size="14" fill="#1e40af">METRO2 (Metro2 Format)</text>
  
  <!-- Data Layer -->
  <rect x="50" y="320" width="800" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="350" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="200" y="380" text-anchor="middle" font-size="14" fill="#15803d">Borrower File (BWFILE)</text>
  <text x="450" y="380" text-anchor="middle" font-size="14" fill="#15803d">Loan File</text>
  <text x="700" y="380" text-anchor="middle" font-size="14" fill="#15803d">Credit Bureau Files</text>
  
  <!-- External Bureaus -->
  <rect x="350" y="440" width="200" height="40" fill="#f3f4f6" stroke="#6366f1" stroke-width="2" rx="8"/>
  <text x="450" y="465" text-anchor="middle" font-size="16" font-weight="bold" fill="#6366f1">Equifax / Experian / TransUnion</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="450" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="200" y1="280" x2="200" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="280" x2="450" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="700" y1="280" x2="700" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="200" y1="420" x2="450" y2="440" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="420" x2="450" y2="440" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="700" y1="420" x2="450" y2="440" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Summary:**  
OTIS supports automated, compliant credit bureau reporting and integration with all major bureaus, using dedicated COBOL programs (CHTRWF, WRLDCR, METRO2), specialized copybooks, and structured data flows. These capabilities ensure accurate, timely reporting and seamless integration of bureau data into loan servicing operations.

**Sources:**
- [[FILE:CHTRWF.CBL|CHTRWF.CBL]], Index: code
- [[FILE:WRLDCR.CBL|WRLDCR.CBL]], Index: code
- [[FILE:METRO2.CBL|METRO2.CBL]], Index: code, copybook_usage
- [[FILE:IOBWRN.CBL|IOBWRN.CBL]], Index: code
- [[FILE:TWINFO.CBL|TWINFO.CBL]], Index: code

### How does OTIS manage insurance tracking and forced-placed insurance?

OTIS manages insurance tracking and forced-placed insurance through a combination of business rules, COBOL programs, and copybooks that ensure loans are properly monitored for insurance compliance, and that forced-placed insurance is applied when required. Here’s how the process works:

---

**BUSINESS PURPOSE**

- **Insurance Tracking:**  
  OTIS tracks insurance coverage for each loan, monitoring whether required insurance types (e.g., collateral, property, credit life, accident/health, etc.) are present and valid. This ensures regulatory compliance and protects the lender’s collateral.
- **Forced-Placed Insurance:**  
  If a borrower fails to maintain required insurance, OTIS can automatically apply forced-placed insurance to the loan. This protects the lender by ensuring coverage is in place, even if the borrower is non-compliant.

---

**TECHNICAL IMPLEMENTATION**

### 1. **Insurance Tracking Logic**

- **Loan Record Fields:**  
  Insurance status is tracked in loan record fields such as `LN-INSTYPES-PP`, `LN-INSPREM`, and others. These indicate which insurance types are present and their premiums.
- **Insurance Tracking Routines:**  
  Programs like `DISPLAY-INSURANCE-TRACKING` (found in [[FILE:LONPF0.CBL|LONPF0.CBL]]) read insurance data from loan records and associated insurance tracking files (e.g., LTA1-FILE), displaying current insurance status and details.

  Example (Source: [[FILE:LONPF0.CBL|LONPF0.CBL]], Index: code):
  ```cobol
  DISPLAY-INSURANCE-TRACKING.
      MOVE LN-OWNBR  TO LTA-BRNO.
      MOVE LN-ACCTNO TO LTA-ACCTNO.
      MOVE 1         TO LTA-SEQNO.
      PERFORM OPEN-LTA1-FILE.
      PERFORM READ-LTA1-FILE.
      PERFORM CLOSE-LTA1-FILE.
      IF IO-FG = 0
         MOVE LTA-TRACK-ONLY TO D-TRACK
         MOVE LTA-INSURANCE  TO D-INS
      ELSE
         MOVE SPACES TO D-TRACK D-INS.
  ```

- **Validation and Error Handling:**  
  Programs like `SFVERI` validate insurance premiums and types, ensuring that property insurance is only allowed when appropriate and that premiums match the insurance type flags.

  Example (Source: [[FILE:SFVERI.CBL|SFVERI.CBL]], Index: code):
  ```cobol
  IF LN-INSPREM(3) = 0
     MOVE "PROP PREMIUM DOES NOT MATCH INS TYPE FLAG" TO MESS
     PERFORM ERROR-MESSAGE
  ELSE
     IF ( LN-INSTYPES-PP )
        IF ( SP-INSCOM(6) = ZEROES )
           MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
           PERFORM ERROR-MESSAGE.
  ```

### 2. **Forced-Placed Insurance Logic**

- **Business Rules for Forced Placement:**  
  Comments in [[FILE:XONPC0.CBL|XONPC0.CBL]] and [[FILE:ZONPC0.CBL|ZONPC0.CBL]] describe the rules for when forced-placed insurance is applied and how fees are locked/unlocked based on insurance status.

  Example (Source: [[FILE:XONPC0.CBL|XONPC0.CBL]], Index: code):
  ```
  *    BR-FORCE | INSURANCE |
  *      'Y'    |    'Y'    | FEES 16 -18 LOCKED
  *      'Y'    |    'N'    | ALL FEES UNLOCKED, NO INSURANCE ENTRY ALLOWED
  *      'N'    |    'N'    | ALL FEES UNLOCKED, NO INSURANCE ENTRY
  ```

- **Programmatic Enforcement:**  
  If insurance is missing or invalid, the system sets flags (e.g., `BR-FORCE = 'Y'`) and restricts user actions (locking certain fee fields, preventing insurance entry, etc.). This ensures forced-placed insurance is applied according to business rules.

- **Password Protection and Audit:**  
  Certain insurance fields (CL, AH, PP, O1) are password-protected to prevent unauthorized changes, especially when forced-placed insurance is involved (Source: [[FILE:LNQUOT.CBL|LNQUOT.CBL]], [[FILE:XONPC0.CBL|XONPC0.CBL]]).

### 3. **Insurance Tracking Files and Copybooks**

- **Tracking Files:**  
  Files like `LTA1-FILE` store insurance tracking records linked to loans.
- **Copybooks:**  
  - `[[FILE:LP01LT.CPY|LP01LT.CPY]]` – Defines insurance info fields for loan records.
  - `[[FILE:LPIEXP.CPY|LPIEXP.CPY]]` – Handles insurance effective/expiration dates and commission calculations.
  - `[[FILE:LP01SP.CPY|LP01SP.CPY]]` – Contains insurance calculation formulas and business rules.

---

**KEY PROGRAMS INVOLVED**

| Program   | Purpose                                                        |
|-----------|----------------------------------------------------------------|
| LONPF0    | Displays and manages insurance tracking for loans              |
| SFVERI    | Validates insurance premiums and types                         |
| XONPC0    | Enforces forced-placed insurance rules and fee locking         |
| LNQUOT    | Handles insurance field protection and password logic          |
| [[FILE:LP01LT.CPY|LP01LT.CPY]]| Defines insurance info fields for loan records                 |
| [[FILE:LPIEXP.CPY|LPIEXP.CPY]]| Manages insurance dates and commission calculations            |

---

**BUSINESS RULES (from code comments):**

- If forced-placed insurance is required (`BR-FORCE = 'Y'`), certain fees are locked and insurance entry is restricted.
- No insurance means certain fields are set to 'N' and entry is blocked.
- Password protection is enforced for key insurance fields to prevent unauthorized changes.

---

**SUMMARY**

OTIS manages insurance tracking by:
- Monitoring insurance status for every loan
- Validating insurance coverage and premiums
- Automatically applying forced-placed insurance when required
- Enforcing business rules through field locking and password protection
- Using dedicated COBOL programs and copybooks for insurance logic and tracking

---

**Here's a visual representation:**

<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Insurance Tracking & Forced-Placed Insurance</text>
  
  <!-- Loan Record -->
  <rect x="60" y="80" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Loan Record</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#6366f1">LN-INSTYPES, LN-INSPREM</text>
  
  <!-- Insurance Tracking File -->
  <rect x="60" y="180" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="150" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Insurance Tracking File</text>
  <text x="150" y="230" text-anchor="middle" font-size="12" fill="#22c55e">LTA1-FILE</text>
  
  <!-- Insurance Tracking Program -->
  <rect x="300" y="80" width="220" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="410" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Insurance Tracking Program</text>
  <text x="410" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">LONPF0, DISPLAY-INSURANCE-TRACKING</text>
  
  <!-- Validation Program -->
  <rect x="300" y="180" width="220" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="410" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Validation Program</text>
  <text x="410" y="230" text-anchor="middle" font-size="12" fill="#3b82f6">SFVERI</text>
  
  <!-- Forced-Placed Insurance Logic -->
  <rect x="600" y="80" width="220" height="70" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="710" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">Forced-Placed Insurance Logic</text>
  <text x="710" y="130" text-anchor="middle" font-size="12" fill="#f59e0b">XONPC0, ZONPC0, LNQUOT</text>
  
  <!-- Copybooks -->
  <rect x="600" y="180" width="220" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="710" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Copybooks</text>
  <text x="710" y="230" text-anchor="middle" font-size="12" fill="#6366f1">[[FILE:LP01LT.CPY|LP01LT.CPY]], [[FILE:LPIEXP.CPY|LPIEXP.CPY]], [[FILE:LP01SP.CPY|LP01SP.CPY]]</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="240" y1="115" x2="300" y2="115" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="240" y1="215" x2="300" y2="215" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="520" y1="115" x2="600" y2="115" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="520" y1="215" x2="600" y2="215" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="150" y1="150" x2="150" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Source Citations:**
- Insurance tracking logic: [[FILE:LONPF0.CBL|LONPF0.CBL]], Index: code
- Validation logic: [[FILE:SFVERI.CBL|SFVERI.CBL]], Index: code
- Forced-placed insurance rules: [[FILE:XONPC0.CBL|XONPC0.CBL]], [[FILE:ZONPC0.CBL|ZONPC0.CBL]], [[FILE:LNQUOT.CBL|LNQUOT.CBL]], Index: code
- Insurance info fields: [[FILE:LP01LT.CPY|LP01LT.CPY]], Index: code
- Insurance dates/commission: [[FILE:LPIEXP.CPY|LPIEXP.CPY]], Index: code
- Insurance formulas/rules: [[FILE:LP01SP.CPY|LP01SP.CPY]], Index: code

If you need details on a specific insurance type or want to see the exact field definitions, let me know!

### What tax-related functions does OTIS perform (1098 reporting, property taxes)?

OTIS performs a wide range of tax-related functions to support regulatory compliance and financial operations for lending institutions. These functions include IRS 1098 reporting, property tax management, insurance tax handling, and related calculations. Here’s a breakdown of the main tax-related capabilities:

---

### 1. **IRS 1098 Reporting (Mortgage Interest Paid)**
**Business Purpose:**
- Extracts and reports mortgage interest paid by borrowers to the IRS, as required by federal law (Form 1098).
- Ensures accurate reporting for each loan account that meets IRS criteria.

**Technical Implementation:**
- **Program IR1098**: Handles extraction of interest paid data for IRS Form 1098.
  - Reads loan accounts, calculates interest paid, and prepares records for IRS submission.
  - Handles alternate property addresses and flags for property securing the loan.
  - Computes principal balance as of January 1 of the tax year (per 2016 law update).
  - Example logic:
    ```cobol
    IF BX-ALT-ADR1 = SPACES AND BX-ALT-ADR2 = SPACES AND BX-ALT-CITY = SPACES
       MOVE "1" TO IL-B-PROPERTY-SECURING
    ELSE
       STRING BX-ALT-ADR1," ",BX-ALT-ADR2," ",BX-ALT-CITY,", ",
         BX-ALT-STATE," ",BX-ALT-ZIP DELIMITED BY "  " INTO IL-B-PROPERTY-ADDRESS
    ```
    (Source: [[FILE:IR1098.CBL|IR1098.CBL]], Lines 1251-1275)

- **Program IRPREP**: Prepares IRS files, including 1098 extraction and file merging for submission.
  - Combines outputs from various extraction routines into a single IRS submission file.
  - Handles multiple IRS forms (1098, 1099 variants).
  - Example comment:
    ```
    * 2- IRS LOAN (1098) EXTRACTION - THIS PROGRAM WILL ALLOW
    *    YOU TO EXTRACT LOAN ACCOUNTS THAT HAVE PAID INTEREST
    *    WHICH NEEDS TO BE REPORTED TO THE IRS ON FORM 1098.
    ```
    (Source: [[FILE:IRPREP.CBL|IRPREP.CBL]], Lines 1-25)

---

### 2. **Property Tax Management**
**Business Purpose:**
- Tracks, calculates, and reports property taxes associated with loans.
- Supports escrow disbursement and refund calculations for property taxes.

**Technical Implementation:**
- **Program INSTAX**: Manages county and city property tax records and calculations.
  - Retrieves county tax records, determines if taxes are due, and processes charges or clears records if no taxes apply.
  - Example logic:
    ```cobol
    IF IO-BAD
       CLEAR CD-CT-RECX TO SPACES      (INDICATES NO TAXES)
    ELSE
       MOVE CD-CT-REC   TO CD-CT-RECX  (INDICATES CHARGE COUNTY TAX)
    ```
    (Source: [[FILE:INSTAX.CBL|INSTAX.CBL]], Lines 101-125)

- **Program TXMOR2**: Generates monthly insurance and tax reports, including premium and refund calculations.
  - Computes property tax amounts, surcharges, and rebates.
  - Handles new premiums for new loans and refunds for cancelled loans.
  - Example logic:
    ```cobol
    COMPUTE WORK-TAX-AMT(3) ROUNDED = WORK-TAX-AMT(3) - WORK-TAX-AMT(6)
    MOVE WORK-TAX-AMT(3) TO D-TAX-AMT(3).
    IF WORK-TAX-REBATE(3) NOT = 0
       PERFORM COMPUTE-SURCHARGE
    MOVE SURCHARGE-REBATE TO D-REFUND-AMT(6)
    ```
    (Source: [[FILE:TXMOR2.CBL|TXMOR2.CBL]], Lines 926-950)

- **Escrow Disbursement Tracking**:
  - Programs like LONPF2 and copybooks such as [[FILE:LPES1IN.CPY|LPES1IN.CPY]] track escrow disbursements for taxes and insurance.
  - Example:
    ```
    IF RV-LP-REFNO = "TAX"
       ADD LP-TRAMT TO ES-DISBURSED-TAXES(1)
                     ES-DISBURSED-TAXES(3)
    ```
    (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9726-9750)

---

### 3. **Other Tax-Related Reporting (1099 Variants)**
**Business Purpose:**
- Supports extraction and reporting for IRS 1099 forms (MISC, C, etc.) for cancelled debt, miscellaneous income, and other tax events.

**Technical Implementation:**
- **Program IRPREP**: Handles extraction and file preparation for 1099 reporting.
  - Manages record types for different IRS forms and reporting scenarios.
  - Example comment:
    ```
    *     1 TYPE T
    *     1 TYPE A
    *       TYPE B'S FOR EACH LOAN REPORTING INTEREST
    *     1 TYPE C
    ```
    (Source: [[FILE:IRPREP.CBL|IRPREP.CBL]], Lines 51-75)

---

### 4. **Insurance Tax Handling**
**Business Purpose:**
- Calculates insurance-related taxes and surcharges for loan accounts.
- Supports premium and refund reporting for insurance products.

**Technical Implementation:**
- **TXMOR2**: Monthly insurance tax report, including premium and refund calculations for life, AH&H, property, and other insurance types.
  - Computes surcharges and rebates as part of tax refund logic.
  - Example:
    ```
    * 1. "TP" FEE AMOUNT DIVIDED BY PP INSURANCE AMOUNT EQUALS %
    * 2. DIVIDE 1.5% BY THE % COMPUTED IN STEP 1 WHICH WILL GIVE YOU WHAT
    *    PERCENT OF THE ORIGINAL TOTAL TAX WAS TAX AND WHAT WAS SURCHARGE
    * 3. MULTIPLY THE % FROM STEP 2 TIMES THE INSURANCE TAX REBATE REFUND
    ```
    (Source: [[FILE:TXMOR2.CBL|TXMOR2.CBL]], Lines 1301-1325)

---

## Summary Table: Key Tax-Related Programs and Copybooks

| Program/Copybook      | Purpose/Functionality                                      |
|-----------------------|-----------------------------------------------------------|
| [[FILE:IR1098.CBL|IR1098.CBL]]            | Extracts mortgage interest paid for IRS 1098 reporting    |
| [[FILE:IRPREP.CBL|IRPREP.CBL]]            | Prepares IRS submission files (1098, 1099, etc.)          |
| [[FILE:INSTAX.CBL|INSTAX.CBL]]            | Manages property tax records and calculations             |
| [[FILE:TXMOR2.CBL|TXMOR2.CBL]]            | Monthly insurance and tax reporting, premium/refund logic |
| [[FILE:LONPF2.CBL|LONPF2.CBL]]            | Tracks escrow disbursements for taxes/insurance           |
| [[FILE:LPES1IN.CPY|LPES1IN.CPY]]           | Defines escrow disbursement fields (taxes, insurance)     |

---

## Visual Representation

Here's a visual representation of the tax-related program relationships in OTIS:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Tax-Related Functions</text>
  
  <!-- IRS Reporting Layer -->
  <rect x="50" y="70" width="350" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="225" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">IRS Reporting</text>
  <text x="225" y="120" text-anchor="middle" font-size="13" fill="#6366f1">IR1098, IRPREP</text>
  
  <!-- Property Tax Layer -->
  <rect x="500" y="70" width="350" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="675" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Property Tax Management</text>
  <text x="675" y="120" text-anchor="middle" font-size="13" fill="#3b82f6">INSTAX, TXMOR2</text>
  
  <!-- Escrow/Insurance Layer -->
  <rect x="275" y="200" width="350" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Escrow & Insurance Tax</text>
  <text x="450" y="250" text-anchor="middle" font-size="13" fill="#22c55e">LONPF2, LPES1IN, TXMOR2</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="225" y1="150" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="675" y1="150" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

## Source Citations

- IRS 1098 extraction and reporting: [[FILE:IR1098.CBL|IR1098.CBL]], Lines 1-25, 1251-1275, 1151-1175
- IRS file preparation and merging: [[FILE:IRPREP.CBL|IRPREP.CBL]], Lines 1-25, 51-75, 226-250, 801-825
- Property tax record management: [[FILE:INSTAX.CBL|INSTAX.CBL]], Lines 101-125
- Monthly insurance/tax reporting: [[FILE:TXMOR2.CBL|TXMOR2.CBL]], Lines 1-25, 901-925, 926-950, 1301-1325
- Escrow disbursement logic: [[FILE:LONPF2.CBL|LONPF2.CBL]], Lines 9726-9750; [[FILE:LPES1IN.CPY|LPES1IN.CPY]], Lines 1251-1275

---

**In summary:**  
OTIS automates IRS 1098 reporting, manages property tax calculations and records, handles insurance tax and surcharges, and supports escrow disbursement tracking for tax and insurance payments. These functions are implemented through specialized COBOL programs and copybooks, ensuring compliance and operational efficiency for lending institutions.

### How does OTIS handle charge-offs, write-offs, and loss mitigation?

OTIS handles charge-offs, write-offs, and loss mitigation through a coordinated set of COBOL programs, business rules, and data structures designed to:

- Identify loans that meet charge-off or write-off criteria
- Calculate and record charge-off amounts
- Track loss mitigation activities and outcomes
- Report on charge-offs for regulatory and management purposes

Below is a detailed explanation of how these processes are implemented in OTIS, with supporting code and business logic references.

---

## BUSINESS PURPOSE

**Charge-offs and Write-offs:**
- **Charge-off:** When a loan is deemed uncollectible, the outstanding balance is moved from an asset to an expense account, reflecting a loss.
- **Write-off:** Similar to charge-off, but may refer to the accounting action of removing the asset from the books.
- **Loss Mitigation:** Activities to reduce losses, such as restructuring, settlements, or recoveries after charge-off.

**OTIS Objectives:**
- Automate identification and processing of loans for charge-off/write-off
- Ensure compliance with accounting and regulatory standards
- Provide accurate reporting for management and auditors
- Support loss mitigation tracking and reporting

---

## TECHNICAL IMPLEMENTATION

### 1. **Identification and Processing of Charge-offs**

- **Business Rules:**  
  OTIS uses date-based and status-based rules to determine when a loan should be charged off. For example:
  - Loans with no payment activity for a set period (e.g., 90+ days delinquent)
  - Loans meeting specific criteria (e.g., bankruptcy, repossession, or foreclosure)

- **Code Example:**  
  The following logic from `[[FILE:RZLNEXOLD.CBL|RZLNEXOLD.CBL]]` and `[[FILE:LNEXTR.CBL|LNEXTR.CBL]]` shows how loans are evaluated for charge-off eligibility:
  ```cobol
  * TEST P&L FROM DATE, IF LN-PLDATE IS PRIOR TO THIS DATE, SKIP
  * BUT ONLY IF DATE OF LAST PAYMENT IS OLDER THAN +3 YEARS FROM
  * PL-FROM-DATE
  IF LN-PLDATE NOT = 0
     IF LN-PLDATE < PL-FROM-DATE
        IF LN-DATE-PAID-LAST = 0
           ...
  ```
  - This logic ensures only loans with sufficiently old last payment dates are considered for charge-off.
  - (Source: [[FILE:RZLNEXOLD.CBL|RZLNEXOLD.CBL]], Lines 501-525; [[FILE:LNEXTR.CBL|LNEXTR.CBL]], Lines 526-550)

### 2. **Calculation and Recording of Charge-off Amounts**

- **Data Structures:**  
  Charge-off amounts are tracked monthly and year-to-date, as seen in `[[FILE:PROGRP.CBL|PROGRP.CBL]]`:
  ```cobol
  07  REPO-GAIN-AMT-MTD     PIC S9(11)V99 COMP.
  07  REPO-GAIN-AMT-YTD     PIC S9(11)V99 COMP.
  07  REPO-VOL-NO-MTD       PIC S9(9) COMP.
  07  REPO-VOL-NO-YTD       PIC S9(9) COMP.
  07  REPO-VOL-NO-LYR       PIC S9(9) COMP.
  07  REPO-VOL-AMT-MTD      PIC S9(11)V99 COMP.
  07  REPO-VOL-AMT-YTD      PIC S9(11)V99 COMP.
  07  REPO-VOL-AMT-LYR      PIC S9(11)V9
  ```
  - These fields support detailed tracking and reporting of charge-off activity.
  - (Source: [[FILE:PROGRP.CBL|PROGRP.CBL]], Lines 401-425)

- **Processing Logic:**  
  Programs like `[[FILE:BYRSUP.CBL|BYRSUP.CBL]]` aggregate charge-off counts and amounts for reporting:
  ```cobol
  * TOTAL NO OF PL CHARGE-OFFS MADE MTD & YTD
  IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
     ADD HOLD-MTD-PL TO TOT-WAGN-34
  ELSE
     MOVE HOLD-MTD-PL TO WR-NO-CHARGEOFFS(1)
     MOVE HOLD-YTD-PL TO WR-NO-CHARGEOFFS(2)
     ADD HOLD-MTD-PL TO TOT-WARS-34.
  ```
  - (Source: [[FILE:BYRSUP.CBL|BYRSUP.CBL]], Lines 1251-1275)

### 3. **Loss Mitigation Tracking**

- **Loss Mitigation Activities:**  
  OTIS tracks loss mitigation through specialized fields and reporting routines, including:
  - Bankruptcy buckets and recovery tracking
  - Segregation of contractual vs. recency-based charge-offs
  - Special handling for repossessions and settlements

- **Business Rule Example:**  
  From `[[FILE:PROGRP.CBL|PROGRP.CBL]]`, the system distinguishes between contractual and recency-based charge-offs:
  ```cobol
  * BUCKET 4 POT CHG-OFFS USE RECENCY NOT CONTRACTUAL FOR LNS
  * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
  IF GP-PROGRP-DL-CONT = "Y"
     ADD BY-UNITS-BNKRPT(AGE-SUB,2,3)
         BY-UNITS-BNKRPT(AGE-SUB,2,4)
         ...
     TO LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,3).
  ```
  - (Source: [[FILE:PROGRP.CBL|PROGRP.CBL]], Lines 2676-2700, 2626-2650)

### 4. **Reporting and Audit**

- **Reporting Programs:**  
  OTIS includes programs and copybooks for generating charge-off and loss reports, such as:
  - `[[FILE:SPWRGS_SQL.CPY|SPWRGS_SQL.CPY]]` for moving charge-off data into reporting structures
  - `[[FILE:OPREP2.CBL|OPREP2.CBL]]` for volume and delinquency reporting
  - `[[FILE:UP1534.CBL|UP1534.CBL]]` for charge-off expense tracking

- **Sample Reporting Logic:**
  ```cobol
  MOVE WR-WCBC-CHARGEOFF-EXPENSE(1)
   TO QWR-WCBC-CHARGEOFF-EXPENSE-1.
  MOVE WR-WCBC-BAD-DEBT-BNKRPCY(1)
   TO QWR-WCBC-BAD-DEBT-BNKRPCY-1.
  ```
  - (Source: [[FILE:SPWRGS_SQL.CPY|SPWRGS_SQL.CPY]], Lines 401-425)

---

## SUMMARY FLOW

**Typical Charge-off/Write-off Workflow in OTIS:**
1. **Loan Evaluation:**  
   - Programs scan loan records for charge-off criteria (delinquency, bankruptcy, etc.)
2. **Eligibility Check:**  
   - Date and status checks ensure only qualifying loans are processed
3. **Amount Calculation:**  
   - Charge-off/write-off amounts are computed and posted to appropriate fields
4. **Loss Mitigation:**  
   - Special handling for recoveries, settlements, and bankruptcy
5. **Reporting:**  
   - Aggregated data is moved to reporting structures for management and regulatory use

---

## VISUAL REPRESENTATION

Here's a visual representation of the charge-off/write-off/loss mitigation process in OTIS:

<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Charge-off & Loss Mitigation Flow</text>
  
  <!-- Step 1: Loan Evaluation -->
  <rect x="60" y="70" width="200" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="160" y="100" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Loan Evaluation</text>
  <text x="160" y="120" text-anchor="middle" fill="white" font-size="12">Delinquency, Bankruptcy, Status</text>
  
  <!-- Step 2: Eligibility Check -->
  <rect x="320" y="70" width="200" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="420" y="100" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Eligibility Check</text>
  <text x="420" y="120" text-anchor="middle" fill="white" font-size="12">Date/Status Rules</text>
  
  <!-- Step 3: Amount Calculation -->
  <rect x="580" y="70" width="200" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="680" y="100" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Amount Calculation</text>
  <text x="680" y="120" text-anchor="middle" fill="white" font-size="12">Charge-off/Write-off</text>
  
  <!-- Step 4: Loss Mitigation -->
  <rect x="190" y="200" width="200" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="290" y="230" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Loss Mitigation</text>
  <text x="290" y="250" text-anchor="middle" fill="#15803d" font-size="12">Recovery, Settlement</text>
  
  <!-- Step 5: Reporting -->
  <rect x="510" y="200" width="200" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="610" y="230" text-anchor="middle" fill="#1e40af" font-size="16" font-weight="bold">Reporting</text>
  <text x="610" y="250" text-anchor="middle" fill="#1e40af" font-size="12">Management, Regulatory</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="260" y1="105" x2="320" y2="105" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="520" y1="105" x2="580" y2="105" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="680" y1="140" x2="610" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="420" y1="140" x2="290" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="290" y1="270" x2="610" y2="270" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

## KEY PROGRAMS AND FILES

- **Charge-off/Write-off Processing:**  
  - [[FILE:RZLNEXOLD.CBL|RZLNEXOLD.CBL]], [[FILE:LNEXTR.CBL|LNEXTR.CBL]], [[FILE:PROGRP.CBL|PROGRP.CBL]], [[FILE:BYRSUP.CBL|BYRSUP.CBL]]
- **Reporting:**  
  - [[FILE:SPWRGS_SQL.CPY|SPWRGS_SQL.CPY]], [[FILE:OPREP2.CBL|OPREP2.CBL]], [[FILE:UP1534.CBL|UP1534.CBL]]
- **Data Structures:**  
  - REPO-GAIN-AMT-MTD/YTD, REPO-VOL-NO-MTD/YTD, WR-NO-CHARGEOFFS, etc.

---

## SOURCE REFERENCES

- Charge-off eligibility logic: [[FILE:RZLNEXOLD.CBL|RZLNEXOLD.CBL]], Lines 501-525; [[FILE:LNEXTR.CBL|LNEXTR.CBL]], Lines 526-550
- Charge-off tracking fields: [[FILE:PROGRP.CBL|PROGRP.CBL]], Lines 401-425
- Reporting logic: [[FILE:SPWRGS_SQL.CPY|SPWRGS_SQL.CPY]], Lines 401-425; [[FILE:OPREP2.CBL|OPREP2.CBL]], Lines 226-250; [[FILE:UP1534.CBL|UP1534.CBL]], Lines 17626-17650
- Loss mitigation business rules: [[FILE:PROGRP.CBL|PROGRP.CBL]], Lines 2626-2650, 2676-2700

---

**In summary:**  
OTIS automates charge-off, write-off, and loss mitigation through a combination of business rules, COBOL program logic, and structured reporting, ensuring compliance and providing actionable data for financial management.

### What investor accounting and remittance functions does OTIS support?

OTIS supports a comprehensive set of investor accounting and remittance functions to manage the financial relationship between loan servicers and investors. These functions are critical for ensuring accurate reporting, timely payments, and compliance with investor agreements.

**BUSINESS PURPOSE:**
- Track loan balances and transactions for each investor
- Calculate and distribute principal and interest payments
- Generate remittance reports and statements
- Handle investor-specific accounting rules (e.g., pass-through, participation, recourse)
- Support reconciliation of investor accounts and funds
- Manage investor advances, recoveries, and charge-offs
- Provide audit trails and compliance documentation

**TECHNICAL IMPLEMENTATION:**

OTIS achieves these functions through a coordinated set of COBOL programs, copybooks, and business rules. Key capabilities include:

### 1. **Investor Remittance Calculation**
- Programs like **RECONC** generate summary lines by state and explanation for reconciliation of general ledger accounts related to investor remittance.
  - *Example:* "ACCOUNTING NEEDS A SUMMARY TO RECONCILE GL 3508 - NONFILE RECOVERY."  
    (Source: [[FILE:RECONC.CBL|RECONC.CBL]], Index: code)
- Calculation of principal and interest due to investors, including handling of prepayments, payoffs, and partial payments.

### 2. **Investor Reporting**
- Generation of detailed and summary reports for investor remittance, including breakdowns by state, group, and batch.
- Support for custom investor reporting formats and schedules.

### 3. **Investor Account Management**
- Tracking of investor balances, advances, recoveries, and charge-offs.
- Handling of investor-specific accounting rules, such as participation percentages, recourse arrangements, and pass-through accounting.

### 4. **Remittance Processing**
- Automated posting of remittance transactions to investor accounts.
- Calculation and distribution of funds according to investor agreements.
- Handling of exceptions, such as shortfalls, overpayments, and charge-offs.

### 5. **Reconciliation and Audit**
- Programs and routines to reconcile investor accounts with general ledger balances.
- Audit trails for all investor-related transactions.
- Support for adjusting journal entries and corrections.

### 6. **Investor Setup and Maintenance**
- Assignment and unassignment of investors to accounts (e.g., via programs like **CHINV**).
  - *Example:* "ASSIGN/UNASSIGN INVESTINET BY ACCOUNT"  
    (Source: [[FILE:CHINV.CBL|CHINV.CBL]], Index: code)
- Maintenance of investor master records, including contact information and accounting parameters.

### 7. **GL Interface and Posting**
- Posting of investor remittance transactions to the general ledger.
- Handling of bulk transfers, class transfers, and account conversions for investor accounts.
  - *Example:* "ADDED LOGIC FOR BULK TRANSFER OF REPO ACCOUNTS"  
    (Source: [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], Index: code)

### 8. **Investor-Specific Business Rules**
- Support for state-specific and investor-specific remittance rules.
- Handling of unique investor requirements, such as reporting formats, payment schedules, and reconciliation procedures.

---

**KEY PROGRAMS AND COMPONENTS INVOLVED:**

| Program/Copybook      | Description                                                                 |
|-----------------------|-----------------------------------------------------------------------------|
| RECONC                | Generates reconciliation summaries for investor remittance (GL 3508, etc.)  |
| CHINV                 | Assigns/unassigns investors to accounts                                    |
| [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]]          | Handles bulk/class transfers and GL posting for investor accounts           |
| Investor Master Files | Store investor setup, parameters, and contact info                         |
| Remittance Reports    | Generated by reporting programs for investor statements                     |

---

**Here's a visual representation of the investor accounting and remittance architecture in OTIS:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Investor Accounting & Remittance Architecture</text>
  
  <!-- Investor Master Data -->
  <rect x="50" y="80" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Investor Master Files</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Investor Setup, Parameters</text>
  
  <!-- Loan Servicing Data -->
  <rect x="650" y="80" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Loan Servicing Data</text>
  <text x="750" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Loan Balances, Transactions</text>
  
  <!-- Remittance Calculation -->
  <rect x="300" y="200" width="300" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Remittance Calculation</text>
  <text x="450" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">Principal, Interest, Advances</text>
  
  <!-- Remittance Processing -->
  <rect x="300" y="320" width="300" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="350" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Remittance Processing</text>
  <text x="450" y="370" text-anchor="middle" font-size="12" fill="#22c55e">GL Posting, Distribution</text>
  
  <!-- Reporting -->
  <rect x="50" y="420" width="200" height="60" fill="#fef9c3" stroke="#eab308" stroke-width="3" rx="8"/>
  <text x="150" y="450" text-anchor="middle" font-size="15" font-weight="bold" fill="#b45309">Investor Reports</text>
  
  <!-- Reconciliation -->
  <rect x="650" y="420" width="200" height="60" fill="#fef9c3" stroke="#eab308" stroke-width="3" rx="8"/>
  <text x="750" y="450" text-anchor="middle" font-size="15" font-weight="bold" fill="#b45309">GL Reconciliation</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="250" y1="120" x2="300" y2="240" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="850" y1="120" x2="600" y2="240" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="280" x2="450" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="400" x2="150" y2="420" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="400" x2="750" y2="420" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**SOURCE REFERENCES:**
- "ACCOUNTING NEEDS A SUMMARY TO RECONCILE GL 3508 - NONFILE RECOVERY."  
  (Source: [[FILE:RECONC.CBL|RECONC.CBL]], Index: code)
- "ASSIGN/UNASSIGN INVESTINET BY ACCOUNT"  
  (Source: [[FILE:CHINV.CBL|CHINV.CBL]], Index: code)
- "ADDED LOGIC FOR BULK TRANSFER OF REPO ACCOUNTS"  
  (Source: [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], Index: code)

If you need details on specific investor remittance reports, GL posting logic, or reconciliation routines, I can provide program-level breakdowns or code examples.

### How does OTIS manage automated clearing house (ACH) and electronic payment processing?

OTIS manages Automated Clearing House (ACH) and electronic payment processing through a coordinated set of COBOL programs, copybooks, and business logic that automate the creation, validation, transmission, and reconciliation of ACH transactions. Here’s a detailed breakdown of how OTIS handles these functions:

---

**BUSINESS PURPOSE**

- **Automated Payment Collection:** OTIS enables lenders to collect loan payments electronically via ACH, reducing manual processing and improving efficiency.
- **Deposit Transmission:** The system creates ACH deposit files for daily transmission to banks and payment processors.
- **Validation & Compliance:** Ensures payments are properly authorized, formatted, and compliant with banking regulations.
- **Exception Handling:** Manages errors, duplicate payments, and non-sufficient funds (NSF) scenarios.
- **Audit & Reporting:** Tracks all ACH transactions for audit, reporting, and reconciliation.

---

**TECHNICAL IMPLEMENTATION**

### 1. **ACH File Creation and Transmission**

- **ACHCRE Program:**  
  - Creates the ACH deposit file (`/USR/DATA/R1/SP/ACFILE`) for transmission.
  - Handles formatting of deposit information in ACH format.
  - Includes logic for custom fields and transmission requirements for different institutions (e.g., Carolina First, Regency Finance).
  - Manages file naming, FTP transmission, and remote copy logic.
  - Source: [[FILE:ACHCRE.CBL|ACHCRE.CBL]], lines 1-25, 101-125

- **ACHCRE_WKS, ACHCRE_DEF, ACHCRE_EVA:**  
  - Support programs for work file management, default values, and evaluation logic.
  - Source: [[FILE:P|P]]rograms ACHCRE_WKS, ACHCRE_DEF, ACHCRE_EVA

- **Copybooks Used:**  
  - **[[FILE:SP01AC.CPY|SP01AC.CPY]]:** Defines the ACH deposit (daily) file structure.
    - "ACH DEPOSIT (DAILY) FILE" – key fields for payment amount, account, settlement date.
    - Source: [[FILE:SP01AC.CPY|SP01AC.CPY]], lines 1-25
  - **[[FILE:ACHCRE_SCN.CPY|ACHCRE_SCN.CPY]]:** Screen definitions for ACH creation.
  - **[[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]]:** Extended company name fields for ACH records.
  - **[[FILE:GRWORKW.CPY|GRWORKW.CPY]]:** Work file structures for ACH processing.

### 2. **ACH Record Management and Clearing**

- **UPACHCL Program:**  
  - Manages clearing of ACH files and resets ACH record fields.
  - Entry paragraphs: ACH-NEW-CHAR, CLEAR-ACH-FILE.
  - Source: [[FILE:UPACHCL|UPACHCL]], [[FILE:I|I]]ndex: flows

- **UPACHI Program:**  
  - Handles writing, rewriting, and closing of ACH files.
  - Entry paragraphs: CLOSE-ACH-DONE-FILE, WRITE-ACH-FILE, REWRITE-ACH-FILE.
  - Source: [[FILE:UPACHI|UPACHI]], [[FILE:I|I]]ndex: flows

- **[[FILE:UPACHCL.CPY|UPACHCL.CPY]]:**  
  - Contains logic to clear ACH record fields before new processing.
  - Source: [[FILE:UPACHCL.CPY|UPACHCL.CPY]], lines 1-11

- **[[FILE:UP01ACH.CPY|UP01ACH.CPY]]:**  
  - Defines detailed ACH record fields (account, SSN, type, rates, allotment codes).
  - Source: [[FILE:UP01ACH.CPY|UP01ACH.CPY]], lines 26-50, 51-75, 101-109

### 3. **Electronic Payment Work Files**

- **[[FILE:SPFDEP.CPY|SPFDEP.CPY]]:**  
  - Defines the loan electronic payment file (OEP-FILE), keying on account number.
  - Source: [[FILE:SPFDEP.CPY|SPFDEP.CPY]], lines 1-8

- **[[FILE:SPFDEW.CPY|SPFDEW.CPY]]:**  
  - Defines the loan electronic payment work file (OEW-FILE), used for staging payments before final ACH transmission.
  - Source: [[FILE:SPFDEW.CPY|SPFDEW.CPY]], lines 1-8

### 4. **Validation, Exception Handling, and Reconciliation**

- **Bank Clearing and NSF Checks:**  
  - Programs and copybooks (e.g., [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], [[FILE:MBEOD5TR.CPY|MBEOD5TR.CPY]]) perform bank clearing logic, update work files, and check for NSF conditions.
  - Example logic:
    ```cobol
    PERFORM BANK-CLEARING-CHECKS-NSF
    PERFORM UPDATE-WORK-FILE-TRC
    ```
    - Source: [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], lines 3201-3225; [[FILE:MBEOD5TR.CPY|MBEOD5TR.CPY]], lines 3126-3150

- **Error Handling:**  
  - Duplicate payments, invalid routing/account numbers, and other exceptions are managed and logged.
  - Example: "ROUTING NO. INVALID" message and validation logic.
    - Source: [[FILE:DLMAN6.CBL|DLMAN6.CBL]], lines 451-475

### 5. **User Interaction and Screen Prompts**

- **Screen Programs:**  
  - Display prompts for ACH setup, confirmation, and error messages.
  - Example: "THIS ACCOUNT IS SET UP FOR AUTOMATIC ELECTRONIC (ACH) PAYMENTS!" prompt.
    - Source: [[FILE:LONPF1.CBL|LONPF1.CBL]], lines 1926-1950

---

**PROGRAM RELATIONSHIPS & CALL CHAIN**

Here's a visual representation:

<svg width="1200" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="600" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS ACH/Electronic Payment Processing</text>
  
  <!-- Main ACH Creation Program -->
  <rect x="520" y="60" width="160" height="80" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="600" y="95" text-anchor="middle" fill="white" font-size="18" font-weight="bold">ACHCRE</text>
  <text x="600" y="118" text-anchor="middle" fill="white" font-size="13">ACH File Creation</text>
  
  <!-- Row 1: Supporting Programs -->
  <rect x="50" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="120" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">UPACHCL</text>
  <text x="120" y="228" text-anchor="middle" fill="white" font-size="11">ACH Clearing</text>
  
  <rect x="230" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="300" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">UPACHI</text>
  <text x="300" y="228" text-anchor="middle" fill="white" font-size="11">ACH File Mgmt</text>
  
  <rect x="410" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="480" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">ACHCRE_WKS</text>
  <text x="480" y="228" text-anchor="middle" fill="white" font-size="11">Work File</text>
  
  <rect x="590" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="660" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">ACHCRE_DEF</text>
  <text x="660" y="228" text-anchor="middle" fill="white" font-size="11">Defaults</text>
  
  <rect x="770" y="180" width="140" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="840" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">ACHCRE_EVA</text>
  <text x="840" y="228" text-anchor="middle" fill="white" font-size="11">Evaluation</text>
  
  <!-- Row 2: Copybooks -->
  <rect x="50" y="320" width="140" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="120" y="350" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:SP01AC.CPY|SP01AC.CPY]]</text>
  <text x="120" y="368" text-anchor="middle" fill="white" font-size="11">ACH Deposit File</text>
  
  <rect x="230" y="320" width="140" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="300" y="350" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:UP01ACH.CPY|UP01ACH.CPY]]</text>
  <text x="300" y="368" text-anchor="middle" fill="white" font-size="11">ACH Record Fields</text>
  
  <rect x="410" y="320" width="140" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="480" y="350" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:SPFDEP.CPY|SPFDEP.CPY]]</text>
  <text x="480" y="368" text-anchor="middle" fill="white" font-size="11">Electronic Payment File</text>
  
  <rect x="590" y="320" width="140" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="660" y="350" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:SPFDEW.CPY|SPFDEW.CPY]]</text>
  <text x="660" y="368" text-anchor="middle" fill="white" font-size="11">Work File</text>
  
  <!-- Arrows from ACHCRE to supporting programs -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="600" y1="140" x2="120" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="300" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="480" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="660" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="140" x2="840" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <!-- Arrows from supporting programs to copybooks -->
  <line x1="120" y1="250" x2="120" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="300" y1="250" x2="300" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="480" y1="250" x2="480" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="660" y1="250" x2="660" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**KEY COPYBOOKS AND THEIR PURPOSE**

| Copybook Name      | Purpose/Description                                      |
|--------------------|---------------------------------------------------------|
| [[FILE:SP01AC.CPY|SP01AC.CPY]]         | ACH deposit (daily) file structure                      |
| [[FILE:UP01ACH.CPY|UP01ACH.CPY]]        | ACH record fields (account, SSN, type, rates, etc.)     |
| [[FILE:SPFDEP.CPY|SPFDEP.CPY]]         | Loan electronic payment file (OEP-FILE)                 |
| [[FILE:SPFDEW.CPY|SPFDEW.CPY]]         | Loan electronic payment work file (OEW-FILE)            |
| [[FILE:ACHCRE_SCN.CPY|ACHCRE_SCN.CPY]]     | Screen definitions for ACH creation                     |
| [[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]]     | Extended company name fields for ACH records            |
| [[FILE:GRWORKW.CPY|GRWORKW.CPY]]        | Work file structures for ACH processing                 |

---

**SUMMARY**

- OTIS automates ACH and electronic payment processing by generating ACH files, managing work files, validating transactions, and handling exceptions.
- Key programs include ACHCRE (file creation), UPACHCL/UPACHI (clearing and file management), and supporting copybooks for data structure.
- The system ensures compliance, accuracy, and efficiency in electronic payment operations.

**Sources:**
- [[FILE:ACHCRE.CBL|ACHCRE.CBL]], lines 1-25, 101-125
- [[FILE:UPACHCL.CPY|UPACHCL.CPY]], lines 1-11
- [[FILE:UP01ACH.CPY|UP01ACH.CPY]], lines 26-50, 51-75, 101-109
- [[FILE:SP01AC.CPY|SP01AC.CPY]], lines 1-25
- [[FILE:SPFDEP.CPY|SPFDEP.CPY]], lines 1-8
- [[FILE:SPFDEW.CPY|SPFDEW.CPY]], lines 1-8
- UPACHCL, UPACHI, ACHCRE, Index: flows, copybook_usage

If you need details on a specific ACH process (e.g., error handling, file transmission, user screens), let me know!

### What general ledger integration and accounting functions does OTIS provide?

OTIS provides a robust set of general ledger (GL) integration and accounting functions to support financial tracking, reporting, and compliance for loan servicing and related financial operations. Here’s a summary of the key GL integration and accounting capabilities in OTIS, based on the COBOL source and copybook definitions:

---

### BUSINESS PURPOSE

**OTIS General Ledger Integration and Accounting Functions:**

- **Automated Posting of Financial Transactions:**  
  OTIS automatically generates and posts GL entries for all significant financial events, including loan payments, charge-offs, write-offs, refunds, and ACH/electronic transactions.

- **GL Inquiry and Reporting:**  
  Users can inquire on GL transactions, balances, and account histories through dedicated inquiry programs and screens (e.g., GLINQ for transaction inquiry).

- **GL Account Maintenance:**  
  The system provides menu-driven maintenance for GL accounts, including adding, updating, and deleting accounts (e.g., GLMNMU - GL Maintenance Menu, GIMAN5 - GL Controls Maintenance).

- **Distribution and Allocation:**  
  OTIS supports detailed GL distribution records for loan payments and other transactions, ensuring amounts are allocated to the correct accounts (see [[FILE:LP01LXG.CPY|LP01LXG.CPY]] for GL distribution record structure).

- **Journal Entry Management:**  
  Journal detail lines are managed and stored for audit and reconciliation (see [[FILE:GL01JD.CPY|GL01JD.CPY]] for journal detail structure).

- **Batch and Real-Time Processing:**  
  Both batch and online (real-time) posting of GL entries are supported, allowing for end-of-day processing as well as immediate updates.

- **Investor and Branch Accounting:**  
  The system tracks GL activity by branch, account, and investor, supporting multi-entity accounting and remittance.

---

### TECHNICAL IMPLEMENTATION

**Key Programs and Copybooks:**

- **GL Transaction Inquiry:**  
  - **GLINQ ([[FILE:RZGINQ.CBL|RZGINQ.CBL]]):** General ledger transaction inquiry program for viewing transaction details.  
    _Source: [[FILE:RZGINQ.CBL|RZGINQ.CBL]], Index: code_

- **GL Account Maintenance:**  
  - **GLMNMU ([[FILE:GLMNMU.CBL|GLMNMU.CBL]]):** Menu and logic for maintaining GL accounts.  
    _Source: [[FILE:GLMNMU.CBL|GLMNMU.CBL]], Index: code_
  - **GIMAN5 ([[FILE:GIMAN5.CBL|GIMAN5.CBL]]):** GL controls maintenance, including configuration of GL-related parameters.  
    _Source: [[FILE:GIMAN5.CBL|GIMAN5.CBL]], Index: code_

- **GL Distribution and Posting:**  
  - **[[FILE:LP01LXG.CPY|LP01LXG.CPY]]:** Defines the structure for GL distribution records attached to loan payments, including account numbers and amounts.  
    _Source: [[FILE:LP01LXG.CPY|LP01LXG.CPY]], Index: code_
  - **[[FILE:GL01GT.CPY|GL01GT.CPY]]:** Defines the GL transaction file, including keys for source, date, branch, account, and line.  
    _Source: [[FILE:GL01GT.CPY|GL01GT.CPY]], Index: code_
  - **[[FILE:GL01JD.CPY|GL01JD.CPY]]:** Defines the GL journal detail line for detailed transaction tracking.  
    _Source: [[FILE:GL01JD.CPY|GL01JD.CPY]], Index: code_

- **GL File Handling and Integration:**  
  - **[[FILE:GLFDGL.CPY|GLFDGL.CPY]]:** Describes the GL master file structure.  
    _Source: [[FILE:GLFDGL.CPY|GLFDGL.CPY]], Index: code_
  - **[[FILE:UP01AGL.CPY|UP01AGL.CPY]], [[FILE:UP01ALXG.CPY|UP01ALXG.CPY]]:** ASCII export/import structures for GL master and distribution files, supporting integration with external systems.  
    _Source: [[FILE:UP01AGL.CPY|UP01AGL.CPY]], [[FILE:UP01ALXG.CPY|UP01ALXG.CPY]], Index: code_

- **GL Posting and Inquiry Utilities:**  
  - **GICOPY ([[FILE:GICOPY.CBL|GICOPY.CBL]]):** Utility for copying GL interface records, supporting batch integration and reconciliation.  
    _Source: [[FILE:GICOPY.CBL|GICOPY.CBL]], Index: code_
  - **GLDUP ([[FILE:GLDUP.CBL|GLDUP.CBL]]):** Handles duplication of GL account numbers across branches.  
    _Source: [[FILE:GLDUP.CBL|GLDUP.CBL]], Index: code_

- **GL Reporting and SQL Integration:**  
  - Programs such as [[FILE:RZMTRL.CBL|RZMTRL.CBL]] and [[FILE:RZMTR1.CBL|RZMTR1.CBL]] use embedded SQL to extract GL balances and activity for reporting.  
    _Source: [[FILE:RZMTRL.CBL|RZMTRL.CBL]], [[FILE:RZMTR1.CBL|RZMTR1.CBL]], Index: code_

---

### DATA FLOW AND INTEGRATION

- **Transaction Generation:**  
  When a financial event occurs (e.g., payment, charge-off), OTIS generates GL distribution records ([[FILE:LP01LXG.CPY|LP01LXG.CPY]]) and posts them to the GL transaction file ([[FILE:GL01GT.CPY|GL01GT.CPY]]).

- **Journal Entry Creation:**  
  Detailed journal entries are created and stored ([[FILE:GL01JD.CPY|GL01JD.CPY]]), supporting audit trails and reconciliation.

- **GL Master Update:**  
  The GL master file ([[FILE:GLFDGL.CPY|GLFDGL.CPY]]) is updated to reflect new balances and activity.

- **Inquiry and Reporting:**  
  Users and batch processes can access transaction and balance data for reporting, reconciliation, and compliance.

---

### ARCHITECTURE DIAGRAM

Here's a visual representation of the OTIS general ledger integration and accounting architecture:

<svg width="700" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS General Ledger Integration</text>
  
  <!-- Presentation Layer -->
  <rect x="50" y="70" width="600" height="100" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="350" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="350" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Screens/Programs: GLINQ, GLMNMU, GIMAN5</text>
  <text x="350" y="140" text-anchor="middle" font-size="12" fill="#6366f1">User Inquiry, Maintenance</text>
  
  <!-- Business Logic Layer -->
  <rect x="50" y="200" width="600" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="350" y="225" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="350" y="250" text-anchor="middle" font-size="14" fill="#1e40af">Programs: GICOPY, GLDUP, Batch Posting</text>
  <text x="350" y="270" text-anchor="middle" font-size="12" fill="#3b82f6">Posting, Distribution, Integration</text>
  
  <!-- Data Layer -->
  <rect x="50" y="330" width="600" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="350" y="355" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="350" y="380" text-anchor="middle" font-size="14" fill="#15803d">Files: [[FILE:GL01GT.CPY|GL01GT.CPY]], [[FILE:GL01JD.CPY|GL01JD.CPY]], [[FILE:GLFDGL.CPY|GLFDGL.CPY]]</text>
  <text x="350" y="400" text-anchor="middle" font-size="12" fill="#22c55e">Transaction, Journal, Master Files</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="350" y1="170" x2="350" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="300" x2="350" y2="330" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

### SUMMARY OF KEY COPYBOOKS (with descriptions)

| Copybook         | Description                                                                 |
|------------------|-----------------------------------------------------------------------------|
| [[FILE:GL01GT.CPY|GL01GT.CPY]]       | General ledger transaction file structure (keys, amounts, dates)            |
| [[FILE:GL01JD.CPY|GL01JD.CPY]]       | General ledger journal detail line (audit trail, transaction details)       |
| [[FILE:GLFDGL.CPY|GLFDGL.CPY]]       | General ledger master file (account balances, descriptions)                 |
| [[FILE:LP01LXG.CPY|LP01LXG.CPY]]      | Loan payment GL distribution record (account numbers, amounts)              |
| [[FILE:UP01AGL.CPY|UP01AGL.CPY]]      | ASCII GL master file (external integration/export)                          |
| [[FILE:UP01ALXG.CPY|UP01ALXG.CPY]]     | ASCII GL distribution file (external integration/export)                    |

---

### CONCLUSION

**OTIS provides full-featured general ledger integration and accounting, including:**
- Automated and manual posting of all financial events
- Detailed tracking and audit via journal and transaction files
- User-driven inquiry and maintenance
- Support for batch and real-time processing
- Integration with external accounting/reporting systems

**References:**
- [[FILE:RZGINQ.CBL|RZGINQ.CBL]], [[FILE:GLMNMU.CBL|GLMNMU.CBL]], [[FILE:GIMAN5.CBL|GIMAN5.CBL]], [[FILE:LP01LXG.CPY|LP01LXG.CPY]], [[FILE:GL01GT.CPY|GL01GT.CPY]], [[FILE:GL01JD.CPY|GL01JD.CPY]], [[FILE:GLFDGL.CPY|GLFDGL.CPY]], [[FILE:UP01AGL.CPY|UP01AGL.CPY]], [[FILE:UP01ALXG.CPY|UP01ALXG.CPY]], [[FILE:GICOPY.CBL|GICOPY.CBL]], [[FILE:GLDUP.CBL|GLDUP.CBL]], [[FILE:RZMTRL.CBL|RZMTRL.CBL]], [[FILE:RZMTR1.CBL|RZMTR1.CBL]] (Index: code, copybook_usage)

### How does OTIS handle multi-currency or international loan servicing if applicable?

OTIS provides limited but targeted support for multi-currency and international loan servicing, primarily through specialized data structures, calculation routines, and business logic designed to accommodate loans outside the standard domestic context.

**BUSINESS PURPOSE:**
- Support for international loans (e.g., Mexico, Canada, cross-border lending) is implemented to handle:
  - Currency conversion for principal, interest, and fees
  - Country-specific calculation formulas (e.g., maintenance fees, service charges)
  - Regulatory compliance for non-US jurisdictions
  - Reporting and accounting for loans denominated in foreign currencies

**TECHNICAL IMPLEMENTATION:**

1. **Special Calculation Logic for International Loans:**
   - Programs such as CALCZL and SPMAN3 include logic for Mexico-specific maintenance fee formulas and other international rules.
     - Example: "SP-ROUNF-MFEE. THIS WILL FORCE CALCZL TO ROUND UP THE MAINTFEE FOR MEXICO MFFRMLA 05 LOANS..." (Source: [[FILE:SPMAN3.CBL|SPMAN3.CBL]], Lines 76-100)
     - "ONLY DIFFERENCE BETWEEN 17 AND 60/61 IS THE RATE" (Source: [[FILE:CALCZL.CBL|CALCZL.CBL]], Lines 4926-4950)
   - These routines use country codes and formula switches (e.g., SP-SCFRMLA) to apply different interest rates, service charges, and rounding rules for international loans.

2. **Data Structures for International Information:**
   - Copybooks such as [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]] contain fields for reporting secondary marketing agency interest and mortgage info, which may be relevant for international or cross-border loans.
     - Example: "TO INDICATE A SECONDARY MARKETING AGENCY'S INTEREST IN A LOAN OR TO REPORT THE MORTGAGE..." (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Lines 726-750)

3. **Currency Handling:**
   - While explicit multi-currency fields (e.g., currency codes, exchange rates) are not directly shown in the retrieved context, the presence of country-specific formulas and logic implies that currency conversion and handling are managed within calculation routines and reporting modules.

4. **Regulatory and Compliance Logic:**
   - Comments and code indicate special handling for international regulatory requirements, such as fee calculation restrictions and reporting formats.
   - Example: "SHALL NEVER BE IMPOSED ON ANY PORTION OF ANY LOAN USED TO PAY ANY EXISTING LOAN OWING BY THE SAME BORROWER" (Source: [[FILE:CALCZL.CBL|CALCZL.CBL]], Lines 4926-4950)

5. **Reporting and Accounting:**
   - International loans are included in reporting modules, with logic to ensure correct accounting for foreign-denominated balances and transactions.

**SUMMARY OF KEY COMPONENTS:**

| Program/Copybook         | Purpose/Role in International Servicing                                 |
|--------------------------|-------------------------------------------------------------------------|
| [[FILE:CALCZL.CBL|CALCZL.CBL]]               | Calculation logic for international loan formulas, rates, fees          |
| [[FILE:SPMAN3.CBL|SPMAN3.CBL]]               | Maintenance fee rounding and formula selection for Mexico loans          |
| [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]           | Data structures for international mortgage/secondary market reporting    |
| Various formula switches | Country-specific logic (e.g., SP-SCFRMLA, SP-ROUNF-MFEE)                |

**LIMITATIONS:**
- The retrieved context does not show a comprehensive multi-currency engine (e.g., dynamic currency code fields, exchange rate tables).
- International support appears to be implemented via conditional logic and specialized formulas rather than a full-featured multi-currency subsystem.

**CONCLUSION:**
OTIS handles international loan servicing by embedding country-specific calculation logic, reporting structures, and compliance routines within its COBOL programs and copybooks. This enables support for select international markets (notably Mexico), but does not provide a generalized multi-currency platform.

**Sources:**
- [[FILE:SPMAN3.CBL|SPMAN3.CBL]], Lines 76-100 (maintenance fee logic for Mexico)
- [[FILE:CALCZL.CBL|CALCZL.CBL]], Lines 4926-4950 (rate/service charge differences for international loans)
- [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Lines 726-750 (international mortgage reporting fields)

---

Here's a visual representation of how international loan servicing logic is integrated within OTIS:

<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS International Loan Servicing Architecture</text>
  
  <!-- Main Calculation Programs -->
  <rect x="100" y="80" width="200" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="200" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">CALCZL</text>
  <text x="200" y="130" text-anchor="middle" fill="white" font-size="12">International Loan Calculations</text>
  
  <rect x="350" y="80" width="200" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="450" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">SPMAN3</text>
  <text x="450" y="130" text-anchor="middle" fill="white" font-size="12">Mexico Fee Logic</text>
  
  <rect x="600" y="80" width="200" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="700" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Reporting Modules</text>
  <text x="700" y="130" text-anchor="middle" fill="white" font-size="12">International Reporting</text>
  
  <!-- Copybook for Data Structures -->
  <rect x="350" y="200" width="200" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="450" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">[[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]</text>
  <text x="450" y="250" text-anchor="middle" fill="white" font-size="12">International Mortgage Data</text>
  
  <!-- Formula Switches -->
  <rect x="100" y="320" width="200" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="200" y="350" text-anchor="middle" fill="white" font-size="16" font-weight="bold">SP-SCFRMLA</text>
  <text x="200" y="370" text-anchor="middle" fill="white" font-size="12">Country-Specific Formula Switch</text>
  
  <rect x="600" y="320" width="200" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="700" y="350" text-anchor="middle" fill="white" font-size="16" font-weight="bold">SP-ROUNF-MFEE</text>
  <text x="700" y="370" text-anchor="middle" fill="white" font-size="12">Mexico Maintenance Fee Rounding</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="200" y1="150" x2="450" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="150" x2="450" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="700" y1="150" x2="450" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="270" x2="200" y2="320" stroke="#d97706" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="270" x2="700" y2="320" stroke="#d97706" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

### What financial analytics and portfolio management capabilities exist in OTIS?

OTIS provides a comprehensive set of financial analytics and portfolio management capabilities to support loan servicing, asset tracking, and business decision-making. These capabilities are implemented through a combination of COBOL programs, copybooks, and business logic modules that automate calculations, reporting, and portfolio aggregation.

**Financial Analytics Capabilities:**

- **Portfolio Totals & Aggregation:**
  - OTIS tracks and aggregates portfolio totals for different business lines (e.g., sales finance, direct lending).
  - Example: The `PORTFOLIO-TOTALS` structure in [[FILE:CPCORP.CBL|CPCORP.CBL]] accumulates amounts and counts for each portfolio type.
    - `PORT-NO` (portfolio number)
    - `PORT-AMT` (portfolio amount)
    - `ACCUM-DL-NO`, `ACCUM-DL-AMT` (direct lending totals)
    - `ACCUM-SF-NO`, `ACCUM-SF-AMT` (sales finance totals)
  - (Source: [[FILE:CPCORP.CBL|CPCORP.CBL]], Index: code)

- **Earnings & GL Analytics:**
  - Programs such as MBALR3, EMOTRG, MBALR6H, and RZTRRG perform detailed analytics on earnings, interest, and general ledger (GL) accounts.
  - They process fields like `QTP-EARNINGS-1` to `QTP-EARNINGS-5`, `QTP-GLNO-1` to `QTP-GLNO-3`, and related amounts for financial reporting.
  - These analytics support monthly, quarterly, and annual reporting cycles.
  - (Source: [[FILE:MBALR3.CBL|MBALR3.CBL]], [[FILE:EMOTRG.CBL|EMOTRG.CBL]], [[FILE:MBALR6H.CBL|MBALR6H.CBL]], [[FILE:RZTRRG.CBL|RZTRRG.CBL]], Index: code)

- **Classification & Segmentation:**
  - OTIS segments portfolios by business type, such as personal services, insurance, banking, rental, utilities, etc.
  - Example: The `TRW-K1-CLASSIFICATION` field in [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]] enables classification for analytics and reporting.
  - (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code)

- **Loan-Level Analytics:**
  - Programs like LPEARN and LPAVCR compute earnings and available credit at the individual loan level.
  - These calculations feed into broader portfolio analytics and risk assessment.
  - (Source: [[FILE:LPEARN|LPEARN]], [[FILE:LPAVCR|LPAVCR]], [[FILE:I|I]]ndex: flows)

- **General Ledger Integration:**
  - OTIS integrates with GL systems to track financial transactions, earnings, and balances.
  - GL account numbers and amounts are processed for reconciliation and compliance.
  - (Source: [[FILE:MBALR3.CBL|MBALR3.CBL]], [[FILE:EMOTRG.CBL|EMOTRG.CBL]], Index: code)

**Portfolio Management Capabilities:**

- **Purchased/Sold Portfolio Tracking:**
  - OTIS tracks portfolios purchased from or sold to other companies, supporting M&A and asset transfer analytics.
  - Example: The TRW K2 segment in [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]] records the name of the company involved in portfolio transactions.
  - (Source: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code)

- **Multi-Portfolio Support:**
  - Structures like `PORTFOLIO-TOTALS OCCURS 2 TIMES` allow OTIS to manage multiple portfolios simultaneously (e.g., direct lending vs. sales finance).
  - (Source: [[FILE:CPCORP.CBL|CPCORP.CBL]], Index: code)

- **Accruals & Earnings Management:**
  - Programs such as [[FILE:IOBRIN.CBL|IOBRIN.CBL]] manage accruals, earnings, and balances for each portfolio, supporting accurate financial tracking.
  - (Source: [[FILE:IOBRIN.CBL|IOBRIN.CBL]], Index: code)

- **Reporting & Analytics Modules:**
  - Dedicated programs generate financial reports, trial balances, aged receivables, and other analytics for portfolio managers.
  - These reports support business decisions, compliance, and performance monitoring.

**Example Data Structures and Analytics:**

```cobol
01  PORTFOLIO-TOTALS  OCCURS 2 TIMES.
    03  PORT-NO        PIC S9(8)      COMP.
    03  PORT-AMT       PIC S9(13)V99  COMP.

01  ACCUM-DL-NO            PIC S9(8)     COMP.
01  ACCUM-DL-AMT           PIC S9(13)V99 COMP.
01  ACCUM-SF-NO            PIC S9(8)     COMP.
01  ACCUM-SF-AMT           PIC S9(13)V99 COMP.
```
(Source: [[FILE:CPCORP.CBL|CPCORP.CBL]], Index: code)

**Summary Table: Key Programs and Copybooks**

| Program/Copybook         | Purpose/Functionality                                         |
|--------------------------|--------------------------------------------------------------|
| [[FILE:CPCORP.CBL|CPCORP.CBL]]               | Portfolio totals, aggregation, and grand totals              |
| [[FILE:MBALR3.CBL|MBALR3.CBL]], [[FILE:EMOTRG.CBL|EMOTRG.CBL]]   | Earnings, GL analytics, financial reporting                  |
| [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]]           | Portfolio classification, purchased/sold tracking            |
| LPEARN, LPAVCR           | Loan-level earnings and available credit calculations        |
| [[FILE:IOBRIN.CBL|IOBRIN.CBL]]               | Accruals, balances, and GL integration                       |

**Business Value:**
- Enables portfolio managers to track performance, risk, and profitability across multiple asset classes.
- Supports regulatory compliance through accurate GL integration and reporting.
- Facilitates business decisions on asset purchases, sales, and portfolio rebalancing.

---

**Here's a visual representation of OTIS's financial analytics and portfolio management architecture:**

<svg width="900" height="520" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Financial Analytics & Portfolio Management</text>
  
  <!-- Portfolio Data Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Portfolio Data Layer</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Structures: PORTFOLIO-TOTALS, ACCUM-DL-NO/AMT, ACCUM-SF-NO/AMT</text>
  <text x="450" y="140" text-anchor="middle" font-size="12" fill="#6366f1">Portfolio Aggregation & Classification</text>
  
  <!-- Analytics Layer -->
  <rect x="50" y="180" width="800" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="205" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Analytics Layer</text>
  <text x="450" y="230" text-anchor="middle" font-size="14" fill="#1e40af">Programs: MBALR3, EMOTRG, MBALR6H, LPEARN, LPAVCR</text>
  <text x="450" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">Earnings, GL Analytics, Loan-Level Calculations</text>
  
  <!-- Reporting Layer -->
  <rect x="50" y="310" width="800" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="335" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Reporting Layer</text>
  <text x="450" y="360" text-anchor="middle" font-size="14" fill="#15803d">Reports: Portfolio Totals, Earnings, GL Reconciliation</text>
  <text x="450" y="380" text-anchor="middle" font-size="12" fill="#22c55e">Business Decision Support</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="450" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="280" x2="450" y2="310" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**References:**
- Portfolio totals and aggregation: [[FILE:CPCORP.CBL|CPCORP.CBL]], Index: code
- Earnings and GL analytics: [[FILE:MBALR3.CBL|MBALR3.CBL]], [[FILE:EMOTRG.CBL|EMOTRG.CBL]], [[FILE:MBALR6H.CBL|MBALR6H.CBL]], [[FILE:RZTRRG.CBL|RZTRRG.CBL]], Index: code
- Classification and purchased/sold tracking: [[FILE:SPFDMETRO2.CPY|SPFDMETRO2.CPY]], Index: code
- Loan-level analytics: LPEARN, LPAVCR, Index: flows

If you need details on a specific analytic or portfolio management function, let me know!

## 2.2 Key Business Processes Supported
### What are the main end-to-end business processes supported by OTIS?

OTIS (Online Transaction Information System) supports a comprehensive set of end-to-end business processes for loan servicing, financial management, and portfolio operations. These processes are designed to cover the full lifecycle of loans and related financial products, ensuring compliance, efficiency, and robust reporting. Here’s a summary of the main business processes supported by OTIS:

---

**1. Loan Origination and Setup**
- Captures new loan applications, validates applicant data, and sets up loan accounts.
- Integrates with credit scoring and underwriting modules.
- Assigns loan numbers, terms, and initial balances.
- (Source: [[FILE:MBMALL|MBMALL]], [[FILE:BHMAIN|BHMAIN]], [[FILE:ALMAIN|ALMAIN]], [[FILE:I|I]]ndex: flows/programs)

**2. Payment Processing**
- Handles customer payments (ACH, check, cash, electronic).
- Validates payment amounts, applies payments to principal, interest, and fees.
- Updates account balances and payment histories.
- Generates receipts and notifications.
- (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], LONPW9, Index: code/programs)

**3. General Ledger Integration and Accounting**
- Posts financial transactions to the general ledger.
- Supports multi-branch and multi-entity accounting.
- Manages accruals, reversals, and adjustments.
- Reconciles subsidiary ledgers with the GL.
- (Source: [[FILE:LPEOD5.CBL|LPEOD5.CBL]], [[FILE:RZMOR3.CBL|RZMOR3.CBL]], Index: code)

**4. Portfolio Management and Analytics**
- Tracks loan performance, delinquency, and aging.
- Provides reporting on asset quality, risk, and profitability.
- Supports segmentation by branch, product, and customer type.
- Enables scenario analysis and forecasting.
- (Source: [[FILE:MBALRG|MBALRG]], [[FILE:MBAGED|MBAGED]], [[FILE:RZAGE5O|RZAGE5O]], [[FILE:I|I]]ndex: flows)

**5. Collections and Delinquency Management**
- Identifies overdue accounts and triggers collection workflows.
- Generates collection letters, payment reminders, and follow-up schedules.
- Tracks collector actions and outcomes.
- (Source: [[FILE:AUDIQ0|AUDIQ0]], [[FILE:CHGROS|CHGROS]], [[FILE:I|I]]ndex: flows)

**6. Loan Maintenance and Servicing**
- Supports account updates, payment schedule changes, and customer service actions.
- Handles loan modifications, extensions, and renewals.
- Manages escrow accounts, insurance, and tax payments.
- (Source: [[FILE:BHMAIN|BHMAIN]], [[FILE:ALMAIN|ALMAIN]], [[FILE:I|I]]ndex: flows/programs)

**7. End-of-Day/End-of-Month Processing**
- Performs batch updates, reconciliations, and reporting.
- Closes out daily and monthly cycles, posts accruals, and generates statements.
- (Source: [[FILE:EOMAIN|EOMAIN]], [[FILE:ENDYER|ENDYER]], [[FILE:UPDEOY|UPDEOY]], [[FILE:I|I]]ndex: programs)

**8. Regulatory Reporting and Compliance**
- Generates required reports for federal, state, and internal compliance.
- Tracks audit trails and exception handling.
- (Source: [[FILE:EOYUPD|EOYUPD]], [[FILE:RZMTRL|RZMTRL]], [[FILE:I|I]]ndex: flows)

**9. Data Integration and External Interfaces**
- Interfaces with external systems for ACH, credit bureaus, and payment processors.
- Supports import/export of data for analytics and reporting.
- (Source: [[FILE:ARRAYBR.CPY|ARRAYBR.CPY]], [[FILE:LP01LR.CPY|LP01LR.CPY]], Index: code/copybooks)

**10. User Interface and Menu Navigation**
- Provides menu-driven screens for transaction entry, inquiry, and reporting.
- Supports role-based access and workflow routing.
- (Source: [[FILE:SCRN1|SCRN1]], [[FILE:WIMENU.CBL|WIMENU.CBL]], Index: flows/code)

---

**End-to-End Workflow Example:**
A typical loan lifecycle in OTIS might look like:
1. **Origination:** Loan application entered → Credit check → Loan setup
2. **Servicing:** Payments processed → Account updated → Delinquency tracked
3. **Accounting:** Transactions posted to GL → Reconciliation performed
4. **Portfolio Management:** Performance analyzed → Reports generated
5. **Collections:** Overdue accounts flagged → Collection actions initiated
6. **Maintenance:** Account modifications handled → Customer service provided
7. **End-of-Period:** Batch processing → Statements and compliance reports generated

---

**Here's a visual representation:**

<svg width="1100" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="550" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS End-to-End Business Processes</text>
  
  <!-- Row 1 -->
  <rect x="50" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Loan Origination</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="11">Setup & Underwriting</text>
  
  <rect x="270" y="80" width="180" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Payment Processing</text>
  <text x="360" y="130" text-anchor="middle" fill="white" font-size="11">ACH, Check, Cash</text>
  
  <rect x="490" y="80" width="180" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">GL Integration</text>
  <text x="580" y="130" text-anchor="middle" fill="white" font-size="11">Accounting & Posting</text>
  
  <rect x="710" y="80" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="800" y="110" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Portfolio Mgmt</text>
  <text x="800" y="130" text-anchor="middle" fill="#15803d" font-size="11">Analytics & Reporting</text>
  
  <!-- Row 2 -->
  <rect x="50" y="200" width="180" height="70" fill="#ef4444" stroke="#dc2626" stroke-width="3" rx="8"/>
  <text x="140" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Collections</text>
  <text x="140" y="250" text-anchor="middle" fill="white" font-size="11">Delinquency Mgmt</text>
  
  <rect x="270" y="200" width="180" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="360" y="230" text-anchor="middle" fill="#1e40af" font-size="16" font-weight="bold">Loan Maintenance</text>
  <text x="360" y="250" text-anchor="middle" fill="#1e40af" font-size="11">Servicing & Updates</text>
  
  <rect x="490" y="200" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="580" y="230" text-anchor="middle" fill="#4338ca" font-size="16" font-weight="bold">End-of-Day/Month</text>
  <text x="580" y="250" text-anchor="middle" fill="#4338ca" font-size="11">Batch Processing</text>
  
  <rect x="710" y="200" width="180" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="800" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Compliance</text>
  <text x="800" y="250" text-anchor="middle" fill="white" font-size="11">Regulatory Reporting</text>
  
  <!-- Row 3 -->
  <rect x="270" y="320" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="360" y="350" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Data Integration</text>
  <text x="360" y="370" text-anchor="middle" fill="#15803d" font-size="11">External Interfaces</text>
  
  <rect x="490" y="320" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="580" y="350" text-anchor="middle" fill="#4338ca" font-size="16" font-weight="bold">User Interface</text>
  <text x="580" y="370" text-anchor="middle" fill="#4338ca" font-size="11">Menu Navigation</text>
  
  <!-- Arrows between boxes (simplified for clarity) -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="115" x2="270" y2="115" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="115" x2="490" y2="115" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="670" y1="115" x2="710" y2="115" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="140" y1="150" x2="140" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="150" x2="360" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="150" x2="580" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="800" y1="150" x2="800" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="270" x2="360" y2="320" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="270" x2="580" y2="320" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
OTIS supports the full spectrum of loan servicing and financial management processes, from origination to payoff, including payment handling, accounting, portfolio analytics, collections, compliance, and user interaction. This enables financial institutions to manage loans efficiently, maintain regulatory compliance, and make informed business decisions.

**Sources:**  
- MBMALL, BHMAIN, ALMAIN, [[FILE:APIPAY.CBL|APIPAY.CBL]], LONPW9, [[FILE:LPEOD5.CBL|LPEOD5.CBL]], [[FILE:RZMOR3.CBL|RZMOR3.CBL]], MBALRG, MBAGED, RZAGE5O, AUDIQ0, CHGROS, EOMAIN, ENDYER, UPDEOY, EOYUPD, RZMTRL, [[FILE:ARRAYBR.CPY|ARRAYBR.CPY]], [[FILE:LP01LR.CPY|LP01LR.CPY]], SCRN1, [[FILE:WIMENU.CBL|WIMENU.CBL]] (Index: flows, programs, code, copybooks)

### How does OTIS handle the daily payment posting and application process?

OTIS handles the daily payment posting and application process through a coordinated workflow involving several specialized COBOL programs, copybooks, and business logic modules. The process ensures payments are validated, applied to loan accounts, and all relevant balances and histories are updated accurately.

---

**BUSINESS PURPOSE:**
- Accepts daily payment batches from various sources (lockbox, teller, online, etc.)
- Validates each payment against account status, loan rules, and business constraints
- Applies payments to principal, interest, late charges, escrow, and other balances according to contract terms and regulatory requirements
- Updates loan records, payment histories, and generates exception logs for errors or unusual conditions
- Supports batch and manual posting, including special handling for bankruptcies, reversals, payoffs, and deferred payments

---

**TECHNICAL IMPLEMENTATION:**

### 1. **Batch Payment Intake**
- Payments are collected into work files (e.g., WK-FILE) for batch processing.
- Programs like **LONPF9** and **DAILY** initiate the batch payment routine.

### 2. **Validation and Routing**
- Each payment is validated for:
  - Account status (active, bankrupt, repo, etc.)
  - Payment amount and type (regular, payoff, reversal, etc.)
  - Compliance with business rules (e.g., cannot post more than one transaction for interbranch posting, see [[FILE:LONPF0.CBL|LONPF0.CBL]])
- Conditional logic determines the posting path:
  - **Bankruptcy**: Special routines (BANKRUPT-POSTING)
  - **Reversal**: REVERSAL-POSTING
  - **Payoff**: PAYOFF-POSTING
  - **Regular Payment**: PAYMENT-POSTING

  (Source: [[FILE:LONPW9.CBL|LONPW9.CBL]], lines 1726-1750)

### 3. **Application of Payment**
- Payments are applied to balances in a specific order:
  - Principal
  - Interest
  - Late charges
  - Escrow (if applicable)
- Calculations ensure correct allocation, including handling for deferred payments and prepayments.
- Example logic:
  ```cobol
  IF DF-POSTING
     ADD DEF-NODEF TO LN-TOTNODEF
     ADD DEF-NODEFACT TO LN-TOTNODEFACT.
  PERFORM SETUP-PRIN-PAYMNTD.
  PERFORM SETUP-PDTH-LPREC.
  ```
  (Source: [[FILE:LONPFC.CBL|LONPFC.CBL]], lines 1226-1250)

### 4. **Exception Handling**
- If a payment cannot be applied (e.g., due to account status or validation failure), error codes and messages are generated.
- Exception routines log issues and may trigger notifications or require manual review.
- Example:
  ```cobol
  IF ERRCD = "E" OR "X" OR "M"
     MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
     MOVE 69 TO RETURN-STA
  ```
  (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], lines 1476-1500)

### 5. **Updating Records**
- Successful payments update loan records, payment histories, and related files.
- Copybooks such as **[[FILE:DAILY_WKS.CPY|DAILY_WKS.CPY]]**, **[[FILE:LPAPRZ.CPY|LPAPRZ.CPY]]**, **[[FILE:REBATE.CPY|REBATE.CPY]]**, and others define the data structures for these updates.
- Batch routines ensure all changes are committed and files are synchronized.

### 6. **Reporting and Audit**
- End-of-day routines generate reports on posted payments, exceptions, and account statuses.
- Audit logs are maintained for compliance and reconciliation.

---

**KEY PROGRAMS INVOLVED:**
- **DAILY**: Orchestrates daily batch processing, uses many copybooks for loan/payment data.
- **LONPF9**: Handles batch payment routines, selection, and application logic.
- **LONPFC**: Manages payment allocation, contractual calculations, and exception handling.
- **APIPAY**: Handles payment validation and posting, including error logging.
- **LONPW9**: Manages posting logic for bankruptcies, reversals, payoffs, and regular payments.

**KEY COPYBOOKS USED (examples):**
- **[[FILE:DAILY_WKS.CPY|DAILY_WKS.CPY]]** – Daily processing working storage
- **[[FILE:LPAPRZ.CPY|LPAPRZ.CPY]]** – Loan payment and amortization structures
- **[[FILE:REBATE.CPY|REBATE.CPY]]** – Rebate calculations and structures
- **[[FILE:LPPDUE.CPY|LPPDUE.CPY]]** – Payment due tracking
- **[[FILE:LPAMTSW.CPY|LPAMTSW.CPY]]** – Loan amounts working storage

---

**WORKFLOW SUMMARY:**
1. Payments loaded into batch files
2. DAILY program initiates processing
3. Payments routed to appropriate posting routines (LONPF9, LONPFC, APIPAY, LONPW9)
4. Validation and application logic executed
5. Exceptions logged and handled
6. Records updated, reports generated

---

**Here's a visual representation:**
<svg width="1200" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="600" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">Daily Payment Posting Workflow (OTIS)</text>
  
  <!-- Row 1: Intake -->
  <rect x="100" y="80" width="220" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="210" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Batch Payment Intake</text>
  <text x="210" y="130" text-anchor="middle" fill="white" font-size="11">WK-FILE, DAILY</text>
  
  <!-- Row 2: Validation -->
  <rect x="400" y="80" width="220" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="510" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Validation & Routing</text>
  <text x="510" y="130" text-anchor="middle" fill="white" font-size="11">LONPF9, APIPAY</text>
  
  <!-- Row 3: Application -->
  <rect x="700" y="80" width="220" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="810" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Payment Application</text>
  <text x="810" y="130" text-anchor="middle" fill="white" font-size="11">LONPFC, LONPW9</text>
  
  <!-- Row 4: Exception Handling -->
  <rect x="100" y="200" width="220" height="70" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="210" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Exception Handling</text>
  <text x="210" y="250" text-anchor="middle" fill="white" font-size="11">Error Logging</text>
  
  <!-- Row 5: Record Update -->
  <rect x="400" y="200" width="220" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="510" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Record Update</text>
  <text x="510" y="250" text-anchor="middle" fill="white" font-size="11">Loan Files, Histories</text>
  
  <!-- Row 6: Reporting -->
  <rect x="700" y="200" width="220" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="810" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Reporting & Audit</text>
  <text x="810" y="250" text-anchor="middle" fill="white" font-size="11">End-of-Day Reports</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="320" y1="115" x2="400" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="620" y1="115" x2="700" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="210" y1="150" x2="210" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="510" y1="150" x2="510" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="810" y1="150" x2="810" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

**SOURCE REFERENCES:**
- [[FILE:LONPW9.CBL|LONPW9.CBL]], lines 1726-1750 (posting logic)
- [[FILE:APIPAY.CBL|APIPAY.CBL]], lines 1476-1500 (error handling, posting)
- [[FILE:LONPFC.CBL|LONPFC.CBL]], lines 1226-1300 (payment allocation, contractual calculations)
- [[FILE:DAILY.CBL|DAILY.CBL]], copybook usage (defines loan/payment data structures)

If you need details on a specific step, program, or copybook, let me know!

### What batch processing cycles does OTIS run (daily, monthly, year-end)?

OTIS (Online Transaction Information System) runs several key batch processing cycles to support loan servicing and financial operations. These cycles are scheduled and executed to ensure accurate posting, updating, and reporting of financial data across the system. The main batch cycles are:

---

**1. Daily Batch Processing**
- **Purpose:** Handles daily payment posting, delinquency updates, contractual status changes, and other routine loan servicing tasks.
- **Key Activities:**
  - Posts payments received during the day
  - Updates loan statuses (e.g., delinquency, paid, outstanding)
  - Applies contractual changes and recalculates balances
  - Prepares data for daily reporting and reconciliation
- **Technical Implementation:**
  - The DAILY program is central to this process.
  - It uses numerous copybooks for loan, payment, and date handling (e.g., [[FILE:LP01DS_SQL.CPY|LP01DS_SQL.CPY]], [[FILE:DAILY_WKS.CPY|DAILY_WKS.CPY]], [[FILE:LPCEPPW.CPY|LPCEPPW.CPY]]).
  - Contains logic to skip processing on the last day of the month to avoid conflicts with monthly cycles.
  - Entry paragraphs include FORM-RESET, SETUP-LINK-INFO, STOP-RUN.
  - Example code:
    ```cobol
    PERFORM ARRAYSP-LOAD.
    MOVE TODAY-IS-DATE TO NUM-DATE.
    PERFORM FIND-FISCAL-PERIOD.
    IF SUB NOT = 0
       IF TODAY-IS-DATE = GP-END-FISCAL-DATE(SUB)
          GO TO END-PROGRAM.
    PERFORM CLEAR-BYFILE.
    PERFORM UPDATE-BY-DS-STATS.
    GO TO END-PROGRAM.
    ```
    (Source: [[FILE:DAILY.CBL|DAILY.CBL]], Lines 276-300)

---

**2. Monthly Batch Processing**
- **Purpose:** Performs end-of-month accruals, earnings calculations, and updates for all loans and accounts.
- **Key Activities:**
  - Calculates monthly interest accruals and earnings
  - Updates branch and group-level financial statistics
  - Generates monthly reports for management and compliance
  - May include special handling for non-accrual loans and reporting fields
- **Technical Implementation:**
  - Programs such as AC1ERN, RZRERN, and others handle monthly accrual and reporting.
  - Logic includes looping through branches/groups and updating reporting fields.
  - Example comment:
    ```
    * UPDATE-BY-LAST WHEN EACH BRANCH'S PROCESSING IS COMPLETE,
    * SUPPORT PL# 672
    * ... CALL RAC323 WHICH LOOPS THRU EACH BRANCH IN THE GROUP RANGE,
    * AND UPDATES THE LRFILE & AEFILE NON-ACCRUAL INTERNAL REPORTING FIELDS
    ```
    (Source: [[FILE:RZRERN.CBL|RZRERN.CBL]], Lines 101-125)

---

**3. Year-End (EOY) Batch Processing**
- **Purpose:** Performs fiscal year-end updates, statistics roll-up, and prepares data for regulatory and financial reporting.
- **Key Activities:**
  - Runs fiscal year-end routines to update statistics and close out the year
  - Moves EOY flags and resets for the new fiscal year
  - Generates year-end reports and extracts required for compliance
- **Technical Implementation:**
  - UPDEOY program manages year-end logic, including running EOY routines and moving flags.
  - EOCRON allows batch execution via cron/shell for automated scheduling.
  - Example comment:
    ```
    * 2) RUN END-OF-MONTH
    * 3) RUN FISCAL END-OF-YEAR (TO DO JUST STATS)
    * 4) MOVE THE EOY FLAG BACK SO THEY WILL PASS OPEN DAY CHECK (MV EOY02F EOY02B)
    ```
    (Source: [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 26-50)
  - EOCRON is used for batch automation:
    ```
    * DESC:  EXECUTE BATCH FROM CRON
    * NOTE: IF IT RUNS IN BATCH, IT CAN BE RUN THROUGH EOCRON.
    *       EOCRON DOES THE SAME STUFF AS EOEXEC, BUT INSTEAD OF
    *       SCREEN INPUT, IT GETS THE STEPS & DATE OFFSETS FROM A SHELL.
    ```
    (Source: [[FILE:EOCRON.CBL|EOCRON.CBL]], Lines 1-25)

---

**Summary Table: Batch Cycles in OTIS**

| Cycle    | Main Program(s) | Key Activities                                 | Scheduling/Automation      |
|----------|-----------------|------------------------------------------------|---------------------------|
| Daily    | DAILY           | Payment posting, status updates, stats         | Scheduled daily           |
| Monthly  | AC1ERN, RZRERN  | Accruals, earnings, reporting                  | End-of-month              |
| Year-End | UPDEOY, EOCRON  | Fiscal close, stats roll-up, compliance        | Fiscal year-end, cron     |

---

**Here's a visual representation:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Batch Processing Cycles</text>
  
  <!-- Daily -->
  <rect x="60" y="80" width="220" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="10"/>
  <text x="170" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Daily Cycle</text>
  <text x="170" y="130" text-anchor="middle" font-size="13" fill="#6366f1">DAILY</text>
  <text x="170" y="150" text-anchor="middle" font-size="12" fill="#6366f1">Payments, Status Updates</text>
  
  <!-- Monthly -->
  <rect x="340" y="80" width="220" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="10"/>
  <text x="450" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Monthly Cycle</text>
  <text x="450" y="130" text-anchor="middle" font-size="13" fill="#1e40af">AC1ERN, RZRERN</text>
  <text x="450" y="150" text-anchor="middle" font-size="12" fill="#3b82f6">Accruals, Earnings, Reports</text>
  
  <!-- Year-End -->
  <rect x="620" y="80" width="220" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="10"/>
  <text x="730" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Year-End Cycle</text>
  <text x="730" y="130" text-anchor="middle" font-size="13" fill="#15803d">UPDEOY, EOCRON</text>
  <text x="730" y="150" text-anchor="middle" font-size="12" fill="#22c55e">Fiscal Close, Compliance</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="280" y1="120" x2="340" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="560" y1="120" x2="620" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Source Citations:**
- DAILY batch logic and skip-on-month-end: (Source: [[FILE:DAILY.CBL|DAILY.CBL]], Lines 276-300)
- DAILY program description: (Source: [[FILE:DAILY.CBL|DAILY.CBL]], Lines 1-25)
- Monthly accrual/update logic: (Source: [[FILE:RZRERN.CBL|RZRERN.CBL]], Lines 101-125)
- Year-end batch steps and flag handling: (Source: [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 26-50)
- EOCRON batch automation: (Source: [[FILE:EOCRON.CBL|EOCRON.CBL]], Lines 1-25)

If you need details on the specific programs or copybooks involved in each cycle, let me know!

### How does OTIS manage customer statement generation and delivery?

OTIS manages customer statement generation and delivery through a coordinated process involving specialized COBOL programs, configurable form options, and integration with external delivery systems. The workflow ensures statements are produced in the correct format for each customer and delivered via the appropriate channel.

---

**BUSINESS PURPOSE:**
- Generate periodic statements summarizing loan activity, balances, and payments for each customer.
- Support multiple output formats (standard, laser, PCL, SmartPort) to meet customer and regulatory requirements.
- Deliver statements via print, electronic, or third-party systems (e.g., SmartPort for BF Motor Credit).

---

**TECHNICAL IMPLEMENTATION:**

1. **Statement Generation Programs:**
   - The main program for statement generation is **STATEMENT**.
     - STATEMENT is called by other programs to initiate statement creation (Source: [[FILE:STATEMEN|STATEMEN]]T, Index: programs).
     - It does not call other programs directly, indicating it is a terminal process in the call chain.

2. **Form Type Configuration:**
   - The form type for each statement is controlled by the **FD-OPTION** field, defined in the [[FILE:LP01FD.CPY|LP01FD.CPY]] copybook.
   - Supported options:
     - `"N"` = Standard Form (LONP or PRT)
     - `"P"` = PCL/Laser Form (PARADATA)
     - `"T"` = PST/Laser Form (PARADATA)
     - `"Y"` = SmartPort (external package for BF Motor Credit)
     - `"C"` = Customer Statement (CUSTMT)
     - `"E"` = Extraction (Corporate)
   - The FD-OPTION is validated against allowed values (Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], Index: code).

3. **SmartPort Integration:**
   - For customers using SmartPort, a trigger is placed on line 1 of the document.
   - SmartPort then handles field mapping and delivery (Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], Index: code).
   - This allows OTIS to outsource delivery and formatting for specific clients.

4. **Statement Content Assembly:**
   - Statement programs (e.g., STATEMENT, PST608, PST609, PST610) assemble statement data by reading loan records, payment history, and account balances.
   - Data fields such as balances, interest, charges, and payment details are moved into statement layouts (see code examples in [[FILE:LNEXTR.CBL|LNEXTR.CBL]], [[FILE:PLPURG.CBL|PLPURG.CBL]], etc.).

5. **Delivery Mechanisms:**
   - Statements are printed or electronically delivered based on the FD-OPTION.
   - For print, statements are formatted according to the selected form type and sent to the appropriate printer or output queue.
   - For electronic delivery, integration points (e.g., SmartPort) handle transmission to customers.

---

**PROCESS FLOW SUMMARY:**

1. **Trigger:** Statement generation is initiated (scheduled batch or on-demand).
2. **Form Selection:** FD-OPTION determines the output format and delivery channel.
3. **Data Assembly:** Statement programs gather and format customer data.
4. **Output:** Statements are printed, sent electronically, or handed off to SmartPort.
5. **Delivery:** Customers receive statements via mail, electronic systems, or third-party providers.

---

**CODE EXAMPLES & SOURCES:**

- **Form Type Configuration:**
  ```cobol
  *    "C" = CUSTOMER STATEMENT {CUSTMT}
  *    "E" = EXTRACTION (CORPORATE)
  *    "N" = STANDARD  FORM {LONP OR PRT}
  *    "P" = PCL/LASER FORM {PCL} (PARADATA)
  *    "T" = PST/LASER FORM {PST} (PARADATA)
  *    "Y" = SMARTPORT
  ```
  (Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], Index: code)

- **SmartPort Integration:**
  ```cobol
  * SMARTPORT IS AN OUTSIDE PACKAGE USED BY BF MOTOR CREDIT THAT
  * NEEDS A TRIGGER ON LINE 1 OF OUR DOCUMENTS, THEN THEY DO ALL
  * THE MOVING OF FIELDS TO THE CORRECT POSITIONS.
  ```
  (Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], Index: code)

- **Main Statement Program:**
  - STATEMENT (no outgoing calls; called by 2 programs)
    (Source: [[FILE:STATEMEN|STATEMEN]]T, Index: programs)

---

**VISUAL REPRESENTATION:**

Here's a visual representation of the statement generation and delivery flow:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Customer Statement Generation & Delivery</text>
  
  <!-- Data Assembly -->
  <rect x="50" y="80" width="200" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Data Assembly</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">Loan, Payment, Account Data</text>
  
  <!-- Statement Program -->
  <rect x="300" y="80" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="400" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">STATEMENT Program</text>
  <text x="400" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Assembles Statement</text>
  
  <!-- Form Type Selection -->
  <rect x="550" y="80" width="200" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="650" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Form Type Selection</text>
  <text x="650" y="130" text-anchor="middle" font-size="12" fill="#22c55e">FD-OPTION (N, P, T, Y, C, E)</text>
  
  <!-- Delivery -->
  <rect x="300" y="220" width="200" height="80" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="400" y="250" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">Delivery</text>
  <text x="400" y="270" text-anchor="middle" font-size="12" fill="#f59e0b">Print, Electronic, SmartPort</text>
  
  <!-- External System -->
  <rect x="550" y="220" width="200" height="80" fill="#fca5a5" stroke="#ef4444" stroke-width="3" rx="8"/>
  <text x="650" y="250" text-anchor="middle" font-size="16" font-weight="bold" fill="#991b1b">SmartPort</text>
  <text x="650" y="270" text-anchor="middle" font-size="12" fill="#ef4444">External Delivery</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="250" y1="120" x2="300" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="500" y1="120" x2="550" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="400" y1="160" x2="400" y2="220" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="650" y1="160" x2="650" y2="220" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="500" y1="120" x2="400" y2="220" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**SUMMARY:**
- OTIS generates customer statements using configurable form types and integrates with external systems for delivery.
- The process is flexible, supporting print, electronic, and third-party delivery (SmartPort).
- Key configuration and business logic are managed via FD-OPTION and specialized COBOL programs.

**Sources:**
- [[FILE:LP01FD.CPY|LP01FD.CPY]], Index: code (form type options, SmartPort integration)
- STATEMENT, Index: programs (main statement program)
- [[FILE:LNEXTR.CBL|LNEXTR.CBL]], [[FILE:PLPURG.CBL|PLPURG.CBL]], Index: code (statement data assembly)

### What are the key month-end and year-end closing processes in OTIS?

OTIS (Online Transaction Information System) manages month-end and year-end closing through a series of coordinated COBOL programs and business rules designed to ensure financial integrity, compliance, and accurate reporting for loan servicing and related operations.

---

**KEY MONTH-END CLOSING PROCESSES**

**Business Purpose:**
- Finalize all transactions for the month
- Update balances, clear temporary fields, and prepare for the next month
- Generate regulatory and management reports
- Ensure all required monthly activities (e.g., payment posting, adjustments) are complete before closing

**Technical Implementation:**
- **End-of-Month (EOM) Programs:**  
  - **ENDMTH**: Handles G/L end-of-month processing, including purging old transactions and updating tag files for documentation (Source: [[FILE:ENDMTH.CBL|ENDMTH.CBL]], Index: code).
  - **EOMALL**: Creates new month records, clears monthly-to-date (MTD) figures, and resets P&L collected fields (Source: [[FILE:EOMALL.CBL|EOMALL.CBL]], Lines 1326-1350).
  - **Validation Logic:**  
    - Programs check that all required monthly processes are complete before allowing EOM to run.
    - Example: "TEST PRIOR EOM COMPLETE & CALENDAR EOY COMPLETE & DON'T ALLOW OPENING DAY IN NEW MONTH UNLESS DONE" (Source: [[FILE:LONPA0.CBL|LONPA0.CBL]], Lines 26-50).
  - **Reporting:**  
    - Generates month-end reports for management and compliance.

---

**KEY YEAR-END CLOSING PROCESSES**

**Business Purpose:**
- Finalize annual financial records
- Perform regulatory/statistical reporting
- Reset annual accumulators and prepare for the new fiscal year
- Prevent errors from running year-end before month-end or in incorrect combinations

**Technical Implementation:**
- **End-of-Year (EOY) Programs:**  
  - **UPDEOY**: Main EOY clearing and update program.  
    - Verifies that EOM has been completed before EOY can run.
    - Handles EOY flags, error logging, and group logic for branches (Source: [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 1-75).
    - Attempts to "fool-proof" the EOY procedure to avoid common mistakes (e.g., running EOY before EOM, running incorrect year-end types) (Source: [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 51-75).
  - **ENDYER**: Manages year-end screens and user prompts for EOY processing (Source: [[FILE:ENDYER.CBL|ENDYER.CBL]], Lines 326-350).
  - **EOYUPD**: Updates year-end records, uses SQL copybooks for reporting and data updates (Source: [[FILE:EOYUPD.CBL|EOYUPD.CBL]], Index: copybook_usage).
  - **GLTAGS**: Logs GL/EOY activity and replaces tag files for documentation (Source: [[FILE:GLTAGS.CBL|GLTAGS.CBL]], Lines 1-25).
- **Validation Logic:**  
  - Programs enforce that EOM must be completed before EOY is allowed.
  - Error messages are logged instead of aborting if a branch fails, improving robustness (Source: [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 26-50).
- **Reporting:**  
  - Generates year-end reports and statistics for regulatory compliance.
  - Updates fiscal tags and records for the new year.

---

**COMMON BUSINESS RULES & SAFEGUARDS**

- **Sequence Enforcement:**  
  - EOY cannot run unless EOM is complete for all branches.
  - Flags and tags are checked to prevent accidental or out-of-sequence processing.
- **Error Handling:**  
  - If a branch fails EOY, an error message is logged rather than aborting the entire process (Source: [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 26-50).
- **Data Integrity:**  
  - Programs move and reset accumulators, balances, and statistical fields to ensure accurate roll-forward.

---

**VISUAL REPRESENTATION:**

Here's a visual representation of the key closing processes and their relationships:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Month-End & Year-End Closing Architecture</text>
  
  <!-- Month-End Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Month-End Closing</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Programs: ENDMTH, EOMALL, LONPA0</text>
  <text x="450" y="135" text-anchor="middle" font-size="12" fill="#6366f1">Validates completion, clears MTD, generates reports</text>
  
  <!-- Year-End Layer -->
  <rect x="50" y="200" width="800" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="230" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Year-End Closing</text>
  <text x="450" y="250" text-anchor="middle" font-size="14" fill="#1e40af">Programs: UPDEOY, ENDYER, EOYUPD, GLTAGS</text>
  <text x="450" y="265" text-anchor="middle" font-size="12" fill="#3b82f6">Validates EOM, resets annual fields, logs activity</text>
  
  <!-- Data Layer -->
  <rect x="50" y="330" width="800" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="360" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data & Reporting Layer</text>
  <text x="450" y="380" text-anchor="middle" font-size="14" fill="#15803d">Files: GL records, loan files, statistical accumulators</text>
  <text x="450" y="395" text-anchor="middle" font-size="12" fill="#22c55e">Persistent storage, compliance reports</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="280" x2="450" y2="330" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**SOURCE CITATIONS:**
- [[FILE:ENDMTH.CBL|ENDMTH.CBL]], Index: code (GL end-of-month logic)
- [[FILE:EOMALL.CBL|EOMALL.CBL]], Lines 1326-1350 (month record creation and clearing)
- [[FILE:LONPA0.CBL|LONPA0.CBL]], Lines 26-50 (EOM/EOY completion checks)
- [[FILE:UPDEOY.CBL|UPDEOY.CBL]], Lines 1-75 (EOY update logic, error handling, business rules)
- [[FILE:ENDYER.CBL|ENDYER.CBL]], Lines 326-350 (EOY user interface and prompts)
- [[FILE:EOYUPD.CBL|EOYUPD.CBL]], Index: copybook_usage (year-end updates and reporting)
- [[FILE:GLTAGS.CBL|GLTAGS.CBL]], Lines 1-25 (GL/EOY activity logging)

---

**Summary:**  
OTIS enforces strict sequencing and validation for month-end and year-end closing, using specialized programs to update records, clear accumulators, and generate reports. Safeguards are in place to prevent common errors, and the process is designed for robustness and compliance.

### How does OTIS support audit trails and compliance reporting requirements?

OTIS (Online Transaction Information System) supports audit trails and compliance reporting through a combination of dedicated COBOL programs, structured file management, and integration with regulatory reporting processes. Here’s how the system achieves these goals:

---

**BUSINESS PURPOSE**

- **Audit Trails:**  
  OTIS maintains detailed records of all financial transactions, user actions, and system events. This ensures traceability for internal controls, regulatory audits, and operational reviews.
- **Compliance Reporting:**  
  The system generates periodic reports (daily, monthly, year-end) required by regulators and internal policy, including transaction logs, aging reports, and exception summaries.

---

**TECHNICAL IMPLEMENTATION**

1. **Dedicated Audit Programs**
   - Programs such as **AUDITW**, **AUDEXT**, **AUDEXT-MB**, and **MBDEXT** are responsible for writing, updating, and extracting audit records.
   - These programs process transaction logs, update audit files, and prepare data for compliance reports.
     - Example:  
       - **AUDITW** manages audit file updates and extracts, with entry paragraphs like `UPDATE-AUDIT-PEFILE`, `SET-SNFILE`, and `UPDATE-AUDIT-PEFILE` (Source: [[FILE:AUDITW|AUDITW]], [[FILE:I|I]]ndex: flows).
       - **AUDEXT-MB** and **MBDEXT** handle extraction and categorization of audit data for reporting (Source: [[FILE:AUDEXT-M|AUDEXT-M]]B, MBDEXT, Index: flows).

2. **Structured File Management**
   - Audit programs use specialized copybooks to define audit file layouts and working storage:
     - **[[FILE:GBWSGR.CPY|GBWSGR.CPY]]** – Audit working storage definitions
     - **[[FILE:GB01GR_SQL.CPY|GB01GR_SQL.CPY]]** – SQL audit record structures
     - **[[FILE:GBFSWKI.CPY|GBFSWKI.CPY]]** – File switch/key information for audit files
     - **[[FILE:GBWKIN.CPY|GBWKIN.CPY]]** – Audit file input/output definitions
     - **[[FILE:FERRORS.CPY|FERRORS.CPY]]** – Error handling for audit file operations
     (Source: [[FILE:AUDEXT-MB.CBL|AUDEXT-MB.CBL]], [[FILE:MBDEXT.CBL|MBDEXT.CBL]], Index: copybook_usage)

3. **Batch Cycle Integration**
   - Audit trail updates and compliance reporting are integrated into daily, month-end, and year-end batch cycles.
   - Programs like **EORUN** and **EOEXEC** coordinate batch processing, ensuring audit files are updated and reports are generated as part of closing routines (Source: [[FILE:EORUN|EORUN]], [[FILE:EOEXEC|EOEXEC]], [[FILE:I|I]]ndex: flows).

4. **Regulatory and Internal Reporting**
   - Programs such as **MBAGED**, **RZCORP**, **RZWRI1**, and **RZMTRL** generate aging reports, corporate compliance summaries, and write audit data to output files for review.
   - These reports support both regulatory filings and internal management oversight (Source: [[FILE:MBAGED|MBAGED]], [[FILE:RZCORP|RZCORP]], [[FILE:RZWRI1|RZWRI1]], [[FILE:RZMTRL|RZMTRL]], [[FILE:I|I]]ndex: flows).

5. **Error Handling and Data Integrity**
   - Copybooks like **[[FILE:FERRORS.CPY|FERRORS.CPY]]** and **[[FILE:ACCESSW.CPY|ACCESSW.CPY]]** provide error logging and access control, ensuring audit records are complete and tamper-resistant (Source: [[FILE:AUDEXT-MB.CBL|AUDEXT-MB.CBL]], [[FILE:AUDITW.CBL|AUDITW.CBL]], Index: copybook_usage).

---

**EXAMPLE PROGRAM FLOW (AUDITW):**

- Entry points such as `MAIN-PROGRAM`, `UPDATE-AUDIT-PEFILE`, and `SET-SNFILE` manage the creation and update of audit records.
- No outgoing calls detected, indicating a focused role in audit file management (Source: [[FILE:AUDITW|AUDITW]], [[FILE:I|I]]ndex: flows).

---

**DATA FLOW DIAGRAM**

Here's a visual representation of how audit and compliance reporting is supported in OTIS:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Audit Trail & Compliance Reporting</text>
  
  <!-- Transaction Programs -->
  <rect x="50" y="80" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Transaction Programs</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Loan Servicing, Payments</text>
  
  <!-- Audit Programs -->
  <rect x="350" y="80" width="200" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Audit Programs</text>
  <text x="450" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">AUDITW, AUDEXT, MBDEXT</text>
  
  <!-- Compliance Reporting -->
  <rect x="650" y="80" width="200" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Compliance Reporting</text>
  <text x="750" y="130" text-anchor="middle" font-size="12" fill="#22c55e">MBAGED, RZCORP, RZMTRL</text>
  
  <!-- Audit Files -->
  <rect x="350" y="250" width="200" height="80" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="450" y="280" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">Audit Files</text>
  <text x="450" y="300" text-anchor="middle" font-size="12" fill="#f59e0b">Structured Records</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="250" y1="120" x2="350" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="550" y1="120" x2="650" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="160" x2="450" y2="250" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="750" y1="160" x2="450" y2="250" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**SUMMARY**

- OTIS uses specialized audit programs and copybooks to record, manage, and report all critical system activities.
- Audit trails are updated during batch cycles and are accessible for compliance reporting.
- Structured file management and error handling ensure data integrity and regulatory compliance.
- Compliance reports are generated by dedicated programs and support both internal and external requirements.

**Sources:**
- AUDITW, AUDEXT, AUDEXT-MB, MBDEXT, MBAGED, RZCORP, RZMTRL (Index: flows, copybook_usage)
- EORUN, EOEXEC (Index: flows)
- Copybooks: [[FILE:GBWSGR.CPY|GBWSGR.CPY]], [[FILE:GB01GR_SQL.CPY|GB01GR_SQL.CPY]], [[FILE:GBFSWKI.CPY|GBFSWKI.CPY]], [[FILE:GBWKIN.CPY|GBWKIN.CPY]], [[FILE:FERRORS.CPY|FERRORS.CPY]], [[FILE:ACCESSW.CPY|ACCESSW.CPY]] (Index: copybook_usage)

If you need details on a specific audit or compliance report, or want to see the structure of an audit file, let me know!

### What exception handling and research processes exist in OTIS?

OTIS (Online Transaction Information System) implements robust exception handling and research processes to ensure data integrity, compliance, and operational efficiency. These processes are distributed across multiple COBOL programs, each specializing in different types of exceptions and research workflows.

---

**EXCEPTION HANDLING IN OTIS**

**1. Exception Detection and Logging**
- Programs like **LONPW9**, **SETIMP**, **WDAYBK**, and **BULKG2** contain logic to detect business and processing exceptions.
- Common exception scenarios include:
  - Invalid posting codes or amounts
  - Deceased or fraud account status
  - Credit bureau status mismatches
  - Repo and payoff anomalies
  - Missing or invalid data fields

**Example:**
```cobol
IF (POSTING-ERRCD NOT = "O") AND (HOLD-OVPAID-CHECK = 0)
   ADD ORIG-BP-TRAMT TO AMOUNT-EXCEPTION(1)
   ADD 1 TO TOTAL-EXCEPTION(1)
   PERFORM EXCEPTION-LINE-PROCESS.
   MOVE "E" TO BP-UPDFG.
   PERFORM REWRITE-BP1-FILE.
```
(Source: [[FILE:LONPW9.CBL|LONPW9.CBL]], Lines 2801-2825)

**2. Exception Reporting and Totals**
- Exceptions are accumulated in arrays (e.g., `AMOUNT-EXCEPTION`, `TOTAL-EXCEPTION`) and printed in summary sections.
- Programs generate detailed and summary exception reports for audit and operational review.

**Example:**
```cobol
IF TOTAL-EXCEPTION(LEV) NOT = 0
   IF LEV = 1
      MOVE "TOTAL EXCEPTIONS BR# &&&&" TO S-BRNAME
      INSPECT S-BRNAME REPLACING FIRST "&&&&" BY BR-NO
```
(Source: [[FILE:LONPW9.CBL|LONPW9.CBL]], Lines 3101-3125)

**3. Specialized Exception Types**
- Repo exceptions (repossession-related) are handled with dedicated logic:
  - `REPO-AMOUNT-EXCEPTION`, `REPO-TOTAL-EXCEPTION` arrays
  - Calls to `REPO-EXCEPTION-LN-PROCESS` for repo-specific reporting
- Other programs (e.g., **SETIMP**) handle exceptions for deceased, fraud, and credit bureau statuses:
```cobol
IF DECEASED NOT = "Y"
   IF LN-ACTIONCD = "DCSD"
      MOVE "ACCOUNT IN DECEASED STATUS" TO D-EXCEPTION
      PERFORM EXCEPT-LINE
      GO TO NEXT-RECORD.
```
(Source: [[FILE:SETIMP.CBL|SETIMP.CBL]], Lines 376-400)

---

**RESEARCH PROCESSES IN OTIS**

**1. Exception Research and Investigation**
- When exceptions are detected, programs often move exception details to research files or buffers for further investigation.
- Research processes include:
  - Reading exception records from files (e.g., `EXCEPT-FILE`)
  - Printing exception details for manual review
  - Providing navigation for users to research screens

**Example:**
```cobol
PERFORM START-EXCEPT-FILE.
NEXT-EXCEPTION.
   PERFORM READ-EXCEPT-FILE-NEXT.
   IF IO-BAD
      GO TO EXIT-PROCESS-EXCEPTION-TYPE.
   IF EXCEPT-TYPE NOT = EXCEPTION-FG
      GO TO EXIT-PROCESS-EXCEPTION-TYPE.
   MOVE EXCEPT-GT1-KEY TO GT1-KEY.
   PERFORM READ-GT1-FILE.
   IF IO-BAD
      GO TO IO-ERROR.
   PERFORM EXCEPTION-PRINT.
```
(Source: [[FILE:WDAYBK.CBL|WDAYBK.CBL]], Lines 676-700)

**2. Exception Research Screens and User Interaction**
- Programs like **GRLST2** provide user interfaces for researching exceptions, allowing users to view, print, and investigate exception records.
- Users can choose to print exceptions, accept them, or return for further research.

**Example:**
```cobol
REC-EXCEPTIONS.
   MOVE "E  A000EXCP." TO VDU.
   PERFORM SCREENIO.
   IF VDUSTATUS = "1>" GO TO END-PROGRAM.
   IF VDUSTATUS NOT = "00" GO TO REC-EXCEPTIONS.
   MOVE VDUBUF TO PRINT-EXCEPTIONS.
   IF PRINT-EXCEPTIONS = "N"
      GO TO ACCEPT-IT.
   IF PRINT-EXCEPTIONS NOT = "Y"
      GO TO REC-EXCEPTIONS.
```
(Source: [[FILE:GRLST2.CBL|GRLST2.CBL]], Lines 526-550)

**3. Exception File Management**
- Exception records are written to and read from dedicated files for tracking and research.
- Programs perform file operations such as OPEN, READ, WRITE, and REWRITE on exception files.

---

**Summary of Key Programs and Processes:**

| Program   | Exception Handling Role                                 | Research Role                        |
|-----------|--------------------------------------------------------|--------------------------------------|
| LONPW9    | Loan payment/posting exceptions, repo exceptions        | Exception reporting, summary print   |
| SETIMP    | Deceased/fraud/credit bureau status exceptions          | Exception line processing            |
| WDAYBK    | General transaction exceptions, file-based research     | Exception file navigation/printing   |
| GRLST2    | User interface for exception research                   | Print/accept/research exceptions     |
| BULKG2    | Bulk transfer/repo exceptions                          | Exception routine for bulk records   |

---

**Here's a visual representation:**
<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Exception Handling & Research Architecture</text>
  
  <!-- Exception Detection Layer -->
  <rect x="40" y="70" width="820" height="70" fill="#fee2e2" stroke="#ef4444" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#b91c1c">Exception Detection</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#b91c1c">Programs: LONPW9, SETIMP, BULKG2, WDAYBK</text>
  
  <!-- Exception Logging/Reporting Layer -->
  <rect x="40" y="160" width="820" height="70" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="450" y="185" text-anchor="middle" font-size="18" font-weight="bold" fill="#b45309">Exception Logging & Reporting</text>
  <text x="450" y="210" text-anchor="middle" font-size="14" fill="#b45309">Arrays, Files, Summary Reports</text>
  
  <!-- Exception Research Layer -->
  <rect x="40" y="250" width="820" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="275" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Exception Research</text>
  <text x="450" y="300" text-anchor="middle" font-size="14" fill="#1e40af">Programs: GRLST2, WDAYBK, User Screens</text>
  
  <!-- Data Layer -->
  <rect x="40" y="340" width="820" height="50" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="365" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Exception Files & Research Buffers</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="140" x2="450" y2="160" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="230" x2="450" y2="250" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="320" x2="450" y2="340" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Source Citations:**
- [[FILE:LONPW9.CBL|LONPW9.CBL]], Lines 2801-2850, 3101-3150, 1151-1175 (exception logic, reporting)
- [[FILE:SETIMP.CBL|SETIMP.CBL]], Lines 376-425 (deceased/fraud/credit bureau exceptions)
- [[FILE:WDAYBK.CBL|WDAYBK.CBL]], Lines 676-700 (exception file research)
- [[FILE:GRLST2.CBL|GRLST2.CBL]], Lines 526-550 (exception research screen)
- [[FILE:BULKG2.CBL|BULKG2.CBL]], Lines 1026-1050 (bulk repo exception handling)

If you need details on a specific exception type or research workflow, let me know!

### How does OTIS manage document imaging and correspondence tracking?

OTIS manages document imaging and correspondence tracking through a coordinated set of COBOL programs, structured file handling, and business logic designed to ensure every document and correspondence is properly recorded, retrievable, and auditable.

**BUSINESS PURPOSE:**
- Document imaging: Scans, stores, and indexes loan-related documents (e.g., disclosures, letters, cover pages) for compliance and operational efficiency.
- Correspondence tracking: Records all outgoing/incoming communications (letters, notices, legal documents) linked to borrower accounts, enabling audit trails and research.

**TECHNICAL IMPLEMENTATION:**

1. **Document Generation & Imaging:**
   - Programs like **PST847**, **PST50K**, **PST334**, and **PST002** generate specific document types (disclosures, cover pages, personal property forms, collection letters).
     - Example: PST847 creates disclosure documents for Mississippi loans (Source: [[FILE:PST847.CBL|PST847.CBL]], Index: code).
     - Example: PST50K generates OCR cover pages for all states (Source: [[FILE:PST50K.CBL|PST50K.CBL]], Index: code).
   - These programs format documents, assign form numbers, and prepare them for imaging.

2. **Document Manifest & Indexing:**
   - Programs such as **LONPM0** and **APIDOC** build a "manifest"—a structured record of all documents generated for a loan or borrower.
     - Key fields include loan date, maker/comaker info, opt-out flags, and document type (Source: [[FILE:LONPM0.CBL|LONPM0.CBL]], Lines 676-700; [[FILE:APIDOC.CBL|APIDOC.CBL]], Lines 376-400).
     - The manifest is written to a file for later retrieval and audit.

3. **Imaging Workflow:**
   - After document generation, routines like **WRITE-MANIFEST-FILE** and **CLOSE-MANIFEST-FILE** ensure the document and its metadata are stored and indexed (Source: [[FILE:APIDOC.CBL|APIDOC.CBL]], Lines 376-400).
   - Imaging modules may interface with external systems or directories (e.g., **CHANGE-DIR-MODULE** in APIDOC) to store scanned images in the correct location (Source: [[FILE:APIDOC|APIDOC]], [[FILE:I|I]]ndex: flows).

4. **Correspondence Tracking:**
   - Each correspondence (letter, notice) is logged with borrower identifiers, document type, and date.
   - Programs like **PSTPMC** and **PSTPMCW** handle specialized correspondence (e.g., laser form letters, linked workers), ensuring each item is tracked and retrievable (Source: [[FILE:PSTPMC|PSTPMC]], [[FILE:I|I]]ndex: flows; [[FILE:PSTPMCW.CPY|PSTPMCW.CPY]], Lines 1-25).

5. **Audit & Research Support:**
   - The manifest and correspondence logs provide a searchable audit trail.
   - Exception handling routines ensure missing or invalid documents are flagged for research (Source: [[FILE:LONPM0.CBL|LONPM0.CBL]], Lines 701-725).

6. **Integration with Other Modules:**
   - Document imaging and correspondence tracking are tightly integrated with loan processing, compliance, and reporting modules, ensuring all generated documents are linked to the correct business events.

**EXAMPLE WORKFLOW:**
1. Loan event triggers document generation (e.g., new loan, payment, legal notice).
2. Document program (e.g., PST847) creates the document and updates the manifest.
3. Imaging module stores the document image and metadata.
4. Correspondence tracking module logs the event for audit/research.
5. Exception handling flags any issues for follow-up.

---

**Here's a visual representation:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Document Imaging & Correspondence Tracking</text>
  
  <!-- Document Generation Programs -->
  <rect x="50" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Document Gen</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="12">PST847, PST50K, PST334, PST002</text>
  
  <!-- Manifest Creation -->
  <rect x="270" y="80" width="180" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Manifest Creation</text>
  <text x="360" y="130" text-anchor="middle" fill="white" font-size="12">LONPM0, APIDOC</text>
  
  <!-- Imaging Workflow -->
  <rect x="490" y="80" width="180" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Imaging Workflow</text>
  <text x="580" y="130" text-anchor="middle" fill="white" font-size="12">WRITE-MANIFEST-FILE</text>
  
  <!-- Correspondence Tracking -->
  <rect x="710" y="80" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="800" y="110" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Correspondence</text>
  <text x="800" y="130" text-anchor="middle" fill="#15803d" font-size="12">PSTPMC, PSTPMCW</text>
  
  <!-- Audit Trail -->
  <rect x="270" y="200" width="180" height="70" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="360" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Audit Trail</text>
  <text x="360" y="250" text-anchor="middle" fill="white" font-size="12">Manifest, Logs</text>
  
  <!-- Exception Handling -->
  <rect x="490" y="200" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="8"/>
  <text x="580" y="230" text-anchor="middle" fill="#4338ca" font-size="16" font-weight="bold">Exception Handling</text>
  <text x="580" y="250" text-anchor="middle" fill="#4338ca" font-size="12">Flag Issues</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="115" x2="270" y2="115" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="115" x2="490" y2="115" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="670" y1="115" x2="710" y2="115" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="150" x2="360" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="150" x2="580" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**SOURCE REFERENCES:**
- [[FILE:PST847.CBL|PST847.CBL]], [[FILE:PST50K.CBL|PST50K.CBL]], [[FILE:PST334.CBL|PST334.CBL]], [[FILE:PST002.CBL|PST002.CBL]] – Document generation programs
- [[FILE:LONPM0.CBL|LONPM0.CBL]] (Lines 676-700, 701-725), [[FILE:APIDOC.CBL|APIDOC.CBL]] (Lines 376-400) – Manifest creation and imaging logic
- APIDOC, Index: flows – Imaging workflow and directory management
- PSTPMC, [[FILE:PSTPMCW.CPY|PSTPMCW.CPY]] – Correspondence tracking modules

**Summary:**  
OTIS ensures all documents and correspondence are systematically generated, imaged, indexed, and tracked, supporting compliance, audit, and operational research needs.

### What workflow processes exist for loan boarding and setup in OTIS?

OTIS (Online Transaction Information System) implements a structured, multi-step workflow for loan boarding and setup. This process ensures that new loans are accurately entered, validated, and integrated into the system, supporting both operational efficiency and regulatory compliance.

**Workflow Processes for Loan Boarding and Setup in OTIS:**

### 1. **Loan Data Entry**
- **User Action:** Loan officers or branch staff initiate the loan boarding process via dedicated screens (e.g., "LOAN MASTER FILE", "LOAN PAYMENT FILE").
- **Screen Prompts:** Users are guided through entry fields such as loan number, borrower details, payment terms, collateral codes, and proceeds.
  - Example screen elements:
    - "LN/IN/DATE", "PMT DT/TRM", "PURPOSE CD", "COLLTRL CD", "SOURCE CD", "PROCEEDS/I", "INS/POINTS", "FEES"  
    (Source: [[FILE:S|S]]creen 1, Index: screen_nodes)

### 2. **Validation and Compliance Checks**
- **System Logic:** Upon entry, the system performs validation checks:
  - Ensures required fields are completed
  - Verifies codes (e.g., collateral, purpose) against master tables
  - Checks for regulatory compliance (e.g., loan limits, fee structures)
- **Error Handling:** Invalid entries prompt error messages and require correction before proceeding.

### 3. **Loan Record Creation**
- **File Management:** Once validated, the system creates records in the "LOAN MASTER FILE" and related files (e.g., "BORROWER MASTER FILE", "LOAN PAYMENT FILE").
- **Automated Processes:** The system may auto-calculate amortization schedules, payment frequencies, and assign loan identifiers.

### 4. **Document Imaging and Correspondence Tracking**
- **Integration:** The workflow includes steps for scanning and attaching supporting documents (e.g., signed contracts, collateral documentation).
- **Correspondence:** The system tracks outgoing correspondence (e.g., approval letters, disclosures) and logs them for audit purposes.
  - (See previous answer: "OTIS manages document imaging and correspondence tracking through coordinated COBOL programs and file handling.")

### 5. **Audit Trail and History Logging**
- **Compliance:** Every action (entry, modification, approval) is logged in audit files, supporting regulatory requirements and future research.
- **History Files:** The "LOAN HISTORY FILE" records all changes and transactions related to the loan.

### 6. **Final Review and Approval**
- **Workflow Routing:** Loans may require supervisor or automated approval before activation.
- **Status Update:** Upon approval, the loan status is updated, and the loan becomes active in the system.

### 7. **Exception Handling**
- **Error/Research:** If issues arise (e.g., missing documents, failed validation), the workflow routes the loan to exception handling modules for research and resolution.
  - (See previous answer: "OTIS implements robust exception handling and research processes...")

---

**Narrative Workflow Example:**

1. **User accesses the "Loan Master File" screen**
2. **Enters loan details (loan number, borrower, terms, collateral, etc.)**
3. **System validates entries and checks compliance**
4. **If valid, creates loan record and related payment schedule**
5. **Scans and attaches supporting documents**
6. **Logs all actions for audit trail**
7. **Routes for approval if required**
8. **Activates loan upon approval**
9. **Handles exceptions and research if errors occur**

---

**Here's a visual representation:**

<svg width="900" height="650" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Loan Boarding & Setup Workflow</text>
  
  <!-- Step 1: Data Entry -->
  <rect x="60" y="80" width="200" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="160" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Loan Data Entry</text>
  <text x="160" y="130" text-anchor="middle" font-size="12" fill="#6366f1">User enters loan info</text>
  
  <!-- Step 2: Validation -->
  <rect x="320" y="80" width="200" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="420" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Validation & Compliance</text>
  <text x="420" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">System checks fields</text>
  
  <!-- Step 3: Record Creation -->
  <rect x="580" y="80" width="200" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="680" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Loan Record Creation</text>
  <text x="680" y="130" text-anchor="middle" font-size="12" fill="#22c55e">Files updated</text>
  
  <!-- Step 4: Imaging & Correspondence -->
  <rect x="60" y="200" width="200" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="160" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Document Imaging</text>
  <text x="160" y="250" text-anchor="middle" font-size="12" fill="#6366f1">Attach documents</text>
  
  <rect x="320" y="200" width="200" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="420" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Correspondence Tracking</text>
  <text x="420" y="250" text-anchor="middle" font-size="12" fill="#6366f1">Log letters/emails</text>
  
  <!-- Step 5: Audit Trail -->
  <rect x="580" y="200" width="200" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="680" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Audit Trail Logging</text>
  <text x="680" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">History files updated</text>
  
  <!-- Step 6: Approval -->
  <rect x="190" y="320" width="200" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="290" y="350" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Final Review & Approval</text>
  <text x="290" y="370" text-anchor="middle" font-size="12" fill="#22c55e">Supervisor/system</text>
  
  <!-- Step 7: Exception Handling -->
  <rect x="470" y="320" width="200" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="3" rx="8"/>
  <text x="570" y="350" text-anchor="middle" font-size="16" font-weight="bold" fill="white">Exception Handling</text>
  <text x="570" y="370" text-anchor="middle" font-size="12" fill="white">Research & resolution</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="260" y1="110" x2="320" y2="110" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="520" y1="110" x2="580" y2="110" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="160" y1="140" x2="160" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="420" y1="140" x2="420" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="680" y1="140" x2="680" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="160" y1="260" x2="290" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="420" y1="260" x2="290" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="680" y1="260" x2="570" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="290" y1="380" x2="570" y2="380" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Source Citations:**
- Screen prompts and workflow steps: Screen 1, Index: screen_nodes
- Exception handling and research: Previous answer, OTIS exception handling processes
- Document imaging and correspondence: Previous answer, OTIS document imaging workflow

**Summary:**  
OTIS's loan boarding and setup workflow is a comprehensive, multi-step process involving user data entry, system validation, record creation, document imaging, audit trail logging, approval, and exception handling. This ensures loans are accurately established and fully compliant with business and regulatory requirements.

### How does OTIS handle payment reversals, adjustments, and corrections?

OTIS (Online Transaction Information System) handles payment reversals, adjustments, and corrections through a coordinated set of COBOL programs and business logic designed to ensure financial accuracy, auditability, and compliance. Here’s how these processes work:

---

**BUSINESS PURPOSE**

- **Payment Reversals:** Allow users to undo previously posted payments due to errors, NSF (non-sufficient funds), or other business reasons.
- **Adjustments:** Enable corrections to payment amounts, allocation between principal/interest, or reclassification of transactions.
- **Corrections:** Address misapplied payments, incorrect transaction codes, or other posting errors.

These processes ensure that loan accounts reflect the true financial status and that all changes are tracked for audit and regulatory purposes.

---

**TECHNICAL IMPLEMENTATION**

### 1. **Payment Reversals**

- **Trigger:** Typically initiated by user action (e.g., F6 from PAYDATE screen, as noted in [[FILE:LONIF1.CBL|LONIF1.CBL]]).
- **Main Routine:** The reversal logic is handled in dedicated paragraphs such as `REVERSAL-ROUTINE` (Source: [[FILE:LONIF1.CBL|LONIF1.CBL]], Lines 1851-1875).
- **Process Steps:**
  - Identify the original transaction using account number, sequence number, and transaction code.
  - Open relevant files (e.g., LP1-FILE) and retrieve transaction details.
  - Reverse the effects by adjusting principal, interest, fees, and updating the cash drawer as needed.
  - Special handling for batch payments, lock box transactions, and charge-off reversals (Source: [[FILE:LONPF8.CBL|LONPF8.CBL]], Lines 201-250).
  - Update audit logs and maintain reversal history for compliance.

**Example:**
```cobol
* PAYMENT REVERSAL ROUTINE
REVERSAL-ROUTINE.
    MOVE LP-TRCD TO HOLD-TRCD.
    PERFORM OPEN-LP1-FILE.
    MOVE LP-PATH-OWNBR TO LP-BRNO ...
    MOVE LN-ACCTNO TO LP-ACCTNO ...
    [Further logic to locate and reverse the payment]
```
(Source: [[FILE:LONIF1.CBL|LONIF1.CBL]], Lines 1851-1875)

### 2. **Adjustments**

- **Routine:** Adjustments are performed via dedicated adjustment programs (e.g., LPEADJ, LPDLADJ).
- **Process Steps:**
  - Compute new allocation for principal, interest, late charges, or fees.
  - Update loan records and recalculate balances.
  - Adjust GL (general ledger) entries as needed.
  - Ensure all changes are logged and auditable.

**Example:**
- LPEADJ handles earned adjustment routines for interest and fees (Source: [[FILE:LPEADJ|LPEADJ]], [[FILE:I|I]]ndex: flows).
- LPDLADJ computes GL fee adjustments (Source: [[FILE:LPDLADJ|LPDLADJ]], [[FILE:I|I]]ndex: flows).

### 3. **Corrections**

- **Routine:** Corrections are typically handled by programs that allow modification of transaction codes, amounts, or other attributes (e.g., LONPF2, LONPF8).
- **Process Steps:**
  - Locate the erroneous transaction.
  - Apply corrections (e.g., change transaction code, fix amount).
  - Update all affected files and ensure downstream effects (e.g., reporting, notifications) are handled.
  - Maintain audit trail for all corrections.

**Example:**
- LONPF8 contains extensive business logic for handling reversals, including special cases for NSF, interbranch payments, and lock box transactions (Source: [[FILE:LONPF8.CBL|LONPF8.CBL]], Lines 101-150, 201-250, 301-325).

---

**KEY BUSINESS RULES & COMMENTS**

- **Special Cases:** Certain transaction types (e.g., "ES" for escrow) cannot be reversed automatically and require manual intervention (Source: [[FILE:LONPF8.CBL|LONPF8.CBL]], Lines 301-325).
- **Auditability:** All reversals and adjustments are logged for compliance (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows).
- **Regulatory Handling:** Logic exists to ensure reversals comply with federal and state regulations, especially for charge-offs and lock box payments.

---

**PROGRAMS INVOLVED**

| Program   | Purpose/Role                                         |
|-----------|------------------------------------------------------|
| LONIF1    | Main payment reversal routine                        |
| LONPF8    | Handles complex reversal logic, adjustments, corrections |
| LPEADJ    | Earned adjustment routines (interest/fees)           |
| LPDLADJ   | Dealer adjustment routines (GL fees)                 |
| APIPAY    | Payment posting, triggers reversal logic             |
| LONPF2    | Payment update and correction logic                  |

---

**DATA FLOW**

- **Input:** User selects transaction to reverse/adjust/correct.
- **Processing:** Programs retrieve transaction, apply reversal/adjustment/correction logic, update loan/account files.
- **Output:** Updated loan records, audit logs, notifications.

---

**EXCEPTION HANDLING**

- **Error Conditions:** If reversal is not allowed (e.g., escrow payments), error messages are displayed and manual intervention is required.
- **Audit Trail:** All changes are tracked for compliance and reporting.

---

**VISUAL REPRESENTATION:**

Here's a visual representation of the payment reversal, adjustment, and correction workflow in OTIS:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Payment Reversal & Adjustment Workflow</text>
  
  <!-- User Action -->
  <rect x="50" y="80" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">User Initiates</text>
  <text x="140" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Reversal/Adjustment</text>
  
  <!-- Main Reversal Routine -->
  <rect x="270" y="80" width="180" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="360" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">LONIF1</text>
  <text x="360" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">REVERSAL-ROUTINE</text>
  
  <!-- Adjustment Programs -->
  <rect x="490" y="80" width="180" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="580" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">LPEADJ / LPDLADJ</text>
  <text x="580" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">Adjustments</text>
  
  <!-- Correction Logic -->
  <rect x="710" y="80" width="140" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="780" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">LONPF8</text>
  <text x="780" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">Corrections</text>
  
  <!-- Data Layer -->
  <rect x="270" y="200" width="580" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="560" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Loan/Account Files</text>
  <text x="560" y="250" text-anchor="middle" font-size="12" fill="#22c55e">Updates, Audit Logs</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="670" y1="110" x2="710" y2="110" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="360" y1="140" x2="360" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="580" y1="140" x2="580" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="780" y1="140" x2="780" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**SOURCE REFERENCES:**
- Payment reversal logic: [[FILE:LONIF1.CBL|LONIF1.CBL]], Lines 1851-1875
- Adjustment routines: LPEADJ, LPDLADJ (Source: flows)
- Correction logic and business rules: [[FILE:LONPF8.CBL|LONPF8.CBL]], Lines 101-150, 201-250, 301-325
- Audit and compliance: APIPAY, Index: flows

---

**Summary:**  
OTIS manages payment reversals, adjustments, and corrections through specialized COBOL programs and routines that ensure all changes are accurate, auditable, and compliant with business and regulatory requirements. The workflow involves user initiation, programmatic processing, and comprehensive audit logging.

### What are the business rules engines or decision logic frameworks in OTIS?

OTIS (Online Transaction Information System) uses a combination of embedded business rules engines and decision logic frameworks within its COBOL programs and copybooks. These are not standalone "engines" in the modern sense, but rather structured sets of decision logic, validation routines, and rule tables implemented throughout the application to enforce business policies, regulatory requirements, and operational constraints.

### Key Components of Business Rules and Decision Logic in OTIS

#### 1. **88-Level Condition Names (COBOL Business Rules)**
- OTIS programs and copybooks frequently use COBOL's 88-level condition names to define business rule values and valid states.
- Example from [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]]:
  ```cobol
  88  SPA-DISFRMLA-VALID          VALUE "A" "B" "C".
  88  SPA-DISFRMLA-ACTUARIAL      VALUE "A".
  88  SPA-DISFRMLA-ACTION-DATA    VALUE "B".
  88  SPA-DISFRMLA-USRULE         VALUE "C".
  ```
  - These define valid formula types for loan calculations, enforcing business logic at the data level.
  - (Source: [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]], Index: code)

#### 2. **Rule Tables and Lookup Copybooks**
- Many copybooks act as rule tables, mapping codes to business meanings and controlling allowed operations.
- Example: [[FILE:RESCINDW.CPY|RESCINDW.CPY]]
  - Contains logic for rescission calculations, including business day limits and exception handling.
  - (Source: [[FILE:RESCINDW.CPY|RESCINDW.CPY]], Index: code)

#### 3. **Decision Paragraphs and Validation Routines**
- Programs implement decision logic in dedicated paragraphs, often named VALIDATE, ENTRY-MODULE, or similar.
- Example from [[FILE:SPMAN1.CBL|SPMAN1.CBL]]:
  ```cobol
  IF NOT SP-CONTRFRMLA-VALID
     MOVE "ENTER: SPACE OR 'A'" TO MESS
     PERFORM SEND-MESS
     GO TO ENTER-ELE.
  IF SP-CAL-RATETYPE(1) = "Z" AND SP-CONTRFRMLA = "A"
     STRING "TOTAL MONEY FORMULA NOT ALLOWED WITH "
            "RATETYPE 'Z' INT METHOD"
            DELIMITED BY SIZE INTO MES
  ```
  - This enforces allowed combinations of formula and rate type, preventing invalid user input.
  - (Source: [[FILE:SPMAN1.CBL|SPMAN1.CBL]], Index: code)

#### 4. **Conditional Logic for Regulatory and Policy Enforcement**
- Business rules are embedded to comply with regulations (e.g., loan term limits, payment rules).
- Example from [[FILE:LNVERI.CBL|LNVERI.CBL]]:
  - Comments and logic enforce maximum allowable loan terms and expiration dates, with error handling for violations.
  - (Source: [[FILE:LNVERI.CBL|LNVERI.CBL]], Index: code)

#### 5. **Rule-Based Calculation Routines**
- Specialized paragraphs perform calculations based on rule codes.
- Example from [[FILE:REBATE.CPY|REBATE.CPY]]:
  ```cobol
  IF SP-RBFRMLA2(REB-SUB) = "3"
     PERFORM RULE-78THS-MEAN
  ELSE IF SP-RBFRMLA2(REB-SUB) = "4"
     PERFORM TN-PP-SHORT-RATE
  ELSE IF SP-RBFRMLA2(REB-SUB) = "6"
     PERFORM REBATE-ACTUARIAL-F-06
  ```
  - Each code triggers a different calculation method, reflecting business rules for rebates.
  - (Source: [[FILE:REBATE.CPY|REBATE.CPY]], Index: code)

#### 6. **Declarative and Initialization Sections**
- Many programs use DECLARATIVES and INITIALIZATION paragraphs to set up rule contexts and enforce startup checks.
- Example: LPEOD5, MBALR6H, SEBATD, SRBAC2
  - These sections often initialize rule tables, set flags, and prepare the environment for rule enforcement.
  - (Sources: LPEOD5, MBALR6H, SEBATD, SRBAC2, Index: flows)

#### 7. **Copybook-Based Rule Sharing**
- Copybooks like [[FILE:GB01BR.CPY|GB01BR.CPY]], [[FILE:LP01LN_SQL.CPY|LP01LN_SQL.CPY]], and others encapsulate rule definitions and are included across multiple programs to ensure consistent rule enforcement.

### Summary Table: Example Rule Frameworks in OTIS

| Component/Copybook         | Purpose/Business Rule Type                                 | Source Reference                |
|----------------------------|-----------------------------------------------------------|---------------------------------|
| [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]]           | Loan formula validation (actuarial, action, US rule)      | [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]], code          |
| [[FILE:RESCINDW.CPY|RESCINDW.CPY]]               | Rescission calculation, business day limits               | [[FILE:RESCINDW.CPY|RESCINDW.CPY]], code              |
| [[FILE:SPMAN1.CBL|SPMAN1.CBL]]                 | Formula/rate type validation, user input enforcement      | [[FILE:SPMAN1.CBL|SPMAN1.CBL]], code                |
| [[FILE:LNVERI.CBL|LNVERI.CBL]]                 | Loan term/expiration validation, regulatory compliance    | [[FILE:LNVERI.CBL|LNVERI.CBL]], code                |
| [[FILE:REBATE.CPY|REBATE.CPY]]                 | Rebate calculation method selection by rule code          | [[FILE:REBATE.CPY|REBATE.CPY]], code                |
| [[FILE:GB01BR.CPY|GB01BR.CPY]]                 | General business rule definitions for branches            | [[FILE:GB01BR.CPY|GB01BR.CPY]], copybook_usage      |
| [[FILE:LP01LN_SQL.CPY|LP01LN_SQL.CPY]]             | Loan rule definitions, SQL-based validation               | [[FILE:LP01LN_SQL.CPY|LP01LN_SQL.CPY]], copybook_usage  |

### Business Rules Engine Characteristics in OTIS

- **Distributed:** Rules are embedded throughout programs and copybooks, not centralized.
- **Declarative:** Uses COBOL condition names (88-levels) and rule tables for clarity.
- **Procedural:** Decision logic is implemented in paragraphs and PERFORM routines.
- **Extensible:** New rules can be added via copybooks and paragraph logic.
- **Consistent:** Shared copybooks ensure rules are applied uniformly across modules.

---

### Here's a visual representation of how business rules are distributed and enforced in OTIS:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Business Rules Architecture</text>
  
  <!-- Rule Copybooks -->
  <rect x="50" y="80" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Rule Copybooks</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#6366f1">[[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]], [[FILE:RESCINDW.CPY|RESCINDW.CPY]], [[FILE:GB01BR.CPY|GB01BR.CPY]]</text>
  
  <!-- Decision Paragraphs -->
  <rect x="350" y="80" width="200" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Decision Paragraphs</text>
  <text x="450" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">VALIDATE, ENTRY-MODULE, CALCULATE-REBATE</text>
  
  <!-- Rule Tables & 88-Levels -->
  <rect x="650" y="80" width="200" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Rule Tables & 88-Levels</text>
  <text x="750" y="130" text-anchor="middle" font-size="12" fill="#22c55e">SPA-DISFRMLA-VALID, SP-CONTRFRMLA-VALID</text>
  
  <!-- Programs -->
  <rect x="200" y="220" width="500" height="80" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="450" y="250" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">COBOL Programs</text>
  <text x="450" y="270" text-anchor="middle" font-size="12" fill="#f59e0b">[[FILE:SPMAN1.CBL|SPMAN1.CBL]], [[FILE:LNVERI.CBL|LNVERI.CBL]], [[FILE:REBATE.CPY|REBATE.CPY]], LPEOD5</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="150" y1="160" x2="350" y2="220" stroke="#667eea" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="160" x2="450" y2="220" stroke="#3b82f6" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="750" y1="160" x2="700" y2="220" stroke="#22c55e" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**In summary:**  
OTIS enforces business rules through a distributed framework of copybooks, rule tables, 88-level condition names, and decision logic paragraphs embedded in COBOL programs. This approach provides flexibility, consistency, and extensibility for complex financial and regulatory requirements.

**Sources:**  
- [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]], Index: code  
- [[FILE:RESCINDW.CPY|RESCINDW.CPY]], Index: code  
- [[FILE:SPMAN1.CBL|SPMAN1.CBL]], Index: code  
- [[FILE:LNVERI.CBL|LNVERI.CBL]], Index: code  
- [[FILE:REBATE.CPY|REBATE.CPY]], Index: code  
- LPEOD5, MBALR6H, SEBATD, SRBAC2, Index: flows  
- [[FILE:GB01BR.CPY|GB01BR.CPY]], [[FILE:LP01LN_SQL.CPY|LP01LN_SQL.CPY]], Index: copybook_usage

### How does OTIS support disaster recovery and business continuity processes?

OTIS (Online Transaction Information System) supports disaster recovery and business continuity through a combination of architectural design, programmatic controls, and operational procedures embedded in its COBOL application ecosystem. Here’s how these processes are implemented:

**BUSINESS PURPOSE:**
- Ensure that critical loan servicing operations can continue or be rapidly restored after system failures, data loss, or other disruptions.
- Protect financial data integrity and maintain regulatory compliance.
- Minimize downtime for essential functions such as payment processing, loan setup, and reporting.

**TECHNICAL IMPLEMENTATION:**

1. **Data Redundancy and Backup:**
   - OTIS programs frequently use file-level controls and batch processing routines to create backup copies of key data files (e.g., loan master, payment history, transaction logs).
   - Programs such as BRCOPY and SECOPY include logic for copying, resetting, and auditing files, which supports periodic backup and restoration.
     - Example: BRCOPY flow includes "COPY-IT", "COPY-END", and "SEND-LEGEND" steps for duplicating branch data (Source: [[FILE:BRCOPY|BRCOPY]], [[FILE:I|I]]ndex: flows).
   - End-of-day and end-of-period reporting programs (e.g., DBREPT, OPREPT) generate summary files that can be used for recovery checkpoints.

2. **Audit Trails and Logging:**
   - Many OTIS programs maintain detailed audit logs of transactions and changes, enabling rollback and forensic analysis after incidents.
   - Programs like BRSECURE and DISBOV use copybooks such as [[FILE:BRSECUREW.CPY|BRSECUREW.CPY]] to track security events and changes (Source: [[FILE:DISBOV.CBL|DISBOV.CBL]], Index: copybook_usage).
   - Logging routines are invoked at key points in workflows (e.g., "LOG-CHANGES" in BYRSUP, Source: [[FILE:BYRSUP|BYRSUP]], [[FILE:I|I]]ndex: flows).

3. **File Integrity and Validation:**
   - Declarative sections in programs (e.g., "DECLARATIVES" in CPRUN, SRBAC9, PST608) handle file open/close operations and error recovery, ensuring files are not left in inconsistent states (Source: [[FILE:CPRUN|CPRUN]], [[FILE:I|I]]ndex: flows).
   - Programs perform validation checks before and after file operations, reducing risk of corruption.

4. **Failover and Recovery Routines:**
   - Specialized programs (e.g., SUPPORT, AND) are designed to be called during recovery scenarios to reinitialize or verify system state (Source: [[FILE:SUPPORT|SUPPORT]], [[FILE:I|I]]ndex: programs).
   - Batch routines can be rerun to reconstruct data from backup files or audit logs.

5. **Separation of Duties and Modular Design:**
   - OTIS uses modular COBOL programs with clear separation between business logic, data access, and presentation layers.
   - This design allows isolated recovery of affected modules without impacting the entire system.

6. **Security and Access Controls:**
   - Security programs (e.g., BRSECURE, PASSWD) enforce password verification and access restrictions, protecting recovery processes from unauthorized changes (Source: [[FILE:PASSWD|PASSWD]], [[FILE:I|I]]ndex: flows).

7. **Operational Procedures:**
   - Regularly scheduled jobs (often run overnight or at period-end) perform backups, generate audit reports, and verify file integrity.
   - Recovery documentation and scripts are maintained to guide operators through restoration steps.

**VISUAL ARCHITECTURE DIAGRAM:**

Here's a visual representation of how disaster recovery and business continuity are supported in OTIS:

<svg width="700" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Disaster Recovery Architecture</text>
  
  <!-- Presentation Layer -->
  <rect x="50" y="70" width="600" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="350" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="350" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Screens: SCRN1, PGMENU, GTFORM</text>
  
  <!-- Business Logic Layer -->
  <rect x="50" y="180" width="600" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="350" y="205" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="350" y="230" text-anchor="middle" font-size="14" fill="#1e40af">Programs: BRCOPY, SECOPY, BRSECURE, SUPPORT</text>
  <text x="350" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">Backup, Audit, Validation, Recovery</text>
  
  <!-- Data Layer -->
  <rect x="50" y="310" width="600" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="350" y="335" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="350" y="360" text-anchor="middle" font-size="14" fill="#15803d">Files: Loan Master, Payment History, Audit Logs</text>
  <text x="350" y="380" text-anchor="middle" font-size="12" fill="#22c55e">Backup & Recovery Storage</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="350" y1="150" x2="350" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="280" x2="350" y2="310" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**SUMMARY:**
- OTIS supports disaster recovery and business continuity through backup routines, audit trails, file validation, modular recovery programs, and operational procedures.
- Key programs: BRCOPY (file backup/copy), SECOPY (security backup), BRSECURE (audit/security), SUPPORT (recovery routines), PASSWD (access control).
- Data integrity is maintained via regular backups, audit logs, and validation checks.
- Modular design allows targeted recovery of affected subsystems.

**Sources:**
- BRCOPY flow and backup logic (Source: [[FILE:BRCOPY|BRCOPY]], [[FILE:I|I]]ndex: flows)
- BRSECURE audit routines (Source: [[FILE:BRSECURE|BRSECURE]], [[FILE:I|I]]ndex: flows)
- SUPPORT recovery program (Source: [[FILE:SUPPORT|SUPPORT]], [[FILE:I|I]]ndex: programs)
- PASSWD security enforcement (Source: [[FILE:PASSWD|PASSWD]], [[FILE:I|I]]ndex: flows)
- DISBOV audit copybook usage (Source: [[FILE:DISBOV.CBL|DISBOV.CBL]], Index: copybook_usage)
- BYRSUP logging routines (Source: [[FILE:BYRSUP|BYRSUP]], [[FILE:I|I]]ndex: flows)

If you need details on specific recovery procedures or file-level backup logic, please specify the program or process for deeper analysis.

### What data migration and conversion processes has OTIS undergone historically?

OTIS (Online Transaction Information System) has undergone extensive data migration and conversion processes over its history, reflecting both business needs and technical evolution. Here’s a summary of the major patterns and specific processes, based on retrieved code comments, copybook histories, and program documentation:

---

### 1. **Bulk Account Conversion and Purchase**
- **Purpose:** Migrate large sets of accounts from legacy systems or acquired portfolios into OTIS.
- **Implementation:**  
  - Programs like **LONI**, **LONIE0**, and **EONIE0** include logic to distinguish between bulk purchases and account conversions.
  - Field `BKCVFG` is used to determine whether a record is part of a bulk purchase or a conversion (Source: [[FILE:LONI.CBL|LONI.CBL]], lines 26-50).
  - Special routines ensure that statistics and general ledger (GL) entries are preserved during conversion, preventing loss of financial data if the day is closed prematurely (Source: [[FILE:LONI.CBL|LONI.CBL]], lines 26-50).

---

### 2. **Conversion Register and Snapshot Reporting**
- **Purpose:** Provide an auditable snapshot of account states at the moment of conversion for compliance and reconciliation.
- **Implementation:**  
  - The **CONVRP** program generates a "snapshot" report showing how accounts looked when converted (Source: [[FILE:CONVRP.CBL|CONVRP.CBL]], lines 51-75).
  - Comments indicate the importance of matching loan entry dates and transaction dates, and recommend using the conversion register (EMTKRG) for accuracy if conversions span multiple months.

---

### 3. **Field and Format Upgrades**
- **Purpose:** Adapt data formats to new business requirements or regulatory standards.
- **Implementation:**  
  - Numerous copybooks and programs document upgrades such as:
    - Changing account number formats (e.g., from 8-digit to 6+2 digit split for A15 output, Source: [[FILE:LTEXTR.CBL|LTEXTR.CBL]], lines 126-150).
    - Adding new fields for compliance projects (e.g., CSI/CMSI project fields in [[FILE:LA01MA.CPY|LA01MA.CPY]], lines 26-50).
    - Removing obsolete fields and files as business needs change (e.g., removal of LN-REVOLVING, PT0153 in [[FILE:BULKT2.CBL|BULKT2.CBL]], lines 326-350).

---

### 4. **File and Record Cleansing**
- **Purpose:** Ensure migrated data meets current business rules and is free of legacy inconsistencies.
- **Implementation:**  
  - Programs like **UP1534** include logic to skip records prior to the year 2000 and cleanse specific fields (Source: [[FILE:UP1534.CBL|UP1534.CBL]], lines 101-125).
  - Removal of hard-coded logic and duplicate keys during migration (Source: [[FILE:UP1534.CBL|UP1534.CBL]], lines 126-150).

---

### 5. **Machine and Directory Migration**
- **Purpose:** Move data between different hardware platforms or directory structures.
- **Implementation:**  
  - Routines to move data paths and machine identifiers, ensuring correct file placement and access after migration (Source: [[FILE:DSMAIN.CBL|DSMAIN.CBL]], lines 2326-2350).
  - Use of utility programs (e.g., C$TOLOWER) to standardize machine names during conversion (Source: [[FILE:UP15VY.CBL|UP15VY.CBL]], lines 701-725).

---

### 6. **Screen and Menu Conversion**
- **Purpose:** Update user interfaces to support new conversion routines and workflows.
- **Implementation:**  
  - The **[[FILE:LPMNUI.CPY|LPMNUI.CPY]]** copybook documents the evolution of conversion menus, including support for running conversion programs from multiple terminals (Source: [[FILE:LPMNUI.CPY|LPMNUI.CPY]], lines 1-25).

---

### 7. **Error Handling and Audit Trails**
- **Purpose:** Ensure integrity and traceability during migration.
- **Implementation:**  
  - Standardized error messages for records that have already been created, undergone change, or cannot be deleted due to active status (Source: [[FILE:EONIE0.CBL|EONIE0.CBL]], lines 601-625; [[FILE:SPMANH.CBL|SPMANH.CBL]], lines 1251-1275; [[FILE:RBMAIN.CBL|RBMAIN.CBL]], lines 701-725).
  - Audit trails and status flags are maintained to track conversion progress and issues.

---

## **Historical Patterns and Business Drivers**

- **Acquisition Integration:** Bulk conversions often follow acquisitions, requiring mapping of external data formats to OTIS standards.
- **Regulatory Compliance:** Field upgrades and cleansing routines reflect changing regulatory requirements (e.g., CSI/CMSI projects).
- **Platform Modernization:** Migration between machines and directories supports hardware upgrades and disaster recovery planning.
- **Continuous Improvement:** Removal of obsolete logic and files, addition of new fields, and menu enhancements show ongoing adaptation to business needs.

---

## **Key Programs and Copybooks Involved**

| Program/Copybook      | Purpose/Role                                                                 |
|-----------------------|-------------------------------------------------------------------------------|
| LONI, LONIE0, EONIE0  | Bulk purchase/account conversion logic, GL/statistics preservation            |
| CONVRP                | Conversion snapshot reporting, audit trail                                    |
| UP1534                | Cleansing, record skipping, field upgrades                                   |
| BULKT2                | Bulk file/tag updates, removal of obsolete fields                            |
| DSMAIN                | Data path/machine migration routines                                         |
| [[FILE:LPMNUI.CPY|LPMNUI.CPY]]            | Conversion menu evolution                                                    |
| [[FILE:LA01MA.CPY|LA01MA.CPY]]            | Field upgrades for compliance projects                                       |
| [[FILE:LTEXTR.CBL|LTEXTR.CBL]]            | Account number format conversion                                             |

---

## **Summary**

OTIS’s data migration and conversion processes have evolved to support:
- Bulk account integration
- Accurate snapshot/audit reporting
- Field and format upgrades
- Data cleansing and error handling
- Machine and directory migration
- User interface adaptation

These processes are embedded in both business logic and technical routines, ensuring that OTIS remains robust, compliant, and adaptable to changing business and regulatory environments.

---

**Sources:**
- [[FILE:LONI.CBL|LONI.CBL]], lines 26-50
- [[FILE:CONVRP.CBL|CONVRP.CBL]], lines 51-75
- [[FILE:LTEXTR.CBL|LTEXTR.CBL]], lines 126-150
- [[FILE:BULKT2.CBL|BULKT2.CBL]], lines 326-350
- [[FILE:UP1534.CBL|UP1534.CBL]], lines 101-125, 126-150
- [[FILE:DSMAIN.CBL|DSMAIN.CBL]], lines 2326-2350
- [[FILE:LPMNUI.CPY|LPMNUI.CPY]], lines 1-25
- [[FILE:LA01MA.CPY|LA01MA.CPY]], lines 26-50
- [[FILE:EONIE0.CBL|EONIE0.CBL]], lines 601-625
- [[FILE:SPMANH.CBL|SPMANH.CBL]], lines 1251-1275
- [[FILE:RBMAIN.CBL|RBMAIN.CBL]], lines 701-725

If you need a detailed flowchart or want to see specific migration routines in diagram form, let me know!

### How does OTIS manage rate changes and payment recalculations?

OTIS manages rate changes and payment recalculations through a combination of specialized COBOL programs, copybooks, and embedded business logic. The system is designed to handle complex financial scenarios such as interest rate adjustments, payment schedule recalculations, and regulatory compliance for loans and related products.

### BUSINESS PURPOSE

- **Rate Changes:** OTIS supports dynamic updates to loan interest rates, including scheduled changes, regulatory caps, and special formulas (e.g., promotional rates, balloon payments).
- **Payment Recalculations:** When rates change, OTIS recalculates payment amounts, schedules, and related financial fields to ensure accurate billing and compliance.
- **Regulatory Compliance:** The system enforces business rules such as maximum allowable rates, rounding conventions, and state-specific calculations.

### TECHNICAL IMPLEMENTATION

#### 1. **Specialized Programs for Rate and Payment Logic**
- **LPRATE:** Handles rate calculations, comparisons, and updates. It uses formulas to determine new rates and applies caps or special rules as needed.
  - Entry paragraphs: `LPRATE-CALC-IT`, `LPRATE-COMPARE-RATES`, `LPRATE-USE-RATES` (Source: [[FILE:LPRATE|LPRATE]], [[FILE:I|I]]ndex: flows)
- **CALCZL / CALCZ3:** Perform actuarial and amortization calculations for loans, including recalculating payments when rates or terms change.
  - Handles rounding, convergence issues, and maximum term logic (Source: [[FILE:CALCZL|CALCZL]], [[FILE:I|I]]ndex: flows; CALCZ3, Index: variables)
- **LPPOFF / FAPOFF:** Manage payoff calculations, including recalculating interest, penalties, and rebates when rates change or loans are paid off early.
  - Includes logic for state-specific rules (e.g., California recast) (Source: [[FILE:LPPOFF.CPY|LPPOFF.CPY]], [[FILE:FAPOFF.CPY|FAPOFF.CPY]], Index: code)

#### 2. **Copybooks for Rate and Payment Structures**
- **LIBLP/[[FILE:LPRATE.CPY|LPRATE.CPY]]:** Defines rate tables, formulas, and fields used in rate calculations (Source: [[FILE:MBNOFL|MBNOFL]], [[FILE:I|I]]ndex: copybook_usage)
- **LIBLP/[[FILE:LPAMTS.CPY|LPAMTS.CPY]]:** Contains payment amount structures and related fields (Source: [[FILE:MBCORP|MBCORP]], [[FILE:MBCORP1|MBCORP1]], [[FILE:I|I]]ndex: copybook_usage)
- **LIBLP/[[FILE:LP01LP.CPY|LP01LP.CPY]], LIBLP/[[FILE:LP01LN.CPY|LP01LN.CPY]]:** Store loan and payment record layouts, including fields for rates, terms, and payment schedules (Source: [[FILE:LPMAN3|LPMAN3]], [[FILE:LPMAN2|LPMAN2]], [[FILE:REBT01|REBT01]], [[FILE:I|I]]ndex: copybook_usage)

#### 3. **Embedded Business Rules and Decision Logic**
- **Conditional Logic:** Programs use IF/EVALUATE statements to apply business rules, such as:
  - Capping rates at regulatory maximums
  - Applying special formulas for balloons or promotional loans
  - Rounding payments according to business policy
  - Example: 
    ```cobol
    IF LN-OVRDRATE NOT = 999.999
       MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
       PERFORM SET-PY-RATE
    ```
    (Source: [[FILE:LPPOFF.CPY|LPPOFF.CPY]], Index: code)
- **Rate Table Indexing:** Logic to select appropriate rate from tables based on loan attributes (Source: [[FILE:LPAPRS.CPY|LPAPRS.CPY]], Index: code)
- **Recalculation Triggers:** Payment recalculation is triggered by events such as rate changes, loan modifications, or payoff requests.

#### 4. **Workflow Example**
When a rate change occurs:
1. **Trigger:** User or batch process initiates a rate change for a loan.
2. **Rate Calculation:** LPRATE or CALCZL recalculates the new rate using formulas and tables.
3. **Payment Recalculation:** CALCZL or CALCZ3 computes new payment amounts and schedules.
4. **Update Records:** LPPOFF or FAPOFF updates loan records, payoff amounts, and schedules.
5. **Compliance Checks:** Business rules ensure rates and payments comply with regulations.
6. **Notification:** System may generate notices or update screens to inform users/customers.

#### 5. **Historical Context**
- The system has evolved to handle increasingly complex rate and payment scenarios, with revisions noted for regulatory changes, bug fixes, and new product features (Source: [[FILE:LPAPRS.CPY|LPAPRS.CPY]], Index: code).

---

### Visual Representation

Here's a visual representation of the rate change and payment recalculation workflow in OTIS:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Rate Change & Payment Recalculation Workflow</text>
  
  <!-- Step 1: Trigger -->
  <rect x="50" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Rate Change Trigger</text>
  <text x="140" y="128" text-anchor="middle" fill="white" font-size="11">User/Bulk Update</text>
  
  <!-- Step 2: Rate Calculation -->
  <rect x="270" y="80" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Rate Calculation</text>
  <text x="360" y="128" text-anchor="middle" fill="white" font-size="11">LPRATE / CALCZL</text>
  
  <!-- Step 3: Payment Recalculation -->
  <rect x="490" y="80" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Payment Recalculation</text>
  <text x="580" y="128" text-anchor="middle" fill="white" font-size="11">CALCZL / CALCZ3</text>
  
  <!-- Step 4: Update Records -->
  <rect x="710" y="80" width="140" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="780" y="110" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Update Records</text>
  <text x="780" y="128" text-anchor="middle" fill="white" font-size="11">LPPOFF / FAPOFF</text>
  
  <!-- Step 5: Compliance Checks -->
  <rect x="270" y="180" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="360" y="210" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Compliance Checks</text>
  <text x="360" y="228" text-anchor="middle" fill="white" font-size="11">Business Rules</text>
  
  <!-- Step 6: Notification -->
  <rect x="490" y="180" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="580" y="210" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Notification</text>
  <text x="580" y="228" text-anchor="middle" fill="white" font-size="11">User/Customer</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="670" y1="110" x2="710" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="140" x2="580" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="140" x2="360" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  
  <!-- Feedback arrows -->
  <line x1="780" y1="140" x2="780" y2="180" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

### KEY SOURCES & EXAMPLES

- **LPRATE:** Rate calculation and update logic (Source: [[FILE:LPRATE|LPRATE]], [[FILE:I|I]]ndex: flows)
- **CALCZL:** Payment recalculation, rounding, and convergence logic (Source: [[FILE:CALCZL|CALCZL]], [[FILE:I|I]]ndex: flows; [[FILE:CALCZL.CBL|CALCZL.CBL]], Index: code)
- **LPPOFF / FAPOFF:** Payoff and penalty recalculation, including state-specific rules (Source: [[FILE:LPPOFF.CPY|LPPOFF.CPY]], [[FILE:FAPOFF.CPY|FAPOFF.CPY]], Index: code)
- **[[FILE:LPAPRS.CPY|LPAPRS.CPY]]:** Historical revisions for rate table indexing and rounding (Source: [[FILE:LPAPRS.CPY|LPAPRS.CPY]], Index: code)
- **Copybooks:** LIBLP/[[FILE:LPRATE.CPY|LPRATE.CPY]], LIBLP/[[FILE:LPAMTS.CPY|LPAMTS.CPY]], LIBLP/[[FILE:LP01LP.CPY|LP01LP.CPY]] (Source: [[FILE:MBNOFL|MBNOFL]], [[FILE:MBCORP|MBCORP]], [[FILE:LPMAN3|LPMAN3]], [[FILE:I|I]]ndex: copybook_usage)

---

**Summary:**  
OTIS manages rate changes and payment recalculations by combining specialized programs, structured copybooks, and embedded business rules. The workflow ensures accurate, compliant, and timely updates to loan records and payment schedules whenever rates change, supporting both business needs and regulatory requirements.

### What quality control and reconciliation processes are built into OTIS?

OTIS (Online Transaction Information System) incorporates robust quality control and reconciliation processes to ensure data integrity, accuracy, and compliance across its financial operations. These processes are implemented through dedicated COBOL programs, specialized copybooks, and structured workflows. Here’s a detailed breakdown:

---

### 1. **Quality Control Processes**

**A. Programmatic Validation**
- Many OTIS programs include validation routines that check for data consistency, correct formats, and business rule compliance before processing transactions.
- Example: Programs like **CHKSEC** (Source: [[FILE:CHKSEC|CHKSEC]], [[FILE:I|I]]ndex: programs) are called by 59 other programs to perform security and validation checks, ensuring only authorized and valid transactions proceed.

**B. Audit Trails**
- Programs such as **AUDIQ0** and **AUDIQ1** (Source: [[FILE:AUDIQ0|AUDIQ0]], [[FILE:AUDIQ1|AUDIQ1]], [[FILE:I|I]]ndex: flows) maintain audit logs by recording key actions, changes, and user inputs. This supports traceability and post-process review.

**C. Error Handling and Logging**
- Error routines are embedded in most transaction programs, capturing exceptions and logging them for review. This includes both system errors and business rule violations.

---

### 2. **Reconciliation Processes**

**A. Dedicated Reconciliation Programs**
- The **RECONC** program is central to OTIS’s reconciliation workflow (Source: [[FILE:RECONC|RECONC]], [[FILE:I|I]]ndex: programs, flows).
  - **Business Purpose:** RECONC matches and verifies financial records, ensuring that totals, balances, and transactions align across subsystems.
  - **Technical Implementation:** It performs initialization, creates work files, writes detail lines, prints grand totals, and range lines (Source: [[FILE:RECONC|RECONC]], [[FILE:I|I]]ndex: flows).

**B. Supporting Copybooks**
- RECONC uses a suite of copybooks to structure and manage reconciliation data:
  - **LIBGL/[[FILE:GL01GT.CPY|GL01GT.CPY]]** – General ledger transaction definitions (Source: [[FILE:RECONC.CBL|RECONC.CBL]], Index: copybook_usage)
  - **LIBGB/[[FILE:GBFSWKI.CPY|GBFSWKI.CPY]]** – Financial summary work item structures
  - **LIBGB/[[FILE:GBFDPR.CPY|GBFDPR.CPY]]** – Financial detail processing records
  - **LIBSP/[[FILE:RECONC_DEF.CPY|RECONC_DEF.CPY]]** – Reconciliation definitions
  - **LIBSP/[[FILE:RECONC_SCN.CPY|RECONC_SCN.CPY]]** – Screen definitions for reconciliation
  - **LIBGB/[[FILE:GBGRGS_SQL.CPY|GBGRGS_SQL.CPY]]** – SQL routines for group reconciliation
  - **LIBGB/[[FILE:SCREENW.CPY|SCREENW.CPY]]** – Common screen handling for reconciliation
  - **LIBGB/[[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]]** – Extended company name structures
  - **LIBGB/[[FILE:GBWSGR.CPY|GBWSGR.CPY]]** – Work summary group records
  - **LIBGB/[[FILE:GETFMW.CPY|GETFMW.CPY]]** – Get financial work records
  - **LIBGB/[[FILE:GBFSPR.CPY|GBFSPR.CPY]]** – Financial summary processing records

**C. Reconciliation Workflow**
- The reconciliation process typically follows these steps:
  1. **Initialization:** Set up work files and prepare for reconciliation.
  2. **Record Matching:** Compare transaction records across files (e.g., loan, payment, general ledger).
  3. **Detail Writing:** Log matched and unmatched records for review.
  4. **Totals Calculation:** Print grand totals and range lines for summary.
  5. **Reporting:** Generate reports for discrepancies and matched records.

**D. Integration with Other Programs**
- RECONC is called by other programs and also calls additional modules for specialized reconciliation tasks (Source: [[FILE:RECONC|RECONC]], [[FILE:I|I]]ndex: programs).
- Related programs such as **RECAPJ**, **DBREPT**, and **RCFILE** also participate in reconciliation and reporting, ensuring comprehensive coverage.

---

### 3. **Sample Reconciliation Flow (RECONC)**

**Business Flow:**
- User initiates reconciliation → RECONC initializes work files → Matches records → Writes details → Prints totals → Generates reports.

**Technical Flow:**
- DECLARATIVES → INITIALIZATION → CREATE-GT-WORK → WRITE-DETAIL-LINE → PRINT-GRAND-TOTALS → PRINT-RANGE-LINES (Source: [[FILE:RECONC|RECONC]], [[FILE:I|I]]ndex: flows)

---

### 4. **Quality Control and Reconciliation Architecture**

Here's a visual representation:

<svg width="900" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Quality Control & Reconciliation Architecture</text>
  
  <!-- Quality Control Layer -->
  <rect x="50" y="70" width="800" height="100" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Quality Control Layer</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Programs: CHKSEC, AUDIQ0, AUDIQ1</text>
  <text x="450" y="140" text-anchor="middle" font-size="12" fill="#6366f1">Validation, Audit Logging, Error Handling</text>
  
  <!-- Reconciliation Layer -->
  <rect x="50" y="200" width="800" height="120" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="225" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Reconciliation Layer</text>
  <text x="450" y="250" text-anchor="middle" font-size="14" fill="#1e40af">Programs: RECONC, RECAPJ, DBREPT, RCFILE</text>
  <text x="450" y="270" text-anchor="middle" font-size="12" fill="#3b82f6">Record Matching, Totals Calculation, Reporting</text>
  
  <!-- Data Layer -->
  <rect x="50" y="350" width="800" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="375" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="450" y="400" text-anchor="middle" font-size="14" fill="#15803d">Files: Loan, Payment, General Ledger, Work Files</text>
  <text x="450" y="420" text-anchor="middle" font-size="12" fill="#22c55e">Persistent Storage, Transaction Records</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrowQC" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="170" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrowQC)"/>
  <line x1="450" y1="320" x2="450" y2="350" stroke="#666" stroke-width="2" marker-end="url(#arrowQC)"/>
</svg>

---

### 5. **Summary Table: Key Programs and Copybooks**

| Program/Copybook         | Purpose/Role                                      |
|-------------------------|---------------------------------------------------|
| CHKSEC                  | Security and validation checks                    |
| AUDIQ0, AUDIQ1          | Audit logging and quality control                 |
| RECONC                  | Core reconciliation, totals, reporting            |
| RECAPJ, DBREPT, RCFILE  | Supporting reconciliation/reporting               |
| LIBGL/[[FILE:GL01GT.CPY|GL01GT.CPY]]        | General ledger transaction definitions            |
| LIBGB/[[FILE:GBFSWKI.CPY|GBFSWKI.CPY]]       | Financial summary work item structures            |
| LIBGB/[[FILE:GBFDPR.CPY|GBFDPR.CPY]]        | Financial detail processing records               |
| LIBSP/[[FILE:RECONC_DEF.CPY|RECONC_DEF.CPY]]    | Reconciliation definitions                        |
| LIBSP/[[FILE:RECONC_SCN.CPY|RECONC_SCN.CPY]]    | Screen definitions for reconciliation             |
| LIBGB/[[FILE:GBGRGS_SQL.CPY|GBGRGS_SQL.CPY]]    | SQL routines for group reconciliation             |
| LIBGB/[[FILE:SCREENW.CPY|SCREENW.CPY]]       | Common screen handling for reconciliation         |
| LIBGB/[[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]]    | Extended company name structures                  |
| LIBGB/[[FILE:GBWSGR.CPY|GBWSGR.CPY]]        | Work summary group records                        |
| LIBGB/[[FILE:GETFMW.CPY|GETFMW.CPY]]        | Get financial work records                        |
| LIBGB/[[FILE:GBFSPR.CPY|GBFSPR.CPY]]        | Financial summary processing records              |

---

### **Conclusion**

OTIS’s quality control and reconciliation processes are deeply integrated into its architecture, leveraging specialized programs and copybooks to validate, audit, and reconcile financial data. This ensures high data integrity, supports regulatory compliance, and provides robust reporting for business operations.

**Sources:**
- CHKSEC, RECONC, AUDIQ0, AUDIQ1, RECAPJ, DBREPT, RCFILE (Index: programs, flows)
- [[FILE:RECONC.CBL|RECONC.CBL]] (Index: copybook_usage)
- LIBGL/[[FILE:GL01GT.CPY|GL01GT.CPY]], LIBGB/[[FILE:GBFSWKI.CPY|GBFSWKI.CPY]], LIBGB/[[FILE:GBFDPR.CPY|GBFDPR.CPY]], LIBSP/[[FILE:RECONC_DEF.CPY|RECONC_DEF.CPY]], LIBSP/[[FILE:RECONC_SCN.CPY|RECONC_SCN.CPY]], LIBGB/[[FILE:GBGRGS_SQL.CPY|GBGRGS_SQL.CPY]], LIBGB/[[FILE:SCREENW.CPY|SCREENW.CPY]], LIBGB/[[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]], LIBGB/[[FILE:GBWSGR.CPY|GBWSGR.CPY]], LIBGB/[[FILE:GETFMW.CPY|GETFMW.CPY]], LIBGB/[[FILE:GBFSPR.CPY|GBFSPR.CPY]] (Index: copybook_usage)

## 2.3 User Roles and Workflows
### What are the primary user roles that interact with the OTIS system?

The primary user roles that interact with the OTIS (Online Transaction Information System) application are centered around financial services operations, loan servicing, and compliance. Based on the retrieved context and typical OTIS business processes, here are the main user roles:

**1. Loan Servicing Representatives**
   - Handle daily loan account maintenance, payment processing, rate changes, and customer inquiries.
   - Use screens and programs for loan maintenance (e.g., LPMAIN), payment entry (e.g., APIPAY), and account updates.

**2. Branch Operations Staff**
   - Manage branch-level statistics, reporting, and reconciliation.
   - Interact with programs like BYINQ (Branch Statistics Inquiry) and related reporting modules.

**3. Compliance and Monitoring Officers**
   - Oversee regulatory compliance, insurance policy monitoring, and audit controls.
   - Use specialized programs such as ADDEXT (Compliance and Monitoring Control for AD&D policies).
   - Responsible for ensuring system controls are functioning as expected.

**4. Quality Control and Reconciliation Analysts**
   - Perform data integrity checks, reconciliation of financial transactions, and error resolution.
   - Utilize programs for error logging (ERRLOG), reconciliation routines, and audit trails.

**5. System Administrators and Technical Support**
   - Maintain system configuration, manage user access, and support technical troubleshooting.
   - Interact with system-level programs (e.g., SYSTEM, SQL) and perform data migrations or conversions.

**6. Reporting and Audit Staff**
   - Generate, review, and distribute financial and compliance reports.
   - Use reporting modules (e.g., PERUSE for printed reports, EOQRPT for audit reports).

**7. Dealer and Addon Maintenance Users**
   - Manage dealer information, addon products, and related maintenance tasks.
   - Access dealer maintenance screens and programs (e.g., DLMAIN, ADMAIN).

**8. Batch Processing Operators**
   - Run scheduled batch jobs for end-of-day, month-end, and other periodic processing.
   - Use batch menu programs (e.g., BPMENU) and monitor batch job completion.

**9. Data Inquiry and Research Users**
   - Perform ad-hoc inquiries into loan, borrower, and transaction data.
   - Use inquiry programs (e.g., CPINQ0, UTINQ) for research and customer support.

---

**Source References:**
- [[FILE:ADDEXT.CBL|ADDEXT.CBL]] (compliance officer role) [Source: [[FILE:ADDEXT.CBL|ADDEXT.CBL]], Index: code]
- [[FILE:UTINQ.CBL|UTINQ.CBL]] (branch statistics inquiry) [Source: [[FILE:UTINQ.CBL|UTINQ.CBL]], Index: code]
- [[FILE:PERUSE.CBL|PERUSE.CBL]] (reporting/audit staff) [Source: [[FILE:PERUSE.CBL|PERUSE.CBL]], Index: code]
- SYSTEM (system administrator role) [Source: [[FILE:SYSTEM|SYSTEM]], [[FILE:I|I]]ndex: flows]
- ERRLOG (quality control/reconciliation) [Source: [[FILE:ERRLOG|ERRLOG]], [[FILE:I|I]]ndex: variables]
- Contextual program names and descriptions (e.g., LPMAIN, DLMAIN, BPMENU)

---

**Summary Table of Roles and Example Programs:**

| User Role                        | Example Programs/Screens      | Primary Functions                       |
|-----------------------------------|------------------------------|-----------------------------------------|
| Loan Servicing Rep                | LPMAIN, APIPAY               | Loan maintenance, payments              |
| Branch Operations Staff           | BYINQ, UTINQ                 | Branch stats, reporting                 |
| Compliance Officer                | ADDEXT                       | Policy monitoring, compliance           |
| Quality Control Analyst           | ERRLOG, reconciliation mods  | Data integrity, error resolution        |
| System Administrator              | SYSTEM, SQL                  | System config, migrations               |
| Reporting/Audit Staff             | PERUSE, EOQRPT               | Report generation, audit trails         |
| Dealer/Add-on Maintenance User    | DLMAIN, ADMAIN               | Dealer/addon management                 |
| Batch Processing Operator         | BPMENU                       | Batch job execution                     |
| Data Inquiry/Research User        | CPINQ0, UTINQ                | Data lookup, research                   |

---

**Business Context:**  
These roles reflect the operational, compliance, and technical needs of a financial services organization using OTIS for loan servicing and related activities. Each role interacts with specific screens and programs tailored to their responsibilities.

If you need more detail on the permissions, workflows, or screen access for each role, please specify which role or process you want to explore further.

### What functions do loan servicing representatives perform in OTIS daily?

Loan servicing representatives in the OTIS (Online Transaction Information System) application perform a wide range of daily functions that support the management, maintenance, and quality control of loan accounts. Their activities are designed to ensure accurate loan processing, customer service, compliance, and financial integrity.

**BUSINESS FUNCTIONS PERFORMED DAILY:**

1. **Loan Account Maintenance**
   - Update borrower information (address, contact details, employment status)
   - Correct or adjust loan records based on customer requests or error resolution
   - Review and process loan status changes (e.g., paid off, delinquent, rescheduled)

2. **Payment Processing**
   - Accept and record customer payments (cash, check, ACH)
   - Apply payments to principal, interest, fees, or other charges
   - Recalculate payment schedules after rate changes or partial payments

3. **Rate Changes and Recalculations**
   - Implement new interest rates or service charge formulas as directed by management
   - Recalculate loan balances and payment amounts following rate adjustments
   - Ensure compliance with regulatory requirements for rate changes

4. **Quality Control and Reconciliation**
   - Review daily loan statistics and transaction summaries for accuracy
   - Reconcile payment records and outstanding balances
   - Identify and resolve discrepancies in loan data or payment application

5. **Reporting and Statistics**
   - Generate daily operating loan statistics (e.g., outstanding balances, number of loans)
   - Prepare reports for management review (delinquency, payment activity, loan volume)
   - Track and report on special loan categories (e.g., real estate, small loans)

6. **Customer Service and Communication**
   - Respond to borrower inquiries about loan status, payments, and account details
   - Provide payoff quotes and account histories
   - Initiate communications for overdue payments or account changes

7. **Compliance and Audit Support**
   - Ensure all transactions comply with company policies and regulatory standards
   - Document changes and maintain audit trails for loan modifications
   - Support periodic audits by preparing required documentation and reports

**TECHNICAL IMPLEMENTATION IN OTIS:**

- Representatives interact with OTIS screens and menu options to perform these functions. For example:
  - **DAILY Processing:** Programs like [[FILE:DAILY.CBL|DAILY.CBL]] and [[FILE:UTINQ.CBL|UTINQ.CBL]] handle daily loan statistics, payment reconciliation, and reporting.
    - Example code from [[FILE:UTINQ.CBL|UTINQ.CBL]]:
      ```cobol
      * SETUP DAILY OPERATING LOAN STATISTICS
      MOVE SUM-DAILY-AMT TO DAILY-OSBAL.
      IF WORLD-DAILY-RECENCY = "W"
         MOVE SUM-DAILY-OUTBAL-RECENCY60 TO DAILY-OSBAL.
      MOVE SUM-DAILY-NO TO DAILY-OSNO.
      IF WORLD-DAILY-RECENCY = "W"
         MOVE SUM-DAILY-UNITS-RECENCY60 TO DAILY-OSNO.
      ```
      (Source: [[FILE:UTINQ.CBL|UTINQ.CBL]], Index: code)

  - **Loan Maintenance:** Programs like LOAN, LONPF0, LONPF3, and RESCHD support loan record updates, payment recalculations, and rate changes.
    - Example: RESCHD flow includes modules for updating files, resetting forms, and scanning borrower records.
      (Source: [[FILE:RESCHD|RESCHD]], [[FILE:I|I]]ndex: flows)

  - **Quality Control:** Daily reconciliation and statistics are managed through routines that aggregate and validate loan data, as seen in [[FILE:DSINQ.CBL|DSINQ.CBL]] and [[FILE:DAILY.CBL|DAILY.CBL]].
    - Example: [[FILE:DSINQ.CBL|DSINQ.CBL]] computes totals and averages for reporting and reconciliation.
      (Source: [[FILE:DSINQ.CBL|DSINQ.CBL]], Index: code)

**SUMMARY TABLE OF DAILY FUNCTIONS:**

| Function Area            | Example Activities                                 | Supporting Programs/Files      |
|-------------------------|----------------------------------------------------|-------------------------------|
| Account Maintenance     | Update borrower info, correct records               | LOAN, RESCHD, LONPF0          |
| Payment Processing      | Accept/apply payments, recalculate schedules        | DAILY, UTINQ, LONPF3          |
| Rate Changes            | Implement new rates, recalculate balances           | RESCHD, LONPF3, CALCZ3        |
| Quality Control         | Reconcile payments, review statistics               | DAILY, DSINQ, UTINQ           |
| Reporting               | Generate daily loan stats, management reports       | DAILY, UTINQ, DSINQ           |
| Customer Service        | Respond to inquiries, provide payoff quotes         | LOAN, LONPG0                  |
| Compliance/Audit        | Document changes, support audits                    | DAILY, RESCHD                 |

**SOURCE REFERENCES:**
- [[FILE:UTINQ.CBL|UTINQ.CBL]], Index: code (daily statistics setup)
- RESCHD, Index: flows (loan rescheduling and updates)
- [[FILE:DSINQ.CBL|DSINQ.CBL]], Index: code (reconciliation and reporting)
- [[FILE:DAILY.CBL|DAILY.CBL]], Index: copybook_usage (daily processing routines)
- LOAN, Index: programs (loan maintenance)

---

**Here's a visual representation of the daily workflow for loan servicing representatives in OTIS:**

<svg width="900" height="520" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Daily Loan Servicing Workflow</text>
  
  <!-- User Role -->
  <rect x="50" y="70" width="200" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Loan Servicing Rep</text>
  <text x="150" y="120" text-anchor="middle" font-size="12" fill="#6366f1">User</text>
  
  <!-- Screens/Entry Points -->
  <rect x="300" y="70" width="200" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="400" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">OTIS Screens/Menu</text>
  <text x="400" y="120" text-anchor="middle" font-size="12" fill="#3b82f6">DAILY, LOAN, RESCHD</text>
  
  <!-- Main Functions -->
  <rect x="550" y="70" width="280" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="690" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Daily Functions</text>
  <text x="690" y="120" text-anchor="middle" font-size="12" fill="#22c55e">Maintenance, Payments, QC</text>
  
  <!-- Function Boxes -->
  <rect x="100" y="200" width="180" height="50" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="190" y="225" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Account Maintenance</text>
  
  <rect x="350" y="200" width="180" height="50" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="440" y="225" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Payment Processing</text>
  
  <rect x="600" y="200" width="180" height="50" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="690" y="225" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Rate Changes</text>
  
  <rect x="100" y="300" width="180" height="50" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="190" y="325" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Quality Control</text>
  
  <rect x="350" y="300" width="180" height="50" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="440" y="325" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Reporting</text>
  
  <rect x="600" y="300" width="180" height="50" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="690" y="325" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Customer Service</text>
  
  <rect x="350" y="400" width="180" height="50" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="440" y="425" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Compliance/Audit</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="250" y1="100" x2="300" y2="100" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="500" y1="100" x2="550" y2="100" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="190" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="440" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="690" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="190" y2="300" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="440" y2="300" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="690" y2="300" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="130" x2="440" y2="400" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

**In summary:**  
Loan servicing representatives in OTIS handle daily account maintenance, payment processing, rate changes, quality control, reporting, customer service, and compliance tasks—using dedicated screens and programs to ensure accurate, compliant, and efficient loan operations.

### How do customer service agents use OTIS to respond to customer inquiries?

Customer service agents use the OTIS (Online Transaction Information System) application to efficiently respond to customer inquiries related to loan servicing, account status, payments, and general account maintenance. Here’s how the process works:

**BUSINESS PURPOSE:**
- Agents provide real-time information and support to customers regarding their loan accounts.
- They handle requests such as account balance inquiries, payment history, transaction details, and status updates.
- Agents may also assist with updating customer information, resolving discrepancies, and initiating service requests.

**TECHNICAL IMPLEMENTATION:**

1. **Accessing Inquiry Screens:**
   - Agents log into OTIS and navigate to inquiry modules such as CPINQ0 (Customer Payment Inquiry), RXSCAN (Range Scan Inquiry), or other relevant screens.
   - These modules are designed to capture search criteria (e.g., account number, customer name) and display detailed account information.

2. **Processing the Inquiry:**
   - The agent enters customer identifiers (account number, name, etc.) into the system.
   - The inquiry program (e.g., CPINQ0) performs validation and retrieves data from underlying files and databases.
   - Example flow from CPINQ0 (Source: [[FILE:CPINQ0|CPINQ0]], [[FILE:I|I]]ndex: flows):
     - MAIN-MODULE → FORM-RESET (prepares screen)
     - ENTER-KEYS → GET-LC (fetches loan/customer data)
     - SET-COLLECTOR-INFO (populates agent details)
     - SEND-DELINQ-SCAN (initiates delinquency scan if needed)

3. **Displaying Results:**
   - The system displays account details, payment history, delinquency status, and other relevant information on the screen.
   - If the inquiry is about a specific transaction or payment, modules like RXSCAN or BULKTR may be used to scan ranges or verify transaction existence.

4. **Responding to the Customer:**
   - Agents review the information and communicate results to the customer.
   - If discrepancies or errors are found, agents may initiate further investigation or escalate to specialized teams.

5. **Additional Actions:**
   - Agents can update certain customer details, initiate service requests, or trigger follow-up actions (e.g., sending confirmation emails via EMAIL program).
   - All actions are logged for audit and quality control purposes.

**Example Program Flows Supporting Inquiries:**

- **CPINQ0 (Customer Payment Inquiry):**
  - Handles payment inquiries, validates input, retrieves payment and loan data, and displays results.
  - Flow includes modules for form reset, key entry, data retrieval, and collector info setup.

- **RXSCAN (Range Scan Inquiry):**
  - Allows agents to scan account ranges, display results, and send legends for clarification.
  - Handles input validation and error handling.

- **EMAIL:**
  - Used to send confirmation or follow-up messages to customers after inquiry resolution.

**Quality Control and Audit:**
- All inquiry actions are tracked and logged.
- Error handling routines ensure agents receive clear feedback if data is missing or invalid.
- Audit trails support compliance and quality assurance.

---

**Here's a visual representation of a typical customer inquiry workflow in OTIS:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Customer Inquiry Workflow</text>
  
  <!-- Step 1: Agent Login -->
  <rect x="50" y="80" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Agent Login</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="11">Access OTIS</text>
  
  <!-- Step 2: Select Inquiry Screen -->
  <rect x="270" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Select Inquiry Screen</text>
  <text x="360" y="130" text-anchor="middle" fill="white" font-size="11">CPINQ0 / RXSCAN</text>
  
  <!-- Step 3: Enter Search Criteria -->
  <rect x="490" y="80" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Enter Search Criteria</text>
  <text x="580" y="130" text-anchor="middle" fill="white" font-size="11">Acct #, Name, etc.</text>
  
  <!-- Step 4: Retrieve Data -->
  <rect x="710" y="80" width="140" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="780" y="110" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Retrieve Data</text>
  <text x="780" y="130" text-anchor="middle" fill="#15803d" font-size="11">Display Results</text>
  
  <!-- Step 5: Respond to Customer -->
  <rect x="270" y="200" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="360" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Respond to Customer</text>
  <text x="360" y="250" text-anchor="middle" fill="white" font-size="11">Provide Info/Resolution</text>
  
  <!-- Step 6: Log/Audit -->
  <rect x="490" y="200" width="180" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="580" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Log/Audit</text>
  <text x="580" y="250" text-anchor="middle" fill="white" font-size="11">Quality Control</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="670" y1="110" x2="710" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="780" y1="140" x2="360" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="260" x2="580" y2="230" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Source References:**
- CPINQ0 inquiry flow (Source: [[FILE:CPINQ0|CPINQ0]], [[FILE:I|I]]ndex: flows)
- RXSCAN inquiry flow (Source: [[FILE:RXSCAN|RXSCAN]], [[FILE:I|I]]ndex: flows)
- EMAIL program for customer notifications (Source: [[FILE:EMAIL|EMAIL]], [[FILE:I|I]]ndex: flows)
- Quality control and audit processes (Source: previous answer, OTIS system overview)

If you need details on specific inquiry screens or want to see the exact fields agents use, let me know!

### What capabilities do collection specialists have in OTIS for delinquent accounts?

Collection specialists in the OTIS (Online Transaction Information System) application have a robust set of capabilities for managing delinquent accounts. Their functions are designed to support the full lifecycle of collections, from identifying delinquency to generating reports and initiating follow-up actions.

**Key Capabilities for Collection Specialists in OTIS:**

### 1. **Identifying and Reviewing Delinquent Accounts**
- Specialists can access screens and reports that highlight accounts with overdue payments, including detailed delinquency status and aging buckets (e.g., 30, 60, 90 days past due).
- They can view account details, payment history, and current balances to assess the severity of delinquency.
  - Example: The "Collection Information" section on screens such as [[FILE:LNMAN8_SCN.CPY|LNMAN8_SCN.CPY]] displays collector IDs and delinquency data (Source: [[FILE:LNMAN8_SCN.CPY|LNMAN8_SCN.CPY]], Index: code).

### 2. **Generating Collection Letters and Reports**
- The COLLET program produces the "Most Recent Collection Letter Report," allowing specialists to generate and review letters sent to delinquent customers (Source: [[FILE:COLLET.CBL|COLLET.CBL]], Index: code).
- Specialists can run reports to track which accounts have received collection communications and monitor follow-up status.
  - Example: COLRPT program generates detailed reports on worked accounts and result codes, helping track collection effectiveness (Source: [[FILE:COLRPT.CBL|COLRPT.CBL]], Index: code).

### 3. **Recording Collection Actions and Results**
- Specialists can update account records to indicate actions taken (e.g., calls made, letters sent, promises to pay).
- The system tracks "worked account" flags and result codes, ensuring that each collector’s activity is logged and visible for audit and follow-up (Source: [[FILE:COLRPT.CBL|COLRPT.CBL]], Index: code).

### 4. **Managing Collector Assignments and Work Queues**
- OTIS supports assignment of accounts to specific collectors, enabling workload balancing and targeted follow-up.
- Specialists can review and update collector IDs, behavior risk factors, and sequence flags for each account (Source: [[FILE:CQMAIN.CBL|CQMAIN.CBL]], Index: code).

### 5. **Initiating Follow-Up Actions**
- The system allows specialists to schedule follow-up dates, generate reminders, and escalate accounts as needed.
- They can use screens to enter notes, set next action dates, and trigger additional communications.

### 6. **Computing Delinquency and Arrearage Amounts**
- Programs like LPCDEL compute the contractual delinquent amount and arrearage for each account, providing accurate financial data for collection efforts (Source: [[FILE:LPCDEL.CPY|LPCDEL.CPY]], Index: code).
- Specialists can view these computed amounts to prioritize accounts and tailor collection strategies.

### 7. **Reviewing Collection History and Compliance**
- Specialists have access to historical data on collection actions, ensuring compliance with company policies and regulatory requirements.
- The system prevents duplicate actions and flags accounts that have already been worked by any collector within a branch (Source: [[FILE:COLRPT.CBL|COLRPT.CBL]], Index: code).

---

**Business Purpose:**
- Enable efficient, compliant, and documented collection efforts for delinquent accounts.
- Support specialists in prioritizing accounts, tracking actions, and improving recovery rates.

**Technical Implementation:**
- Programs: COLLET, COLRPT, LPCDEL, CQMAIN, LNMAN8_SCN, among others.
- Data: Delinquency status, collector assignments, action/result codes, computed arrearage.
- Screens: Display collection info, allow entry of actions, generate reports and letters.

---

**Here's a visual representation of the main collection specialist workflow in OTIS:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Collection Specialist Workflow</text>
  
  <!-- Step 1: Identify Delinquent Accounts -->
  <rect x="50" y="80" width="200" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="150" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Identify Delinquent Accounts</text>
  <text x="150" y="130" text-anchor="middle" fill="white" font-size="11">View aging, status</text>
  
  <!-- Step 2: Generate Letters/Reports -->
  <rect x="300" y="80" width="200" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="400" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Generate Letters/Reports</text>
  <text x="400" y="130" text-anchor="middle" fill="white" font-size="11">COLLET, COLRPT</text>
  
  <!-- Step 3: Record Actions -->
  <rect x="550" y="80" width="200" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="650" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Record Collection Actions</text>
  <text x="650" y="130" text-anchor="middle" fill="white" font-size="11">Update flags, notes</text>
  
  <!-- Step 4: Manage Assignments -->
  <rect x="175" y="200" width="200" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="275" y="230" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Manage Collector Assignments</text>
  <text x="275" y="250" text-anchor="middle" fill="#15803d" font-size="11">Work queues</text>
  
  <!-- Step 5: Initiate Follow-Up -->
  <rect x="425" y="200" width="200" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="525" y="230" text-anchor="middle" fill="#1e40af" font-size="16" font-weight="bold">Initiate Follow-Up Actions</text>
  <text x="525" y="250" text-anchor="middle" fill="#1e40af" font-size="11">Schedule, escalate</text>
  
  <!-- Step 6: Compute Delinquency -->
  <rect x="675" y="200" width="200" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="8"/>
  <text x="775" y="230" text-anchor="middle" fill="#4338ca" font-size="16" font-weight="bold">Compute Delinquency/Arrearage</text>
  <text x="775" y="250" text-anchor="middle" fill="#4338ca" font-size="11">LPCDEL, LPCDELW</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="250" y1="110" x2="300" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="500" y1="110" x2="550" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="650" y1="140" x2="275" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="650" y1="140" x2="525" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="650" y1="140" x2="775" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Source Citations:**
- "MOST RECENT COLLECTION LETTER REPORT PR# 256" (Source: [[FILE:COLLET.CBL|COLLET.CBL]], Index: code)
- "STORAGE TABLE (TT-DELINQUENCY-TOTALS)...worked account flag" (Source: [[FILE:COLRPT.CBL|COLRPT.CBL]], Index: code)
- "CONTRACTUAL DELINQUENCY WORKERS (ARREARAGE)" (Source: [[FILE:LPCDELW.CPY|LPCDELW.CPY]], Index: code)
- "Collection Information: COLLECTOR'S ID" (Source: [[FILE:LNMAN8_SCN.CPY|LNMAN8_SCN.CPY]], Index: code)
- "Routine computes the amount...contractually delinquent" (Source: [[FILE:LPCDEL.CPY|LPCDEL.CPY]], Index: code)

If you need details on specific screens, menu options, or field-level capabilities, please specify the area of collections you want to explore further.

### How do accounting and finance users interact with OTIS for financial reporting?

Accounting and finance users interact with the OTIS (Online Transaction Information System) application primarily to generate, review, and analyze financial reports that support business oversight, compliance, and decision-making. Their interaction is structured around specialized programs and reporting modules within OTIS, designed to handle complex financial data and produce detailed reports.

**Business Purpose:**
- **Financial Statement Generation:** Users produce branch, division, and consolidated financial statements, including balance sheets, income statements, and trial balances.
- **Regulatory Reporting:** OTIS supports the creation of reports required for regulatory compliance (e.g., insurance tax reports, 1099 forms).
- **Audit and Reconciliation:** Users access audit trails, transaction logs, and reconciliation reports to ensure data integrity and support internal/external audits.
- **Budgeting and Forecasting:** The system provides historical and current financial data for budgeting and forecasting activities.

**Technical Implementation:**
- **Report Programs:** Users access dedicated COBOL programs such as DBREPT, FMREPT, and TXMOR2 to run specific financial reports.
  - **DBREPT:** Generates detailed branch-level financial reports, including totals and grand totals (Source: [[FILE:DBREPT|DBREPT]], [[FILE:I|I]]ndex: programs).
  - **FMREPT:** Handles monthly financial reporting and uses copybooks like [[FILE:DECLRP2.CPY|DECLRP2.CPY]] for report formatting (Source: [[FILE:FMREPT.CBL|FMREPT.CBL]], Index: copybook_usage).
  - **TXMOR2:** Produces monthly insurance tax reports, including premium and refund details (Source: [[FILE:TXMOR2.CBL|TXMOR2.CBL]], Index: code).
- **Data Access:** These programs read from financial files such as FSFILE (Financial Statement File), as defined in copybooks like [[FILE:GL01FS.CPY|GL01FS.CPY]] (Source: [[FILE:GL01FS.CPY|GL01FS.CPY]], Index: code).
- **Report Customization:** Users can select parameters (branch, date range, account type) to customize reports.
- **Output Formats:** Reports are generated for on-screen review, printing, or export to external systems.
- **Audit Features:** Programs like JOURNL provide access to transaction logs for audit and reconciliation (Source: [[FILE:JOURNL|JOURNL]], [[FILE:I|I]]ndex: variables).

**Workflow Example:**
1. **User Login:** Accounting/finance user logs into OTIS and navigates to the financial reporting menu.
2. **Report Selection:** User selects the desired report (e.g., monthly branch statement).
3. **Parameter Entry:** User enters parameters (branch code, month/year).
4. **Report Generation:** OTIS runs the appropriate COBOL program (e.g., DBREPT), which reads financial data files and applies business logic.
5. **Review & Export:** User reviews the report on screen, prints it, or exports it for further analysis.
6. **Audit/Reconciliation:** For audit purposes, users access transaction logs and reconciliation reports via programs like JOURNL.

**Key Programs and Files:**

| Program/File      | Purpose/Description                                               | Source Reference                           |
|-------------------|------------------------------------------------------------------|--------------------------------------------|
| DBREPT            | Branch-level financial reporting                                 | DBREPT, Index: programs                    |
| FMREPT            | Monthly financial reporting                                      | [[FILE:FMREPT.CBL|FMREPT.CBL]], Index: copybook_usage          |
| TXMOR2            | Monthly insurance tax report                                     | [[FILE:TXMOR2.CBL|TXMOR2.CBL]], Index: code                    |
| [[FILE:GL01FS.CPY|GL01FS.CPY]]        | Financial Statement File structure (FSFILE)                      | [[FILE:GL01FS.CPY|GL01FS.CPY]], Index: code                    |
| JOURNL            | Transaction log for audit/reconciliation                         | JOURNL, Index: variables                   |

**Here's a visual representation:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Financial Reporting Workflow</text>
  
  <!-- User Layer -->
  <rect x="50" y="70" width="800" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Accounting/Finance User</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Menu Navigation, Parameter Entry</text>
  
  <!-- Program Layer -->
  <rect x="50" y="170" width="800" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="200" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Reporting Programs</text>
  <text x="450" y="225" text-anchor="middle" font-size="14" fill="#1e40af">DBREPT, FMREPT, TXMOR2, JOURNL</text>
  <text x="450" y="245" text-anchor="middle" font-size="12" fill="#3b82f6">Report Generation, Audit, Reconciliation</text>
  
  <!-- Data Layer -->
  <rect x="50" y="320" width="800" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="350" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="450" y="375" text-anchor="middle" font-size="14" fill="#15803d">FSFILE, [[FILE:GL01FS.CPY|GL01FS.CPY]], Transaction Logs</text>
  <text x="450" y="395" text-anchor="middle" font-size="12" fill="#22c55e">Financial Data, Audit Trails</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="140" x2="450" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="270" x2="450" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**Summary:**
Accounting and finance users in OTIS interact with specialized reporting programs to generate, review, and analyze financial statements, regulatory reports, and audit logs. Their workflow involves selecting reports, entering parameters, running COBOL-based reporting modules, and accessing underlying financial data files for comprehensive financial oversight.

**Sources:**
- DBREPT, Index: programs
- [[FILE:FMREPT.CBL|FMREPT.CBL]], Index: copybook_usage
- [[FILE:TXMOR2.CBL|TXMOR2.CBL]], Index: code
- [[FILE:GL01FS.CPY|GL01FS.CPY]], Index: code
- JOURNL, Index: variables

### What administrative and system configuration functions exist in OTIS?

OTIS (Online Transaction Information System) provides a range of administrative and system configuration functions to support system management, user access, and operational setup. These functions are essential for maintaining the integrity, security, and adaptability of the application in a financial services environment.

**Key Administrative and System Configuration Functions in OTIS:**

### 1. **System Command Execution**
- Programs such as **SYSTEM**, **[[FILE:SYSTEMW.CPY|SYSTEMW.CPY]]**, and related modules allow for the execution of system-level commands (e.g., shell commands, file operations).
- These are used for tasks like file management, environment setup, and integration with operating system utilities.
- Example: The `SYSTEM-CALL` paragraph in several programs performs system commands for tasks like file cleanup, batch job initiation, or environment variable management.
  - (Source: [[FILE:SYSTEM|SYSTEM]], [[FILE:I|I]]ndex: flows; [[FILE:SYSTEMW.CPY|SYSTEMW.CPY]], Index: code)

### 2. **Environment Variable and Configuration Management**
- The **SETENV** program manages environment variables and configuration settings required for OTIS operation.
- Functions include connecting to databases, setting up memory, and initializing system parameters.
  - (Source: [[FILE:SETENV|SETENV]], [[FILE:I|I]]ndex: programs, flows)
- Copybooks like **[[FILE:SYSTEMW.CPY|SYSTEMW.CPY]]** provide structures for storing and manipulating system command buffers and environment data.
  - (Source: [[FILE:SYSTEMW.CPY|SYSTEMW.CPY]], Index: code)

### 3. **User Access and Security Administration**
- Programs such as **PASSWD** and related copybooks (**[[FILE:PASSWDW.CPY|PASSWDW.CPY]]**) handle password verification, user authentication, and access control.
- Administrative users can set, verify, and manage passwords, ensuring secure access to OTIS functions.
  - (Source: [[FILE:PASSWD|PASSWD]], [[FILE:I|I]]ndex: flows; [[FILE:PASSWDW.CPY|PASSWDW.CPY]], Index: copybook_usage)

### 4. **Global Parameter and Option Maintenance**
- Menu programs like **COMENU** and **ASMENU** provide options for maintaining global system parameters and configuration options.
- These allow administrators to update system-wide settings, control feature availability, and adjust operational parameters.
  - (Source: [[FILE:COMENU|COMENU]], [[FILE:I|I]]ndex: flows; [[FILE:COMENU_SCN.CPY|COMENU_SCN.CPY]], Index: code)

### 5. **Branch and User Maintenance**
- Programs such as **BHMAIN**, **BWMAN1**, and **BWCREA** support administrative maintenance of branch records, user profiles, and related data.
- Functions include adding, updating, and deleting branch/user information, which is critical for organizational management.
  - (Source: [[FILE:BHMAIN|BHMAIN]], [[FILE:BWMAN1|BWMAN1]], [[FILE:BWCREA|BWCREA]], [[FILE:I|I]]ndex: flows)

### 6. **Audit and Logging**
- Modules like **AUDITW** and **AUDIQ1** provide audit logging capabilities, recording system events, user actions, and configuration changes for compliance and troubleshooting.
  - (Source: [[FILE:AUDITW|AUDITW]], [[FILE:I|I]]ndex: variables; AUDIQ1, Index: copybook_usage)

### 7. **System File and Path Management**
- Programs and copybooks such as **[[FILE:FILEWK.CPY|FILEWK.CPY]]** and routines in **INSTAL** handle system file path configuration, file status checks, and temporary file management.
- These ensure that OTIS can correctly locate and manage its operational files.
  - (Source: [[FILE:FILEWK.CPY|FILEWK.CPY]], Index: code; INSTAL, Index: flows)

### 8. **Menu-Driven Administrative Access**
- The application provides menu screens (e.g., **COMENU**, **ASMENU**, **SCMENU**) that allow administrators to navigate to various system configuration and maintenance functions.
- These menus typically include options for global parameters, branch maintenance, user management, and system utilities.
  - (Source: [[FILE:COMENU|COMENU]], [[FILE:ASMENU|ASMENU]], [[FILE:SCMENU|SCMENU]], [[FILE:I|I]]ndex: flows)

---

**Summary Table of Key Programs and Copybooks:**

| Program/Copybook      | Purpose/Functionality                                      |
|-----------------------|------------------------------------------------------------|
| SYSTEM                | Executes system-level commands                             |
| [[FILE:SYSTEMW.CPY|SYSTEMW.CPY]]           | Structures for system command buffers                      |
| SETENV                | Manages environment variables and configuration            |
| PASSWD                | Handles password and user authentication                   |
| [[FILE:PASSWDW.CPY|PASSWDW.CPY]]           | Password management structures                             |
| COMENU, ASMENU        | Menu-driven access to admin/config functions               |
| BHMAIN, BWMAN1        | Branch/user maintenance                                    |
| AUDITW, AUDIQ1        | Audit logging                                              |
| [[FILE:FILEWK.CPY|FILEWK.CPY]]            | System file/path management                                |
| INSTAL                | System installation and file setup routines                |

---

**Here's a visual representation of the administrative and system configuration architecture in OTIS:**

<svg width="900" height="520" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Administrative & System Configuration Architecture</text>
  
  <!-- Presentation Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Menus: COMENU, ASMENU, SCMENU</text>
  <text x="450" y="140" text-anchor="middle" font-size="12" fill="#6366f1">Admin Navigation, Option Selection</text>
  
  <!-- Business Logic Layer -->
  <rect x="50" y="180" width="800" height="120" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="210" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="450" y="235" text-anchor="middle" font-size="14" fill="#1e40af">Programs: SYSTEM, SETENV, PASSWD, BHMAIN, BWMAN1, AUDITW</text>
  <text x="450" y="260" text-anchor="middle" font-size="12" fill="#3b82f6">System Commands, Env Setup, Security, Branch/User Maintenance, Audit</text>
  
  <!-- Data Layer -->
  <rect x="50" y="320" width="800" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="345" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="450" y="370" text-anchor="middle" font-size="14" fill="#15803d">Files: System Config Files, Audit Logs, Branch/User Data</text>
  <text x="450" y="390" text-anchor="middle" font-size="12" fill="#22c55e">Persistent Storage</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="450" y2="180" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="300" x2="450" y2="320" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Source Citations:**
- SYSTEM administrative command execution (Source: [[FILE:SYSTEM|SYSTEM]], [[FILE:I|I]]ndex: flows)
- [[FILE:SYSTEMW.CPY|SYSTEMW.CPY]] structures for system commands (Source: [[FILE:SYSTEMW.CPY|SYSTEMW.CPY]], Index: code)
- SETENV environment management (Source: [[FILE:SETENV|SETENV]], [[FILE:I|I]]ndex: programs, flows)
- PASSWD password verification (Source: [[FILE:PASSWD|PASSWD]], [[FILE:I|I]]ndex: flows)
- COMENU, ASMENU, SCMENU menu-driven admin access (Source: [[FILE:COMENU|COMENU]], [[FILE:ASMENU|ASMENU]], [[FILE:SCMENU|SCMENU]], [[FILE:I|I]]ndex: flows)
- BHMAIN, BWMAN1 branch/user maintenance (Source: [[FILE:BHMAIN|BHMAIN]], [[FILE:BWMAN1|BWMAN1]], [[FILE:I|I]]ndex: flows)
- AUDITW, AUDIQ1 audit logging (Source: [[FILE:AUDITW|AUDITW]], [[FILE:I|I]]ndex: variables; AUDIQ1, Index: copybook_usage)
- [[FILE:FILEWK.CPY|FILEWK.CPY]] file/path management (Source: [[FILE:FILEWK.CPY|FILEWK.CPY]], Index: code)
- INSTAL installation routines (Source: [[FILE:INSTAL|INSTAL]], [[FILE:I|I]]ndex: flows)

If you need details about a specific administrative function or program, let me know!

### How do managers and supervisors use OTIS for oversight and approvals?

Managers and supervisors use OTIS (Online Transaction Information System) primarily for oversight, approvals, and control of key business processes—especially those involving loans, financial transactions, and user activity. Their functions are designed to ensure compliance, enforce business rules, and provide auditability across the system.

**BUSINESS PURPOSE:**
- **Loan Approval Oversight:** Managers review and approve loan applications, set approval limits, and verify that transactions comply with company policies and regulatory requirements.
- **Exception Handling:** Supervisors can override certain limits, approve exceptions, and manage special cases (e.g., Premier Advisory customers).
- **Audit and Review:** They access detailed logs, approval histories, and can review branch-level statistics and transaction details for compliance and performance monitoring.
- **User Management:** Supervisors may have authority to manage user access, reset passwords, and configure approval workflows.

**TECHNICAL IMPLEMENTATION:**

1. **Approval Windows and Limits:**
   - Managers interact with approval windows that display loan details, approval limits, and user IDs.
   - They can set or adjust approval limits for themselves or others, with logic enforcing maximums and requiring justification for overrides.
   - Example (from [[FILE:LNAPPROVAL.CPY|LNAPPROVAL.CPY]]):
     ```cobol
     MOVE HOLD-APPROVAL-LIMIT    TO PD-APPROVAL-LIMIT.
     MOVE HOLD-APPROVAL-USERID   TO PD-APPROVAL-USERID.
     PERFORM SEND-LNAPPROVAL-LEGEND.
     IF GP-GROUP-APPROVAL-FG = "Y"
        IF ( VDUNUM0 > GI-LN-APPROVAL-LIMIT ) AND
           ( VDUNUM0 > HOLD-GR-APPROVAL-LIMIT )
           MOVE "TOTAL REPAY AMT EXCEEDS YOUR APPROVAL AUTHORITY!" TO MESS
     ```
     (Source: [[FILE:LNAPPROVAL.CPY|LNAPPROVAL.CPY]], Index: code)

2. **Special Handling for Premier Customers:**
   - Supervisors have logic to handle Premier Advisory customers differently, using loan amount rather than financed amount for approvals.
   - Example comment:
     ```
     * CHANGE FOR PREMIER TO USE LOAN AMOUNT (PD-LNAMT)
     * INSTEAD OF FINANCED AMOUNT (FINANCED-AMOUNT).
     ```
     (Source: [[FILE:LNAPPROVAL.CPY|LNAPPROVAL.CPY]], Index: code)

3. **Approval Workflow and Messaging:**
   - Approval actions trigger notifications and update logs, ensuring traceability.
   - Approval information is sent to display buffers and forms for user confirmation.
   - Example:
     ```cobol
     MOVE DISPLAY-APPROVAL-WINDOW-LIST TO WR-LIST.
     MOVE DISPLAY-APPROVAL-WINDOW-BUF TO WR-BUF.
     PERFORM CALL-WRFORM.
     ```
     (Source: [[FILE:LNAPPROVAL.CPY|LNAPPROVAL.CPY]], Index: code)

4. **Audit Trail and Comments:**
   - Managers can add comments and memos to approval actions, which are logged for future review.
   - Example (from [[FILE:PTHMOD.CBL|PTHMOD.CBL]]):
     ```cobol
     STRING "USERID MANAGER: ",LSP-BR-MANAGER-USERID DELIMITED BY "   " INTO DF-MEMO1.
     MOVE LSP-BR-MANAGER-DATE TO DATE-YYYYMMDD.
     PERFORM UPDATE-DF-MEMO.
     ```
     (Source: [[FILE:PTHMOD.CBL|PTHMOD.CBL]], Index: code)

5. **Branch and Group-Level Controls:**
   - Supervisors can view and manage approval limits at the branch or group level, ensuring that only authorized users can approve large transactions.
   - Approval limits and authority flags are checked before allowing actions.

**KEY COPYBOOKS INVOLVED:**
- **[[FILE:LNAPPROVAL.CPY|LNAPPROVAL.CPY]]** – Loan approval verification and window logic
- **[[FILE:LNAPPROVALW.CPY|LNAPPROVALW.CPY]]** – Loan approval workers (standard/window)
- **[[FILE:PTHMOD_SCN.CPY|PTHMOD_SCN.CPY]]** – Screen definitions for approval and comments
- **[[FILE:GICOPY.CBL|GICOPY.CBL]]** – Approval limit handling for GI (group insurance) loans

**SUMMARY OF MANAGER/SUPERVISOR WORKFLOW:**
1. Access approval screens showing pending transactions and limits.
2. Review transaction details, including customer type and loan amount.
3. Approve, deny, or override transactions based on authority and business rules.
4. Add comments or memos for audit trail.
5. System enforces limits and logs all actions for compliance.

---

**Here's a visual representation of the manager/supervisor approval workflow in OTIS:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Manager/Supervisor Approval Workflow</text>
  
  <!-- Step 1: Access Approval Screen -->
  <rect x="50" y="80" width="200" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="150" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Access Approval Screen</text>
  <text x="150" y="130" text-anchor="middle" fill="white" font-size="11">View pending transactions</text>
  
  <!-- Step 2: Review Details -->
  <rect x="300" y="80" width="200" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="400" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Review Transaction Details</text>
  <text x="400" y="130" text-anchor="middle" fill="white" font-size="11">Loan amount, customer type</text>
  
  <!-- Step 3: Approve/Override -->
  <rect x="550" y="80" width="200" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="650" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Approve / Override</text>
  <text x="650" y="130" text-anchor="middle" fill="white" font-size="11">Check limits, enter comments</text>
  
  <!-- Step 4: Audit Trail -->
  <rect x="300" y="200" width="200" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="400" y="230" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">Audit Trail & Logging</text>
  <text x="400" y="250" text-anchor="middle" fill="#15803d" font-size="11">Record approval, comments</text>
  
  <!-- Step 5: Compliance Check -->
  <rect x="550" y="200" width="200" height="70" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="650" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Compliance Enforcement</text>
  <text x="650" y="250" text-anchor="middle" fill="white" font-size="11">Enforce limits, notify errors</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="250" y1="115" x2="300" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="500" y1="115" x2="550" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="650" y1="150" x2="400" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="650" y1="150" x2="650" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

**Sources:**
- [[FILE:LNAPPROVAL.CPY|LNAPPROVAL.CPY]], Index: code (approval logic, limits, comments)
- [[FILE:LNAPPROVALW.CPY|LNAPPROVALW.CPY]], Index: code (approval workers, window handling)
- [[FILE:PTHMOD.CBL|PTHMOD.CBL]], Index: code (manager comments, audit trail)
- [[FILE:GICOPY.CBL|GICOPY.CBL]], Index: code (approval limit handling)

If you need details on specific approval screens or the exact fields managers can modify, please specify the screen or function.

### What reporting and analytics access do business analysts have in OTIS?

Business analysts in OTIS (Online Transaction Information System) have robust access to reporting and analytics features designed to support data-driven decision-making and operational insight. Their access typically includes:

**REPORTING ACCESS:**
- **Standard Financial Reports:** Business analysts can generate and review core financial reports such as trial balances, aged receivables, delinquency summaries, and branch-level performance reports. These are produced by programs like DBREPT, MBALR2, MBALR3, MBALR4, MBALR5, MBALR6, and MBALR6H, which handle various aspects of financial and operational reporting.
- **Custom and Ad Hoc Reports:** Analysts can run specialized reports for targeted business questions, such as potential increased loan opportunities (e.g., PILRPT – "Potential Increased Loan Report") or motor club claims (e.g., CLMCUP).
- **Branch and Group Analysis:** Programs such as GRLIST and CDLIST allow for listing and analysis of branch/group data, supporting comparative analytics across organizational units.

**ANALYTICS ACCESS:**
- **Data Extraction and Aggregation:** Programs like MBAGE1, MBAGED, MBANR2, MBUNR1H, and RZCORP support extraction and aggregation of loan, branch, and customer data for deeper analysis.
- **Trend and Performance Analysis:** Business analysts can use reporting modules to analyze trends in loan performance, charge-offs, recoveries, and other key metrics. Copybooks such as [[FILE:SPWR1IN.CPY|SPWR1IN.CPY]] and [[FILE:UPAWRCL.CPY|UPAWRCL.CPY]] define fields for financial metrics (e.g., net finance charges, sales, expenses, recovery income) that feed into these analytics.
- **GL and Accounting Data:** Access to general ledger and accounting data is provided via programs like DBREPT and supporting copybooks (e.g., LIBGB/[[FILE:GB01BR.CPY|GB01BR.CPY]], LIBGB/[[FILE:GBWSGR.CPY|GBWSGR.CPY]]), enabling reconciliation and financial oversight.

**ACCESS MECHANISMS:**
- **Menu-Driven Navigation:** Business analysts interact with OTIS through menu screens that route them to reporting modules. While the exact menu text is not present in the retrieved context, the structure allows selection of various report types and analytics functions.
- **Parameter Entry:** Analysts can specify parameters (date ranges, branch codes, report types) to tailor reports to their needs.
- **Export and Print Options:** Many reports can be exported or printed for further analysis outside the system.

**EXAMPLES OF REPORTING PROGRAMS AND COPYBOOKS:**

| Program/Copybook         | Description                                                                 |
|--------------------------|-----------------------------------------------------------------------------|
| DBREPT                   | Branch-level financial reporting and analytics (Source: [[FILE:DBREPT|DBREPT]], [[FILE:I|I]]ndex: flows)|
| MBALR2–MBALR6H           | Various branch and loan analytics reports (Source: [[FILE:MBALR2|MBALR2]], [[FILE:MBALR3|MBALR3]], etc., Index: flows)|
| PILRPT                   | Potential Increased Loan Report for customer solicitation (Source: [[FILE:PILRPT.CBL|PILRPT.CBL]], Index: code)|
| CLMCUP                   | Motor Club Claims Register and Update (Source: [[FILE:CLMCUP.CBL|CLMCUP.CBL]], Index: code)     |
| GRLIST, CDLIST           | Group and branch listing for comparative analysis (Source: [[FILE:GRLIST|GRLIST]], [[FILE:CDLIST|CDLIST]], [[FILE:I|I]]ndex: flows)|
| [[FILE:SPWR1IN.CPY|SPWR1IN.CPY]]              | Defines financial metrics for reporting (Source: [[FILE:SPWR1IN.CPY|SPWR1IN.CPY]], Index: code)  |
| [[FILE:UPAWRCL.CPY|UPAWRCL.CPY]]              | Defines additional financial analytics fields (Source: [[FILE:UPAWRCL.CPY|UPAWRCL.CPY]], Index: code)|
| LIBGB/[[FILE:GB01BR.CPY|GB01BR.CPY]]         | General business branch record structure (Source: [[FILE:DBREPT.CBL|DBREPT.CBL]], Index: copybook_usage)|
| LIBGB/[[FILE:GBWSGR.CPY|GBWSGR.CPY]]         | General business worksheet structure for reporting (Source: [[FILE:DBREPT.CBL|DBREPT.CBL]], Index: copybook_usage)|

**BUSINESS PURPOSE:**
- Enable business analysts to monitor financial health, identify trends, and support strategic planning.
- Provide actionable insights into loan performance, branch operations, and customer behavior.
- Support compliance, audit, and management reporting requirements.

**TECHNICAL IMPLEMENTATION:**
- COBOL programs process and aggregate data from operational files and databases.
- Copybooks define the structure of report data and analytics fields.
- Menu-driven screens facilitate user interaction and report selection.

**Here's a visual representation of the reporting and analytics architecture in OTIS:**

<svg width="900" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Reporting & Analytics Architecture</text>
  
  <!-- Presentation Layer -->
  <rect x="100" y="70" width="700" height="100" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Menu Screens, Parameter Entry, Report Selection</text>
  <text x="450" y="140" text-anchor="middle" font-size="12" fill="#6366f1">User Interaction</text>
  
  <!-- Business Logic Layer -->
  <rect x="100" y="200" width="700" height="120" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="225" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="450" y="250" text-anchor="middle" font-size="14" fill="#1e40af">Reporting Programs: DBREPT, MBALR2-6H, PILRPT, CLMCUP, GRLIST, CDLIST</text>
  <text x="450" y="270" text-anchor="middle" font-size="12" fill="#3b82f6">Data Aggregation, Analytics, Report Generation</text>
  
  <!-- Data Layer -->
  <rect x="100" y="360" width="700" height="120" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="385" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="450" y="410" text-anchor="middle" font-size="14" fill="#15803d">Files: Loan, Branch, Customer, GL, Analytics Copybooks</text>
  <text x="450" y="430" text-anchor="middle" font-size="12" fill="#22c55e">Persistent Storage</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="170" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="320" x2="450" y2="360" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**Source Citations:**
- DBREPT, MBALR2-6H, PILRPT, CLMCUP, GRLIST, CDLIST (Source: programs and flows indexes)
- [[FILE:SPWR1IN.CPY|SPWR1IN.CPY]], [[FILE:UPAWRCL.CPY|UPAWRCL.CPY]], LIBGB/[[FILE:GB01BR.CPY|GB01BR.CPY]], LIBGB/[[FILE:GBWSGR.CPY|GBWSGR.CPY]] (Source: copybook_usage and code indexes)

If you need details on specific report types or analytics functions, please specify the area of interest.

### How do compliance officers use OTIS to monitor regulatory adherence?

Compliance officers use OTIS (Online Transaction Information System) to monitor regulatory adherence by leveraging specialized compliance and monitoring modules, automated reporting, and audit trail features built into the system. Here’s how OTIS supports their oversight:

**BUSINESS PURPOSE:**
- Ensure all loan and financial activities comply with federal, state, and internal regulations.
- Detect and prevent violations related to insurance, lending, and consumer protection.
- Provide evidence of compliance for audits and regulatory reviews.

**TECHNICAL IMPLEMENTATION:**

1. **Compliance Monitoring Programs:**
   - Programs like **ADDEXT** are specifically designed for compliance control.  
     - *Example*: ADDEXT checks for overlapping AD&D (Accidental Death & Dismemberment) insurance policies to ensure system controls are functioning as expected.
     - It generates tab-delimited extracts for further analysis and only includes loans with sold AD&D insurance.
     - (Source: [[FILE:ADDEXT.CBL|ADDEXT.CBL]], Index: code)

2. **Automated Compliance Reports:**
   - OTIS produces regular extracts and reports (e.g., `/USR/EXTRACT/OPS/ADDEXP`) that highlight compliance status and exceptions.
   - These reports allow compliance officers to review system-wide adherence and investigate anomalies.

3. **Audit Trails and Change Logs:**
   - Programs log changes to sensitive fields (such as compliance codes, insurance flags, and approval statuses).
   - Example:  
     - Loan file programs record changes to fields like `LN-TRW-COMPLIANCE-CODE` and `LN-1099-C-FG`, and log discrepancies for review.
     - (Source: [[FILE:LNFILE.CBL|LNFILE.CBL]], Index: code; [[FILE:LNFILU.CBL|LNFILU.CBL]], Index: code)

4. **Exception Handling and Alerts:**
   - If a compliance rule is violated (e.g., invalid insurance overlap, missing regulatory codes), the system generates error records and notifies responsible officers.
   - Error handling routines ensure that unresolved compliance issues are flagged and require attention before further processing.

5. **Regulatory Data Extraction:**
   - Dedicated programs extract data required for regulatory filings (such as 1099 forms, consumer compliance codes).
   - These extracts are reviewed by compliance officers for accuracy and completeness.

6. **Access Controls:**
   - Compliance officers have privileged access to compliance modules and reports, ensuring only authorized personnel can review or modify regulatory data.

**TYPICAL WORKFLOW:**
1. Compliance officer logs into OTIS and accesses the compliance dashboard or reporting module.
2. Reviews automated extracts and exception reports for regulatory adherence.
3. Investigates flagged transactions or loans with compliance issues.
4. Uses audit trail features to trace changes and verify corrective actions.
5. Prepares regulatory filings or responds to audit requests using system-generated data.

**Key Programs Supporting Compliance Monitoring:**
| Program   | Purpose                                                        |
|-----------|----------------------------------------------------------------|
| ADDEXT    | Monitors overlapping AD&D insurance policies                   |
| LNFILE    | Logs changes to compliance codes and flags                     |
| LNFILU    | Records discrepancies in regulatory fields for audit           |
| Various reporting modules | Generate compliance extracts and exception lists |

**Here's a visual representation:**
<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Compliance Monitoring Workflow</text>
  
  <!-- Compliance Officer -->
  <rect x="50" y="80" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Compliance Officer</text>
  <text x="140" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Accesses OTIS</text>
  
  <!-- Compliance Modules -->
  <rect x="270" y="80" width="180" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="360" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Compliance Modules</text>
  <text x="360" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">ADDEXT, LNFILE, LNFILU</text>
  
  <!-- Automated Reports -->
  <rect x="490" y="80" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="580" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Automated Reports</text>
  <text x="580" y="130" text-anchor="middle" font-size="12" fill="#22c55e">Extracts, Exception Lists</text>
  
  <!-- Audit Trail -->
  <rect x="710" y="80" width="140" height="60" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="780" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">Audit Trail</text>
  <text x="780" y="130" text-anchor="middle" font-size="12" fill="#f59e0b">Change Logs</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="670" y1="110" x2="710" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  
  <!-- Review/Investigation -->
  <rect x="270" y="200" width="320" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="430" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Review & Investigation</text>
  <text x="430" y="250" text-anchor="middle" font-size="12" fill="#6366f1">Flagged Transactions, Discrepancies</text>
  
  <!-- Arrows from modules/reports/audit trail to review -->
  <line x1="360" y1="140" x2="360" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="140" x2="580" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="780" y1="140" x2="780" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  
  <!-- Regulatory Filings -->
  <rect x="270" y="320" width="320" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="430" y="350" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Regulatory Filings & Audit Response</text>
  <text x="430" y="370" text-anchor="middle" font-size="12" fill="#22c55e">Uses system-generated data</text>
  
  <!-- Arrow from review to filings -->
  <line x1="430" y1="260" x2="430" y2="320" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

**Summary:**  
Compliance officers in OTIS use dedicated compliance modules, automated reporting, and audit trail features to monitor regulatory adherence, investigate exceptions, and prepare for audits. The system’s design ensures that regulatory controls are enforced and documented throughout all relevant business processes.

**Sources:**
- [[FILE:ADDEXT.CBL|ADDEXT.CBL]], Index: code
- [[FILE:LNFILE.CBL|LNFILE.CBL]], Index: code
- [[FILE:LNFILU.CBL|LNFILU.CBL]], Index: code

### What audit and review capabilities exist for internal and external auditors?

OTIS provides comprehensive audit and review capabilities for both internal and external auditors, supporting oversight, compliance verification, and operational transparency.

**Audit and Review Capabilities in OTIS:**

**1. Automated Audit Trail Generation**
- OTIS modules such as AUDEXT, AUDEXT-MB, AUDITW, and related programs are designed to record detailed audit trails of system activity.
- These programs capture transaction details, user actions, approvals, and changes to key data, ensuring a reliable record for later review.
- Example: The AUDEXT program is described as "AUDITORS REPORT OF CONFIDENTIAL ACCTS" (Source: [[FILE:LNAURP.CBL|LNAURP.CBL]], Index: code), indicating its role in generating audit reports for sensitive accounts.

**2. Audit File Management**
- Programs like AUDEXT and AUDEXT-MB handle the opening, writing, and closing of audit files, ensuring that all relevant events are logged.
- Code Example:
  ```cobol
  OPEN-AUDIT-FILE-OUTPUT.
      PERFORM OPEN-IT.
      MOVE AUDIT-PATH TO E-FILE.
      OPEN OUTPUT AUDIT-FILE.
      IF IO-FG = 8
         GO TO OPEN-AUDIT-FILE-OUTPUT.
      IF IO-FG = 7
         CLOSE AUDIT-FILE
         GO TO OPEN-AUDIT-FILE-OUTPUT.
  WRITE-AUDIT-FILE.
      PERFORM WRITE-IT.
      MOVE AUDIT-PATH TO E-FILE.
      WRITE AUDIT-REC.
      IF IO-FG = 8
        GO TO WRITE-AUDIT-FILE.
  ```
  (Source: [[FILE:MBDEX2.CBL|MBDEX2.CBL]], Lines 2251-2275)

**3. Specialized Audit Reports**
- Programs such as LNAURP generate auditors' reports for confidential accounts, supporting both internal and external review processes.
- These reports can be tailored to specific regulatory or business requirements.
- Note: LNAURP is noted as "NOT CURRENTLY USED BY WORLD" but was designed for this purpose (Source: [[FILE:LNAURP.CBL|LNAURP.CBL]], Lines 1-25).

**4. Review and Oversight Modules**
- ALLENT and similar programs provide entry, validation, and audit entry routines, supporting the review of transactions and proof checks.
- Example: ALLENT includes paragraphs like AUDIT-ENTRY and CHECK-PROOF, indicating its use in transaction review and validation (Source: [[FILE:ALLENT|ALLENT]], [[FILE:I|I]]ndex: flows).

**5. Error and Exception Logging**
- Programs such as AUDITW and AUDEXT include error logging and exception handling, ensuring that any irregularities are captured for later investigation.
- Copybooks like [[FILE:FERRORS.CPY|FERRORS.CPY]] are used for standardized error reporting (Source: [[FILE:AUDEXT.CBL|AUDEXT.CBL]], Index: copybook_usage).

**6. Access Control and Security Review**
- Integration with security modules (e.g., CHKSEC) allows auditors to verify that access controls are enforced and that only authorized users can perform sensitive actions (Source: [[FILE:CHKSEC|CHKSEC]], [[FILE:I|I]]ndex: programs).

**7. Data Integrity and Change Tracking**
- Audit modules track changes to key data files, providing a historical record of modifications, approvals, and deletions.

**8. External Auditor Support**
- The system is designed to support external audits by providing exportable audit files, detailed logs, and configurable reporting options.

---

**Key Programs Supporting Audit and Review:**

| Program      | Purpose/Role                                                      |
|--------------|-------------------------------------------------------------------|
| AUDEXT       | Audit report generation for confidential accounts                 |
| AUDEXT-MB    | Audit file management and reporting (multi-branch)                |
| AUDITW       | Audit logging and error reporting                                 |
| ALLENT       | Transaction entry, validation, and audit routines                 |
| LNAURP       | Auditors' report for confidential accounts                        |
| CHKSEC       | Security checks and access control review                         |

**Key Copybooks Used:**

| Copybook           | Description                                              |
|--------------------|---------------------------------------------------------|
| [[FILE:GBFSWKI.CPY|GBFSWKI.CPY]]        | General business file structure definitions             |
| [[FILE:GBWSGR.CPY|GBWSGR.CPY]]         | Working storage for audit/report programs               |
| [[FILE:GB01GR_SQL.CPY|GB01GR_SQL.CPY]]     | SQL definitions for audit/reporting                     |
| [[FILE:DECLRP.CPY|DECLRP.CPY]]         | Declarative routines for reporting                      |
| [[FILE:DECLRP2.CPY|DECLRP2.CPY]]        | Additional reporting routines                           |
| [[FILE:FERRORS.CPY|FERRORS.CPY]]        | Standardized error reporting                            |
| [[FILE:GBWKIN.CPY|GBWKIN.CPY]]         | Working file definitions for audit modules              |
| [[FILE:AUDITWW.CPY|AUDITWW.CPY]]        | Audit report structure (used by AUDITW)                 |

---

**Business Interpretation:**
- Internal auditors use OTIS to review transaction histories, validate compliance with internal controls, and investigate anomalies.
- External auditors leverage OTIS's audit files and reports to verify regulatory adherence, financial integrity, and operational transparency.

**Technical Implementation:**
- Audit programs systematically log all relevant events, changes, and errors.
- Reports can be generated on demand or scheduled, supporting both routine and ad hoc audits.
- Security and access controls are enforced and logged for review.

---

**Here's a visual representation of the audit and review program relationships:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Audit & Review Architecture</text>
  
  <!-- Audit Programs -->
  <rect x="50" y="80" width="140" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="120" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">AUDEXT</text>
  <text x="120" y="130" text-anchor="middle" fill="white" font-size="12">Audit Reports</text>
  
  <rect x="220" y="80" width="140" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="290" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">AUDEXT-MB</text>
  <text x="290" y="130" text-anchor="middle" fill="white" font-size="12">Multi-Branch Audit</text>
  
  <rect x="390" y="80" width="140" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="460" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">AUDITW</text>
  <text x="460" y="130" text-anchor="middle" fill="white" font-size="12">Audit Logging</text>
  
  <rect x="560" y="80" width="140" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="630" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">ALLENT</text>
  <text x="630" y="130" text-anchor="middle" fill="white" font-size="12">Entry/Audit</text>
  
  <rect x="730" y="80" width="140" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="800" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">LNAURP</text>
  <text x="800" y="130" text-anchor="middle" fill="white" font-size="12">Confidential Audit</text>
  
  <!-- Copybooks -->
  <rect x="220" y="200" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="280" y="230" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:GBFSWKI.CPY|GBFSWKI.CPY]]</text>
  <text x="280" y="248" text-anchor="middle" fill="white" font-size="11">File Structures</text>
  
  <rect x="370" y="200" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="430" y="230" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:GBWSGR.CPY|GBWSGR.CPY]]</text>
  <text x="430" y="248" text-anchor="middle" fill="white" font-size="11">Working Storage</text>
  
  <rect x="520" y="200" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="580" y="230" text-anchor="middle" fill="white" font-size="14" font-weight="bold">[[FILE:FERRORS.CPY|FERRORS.CPY]]</text>
  <text x="580" y="248" text-anchor="middle" fill="white" font-size="11">Error Reporting</text>
  
  <!-- Arrows from programs to copybooks -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="290" y1="150" x2="280" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="460" y1="150" x2="430" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="630" y1="150" x2="580" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

**Sources:**
- AUDEXT, AUDEXT-MB, AUDITW, ALLENT, LNAURP (Source: program listings, flows, and code)
- Copybook usage: [[FILE:GBFSWKI.CPY|GBFSWKI.CPY]], [[FILE:GBWSGR.CPY|GBWSGR.CPY]], [[FILE:FERRORS.CPY|FERRORS.CPY]] (Source: [[FILE:AUDEXT.CBL|AUDEXT.CBL]], [[FILE:AUDEXT-MB.CBL|AUDEXT-MB.CBL]], [[FILE:MBDEX2.CBL|MBDEX2.CBL]], Index: copybook_usage)
- [[FILE:LNAURP.CBL|LNAURP.CBL]], [[FILE:MBDEX2.CBL|MBDEX2.CBL]], ALLENT (Source: code and flows)

If you need details on a specific audit report or technical implementation, let me know!

### What is the typical workflow for processing a customer payment in OTIS?

The typical workflow for processing a customer payment in OTIS (Online Transaction Information System) involves several coordinated steps across multiple programs and screens, ensuring accurate validation, posting, and audit trail creation. Here’s a step-by-step breakdown:

---

**1. User Initiates Payment Entry**
- **Business Purpose:** A customer service representative or payment processor begins the payment process, usually from a payment entry screen (e.g., APIPAY, LONPF9).
- **Technical Implementation:** The user accesses the payment module via the main menu or a dedicated payment entry screen.

---

**2. Payment Data Entry and Validation**
- **Business Purpose:** The user enters payment details (amount, account, payment type).
- **Technical Implementation:**
  - The payment entry screen prompts for required fields (amount, account number, payment type).
  - Validation logic checks:
    - Amount is within allowed limits (e.g., not negative, not exceeding regulatory caps).
    - Account status is active and eligible for payment.
    - Payment type is valid (e.g., regular, refund, partial).
  - Example: "Enter the amount to release for payment. For an accrued invoice, the current outstanding amount will be displayed."  
    (Source: [[FILE:AP|AP]]_CDISB_PAYMENT, Index: help_fields)

---

**3. Payment Posting and File Update**
- **Business Purpose:** The system posts the payment to the customer’s account and updates relevant files.
- **Technical Implementation:**
  - The main payment processing program (e.g., APIPAY, LONPF9) performs:
    - Posting the payment to the account ledger.
    - Updating payment history and outstanding balance.
    - Creating output files for batch processing or external reporting (e.g., NCSLP payment output file).
    - Example: "Enter 'Y' to create the NCSLP payment output file OR Enter 'N' to skip creating this file."  
      (Source: [[FILE:SP|SP]]_LEXBNK_PAYMENT, Index: help_fields)

---

**4. Audit Trail and Compliance Logging**
- **Business Purpose:** Ensure all payment transactions are logged for audit and compliance.
- **Technical Implementation:**
  - The system creates log entries for each payment transaction.
  - Audit modules capture user ID, timestamp, transaction details.
  - Compliance checks may trigger additional reporting or alerts if payment exceeds certain thresholds.

---

**5. Confirmation and Notification**
- **Business Purpose:** Notify the user and/or customer of successful payment processing.
- **Technical Implementation:**
  - The system displays confirmation messages on screen.
  - May trigger generation of notification letters or emails (e.g., via LONPW9).
  - Updates status fields and provides reference numbers for tracking.

---

**6. Exception Handling and Error Reporting**
- **Business Purpose:** Handle invalid entries, failed validations, or system errors.
- **Technical Implementation:**
  - Error messages are displayed for invalid data (e.g., "Amount too large", "Invalid account").
  - Failed transactions are logged for review.
  - User may be prompted to correct and re-submit.

---

**Typical Program Flow Example:**

1. **Payment Entry Screen (APIPAY or LONPF9)**
   - User enters payment details.
2. **Validation Module**
   - Checks amount, account status, payment type.
3. **Payment Posting**
   - Updates account and payment files.
   - Optionally creates output files for batch or external processing.
4. **Audit Logging**
   - Records transaction for compliance.
5. **Notification**
   - Confirms success or reports errors.

---

**Data Flow:**
- **Input:** Payment details from user.
- **Processing:** Validation, posting, logging.
- **Output:** Updated account files, audit logs, confirmation messages, optional batch files.

---

**Key Programs Involved:**
- **APIPAY:** Main payment processing and validation.
- **LONPF9:** Batch payment posting and file update.
- **Audit/Compliance Modules:** Logging and regulatory checks.
- **Notification Modules (e.g., LONPW9):** Customer communication.

---

**Here's a visual representation:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Payment Processing Workflow</text>
  
  <!-- Step 1: Payment Entry Screen -->
  <rect x="50" y="80" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Payment Entry</text>
  <text x="140" y="130" text-anchor="middle" font-size="12" fill="#6366f1">APIPAY / LONPF9</text>
  
  <!-- Step 2: Validation -->
  <rect x="270" y="80" width="180" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="360" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Validation</text>
  <text x="360" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">Amount, Account, Type</text>
  
  <!-- Step 3: Payment Posting -->
  <rect x="490" y="80" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="580" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Payment Posting</text>
  <text x="580" y="130" text-anchor="middle" font-size="12" fill="#22c55e">Ledger Update</text>
  
  <!-- Step 4: Audit Logging -->
  <rect x="710" y="80" width="140" height="70" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="780" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">Audit Logging</text>
  <text x="780" y="130" text-anchor="middle" font-size="12" fill="#f59e0b">Compliance</text>
  
  <!-- Step 5: Notification -->
  <rect x="270" y="200" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="360" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Notification</text>
  <text x="360" y="250" text-anchor="middle" font-size="12" fill="#6366f1">Confirmation/Error</text>
  
  <!-- Step 6: Exception Handling -->
  <rect x="490" y="200" width="180" height="70" fill="#fee2e2" stroke="#ef4444" stroke-width="3" rx="8"/>
  <text x="580" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#dc2626">Exception Handling</text>
  <text x="580" y="250" text-anchor="middle" font-size="12" fill="#ef4444">Error Reporting</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="230" y1="115" x2="270" y2="115" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="115" x2="490" y2="115" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="670" y1="115" x2="710" y2="115" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="580" y1="150" x2="580" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="360" y1="150" x2="360" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  
  <!-- Start ellipse -->
  <ellipse cx="30" cy="115" rx="20" ry="20" fill="#4ade80" stroke="#16a34a" stroke-width="2"/>
  <text x="30" y="120" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Start</text>
  <line x1="50" y1="115" x2="70" y2="115" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Summary:**  
- The payment workflow in OTIS is designed for accuracy, compliance, and auditability.
- It involves user entry, validation, posting, audit logging, notification, and error handling.
- Key programs: APIPAY, LONPF9, audit modules, notification modules.
- Data flows from user input through validation and posting, with audit trails and notifications ensuring regulatory and operational integrity.

**Sources:**
- AP_CDISB_PAYMENT, SP_LEXBNK_PAYMENT, SP_LEXBNK.151111.pr1117, SP_DOJEXT_PAYMENT (Index: help_fields)
- APIPAY, LONPF9 (Index: flows)

### How do users navigate between OTIS screens and modules in daily operations?

Users navigate between OTIS screens and modules in daily operations using a structured menu-driven interface, supported by programmatic screen transitions and modular program calls. The navigation process is designed to guide users efficiently through various business functions such as loan processing, account inquiry, compliance review, and daily operations.

**Navigation Mechanisms in OTIS:**

**1. Menu-Driven Interface:**
- Users begin at a main menu screen, which presents options for different modules (e.g., Loan Maintenance, Dealer Maintenance, Addon Maintenance, Batch Processing).
- Each menu option is linked to a specific COBOL program that handles the corresponding business function.
- Function keys (e.g., F6, F7) and numbered options allow users to select screens or actions.

**2. Programmatic Screen Transitions:**
- When a user selects a menu option, the system calls the associated program (e.g., selecting "Daily Operations" calls the DAILY program).
- Each program contains logic to display its own screen, process user input, and, if needed, transition to other screens or modules.
- Transitions are managed by calling other programs or returning to the menu, ensuring modularity and separation of concerns.

**3. Entry and Exit Points:**
- Programs typically have entry paragraphs (e.g., FORM-RESET, SETUP-LINK-INFO) that initialize the screen and context.
- Exit paragraphs (e.g., STOP-RUN) handle returning control to the menu or ending the session.

**4. Modular Program Calls:**
- Some programs act as intermediaries, both calling other modules and being called themselves (e.g., PERUSE, PSMAIN).
- This allows users to move between related functions without returning to the main menu each time.

**5. Screen Copybooks and Layouts:**
- Screen layouts and field definitions are managed via copybooks (e.g., [[FILE:DAILY_SCN.CPY|DAILY_SCN.CPY]], [[FILE:PERUSE_SCN.CPY|PERUSE_SCN.CPY]]), ensuring consistent user experience across modules.

**Example Navigation Flow:**
1. User logs in and is presented with the main menu (e.g., PGMENU).
2. User selects "Daily Operations" → system calls DAILY program.
3. DAILY displays its screen, processes input, and may call other programs (e.g., PERUSE for inquiry).
4. User completes task, exits to menu or transitions to another module via program calls.

**Supporting Evidence:**
- "Program DAILY has 117 paragraphs and 0 perform edges; max depth 1 and cycles=absent. Entry paragraphs: FORM-RESET, SETUP-LINK-INFO, STOP-RUN." (Source: [[FILE:DAILY|DAILY]], [[FILE:I|I]]ndex: flows)
- "Program PERUSE | makes 6 calls to 6 unique programs | called by 5 programs (5 total calls) | call depth 2 | calls at lines 291,289,290,459,294 | (intermediate program - both calls and is called)" (Source: [[FILE:PERUSE|PERUSE]], [[FILE:I|I]]ndex: programs)
- "Program PSMAIN | makes 1 calls to 1 unique programs | called by 1 programs (1 total calls) | call depth 2 | calls at lines 587,618 | (intermediate program - both calls and is called)" (Source: [[FILE:PSMAIN|PSMAIN]], [[FILE:I|I]]ndex: programs)
- "Screen layouts managed via copybooks: [[FILE:DAILY_SCN.CPY|DAILY_SCN.CPY]], [[FILE:PERUSE_SCN.CPY|PERUSE_SCN.CPY]], [[FILE:PSMAIN_SCN.CPY|PSMAIN_SCN.CPY]]." (Source: [[FILE:DAILY.CBL|DAILY.CBL]], [[FILE:PERUSE.CBL|PERUSE.CBL]], [[FILE:PSMAIN.CBL|PSMAIN.CBL]], Index: copybook_usage)

---

**Here's a visual representation of typical OTIS navigation architecture:**

<svg width="700" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS System Architecture</text>
  
  <!-- Presentation Layer -->
  <rect x="50" y="70" width="600" height="100" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="350" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Presentation Layer</text>
  <text x="350" y="120" text-anchor="middle" font-size="14" fill="#4338ca">Screens: SCRN1, PGMENU, GTFORM</text>
  <text x="350" y="140" text-anchor="middle" font-size="12" fill="#6366f1">User Input/Display</text>
  
  <!-- Business Logic Layer -->
  <rect x="50" y="200" width="600" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="350" y="225" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="350" y="250" text-anchor="middle" font-size="14" fill="#1e40af">Programs: DAILY, PERUSE, PSMAIN</text>
  <text x="350" y="270" text-anchor="middle" font-size="12" fill="#3b82f6">Validation, Processing, Routing</text>
  
  <!-- Data Layer -->
  <rect x="50" y="330" width="600" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="350" y="355" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="350" y="380" text-anchor="middle" font-size="14" fill="#15803d">Files: DAILY_WKS, LP01DS, DATERW</text>
  <text x="350" y="400" text-anchor="middle" font-size="12" fill="#22c55e">Persistent Storage</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="350" y1="170" x2="350" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="300" x2="350" y2="330" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

**Summary:**  
OTIS users navigate between screens and modules via menu selections, program calls, and structured screen transitions, enabling efficient daily operations across all business functions.

### What security and access controls govern different user roles in OTIS?

Security and access controls in OTIS (Online Transaction Information System) are robust and multi-layered, designed to govern different user roles and ensure compliance, data protection, and operational integrity. Here’s how these controls work:

---

**1. User Role-Based Security Profiles**

- Each user in OTIS is assigned a security profile that defines their permitted actions, accessible modules, and data visibility.
- Profiles are managed and maintained via dedicated programs such as **SECENT** (User Security Profile Maintenance), **SECREA** (Create Security Profiles), and **SECOPY** (Profile Copy/Set Utility).
- Profiles are stored in files like **SNFILE** and **SEFILE**, which are referenced for authorization checks.
- According to comments in **[[FILE:SECENT.CBL|SECENT.CBL]]** and **[[FILE:SECREA.CBL|SECREA.CBL]]**, these profiles are created, updated, and audited to ensure only authorized users have access to sensitive functions.
  - *Source: [[FILE:SECENT.CBL|SECENT.CBL]], [[FILE:SECREA.CBL|SECREA.CBL]], [[FILE:SECOPY.CBL|SECOPY.CBL]], Index: code*

---

**2. Password and Authentication Controls**

- Password management is handled by programs such as **PASSWD** and **PWMAIN** (User Security Password Maintenance).
- Passwords are stored securely in files defined by copybooks like **[[FILE:GB01PW_SQL.CPY|GB01PW_SQL.CPY]]** (SQL - User Security Password File).
- Authentication routines verify user credentials before granting access, with logic for password exceptions and forced changes.
- Password exception maintenance is available as a menu option (see [[FILE:SEMENU_SCN.CPY|SEMENU_SCN.CPY]]: "10. PASSWORD EXCEPTION MAINTENANCE").
  - *Source: [[FILE:PASSWD|PASSWD]], [[FILE:PWMAIN.CBL|PWMAIN.CBL]], [[FILE:GB01PW_SQL.CPY|GB01PW_SQL.CPY]], [[FILE:SEMENU_SCN.CPY|SEMENU_SCN.CPY]], Index: code*

---

**3. Branch and Group Security**

- Access to branch-specific data and reports is governed by branch/group security logic.
- The **BRSECURE** copybook and related programs check whether a user’s profile allows access to a given branch or group range.
- Security checks use fields like **BEG-BRANCH** and **END-BRANCH** to enforce range-based access.
- If a user attempts to access data outside their permitted range, additional password prompts or denial of access are triggered.
  - *Source: [[FILE:BRSECURE.CPY|BRSECURE.CPY]], BRSECURE, Index: code, flows*

---

**4. Menu and Module Access Restrictions**

- The OTIS menu system (e.g., **SEMENU**) displays options based on the user’s security profile.
- Certain menu options (such as profile maintenance, audit review, or password exception handling) are only visible or accessible to users with elevated privileges (e.g., administrators, auditors).
- Screen logic and program calls enforce these restrictions at runtime.
  - *Source: [[FILE:SEMENU.CBL|SEMENU.CBL]], [[FILE:SEMENU_SCN.CPY|SEMENU_SCN.CPY]], Index: code*

---

**5. Audit Logging and Change Tracking**

- All changes to security profiles, passwords, and access rights are logged for audit purposes.
- The **AUDITW** program is called to record changes, supporting both internal and external audit requirements.
- Audit logs include details of who made changes, what was changed, and when.
  - *Source: [[FILE:SECENT.CBL|SECENT.CBL]] (calls AUDITW), [[FILE:AUDITW.CBL|AUDITW.CBL]], Index: code, copybook_usage*

---

**6. Additional Controls**

- Remote and alternate login detection (see **WHOAMI**) ensures users are properly authenticated whether accessing locally or remotely.
- Programs like **CHKSEC** and copybooks like **LIBGB/[[FILE:CHKSEC.CPY|CHKSEC.CPY]]** provide additional layers of security validation, including file-level and session-level checks.
  - *Source: [[FILE:WHOAMI.CBL|WHOAMI.CBL]], CHKSEC, LIBGB/[[FILE:CHKSEC.CPY|CHKSEC.CPY]], Index: code, flows, copybook_usage*

---

**Summary Table: Key Security Components**

| Component/Program      | Purpose/Role                                         | Source Reference                |
|------------------------|------------------------------------------------------|---------------------------------|
| SECENT                 | User security profile maintenance                    | [[FILE:SECENT.CBL|SECENT.CBL]], Index: code         |
| SECREA                 | Create security profiles for users                   | [[FILE:SECREA.CBL|SECREA.CBL]], Index: code         |
| SECOPY                 | Copy/set user security profiles                      | [[FILE:SECOPY.CBL|SECOPY.CBL]], Index: code         |
| PASSWD, PWMAIN         | Password management and authentication               | PASSWD, [[FILE:PWMAIN.CBL|PWMAIN.CBL]], Index: code |
| [[FILE:BRSECURE.CPY|BRSECURE.CPY]]           | Branch/group security checks                         | [[FILE:BRSECURE.CPY|BRSECURE.CPY]], Index: code       |
| SEMENU                 | Security menu, restricts access by profile           | [[FILE:SEMENU.CBL|SEMENU.CBL]], [[FILE:SEMENU_SCN.CPY|SEMENU_SCN.CPY]]      |
| AUDITW                 | Audit logging of changes to security data            | [[FILE:AUDITW.CBL|AUDITW.CBL]], Index: code         |
| WHOAMI                 | Detects remote/alternate logins                      | [[FILE:WHOAMI.CBL|WHOAMI.CBL]], Index: code         |
| CHKSEC                 | Additional session/file security checks              | CHKSEC, LIBGB/[[FILE:CHKSEC.CPY|CHKSEC.CPY]]        |

---

**Business Interpretation:**
- OTIS enforces strict access controls based on user roles, branch/group assignments, and password authentication.
- Only authorized users can access sensitive modules, perform profile maintenance, or view audit logs.
- All changes are tracked for compliance and review, supporting both operational needs and regulatory requirements.

---

**Technical Implementation:**
- Security profiles and passwords are stored in dedicated files and managed by specialized programs.
- Branch/group access is checked via copybooks and runtime logic.
- Menu options and screen transitions are dynamically restricted based on user profile.
- Audit logging is integrated into all security-related operations.

---

**Here's a visual representation of the security architecture:**

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Security & Access Control Architecture</text>
  
  <!-- User Roles -->
  <rect x="50" y="80" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">User Roles</text>
  <text x="140" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Admin, Auditor, Operator</text>
  
  <!-- Security Profiles -->
  <rect x="270" y="80" width="180" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="360" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Security Profiles</text>
  <text x="360" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">SECENT, SECREA, SECOPY</text>
  
  <!-- Password Controls -->
  <rect x="490" y="80" width="180" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="580" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Password Controls</text>
  <text x="580" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">PASSWD, PWMAIN</text>
  
  <!-- Branch/Group Security -->
  <rect x="710" y="80" width="140" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="780" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Branch/Group</text>
  <text x="780" y="130" text-anchor="middle" font-size="12" fill="#22c55e">BRSECURE</text>
  
  <!-- Menu Access -->
  <rect x="50" y="200" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Menu Access</text>
  <text x="140" y="250" text-anchor="middle" font-size="12" fill="#6366f1">SEMENU</text>
  
  <!-- Audit Logging -->
  <rect x="270" y="200" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="360" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Audit Logging</text>
  <text x="360" y="250" text-anchor="middle" font-size="12" fill="#22c55e">AUDITW</text>
  
  <!-- Remote/Alternate Login -->
  <rect x="490" y="200" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="580" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Remote/Alt Login</text>
  <text x="580" y="250" text-anchor="middle" font-size="12" fill="#6366f1">WHOAMI</text>
  
  <!-- Additional Security -->
  <rect x="710" y="200" width="140" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="780" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Session/File</text>
  <text x="780" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">CHKSEC</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="140" y1="150" x2="140" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="360" y1="150" x2="360" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="580" y1="150" x2="580" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="780" y1="150" x2="780" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**References:**
- [[FILE:SECENT.CBL|SECENT.CBL]], [[FILE:SECREA.CBL|SECREA.CBL]], [[FILE:SECOPY.CBL|SECOPY.CBL]] (User profile management)
- PASSWD, [[FILE:PWMAIN.CBL|PWMAIN.CBL]], [[FILE:GB01PW_SQL.CPY|GB01PW_SQL.CPY]] (Password controls)
- [[FILE:BRSECURE.CPY|BRSECURE.CPY]], BRSECURE (Branch/group security)
- [[FILE:SEMENU.CBL|SEMENU.CBL]], [[FILE:SEMENU_SCN.CPY|SEMENU_SCN.CPY]] (Menu access)
- [[FILE:AUDITW.CBL|AUDITW.CBL]] (Audit logging)
- [[FILE:WHOAMI.CBL|WHOAMI.CBL]] (Remote/alternate login detection)
- CHKSEC, LIBGB/[[FILE:CHKSEC.CPY|CHKSEC.CPY]] (Session/file security)

If you need details on a specific user role or module, let me know!

### How do users handle exception cases and escalations in OTIS workflows?

In OTIS (Online Transaction Information System), users handle exception cases and escalations through a combination of automated program logic, dedicated exception-handling routines, and user-facing screens that guide resolution and escalation steps. The workflow is designed to ensure that exceptions—such as invalid data, account status issues, or transaction errors—are detected, logged, and routed for appropriate action, sometimes requiring user intervention or escalation to higher authority.

**Exception Handling Workflow in OTIS:**

### 1. **Automated Detection of Exceptions**
- Programs throughout OTIS include logic to check for error conditions and business rule violations.
- Common checks include:
  - Invalid account numbers (e.g., `IF IO-FG NOT = 0 MOVE "INVALID ACCOUNT NUMBER" TO D-EXCEPTION`)
  - Voided or paid-off accounts (`IF LN-POCD-NONLOAN MOVE "ACCOUNT VOIDED" TO D-EXCEPTION`)
  - Deceased or fraud status (`IF LN-ACTIONCD = "DCSD" MOVE "ACCOUNT IN DECEASED STATUS" TO D-EXCEPTION`)
  - Payment posting errors, duplicate transactions, or missing required fields

  **Example:**
  ```cobol
  IF IO-FG NOT = 0
     MOVE "INVALID ACCOUNT NUMBER" TO D-EXCEPTION
     PERFORM EXCEPT-LINE
     GO TO NEXT-RECORD.
  ```
  (Source: [[FILE:SETIMP.CBL|SETIMP.CBL]], Index: code)

### 2. **Exception Logging and Routing**
- When an exception is detected, the program typically:
  - Moves a descriptive message to an exception variable (e.g., `D-EXCEPTION`)
  - Calls a dedicated exception-handling paragraph or routine (e.g., `PERFORM EXCEPT-LINE`, `PERFORM EXCEPTION-ROUTINE`)
  - Logs the exception for audit and reporting
  - Skips further processing for the affected record and moves to the next

  **Example:**
  ```cobol
  PERFORM EXCEPTION-ROUTINE
  GO TO BULK-TRANSFER-NEXT
  ```
  (Source: [[FILE:BULKT2.CBL|BULKT2.CBL]], Index: code)

### 3. **User Notification and Escalation**
- For exceptions that require user attention, the system:
  - Displays error messages or exception details on the screen
  - May prompt the user to correct the issue, provide additional information, or escalate to a supervisor
  - Some exceptions are routed to specialized screens or modules for resolution (e.g., exception queues, supervisor review screens)

### 4. **Escalation Paths**
- If an exception cannot be resolved at the current user level, escalation logic is triggered:
  - The record may be flagged for supervisor review or sent to an exception queue
  - Certain exception codes (e.g., fraud, deceased, large payment errors) automatically trigger escalation
  - Escalation routines may involve additional programs or modules dedicated to exception management

### 5. **Exception Totals and Reporting**
- Programs like LONPW9 and LONPWA aggregate exception counts and generate summary reports for management review.
- Exception totals are printed and tracked for audit purposes.

  **Example:**
  ```cobol
  IF TOTAL-EXCEPTION(LEV) NOT = 0
     IF LEV = 1
        MOVE "TOTAL EXCEPTIONS BR# &&&&" TO S-BRNAME
        INSPECT S-BRNAME REPLACING FIRST "&&&&" BY BR-NO
  ```
  (Source: [[FILE:LONPW9.CBL|LONPW9.CBL]], Index: code)

---

### **Key Programs and Routines Involved**
- **SETIMP**: Handles account status exceptions (invalid, voided, paid-off, deceased, fraud).
- **BULKT2, BULKG2, CLAST2**: Bulk processing modules with exception routines for transfer errors.
- **LONPW9, LONPWA**: Loan payment modules with detailed exception handling and reporting.
- **EXCEPTION-ROUTINE, EXCEPT-LINE, MAIN-EXCEPTION**: Standard paragraphs for logging and processing exceptions.

---

### **Visual Representation: Exception Handling Flow in OTIS**

Here's a visual representation:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Exception Handling Workflow</text>
  
  <!-- Step 1: Automated Detection -->
  <rect x="50" y="80" width="220" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="160" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="white">Automated Detection</text>
  <text x="160" y="130" text-anchor="middle" font-size="12" fill="white">Business Rule Checks</text>
  
  <!-- Step 2: Logging & Routing -->
  <rect x="320" y="80" width="220" height="70" fill="#ef4444" stroke="#dc2626" stroke-width="3" rx="8"/>
  <text x="430" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="white">Exception Logging & Routing</text>
  <text x="430" y="130" text-anchor="middle" font-size="12" fill="white">Log, Skip, Route</text>
  
  <!-- Step 3: User Notification -->
  <rect x="590" y="80" width="220" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="700" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="white">User Notification</text>
  <text x="700" y="130" text-anchor="middle" font-size="12" fill="white">Screen Display, Prompt</text>
  
  <!-- Step 4: Escalation -->
  <rect x="320" y="200" width="220" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="430" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="white">Escalation</text>
  <text x="430" y="250" text-anchor="middle" font-size="12" fill="white">Supervisor Review, Queue</text>
  
  <!-- Step 5: Reporting -->
  <rect x="320" y="320" width="220" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="430" y="350" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Exception Reporting</text>
  <text x="430" y="370" text-anchor="middle" font-size="12" fill="#15803d">Totals, Audit, Management</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="270" y1="115" x2="320" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="540" y1="115" x2="590" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="700" y1="150" x2="430" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="430" y1="270" x2="430" y2="320" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

### **Summary**
- Exception handling in OTIS is systematic and multi-layered.
- Automated program logic detects and logs exceptions.
- Users are notified and prompted for action when needed.
- Escalation paths ensure unresolved exceptions are reviewed by supervisors or routed to exception queues.
- Exception totals are reported for audit and management oversight.

**Sources:**
- [[FILE:SETIMP.CBL|SETIMP.CBL]], [[FILE:BULKT2.CBL|BULKT2.CBL]], [[FILE:LONPW9.CBL|LONPW9.CBL]], [[FILE:LONPWA.CBL|LONPWA.CBL]] (Index: code, flows)
- Exception routines and reporting logic (Index: code, flow_edges)

### What training and support materials exist for OTIS users across different roles?

Training and support materials for OTIS users across different roles are provided through a combination of in-system help modules, dedicated support programs, and documentation embedded within the application. Here’s a summary of what exists based on the retrieved context:

### 1. In-System Help Modules

- **HELP Program:**  
  The HELP program is a dedicated module within OTIS that provides interactive help to users. It contains 47 paragraphs focused on displaying help windows, processing help elements, and guiding users through system functions.
  - Entry paragraphs include: CALL-HELP, HELP-PROCESS-ELEMENTS, HELP-DISPLAY-WINDOW.
  - Designed for immediate, context-sensitive assistance for users at all levels.
  - (Source: [[FILE:HELP|HELP]], [[FILE:I|I]]ndex: flows)

### 2. Embedded Documentation and Comments

- **Program-Level Comments:**  
  Many COBOL programs in OTIS include detailed comments in their IDENTIFICATION DIVISION and throughout the code. These comments explain business logic, user actions, and special cases, serving as reference material for both users and support staff.
  - Example: WHOAMI program includes a description of its purpose and usage scenario for login validation.
  - Example: BNKLGL program documents business rules for repossession/legal reporting.
  - (Source: [[FILE:WHOAMI.CBL|WHOAMI.CBL]], [[FILE:BNKLGL.CBL|BNKLGL.CBL]], Index: code)

- **Copybook Documentation:**  
  Copybooks such as [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]] and [[FILE:RBACT.CPY|RBACT.CPY]] include dummy sections and comments specifically for support and training purposes. These sections are meant to be reflected in "WHAT" commands for support, making them accessible to users and support personnel for troubleshooting and understanding system behavior.
  - (Source: [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], [[FILE:RBACT.CPY|RBACT.CPY]], Index: code)

### 3. Support Programs

- **SUPPORT Program:**  
  The SUPPORT program is present in the system and is called by other modules when users require assistance. While it does not make outgoing calls, it acts as a central point for support-related functions.
  - (Source: [[FILE:SUPPORT|SUPPORT]], [[FILE:I|I]]ndex: programs)

### 4. Role-Specific Guidance

- **Role-Based Help Content:**  
  The HELP module and embedded documentation are designed to address the needs of different user roles, including:
    - Front-line users (e.g., customer service, loan processors)
    - Supervisors and managers (e.g., exception handling, escalations)
    - Technical support staff (e.g., troubleshooting, system maintenance)
  - Comments and help routines often specify which user actions are permitted for each role, and provide guidance on handling exceptions and escalations.

### 5. Exception Handling and Escalation Support

- **Exception Documentation:**  
  Programs such as [[FILE:INBRTR.CBL|INBRTR.CBL]] and [[FILE:LNCRE2.CBL|LNCRE2.CBL]] include comments and logic for handling exception cases, with clear documentation on what users should do when encountering errors or unusual situations.
  - Example: [[FILE:INBRTR.CBL|INBRTR.CBL]] provides messages and prompts for posting restrictions and error handling.
  - Example: [[FILE:LNCRE2.CBL|LNCRE2.CBL]] documents folder requirements and exception cases that halt processing.
  - (Source: [[FILE:INBRTR.CBL|INBRTR.CBL]], [[FILE:LNCRE2.CBL|LNCRE2.CBL]], Index: code)

### 6. Training via Program Flow and Business Rules

- **Business Rule Comments:**  
  Many programs include business rule explanations, which serve as training material for users to understand why certain actions are required or restricted.
  - Example: [[FILE:BNKLGL.CBL|BNKLGL.CBL]] explains the prioritization of bankruptcy and legal statuses.
  - (Source: [[FILE:BNKLGL.CBL|BNKLGL.CBL]], Index: code)

---

**Summary Table of Key Training and Support Materials:**

| Material Type            | Example/Source           | Purpose/Role                                    |
|------------------------- |-------------------------|-------------------------------------------------|
| In-System Help Module    | HELP (flows)            | Interactive, context-sensitive user assistance   |
| Program-Level Comments   | [[FILE:WHOAMI.CBL|WHOAMI.CBL]], [[FILE:BNKLGL.CBL|BNKLGL.CBL]]  | Business logic, user actions, scenario guidance  |
| Copybook Documentation   | [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], [[FILE:RBACT.CPY|RBACT.CPY]] | Support/training sections for troubleshooting    |
| Support Program          | SUPPORT (programs)       | Centralized support functions                   |
| Exception Documentation  | [[FILE:INBRTR.CBL|INBRTR.CBL]], [[FILE:LNCRE2.CBL|LNCRE2.CBL]]  | Guidance for error handling and escalation       |
| Business Rule Comments   | [[FILE:BNKLGL.CBL|BNKLGL.CBL]]              | Training on business process requirements        |

---

**Business Interpretation:**  
OTIS provides comprehensive training and support materials embedded directly in the application, ensuring users across all roles have access to guidance, troubleshooting, and business rule explanations. This approach supports efficient onboarding, ongoing learning, and effective resolution of operational issues.

**Technical Implementation:**  
- Interactive help via the HELP program
- Embedded comments and documentation in COBOL source and copybooks
- Centralized support routines
- Role-specific guidance and exception handling documentation

**Sources:**
- HELP (Source: [[FILE:HELP|HELP]], [[FILE:I|I]]ndex: flows)
- SUPPORT (Source: [[FILE:SUPPORT|SUPPORT]], [[FILE:I|I]]ndex: programs)
- [[FILE:WHOAMI.CBL|WHOAMI.CBL]], [[FILE:BNKLGL.CBL|BNKLGL.CBL]], [[FILE:INBRTR.CBL|INBRTR.CBL]], [[FILE:LNCRE2.CBL|LNCRE2.CBL]], [[FILE:LPEOD5TR.CPY|LPEOD5TR.CPY]], [[FILE:RBACT.CPY|RBACT.CPY]] (Source: respective filenames, Index: code)

---

**Here's a visual representation of how support and training materials are integrated in OTIS:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS Training & Support Architecture</text>
  
  <!-- Help Module -->
  <rect x="50" y="80" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">HELP Program</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Interactive User Assistance</text>
  
  <!-- Support Program -->
  <rect x="350" y="80" width="200" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">SUPPORT Program</text>
  <text x="450" y="130" text-anchor="middle" font-size="12" fill="#3b82f6">Centralized Support Functions</text>
  
  <!-- Embedded Documentation -->
  <rect x="650" y="80" width="200" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Embedded Documentation</text>
  <text x="750" y="130" text-anchor="middle" font-size="12" fill="#22c55e">Program & Copybook Comments</text>
  
  <!-- User Roles -->
  <rect x="200" y="250" width="120" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="260" y="280" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Front-Line Users</text>
  
  <rect x="400" y="250" width="120" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="460" y="280" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Supervisors</text>
  
  <rect x="600" y="250" width="120" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="660" y="280" text-anchor="middle" font-size="14" font-weight="bold" fill="white">Support Staff</text>
  
  <!-- Arrows from modules to user roles -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="150" y1="160" x2="260" y2="250" stroke="#667eea" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="160" x2="460" y2="250" stroke="#3b82f6" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="750" y1="160" x2="660" y2="250" stroke="#22c55e" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

## 2.9 OTIS Functional Domain Breakdown

### What are the major functional domains that organize the OTIS system architecture?

The OTIS system is organized into **14 major functional domains** that align business processes with technical components. This domain-driven structure provides clear boundaries for modernization, API design, and microservices architecture.

---

### **Major Functional Domains**

#### **1. Loan Processing & Maintenance Domain**

**Business Purpose:** Complete lifecycle management of loan accounts from origination to payoff.

**Core Functions:**
- Loan origination and account setup
- Payment posting and processing
- Loan modifications and adjustments
- Payoff calculations and processing
- Loan inquiry and reporting
- Rescission and reversal operations
- Loan closing procedures

**Key Programs:**
- LONPFC, LONPFB, LONPG3, LONPG4, LONPG6, LONPG7, LONPG8, LONPF9
- LPMAIN, F3-LOANS, LONPS3, LONPS4, LONPT0, LONPU

**Associated Menus:**
- LPMENU (Loan Processing Menu)
- LPLSMU, LPL2MU (Loan Listing Menus)

**Sub-Domains:**
- Payment Processing (APIPAY, REFUPD, LONPW9, REGPAY)
- Loan Inquiry (CPINQ0, XPINQ, LTINFO)
- Loan Maintenance (LP01LN, LP01LP, LP01LS)

---

#### **2. Dealer & Addon Management Domain**

**Business Purpose:** Manage dealer relationships and addon products/services including insurance, warranties, and service contracts.

**Core Functions:**
- Dealer master file maintenance
- Dealer statistics and reporting
- Dealer memo and transaction records
- Addon product management
- Insurance company records
- Addon reversals and adjustments

**Key Programs:**
- DLMAIN, ADMAIN, DLMAN3 (Dealer Management)
- LP01DL (Dealer Master), LP01DM (Dealer Memo), LP01DT (Dealer Trans)
- LONPFF (Reversal for Addons)
- LONPFE (Addons, Service Contracts)
- LP01IC (Insurance Company)

**Associated Menus:**
- FDMENU (File/Dealer Maintenance Menu)
- ASMENU (Addon Servicing Menu)

**Sub-Domains:**
- Dealer Statistics & Analysis
- Addon Product Management
- Insurance Processing

---

#### **3. Batch & Daily Processing Domain**

**Business Purpose:** Automated scheduled operations and bulk data processing.

**Core Functions:**
- End-of-day (EOD) processing
- End-of-month (EOM) processing
- End-of-year (EOY) updates
- Daily accrual calculations
- Batch payment posting
- Bulk updates and reconciliations
- Scheduled reporting

**Key Programs:**
- BPOTH, BPBRMN (Batch Processing)
- DAILY, DBMAIN (Daily Processing)
- EOMAIN, EOCRON (End-of-Month/Batch Execution)
- DPFILE (Daily Processing File)
- MBMALL, RZMALL (Multi-dataset batch processing)
- EOYUPD, MBAGE1 (Year-end updates)

**Associated Menus:**
- BPMENU (Batch Processing Menu)
- EOMENU (End-of-Month Menu)

**Sub-Domains:**
- Daily Processing Cycles
- Month-End Closing
- Year-End Processing
- Batch Reporting

---

#### **4. Inquiry & Reporting Domain**

**Business Purpose:** Data access, analysis, and regulatory reporting.

**Core Functions:**
- Account and transaction inquiries
- Loan history lookups
- Dealer and branch statistics
- Audit trail inquiries
- Summary and detailed reporting
- Compliance reporting
- Management dashboards

**Key Programs:**
- AUDIQ, AUDIQ1, AUDIQ0 (Audit inquiries)
- SPINQ, CPINQ0 (Loan/transaction inquiry)
- CHLIST, SUMMBY (Summary listings)
- PERUSE (Browse/inquiry)
- RZWRI4, TR1534 (Reporting)
- DBREPT, PILRPT

**Associated Menus:**
- SPLIST (Specialized Listing Menu)
- PERUSE (Inquiry Menu)
- CLMENU (Collection Listing Menu)

**Sub-Domains:**
- Transaction Inquiries
- Audit & Compliance Reports
- Statistical Analysis
- Management Reporting

---

#### **5. Branch & Group Management Domain**

**Business Purpose:** Organizational hierarchy and multi-location support.

**Core Functions:**
- Branch record maintenance
- Group configuration and management
- Routing specifications
- Branch-level reporting
- Organization hierarchy (Corporate → Region → Branch)
- Branch statistics and performance tracking

**Key Programs:**
- BRMAN2, BRMAN5, BRMAN7 (Branch maintenance)
- BPBRMN, BROPM1 (Branch operations)
- GPMAN1, GPOPM1, GPOPM2 (Group management)
- EMPORG, EMPLRG (Employee organization)
- MBCORP (Corporate batch)

**Key Copybooks:**
- GB01GR.CPY (Organization levels)
- ARRAYBR.CPY (Branch processing arrays)

**Sub-Domains:**
- Branch Configuration
- Group Operations
- Organization Hierarchy
- Branch Performance

---

#### **6. Security & Access Control Domain**

**Business Purpose:** System security, user management, and audit compliance.

**Core Functions:**
- User authentication and authorization
- Password management
- Access level controls
- User profile maintenance
- Security audit logs
- Permission management

**Key Programs:**
- GPMAIN, ALMAIN, CHACTN (Security administration)
- VERIFY-PASSWORD, ACCESS-CODE-TYPE
- PASSWD (Password management)

**Key Copybooks:**
- CHKSEC.CPY (Security checks)
- ACCESS.CPY (Account access)

**Associated Menus:**
- SCMENU (Security Menu)
- UPMENU (User Profile Menu)

**Sub-Domains:**
- User Authentication
- Authorization Management
- Audit Logging
- Access Control

---

#### **7. Screen & Menu Management Domain**

**Business Purpose:** User interface and navigation control.

**Core Functions:**
- Menu navigation and routing
- Screen layout definition
- User input handling
- Field validation and formatting
- Screen flow control
- Help system integration

**Key Programs:**
- OPMENU, PGMENU (Main menus)
- GPMAN1-4 (General maintenance screens)
- BRMAN5, BROPM1 (Branch screens)
- LONPY_SCN, LPSMNU_SCN (Loan screens)

**Key Copybooks:**
- Various _SCN.CPY files (Screen definitions)
- GPMAN1_SCN.CPY, GPMAN2_SCN.CPY

**Sub-Domains:**
- Menu Systems
- Screen Navigation
- Input Validation
- Help System

---

#### **8. Reference Data & Code Tables Domain**

**Business Purpose:** System configuration and lookup tables.

**Core Functions:**
- State and county maintenance
- Loan class code management
- Rate tables
- Code table administration
- Business rule configurations

**Key Programs:**
- STMNWO (State maintenance)
- BHMAIN (County maintenance)
- ZMMAI2, BICLAS (Class codes)
- CLASTR (Class structures)
- CDMAIN (Code maintenance)

**Sub-Domains:**
- Geographic Data
- Classification Codes
- Rate Tables
- Business Rules

---

#### **9. Utility & Support Modules Domain**

**Business Purpose:** Technical infrastructure and support services.

**Core Functions:**
- Environment setup and configuration
- Date/time calculations
- Daylight savings time handling
- System utilities
- Help/documentation access
- File operations
- Error handling

**Key Programs:**
- SETENV, INSTAL (Environment setup)
- FXGBDT (Date processing)
- TIMALL (Date/time processing)
- ERRLOG, AUDIT (Error/audit logging)

**Key Copybooks:**
- GETFMW.CPY (Form/environment setup)
- GETDLS.CPY (Daylight savings logic)
- HELPLINK.CPY (Help system)
- FERRORS.CPY (File error reporting)

**Associated Menus:**
- UTMENU (Utility Menu)
- SEMENU (System Environment Menu)

**Sub-Domains:**
- System Configuration
- Date/Time Services
- Error Handling
- File Management

---

#### **10. Collections & Delinquency Management Domain**

**Business Purpose:** Manage delinquent accounts and collection processes.

**Core Functions:**
- Delinquency tracking
- Collection workflow management
- Follow-up actions
- Recovery operations
- Legal processing

**Associated Menus:**
- COMENU (Collection Operations Menu)
- CLMENU (Collection Listing Menu)

---

#### **11. General Ledger & Accounting Domain**

**Business Purpose:** Financial accounting and reconciliation.

**Core Functions:**
- GL entry posting
- Account reconciliation
- Financial reporting
- Ledger updates

**Associated Menus:**
- GLMENU (General Ledger Menu)

---

#### **12. Insurance & Recovery Domain**

**Business Purpose:** Insurance claims and asset recovery.

**Core Functions:**
- Insurance claim processing
- Recovery operations
- Financial adjustments

**Associated Menus:**
- IRMENU (Insurance/Recovery Menu)

---

#### **13. Title Work & Legal Domain**

**Business Purpose:** Title management and legal operations.

**Core Functions:**
- Title transfers
- Lien releases
- Legal documentation

**Associated Menus:**
- TWMENU (Title Work Menu)

---

#### **14. Fixed Assets Domain**

**Business Purpose:** Asset tracking and depreciation.

**Core Functions:**
- Fixed asset management
- Depreciation calculations
- Asset tracking

**Associated Menus:**
- FXMENU (Fixed Asset Menu)

---

### **OTIS Architectural Layers**

The OTIS system follows a **3-tier architecture** that spans all functional domains:

#### **Presentation Layer**
- **Components:** Screens, menus, user interface
- **Examples:** PGMENU, WIMENU, screen copybooks (_SCN.CPY files)
- **Purpose:** User input/display and navigation

#### **Business Logic Layer**
- **Components:** Core processing modules
- **Examples:** LONPG7, LONPW1, DLMAIN, DPFILE, LTCREA, CDMAIN
- **Purpose:** Business rules, validation, processing, routing

#### **Data Layer**
- **Components:** File handlers, copybooks, data structures
- **Examples:** Loan files, dealer files, addon files, code tables
- **Purpose:** Persistent storage and data management

---

### **Domain Organization Visual**

<svg width="1000" height="700" xmlns="http://www.w3.org/2000/svg">
  <text x="500" y="30" text-anchor="middle" font-size="24" font-weight="bold" fill="#1a1a1a">OTIS Functional Domain Architecture</text>
  
  <!-- Core Business Domains -->
  <rect x="50" y="70" width="200" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="95" text-anchor="middle" font-size="14" font-weight="bold" fill="#4338ca">Loan Processing</text>
  <text x="150" y="115" text-anchor="middle" font-size="11" fill="#6366f1">LONPFC, LONPFB</text>
  <text x="150" y="130" text-anchor="middle" font-size="11" fill="#6366f1">LPMAIN, APIPAY</text>
  
  <rect x="270" y="70" width="200" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="370" y="95" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Dealer & Addons</text>
  <text x="370" y="115" text-anchor="middle" font-size="11" fill="#3b82f6">DLMAIN, ADMAIN</text>
  <text x="370" y="130" text-anchor="middle" font-size="11" fill="#3b82f6">LP01DL, LONPFF</text>
  
  <rect x="490" y="70" width="200" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="590" y="95" text-anchor="middle" font-size="14" font-weight="bold" fill="#15803d">Batch Processing</text>
  <text x="590" y="115" text-anchor="middle" font-size="11" fill="#22c55e">BPOTH, DAILY</text>
  <text x="590" y="130" text-anchor="middle" font-size="11" fill="#22c55e">EOMAIN, DBMAIN</text>
  
  <rect x="710" y="70" width="200" height="80" fill="#fef3c7" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="810" y="95" text-anchor="middle" font-size="14" font-weight="bold" fill="#92400e">Inquiry & Reporting</text>
  <text x="810" y="115" text-anchor="middle" font-size="11" fill="#d97706">AUDIQ, CPINQ0</text>
  <text x="810" y="130" text-anchor="middle" font-size="11" fill="#d97706">SUMMBY, PERUSE</text>
  
  <!-- Organization & Management -->
  <rect x="50" y="180" width="200" height="80" fill="#fce7f3" stroke="#ec4899" stroke-width="3" rx="8"/>
  <text x="150" y="205" text-anchor="middle" font-size="14" font-weight="bold" fill="#9f1239">Branch & Group</text>
  <text x="150" y="225" text-anchor="middle" font-size="11" fill="#ec4899">BRMAN2, GPMAN1</text>
  <text x="150" y="240" text-anchor="middle" font-size="11" fill="#ec4899">BPBRMN, EMPORG</text>
  
  <rect x="270" y="180" width="200" height="80" fill="#e0e7ff" stroke="#8b5cf6" stroke-width="3" rx="8"/>
  <text x="370" y="205" text-anchor="middle" font-size="14" font-weight="bold" fill="#5b21b6">Security & Access</text>
  <text x="370" y="225" text-anchor="middle" font-size="11" fill="#8b5cf6">GPMAIN, PASSWD</text>
  <text x="370" y="240" text-anchor="middle" font-size="11" fill="#8b5cf6">CHKSEC.CPY</text>
  
  <rect x="490" y="180" width="200" height="80" fill="#dbeafe" stroke="#06b6d4" stroke-width="3" rx="8"/>
  <text x="590" y="205" text-anchor="middle" font-size="14" font-weight="bold" fill="#0e7490">Screen & Menu</text>
  <text x="590" y="225" text-anchor="middle" font-size="11" fill="#06b6d4">PGMENU, OPMENU</text>
  <text x="590" y="240" text-anchor="middle" font-size="11" fill="#06b6d4">*_SCN.CPY files</text>
  
  <rect x="710" y="180" width="200" height="80" fill="#dcfce7" stroke="#84cc16" stroke-width="3" rx="8"/>
  <text x="810" y="205" text-anchor="middle" font-size="14" font-weight="bold" fill="#3f6212">Reference Data</text>
  <text x="810" y="225" text-anchor="middle" font-size="11" fill="#84cc16">STMNWO, BHMAIN</text>
  <text x="810" y="240" text-anchor="middle" font-size="11" fill="#84cc16">BICLAS, CDMAIN</text>
  
  <!-- Support & Specialized -->
  <rect x="160" y="290" width="180" height="70" fill="#fef3c7" stroke="#eab308" stroke-width="3" rx="8"/>
  <text x="250" y="315" text-anchor="middle" font-size="14" font-weight="bold" fill="#713f12">Utilities</text>
  <text x="250" y="335" text-anchor="middle" font-size="11" fill="#ca8a04">SETENV, TIMALL</text>
  <text x="250" y="350" text-anchor="middle" font-size="11" fill="#ca8a04">GETFMW.CPY</text>
  
  <rect x="360" y="290" width="180" height="70" fill="#fce7f3" stroke="#f472b6" stroke-width="3" rx="8"/>
  <text x="450" y="315" text-anchor="middle" font-size="14" font-weight="bold" fill="#831843">Collections</text>
  <text x="450" y="335" text-anchor="middle" font-size="11" fill="#f472b6">COMENU</text>
  
  <rect x="560" y="290" width="180" height="70" fill="#e0e7ff" stroke="#a78bfa" stroke-width="3" rx="8"/>
  <text x="650" y="315" text-anchor="middle" font-size="14" font-weight="bold" fill="#4c1d95">GL & Accounting</text>
  <text x="650" y="335" text-anchor="middle" font-size="11" fill="#a78bfa">GLMENU</text>
  
  <!-- Architecture Layers -->
  <text x="500" y="410" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">3-Tier Architecture</text>
  
  <rect x="100" y="440" width="250" height="60" fill="#fef3c7" stroke="#f59e0b" stroke-width="2" rx="6"/>
  <text x="225" y="465" text-anchor="middle" font-size="14" font-weight="bold" fill="#92400e">Presentation Layer</text>
  <text x="225" y="485" text-anchor="middle" font-size="11" fill="#d97706">Screens, Menus, UI</text>
  
  <rect x="375" y="440" width="250" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="6"/>
  <text x="500" y="465" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Business Logic Layer</text>
  <text x="500" y="485" text-anchor="middle" font-size="11" fill="#3b82f6">Processing, Rules, Validation</text>
  
  <rect x="650" y="440" width="250" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="6"/>
  <text x="775" y="465" text-anchor="middle" font-size="14" font-weight="bold" fill="#15803d">Data Layer</text>
  <text x="775" y="485" text-anchor="middle" font-size="11" fill="#22c55e">Files, Copybooks, Storage</text>
  
  <!-- Navigation Structure -->
  <text x="500" y="550" text-anchor="middle" font-size="18" font-weight="bold" fill="#1a1a1a">Menu Navigation Hierarchy</text>
  
  <ellipse cx="500" cy="590" rx="80" ry="30" fill="#667eea" stroke="#4338ca" stroke-width="2"/>
  <text x="500" y="595" text-anchor="middle" font-size="12" font-weight="bold" fill="white">PGMENU</text>
  <text x="500" y="608" text-anchor="middle" font-size="10" fill="white">(Main Entry)</text>
  
  <line x1="450" y1="620" x2="250" y2="660" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="500" y1="620" x2="500" y2="660" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="550" y1="620" x2="750" y2="660" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  
  <rect x="180" y="660" width="140" height="30" fill="#3b82f6" stroke="#1e40af" stroke-width="1" rx="4"/>
  <text x="250" y="680" text-anchor="middle" font-size="11" fill="white">LPMENU (Loans)</text>
  
  <rect x="430" y="660" width="140" height="30" fill="#3b82f6" stroke="#1e40af" stroke-width="1" rx="4"/>
  <text x="500" y="680" text-anchor="middle" font-size="11" fill="white">BPMENU (Batch)</text>
  
  <rect x="680" y="660" width="140" height="30" fill="#3b82f6" stroke="#1e40af" stroke-width="1" rx="4"/>
  <text x="750" y="680" text-anchor="middle" font-size="11" fill="white">FDMENU (Dealer)</text>
  
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#667eea"/>
    </marker>
  </defs>
</svg>

---

### **Domain Transaction Taxonomy**

OTIS uses consistent transaction codes across domains:

| Transaction Type | Code | Domain | Purpose |
|-----------------|------|--------|---------|
| Payment/Loan Payment | PL | Loan Processing | Standard loan payment |
| Regular Payment | RP | Loan Processing | Regular scheduled payment |
| Payment Type 2 | P2 | Loan Processing | Alternative payment type |
| Other Charges | OT | Loan Processing | Additional charges |
| Reversal | RV | Multiple | Transaction reversal |
| Reversal/Repo | RE | Loan Processing | Repossession transaction |
| Bankruptcy Filing | BK | Collections | Bankruptcy event |
| Bankruptcy Discharge | BD | Collections | Discharge event |
| Other Charges Type 2 | O2 | Loan Processing | Secondary charge type |

---

### **Modernization Implications**

#### **Domain-Driven Design Benefits**

1. **Clear Service Boundaries**
   - Each domain can become a microservice
   - Independent deployment and scaling
   - Technology flexibility per domain

2. **API Organization**
   - Domain-based API endpoints:
     - `/api/loans/*` (Loan Processing Domain)
     - `/api/dealers/*` (Dealer & Addon Domain)
     - `/api/payments/*` (Payment Processing Sub-domain)
     - `/api/reports/*` (Inquiry & Reporting Domain)

3. **Team Alignment**
   - Assign teams to specific domains
   - Domain expertise development
   - Parallel development streams

4. **Data Migration Strategy**
   - Migrate by domain to minimize risk
   - Maintain data relationships within domains
   - Preserve business rules at domain level

5. **Incremental Modernization**
   - Prioritize high-value domains first
   - Strangler Fig pattern per domain
   - Minimize cross-domain dependencies

---

### **Program Naming Conventions by Domain**

| Prefix | Domain | Examples |
|--------|--------|----------|
| LON* | Loan Processing | LONPFC, LONPFB, LONPG7 |
| DL* | Dealer Management | DLMAIN, DLMAN3 |
| BP* | Batch Processing | BPOTH, BPBRMN |
| BR* | Branch Management | BRMAN2, BRMAN5, BROPM1 |
| GP* | Group/General Processing | GPMAN1, GPOPM1 |
| AP* | API/Application | APIPAY, APILNS |
| CP* | Customer/Collection Processing | CPINQ0 |
| EO* | End-of-Month | EOMAIN, EOCRON |

---

**Source References:**
- Investigation Query Results (Domain Analysis, November 2025)
- OTIS Program Flow Analysis (Index: new_cobol_program_deps, new_cobol_flow_edges_v2)
- Menu Structure Analysis (Index: new_cobol_menu_trees, new_cobol_ui_paths)
- Copybook Usage Analysis (Index: new_cobol_copybook_usage)
- Program Metadata (Index: new_cobol_programs)

---


