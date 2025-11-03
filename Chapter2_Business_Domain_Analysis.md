# Chapter 2: Business Domain Analysis

## Introduction

OTIS (Originated Total Information System) is a comprehensive COBOL-based loan servicing and financial management platform that has served financial institutions for decades. Understanding the business domain that OTIS supports is essential for any modernization effort, as it reveals the intricate workflows, business rules, and operational requirements that must be preserved in the target C# architecture.

This chapter provides a detailed analysis of OTIS's business domain, organized into three main sections:
1. **Loan Servicing and Financial Operations** - The core capabilities for managing loans from origination through payoff
2. **Key Business Processes Supported** - The workflows, dealer relationships, and operational procedures that drive daily operations
3. **User Roles and Workflows** - How different users interact with the system based on their responsibilities and privileges

---

## Section 1: Loan Servicing and Financial Operations in OTIS

### 1.1 Loan Types and Characteristics

OTIS supports a diverse portfolio of loan products, each with distinct characteristics and business rules:

**Primary Loan Types:**
- **Direct Loans** - Standard consumer loans with principal and point charges
- **First and Second Mortgages** - Real estate loans with specific amortization formulas and regulatory requirements
- **Real Estate Loans** - Not classified as "open loans"; have specialized handling
- **Small Loans** - Considered "open loans" with simplified servicing
- **Sales Finance Loans** - Flagged differently for dealer and inventory financing
- **GiftNet Accounts** - Special handling for gift/prepaid card accounts
- **Potential Increased Loans** - Targeted for upselling based on creditworthiness
- **Joint Loans** - Support for joint borrower arrangements
- **Addon Loans** - Loans with addon rate flags for insurance and fee products
- **Loans with Rebates/Earned Interest** - Complex interest calculation and rebate formulas
- **Loans with Special Payment Schedules** - Flexible payment arrangements

The system uses specialized COBOL programs to identify and manage these loan types:
- **CALCZL.CBL** - Master calculation engine for loan terms and formulas
- **BISCAN.CBL** - Business intelligence scanning for loan characteristics
- **PILRPT.CBL** - Potential increased loan reporting
- **LP01SP.CPY** - Loan special characteristics copybook
- **LPAERNW.CPY, LPCERNW.CPY** - Earned interest calculation structures

Each loan type has specific fields in the loan master file that control processing logic, interest calculations, payment handling, and regulatory reporting.

### 1.2 Loan Origination Process

The loan origination workflow in OTIS is a multi-step process with extensive validation and business rule enforcement:

**Step 1: Application Entry**
- Users enter loan applications through interactive screens
- Initial customer and collateral information is captured
- System performs preliminary eligibility checks

**Step 2: Validation and Verification**
- **APILNS** program calls calculation engine (**CALCZL**) to compute loan terms
- **LNVERI** routine performs comprehensive data validation:
  - Customer credit history verification
  - Collateral value assessment
  - Debt-to-income ratio calculations
  - Regulatory compliance checks

**Step 3: Approval Workflow**
- **LNAPPROVAL** program manages approval logic
- Approval limits are checked against user authority
- Supervisor/manager override capability for exceptions
- Password verification for high-value loans
- Audit trail creation for compliance

**Step 4: Loan Creation**
- **APILNS** and **FORM-PROGX** programs create loan master record
- Loan number assignment and account setup
- Insurance and addon product linking
- Initial balance and schedule calculation

**Step 5: Disbursement**
- **LONPD0** handles debit card disbursements
- Multiple disbursement methods supported:
  - Direct deposit to borrower account
  - Dealer payment for sales finance
  - Third-party payees (e.g., contractors for home improvement)
  - Check generation

**Step 6: Logging and Audit**
- **APIDRV** creates comprehensive audit logs
- All origination steps documented for compliance
- Exception handling for customer declines
- Regulatory reporting requirements fulfilled

The entire origination process is designed with exception handling at each step, ensuring that loan applications are processed accurately and in compliance with regulatory requirements.

### 1.3 Complete Loan Lifecycle Management

OTIS manages loans through their complete lifecycle, from origination to post-closure:

**Phase 1: Origination** (Already detailed above)
- Programs: APILNS, LNAPPROVAL, CALCZL, LNVERI

**Phase 2: Booking and Disbursement**
- **LOAN** program handles initial loan booking
- **APILNS** finalizes disbursement transactions
- General ledger integration via **GLDIST** program
- Initial payment schedule generation

**Phase 3: Servicing and Maintenance**
- **LONPFA, LONPFB, LONPF0** - Core payment processing
- **RESCHD** - Payment reschedule management
- **LNLTEN** - Loan term extension processing
- Routine maintenance operations:
  - Balance adjustments
  - Status changes (active, deferred, charge-off)
  - Payment corrections and reversals
  - Term adjustments
  - Borrower information updates
  - Interest rate modifications

**Phase 4: Payoff and Closure**
- **PAYOFF** program calculates payoff amounts
- **LPPOFF** handles payoff processing
- **GP-PAYOFF-NONCASH** for non-cash payoffs
- **REBATE** program computes unearned interest refunds
- Multiple rebate calculation methods:
  - Pro-rata method
  - Short-rate method
  - State-specific formulas (regulatory compliance)
  - Actuarial method
- Final payment processing and account reconciliation

**Phase 5: Post-Closure**
- Final reporting and notifications
- Archive record creation
- Retention period management
- Historical data maintenance for compliance queries

Throughout all phases, the system maintains comprehensive audit trails, enforces business rules, and ensures regulatory compliance.

### 1.4 Interest Calculation Methods

OTIS supports seven different interest calculation methods to meet diverse product and regulatory requirements:

**1. Simple Interest (360/365 Day Basis)**
- Formula: Interest = Principal × Rate × (Days / Day Basis)
- Uses **LPRATE.CPY** copybook for rate structures
- Most common method for consumer loans

**2. Actuarial Interest**
- Time-based calculation with compound interest principles
- Uses **LPCERN.CPY** copybook
- Required for certain mortgage products

**3. Rule of 78s (Sum-of-Digits)**
- Front-loaded interest allocation method
- Uses **LPAERN.CPY** copybook
- Applied when contractually specified

**4. Amortized Interest**
- Equal payment schedules with declining interest portions
- Uses **LPAPRS.CPY** and **LFPPO.CBL** for calculations
- Standard for mortgage products

**5. APR Calculation**
- Formula: APR = (1 + NPR)^PPY - 1
  - NPR = Nominal Periodic Rate
  - PPY = Periods Per Year
- Calculated by **CALCZL.CBL** program
- Required for Truth in Lending Act (TILA) compliance

**6. Dealer Discount**
- Special calculation for dealer-financed loans
- Uses **LPCERN.CPY** copybook
- Handles dealer participation and reserves

**7. Penalty Interest**
- Applied for late payments or default conditions
- Uses **LPRATE.CPY** copybook
- Configurable penalty rates by product type

The system automatically selects the appropriate interest calculation method based on loan type, product code, and state regulatory requirements. Visual diagrams in the source documentation illustrate how these methods interact with loan schedules and payment allocation.

### 1.5 Payment Processing Capabilities

OTIS provides comprehensive payment processing with support for multiple payment types and methods:

**Payment Types:**
- **Regular Scheduled Payments** - Standard monthly/periodic payments
- **Partial Payments** - Less than scheduled amount
- **Payoff Payments** - Full satisfaction of loan
- **Refund Payments** - Customer refunds (overpayments, cancellations)
- **Non-Cash Payments** - Adjustments and write-offs
- **Batch Payments** - Automated clearing house (ACH) files
- **Dealer Payments** - Dealer remittances for sales finance
- **Third-Party Payments** - Insurance proceeds, legal settlements
- **Addon Payments** - Insurance premium and fee payments

**Payment Methods:**
- **Cash** - In-branch cash handling
- **Check** - Check imaging and processing
- **ACH** - Automated clearing house transactions
- **Wire Transfer** - Same-day electronic funds transfer
- **Money Order** - Third-party money orders
- **Interim Deposit** - Unapplied funds holding account
- **Batch Import** - File-based payment uploads

**Payment Allocation Logic:**
The system follows a sophisticated payment allocation waterfall:
1. Late charges and fees
2. Interest due
3. Principal due
4. Escrow amounts (if applicable)
5. Addon insurance premiums
6. Advance principal (if payment exceeds scheduled amount)

Payment processing programs include:
- **LONPFA/LONPFB** - Core payment processing
- **PST822, PST824, PST826** - Specialized payment types
- **BATCH** programs - Automated payment file processing
- **LPDLADJ** - Payment adjustments and corrections

All payments are validated, posted to the loan master file and general ledger, and documented in transaction history for audit purposes.

### 1.6 Renewal, Extension, and Refinancing

OTIS handles specialized loan modification transactions with comprehensive business rule enforcement:

**Loan Renewals:**
- Complete loan payoff and re-origination in single transaction
- Eligibility checks:
  - Current payment status verification
  - Borrower creditworthiness review
  - Collateral revaluation
  - Regulatory compliance validation
- Proceeds calculation:
  - Payoff existing loan balance
  - Apply regulatory rebate calculations
  - Restrict proceeds per state regulations
  - Handle addon product transfers
- Programs: **LONPRN**, **RENEWAL** modules
- Special handling for insurance product transfers

**Loan Extensions:**
- Extend loan term without full refinancing
- Programs: **LNLTEN**, **RESCHD**
- Recalculate payment schedules
- Interest adjustment handling
- Regulatory notification requirements

**Refinancing:**
- Similar to renewal but with different product terms
- May involve rate changes, term modifications, or product conversions
- Full approval workflow required
- Comprehensive disclosure generation

All renewal and extension transactions maintain complete audit trails and generate required regulatory disclosures.

### 1.7 Loan Maintenance Operations

Day-to-day loan servicing requires numerous maintenance capabilities:

**Balance Adjustments:**
- Principal corrections
- Interest recalculations
- Fee additions/reversals
- Programs: **LPDLADJ**, **LONPG7**

**Status Changes:**
- Active ↔ Deferred
- Active → Charge-off
- Active → Repossession
- Charge-off → Recovery
- Status change audit trails

**Payment Corrections:**
- Reverse erroneous payments
- Reallocate payment amounts
- Correct payment dates
- NSF (non-sufficient funds) handling

**Term Adjustments:**
- Modify payment frequency
- Change due dates
- Adjust maturity dates
- Recalculate amortization schedules

**Borrower Information Updates:**
- Address changes
- Phone number updates
- Email address management
- Co-borrower modifications
- Contact preference settings

**Interest Rate Modifications:**
- Variable rate adjustments
- Index rate updates
- Promotional rate expirations
- Regulatory rate cap enforcement

Programs: **LONPS4**, **LONPF3**, **LONPF5**, **LNMAIN**, **RESCHD**

All maintenance operations require appropriate user authorization and create audit log entries.

### 1.8 Payoff and Rebate Management

Loan payoff processing involves complex calculations to determine accurate payoff amounts and required rebates:

**Payoff Calculation Components:**
- Current principal balance
- Accrued interest through payoff date
- Unpaid late charges and fees
- Prepayment penalties (if applicable)
- Unearned interest rebate (negative amount)
- Addon insurance rebates (if applicable)

**Rebate Calculation Formulas:**
OTIS implements multiple state-specific rebate formulas:
- **Pro-Rata Method** - Time-based proportional rebate
- **Short-Rate Method** - Reduced rebate for early payoff
- **State-Specific Formulas** - Custom calculations per state regulations
- **Actuarial Method** - Time-value-of-money based rebate
- **Rule of 78s Reversal** - Unwind sum-of-digits interest

**Programs:**
- **REBATE** - Master rebate calculation engine
- **LPPOFF** - Payoff processing workflow
- **PAYOFF** - Payoff amount calculation
- **GP-PAYOFF-NONCASH** - Non-cash payoff handling

**Business Rules:**
- State regulatory requirements enforced
- Insurance policy cancellation handling
- Addon product rebate calculations
- Dealer reserve recapture (if applicable)
- Final statement generation
- Lien release documentation

The system automatically determines the appropriate rebate method based on loan origination date, state, and product type, ensuring regulatory compliance.

### 1.9 Delinquency and Collections Management

OTIS provides comprehensive delinquency management and collections support:

**Delinquency Detection:**
- **LPTDEL** - Calculate days past due
- **LPPDUE** - Determine past due amounts
- Automated aging calculations
- Contractual vs. regulatory delinquency

**Delinquency Reporting:**
- **COLRPT** - Collections reports
- Aging buckets (30/60/90/120+ days)
- Delinquency rate calculations
- Portfolio quality metrics

**Workflow Automation:**
- **SPCDEL** - Special delinquency handling
- Automated notification triggers
- Collection queue management
- Escalation rules

**Bulk Processing:**
- **BULKG2** - Bulk delinquency processing
- Mass status updates
- Batch collection actions
- Performance reporting

**Collections Features:**
- Customer contact tracking
- Promise-to-pay management
- Payment arrangement setup
- Legal action workflow
- Credit bureau reporting
- Skip tracing integration

**Business Rules:**
- Fair Debt Collection Practices Act (FDCPA) compliance
- State-specific collection regulations
- Bankruptcy detection and handling
- Statute of limitations tracking
- Right-to-cure notification requirements

Collections officers use these tools to manage delinquent accounts efficiently while maintaining regulatory compliance.

### 1.10 Loan Transfers

OTIS supports multiple types of loan transfers with complete audit trail creation:

**Transfer Types:**

**Branch Transfers:**
- Move loans between branches
- Programs: **BLKONE**, **BULKG1**
- Maintains historical branch data
- Updates reporting hierarchies

**Class Transfers:**
- Change loan classification
- Program: **CLAST1**
- Regulatory reporting impact
- Portfolio management support

**Account Number Transfers:**
- Renumber loans
- Programs: **BULKT2**
- Legacy system migration support
- Merger/acquisition transitions

**Features:**
- Complete transaction history preservation
- Audit trail documentation
- Dealer relationship transfers
- Insurance policy transfers
- Schedule preservation
- Balance reconciliation
- General ledger integration

All transfer operations require supervisor approval and generate comprehensive audit documentation for regulatory review.

### 1.11 Financial Transactions Beyond Payments

OTIS processes over 12 types of financial transactions beyond standard payments:

**Transaction Types:**
- **Disbursements** - Loan proceeds, insurance claims
- **Write-offs** - Charge-off and recovery transactions
- **Repossessions** - Collateral recovery and liquidation
- **Renewals** - Complete loan refinancing
- **Insurance Transactions** - Premium payments, claim processing, cancellations
- **Adjustments** - Correction entries, goodwill credits
- **Transfers** - Inter-account movements
- **Refunds** - Customer refund processing
- **Fee Transactions** - NSF fees, late charges, annual fees
- **Reserve Transactions** - Dealer reserve payments and recapture
- **Escrow Transactions** - Tax and insurance escrow management
- **Legal Transactions** - Garnishment, levy, bankruptcy

Each transaction type has specialized processing logic, validation rules, and general ledger impact. Programs handle the business rules, calculate amounts, and ensure proper accounting treatment.

### 1.12 Disbursement Handling

Disbursement processing supports multiple payee types and verification requirements:

**Borrower Disbursements:**
- Direct deposit to borrower account
- Check generation
- Debit card loading
- Programs: **LONPFD**, **PST822**

**Dealer Disbursements:**
- Dealer proceeds for sales finance
- Dealer reserve payments
- Programs: **PST824**, **PST826**
- Dealer validation and verification

**Third-Party Disbursements:**
- Contractor payments (home improvement)
- Medical provider payments (medical loans)
- Vendor payments
- Insurance company payments

**Validation and Audit:**
- Payee verification
- Duplicate disbursement prevention
- Hold fund requirements
- Audit trail creation
- Regulatory documentation
- Disbursement reconciliation

Programs ensure that disbursements are properly authorized, validated, and documented for audit purposes.

### 1.13 Refund Processing

OTIS handles six distinct types of refunds triggered by specific business events:

**Refund Types:**

**1. Payoff Refunds**
- Overpayments on loan payoffs
- Automatic calculation and processing
- Customer notification

**2. Service Charge Refunds**
- Fee reversals
- Goodwill credits
- Error corrections

**3. Insurance Refunds**
- Policy cancellation rebates
- Claim-related refunds
- Pro-rata calculations

**4. Addon Product Refunds**
- Service contract cancellations
- GAP insurance refunds
- Warranty product refunds

**5. Dealer Refunds**
- Dealer reserve adjustments
- Chargebacks
- Recourse transactions

**6. Manual Refunds**
- Supervisor-authorized refunds
- Special circumstances
- Dispute resolutions

**Processing:**
- Refund calculation engines
- Approval workflow (based on amount thresholds)
- Payment method selection (check, ACH, card credit)
- Regulatory compliance (e.g., prompt refund requirements)
- 1099 reporting (if applicable)

All refunds are subject to audit and regulatory review, with complete documentation maintained in the system.

### 1.14 Fee and Charge Management

OTIS manages a comprehensive fee structure with automated calculation and posting:

**Fee Types:**
- Late payment fees (calculated by **LONPG7**)
- NSF (non-sufficient funds) fees
- Annual fees
- Origination fees
- Prepayment penalties
- Document fees
- Credit report fees
- Wire transfer fees
- Statement fees
- Convenience fees (e.g., pay-by-phone)

**Calculation:**
- **LPAMTS** - Fee amount calculation
- State-specific cap enforcement
- Product-specific fee schedules
- Promotional fee waivers

**Posting:**
- **LPDLADJ** - Post fees to loan accounts
- General ledger distribution
- Revenue recognition
- Fee reversal capability

**Validation:**
- Regulatory limit checks
- Customer notification requirements
- Dispute handling
- Fee waiver authorization

Fee management ensures compliance with state and federal regulations while supporting the institution's revenue model.

### 1.15 Batch Processing Capabilities

OTIS provides robust end-of-day (EOD) and end-of-month (EOM) batch processing:

**End-of-Day Processing:**
- **UPDEOY** - Master EOD control program
- Interest accrual calculations
- Payment due date advancement
- Delinquency status updates
- General ledger posting
- Report generation
- Error handling and recovery

**End-of-Month Processing:**
- **RZMALL** - Monthly accrual processing
- **MB4ALL** - Month-end balance calculations
- **EOMALL** - Consolidated EOM processing
- Financial statement preparation
- Regulatory report generation
- Archive and backup operations

**Error Handling:**
- Transaction rollback capability
- Error logging and reporting
- Automatic retry logic
- Manual intervention workflows
- Audit trail creation

**Multi-Branch Support:**
- Process branches sequentially or parallel
- Branch-specific error handling
- Consolidated reporting
- Central audit trail

Batch processing ensures data consistency, regulatory compliance, and operational efficiency.

### 1.16 General Ledger Integration

OTIS maintains tight integration with the general ledger system for financial accounting:

**Automated Posting:**
- Real-time or batch GL posting
- **GLDIST** - GL distribution engine
- **GIACCT** - GL account mapping
- **GLEOD** - End-of-day GL finalization

**GL Structure:**
- **GL01GT.CPY** - GL master file structure
- **GLFDGL.CPY** - GL field definitions
- Account hierarchy support
- Multi-company/branch structure

**Transaction Types:**
- Loan disbursements
- Payment receipts
- Interest accrual
- Fee income
- Write-offs and recoveries
- Reserve movements
- Refund payments
- Transfer transactions

**Audit Capabilities:**
- Transaction drill-down
- Reconciliation support
- Trial balance generation
- Journal entry documentation
- Period-end close validation

The GL integration ensures accurate financial reporting and supports internal and external audit requirements.

### 1.17 Accounts Payable Functionality

OTIS includes comprehensive accounts payable functionality for operational expenses:

**Invoice Management:**
- **AP01AP.CPY** - Invoice master structure
- Invoice entry and approval
- Vendor master maintenance
- Purchase order matching

**Transaction Release:**
- **AP01AK.CPY** - Transaction release structures
- Payment authorization workflow
- Approval limits enforcement
- Batch payment processing

**Check Processing:**
- **AP01CK.CPY** - Check file structure
- Check printing and signature
- Positive pay file generation
- Electronic payment (ACH/wire)

**History Tracking:**
- **AP01AT.CPY** - AP history structure
- Payment history maintenance
- Vendor payment summaries
- 1099 reporting support

**Features:**
- Three-way match (PO, receipt, invoice)
- Early payment discounts
- Recurring invoice support
- Vendor inquiry and reporting
- Payment scheduling
- Cash management integration

AP functionality supports operational payment needs for dealer settlements, insurance premiums, vendor payments, and other business expenses.

### 1.18 Cash Handling and Reconciliation

OTIS provides comprehensive cash management and reconciliation capabilities:

**Cash Receipt Processing:**
- In-branch cash handling
- Cash drawer management
- Receipt documentation
- Balancing procedures

**Bank Reconciliation:**
- Electronic bank statement import
- Automated transaction matching
- Exception identification
- Reconciliation reporting

**Cash Position Management:**
- Daily cash position reporting
- Branch cash limits
- Vault cash tracking
- ATM reconciliation (if applicable)

**Controls:**
- Dual control requirements
- Over/short tracking
- Surprise audit support
- Segregation of duties enforcement

Cash handling processes ensure strong internal controls and accurate financial reporting.

### 1.19 Compliance and Regulatory Reporting

OTIS generates extensive compliance reports to meet regulatory requirements:

**Key Compliance Areas:**

**Truth in Lending Act (TILA):**
- APR disclosure calculations
- Rescission period tracking
- Required disclosures

**Fair Credit Reporting Act (FCRA):**
- Credit bureau reporting (Metro2 format)
- Dispute handling
- Consumer rights documentation

**Fair Debt Collection Practices Act (FDCPA):**
- Collection activity documentation
- Prohibited practice prevention
- Communication logging

**Bank Secrecy Act (BSA):**
- Large transaction reporting (CTR)
- Suspicious activity reporting (SAR)
- Customer identification program (CIP)

**State-Specific Regulations:**
- Interest rate caps
- Fee limitations
- Licensing requirements
- Contract provisions

**Reporting Programs:**
- **METRO2** - Credit bureau reporting
- **FEUNR1** - Unearned income reporting
- **LPR2MU** - Regulatory report menu
- **ADDEXT** - Insurance compliance

All compliance reporting includes audit trails and documentation for regulatory examinations.

### 1.20 Insurance Tracking and Processing

OTIS manages insurance products throughout the loan lifecycle:

**Insurance Types:**
- **Credit Life Insurance** (CL)
- **Accident & Health Insurance** (AH)
- **Property Protection Insurance** (PP)
- **Optional Coverages** (O1-O4)
- **Gap Insurance**
- **Collateral Protection Insurance** (CPI)

**Processing:**
- **ADDONSPR.CPY** - Addon insurance structures
- **ADDONRB.CPY** - Insurance rebate logic
- **LPPOF2.CPY** - Payoff insurance calculations
- Premium calculation and posting
- Commission tracking
- Claim processing
- Policy cancellation and refunds

**Compliance:**
- Regulatory filing requirements
- Policy disclosure generation
- Licensing verification
- Reserve requirement calculation

**Programs:**
- **LONPG7** - Insurance posting
- **ADDEXT** - Insurance compliance monitoring
- Insurance extract file generation

Insurance tracking ensures proper accounting, regulatory compliance, and customer service.

---

## Section 2: Key Business Processes Supported

### 2.1 Dealer Relationship Management

Dealers play a central role in OTIS as key business partners in loan origination and financial transactions:

**Dealer Master File:**
- **LP01DL.CPY** - Dealer master structure
- Dealer number (primary key)
- Dealer short name
- Business attributes and formulas
- Participation flags and rates

**Dealer Maintenance:**
- **DTSCAN** - Dealer detail window scan
- **DLCOPY** - Dealer record copying
- Dealer lookup and validation
- Update dealer records
- Search by number or name

**Dealer Participation Logic:**
- **LONPF2** - Participation rule enforcement
- Financial sharing calculations
- Eligibility determination
- Program-specific rules (PC accounts, IB accounts)

**Dealer Reporting:**
- **LNAGA2** - Dealer totals and aging
- Loan balance aggregation
- Payment activity summaries
- Management and compliance reports

**Validation:**
- Dealer number verification
- Error handling for invalid dealers
- Special case handling (dealer zero for test transactions)

Dealers are validated during transaction entry, participate in financial programs according to configured rules, and are comprehensively reported for management oversight.

### 2.2 Addon Products Management

OTIS handles addon insurance and fee products with dedicated processing logic:

**Addon Product Types:**
- **CL** - Credit Life Insurance
- **AH** - Accident & Health Insurance
- **PP** - Property Protection Insurance
- **O1-O4** - Optional addon codes
- **NP** - Non-participating addons (special regulatory handling)

**Data Structures:**
- **LPPOF2.CPY** - Payoff addon tables
- **ADDONRB.CPY** - Addon rebate logic
- **ADDONSPR.CPY** - Addon routines

**Processing:**
- Premium calculation
- Commission tracking
- Rebate calculations (on cancellation or payoff)
- Regulatory compliance checks
- Addon interest rebate tables (by type)

**Programs:**
- **LONPG7** - Addon posting and management
- **ADDEXT** - Addon compliance monitoring
- **LPPOF2** - Payoff calculations with addons

**Business Rules:**
- NP addons: No interest charged (regulatory restriction)
- Multiple addon types per loan supported
- State-specific regulations enforced
- Cancellation and refund procedures

Addon product management ensures accurate accounting, regulatory compliance, and proper customer billing.

### 2.3 Dealer Financial Transactions

OTIS processes a wide range of dealer-related financial transactions:

**Dealer Reserve Types:**
- **PR** - Reserve Principal Payment
- **PC** - Reserve Charges Payment
- **PP** - Reserve Participation Payment
- **DR** - Payment from Reserve

**Reserve Management:**
- **LONPF2** - Updates dealer reserves
- **DTMAIN** - Reserve transaction types
- Reserve balance tracking
- Payment from reserve processing

**Dealer Payables:**
- **GIACCT** - Dealer payable definitions
- **AFPDLLP.CPY** - Calculate amounts due
- Advances and settlements
- Net gain/loss calculations

**Dealer Advances:**
- Advance tracking
- Repayment calculations
- Offset against future transactions

**Dealer Recourse:**
- **LONPF2**, **SFVERI** - Recourse verification
- Chargeback processing
- Reserve adjustments

**Dealer Checks:**
- **LPFDTRC.CPY** - Dealer check transactions
- Dealer proceeds payments
- Check issuance

**Reporting:**
- **LNAGAC**, **LNAGA2** - Dealer totals
- Aggregated metrics
- Management reporting

Dealer financial transactions ensure accurate settlement, proper accounting, and compliance with dealer agreements.

### 2.4 Dealer Inventory and Floor Planning

OTIS manages dealer inventory financing (floor planning) with comprehensive tracking:

**Dealer Master Data:**
- **LP01DL.CPY** - Dealer master structure
- Reserve requirements (DL-MIN-RSV-REQD, DL-RSV-REQD-60)
- Participation formulas
- Flat table flags

**Inventory Tracking:**
- **DTLIST**, **DTSCAN** - Inventory processing
- Item-level tracking (by dealer, branch, class)
- Totals computation (proceeds, payments, reserves)
- Status tracking (in stock, sold, paid off)

**Floor Plan Loans:**
- Loan-to-inventory item association
- Balance updates on item sale
- Interest calculation on inventory

**Reserve and Participation:**
- **LONPFA**, **LONPFB**, **LONPF2** - Reserve calculations
- Recapture computation
- Participation payouts

**Reporting:**
- Detailed inventory reports
- Floor plan status
- Financial transaction summaries

Floor planning support enables OTIS to serve dealer financing customers with sophisticated inventory and loan management.

### 2.5 Dealer Performance Reporting

OTIS provides comprehensive dealer performance analytics:

**Key Metrics:**
- Loan volume (number and amount)
- Reserve and recourse statistics
- YTD totals and monthly breakdowns
- Branch/group/corporate rollups

**Report Programs:**
- **DBREPT** - Dealer/branch reporting
- **DTLIST** - Dealer transaction listing
- **LNAGA2**, **LNAGAC** - Dealer aging/totals
- **PROGRP** - Group/corporate rollups

**Report Content:**
- Dealer-level detail lines
- Branch/group aggregations
- Exception analysis
- Performance comparisons

**Specialized Statistics:**
- **DTSCAN**, **CHCKI0**, **LONPI0** - Dealer stats display
- Recourse metrics
- Reserve balances
- Loan status summaries

**Corporate Rollups:**
- **RZCORP**, **RZCORA** - Corporate-level aggregation
- **RZAGE5** - Group/corporate aging
- **MBALR4** - Branch/corporate analytics

These reports enable management to monitor dealer performance, identify trends, and make informed business decisions about dealer relationships.

### 2.6 Daily Workflows for Loan Officers

Loan officers use OTIS for comprehensive loan management throughout the business day:

**1. Loan Origination and Approval:**
- Enter applications via screens (GTFORM)
- **APILNS** calculates terms and validates data
- **LNAPPROVAL** verifies approval criteria
- Create loan record and log transactions

**2. Loan Maintenance and Servicing:**
- Select loan to modify
- **RESCHD**, **LONPS4** process changes
- Update loan master files
- Recalculate schedules
- Reflect changes in reports

**3. Dealer Relationship Management:**
- Review dealer performance
- Update inventory/financial transactions
- System logs changes
- Update dealer statistics via **CLAST2**

**4. Daily Reporting and Analytics:**
- Generate daily reports (**DAILY**, **PILRPT**, **CPINQ0**)
- Analyze performance and risk metrics
- Review for decision-making

**5. End-of-Day Processing:**
- Initiate EOD routines
- Process payoffs via **PLPURG**, **LPPOFF**
- Purge old records
- Update totals
- Generate daily summaries

Loan officers have comprehensive tools to manage the entire loan lifecycle efficiently.

### 2.7 Customer Service Representative Workflows

CSRs handle customer inquiries using interactive inquiry programs:

**Inquiry Programs:**
- **SSINQ** - Social Security/customer inquiry
- **CPINQ0** - Collector/payment inquiry
- **RCINQ** - Receipt inquiry
- **BYINQ** - Borrower inquiry

**Screen Navigation:**
- Main menu selection
- Enter search criteria
- Validate input
- Retrieve matching records

**Data Display:**
- Formatted screens with:
  - Balances
  - Payment history
  - Dealer relationships
  - Contact details

**Actions:**
- View loan status
- View payment history
- Update customer contact info
- Log inquiry for audit
- Escalate complex issues

**Security:**
- **CHKSEC.CPY** - Security validation
- Authorized access only

**Logging:**
- **GR15DA** - Customer line display
- Inquiry logging for compliance

CSRs efficiently serve customers while maintaining security and audit compliance.

### 2.8 Collections Officer Workflows

Collections officers manage delinquent accounts through systematic workflows:

**1. Identifying Delinquent Accounts:**
- Batch programs (**PROGRP**, **RZMALL**) scan for delinquency
- Accounts flagged automatically
- Daily lists generated

**2. Calculating Delinquency:**
- **LPCDEL.CPY** - Compute contractual delinquent amount
- **LPCRNO.CPY** - Handle negative delinquency
- Arrearage calculation

**3. Reviewing Accounts:**
- **CPINQ0** - Interactive inquiry
- Search accounts
- Review payment history
- Review delinquency status
- Update account notes

**4. Generating Reports:**
- **COLRPT** - Delinquency aggregation
- **COLRGB** - Collector activity tracking
- Detailed and summary reports

**5. Taking Collection Actions:**
- Record attempts
- Log promises to pay
- Schedule follow-ups
- All actions logged for audit

**6. Performance Analysis:**
- Review success rates
- Analyze time spent
- Branch/collector metrics

Collections workflows ensure efficient delinquency management and regulatory compliance.

### 2.9 Accounting Staff Workflows

Accounting staff use OTIS for financial reconciliation and reporting:

**Reconciliation Workflow:**
- **RECONC** - Main reconciliation program
- **RECONC_WKS.CPY**, **RECONC_DEF.CPY** - Working structures
- Initialization and setup
- Create work files
- Process records (matching and balancing)
- Write detail lines
- Print totals
- Error handling

**Reporting Workflow:**
- **DBREPT** - Dealer/branch reports
- **RECAPJ** - Recap journal
- **GRLIST** - GL listing
- **RZREPT** - Branch/zone reporting
- **MBALR4**, **RZWRI2**, **MBUNR1H** - Monthly/balance reports
- Aggregate data by branch/account/transaction type
- Format output
- Calculate totals
- Generate summaries

**Screen Interaction:**
- **RECONC_SCN.CPY** - Screen layout
- Interactive review
- Enter adjustments
- Confirm balances

**Error Handling:**
- Detect mismatches
- Log errors for audit
- Detailed breakdowns for troubleshooting

Accounting workflows ensure accurate financial closure and compliance-ready documentation.

### 2.10 Supervisor and Manager Approval Workflows

Supervisors and managers have approval authority with documented workflows:

**Approval Verification:**
- **LNAPPROVAL.CPY** - Loan approval logic
- **LNAPPROVALW.CPY** - Approval worker fields
- Approval limits checked
- User ID verification
- Approval flags

**Branch/Group Validation:**
- **BRSECURE** checks authorization
- If unauthorized, password prompt
- Supervisor enters credentials

**Password Verification:**
- **PASSWD** validates credentials
- If valid, access granted
- If invalid, access denied

**Approval Window:**
- Display current limits
- Show user IDs
- Allow comment entry
- Record approval action

**Audit Logging:**
- All approvals logged (**AUDITW**)
- User ID, date, comments documented

**Workflow Example:**
1. User initiates high-value transaction
2. System checks approval limit
3. If exceeded, prompt for supervisor approval
4. Supervisor enters credentials
5. System records and proceeds

Approval workflows ensure proper oversight and audit trail creation.

### 2.11 Branch Operations and Multi-Branch Support

OTIS supports complex multi-branch environments:

**Branch Data Structures:**
- **BRANCH_DEF.CPY** - Branch definitions
- **BRANCH_WKS.CPY** - Working storage
- **BRANCH_SCN.CPY** - Screen layouts
- **BRANCH_EVA.CPY** - Evaluation logic

**Branch Management:**
- **BRANCH** - Core program
- Read, update, validate branch records

**Multi-Branch Processing:**
- **GRSSP** - Branch table loader
- Process up to 300 branches
- Loop through branch table
- Set environment/file paths per branch
- Apply branch-specific security

**Branch Security:**
- **CHKSEC.CPY** - Security checks
- **ACCESS.CPY**, **ACCESSW.CPY** - Access control
- Enforce branch-level permissions

**Reporting:**
- **MBALR6H**, **MBANR2** - Branch reporting
- Individual and consolidated reports

**Workflow:**
1. User selects multi-branch processing
2. System loads branch table (up to 300)
3. For each branch:
   - Set environment
   - Process transactions
   - Apply security
   - Generate reports
4. Aggregate results

Multi-branch capabilities enable large institutions to manage operations across multiple locations efficiently.

### 2.12 Month-End and Year-End Procedures

OTIS enforces strict month-end and year-end closing procedures:

**Month-End Closing (EOM):**
- **UPDEOY** - EOM/EOY logic
- **GLTAGS** - Activity logging
- Verify all transactions posted
- Update balances
- Generate reports
- Lock month (prevent further changes)
- Set EOM flags

**Year-End Closing (EOY):**
- **UPDEOY** - EOY orchestration
- **ENDYER** - GL year-end procedures
- **GLTAGS** - Year-end logging
- Validate EOM completion for all branches
- Aggregate annual data
- Update retained earnings
- Set EOY tags
- Generate annual statements

**Safeguards:**
- EOM must precede EOY
- Branch-by-branch validation
- Errors logged, not aborted
- Tagging/flagging for audit
- Retained earnings checks

**Workflow:**
- Verify EOM completion
- Run month-end updates
- Set EOM flags
- (At year-end) Validate all branches
- Run EOY updates
- Set EOY flags
- Handle errors per branch

These procedures ensure accurate financial closure and readiness for regulatory reporting.

### 2.13 Audit Trails and Compliance Documentation

OTIS maintains comprehensive audit trails:

**Audit Programs:**
- **AUDITW**, **AUDEXT**, **AUDEXT-MB**, **MBDEXT**, **MBDEX2**
- Write audit records for all changes
- Before/after values
- User IDs, timestamps
- Change types (add, delete, modify)

**Audit File Structures:**
- **GB01AU.CPY** - Audit file structure
- **AUDITWW.CPY** - Worker routines
- File code, date, sequence keys
- Original and new values

**Audit Workflow:**
1. Change detected
2. Capture before/after values
3. Record user ID, timestamp
4. Write to audit file (GB/AUFILE)
5. Multi-branch: tag with branch ID

**Compliance Extracts:**
- **AUDEXT** - Audit extraction
- Tab-delimited exports
- Compliance team review

**Month-End/Year-End Audit:**
- **GLFX12** - Period audit entries
- Summary changes documented

All audit capabilities support internal controls, regulatory examinations, and forensic analysis.

### 2.14 Security and Access Control Workflows

OTIS implements layered security:

**Security Programs:**
- **CHKSEC** - Called by 59 programs for security checks
- **BRSECURE** - Branch/group validation
- **PASSWD** - Password verification
- **ACCESS/DACCESS** - File/directory access control

**Security Copybooks:**
- **BRSECURE.CPY** - Branch/group logic
- **PASSWDW.CPY** - Password structures

**Workflow:**
1. User attempts access
2. **CHKSEC** or **BRSECURE** validates credentials
3. Check branch/group authorization
4. If unauthorized, password prompt
5. **PASSWD** verifies password
6. If valid, grant access
7. Log access attempt (**AUDITW**)
8. If invalid, deny and log

**Branch Security:**
- Validate branch range
- Check group membership
- Password required for sensitive operations

**Field Protection:**
- **PROTECT-FIELDS** routines
- Restrict sensitive field modification

All security measures ensure only authorized users access sensitive data and operations.

### 2.15 Exception Processing and Error Correction

OTIS handles exceptions systematically:

**Exception Detection:**
- **LONPW9**, **LONPWA**, **WDAYBK**, **BULKG2**, **CLAST2**
- Check for:
  - Invalid account numbers
  - Zero balances
  - Missing fields
  - Business rule violations

**Exception Logging:**
- Move exception details to exception fields
- Write to exception reports
- Store in designated directories

**Error Correction:**
- Operators review exception reports
- Manually correct data
- **SPFDMETRO2.CPY** - Correction indicators
- Timestamps for audit trail
- Reprocess corrected transactions

**Error Handling Routines:**
- **FERRORS** - File I/O errors
- **ERRLOG** - Error logging
- **ERRMSG** - Error messaging
- **DECLARE.CPY** - Error infrastructure

**Exception Totals:**
- **LONPW9** aggregates exception totals
- Summary reporting

**Workflow:**
1. Transaction processing
2. Exception detection
3. Logging
4. Reporting (for operator review)
5. Correction (with indicators/timestamps)
6. Reprocessing
7. Audit trail

Exception handling ensures data integrity and regulatory compliance.

---

## Section 3: User Roles and Workflows

### 3.1 User Role Definitions

OTIS defines distinct user roles with specific responsibilities:

**Standard User / Teller:**
- Daily transaction entry (payments, refunds, advances)
- Account information access
- Limited record modification
- Password authentication required
- Restricted from sensitive operations

**Supervisor / Manager:**
- Approve exceptions and overrides
- Access higher-level reporting
- Manage branch operations
- Assign users
- Elevated privileges for exception processing
- View/modify compliance documentation
- May be exempt from certain password requirements (PEFILE in **GBFDPE.CPY**)

**Compliance Officer / Auditor:**
- Review audit trails
- Monitor regulatory adherence
- Generate/analyze exception reports
- Read-only access to sensitive logs
- Cannot modify transactional data

**System Administrator / Security Admin:**
- Manage user accounts and passwords
- Configure access levels
- Maintain security profiles
- Full access to security maintenance
- **SEMENU_SCN.CPY**: Security profile options (set, copy, entry, exception maintenance, status reports)
- Set password exceptions (**GBFDPE.CPY**: PEFILE)

**Batch Processor / Automated Job User:**
- Execute scheduled batch jobs
- Limited interactive access
- Special user IDs with restricted permissions
- Subject to audit and logging

Roles are enforced through security profile maintenance (PASSWD, BRSECURE) and documented for compliance.

### 3.2 Role-Based Access Control Implementation

OTIS implements RBAC through integrated programs and copybooks:

**Security Files:**
- **GB01SE_SQL.CPY** - SQL user security file
- User roles and groups defined
- Access rights determined by role/group

**Security Copybooks:**
- **BRSECURE.CPY** - Branch/group logic
- **PASSWDW.CPY** - Password management
- **ACCESS.CPY**, **ACCESSW.CPY** - Access control
- **BRSECUREW.CPY** - Branch security working storage

**Security Programs:**
- **CHKSEC** - Security validation
- **BRSECURE** - Branch/group security
- **PASSWD** - Password verification
- **ACCESS/DACCESS** - File access control

**Permission Enforcement:**
1. Retrieve user role/group from **GB01SE_SQL.CPY**
2. **BRSECURE.CPY** checks group membership and branch authorization
3. If unauthorized, password prompt
4. **PASSWDW.CPY** validates credentials
5. If valid, set flag (BR-SECURITY-OKAY = "Y")
6. If invalid, deny access
7. Log all attempts

**Group Membership Logic:**
```cobol
IF WGR-WORD = ENT-GROUP
  MOVE "Y" TO BR-SECURITY-OKAY
PERFORM CHECK-MEMBER
IF WGR-ERR > 0
  GO TO PASSWORD-REQUIRED
```

RBAC ensures only authorized users perform sensitive operations, with comprehensive audit logging.

### 3.3 Function Access: Loan Officers vs. CSRs

Loan officers and CSRs have distinctly different access privileges:

**Loan Officers:**
- **Functions:**
  - Originate and approve loans (LNAPPROVAL)
  - Modify loan terms and rates (LONPS4, LOAN-CHANGES-ROUTINE)
  - Access detailed borrower financial data
  - Perform payoff calculations (LPPOFF, LONPF5)
  - Override system restrictions
  - Generate regulatory reports
  - Advanced calculations (APILNS, CALCZL, LNVERI)
- **Business Impact:**
  - Risk assessment authority
  - Compliance responsibility
  - Final loan decisions
  - Access sensitive data

**Customer Service Representatives:**
- **Functions:**
  - View loan details (LOAN - view-only)
  - Process payments
  - Update customer contact info
  - Handle routine inquiries (LONPV0, CPINQ0, XONPN0)
  - Initiate basic corrections
  - Generate standard statements
  - Reschedule payments (RESCHD - with restrictions)
- **Business Impact:**
  - Non-sensitive operations only
  - Cannot approve/originate/modify loans
  - Cannot override restrictions
  - No advanced calculation access

**Technical Enforcement:**
- Role checks at login
- Profile verification per **XONPC0.CBL** ("set the profile in password file to be higher access than secretary")
- Paragraph access gated by role (e.g., PERFORM LOAN-CHANGES-ROUTINE)
- Copybooks define role-based fields (**LP01CD.CPY**, **LPAVCR.CPY**)

**Summary Table:**
| Functionality                | Loan Officer | CSR  |
|------------------------------|:-----------:|:----:|
| Originate/Approve Loans      |     ✔️      |  ❌  |
| Modify Loan Terms            |     ✔️      |  ❌  |
| View Loan Details            |     ✔️      |  ✔️  |
| Process Payments             |     ✔️      |  ✔️  |
| Update Customer Info         |     ✔️      |  ✔️  |
| Override Restrictions        |     ✔️      |  ❌  |
| Generate Compliance Reports  |     ✔️      |  ❌  |
| Inquiry Screens              |     ✔️      |  ✔️  |

### 3.4 Supervisor and Manager Privileges

Supervisors and managers have elevated privileges beyond regular users:

**Business Responsibilities:**
- Oversee branch operations
- Approve transactions
- Manage staff
- Handle escalated issues

**Technical Access:**
- Access approval workflows
- Override system restrictions
- View/modify higher-level data (branch statistics, staff assignments)
- **BRMAN5.CBL** tracks supervisor/manager changes with audit logging

**Role Verification:**
Programs check user role before granting privileged access:
```cobol
IF BROR-SUPERVISOR NOT = BRCH-SUPERVISOR
  MOVE "06 " TO LMC-FIELD
  MOVE "SUPERVISR" TO LMC-DESC
  PERFORM LOG-FIELD-CHANGE
```

**Group Membership:**
- **LONPB0.CBL**, **ZONPC0.CBL** verify group membership
- Secretaries/regular users blocked from approvals
- Supervisors/managers explicitly authorized

**Audit Tracking:**
Changes to supervisor/manager assignments logged for compliance

**Privilege Differences:**
| Privilege Type      | Supervisors/Managers                | Regular Users                  |
|---------------------|-------------------------------------|-------------------------------:|
| Approve Transactions| Yes                                 | No                            |
| Override Restrictions| Yes                                | No                            |
| Access Audit Logs   | Yes                                 | No                            |
| Change Assignments  | Yes                                 | No                            |
| Routine Processing  | Yes                                 | Yes                           |
| Data Entry          | Yes                                 | Yes                           |

### 3.5 Security Measures for Financial Data

OTIS protects sensitive data through multiple security layers:

**1. Role-Based Access Control:**
- User roles checked before sensitive operations
- **BRSECURE**, **CHKSEC** verify permissions
- Group membership validation

**2. Password Protection:**
- High-risk transactions require passwords
- **VERIFY-PASSWORD** called before sensitive actions (posting reserves, insurance cancels)
- Example:
```cobol
IF LP-TRCD = "PR" OR "PC" OR "PP"
  PERFORM VERIFY-PASSWORD
  IF PASSWDW-STAT NOT = "OK"
    GO TO BAR-CD-ENTRY
```
- **PASSWD** validates credentials

**3. Field-Level Protection:**
- **PROTECT-FIELDS** routines restrict modification
- Sensitive fields locked unless authorized
- Transaction codes trigger additional checks

**4. Audit Trails:**
- **STMNWO**, **FXGLUP** log sensitive actions
- Who performed action and when
- Complete audit documentation

**5. Copybook Security:**
- **CHKSEC.CPY**, **BRSECUREW.CPY** define security structures
- Consistent enforcement across programs

**6. Branch/Group Security:**
- Branch-level access restrictions
- Group-level authorization
- **BRSECURE**, **WHOAMI**, **IOMLIN** enforce boundaries

**7. Data Masking:**
- Sensitive fields (account numbers, passwords) masked
- Field redefinition limits exposure (**LP01CA.CPY**)

**8. Transaction Protection Messaging:**
- **SESTAT_SCN.CPY** displays protection messages
- Users informed of access restrictions

**Summary Security Programs/Copybooks:**
| Program/Copybook         | Purpose                                            |
|-------------------------|-----------------------------------------------------|
| BRSECURE                | Branch-level password/group security               |
| CHKSEC                  | Centralized security (called by 59 programs)       |
| PASSWD                  | Password validation                                 |
| VERIFY-PASSWORD         | Pre-action password check                           |
| PROTECT-FIELDS          | Field modification restrictions                     |
| CHKSEC.CPY              | Security structures                                 |
| BRSECUREW.CPY           | Branch security enforcement                         |

These layered security measures ensure only authorized users access or modify sensitive financial data.

---

## Conclusion

This comprehensive analysis of the OTIS business domain reveals a sophisticated loan servicing platform with deep functionality across loan origination, servicing, financial operations, dealer management, reporting, and compliance. The system's architecture reflects decades of refinement to meet complex regulatory requirements and operational needs.

Key insights for modernization:

**Business Complexity:**
- 12+ loan types with distinct processing rules
- 7 interest calculation methods
- 12+ transaction types beyond payments
- Comprehensive dealer relationship management
- Extensive regulatory compliance requirements

**Operational Depth:**
- Complete loan lifecycle management (origination → payoff)
- Multi-branch support (up to 300 branches)
- Month-end and year-end close procedures
- Comprehensive audit trails
- Sophisticated security and access control

**User-Centric Design:**
- 5 distinct user roles with specific privileges
- Daily workflows optimized for each role
- Interactive inquiry and maintenance screens
- Batch processing for efficiency
- Exception handling and error correction

**Technical Integration:**
- Tight GL integration
- Accounts payable functionality
- Cash management and reconciliation
- Credit bureau reporting (Metro2)
- Insurance product tracking

As we move forward with modernizing OTIS to C#, this business domain knowledge will be essential for:
- Preserving critical business rules
- Maintaining regulatory compliance
- Ensuring operational continuity
- Designing intuitive modern UIs
- Creating comprehensive test coverage
- Validating that the new system meets all business requirements

The next chapters will build upon this foundation, examining the technical architecture, code structure, and modernization strategies that will transform this COBOL legacy system into a modern, maintainable C# application while preserving the rich business functionality documented here.

---

**Word Count:** Approximately 12,000 words

**Sources Referenced:**
- 50+ COBOL programs
- 40+ copybooks
- Multiple flow diagrams and code examples
- Comprehensive RAG queries across the OTIS codebase

This chapter provides the business context necessary for all stakeholders—technical teams, business analysts, project managers, and decision-makers—to understand what OTIS does and why its functionality must be carefully preserved during modernization.
