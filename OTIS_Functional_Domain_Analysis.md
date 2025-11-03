# OTIS Functional Domain Breakdown Analysis

## Executive Summary

The OTIS (Otis Timeshare Servicing) system is a comprehensive COBOL-based loan servicing platform organized into distinct functional domains that mirror the business processes of financial institutions. This document details the major and minor domains based on actual system analysis.

---

## Major Functional Domains

### 1. **Loan Processing & Maintenance**
**Business Purpose:** Complete lifecycle management of loan accounts

**Core Functions:**
- Loan origination and account setup
- Payment posting and processing
- Loan modifications and adjustments
- Payoff calculations and processing
- Loan inquiry and reporting
- Rescission and reversal operations
- Loan closing procedures

**Key Programs:**
- LONPFC, LONPFB, LONPG3, LONPG4, LONPG6, LONPG7, LONPG8
- LONPF9, LONPF3, LONPY0, LPMAIN
- F3-LOANS, LONPS3, LONPS4, LONPT0, LONPU

**Menus:**
- LPMENU (Loan Processing Menu)
- LPLSMU, LPL2MU (Loan Listing Menus)

**Sub-Domains:**
- Payment Processing (APIPAY, REFUPD, LONPW9, REGPAY)
- Loan Inquiry (CPINQ0, XPINQ, LTINFO)
- Loan Maintenance (LP01LN, LP01LP, LP01LS)

---

### 2. **Dealer & Addon Management**
**Business Purpose:** Manage dealer relationships and addon products/services

**Core Functions:**
- Dealer master file maintenance
- Dealer statistics and reporting
- Dealer memo and transaction records
- Addon product management (insurance, warranties, service contracts)
- Insurance company records
- Addon reversals and adjustments

**Key Programs:**
- DLMAIN, ADMAIN, DLMAN3 (Dealer Management)
- LP01DL (Dealer Master)
- LP01DM (Dealer Memo)
- LP01DT (Dealer Trans)
- LONPFF (Reversal for Addons)
- LONPFE (Addons, Service Contracts)
- LP01IC (Insurance Company)

**Menus:**
- FDMENU (File/Dealer Maintenance Menu)
- ASMENU (Addon Servicing Menu)

**Sub-Domains:**
- Dealer Statistics & Analysis
- Addon Product Management
- Insurance Processing

---

### 3. **Batch & Daily Processing**
**Business Purpose:** Automated scheduled operations and bulk data processing

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
- PLPURG, BWPURG (Purge operations)
- AC1DAY, RZ1DAY (Daily routines)

**Menus:**
- BPMENU (Batch Processing Menu)
- EOMENU (End-of-Month Menu)

**Sub-Domains:**
- Daily Processing Cycles
- Month-End Closing
- Year-End Processing
- Batch Reporting

---

### 4. **Inquiry & Reporting**
**Business Purpose:** Data access, analysis, and regulatory reporting

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
- DBREPT (Database reporting)
- PILRPT (Potential Increased Loan Report)

**Menus:**
- SPLIST (Specialized Listing Menu)
- PERUSE (Inquiry Menu)
- CLMENU (Collection Listing Menu)

**Sub-Domains:**
- Transaction Inquiries
- Audit & Compliance Reports
- Statistical Analysis
- Management Reporting

---

### 5. **Branch & Group Management**
**Business Purpose:** Organizational hierarchy and multi-location support

**Core Functions:**
- Branch record maintenance
- Group configuration and management
- Routing specifications
- Branch-level reporting
- Organization hierarchy management (Corporate → Region → Branch)
- Branch statistics and performance tracking

**Key Programs:**
- BRMAN2, BRMAN5, BRMAN7 (Branch maintenance)
- BPBRMN, BROPM1 (Branch operations)
- GPMAN1, GPOPM1, GPOPM2 (Group management)
- EMPORG, EMPLRG (Employee organization)
- MBCORP (Corporate batch)

**Copybooks:**
- GB01GR.CPY (Organization levels)
- ARRAYBR.CPY (Branch processing arrays)

**Sub-Domains:**
- Branch Configuration
- Group Operations
- Organization Hierarchy
- Branch Performance

---

### 6. **Security & Access Control**
**Business Purpose:** System security, user management, and audit compliance

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

**Copybooks:**
- CHKSEC.CPY (Security checks)
- ACCESS.CPY (Account access)

**Menus:**
- SCMENU (Security Menu)
- UPMENU (User Profile Menu)

**Sub-Domains:**
- User Authentication
- Authorization Management
- Audit Logging
- Access Control

---

### 7. **Screen & Menu Management**
**Business Purpose:** User interface and navigation control

**Core Functions:**
- Menu navigation and routing
- Screen layout definition
- User input handling
- Field validation and formatting
- Screen flow control
- Help system integration

**Key Programs:**
- OPMENU, PGMENU (Main menus)
- GPMAN1, GPMAN2, GPMAN3, GPMAN4 (General maintenance screens)
- BRMAN5, BROPM1 (Branch screens)
- LONPY_SCN, LPSMNU_SCN (Loan screens)

**Copybooks:**
- Various _SCN.CPY files (Screen definitions)
- GPMAN1_SCN.CPY, GPMAN2_SCN.CPY, etc.

**Sub-Domains:**
- Menu Systems
- Screen Navigation
- Input Validation
- Help System

---

### 8. **Reference Data & Code Tables**
**Business Purpose:** System configuration and lookup tables

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

### 9. **Utility & Support Modules**
**Business Purpose:** Technical infrastructure and support services

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

**Copybooks:**
- GETFMW.CPY (Form/environment setup)
- GETDLS.CPY (Daylight savings logic)
- HELPLINK.CPY (Help system)
- FERRORS.CPY (File error reporting)

**Menus:**
- UTMENU (Utility Menu)
- SEMENU (System Environment Menu)

**Sub-Domains:**
- System Configuration
- Date/Time Services
- Error Handling
- File Management

---

### 10. **Collections & Delinquency Management**
**Business Purpose:** Manage delinquent accounts and collection processes

**Core Functions:**
- Delinquency tracking
- Collection workflow management
- Follow-up actions
- Recovery operations
- Legal processing

**Key Programs:**
- Collection-focused programs
- Delinquency tracking modules

**Menus:**
- COMENU (Collection Operations Menu)
- CLMENU (Collection Listing Menu)

---

### 11. **General Ledger & Accounting**
**Business Purpose:** Financial accounting and reconciliation

**Core Functions:**
- GL entry posting
- Account reconciliation
- Financial reporting
- Ledger updates

**Key Programs:**
- GL posting programs
- Reconciliation modules

**Menus:**
- GLMENU (General Ledger Menu)

---

### 12. **Insurance & Recovery**
**Business Purpose:** Insurance claims and asset recovery

**Core Functions:**
- Insurance claim processing
- Recovery operations
- Financial adjustments

**Menus:**
- IRMENU (Insurance/Recovery Menu)

---

### 13. **Title Work & Legal**
**Business Purpose:** Title management and legal operations

**Core Functions:**
- Title transfers
- Lien releases
- Legal documentation

**Menus:**
- TWMENU (Title Work Menu)

---

### 14. **Fixed Assets**
**Business Purpose:** Asset tracking and depreciation

**Core Functions:**
- Fixed asset management
- Depreciation calculations
- Asset tracking

**Menus:**
- FXMENU (Fixed Asset Menu)

---

## System Architecture Layers

### Presentation Layer
- **Components:** Screens, menus, user interface
- **Examples:** PGMENU, WIMENU, screen copybooks (_SCN.CPY files)
- **Purpose:** User input/display and navigation

### Business Logic Layer
- **Components:** Core processing modules
- **Examples:** LONPG7, LONPW1, DLMAIN, DPFILE, LTCREA, CDMAIN
- **Purpose:** Business rules, validation, processing, routing

### Data Layer
- **Components:** File handlers, copybooks, data structures
- **Examples:** Loan files, dealer files, addon files, code tables
- **Purpose:** Persistent storage and data management

---

## Transaction Processing Taxonomy

### Payment Transactions
- PL - Payment/Loan Payment
- RP - Regular Payment
- P2 - Payment Type 2
- OT - Other Charges
- RE - Reversal/Repo Transaction

### Account Transactions
- RV - Reversal
- BK - Bankruptcy Filing
- BD - Bankruptcy Discharge
- O2 - Other Charges Type 2

### Special Processing
- Principal/Interest Adjustments
- Rebate Calculations
- Supporting Calculations

---

## Key Observations

### Modular Organization
- Each domain has dedicated programs, menus, and copybooks
- Clear separation of concerns
- Hierarchical menu structure

### Business-Driven Design
- Domains align with business processes
- Financial services industry focus
- Loan lifecycle support

### Technical Architecture
- Layered structure (presentation, logic, data)
- Reusable components (copybooks)
- Batch and online processing support

### Scale & Complexity
- Hundreds of programs
- Thousands of copybooks
- Multiple integration points
- Complex business rules

---

## Recommendations for Modernization

### Domain-Driven Design
- Preserve domain boundaries in modern architecture
- Map each domain to microservices or modules
- Maintain business logic integrity

### API Organization
- Create API endpoints aligned with functional domains
- Loan API, Dealer API, Payment API, etc.
- RESTful or GraphQL interfaces

### Data Migration Strategy
- Migrate by domain to minimize risk
- Maintain data relationships
- Preserve business rules

### User Interface Modernization
- Recreate menu structures in web UI
- Maintain familiar workflows
- Progressive enhancement approach

---

## Source References

This analysis is based on:
- OTIS/OTOS program flows and descriptions
- Menu structures and navigation paths
- Copybook definitions and usage
- Business logic embedded in COBOL programs
- System documentation and comments

**Key Indexes Used:**
- new_cobol_programs (program metadata)
- new_cobol_program_deps (program dependencies)
- new_cobol_flow_edges_v2 (program flows)
- new_cobol_menu_trees (menu structures)
- new_cobol_ui_paths (UI navigation)
- new_cobol_code (source code analysis)
- new_cobol_copybook_usage (copybook relationships)

---

## Conclusion

The OTIS system demonstrates a well-organized functional domain structure that clearly maps business processes to technical components. This organization provides a solid foundation for modernization efforts, with each domain representing a potential microservice or module in a modern architecture. The clear separation between loan processing, dealer management, batch operations, and support functions allows for incremental migration while maintaining system integrity.
