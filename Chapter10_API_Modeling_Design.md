# Chapter 10: API Modeling and Design

## Introduction

The transformation from COBOL programs to modern REST APIs represents one of the most critical architectural shifts in the migration journey. This chapter addresses the strategic design decisions required to expose COBOL business logic and data through well-designed, RESTful API endpoints that can support modern web and mobile applications. The goal is not simply to create a thin wrapper around legacy code, but to thoughtfully model APIs that are intuitive, maintainable, and aligned with industry best practices.

The process begins with understanding how to identify which COBOL programs should become public API endpoints versus internal service methods, and how to map COBOL's procedural operations to resource-oriented REST patterns. COBOL programs were designed around file operations and screen flows; APIs must be designed around business resources and workflows. This requires careful analysis of program purpose, data access patterns, and transaction boundaries to create cohesive service boundaries that make sense in a modern architecture.

Data transformation is equally critical—COBOL's rich type system with PICTURE clauses, REDEFINES, and packed decimals must be mapped to C# DTOs that are both faithful to the business domain and convenient for API consumers. Finally, designing CRUD operations and business workflows requires understanding how to represent complex multi-step COBOL processes as stateless or stateful API interactions, implementing validation, error handling, and transaction management in ways that preserve business logic integrity while embracing modern API patterns. This chapter provides a comprehensive framework for API design that balances pragmatism with architectural excellence.

## Section 10.1: Translating COBOL Programs to API Endpoints

### Identifying API Candidates and Resource Mapping

**Q1: What are the key principles for identifying which COBOL programs in the OTIS system should become REST API endpoints versus internal service methods?**

To determine which COBOL programs in the OTIS system should be exposed as REST API endpoints versus kept as internal service methods, follow these key principles:

---

**1. Business Functionality & User Interaction**
- **REST API Endpoints:**  
  - Programs that represent distinct business operations needed by external systems, web/mobile apps, or integration partners.
  - Examples: Payment processing (APIPAY), loan creation, customer lookup, account status retrieval.
  - Should encapsulate a complete business transaction or query.
- **Internal Service Methods:**  
  - Programs that perform supporting tasks, calculations, or data transformations not directly needed by external consumers.
  - Examples: Utility routines, file format conversions, internal validation modules.

---

**2. Data Ownership & Security**
- **REST API Endpoints:**  
  - Programs that access or modify data entities meant to be shared or updated by external clients.
  - Must enforce authentication, authorization, and data validation.
- **Internal Service Methods:**  
  - Programs that handle sensitive internal data, batch processing, or system maintenance.
  - Should not expose internal-only data structures or logic.

---

**3. Granularity & Transaction Boundaries**
- **REST API Endpoints:**  
  - Should provide coarse-grained operations (e.g., "Process Payment", "Get Loan Details") that map to business use cases.
  - Avoid exposing fine-grained internal steps (e.g., "Update Ledger Row", "Validate Field X") as APIs.
- **Internal Service Methods:**  
  - Handle fine-grained, reusable logic called by multiple programs but not meaningful as standalone API operations.

---

**4. Error Handling & Messaging**
- **REST API Endpoints:**  
  - Must return clear, standardized error messages and status codes suitable for external clients.
  - Example: Use EXT-API-MESS to pass error details (see [[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]], lines 351-375).
- **Internal Service Methods:**  
  - Can use internal error handling conventions, logging, or propagate errors up to API-level programs.

---

**5. Screen & UI Dependencies**
- **REST API Endpoints:**  
  - Should not depend on interactive screen logic or require user input via terminal screens.
  - Programs with logic to disable screen interactions (e.g., EXT-API-SCREEN-DISABLE) are candidates for API adaptation.
  - Example:  
    ```
    01  EXT-API-WORKERS EXTERNAL.
        03  EXT-API-SCREEN-DISABLE PIC X(1).
        03  EXT-API-MESS PIC X(61).
    ```
    (Source: [[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]], lines 351-375)
- **Internal Service Methods:**  
  - May include screen display, menu navigation, or require user prompts.

---

**6. Reusability & Modularity**
- **REST API Endpoints:**  
  - Programs designed for modular, stateless operation with clear input/output contracts.
- **Internal Service Methods:**  
  - Programs tightly coupled to batch jobs, session state, or legacy workflows.

---

**7. Performance & Scalability**
- **REST API Endpoints:**  
  - Should be optimized for real-time, stateless requests with predictable performance.
- **Internal Service Methods:**  
  - May be suitable for long-running, batch, or background processing.

---

### Example Application in OTIS

- **APIPAY**: Processes payments, validates input, updates accounts—ideal REST API endpoint.
- **REFUPD**: Updates ledger, called by APIPAY—internal service method.
- **Programs with EXT-API-SCREEN-DISABLE logic**: Designed to run without screen interaction, suitable for API exposure.
- **Programs with menu/screen dependencies**: Should remain internal unless refactored.

---

**Summary Table**

| REST API Endpoint Candidates | Internal Service Methods      |
|-----------------------------|------------------------------|
| APIPAY (payment processing) | REFUPD (ledger update)       |
| Customer lookup modules     | File conversion utilities    |
| Loan creation modules       | Batch job controllers        |
| Programs using EXT-API-MESS | Screen/menu navigation logic |

---

**Source References:**
- EXT-API-WORKERS, EXT-API-SCREEN-DISABLE, EXT-API-MESS for API adaptation (Source: [[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]], lines 351-375)
- API error handling and messaging conventions (Source: [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]], lines 451-475)
- Business process encapsulation in APIPAY (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows)

---

**Here's a visual representation:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">OTIS API Endpoint Selection Principles</text>
  
  <!-- REST API Endpoints -->
  <rect x="60" y="80" width="320" height="220" fill="#e0f7fa" stroke="#0097a7" stroke-width="3" rx="12"/>
  <text x="220" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#006064">REST API Endpoints</text>
  <text x="80" y="140" font-size="14" fill="#006064">• Business operations</text>
  <text x="80" y="160" font-size="14" fill="#006064">• Data access for external clients</text>
  <text x="80" y="180" font-size="14" fill="#006064">• No screen/UI dependencies</text>
  <text x="80" y="200" font-size="14" fill="#006064">• Clear error messaging</text>
  <text x="80" y="220" font-size="14" fill="#006064">• Coarse-grained transactions</text>
  <text x="80" y="240" font-size="14" fill="#006064">• Example: APIPAY</text>
  <text x="80" y="260" font-size="14" fill="#006064">• Uses EXT-API-MESS</text>
  
  <!-- Internal Service Methods -->
  <rect x="520" y="80" width="320" height="220" fill="#f3e8ff" stroke="#8e24aa" stroke-width="3" rx="12"/>
  <text x="680" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#6a1b9a">Internal Service Methods</text>
  <text x="540" y="140" font-size="14" fill="#6a1b9a">• Utility routines</text>
  <text x="540" y="160" font-size="14" fill="#6a1b9a">• Batch processing</text>
  <text x="540" y="180" font-size="14" fill="#6a1b9a">• Screen/menu logic</text>
  <text x="540" y="200" font-size="14" fill="#6a1b9a">• Fine-grained steps</text>
  <text x="540" y="220" font-size="14" fill="#6a1b9a">• Internal error handling</text>
  <text x="540" y="240" font-size="14" fill="#6a1b9a">• Example: REFUPD</text>
  <text x="540" y="260" font-size="14" fill="#6a1b9a">• Ledger/file updates</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="380" y1="190" x2="520" y2="190" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <text x="450" y="175" text-anchor="middle" font-size="13" fill="#333">Internal Calls</text>
</svg>

---

**In summary:**  
Expose programs as REST API endpoints if they represent business operations needed by external clients, are stateless, and have no UI dependencies. Keep internal service methods for supporting logic, batch jobs, and screen/menu routines. Use structures like EXT-API-MESS and EXT-API-SCREEN-DISABLE to adapt legacy programs for API use.

(Source: [[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]], [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]], APIPAY, OTIS system flows)

**Q2: How do we map COBOL program entry points (CALL statements, screen programs) to API resource endpoints following REST conventions?**

To map COBOL program entry points (CALL statements, screen programs) to API resource endpoints following REST conventions, you should follow a structured approach that aligns business operations with RESTful resource modeling. Here’s how to do it:

---

**1. Identify Business Resources and Operations**

- **Resource Identification:**  
  Analyze COBOL programs and screens to determine the core business entities they manage (e.g., Loan, Payment, Account, Statement).
  - Example: A program that processes payments (e.g., APIPAY) maps to the "Payment" resource.
  - Example: A screen for account inquiry maps to the "Account" resource.

- **Operation Mapping:**  
  For each program entry point (CALL statement, main module, or screen), determine the business operation:
  - Create (e.g., new loan, new payment)
  - Read (e.g., account inquiry, statement view)
  - Update (e.g., modify borrower info)
  - Delete (e.g., purge loan record)

---

**2. REST Endpoint Design Principles**

- **Resource-Oriented URLs:**  
  Map each business entity to a REST resource path:
  - `/loans` for loan-related operations
  - `/payments` for payment processing
  - `/accounts` for account management

- **HTTP Method Mapping:**  
  Align COBOL operations with HTTP verbs:
  - `GET /loans/{id}` → Inquiry programs/screens (read-only)
  - `POST /payments` → Payment processing programs (create)
  - `PUT /accounts/{id}` → Update programs (modify)
  - `DELETE /loans/{id}` → Purge/archive programs (delete)

- **Entry Point to Endpoint Mapping:**  
  - **Screen Programs:**  
    If a COBOL screen is the main entry for a business function (e.g., loan creation), map it to the corresponding REST endpoint.
    - Example: The LNCREA program (loan creation) becomes `POST /loans`
    - Example: The LNARCH program (loan archive) becomes `DELETE /loans/{id}`

  - **CALL Statements:**  
    For internal service programs called via `CALL`, expose only those that represent atomic business actions or queries as endpoints.
    - Example: APIPAY (called for payment processing) → `POST /payments`
    - Internal utility calls (e.g., validation routines) remain internal and are not mapped to endpoints.

---

**3. Example Mapping Table**

| COBOL Program/Screen | Business Resource | REST Endpoint         | HTTP Method | Operation         |
|----------------------|-------------------|----------------------|-------------|-------------------|
| APIPAY               | Payment           | /payments            | POST        | Create Payment    |
| LNCREA               | Loan              | /loans               | POST        | Create Loan       |
| LNARCH               | Loan              | /loans/{id}          | DELETE      | Archive Loan      |
| ACCOUNT-INQUIRY      | Account           | /accounts/{id}       | GET         | Get Account Info  |
| STATEMENT-VIEW       | Statement         | /statements/{id}     | GET         | View Statement    |

---

**4. Mapping Workflow**

- **Step 1:** Catalog all COBOL entry points (main modules, screens, CALL targets).
- **Step 2:** For each, determine the business resource and CRUD operation.
- **Step 3:** Design RESTful endpoints using plural nouns and resource IDs.
- **Step 4:** Map HTTP methods to the business operation.
- **Step 5:** Expose only business-facing entry points as API endpoints; keep utility/service programs internal.

---

**5. Example: Payment Processing Flow**

- COBOL Entry Point: APIPAY (main module or called via CALL)
- Business Resource: Payment
- REST Endpoint: `POST /payments`
- HTTP Method: POST
- Operation: Accept and process a new payment

---

**6. Visual Representation**

Here's a visual representation of how COBOL entry points map to REST API endpoints:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Entry Points to REST API Endpoints</text>
  
  <!-- COBOL Programs -->
  <rect x="50" y="80" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">APIPAY</text>
  <text x="140" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Payment Processing</text>
  
  <rect x="50" y="180" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">LNCREA</text>
  <text x="140" y="230" text-anchor="middle" font-size="12" fill="#6366f1">Loan Creation</text>
  
  <rect x="50" y="280" width="180" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="310" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">LNARCH</text>
  <text x="140" y="330" text-anchor="middle" font-size="12" fill="#6366f1">Loan Archive</text>
  
  <!-- REST Endpoints -->
  <rect x="670" y="80" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="760" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">POST /payments</text>
  <text x="760" y="130" text-anchor="middle" font-size="12" fill="#22c55e">Create Payment</text>
  
  <rect x="670" y="180" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="760" y="210" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">POST /loans</text>
  <text x="760" y="230" text-anchor="middle" font-size="12" fill="#22c55e">Create Loan</text>
  
  <rect x="670" y="280" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="760" y="310" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">DELETE /loans/{id}</text>
  <text x="760" y="330" text-anchor="middle" font-size="12" fill="#22c55e">Archive Loan</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="670" y2="110" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="230" y1="210" x2="670" y2="210" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="230" y1="310" x2="670" y2="310" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary of Approach:**
- Map each COBOL program entry point to a REST resource and operation.
- Use business entities as REST resources.
- Align CRUD operations with HTTP methods.
- Expose only business-facing entry points as API endpoints.
- Keep internal service methods (utility, validation, batch-only) as internal calls.

**Source References:**
- APIPAY, LNCREA, LNARCH program flows and business descriptions (Source: help_fields, flows, screen_nodes)
- REST conventions (industry standard, not specific to OTIS system)

If you need a detailed mapping for specific OTIS programs, provide their names or business functions for targeted analysis.

**Q3: What strategy should we use to convert COBOL transaction programs (e.g., APIPAY, REFUPD) into HTTP POST/PUT operations?**

To convert COBOL transaction programs like **APIPAY** and **REFUPD** into HTTP POST/PUT operations, use the following strategy:

---

### 1. **Identify Transactional Entry Points**
- Focus on COBOL programs that **initiate business transactions** (e.g., payment posting, batch updates).
- Example: APIPAY processes single payment reference codes; REFUPD updates batch payments.
  - (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: code; [[FILE:REFUPD.CBL|REFUPD.CBL]], Index: code)

### 2. **Map COBOL Inputs to API Request Bodies**
- Analyze the COBOL program’s input data structures (e.g., BT-REC, payment details).
- Define JSON schemas for API requests that mirror these structures.
  - Example: APIPAY expects a data string with payment info; this becomes the POST body.

### 3. **Align COBOL Actions with RESTful Semantics**
- **POST**: Use for creating new transactions (e.g., posting a payment).
- **PUT**: Use for updating existing resources (e.g., updating a batch payment).
- Each COBOL transaction maps to a resource-oriented endpoint:
  - `/payments` → POST for new payment (APIPAY)
  - `/batch-payments/{id}` → PUT for update (REFUPD)

### 4. **Handle Transactional Side Effects**
- Ensure the API triggers all necessary COBOL logic (validation, logging, updates).
- Example: APIPAY performs validation, calls logging routines, and updates payment files.
  - (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: code, lines 1651-1675)

### 5. **Map COBOL Output/Status to API Responses**
- Translate COBOL return codes, error messages, and output data into HTTP status codes and JSON responses.
  - Success → HTTP 200/201 with transaction details
  - Failure → HTTP 400/500 with error message (e.g., "PMT NOT APPLIED")

### 6. **Preserve Atomicity and Error Handling**
- Ensure each API call wraps the COBOL transaction in an atomic operation.
- Map COBOL error handling (e.g., GO TO POST-PO-EXIT) to API error responses.

### 7. **Design for Idempotency**
- PUT operations should be idempotent (repeatable without side effects).
- POST operations should create new resources each time.

---

#### Example Mapping: APIPAY

- **COBOL Entry:** Shell script runs APIPAY with payment data string (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: code)
- **API Mapping:**  
  - **Endpoint:** `POST /payments`
  - **Request Body:** JSON with payment fields (account, amount, date, etc.)
  - **Response:** JSON with status, transaction ID, error message if any

#### Example Mapping: REFUPD

- **COBOL Entry:** Batch payment update, selects lowest batch file, processes update (Source: [[FILE:REFUPD.CBL|REFUPD.CBL]], Index: code)
- **API Mapping:**  
  - **Endpoint:** `PUT /batch-payments/{batchId}`
  - **Request Body:** JSON with batch update details
  - **Response:** JSON with update status, error details

---

### Visual Representation

Here's a visual representation of the strategy for converting COBOL transaction programs to RESTful POST/PUT endpoints:

<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Transaction → REST API Mapping</text>
  
  <!-- COBOL Programs -->
  <rect x="60" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">APIPAY</text>
  <text x="150" y="130" text-anchor="middle" fill="white" font-size="12">Single Payment</text>
  
  <rect x="60" y="180" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="210" text-anchor="middle" fill="white" font-size="16" font-weight="bold">REFUPD</text>
  <text x="150" y="230" text-anchor="middle" fill="white" font-size="12">Batch Payment Update</text>
  
  <!-- API Endpoints -->
  <rect x="360" y="80" width="220" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="470" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">POST /payments</text>
  <text x="470" y="130" text-anchor="middle" fill="white" font-size="12">Creates Payment</text>
  
  <rect x="360" y="180" width="220" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="470" y="210" text-anchor="middle" fill="white" font-size="16" font-weight="bold">PUT /batch-payments/{id}</text>
  <text x="470" y="230" text-anchor="middle" fill="white" font-size="12">Updates Batch Payment</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="240" y1="115" x2="360" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="240" y1="215" x2="360" y2="215" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- API Response -->
  <rect x="650" y="80" width="180" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="740" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">HTTP Response</text>
  <text x="740" y="130" text-anchor="middle" fill="white" font-size="12">Status, Data, Errors</text>
  
  <rect x="650" y="180" width="180" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="740" y="210" text-anchor="middle" fill="white" font-size="16" font-weight="bold">HTTP Response</text>
  <text x="740" y="230" text-anchor="middle" fill="white" font-size="12">Status, Data, Errors</text>
  
  <line x1="580" y1="115" x2="650" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="215" x2="650" y2="215" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
- Transactional COBOL programs become POST/PUT endpoints.
- Map COBOL input/output to JSON request/response.
- Preserve business logic, atomicity, and error handling.
- Use RESTful conventions for resource naming and HTTP verbs.

(Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: code; [[FILE:REFUPD.CBL|REFUPD.CBL]], Index: code; APIPAY, Index: programs; REFUPD, Index: programs)

**Q4: How can we identify COBOL programs that perform read-only operations and should become HTTP GET endpoints?**

To identify COBOL programs in the OTIS system that perform **read-only operations** and are suitable for exposure as HTTP **GET** endpoints, follow this structured approach:

---

### 1. **Look for Programs with Read-Only Data Access Patterns**
- **File Operations:** Programs that only OPEN files for INPUT, perform READs, and do not WRITE, REWRITE, or DELETE records.
- **SQL Operations:** Programs that execute SELECT statements but do not perform INSERT, UPDATE, or DELETE.
- **No Data Modification:** Absence of MOVE, WRITE, or UPDATE statements that change persistent data.

**Example:**
- Programs like **BW_READ** (Source: [[FILE:BW|BW]]_READ, Index: programs) are called by other programs but have no outgoing calls and are likely focused on data retrieval.
- Programs such as **SQLLC_READ_LC8** and **SQLLC_READ_NEXT_LC8** (Source: [[FILE:SQLLC|SQLLC]]_READ_LC8, SQLLC_READ_NEXT_LC8, Index: programs) are labeled as DATA_ACCESS and only called by others, indicating read-only SQL operations.

---

### 2. **Check Program Documentation and Comments**
- Look for comments or documentation indicating the program's purpose is inquiry, reporting, or lookup.
- Common keywords: "INQUIRY", "REPORT", "LOOKUP", "DISPLAY", "READ-ONLY".

**Example:**
- **[[FILE:SSINQ.CBL|SSINQ.CBL]]** (Source: [[FILE:SSINQ.CBL|SSINQ.CBL]], Index: code) uses copybooks for loan and borrower inquiry, suggesting read-only access.

---

### 3. **Analyze Program Flowcharts and Entry Points**
- Flowcharts showing only data retrieval steps (e.g., "READ-FILE", "DISPLAY-RECORD", "GET-INFO") without branches to update or write logic.
- Entry paragraphs named "READ", "INQUIRE", "GET", "SHOW", "DISPLAY".

**Example:**
- **CDREAD** (Source: [[FILE:CDREAD|CDREAD]], [[FILE:I|I]]ndex: flows) has entry paragraphs for reading and displaying data, with no perform edges for writing.

---

### 4. **Review Copybooks Used**
- Programs using copybooks with structures for inquiry or reporting (e.g., "INQ", "RPT", "DISP") are likely read-only.

---

### 5. **Exclude Programs with Transactional or Update Logic**
- Programs like **APIPAY** or **REFUPD** (from previous context) perform updates and should be mapped to POST/PUT, not GET.

---

## **Summary Table: Candidate GET Endpoint Programs**

| Program Name           | Read-Only Pattern         | Source Reference                  | Typical Use Case                |
|----------------------- |--------------------------|-----------------------------------|---------------------------------|
| BW_READ               | No outgoing calls, called by others | BW_READ, Index: programs         | Borrower record inquiry         |
| SQLLC_READ_LC8        | DATA_ACCESS, called by 1  | SQLLC_READ_LC8, Index: programs   | Loan code lookup                |
| SQLLC_READ_NEXT_LC8   | DATA_ACCESS, called by 1  | SQLLC_READ_NEXT_LC8, Index: programs | Sequential loan code read      |
| CDREAD                | Entry: READ-FILE, DISPLAY | CDREAD, Index: flows              | Code file inquiry               |
| [[FILE:SSINQ.CBL|SSINQ.CBL]]             | Uses inquiry copybooks    | [[FILE:SSINQ.CBL|SSINQ.CBL]], Index: code            | Loan/borrower inquiry           |
| GLFS1RN               | Only LOAD/START/CLOSE/READ| GLFS1RN, Index: flows             | General ledger file inquiry     |

---

## **How to Map to HTTP GET Endpoints**

- **Resource Naming:** Use nouns representing the data (e.g., /borrowers, /loans, /codes).
- **Parameters:** Map COBOL input fields (e.g., account number, branch code) to query parameters.
- **Endpoint Example:**  
  - COBOL: CALL BW_READ USING BRNO, ACCTNO  
  - REST: GET /borrowers?branch=BRNO&account=ACCTNO

---

## **Visual Representation: Read-Only Program Identification**

Here's a visual representation:

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">Identifying Read-Only COBOL Programs for GET Endpoints</text>
  
  <!-- Step 1: Data Access -->
  <rect x="50" y="70" width="200" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Read-Only Data Access</text>
  <text x="150" y="120" text-anchor="middle" font-size="12" fill="#6366f1">OPEN INPUT, READ, SELECT</text>
  
  <!-- Step 2: No Update Logic -->
  <rect x="350" y="70" width="200" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">No Update/Write Logic</text>
  <text x="450" y="120" text-anchor="middle" font-size="12" fill="#3b82f6">No WRITE, REWRITE, UPDATE</text>
  
  <!-- Step 3: Inquiry/Report Purpose -->
  <rect x="650" y="70" width="200" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="750" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Inquiry/Report Purpose</text>
  <text x="750" y="120" text-anchor="middle" font-size="12" fill="#22c55e">INQUIRY, REPORT, LOOKUP</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="250" y1="105" x2="350" y2="105" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="550" y1="105" x2="650" y2="105" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  
  <!-- Example Programs -->
  <rect x="200" y="200" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="260" y="230" text-anchor="middle" fill="white" font-size="14" font-weight="bold">BW_READ</text>
  <text x="260" y="248" text-anchor="middle" fill="white" font-size="11">Borrower Inquiry</text>
  
  <rect x="400" y="200" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="460" y="230" text-anchor="middle" fill="white" font-size="14" font-weight="bold">SQLLC_READ_LC8</text>
  <text x="460" y="248" text-anchor="middle" fill="white" font-size="11">Loan Code Lookup</text>
  
  <rect x="600" y="200" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="660" y="230" text-anchor="middle" fill="white" font-size="14" font-weight="bold">CDREAD</text>
  <text x="660" y="248" text-anchor="middle" fill="white" font-size="11">Code Inquiry</text>
  
  <!-- Arrows from criteria to programs -->
  <line x1="150" y1="140" x2="260" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="140" x2="460" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="750" y1="140" x2="660" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**References:**
- BW_READ (Source: [[FILE:BW|BW]]_READ, Index: programs)
- SQLLC_READ_LC8 (Source: [[FILE:SQLLC|SQLLC]]_READ_LC8, Index: programs)
- SQLLC_READ_NEXT_LC8 (Source: [[FILE:SQLLC|SQLLC]]_READ_NEXT_LC8, Index: programs)
- CDREAD (Source: [[FILE:CDREAD|CDREAD]], [[FILE:I|I]]ndex: flows)
- [[FILE:SSINQ.CBL|SSINQ.CBL]] (Source: [[FILE:SSINQ.CBL|SSINQ.CBL]], Index: code)
- GLFS1RN (Source: [[FILE:GLFS1RN|GLFS1RN]], [[FILE:I|I]]ndex: flows)

**In summary:**  
Focus on programs that only read and display data, have no update logic, and are used for inquiry/reporting. These are ideal candidates for HTTP GET endpoints in your REST API design.

### Transaction Programs and CRUD Operations

**Q5: What approach should we take to handle COBOL programs that perform multiple operations (read, validate, update) in a single API design?**

When converting COBOL programs that perform **multiple operations** (such as read, validate, and update) into an API design, you should use a **resource-oriented, operation-separating approach** to ensure clarity, maintainability, and RESTful compliance.

---

### **Recommended Approach**

#### 1. **Decompose by Operation Type**
- **Identify** each distinct business operation within the COBOL program:
  - **Read-only** (e.g., SELECT, file reads) → HTTP **GET**
  - **Validation** (e.g., business rule checks, eligibility) → HTTP **POST** or **GET** (if stateless)
  - **Update/Write** (e.g., file updates, database writes) → HTTP **PUT** or **POST**

#### 2. **Map to Separate API Endpoints**
- **Create distinct endpoints** for each operation, even if the COBOL program combines them:
  - `/resource/{id}` (GET) → Fetch data
  - `/resource/{id}/validate` (POST) → Validate data or business rules
  - `/resource/{id}` (PUT/POST) → Update or create data

#### 3. **Avoid Overloading a Single Endpoint**
- Do **not** expose all operations via a single API endpoint (e.g., `/resource` with multiple verbs or flags).
- Instead, **split** the logic so each endpoint has a clear, single responsibility.

#### 4. **Transaction-Oriented Endpoints (if needed)**
- If the COBOL program’s business logic requires **atomic multi-step operations** (e.g., validate then update), consider a **transaction endpoint**:
  - `/resource/{id}/process` (POST) → Accepts a payload, performs validation, then update as a single transaction.
- Document the workflow and error handling clearly.

#### 5. **Preserve Business Rules**
- Ensure that **validation logic** is preserved and surfaced in the API:
  - Return meaningful error codes/messages for validation failures.
  - Use HTTP status codes appropriately (400 for validation errors, 200 for success, etc.).

#### 6. **Document the Mapping**
- For each COBOL program, document:
  - Which operations map to which endpoints
  - What business rules are enforced at each step
  - Data flow between endpoints

---

### **Example Mapping**

Suppose a COBOL program (e.g., **APIPAY**) does the following:
- Reads account data
- Validates payment amount
- Updates payment record

**API Design:**
- `GET /accounts/{id}` → Read account data (maps to read logic)
- `POST /payments/validate` → Validate payment (maps to validation logic)
- `POST /payments` → Create payment (maps to update logic)

If the COBOL program requires validation before update, you can:
- Enforce validation in the `POST /payments` endpoint, or
- Require clients to call `/validate` first, then `/payments`

---

### **Why This Approach?**

- **Aligns with REST principles**: Each endpoint represents a resource and a single operation.
- **Improves maintainability**: Easier to test, monitor, and evolve each operation separately.
- **Enhances clarity**: Clients know exactly what each endpoint does.
- **Supports future extensibility**: New operations can be added as new endpoints.

---

### **Source Reference**

This approach is supported by analysis of COBOL program flows, such as APIPAY, which combines validation and update logic (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows). By separating these into distinct API endpoints, you maintain business integrity and system clarity.

---

#### **Visual Representation**

Here's a visual representation of the recommended API mapping for a multi-operation COBOL program:

<svg width="700" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">API Mapping for Multi-Operation COBOL Program</text>
  
  <!-- COBOL Program -->
  <rect x="50" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">COBOL Program</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="11">Read, Validate, Update</text>
  
  <!-- API Endpoints -->
  <rect x="350" y="80" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="410" y="105" text-anchor="middle" fill="white" font-size="14" font-weight="bold">GET /resource</text>
  <text x="410" y="125" text-anchor="middle" fill="white" font-size="11">Read</text>
  
  <rect x="350" y="170" width="120" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="410" y="195" text-anchor="middle" fill="white" font-size="14" font-weight="bold">POST /resource/validate</text>
  <text x="410" y="215" text-anchor="middle" fill="white" font-size="11">Validate</text>
  
  <rect x="350" y="260" width="120" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="410" y="285" text-anchor="middle" fill="white" font-size="14" font-weight="bold">POST /resource</text>
  <text x="410" y="305" text-anchor="middle" fill="white" font-size="11">Update</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="115" x2="350" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="230" y1="115" x2="350" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="230" y1="115" x2="350" y2="290" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
Decompose multi-operation COBOL programs into separate API endpoints for each operation (read, validate, update), following RESTful conventions. This ensures clarity, maintainability, and proper mapping of business logic.

**Q6: How do we design API endpoints for COBOL batch programs that process large datasets or perform bulk operations?**

To design API endpoints for COBOL **batch programs** that process large datasets or perform bulk operations, follow these best practices:

---

### 1. **Understand the COBOL Batch Workflow**

- COBOL batch programs like **BULKG1**, **BULKT1**, **BULKT2**, **BULKG2**, and **BULKTR** are designed to process large volumes of data in scheduled, non-interactive runs.
- They typically:
  - Read large input files or datasets
  - Perform calculations, aggregations, or transformations
  - Write output files or update records in bulk
  - May generate summary reports or logs

**Source Example:**  
- BULKG1, BULKT1, BULKT2, BULKG2, BULKTR all have flowcharts showing multi-step processing, e.g.:
  - INITIALIZATION → BULK-TRANSFER → CLASS-TOTALS → COMPANY-TOTALS → PRINT-RANGE-LINES  
  (Source: [[FILE:BULKG1|BULKG1]], [[FILE:BULKT1|BULKT1]], [[FILE:BULKT2|BULKT2]], [[FILE:BULKG2|BULKG2]], [[FILE:BULKTR|BULKTR]], [[FILE:I|I]]ndex: flows)

---

### 2. **API Endpoint Design Principles**

#### **A. Use Asynchronous Operations**
- **Bulk operations** can be time-consuming. Design endpoints to accept requests and process them asynchronously.
- Example:  
  - **POST /bulk-payments**: Accepts a file or dataset, returns a job ID.
  - **GET /bulk-payments/{jobId}/status**: Returns processing status.
  - **GET /bulk-payments/{jobId}/results**: Returns output or summary.

#### **B. Accept Large Payloads Efficiently**
- Support file uploads (CSV, JSON, etc.) or references to data locations (e.g., S3 bucket, FTP path).
- Use streaming or chunked uploads for very large files.

#### **C. Provide Job Tracking**
- Return a **job ID** for each submitted batch.
- Allow clients to poll for status, errors, and results.

#### **D. Expose Summary and Detail Retrieval**
- Separate endpoints for:
  - **Summary results** (totals, counts, errors)
  - **Detailed output** (processed records, logs)

#### **E. Error Handling and Reporting**
- Provide endpoints to retrieve error logs or failed records.
- Example: **GET /bulk-payments/{jobId}/errors**

---

### 3. **Mapping COBOL Batch Logic to API Endpoints**

#### **Typical COBOL Batch Flow (from context):**
- INITIALIZATION → BULK-TRANSFER → CLASS-TOTALS → COMPANY-TOTALS → PRINT-RANGE-LINES

#### **API Endpoint Mapping:**

| COBOL Step         | API Endpoint Example                | HTTP Method | Description                                 |
|--------------------|-------------------------------------|-------------|---------------------------------------------|
| INITIALIZATION     | /bulk-ops/init                      | POST        | Initialize batch job (optional)             |
| BULK-TRANSFER      | /bulk-ops                          | POST        | Submit bulk data for processing             |
| CLASS-TOTALS       | /bulk-ops/{jobId}/summary           | GET         | Retrieve summary totals                     |
| COMPANY-TOTALS     | /bulk-ops/{jobId}/company-totals    | GET         | Retrieve company-level totals               |
| PRINT-RANGE-LINES  | /bulk-ops/{jobId}/output            | GET         | Download processed output/report            |

---

### 4. **Example API Workflow for BULKG1/BULKT1/BULKT2**

1. **Client submits bulk data:**
   - `POST /bulk-ops` with file or data reference
   - API responds with `{ jobId: "12345" }`

2. **Client checks status:**
   - `GET /bulk-ops/12345/status`
   - Response: `{ status: "processing" }`

3. **Client retrieves results:**
   - `GET /bulk-ops/12345/summary`
   - `GET /bulk-ops/12345/output`
   - `GET /bulk-ops/12345/errors`

---

### 5. **Additional Considerations**

- **Scalability:** Use background workers or queues to process jobs.
- **Security:** Authenticate and authorize bulk operations.
- **Idempotency:** Ensure repeated submissions do not duplicate processing.
- **Pagination:** For large result sets, support pagination or streaming.

---

### 6. **Visual Representation**

Here's a visual representation of the API design for COBOL batch/bulk programs:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">API Design for COBOL Batch/Bulk Operations</text>
  
  <!-- Client -->
  <rect x="50" y="100" width="120" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="110" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Client</text>
  
  <!-- API Gateway -->
  <rect x="250" y="100" width="160" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="330" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">API Gateway</text>
  
  <!-- Batch Job Queue -->
  <rect x="500" y="100" width="160" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="580" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Batch Job Queue</text>
  
  <!-- COBOL Batch Processor -->
  <rect x="700" y="100" width="160" height="60" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="780" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">COBOL Batch Processor</text>
  <text x="780" y="150" text-anchor="middle" font-size="12" fill="#b45309">BULKG1, BULKT1, BULKT2, BULKG2, BULKTR</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="170" y1="130" x2="250" y2="130" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="410" y1="130" x2="500" y2="130" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="660" y1="130" x2="700" y2="130" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  
  <!-- Status/Results Arrows -->
  <line x1="780" y1="170" x2="780" y2="220" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <rect x="700" y="220" width="160" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="780" y="250" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">Results/Status</text>
  <line x1="700" y1="250" x2="330" y2="250" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="330" y1="250" x2="110" y2="250" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <rect x="50" y="220" width="120" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="110" y="250" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">Client</text>
</svg>

---

### **Summary**

- Use **asynchronous, job-based API endpoints** for batch/bulk COBOL programs.
- Map COBOL batch steps to logical API resources and operations.
- Provide endpoints for job submission, status, results, and error retrieval.
- Ensure scalability, reliability, and clear separation of concerns.

**References:**  
- BULKG1, BULKT1, BULKT2, BULKG2, BULKTR (Source: flows, programs)  
- Example code and flowcharts from retrieved context

Let me know if you need specific endpoint examples for a particular COBOL batch program!

**Q7: What patterns should we use to represent COBOL screen navigation flows (menu systems, multi-step processes) as API workflows?**

To represent COBOL **screen navigation flows**—including menu systems and multi-step processes—as **API workflows**, you should use patterns that map user-driven navigation and stepwise processing into clear, resource-oriented, and stateful API designs. Here’s how to approach this transformation:

---

### 1. **Menu Systems → Resource Collections & Navigation Endpoints**

**COBOL Pattern:**  
- Users see a menu screen (e.g., MASTER MENU, LOAN SYSTEM MASTER MENU) with options to select different subsystems or actions.
- Each menu option triggers a program or navigates to a new screen.

**API Pattern:**  
- **Resource Collections:** Each menu option becomes a top-level resource or endpoint (e.g., `/loans`, `/dealers`, `/addons`, `/batch-processing`).
- **Navigation Endpoints:** Use endpoints to list available actions (e.g., `GET /menu` returns available resources).
- **Hypermedia Links (HATEOAS):** Include links in responses to guide clients to next possible actions, mimicking menu navigation.

**Example:**
- COBOL: User selects "Loan Maintenance" from MASTER MENU.
- API:  
  - `GET /menu` → returns `{ "options": ["/loans", "/dealers", ...] }`
  - Client follows `/loans` for loan-related actions.

---

### 2. **Multi-Step Processes → State Machines or Workflow Resources**

**COBOL Pattern:**  
- Users complete a sequence of screens (e.g., loan application: enter borrower info → enter loan details → review → submit).
- Each step may validate, collect, or update data.

**API Pattern:**  
- **Workflow Resource:** Represent the process as a resource with a state (e.g., `/loan-applications/{id}`).
- **Stepwise Endpoints:**  
  - `POST /loan-applications` → create new workflow instance  
  - `PATCH /loan-applications/{id}/borrower` → update borrower info  
  - `PATCH /loan-applications/{id}/details` → update loan details  
  - `POST /loan-applications/{id}/submit` → finalize application
- **State Transitions:** Use status fields or sub-resources to track progress (e.g., `status: "awaiting-details"`).

**Example:**
- COBOL: User completes steps 1-3 on different screens.
- API:  
  - Step 1: `PATCH /loan-applications/{id}/borrower`  
  - Step 2: `PATCH /loan-applications/{id}/details`  
  - Step 3: `POST /loan-applications/{id}/submit`

---

### 3. **Screen Navigation (Back, Next, Cancel) → API Actions & State**

**COBOL Pattern:**  
- Users use function keys (F1, F3, F7) to navigate (e.g., back, next page, cancel).
- Navigation logic is embedded in screen handling code.

**API Pattern:**  
- **Explicit Actions:** Provide endpoints for navigation actions (e.g., `POST /loan-applications/{id}/next`, `POST /loan-applications/{id}/cancel`).
- **Stateful Responses:** Each API response includes the current state and possible next actions.
- **Idempotency:** Ensure navigation actions are idempotent and stateless where possible.

---

### 4. **Validation & Error Handling → API Response Codes and Messages**

**COBOL Pattern:**  
- Validation occurs at each screen; errors are displayed to the user.

**API Pattern:**  
- **Validation on Each Step:** Validate input at each endpoint; return HTTP 400 with error details if invalid.
- **Consistent Error Structure:** Use structured error responses (e.g., `{ "error": "Invalid loan amount", "field": "amount" }`).

---

### 5. **Bulk Operations & Batch Processing → Asynchronous API Workflows**

**COBOL Pattern:**  
- Batch programs process large datasets, often triggered from menu options.

**API Pattern:**  
- **Job Resource:**  
  - `POST /batch-jobs` → submit batch request  
  - `GET /batch-jobs/{id}` → check status  
  - `GET /batch-jobs/{id}/results` → retrieve output
- **Polling or Webhooks:** Support for long-running operations.

---

## Visual Representation

Here's a visual representation of how a COBOL menu-driven workflow maps to API endpoints and stateful resources:

<svg width="900" height="500" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Menu Navigation → API Workflow Mapping</text>
  
  <!-- COBOL Menu Layer -->
  <rect x="50" y="70" width="800" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Menu Screen</text>
  <text x="450" y="125" text-anchor="middle" font-size="14" fill="#4338ca">Options: Loan Maintenance, Dealer Maintenance, Addon Maintenance, Batch Processing</text>
  
  <!-- API Resource Layer -->
  <rect x="50" y="200" width="800" height="80" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="230" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">API Resource Endpoints</text>
  <text x="200" y="255" text-anchor="middle" font-size="14" fill="#1e40af">GET /loans</text>
  <text x="400" y="255" text-anchor="middle" font-size="14" fill="#1e40af">GET /dealers</text>
  <text x="600" y="255" text-anchor="middle" font-size="14" fill="#1e40af">GET /addons</text>
  <text x="800" y="255" text-anchor="middle" font-size="14" fill="#1e40af">POST /batch-jobs</text>
  
  <!-- Workflow Layer -->
  <rect x="50" y="350" width="800" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="450" y="380" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">API Workflow/State Machine</text>
  <text x="200" y="405" text-anchor="middle" font-size="14" fill="#15803d">PATCH /loan-applications/{id}/borrower</text>
  <text x="400" y="405" text-anchor="middle" font-size="14" fill="#15803d">PATCH /loan-applications/{id}/details</text>
  <text x="600" y="405" text-anchor="middle" font-size="14" fill="#15803d">POST /loan-applications/{id}/submit</text>
  <text x="800" y="405" text-anchor="middle" font-size="14" fill="#15803d">GET /batch-jobs/{id}/results</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="450" y1="150" x2="200" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="150" x2="400" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="150" x2="600" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="150" x2="800" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="200" y1="280" x2="200" y2="350" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="400" y1="280" x2="400" y2="350" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="600" y1="280" x2="600" y2="350" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="800" y1="280" x2="800" y2="350" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

## Summary of Patterns

- **Menu options** → **Top-level API resources**
- **Screen transitions** → **Hypermedia links or explicit navigation endpoints**
- **Multi-step processes** → **Workflow/state machine resources**
- **Function keys/navigation** → **API actions or state transitions**
- **Validation/errors** → **Structured API error responses**
- **Batch operations** → **Asynchronous job resources**

These patterns ensure that the user-driven, stepwise nature of COBOL screen flows is preserved and made accessible in a modern, RESTful API architecture.

(Source: [[FILE:LPMENU|LPMENU]], [[FILE:MAINMU|MAINMU]], [[FILE:APIPAY|APIPAY]], [[FILE:BPMENU|BPMENU]], [[FILE:I|I]]ndex: help_fields, flows, programs)

**Q8: How can we convert COBOL file I/O operations (READ, WRITE, REWRITE, DELETE) to RESTful CRUD operations?**

To convert COBOL file I/O operations—**READ, WRITE, REWRITE, DELETE**—to RESTful CRUD operations, you should map each COBOL file action to its corresponding HTTP method and RESTful resource operation. Here’s how to approach this transformation:

---

### 1. **Mapping COBOL File I/O to RESTful CRUD**

| COBOL Operation | RESTful CRUD | HTTP Method | Typical Endpoint Example         |
|-----------------|-------------|-------------|----------------------------------|
| READ            | Retrieve     | GET         | GET /loans/{id}                  |
| WRITE           | Create       | POST        | POST /loans                      |
| REWRITE         | Update       | PUT/PATCH   | PUT /loans/{id}                  |
| DELETE          | Delete       | DELETE      | DELETE /loans/{id}               |

---

### 2. **Detailed Mapping**

- **READ (COBOL):**
  - COBOL: `READ FILE-NAME`
  - REST: Retrieve a resource or list of resources.
  - Example: `GET /accounts/{accountId}` returns account details.

- **WRITE (COBOL):**
  - COBOL: `WRITE FILE-NAME`
  - REST: Create a new resource.
  - Example: `POST /accounts` with account data creates a new account.

- **REWRITE (COBOL):**
  - COBOL: `REWRITE FILE-NAME`
  - REST: Update an existing resource.
  - Example: `PUT /accounts/{accountId}` updates account info.

- **DELETE (COBOL):**
  - COBOL: `DELETE FILE-NAME`
  - REST: Remove a resource.
  - Example: `DELETE /accounts/{accountId}` deletes the account.

---

### 3. **Design Considerations**

- **Resource-Oriented Endpoints:**  
  Each COBOL file (e.g., LOAN-FILE, ACCOUNT-FILE) becomes a REST resource (e.g., `/loans`, `/accounts`).

- **ID Mapping:**  
  COBOL record keys (e.g., ACCOUNT-NUMBER) map to resource IDs in the API path.

- **Bulk Operations:**  
  For batch COBOL operations, consider endpoints like `POST /loans/bulk` or `DELETE /loans/bulk` with payloads containing multiple records.

- **Status Codes:**  
  Map COBOL file status checks (e.g., `AT END`, `INVALID KEY`) to HTTP status codes (e.g., 404 Not Found, 400 Bad Request).

- **Error Handling:**  
  COBOL error paragraphs (e.g., `INVALID KEY`) become structured error responses in JSON.

---

### 4. **Example Conversion**

**COBOL:**
```cobol
READ LOAN-FILE KEY IS LOAN-NUMBER
IF INVALID KEY
   DISPLAY "Loan not found"
END-IF
```

**RESTful API:**
```http
GET /loans/{loanNumber}
Response:
  200 OK: {loan data}
  404 Not Found: {"error": "Loan not found"}
```

---

### 5. **Batch and Bulk Operations**

- **COBOL Batch:**
  - Processes many records in a loop (e.g., `PERFORM UNTIL END-OF-FILE`).
- **REST API:**
  - Use endpoints like `POST /loans/bulk` for creation, `PUT /loans/bulk` for updates, etc.
  - Accept arrays of records in the request body.

---

### 6. **Summary Table**

| COBOL Statement         | RESTful Action         | HTTP Method | Endpoint Example         |
|------------------------|------------------------|-------------|-------------------------|
| READ FILE-NAME         | Retrieve resource      | GET         | /resource/{id}          |
| WRITE FILE-NAME        | Create resource        | POST        | /resource               |
| REWRITE FILE-NAME      | Update resource        | PUT/PATCH   | /resource/{id}          |
| DELETE FILE-NAME       | Delete resource        | DELETE      | /resource/{id}          |

---

### 7. **Visual Representation**

Here's a visual representation:

<svg width="700" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">COBOL File I/O to RESTful CRUD Mapping</text>
  
  <!-- COBOL Operations -->
  <rect x="50" y="70" width="140" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="8"/>
  <text x="120" y="100" text-anchor="middle" font-size="15" font-weight="bold" fill="#4338ca">COBOL READ</text>
  <rect x="50" y="150" width="140" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="8"/>
  <text x="120" y="180" text-anchor="middle" font-size="15" font-weight="bold" fill="#4338ca">COBOL WRITE</text>
  <rect x="50" y="230" width="140" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="8"/>
  <text x="120" y="260" text-anchor="middle" font-size="15" font-weight="bold" fill="#4338ca">COBOL REWRITE</text>
  <rect x="50" y="310" width="140" height="60" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="8"/>
  <text x="120" y="340" text-anchor="middle" font-size="15" font-weight="bold" fill="#4338ca">COBOL DELETE</text>
  
  <!-- RESTful Operations -->
  <rect x="510" y="70" width="140" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="580" y="100" text-anchor="middle" font-size="15" font-weight="bold" fill="#15803d">GET /resource/{id}</text>
  <rect x="510" y="150" width="140" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="580" y="180" text-anchor="middle" font-size="15" font-weight="bold" fill="#15803d">POST /resource</text>
  <rect x="510" y="230" width="140" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="580" y="260" text-anchor="middle" font-size="15" font-weight="bold" fill="#15803d">PUT /resource/{id}</text>
  <rect x="510" y="310" width="140" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="580" y="340" text-anchor="middle" font-size="15" font-weight="bold" fill="#15803d">DELETE /resource/{id}</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="190" y1="100" x2="510" y2="100" stroke="#667eea" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="190" y1="180" x2="510" y2="180" stroke="#667eea" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="190" y1="260" x2="510" y2="260" stroke="#667eea" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="190" y1="340" x2="510" y2="340" stroke="#667eea" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**Summary:**  
By mapping COBOL file I/O operations to RESTful CRUD endpoints, you create a clear, maintainable API that aligns with modern web standards and supports interoperability, scalability, and ease of integration.

### Batch Programs and Workflow Design

**Q9: What strategy should we use for handling COBOL PERFORM statements and paragraph calls in API method design?**

To handle COBOL **PERFORM statements** and **paragraph calls** when designing API methods, use the following strategy:

---

### 1. **Map Paragraphs to API Methods**

- **COBOL paragraphs** are logical units of work (e.g., validation, calculation, file I/O).
- **PERFORM statements** invoke these paragraphs, sometimes with parameters or in loops.
- **Strategy:**  
  - **Each paragraph** that represents a distinct business function should become a separate API method or endpoint.
  - **Group related paragraphs** into composite API methods if they always execute together.

---

### 2. **Preserve Control Flow Semantics**

- **Sequential PERFORMs:**  
  - Map to sequential API calls or a single composite endpoint that executes steps in order.
- **Conditional PERFORMs (IF/EVALUATE):**  
  - Use API logic to conditionally invoke methods based on input parameters or business rules.
- **PERFORM UNTIL/ VARYING (loops):**  
  - Map to bulk operations or batch endpoints that process arrays/lists of items.

---

### 3. **Parameter Passing**

- COBOL often uses **shared variables**; APIs should use **explicit parameters**.
- For paragraphs that require context (e.g., current record, status), pass these as method arguments in the API.

---

### 4. **Error Handling and Return Values**

- COBOL paragraphs may set error codes or status flags.
- API methods should return **structured responses** (e.g., HTTP status, error objects) reflecting paragraph outcomes.

---

### 5. **Idempotency and State**

- COBOL PERFORMs may change program state; APIs should be designed to be **stateless** where possible.
- For stateful operations, use session tokens or transaction IDs.

---

### 6. **Example Mapping**

Suppose a COBOL program has:

```cobol
PERFORM VALIDATE-INPUT
PERFORM PROCESS-RECORDS UNTIL END-OF-FILE
PERFORM WRITE-OUTPUT
```

**API Mapping:**

- `POST /validate-input` → Runs VALIDATE-INPUT logic
- `POST /process-records` → Accepts a list of records, processes each (bulk operation)
- `POST /write-output` → Triggers output writing

---

### 7. **Composite Workflows**

- For multi-step COBOL flows, create an API endpoint that orchestrates the sequence, or use an API workflow engine.

---

### 8. **Documentation and Traceability**

- Document which COBOL paragraphs map to which API methods for maintainability.
- Example:  
  - `VALIDATE-INPUT` → `/validate-input`  
  - `PROCESS-RECORDS` → `/process-records`  
  - `WRITE-OUTPUT` → `/write-output`

---

**Summary Table: COBOL to API Mapping**

| COBOL Pattern                  | API Design Pattern                |
|------------------------------- |-----------------------------------|
| PERFORM paragraph              | API method/endpoint               |
| PERFORM UNTIL/VARYING (loop)   | Bulk/batch API operation          |
| IF/EVALUATE + PERFORM          | Conditional API logic             |
| Shared variables               | Explicit parameters               |
| Error/status flags             | Structured API response           |

---

**Source Reference:**  
This strategy is based on standard COBOL control flow analysis and best practices for API design. For example, the flowchart for APIPAY (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows) shows distinct paragraphs that can be mapped to API methods.

---

**Key Takeaway:**  
**Treat each COBOL paragraph as a candidate for an API method. Use the PERFORM structure to guide API workflow orchestration, ensuring business logic and control flow are preserved in the new architecture.**

**Q10: How do we map COBOL copybooks used across multiple programs to shared API resources or microservices?**

To map COBOL **copybooks**—which define shared data structures and business rules across multiple programs in OTIS—to **shared API resources or microservices**, follow this strategy:

---

### 1. **Identify Copybook Purpose and Scope**

- **Copybooks** (e.g., `[[FILE:LOAN_REC.CPY|LOAN_REC.CPY]]`, `[[FILE:GB01RC.CPY|GB01RC.CPY]]`) typically define:
  - Data structures (record layouts, fields)
  - Validation rules
  - Common business logic (status codes, error messages)
- Review copybook content and usage across programs to determine their business domain (e.g., loans, payments, customer info).

---

### 2. **Group Programs by Copybook Usage**

- Find all programs that use each copybook (via `COPY` statements).
- Group programs into functional domains based on shared copybook dependencies.
  - Example: All programs using `[[FILE:LOAN_REC.CPY|LOAN_REC.CPY]]` are likely part of the loan servicing domain.

---

### 3. **Define Shared API Resources**

- For each copybook, define a **RESTful resource** or **microservice** that encapsulates the data and logic:
  - **Resource Example:** `/loans`, `/payments`, `/customers`
  - **Microservice Example:** `LoanService`, `PaymentService`
- The resource should expose CRUD operations and business logic corresponding to the copybook's fields and rules.

---

### 4. **Centralize Validation and Business Rules**

- Move validation logic from copybooks into the API/microservice layer.
- Ensure all programs (now API clients) use the same validation and business rules, maintaining consistency.

---

### 5. **Design API Contracts Based on Copybook Structure**

- Use the copybook's field definitions to design API request/response schemas (e.g., JSON objects).
- Example: If `[[FILE:LOAN_REC.CPY|LOAN_REC.CPY]]` defines `LOAN-NUMBER`, `AMOUNT`, `STATUS`, the API resource `/loans` should include these fields.

---

### 6. **Handle Shared Utility Copybooks**

- Copybooks like `[[FILE:GB01RC.CPY|GB01RC.CPY]]` (return codes) or `[[FILE:ERROR_MSGS.CPY|ERROR_MSGS.CPY]]` (error messages) map to shared utility services or standardized API error responses.

---

### 7. **Example Mapping Table**

| COBOL Copybook      | API Resource / Microservice | Purpose/Domain                |
|---------------------|----------------------------|-------------------------------|
| [[FILE:LOAN_REC.CPY|LOAN_REC.CPY]]        | /loans, LoanService        | Loan record management        |
| [[FILE:PAYMENT_REC.CPY|PAYMENT_REC.CPY]]     | /payments, PaymentService  | Payment processing            |
| [[FILE:CUSTOMER_REC.CPY|CUSTOMER_REC.CPY]]    | /customers, CustomerService| Customer data                 |
| [[FILE:GB01RC.CPY|GB01RC.CPY]]          | Error handling utility     | Standard return codes/errors  |
| [[FILE:ADDON_REC.CPY|ADDON_REC.CPY]]       | /addons, AddonService      | Add-on product management     |

---

### 8. **Benefits of This Mapping**

- **Consistency:** All API clients use the same data definitions and rules.
- **Reusability:** Shared resources/microservices reduce duplication.
- **Maintainability:** Changes to business rules or data structures are centralized.

---

**Summary:**  
In OTIS, COBOL copybooks used across multiple programs should be mapped to shared API resources or microservices that encapsulate their data structures and business logic. This ensures consistency, reusability, and maintainability in the modernized architecture.

If you need a specific mapping for a particular copybook or want to see which programs use a given copybook, please provide the copybook name or more context.

**Q11: What approach should we take to design API endpoints for COBOL programs that interact with screens (SCRN1, PGMENU, GTFORM)?**

To design API endpoints for COBOL programs that interact with screens like **SCRN1**, **PGMENU**, and **GTFORM**, you should follow a strategy that translates screen-driven workflows and user interactions into RESTful API resources and operations. Here’s a recommended approach:

---

### 1. **Identify Screen Functions and User Actions**

- **SCRN1**: Handles directory listing, file reading, and prompt building (Source: [[FILE:SCRN1|SCRN1]], [[FILE:I|I]]ndex: flows).
- **PGMENU**: Acts as a main menu, routing user choices to subsystems (Source: [[FILE:PGMENU|PGMENU]], [[FILE:I|I]]ndex: programs).
- **GTFORM**: Presents forms for user input, likely for transaction initiation (Source: [[FILE:GTFORM|GTFORM]], [[FILE:I|I]]ndex: programs).

---

### 2. **Map Screen Interactions to API Resources**

- **Each screen** becomes an API resource representing a business entity or workflow step.
- **User actions** (e.g., selecting a menu option, submitting a form) become API operations (HTTP methods).

**Examples:**
- **SCRN1**: `/directories` (GET for listing, POST for creating a new directory entry)
- **PGMENU**: `/menu` (GET for menu options, POST for selection)
- **GTFORM**: `/forms/transaction` (GET for form schema, POST for submitting transaction data)

---

### 3. **Design Endpoints for Screen-Driven Workflows**

- **GET** endpoints to retrieve screen data or options.
- **POST** endpoints to submit user input or trigger actions.
- **PUT/PATCH** for updating form data (if applicable).
- **DELETE** for removing entries (if supported by the screen logic).

**Example Endpoint Designs:**

| COBOL Screen | API Endpoint           | HTTP Method | Purpose                                 |
|--------------|-----------------------|-------------|-----------------------------------------|
| SCRN1        | `/directories`        | GET         | List directories (CREATE-DIR-LIST)      |
| SCRN1        | `/directories`        | POST        | Add new directory (SET-PATHS)           |
| SCRN1        | `/input-file`         | GET         | Read input file (A100-READ)             |
| PGMENU       | `/menu`               | GET         | Retrieve menu options                   |
| PGMENU       | `/menu/selection`     | POST        | Submit menu selection                   |
| GTFORM       | `/forms/transaction`  | GET         | Get transaction form schema             |
| GTFORM       | `/forms/transaction`  | POST        | Submit transaction data                 |

---

### 4. **Handle State and Navigation**

- Use API responses to guide client navigation (e.g., next available actions, error messages).
- Maintain session or workflow state as needed (e.g., via tokens or workflow IDs).

---

### 5. **Translate COBOL Paragraphs and PERFORMs to API Logic**

- Each **paragraph** or **PERFORM** block handling a screen event becomes a backend function or microservice method.
- API endpoints invoke these functions, passing user input as parameters.

---

### 6. **Error Handling and Validation**

- Map COBOL screen validation and error messages to API response codes and error objects.
- Ensure business rules enforced in COBOL are reflected in API validation logic.

---

### 7. **Example Workflow Mapping**

**COBOL Flow (SCRN1):**
- User selects directory → SCRN1 builds list → User reads file → SCRN1 prompts for next action

**API Flow:**
1. `GET /directories` → returns directory list
2. `POST /directories` → creates new directory entry
3. `GET /input-file?dir=xyz` → reads file contents
4. `POST /prompt` → submits user action, returns next prompt or result

---

### Visual Representation

Here's a visual representation of how these screens map to API endpoints and workflow:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Screen to API Endpoint Mapping</text>
  
  <!-- SCRN1 -->
  <rect x="50" y="80" width="200" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">SCRN1</text>
  <text x="150" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Directory/File Ops</text>
  
  <!-- PGMENU -->
  <rect x="350" y="80" width="200" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="450" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">PGMENU</text>
  <text x="450" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Menu Navigation</text>
  
  <!-- GTFORM -->
  <rect x="650" y="80" width="200" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">GTFORM</text>
  <text x="750" y="130" text-anchor="middle" font-size="12" fill="#6366f1">Form Input</text>
  
  <!-- API Endpoints -->
  <rect x="50" y="200" width="200" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="150" y="230" text-anchor="middle" font-size="15" font-weight="bold" fill="#1e40af">/directories</text>
  <text x="150" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">GET, POST</text>
  
  <rect x="350" y="200" width="200" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="230" text-anchor="middle" font-size="15" font-weight="bold" fill="#1e40af">/menu</text>
  <text x="450" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">GET, POST</text>
  
  <rect x="650" y="200" width="200" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="750" y="230" text-anchor="middle" font-size="15" font-weight="bold" fill="#1e40af">/forms/transaction</text>
  <text x="750" y="250" text-anchor="middle" font-size="12" fill="#3b82f6">GET, POST</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="150" y1="150" x2="150" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="150" x2="450" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="750" y1="150" x2="750" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

### Source Citations

- SCRN1 screen logic and file operations (Source: [[FILE:SCRN1|SCRN1]], [[FILE:I|I]]ndex: flows)
- PGMENU menu routing and program calls (Source: [[FILE:PGMENU|PGMENU]], [[FILE:I|I]]ndex: programs)
- GTFORM form handling (Source: [[FILE:GTFORM|GTFORM]], [[FILE:I|I]]ndex: programs)

---

**Summary:**  
Design API endpoints by mapping each COBOL screen’s business function to a RESTful resource, with HTTP methods representing user actions. Ensure workflows, validation, and navigation logic are preserved in the API layer, and use clear resource naming to reflect the original screen purposes.

**Q12: How can we identify and group related COBOL programs into cohesive API controllers or service boundaries?**

To identify and group related COBOL programs into **cohesive API controllers or service boundaries**, use a systematic approach that leverages program relationships, shared data structures, and business workflows. Here’s a recommended strategy:

---

### 1. **Analyze Program Relationships (Call Chains & Dependencies)**

- **Trace call chains:** Use call graphs and flowcharts to see which programs call each other directly or indirectly.
    - Example: CHBORR calls 3 unique programs and is called by 1 program (Source: [[FILE:CHBORR|CHBORR]], [[FILE:I|I]]ndex: programs).
    - Programs with tight call relationships often belong in the same API controller or service boundary.

- **Group by call depth and cycles:** Programs participating in deep call chains or cycles (e.g., LONPFB, PST593) may represent a tightly coupled business process.

---

### 2. **Leverage Shared Copybooks and Data Structures**

- **Identify common copybooks:** Programs that use the same copybooks (e.g., LIBGB/[[FILE:GB01BR.CPY|GB01BR.CPY]], LIBLP/[[FILE:LP01DL.CPY|LP01DL.CPY]]) are likely operating on similar data and business rules.
    - Example: [[FILE:CHBORR.CBL|CHBORR.CBL]], [[FILE:LONPN0.CBL|LONPN0.CBL]], and [[FILE:LONPFB.CBL|LONPFB.CBL]] all use LIBGB/[[FILE:GB01BR.CPY|GB01BR.CPY]] and LIBLP/[[FILE:LP01DL.CPY|LP01DL.CPY]] (Sources: [[FILE:CHBORR.CBL|CHBORR.CBL]], [[FILE:LONPN0.CBL|LONPN0.CBL]], [[FILE:LONPFB.CBL|LONPFB.CBL]], Index: code).
- **Group programs by shared copybooks:** These can form the basis for a shared API resource or microservice.

---

### 3. **Map to Business Workflows and Screen Interactions**

- **Follow screen-driven flows:** Programs interacting with the same screens (e.g., SCRN1, PGMENU, GTFORM) are part of the same user workflow.
    - Example: Programs with flowcharts showing ENTRY-MODULE, FORM-RESET, WRITE-MODULE, etc., are part of the same CRUD workflow (Sources: BRMAN2, BPBRMN, GPMAN1, BROPM1, Index: flows).
- **Group by workflow:** Each workflow (e.g., loan maintenance, branch management) can be a controller or service boundary.

---

### 4. **Use Business Domain Boundaries**

- **Identify domain-specific programs:** Programs with similar prefixes (e.g., LONPFB, LONPN0, LONPF2 for loan processing) or that operate on the same business entity (e.g., borrower, branch, loan) should be grouped.
- **Align with business domains:** Each domain (loan, branch, dealer, etc.) becomes a logical API controller or microservice.

---

### 5. **Visualize Relationships for Cohesive Grouping**

Here’s a visual representation of how programs can be grouped into service boundaries based on call relationships and shared copybooks:

<svg width="1200" height="600" xmlns="http://www.w3.org/2000/svg">
  <text x="600" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">Service Boundaries: COBOL Program Grouping</text>
  
  <!-- Loan Processing Service -->
  <rect x="50" y="80" width="320" height="180" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="12"/>
  <text x="210" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Loan Processing Service</text>
  <rect x="70" y="140" width="120" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="130" y="170" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LONPFB</text>
  <rect x="210" y="140" width="120" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="270" y="170" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LONPN0</text>
  <rect x="140" y="220" width="120" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="200" y="250" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LONPF2</text>
  <line x1="130" y1="200" x2="200" y2="220" stroke="#4338ca" stroke-width="2"/>
  <line x1="270" y1="200" x2="200" y2="220" stroke="#4338ca" stroke-width="2"/>
  
  <!-- Branch Management Service -->
  <rect x="400" y="80" width="320" height="180" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="12"/>
  <text x="560" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Branch Management Service</text>
  <rect x="420" y="140" width="120" height="60" fill="#3b82f6" stroke="#1e40af" stroke-width="2" rx="8"/>
  <text x="480" y="170" text-anchor="middle" fill="white" font-size="14" font-weight="bold">BRMAN2</text>
  <rect x="560" y="140" width="120" height="60" fill="#3b82f6" stroke="#1e40af" stroke-width="2" rx="8"/>
  <text x="620" y="170" text-anchor="middle" fill="white" font-size="14" font-weight="bold">BPBRMN</text>
  <rect x="490" y="220" width="120" height="60" fill="#3b82f6" stroke="#1e40af" stroke-width="2" rx="8"/>
  <text x="550" y="250" text-anchor="middle" fill="white" font-size="14" font-weight="bold">BROPM1</text>
  <line x1="480" y1="200" x2="550" y2="220" stroke="#1e40af" stroke-width="2"/>
  <line x1="620" y1="200" x2="550" y2="220" stroke="#1e40af" stroke-width="2"/>
  
  <!-- Borrower Service -->
  <rect x="750" y="80" width="320" height="180" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="12"/>
  <text x="910" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Borrower Service</text>
  <rect x="770" y="140" width="120" height="60" fill="#22c55e" stroke="#15803d" stroke-width="2" rx="8"/>
  <text x="830" y="170" text-anchor="middle" fill="white" font-size="14" font-weight="bold">CHBORR</text>
  <rect x="910" y="140" width="120" height="60" fill="#22c55e" stroke="#15803d" stroke-width="2" rx="8"/>
  <text x="970" y="170" text-anchor="middle" fill="white" font-size="14" font-weight="bold">CIFILE</text>
  <rect x="840" y="220" width="120" height="60" fill="#22c55e" stroke="#15803d" stroke-width="2" rx="8"/>
  <text x="900" y="250" text-anchor="middle" fill="white" font-size="14" font-weight="bold">LPMNMU</text>
  <line x1="830" y1="200" x2="900" y2="220" stroke="#15803d" stroke-width="2"/>
  <line x1="970" y1="200" x2="900" y2="220" stroke="#15803d" stroke-width="2"/>
</svg>

---

### 6. **Summary of Steps**

- **Trace call relationships** to find tightly coupled program groups.
- **Group by shared copybooks/data structures** for common business logic.
- **Align with business workflows and screen interactions** for user-driven boundaries.
- **Map to business domains** for logical API controller/service boundaries.
- **Visualize and validate** groupings with diagrams and call graphs.

---

**Example Groupings:**

| Service Boundary         | Programs Included                | Shared Copybooks/Data | Business Domain         |
|-------------------------|----------------------------------|----------------------|------------------------|
| Loan Processing         | LONPFB, LONPN0, LONPF2           | [[FILE:LP01DL.CPY|LP01DL.CPY]], [[FILE:GB01BR.CPY|GB01BR.CPY]] | Loan/Payment           |
| Branch Management       | BRMAN2, BPBRMN, BROPM1           | [[FILE:GB01BR.CPY|GB01BR.CPY]]           | Branch/Location        |
| Borrower Service        | CHBORR, CIFILE, LPMNMU           | [[FILE:GB01BR.CPY|GB01BR.CPY]]           | Borrower/Customer      |

---

**References:**
- Program call relationships (Source: [[FILE:CHBORR|CHBORR]], [[FILE:LONPFB|LONPFB]], [[FILE:BRMAN2|BRMAN2]], [[FILE:BPBRMN|BPBRMN]], [[FILE:BROPM1|BROPM1]], [[FILE:CIFILE|CIFILE]], [[FILE:LPMNMU|LPMNMU]], [[FILE:I|I]]ndex: programs)
- Copybook usage (Source: [[FILE:CHBORR.CBL|CHBORR.CBL]], [[FILE:LONPN0.CBL|LONPN0.CBL]], [[FILE:LONPFB.CBL|LONPFB.CBL]], Index: code)
- Screen-driven workflows (Source: [[FILE:BRMAN2|BRMAN2]], [[FILE:BPBRMN|BPBRMN]], [[FILE:GPMAN1|GPMAN1]], [[FILE:BROPM1|BROPM1]], [[FILE:I|I]]ndex: flows)

---

**In summary:**  
Group COBOL programs into API controllers or service boundaries by analyzing call chains, shared copybooks, business workflows, and domain boundaries. Use diagrams to visualize and validate your groupings for effective API design.

### Program Interactions and Service Boundaries

**Q13: What patterns should we use to handle COBOL program-to-program calls (CALL statements) in a microservices or API architecture?**

To handle **COBOL program-to-program calls (CALL statements)** in a microservices or API architecture, you should use patterns that translate legacy inter-program invocation into modern, loosely-coupled service interactions. Here’s a direct answer with recommended patterns and rationale:

---

## Recommended Patterns for COBOL CALL Statement Modernization

### 1. **API Gateway Pattern**
- **Purpose:** Centralizes all external calls, routing requests to the appropriate microservice.
- **How to Apply:** Replace direct COBOL `CALL` statements with API calls routed through an API Gateway. The gateway handles authentication, routing, and protocol translation.
- **Example:** Instead of `CALL 'REFUPD' USING ...`, the modernized program sends an HTTP request to `/api/payments/update`.

### 2. **Service Mesh / Orchestration**
- **Purpose:** Manages service-to-service communication, retries, and monitoring.
- **How to Apply:** Use a service mesh (e.g., Istio, Linkerd) to transparently handle calls between microservices that replace COBOL programs. This abstracts network concerns and enables tracing.
- **Example:** A payment service calls a ledger service via the mesh, replacing `CALL 'LEDGERUPD'`.

### 3. **Request/Response API Endpoints**
- **Purpose:** Each COBOL program with a business function becomes a RESTful or RPC endpoint.
- **How to Apply:** Map each callable COBOL program to a distinct API endpoint. Define clear input/output schemas based on copybook structures.
- **Example:** `CALL 'LONPW9' USING ...` becomes `POST /api/notifications/payment`.

### 4. **Event-Driven Architecture**
- **Purpose:** Decouples services using asynchronous messaging (e.g., Kafka, RabbitMQ).
- **How to Apply:** For batch or workflow triggers, replace `CALL` with event publishing. Downstream services subscribe and react.
- **Example:** Instead of `CALL 'AUDIT'`, publish an `AuditEvent` to a message queue.

### 5. **Adapter/Facade Pattern**
- **Purpose:** Provides a translation layer between legacy COBOL calls and new APIs.
- **How to Apply:** Build adapters that expose legacy COBOL programs as APIs, or vice versa, allowing gradual migration.
- **Example:** An adapter receives an API call and internally performs the COBOL `CALL`, returning results as JSON.

### 6. **Synchronous vs. Asynchronous Calls**
- **Purpose:** Match the original COBOL call semantics (blocking/non-blocking) to appropriate API patterns.
- **How to Apply:** Use synchronous HTTP for immediate results; use async messaging for deferred processing.
- **Example:** Payment validation (`CALL 'VALAMT'`) is synchronous; notification dispatch (`CALL 'LONPW9'`) can be async.

---

## Technical Implementation Steps

1. **Inventory All CALL Statements**
   - Identify every COBOL `CALL` (static and dynamic) and its parameters.
   - Example: APIPAY calls REFUPD, LONPW9, ERRLOG, etc. (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: calls)

2. **Define Service Boundaries**
   - Group related programs into logical services (see previous answer on service boundaries).
   - Example: Payment-related programs form a Payment Service.

3. **Map Data Contracts**
   - Use copybook definitions to create API schemas (JSON, XML, etc.).
   - Example: PAYMNT-WKS copybook → PaymentRequest DTO.

4. **Replace CALLs with API Invocations**
   - Refactor COBOL logic to invoke APIs via HTTP, gRPC, or message queues.
   - Example: MOVE parameters to JSON payload, POST to endpoint.

5. **Handle Error and Response Codes**
   - Map COBOL return codes to HTTP status codes or structured error responses.

6. **Maintain Transactional Integrity**
   - For multi-step workflows, use orchestration (Saga, BPMN) to ensure consistency.

---

## Example: COBOL CALL Chain Modernized

Original COBOL:
```
CALL 'REFUPD' USING PAYMENT-FILE, ACCOUNT-FILE
CALL 'LONPW9' USING PAYMENT-FILE
```

Modernized Microservices:
- `POST /api/payments/update` with payment/account payload
- `POST /api/notifications/payment` with payment payload

---

## Visual Representation

Here's a visual representation of how COBOL program-to-program calls can be mapped to microservices/API calls:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL CALL Modernization Patterns</text>
  
  <!-- Legacy COBOL Programs -->
  <rect x="60" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">APIPAY</text>
  <text x="150" y="130" text-anchor="middle" fill="white" font-size="12">COBOL Program</text>
  
  <rect x="60" y="200" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">REFUPD</text>
  <text x="150" y="250" text-anchor="middle" fill="white" font-size="12">COBOL Program</text>
  
  <rect x="60" y="320" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="350" text-anchor="middle" fill="white" font-size="16" font-weight="bold">LONPW9</text>
  <text x="150" y="370" text-anchor="middle" fill="white" font-size="12">COBOL Program</text>
  
  <!-- Arrows from APIPAY to others -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="240" y1="115" x2="240" y2="235" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="240" y1="115" x2="240" y2="355" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- Modernized Microservices -->
  <rect x="400" y="80" width="180" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="490" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Payment Service</text>
  <text x="490" y="130" text-anchor="middle" fill="white" font-size="12">/api/payments/update</text>
  
  <rect x="400" y="200" width="180" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="490" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Ledger Service</text>
  <text x="490" y="250" text-anchor="middle" fill="white" font-size="12">/api/ledger/update</text>
  
  <rect x="400" y="320" width="180" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="490" y="350" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Notification Service</text>
  <text x="490" y="370" text-anchor="middle" fill="white" font-size="12">/api/notifications/payment</text>
  
  <!-- Arrows from Payment Service to others -->
  <line x1="580" y1="115" x2="580" y2="235" stroke="#16a34a" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="115" x2="580" y2="355" stroke="#16a34a" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- Labels -->
  <text x="320" y="60" text-anchor="middle" font-size="14" fill="#6366f1">Legacy COBOL CALL</text>
  <text x="700" y="60" text-anchor="middle" font-size="14" fill="#22c55e">Modern API Call</text>
</svg>

---

## Summary Table: Patterns for COBOL CALL Modernization

| Pattern                | COBOL Usage           | API/Microservice Equivalent         | When to Use                |
|------------------------|----------------------|-------------------------------------|----------------------------|
| API Gateway            | CALL 'PROG'          | HTTP/gRPC via Gateway               | All external calls         |
| Service Mesh           | CALL 'PROG'          | Service-to-service mesh routing     | Internal service calls     |
| Request/Response API   | CALL 'PROG' USING... | REST/RPC endpoint                   | Synchronous business logic |
| Event-Driven           | CALL 'PROG' (batch)  | Publish/Subscribe events            | Async workflows            |
| Adapter/Facade         | CALL 'PROG'          | Adapter layer (legacy ↔ API)        | Gradual migration          |
| Sync/Async Handling    | CALL 'PROG'          | HTTP (sync), MQ (async)             | Match original semantics   |

---

**Key Takeaway:**  
Replace COBOL `CALL` statements with well-defined API/service interactions using gateway, mesh, and adapter patterns. Map data contracts from copybooks, maintain transactional integrity, and choose synchronous or asynchronous communication as appropriate for each business process.

(Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: calls; general COBOL modernization best practices)

**Q14: How do we design API versioning strategy to support incremental migration from COBOL to C# while maintaining backward compatibility?**

To design an **API versioning strategy** that supports incremental migration from COBOL to C# while maintaining backward compatibility, you should use a layered, explicit versioning approach that accommodates both legacy COBOL workflows and new C# implementations. Here’s how to structure your strategy:

---

### 1. **Explicit Versioning in API Endpoints**
- **Use URI-based versioning:**  
  Example:  
  - `/api/v1/payments` → routes to COBOL backend (e.g., APIPAY, REFUPD)
  - `/api/v2/payments` → routes to new C# service

- **Alternative:** Use header-based versioning (e.g., `X-API-Version: 1`), but URI versioning is clearer for migration and routing.

---

### 2. **Parallel Support for COBOL and C# Implementations**
- **Maintain both versions during migration:**  
  - v1 endpoints call COBOL programs via wrappers/adapters (screen emulation, data translation).
  - v2 endpoints call C# services directly.
- **Allow clients to choose version:**  
  - Legacy clients use v1 (COBOL logic).
  - New clients use v2 (C# logic).

---

### 3. **Backward Compatibility Guarantees**
- **Never break v1 contracts:**  
  - Preserve request/response formats for v1 endpoints.
  - Emulate legacy screen flows and business rules (e.g., SCRN1, PGMENU, GTFORM logic) in adapters.

- **Document differences in v2:**  
  - Clearly state enhancements, field changes, or business rule updates in v2.
  - Provide migration guides for clients.

---

### 4. **Incremental Migration Workflow**
- **Migrate one endpoint or business domain at a time:**  
  - Example: Start with `/api/v2/payments` (C#), keep `/api/v1/payments` (COBOL).
  - Gradually migrate other endpoints (e.g., inquiries, reporting).

- **Use feature flags or routing logic:**  
  - Route requests to COBOL or C# based on version, user, or feature flag.

---

### 5. **Testing and Validation**
- **Regression test v1 endpoints:**  
  - Ensure COBOL logic is preserved.
- **Acceptance test v2 endpoints:**  
  - Validate new C# logic and enhancements.

---

### 6. **Deprecation Policy**
- **Announce deprecation timelines for v1:**  
  - Give clients time to migrate.
- **Support v1 for a defined period post-migration.**

---

### 7. **Example Versioning Flow**

**Business Example:**  
- `/api/v1/payments` → Calls APIPAY COBOL program, emulates legacy screen flow (Source: [[FILE:APIPAY|APIPAY]], [[FILE:SCRN1|SCRN1]], [[FILE:PGMENU|PGMENU]], [[FILE:GTFORM|GTFORM]])
- `/api/v2/payments` → Calls C# PaymentService, uses modern validation and data structures

**Call Chain Diagram:**

Here's a visual representation:

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">API Versioning for COBOL to C# Migration</text>
  
  <!-- v1 API box -->
  <rect x="100" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="190" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">/api/v1/payments</text>
  <text x="190" y="130" text-anchor="middle" fill="white" font-size="12">COBOL Adapter</text>
  
  <!-- COBOL programs -->
  <rect x="100" y="180" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="160" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">APIPAY</text>
  <rect x="250" y="180" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="310" y="210" text-anchor="middle" fill="white" font-size="14" font-weight="bold">REFUPD</text>
  
  <!-- v2 API box -->
  <rect x="600" y="80" width="180" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="690" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">/api/v2/payments</text>
  <text x="690" y="130" text-anchor="middle" fill="white" font-size="12">C# Service</text>
  
  <!-- C# service box -->
  <rect x="600" y="180" width="120" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="660" y="210" text-anchor="middle" fill="#15803d" font-size="14" font-weight="bold">PaymentService</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="190" y1="150" x2="160" y2="180" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="190" y1="150" x2="310" y2="180" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="690" y1="150" x2="660" y2="180" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

### **Summary of Strategy**

- **Versioned endpoints** allow parallel support for COBOL and C# logic.
- **Adapters** ensure legacy business rules and screen flows are preserved for v1.
- **Incremental migration** lets you move one business domain at a time.
- **Backward compatibility** is maintained by never breaking v1 contracts.
- **Clear documentation** and deprecation policies guide clients through migration.

---

**Source References:**
- APIPAY, REFUPD, SCRN1, PGMENU, GTFORM (Source: [[FILE:APIPAY|APIPAY]], [[FILE:SCRN1|SCRN1]], [[FILE:PGMENU|PGMENU]], [[FILE:GTFORM|GTFORM]], [[FILE:I|I]]ndex: flows)
- [[FILE:WIMESS.CPY|WIMESS.CPY]] - Message box logic for screen suppression (Source: [[FILE:WIMESS.CPY|WIMESS.CPY]], Index: code)

If you need more details on how to structure adapters or handle specific screen flows, let me know!

**Q15: What approach should we take for API authentication and authorization based on COBOL security models (user IDs, access levels)?**

To design API authentication and authorization based on COBOL security models (user IDs, access levels), you should **map legacy security concepts to modern API standards** while preserving business rules and compatibility. Here’s a recommended approach:

---

### 1. **Extract and Analyze COBOL Security Logic**

- **User IDs and Passwords:** COBOL programs typically prompt for user IDs and passwords, as seen in [[FILE:LCUST2.CBL|LCUST2.CBL]]:
  ```cobol
  display "Enter your user id (or press RETURN to use Windows Authentication): ", no.
  accept userid.
  if userid not = spaces
      display "Enter your password : ", no
      accept passwd-name
  ```
  (Source: [[FILE:LCUST2.CBL|LCUST2.CBL]], Index: code)

- **Access Levels and Profiles:** Programs like SECENT and PWMAIN maintain user security profiles and access levels:
  - SECENT: "USER SECURITY PROFILE MAINTENANCE" (Source: [[FILE:SECENT.CBL|SECENT.CBL]], Index: code)
  - PWMAIN: "USER SECURITY PASSWORD MAINTENANCE" (Source: [[FILE:PWMAIN.CBL|PWMAIN.CBL]], Index: code)

- **Authorization Checks:** Programs such as CHKSEC perform company option and user security checks:
  - CHKSEC: "COMPANY OPTION & USER SECURITY CHECK" (Source: [[FILE:CHKSEC.CBL|CHKSEC.CBL]], Index: code)

---

### 2. **Map COBOL Security to API Standards**

- **Authentication:**
  - **User ID/Password → API Token:** Replace direct user ID/password prompts with secure token-based authentication (e.g., OAuth2, JWT).
  - **Windows Authentication Option:** If legacy supports Windows authentication, map to SSO or federated identity providers.

- **Authorization:**
  - **Access Levels → API Roles/Scopes:** Translate COBOL access levels (e.g., admin, user, read-only) into API roles or scopes.
  - **Profile Maintenance:** Ensure user profile changes in COBOL (e.g., via SECENT, PWMAIN) are reflected in the API’s identity store.

---

### 3. **Implement API Security Boundaries**

- **Centralized Authentication Service:** Create a dedicated authentication microservice that validates credentials against the legacy user store (or a synchronized modern store).
- **Authorization Middleware:** Implement middleware that checks user roles/scopes before allowing access to API endpoints, mirroring COBOL’s access checks.

---

### 4. **Preserve Business Rules and Audit Trails**

- **Business Rule Enforcement:** Maintain COBOL business rules for access (e.g., field-level restrictions, password policies) in the API layer.
- **Audit Logging:** Replicate COBOL audit/change logging (e.g., SECENT calls AUDITW) in API logs for traceability.

---

### 5. **Sample Mapping Table**

| COBOL Concept         | API Equivalent           | Implementation Notes                       |
|-----------------------|-------------------------|--------------------------------------------|
| User ID/Password      | OAuth2/JWT Token        | Secure credential storage, token issuance  |
| Access Level (Admin)  | Role: admin             | Role-based access control (RBAC)           |
| Access Level (User)   | Role: user              | Endpoint-level permission checks           |
| Profile Maintenance   | User Profile API        | Sync changes between legacy and API        |
| Audit Log             | API Audit Trail         | Log all auth and profile changes           |

---

### 6. **Example Flowchart**

Here's a visual representation:

<svg width="700" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">API Authentication & Authorization Flow</text>
  
  <!-- User Input -->
  <rect x="50" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">User Credentials</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="11">User ID / Password</text>
  
  <!-- Auth Service -->
  <rect x="270" y="80" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Auth Service</text>
  <text x="360" y="130" text-anchor="middle" fill="white" font-size="11">Token Issuance</text>
  
  <!-- API Gateway -->
  <rect x="490" y="80" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">API Gateway</text>
  <text x="580" y="130" text-anchor="middle" fill="white" font-size="11">Role/Scope Check</text>
  
  <!-- Legacy COBOL -->
  <rect x="270" y="200" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="360" y="230" text-anchor="middle" fill="#15803d" font-size="16" font-weight="bold">COBOL Security</text>
  <text x="360" y="250" text-anchor="middle" fill="#15803d" font-size="11">User Profile, Access Level</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="140" x2="360" y2="200" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

### **Summary**

- **Extract COBOL security logic** (user IDs, passwords, access levels, audit).
- **Map to API standards** (tokens, roles/scopes, SSO).
- **Centralize authentication/authorization** in API gateway/middleware.
- **Preserve business rules and audit trails** from legacy COBOL.
- **Synchronize user profile changes** between COBOL and API systems.

This approach ensures secure, compatible, and maintainable authentication and authorization as you migrate from COBOL to modern API architectures.

**Sources:**
- [[FILE:LCUST2.CBL|LCUST2.CBL]], [[FILE:SECENT.CBL|SECENT.CBL]], [[FILE:PWMAIN.CBL|PWMAIN.CBL]], [[FILE:CHKSEC.CBL|CHKSEC.CBL]] (user ID/password, profile, access logic)
- [[FILE:CONAME_EXT.CPY|CONAME_EXT.CPY]] (API program notes on screen disable and error messaging)
- [[FILE:PASSWD.CBL|PASSWD.CBL]] (password prompt and logic)

**Q16: How can we map COBOL transaction boundaries (COMMIT, ROLLBACK logic) to API transaction scopes and database transactions?**

To map **COBOL transaction boundaries (COMMIT, ROLLBACK logic)** to API transaction scopes and database transactions in a modern architecture, you need to translate legacy batch-oriented, program-level transaction control into explicit, service-level transaction management. Here’s how to approach this:

---

### 1. **Understand COBOL Transaction Boundaries**

- **COBOL programs often use EXEC SQL COMMIT/ROLLBACK** statements to control database transactions.
- Transaction boundaries are typically defined by:
  - End of a logical unit of work (e.g., after processing a batch, updating a record, or completing a payment).
  - Error handling (ROLLBACK on failure).
- Example from context:
  ```cobol
  *    IF ( IO-FG = 0 )
  *       EXEC SQL COMMIT END-EXEC
  *       PERFORM SQL-IO-VALIDATION.
  ```
  (Source: [[FILE:ACRERN.CBL|ACRERN.CBL]], Index: code)

---

### 2. **API Transaction Scope Design**

- **Each API call should represent a single logical unit of work** (e.g., "Process Payment", "Update Loan").
- The API should:
  - Begin a database transaction at the start of the request.
  - Commit the transaction if all operations succeed.
  - Rollback the transaction if any error occurs.
- This matches the COBOL pattern of grouping related operations within a transaction boundary.

---

### 3. **Mapping Patterns**

#### **A. Direct Mapping (One-to-One)**
- For simple COBOL programs where a single COMMIT/ROLLBACK wraps the entire process, map each API endpoint to a single database transaction.
- Example:
  - COBOL: Process batch → COMMIT
  - API: POST /processBatch → Begin transaction → Process → COMMIT

#### **B. Chained/Composite Transactions**
- For COBOL flows with multiple steps (possibly across several programs), use **distributed transaction management** or **saga patterns**:
  - Each microservice/API manages its own transaction.
  - Use orchestration to ensure all steps succeed, or compensate (undo) if any step fails.

#### **C. Error Handling**
- COBOL ROLLBACK logic (on error) should be mapped to API error handling:
  - If any validation or processing fails, rollback the transaction and return an error response.

---

### 4. **Database Transaction Implementation**

- In C#/.NET, use `TransactionScope` or explicit database transactions (`DbTransaction`).
- Example:
  ```csharp
  using (var scope = new TransactionScope())
  {
      // Perform DB operations
      scope.Complete(); // Commit
  }
  // If exception occurs, transaction is rolled back
  ```

---

### 5. **Special Considerations**

- **Legacy batch jobs:** If COBOL commits after every N records, consider batching in the API or using chunked transactions.
- **Long-running transactions:** Avoid holding transactions open across multiple API calls; use compensation logic instead.
- **Idempotency:** Ensure API operations are idempotent to handle retries without duplicate commits.

---

### 6. **Example Mapping Table**

| COBOL Logic                | API/Modern Equivalent                |
|----------------------------|--------------------------------------|
| EXEC SQL COMMIT END-EXEC   | dbTransaction.Commit()               |
| EXEC SQL ROLLBACK END-EXEC | dbTransaction.Rollback()             |
| PERFORM SQL-IO-VALIDATION  | Validate DB operation, throw error   |
| Batch commit every N rows  | API batch endpoint, chunked commit   |

---

### 7. **Source References**

- COBOL COMMIT/ROLLBACK usage:
  - "EXEC SQL COMMIT END-EXEC" (Source: [[FILE:ACRERN.CBL|ACRERN.CBL]], Index: code)
  - "EXEC SQL CLOSE TRB_CURSOR END-EXEC" (Source: [[FILE:TRBFLE.CBL|TRBFLE.CBL]], Index: code)
  - "PERFORM SQL-IO-VALIDATION." (Source: [[FILE:LPTRX1RN.CPY|LPTRX1RN.CPY]], Index: code)

---

### 8. **Visual Representation**

Here's a visual representation of how COBOL transaction boundaries map to API/database transaction scopes:

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL to API Transaction Mapping</text>
  
  <!-- COBOL Side -->
  <rect x="50" y="80" width="250" height="180" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="175" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Program</text>
  <text x="175" y="140" text-anchor="middle" font-size="14" fill="#4338ca">EXEC SQL COMMIT/ROLLBACK</text>
  <text x="175" y="170" text-anchor="middle" font-size="12" fill="#6366f1">Batch/Unit of Work</text>
  <text x="175" y="200" text-anchor="middle" font-size="12" fill="#6366f1">PERFORM SQL-IO-VALIDATION</text>
  
  <!-- API Side -->
  <rect x="350" y="80" width="250" height="180" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="475" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">API Endpoint</text>
  <text x="475" y="140" text-anchor="middle" font-size="14" fill="#1e40af">Begin Transaction</text>
  <text x="475" y="170" text-anchor="middle" font-size="12" fill="#3b82f6">Process Request</text>
  <text x="475" y="200" text-anchor="middle" font-size="12" fill="#3b82f6">Commit/Rollback</text>
  
  <!-- Database Side -->
  <rect x="650" y="80" width="200" height="180" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Database</text>
  <text x="750" y="140" text-anchor="middle" font-size="14" fill="#15803d">Transaction Scope</text>
  <text x="750" y="170" text-anchor="middle" font-size="12" fill="#22c55e">Commit/Rollback</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="300" y1="170" x2="350" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="600" y1="170" x2="650" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Summary:**  
- Map COBOL COMMIT/ROLLBACK logic to explicit API transaction scopes.
- Ensure each API call represents a logical unit of work with clear transaction boundaries.
- Use modern transaction management tools (e.g., TransactionScope in .NET).
- Handle errors and compensation to maintain data integrity.
- Reference COBOL code for transaction boundary locations to guide API design.

If you need specific examples from your COBOL codebase, I can analyze particular programs for their transaction boundary logic.

**Q17: What strategy should we use to handle COBOL error handling (FILE STATUS, error codes) in API error responses (HTTP status codes, error payloads)?**

To handle **COBOL error handling (FILE STATUS, error codes)** in API error responses (HTTP status codes, error payloads), you should use a **mapping strategy** that translates legacy error semantics into modern API conventions, ensuring clarity for API consumers and preserving business logic integrity.

### Recommended Strategy

#### 1. **Error Code Mapping Table**
- **Create a comprehensive mapping** between COBOL error codes (e.g., FILE STATUS, IO-FG, SQLCODE) and HTTP status codes.
- Example mappings:
  - COBOL FILE STATUS "00" (Success) → HTTP 200 OK
  - COBOL FILE STATUS "02" (Record not found) → HTTP 404 Not Found
  - COBOL FILE STATUS "22" (Duplicate key) → HTTP 409 Conflict
  - COBOL FILE STATUS "98" (File open error) → HTTP 500 Internal Server Error
  - COBOL SQLCODE 100 (No data found) → HTTP 404 Not Found
  - COBOL SQLCODE < 0 (SQL error) → HTTP 500 Internal Server Error

#### 2. **Detailed Error Payloads**
- **Include legacy error details** in the API response body for troubleshooting and traceability.
- Example payload:
  ```json
  {
    "status": 409,
    "error": "Duplicate Key",
    "cobolFileStatus": "22",
    "cobolErrorMessage": "WRITE ERROR",
    "program": "APIPAY",
    "additionalInfo": "Key already exists in PAYMENT-FILE"
  }
  ```
- Always include:
  - HTTP status code
  - Human-readable error message
  - COBOL error code(s) (FILE STATUS, SQLCODE, etc.)
  - Source program/module name
  - Any relevant context (e.g., file name, key value)

#### 3. **Preserve Business Logic**
- **Do not lose business meaning**: Some COBOL errors have specific business implications (e.g., "record locked" may mean "try again later").
- Document and surface these meanings in the API documentation and error payloads.

#### 4. **Centralized Error Handling Layer**
- Implement a **middleware or adapter** that intercepts COBOL errors and performs the mapping before sending API responses.
- This layer should:
  - Catch all error codes and messages from COBOL modules
  - Apply the mapping table
  - Format the API response consistently

#### 5. **Extensible for Migration**
- As you migrate more logic from COBOL to C#, **keep the mapping table extensible**.
- For new error types or business rules, update the mapping and documentation.

---

### Example: COBOL Error Handling Code

From [[FILE:DECLARE.CPY|DECLARE.CPY]] and [[FILE:DECLRP.CPY|DECLRP.CPY]], typical COBOL error handling looks like:
```cobol
MOVE "WRITE ERROR" TO E-MSG.
IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
MOVE 0 TO IO-FG IO-STATUS.
```
(Source: [[FILE:DECLARE.CPY|DECLARE.CPY]], Index: code)

And for SQL errors:
```cobol
MOVE "CODE: "  TO SQL-ERROR-05-SQLCODEX.
MOVE SQLCODE   TO SQL-ERROR-05-SQLCODE.
MOVE "STATE: " TO SQL-ERROR-05-SQLSTATEX.
MOVE SQLSTATE  TO SQL-ERROR-05-SQLSTATE.
```
(Source: [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]], Index: code)

---

### Implementation Steps

1. **Catalog all COBOL error codes** used in your programs (FILE STATUS, IO-FG, SQLCODE, etc.).
2. **Define the mapping table** to HTTP status codes and error messages.
3. **Implement error translation logic** in your API layer (C# or middleware).
4. **Test with real error scenarios** to ensure correct mapping and payload formatting.
5. **Document the mappings** for API consumers and support teams.

---

### Summary Table: Example Error Code Mappings

| COBOL Error Type     | COBOL Code | HTTP Status | API Error Message         |
|---------------------|------------|-------------|--------------------------|
| Success             | "00"       | 200         | OK                       |
| Record Not Found    | "02", 100  | 404         | Not Found                |
| Duplicate Key       | "22"       | 409         | Conflict                 |
| File Open Error     | "98"       | 500         | Internal Server Error    |
| SQL Error           | < 0        | 500         | Database Error           |
| Record Locked       | "91"       | 423         | Locked                   |

---

### Source References

- COBOL error handling patterns: [[FILE:DECLARE.CPY|DECLARE.CPY]], [[FILE:DECLRP.CPY|DECLRP.CPY]], [[FILE:DECLRP_SQL.CPY|DECLRP_SQL.CPY]], [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]] (Source: [[FILE:DECLARE.CPY|DECLARE.CPY]], [[FILE:DECLRP.CPY|DECLRP.CPY]], [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]], Index: code)
- Example error code assignments and business logic: [[FILE:DECLAREOD.CPY|DECLAREOD.CPY]], [[FILE:DECLRP_ASCII.CPY|DECLRP_ASCII.CPY]] (Source: [[FILE:DECLAREOD.CPY|DECLAREOD.CPY]], [[FILE:DECLRP_ASCII.CPY|DECLRP_ASCII.CPY]], Index: code)

---

**This strategy ensures that legacy COBOL error semantics are faithfully represented in modern API responses, supporting both backward compatibility and clear communication for API consumers.**

**Q18: How do we design API rate limiting and throttling to replace COBOL queue management or concurrency controls?**

To design **API rate limiting and throttling** that effectively replaces COBOL queue management or concurrency controls, you should translate the legacy mechanisms for controlling workload, user access, and resource contention into modern API governance techniques. Here’s how to approach this:

---

**1. Understand COBOL Queue Management & Concurrency Controls**

- **COBOL queue management** often uses:
  - Collector queues (e.g., CQFILE, VCQFILE) to stage work items for processing (Source: [[FILE:CQMAIN.CBL|CQMAIN.CBL]], Index: code)
  - Locking mechanisms to prevent simultaneous updates (e.g., file locks, record-level locks)
  - Concurrency controls to avoid race conditions (e.g., only one user can process a queue item at a time)
  - Batch limits (e.g., only N items processed per run, or per user session)
  - Error handling for contention (e.g., FILE STATUS 22 for duplicate access, see [[FILE:MBRUN.CBL|MBRUN.CBL]], Index: code)

---

**2. Map COBOL Controls to API Rate Limiting Concepts**

- **API rate limiting** controls how many requests a user/system can make in a given time window.
- **Throttling** slows down or blocks requests that exceed limits, preventing overload and ensuring fairness.
- **Concurrency controls** in APIs restrict simultaneous operations (e.g., only one update per resource at a time).

---

**3. Design Strategy**

**A. Define Rate Limit Policies**
   - Per-user, per-IP, or per-API-key limits (e.g., 100 requests/minute)
   - Map COBOL batch limits (e.g., “max 50 queue items per run”) to API request quotas

**B. Implement Throttling Mechanisms**
   - Use middleware or API gateways to enforce limits
   - Return HTTP 429 (Too Many Requests) when limits are exceeded
   - Include “Retry-After” headers to guide clients

**C. Replace Queue Locks with Resource-Level Concurrency Controls**
   - Use optimistic or pessimistic locking for critical resources (e.g., loan records)
   - Implement atomic operations (e.g., PATCH with If-Match headers)
   - Prevent double-processing by tracking item status (e.g., “in-process”, “completed”)

**D. Monitor and Audit**
   - Log rate limit violations and throttling events
   - Provide metrics similar to COBOL queue statistics (e.g., items processed, items pending)

**E. Graceful Error Handling**
   - Map COBOL error codes (e.g., FILE STATUS 22 for contention) to API error responses (HTTP 409 Conflict, HTTP 429 Too Many Requests)
   - Include clear error payloads with actionable messages

---

**4. Example Mapping**

| COBOL Mechanism                | API Equivalent                        |
|------------------------------- |---------------------------------------|
| CQFILE queue size limit        | API request quota (max N per window)  |
| Record-level lock              | Resource-level concurrency control    |
| Batch run limit                | Per-user rate limit                   |
| FILE STATUS 22 (duplicate)     | HTTP 409 Conflict or 429 Too Many Req |
| Queue item “in-process” flag   | Resource status field in API response |

---

**5. Business & Technical Benefits**

- **Business:** Ensures fair usage, prevents overload, and maintains data integrity as in legacy COBOL systems.
- **Technical:** Modern, scalable, and observable controls that integrate with cloud-native API platforms.

---

**Source References:**
- [[FILE:CQMAIN.CBL|CQMAIN.CBL]], Index: code – Collector queue file maintenance and queue item management
- [[FILE:MBRUN.CBL|MBRUN.CBL]], Index: code – Notes on concurrency issues and duplicate access (FILE STATUS 22)
- [[FILE:SPINQ2.CPY|SPINQ2.CPY]], Index: code – VCQFILE logic for queue processing and item status

---

**Here's a visual representation:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Queue Management vs. API Rate Limiting</text>
  
  <!-- COBOL Side -->
  <rect x="50" y="80" width="300" height="120" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="200" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Queue Management</text>
  <text x="200" y="135" text-anchor="middle" font-size="14" fill="#4338ca">CQFILE, VCQFILE, Locks</text>
  <text x="200" y="160" text-anchor="middle" font-size="12" fill="#6366f1">Batch limits, contention, status flags</text>
  
  <!-- API Side -->
  <rect x="550" y="80" width="300" height="120" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="700" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">API Rate Limiting & Throttling</text>
  <text x="700" y="135" text-anchor="middle" font-size="14" fill="#1e40af">Request quotas, HTTP 429/409</text>
  <text x="700" y="160" text-anchor="middle" font-size="12" fill="#3b82f6">Concurrency controls, error payloads</text>
  
  <!-- Mapping Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="350" y1="140" x2="550" y2="140" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <text x="450" y="130" text-anchor="middle" font-size="14" fill="#f59e0b" font-weight="bold">Map limits, locks, status to API controls</text>
  
  <!-- COBOL Error -->
  <rect x="100" y="250" width="200" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="200" y="280" text-anchor="middle" fill="white" font-size="14" font-weight="bold">FILE STATUS 22</text>
  <text x="200" y="295" text-anchor="middle" fill="white" font-size="11">Duplicate/Contention</text>
  
  <!-- API Error -->
  <rect x="600" y="250" width="200" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="700" y="280" text-anchor="middle" fill="white" font-size="14" font-weight="bold">HTTP 429/409</text>
  <text x="700" y="295" text-anchor="middle" fill="white" font-size="11">Too Many Requests/Conflict</text>
  
  <line x1="300" y1="280" x2="600" y2="280" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <text x="450" y="270" text-anchor="middle" font-size="14" fill="#f59e0b" font-weight="bold">Map error codes to API responses</text>
</svg>

---

**Summary:**  
Replace COBOL queue and concurrency controls with API rate limiting, throttling, and resource-level locks. Map legacy error codes and batch limits to modern HTTP status codes and quotas for robust, scalable API governance.

If you need specific code examples or want to see how a particular COBOL queue routine would map to API logic, let me know!

## Section 10.2: Mapping Data Structures to DTOs and Models

### COBOL to C# Data Type Mapping

**Q19: What are the best practices for mapping COBOL data structures (FD entries, 01 levels, group items) to C# DTOs and domain models?**

To map **COBOL data structures (FD entries, 01 levels, group items)** to C# DTOs and domain models, follow these best practices:

---

**1. Understand COBOL Structure Types:**
- **FD Entries:** Define file record layouts (e.g., `FD WK-FILE`), typically mapped to DTOs representing file rows.
- **01 Levels:** Top-level data structures, often representing a record or object.
- **Group Items:** Nested fields under a parent (e.g., `03`, `05` levels), map to C# classes or complex properties.

---

**2. Mapping Strategy:**

**A. Direct Field Mapping**
- Map each COBOL elementary item (e.g., `PIC 9(4)`, `PIC X(40)`) to a C# property:
  - `PIC 9(n)` → `int`, `long`, or `decimal` (depending on size/decimals)
  - `PIC X(n)` → `string`
  - `COMP-3` (packed decimal) → `decimal`
- Example:
  ```cobol
  01 FD-REC.
     03 FD1-KEY.
        05 FD-FMNO PIC 9(4).
     03 BR-NAME PIC X(40).
  ```
  C# DTO:
  ```csharp
  public class FdRecDto {
      public int FdFmno { get; set; }
      public string BrName { get; set; }
  }
  ```

**B. Group Items → Nested Classes**
- COBOL group items (e.g., `03 COCODES. 05 COCODE PIC XX OCCURS 30.`) become nested classes or collections:
  - OCCURS clause → C# arrays or `List<T>`
  - REDEFINES → Use inheritance or alternate DTOs if needed

**C. File Records → DTOs**
- Each FD record layout (from FILE SECTION) becomes a DTO representing a row in a file or table.

**D. Domain Models**
- For business logic, aggregate related DTOs into domain models reflecting business entities (e.g., Loan, Branch).

---

**3. Data Type Conversion:**
- Handle COBOL-specific types:
  - **COMP-3 (packed decimal):** Convert to `decimal` in C#
  - **Date fields (e.g., `PIC 9(8)`):** Map to `DateTime` or `string` with parsing logic
  - **Alphanumeric fields:** Map to `string`
- Example from context:
  ```cobol
  03 BR-LTOUCH-DATE PIC 9(8) COMP-3.
  03 BR-NAME        PIC X(40).
  ```
  C#:
  ```csharp
  public DateTime BrLtouchDate { get; set; } // with parsing logic
  public string BrName { get; set; }
  ```

---

**4. Handling Arrays and OCCURS:**
- COBOL:
  ```cobol
  03 COCODES.
     05 COCODE PIC XX OCCURS 30.
  ```
- C#:
  ```csharp
  public string[] Cocodes { get; set; } // or List<string>
  ```

---

**5. Dealing with REDEFINES:**
- COBOL REDEFINES allows alternate views of the same memory.
- In C#, use separate DTOs or properties, and document which fields are mutually exclusive.

---

**6. Field Naming Conventions:**
- Convert COBOL names (often uppercase, underscores) to C# PascalCase.
- Example: `FD-FMNO` → `FdFmno`

---

**7. Validation and Business Rules:**
- Implement validation logic in domain models, not DTOs.
- Use attributes or custom logic to enforce COBOL business rules (e.g., field lengths, required fields).

---

**8. Documentation and Comments:**
- Preserve COBOL comments describing field purpose in C# XML comments.
- Example:
  ```csharp
  /// <summary>
  /// Branch name (up to 40 characters)
  /// </summary>
  public string BrName { get; set; }
  ```

---

**9. Automated Tools:**
- Use code generation tools to automate mapping if possible, but always review for business logic nuances.

---

**Summary Table: COBOL → C# Mapping**

| COBOL Example                | C# Mapping Example                | Notes                          |
|------------------------------|-----------------------------------|-------------------------------|
| `PIC 9(4)`                   | `int` or `short`                  | Numeric field                 |
| `PIC X(40)`                  | `string`                          | Alphanumeric field            |
| `COMP-3`                     | `decimal`                         | Packed decimal                |
| `OCCURS 30`                  | `string[]` or `List<string>`      | Array/collection              |
| `REDEFINES`                  | Separate DTOs or properties       | Alternate views               |
| Group items (`03`, `05`)     | Nested classes/properties         | Hierarchical structure        |

---

**Source Examples:**
- FD record layout: [Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], Index: code]
- Group item with OCCURS: [Source: [[FILE:IR1098.CBL|IR1098.CBL]], Index: code]
- Packed decimal date: [Source: [[FILE:RZRNRP.CBL|RZRNRP.CBL]], Index: code]

---

**Best Practice Summary:**
- Map COBOL FD/01/group items to C# DTOs with clear type conversion.
- Use nested classes for group items, arrays for OCCURS.
- Handle REDEFINES with alternate DTOs.
- Apply business rules in domain models.
- Document field purposes and conversion logic.

If you need a specific mapping for a COBOL record, provide the record definition and I can generate the exact C# DTO for you.

**Q20: How should we convert COBOL PICTURE clauses (numeric, alphanumeric, edited fields) to appropriate C# data types (int, decimal, string)?**

To convert **COBOL PICTURE clauses** (numeric, alphanumeric, edited fields) to appropriate **C# data types**, follow these mapping guidelines:

---

### 1. Numeric PICTURE Clauses

- **Simple Numeric (e.g., PIC 9(5), PIC 99999):**
  - **COBOL:** `PIC 9(5)` (five-digit integer)
  - **C#:** `int` or `long` (depending on field size)
  - **Example:** `PIC 9(9)` → `long` in C#

- **Decimal Numeric (e.g., PIC 9(7)V99, PIC S9(9)V99):**
  - **COBOL:** `PIC 9(7)V99` (7 digits before decimal, 2 after)
  - **C#:** `decimal`
  - **Example:** `PIC S9(9)V99` → `decimal` in C#

- **Signed Numeric (e.g., PIC S9(5), PIC S9(7)V99):**
  - **COBOL:** `S` indicates signed
  - **C#:** `int`, `long`, or `decimal` (with sign)
  - **Example:** `PIC S9(7)V99` → `decimal` in C#

- **COMP-3 (Packed Decimal):**
  - **COBOL:** Often used for financials, e.g., `COMP-3`
  - **C#:** `decimal` (after unpacking)

---

### 2. Alphanumeric PICTURE Clauses

- **Simple Alphanumeric (e.g., PIC X(10), PIC X(40)):**
  - **COBOL:** `PIC X(10)` (10-character string)
  - **C#:** `string`
  - **Example:** `PIC X(40)` → `string` in C#

---

### 3. Edited Fields (Display Formatting)

- **Edited Numeric (e.g., PIC ZZZ,ZZZ.99, PIC ZZ9.99-):**
  - **COBOL:** Used for formatted output, not storage
  - **C#:** Store as `decimal` or `int`, format for display using `.ToString("N2")` or custom format strings
  - **Example:**  
    - `PIC ZZZ,ZZZ.99` → `decimal` in C#, display as `value.ToString("N2")`
    - `PIC ZZ9.99-` → `decimal`, format for sign

---

### 4. Special Cases

- **Single Character (e.g., PIC X):**
  - **COBOL:** `PIC X` (single character)
  - **C#:** `char` or `string` (length 1)

- **Date Fields (e.g., PIC 99/99/9999):**
  - **COBOL:** Often stored as string or numeric
  - **C#:** Prefer `DateTime` if possible, else `string`

---

### 5. Mapping Table

| COBOL PICTURE Example   | Typical Meaning         | C# Data Type   | Notes                                 |
|------------------------ |------------------------|--------------- |---------------------------------------|
| PIC 9(5)                | 5-digit integer        | int            | Use long for >9 digits                |
| PIC S9(9)               | Signed 9-digit integer | long           |                                      |
| PIC 9(7)V99             | 7 digits, 2 decimals   | decimal        | V = implied decimal                   |
| PIC S9(7)V99 COMP-3     | Signed packed decimal  | decimal        | Unpack COMP-3                         |
| PIC X(10)               | 10-char string         | string         |                                      |
| PIC ZZZ,ZZZ.99          | Formatted numeric      | decimal        | Format for display in C#              |
| PIC X                   | Single character       | char/string    |                                      |
| PIC 99/99/9999          | Date (MM/DD/YYYY)      | DateTime/string| Parse to DateTime if possible         |

---

### 6. Example Conversion

**COBOL:**
```cobol
01  CUSTOMER-NAME   PIC X(40).
01  ACCOUNT-BAL     PIC S9(7)V99 COMP-3.
01  ACCOUNT-NO      PIC 9(9).
01  LAST-PAY-DATE   PIC 99/99/9999.
```

**C# DTO:**
```csharp
public class AccountDto
{
    public string CustomerName { get; set; }      // PIC X(40)
    public decimal AccountBalance { get; set; }   // PIC S9(7)V99 COMP-3
    public long AccountNumber { get; set; }       // PIC 9(9)
    public string LastPayDate { get; set; }       // PIC 99/99/9999 (or DateTime if parsed)
}
```

---

### 7. Key Best Practices

- **Always use C# native types for storage; format for display as needed.**
- **For edited fields, store as numeric and apply formatting in presentation layer.**
- **For COMP-3, unpack to decimal before mapping.**
- **For dates, parse to DateTime if possible, else use string.**
- **Document any assumptions about field length, sign, or formatting.**

---

**Source references:**
- Examples from [[FILE:MODBOR.CBL|MODBOR.CBL]], [[FILE:TRKMN3.CBL|TRKMN3.CBL]], [[FILE:IOFDBI.CBL|IOFDBI.CBL]], [[FILE:CHLOAN.CBL|CHLOAN.CBL]], [[FILE:PLSRC2.CBL|PLSRC2.CBL]], [[FILE:AQMAIN.CBL|AQMAIN.CBL]], [[FILE:LP01BY_SQL.CPY|LP01BY_SQL.CPY]] (Source: various code files, Index: code)
- Mapping conventions based on COBOL and C# type systems

If you need help with a specific COBOL structure, provide the PICTURE clause and I'll give the exact C# mapping.

**Q21: What strategy should we use to handle COBOL REDEFINES clauses when designing C# DTOs (union types, inheritance, separate models)?**

To handle **COBOL REDEFINES clauses** when designing C# DTOs, use a strategy that accurately models the union-like behavior and memory overlay semantics of REDEFINES. Here are best practices and recommended approaches:

---

### 1. **Understand COBOL REDEFINES**

- **Purpose:** REDEFINES allows multiple data layouts to share the same memory area, acting like a union or overlay.
- **Common Use Cases:** Variant records, alternative interpretations of the same bytes, conditional data formats.

**Example from context:**
```cobol
03  D-XX.
    05  D-FLAG                PIC X.
    05  FILLER                PIC X(5).

03  D-XZ REDEFINES D-XX.
    05  D-FLAG3               PIC X(3).
    05  FILLER                PIC X.
```
(Source: [[FILE:BRLIST.CBL|BRLIST.CBL]], Lines 176-200)

---

### 2. **C# Mapping Strategies**

#### **A. Union Types / Discriminated Unions (Recommended for Variant Records)**
- **Approach:** Use a C# class with a discriminant (e.g., an enum or flag) and separate properties for each possible layout.
- **Implementation:** Only one property is populated at a time, based on the discriminant value.
- **Example:**
  ```csharp
  public enum DVariantType { XX, XZ }

  public class DRecordDto
  {
      public DVariantType Variant { get; set; }
      public DXXDto DXX { get; set; }      // For D-XX layout
      public DXZDto DXZ { get; set; }      // For D-XZ layout
  }

  public class DXXDto
  {
      public string DFlag { get; set; }
      // ...
  }

  public class DXZDto
  {
      public string DFlag3 { get; set; }
      // ...
  }
  ```
- **When to use:** Most common for REDEFINES, especially when the overlay is used for variant records.

#### **B. Inheritance (For Related Types with Shared Base)**
- **Approach:** Use a base class for shared fields, with derived classes for each REDEFINES variant.
- **Implementation:** Only instantiate the derived class that matches the actual data format.
- **Example:**
  ```csharp
  public abstract class DBaseDto { /* shared fields */ }
  public class DXXDto : DBaseDto { /* D-XX fields */ }
  public class DXZDto : DBaseDto { /* D-XZ fields */ }
  ```
- **When to use:** When REDEFINES overlays extend or specialize a base structure.

#### **C. Separate Models (For Completely Distinct Layouts)**
- **Approach:** Define separate DTOs for each REDEFINES layout, and use a wrapper or context to select the correct one.
- **Implementation:** The wrapper contains only one of the possible DTOs at a time.
- **Example:**
  ```csharp
  public class DRecordWrapper
  {
      public DXXDto DXX { get; set; }
      public DXZDto DXZ { get; set; }
      // Only one is non-null
  }
  ```
- **When to use:** When layouts are unrelated and never used together.

---

### 3. **Additional Considerations**

- **Serialization:** Ensure only the active variant is serialized/deserialized.
- **Validation:** Use the discriminant to enforce which fields are valid.
- **Memory Overlay:** C# does not support true memory overlays; use mapping logic to convert raw bytes to the correct DTO.
- **Nullable Properties:** For overlays, make non-active properties nullable or ignore during serialization.

---

### 4. **Summary Table**

| COBOL Pattern         | C# Strategy             | Use Case Example                |
|----------------------|-------------------------|---------------------------------|
| REDEFINES (variant)  | Discriminated Union     | Variant record, union           |
| REDEFINES (extends)  | Inheritance             | Base + specialized overlays     |
| REDEFINES (distinct) | Separate Models         | Unrelated layouts, wrapper      |

---

### 5. **Source Reference**

- Example REDEFINES usage: [[FILE:BRLIST.CBL|BRLIST.CBL]], Lines 176-200
- Overlay for address layouts: [[FILE:LP01LT.CPY|LP01LT.CPY]], Lines 351-375

---

**Best Practice:**  
Use **discriminated unions** (with a variant type and separate DTOs for each REDEFINES layout) for most COBOL REDEFINES cases. Use inheritance only if overlays extend a base structure. Always ensure only one variant is active at a time in your C# model.

If you need a diagram of how these mappings work, let me know!

**Q22: How do we map COBOL OCCURS clauses (arrays, tables) to C# collections (List<T>, arrays, dictionaries)?**

To map **COBOL OCCURS clauses** (arrays, tables) to C# collections, follow these best practices:

---

### 1. Understand COBOL OCCURS Semantics

- **OCCURS** defines an array or table of items, often with nested levels.
- Example:
  ```cobol
  01 TOTAL-TABLE.
     03 TOTAL-TBL OCCURS 3 TIMES.
        05 T-AGETBL OCCURS 10 TIMES.
           07 T-TYPETBL OCCURS 3 TIMES.
              09 T-NO   PIC S9(5).
              09 T-BAL  PIC S9(9)V99.
  ```
  (Source: [[FILE:CLAST1.CBL|CLAST1.CBL]], Index: code)

---

### 2. Map OCCURS to C# Collections

- **Simple OCCURS (1D array):**
  - COBOL: `03 ITEM OCCURS 10 TIMES.`
  - C#: `List<Item>` or `Item[]`
- **Nested OCCURS (multi-dimensional):**
  - COBOL: `03 OUTER OCCURS 3 TIMES. 05 INNER OCCURS 10 TIMES.`
  - C#: `List<List<Inner>>` or `Outer[][]`
- **Tables with group items:**
  - Define a C# class for the group, then use a collection of that class.

#### Example Mapping

COBOL:
```cobol
01 TOTAL-TABLE.
   03 TOTAL-TBL OCCURS 3 TIMES.
      05 T-AGETBL OCCURS 10 TIMES.
         07 T-TYPETBL OCCURS 3 TIMES.
            09 T-NO   PIC S9(5).
            09 T-BAL  PIC S9(9)V99.
```
(Source: [[FILE:CLAST1.CBL|CLAST1.CBL]], Index: code)

C#:
```csharp
public class TypeTbl {
    public int No { get; set; }
    public decimal Bal { get; set; }
}

public class AgeTbl {
    public List<TypeTbl> TypeTbls { get; set; } = new List<TypeTbl>();
}

public class TotalTbl {
    public List<AgeTbl> AgeTbls { get; set; } = new List<AgeTbl>();
}

public class TotalTable {
    public List<TotalTbl> TotalTbls { get; set; } = new List<TotalTbl>();
}
```

---

### 3. Choose the Right C# Collection Type

- **List<T>**: Preferred for variable-length arrays/tables.
- **T[]**: Use for fixed-length arrays if size is always constant.
- **Dictionary<TKey, TValue>**: Use if COBOL table is accessed by key (rare; usually indexed by position).
- **Multi-dimensional arrays**: Only if the COBOL OCCURS are strictly rectangular and fixed-size.

---

### 4. Handle Indexed Access

- COBOL often uses subscripts: `TYPETBL(I, J, K)`
- In C#, access via collection indexers: `TotalTbls[i].AgeTbls[j].TypeTbls[k]`

---

### 5. Special Cases

- **OCCURS DEPENDING ON**: If the array/table size is dynamic, use `List<T>` and set the count at runtime.
- **REDEFINES with OCCURS**: Model as separate DTOs or use union-like structures.

---

### 6. Example from Context

COBOL:
```cobol
01 TOTAL-TABLE VALUE ZEROS.
   03 CLASS-TBL OCCURS 2 TIMES.
      05 TOTAL-TBL OCCURS 3 TIMES.
         07 T-NM-TBL OCCURS 31 TIMES.
            09 T-NM-NO   PIC S9(7).
            09 T-NM-BAL  PIC S9(9)V99.
```
(Source: [[FILE:CPCORP.CBL|CPCORP.CBL]], Index: code)

C#:
```csharp
public class TnmTbl {
    public int NmNo { get; set; }
    public decimal NmBal { get; set; }
}

public class TotalTbl {
    public List<TnmTbl> TnmTbls { get; set; } = new List<TnmTbl>();
}

public class ClassTbl {
    public List<TotalTbl> TotalTbls { get; set; } = new List<TotalTbl>();
}

public class TotalTable {
    public List<ClassTbl> ClassTbls { get; set; } = new List<ClassTbl>();
}
```

---

## Summary Table

| COBOL OCCURS Pattern         | C# Mapping                | Notes                          |
|-----------------------------|---------------------------|--------------------------------|
| OCCURS N TIMES              | List<T> or T[]            | Use List<T> for flexibility    |
| Nested OCCURS               | List<List<T>> or T[][]    | Model each group as a class    |
| OCCURS DEPENDING ON         | List<T>                   | Set count at runtime           |
| Indexed access              | Collection indexers       | Use [i], [j], [k]              |

---

## Best Practice

- **Model each group item as a C# class.**
- **Use List<T> for each OCCURS level.**
- **Preserve nesting to match COBOL structure.**
- **Initialize lists to avoid null references.**

---

**Source References:**
- [[FILE:CLAST1.CBL|CLAST1.CBL]], Index: code
- [[FILE:CPCORP.CBL|CPCORP.CBL]], Index: code

If you need help with a specific COBOL table or OCCURS clause, provide the code and I can generate the exact C# mapping.

**Q23: What approach should we take for converting COBOL hierarchical data structures (nested group items) to nested C# objects or flattened DTOs?**

To convert **COBOL hierarchical data structures** (nested group items) to C# objects, you should choose an approach that balances **data fidelity**, **clarity**, and **ease of use** for your application. Here are best practices and recommendations:

---

### 1. **Direct Mapping to Nested C# Classes (Recommended for Most Cases)**

- **COBOL group items** (e.g., 01, 03, 05 levels) represent nested records and sub-records.
- **C# classes** can mirror this hierarchy using nested classes or properties.

**Example:**

COBOL:
```cobol
01 CUSTOMER-RECORD.
   05 NAME.
      10 FIRST-NAME PIC X(10).
      10 LAST-NAME  PIC X(15).
   05 ADDRESS.
      10 STREET     PIC X(20).
      10 CITY       PIC X(15).
      10 ZIP        PIC 9(5).
   05 BALANCE       PIC 9(7)V99.
```

C#:
```csharp
public class CustomerRecord
{
    public NameGroup Name { get; set; }
    public AddressGroup Address { get; set; }
    public decimal Balance { get; set; }
}

public class NameGroup
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
}

public class AddressGroup
{
    public string Street { get; set; }
    public string City { get; set; }
    public int Zip { get; set; }
}
```

**Advantages:**
- Preserves the original structure and meaning.
- Easier to maintain and extend.
- Supports serialization/deserialization for APIs.

---

### 2. **Flattening to a Single DTO (Use Only for Simple or Legacy Integration)**

- **Flattening** means representing all fields at the top level of a DTO, losing the hierarchical grouping.
- Useful if the hierarchy is shallow or if integration targets require flat structures.

**Example:**
```csharp
public class CustomerRecordFlat
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string Street { get; set; }
    public string City { get; set; }
    public int Zip { get; set; }
    public decimal Balance { get; set; }
}
```

**Disadvantages:**
- Loses semantic grouping (e.g., can't easily distinguish address vs. name fields).
- Harder to maintain if the COBOL structure changes or is reused elsewhere.

---

### 3. **Guidelines for Choosing Between Nested vs. Flattened**

- **Use Nested Classes** when:
  - The COBOL structure is reused in multiple places (modularity).
  - The hierarchy is meaningful (e.g., address, name, payment info).
  - You need to support future extensions or complex business logic.

- **Flatten Only** when:
  - The structure is very simple and unlikely to change.
  - Integration targets (e.g., flat database tables, legacy APIs) require it.
  - Performance or serialization constraints demand a flat structure.

---

### 4. **Special Considerations**

- **OCCURS Clauses:** Map to collections (see previous answer).
- **REDEFINES Clauses:** Use union types or inheritance (see previous answer).
- **FILLER Fields:** Omit from C# DTOs unless needed for byte alignment.

---

### 5. **Real Example from Context**

From [Source: [[FILE:MBALR5.CBL|MBALR5.CBL]], Index: code]:
```cobol
03  D2-GROUP             PIC X(19).
03  D2-GROUP-1           REDEFINES D2-GROUP.
    05  DS-CLASS         PIC 99.
    05  FILLER           PIC X.
    05  DS-DESC          PIC X(16).
03  D2-GROUP-2           REDEFINES D2-GROUP.
    05  D2-PROMPT1       PIC X(7).
    05  FILLER           PIC X(2).
    05  D2-GRNO          PIC X(4).
    05  D2-BRNO          REDEFINES D2-GRNO
```
**Recommended C# Mapping:**
- Use a base class or interface for D2-GROUP, with derived classes for each REDEFINES variant.
- Each derived class contains its own nested properties.

---

## Summary Table

| COBOL Structure Type | Recommended C# Mapping | When to Use |
|---------------------|------------------------|-------------|
| Nested GROUP items  | Nested classes         | Most cases  |
| Flat structure      | Flat DTO               | Simple/legacy integration |
| REDEFINES           | Union/inheritance      | When overlays present |

---

**Conclusion:**  
**Prefer nested C# classes** to preserve the COBOL hierarchy and semantics. Flatten only when required by integration constraints. Always analyze the business meaning of the groups before deciding.

(Source: [[FILE:MBALR5.CBL|MBALR5.CBL]], Index: code; [[FILE:EMPORG.CBL|EMPORG.CBL]], Index: code; [[FILE:DLLIST.CBL|DLLIST.CBL]], Index: code)

### Complex Data Structures and Patterns

**Q24: How can we handle COBOL COMP-3 (packed decimal) fields in C# DTOs, ensuring precision and performance?**

To handle **COBOL COMP-3 (packed decimal) fields** in C# DTOs while ensuring **precision** and **performance**, follow these best practices:

---

### 1. **Understand COMP-3 Semantics**

- **COMP-3** (packed decimal) stores decimal numbers in a compact binary format, with each digit occupying 4 bits (a nibble).
- Used for financial data to maintain **exact decimal precision** (no floating-point rounding errors).

---

### 2. **C# Data Type Mapping**

- **Use `decimal` type in C#:**
  - C# `decimal` provides high precision (28-29 significant digits) and is ideal for financial calculations.
  - Example mapping:
    - COBOL: `PIC S9(9)V99 COMP-3` (signed, 9 digits before decimal, 2 after)
    - C#: `decimal Amount` (with appropriate scale)

---

### 3. **Serialization/Deserialization Strategy**

- **When reading/writing raw COBOL data files:**
  - You must **decode/encode COMP-3** fields to/from C# `decimal`.
  - Use a library or custom code to:
    - Parse packed decimal bytes into a C# `decimal`.
    - Convert C# `decimal` back to packed decimal bytes for output.

- **If using middleware (e.g., mainframe integration tools):**
  - Many tools (e.g., IBM Host Integration, Micro Focus) automatically map COMP-3 to .NET `decimal` or `BigInteger`.
  - Validate that the mapping preserves scale and sign.

---

### 4. **Precision and Performance Considerations**

- **Precision:**
  - Always use `decimal` (not `double` or `float`) to avoid rounding errors.
  - Ensure correct scale (number of decimal places) matches COBOL definition (`V99` = 2 decimal places).

- **Performance:**
  - For bulk data, use efficient byte array parsing (avoid string conversions).
  - Cache field definitions (length, scale) to avoid repeated lookups.
  - If possible, batch conversions for arrays (OCCURS) to minimize overhead.

---

### 5. **Example Conversion Code**

**COBOL Field:**
```cobol
03  TOT-AMT-PB PIC S9(9)V99 COMP-3.
```
(Source: [[FILE:PLSRC2.CBL|PLSRC2.CBL]], Index: code)

**C# DTO:**
```csharp
public decimal TotalAmountPB { get; set; }
```

**COMP-3 Decoding Example:**
```csharp
public decimal DecodeComp3(byte[] packed, int scale)
{
    // Implement packed decimal to decimal conversion
    // Use libraries like HostFileParser, or custom logic
}
```

---

### 6. **Testing and Validation**

- **Test with real data:** Validate conversion with sample COBOL files to ensure accuracy.
- **Edge cases:** Handle negative values, zero, and maximum field sizes.

---

### 7. **Summary Table**

| COBOL COMP-3 Field         | C# DTO Property      | Notes                       |
|----------------------------|----------------------|-----------------------------|
| PIC S9(7) COMP-3           | decimal              | 7 digits, signed            |
| PIC S9(9)V99 COMP-3        | decimal (scale=2)    | 9 digits + 2 decimals       |
| PIC 9(5) COMP-3            | decimal              | 5 digits, unsigned          |

---

**In summary:**  
- Map COMP-3 fields to C# `decimal` for precision.
- Use efficient byte parsing for performance.
- Validate scale and sign.
- Use libraries or custom code for packed decimal conversion.

**Source references:**  
- Field examples from [[FILE:PLSRC2.CBL|PLSRC2.CBL]] (Index: code)
- General COMP-3 usage in COBOL programs (e.g., [[FILE:FDLIST.CBL|FDLIST.CBL]], [[FILE:CALCZL.CBL|CALCZL.CBL]])

---

Here's a visual representation of the mapping process:

<svg width="700" height="300" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">COBOL COMP-3 to C# DTO Mapping</text>
  
  <!-- COBOL Field -->
  <rect x="50" y="80" width="180" height="70" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">COBOL COMP-3</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="12">PIC S9(9)V99 COMP-3</text>
  
  <!-- Arrow to C# -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="115" x2="320" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- C# DTO Field -->
  <rect x="320" y="80" width="180" height="70" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="410" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">C# DTO</text>
  <text x="410" y="130" text-anchor="middle" fill="white" font-size="12">decimal TotalAmountPB</text>
  
  <!-- Arrow to Conversion Logic -->
  <line x1="500" y1="115" x2="590" y2="115" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- Conversion Logic -->
  <rect x="590" y="80" width="80" height="70" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="630" y="110" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Decode</text>
  <text x="630" y="130" text-anchor="middle" fill="white" font-size="11">COMP-3</text>
</svg>

**Q25: What patterns should we use to represent COBOL sign conventions (SIGN LEADING/TRAILING) in C# numeric types?**

To represent **COBOL sign conventions** (SIGN LEADING/TRAILING) in C# numeric types, use the following patterns:

---

### 1. **Understand COBOL Sign Conventions**

- **SIGN LEADING/TRAILING**: Indicates where the sign (+/-) is stored in the COBOL field (either before or after the digits).
- **SIGN SEPARATE**: The sign is stored as a separate character.
- **COMP-3 (Packed Decimal)**: The sign is encoded in the last nibble of the packed field.

---

### 2. **C# Numeric Types**

- C# types like `int`, `long`, `decimal`, and `double` natively support positive and negative values.
- The sign is always part of the value, not stored separately.

---

### 3. **Conversion Patterns**

#### **A. Parsing COBOL Data to C#**

- **When reading COBOL data files**, you must interpret the sign position and convert it to the C# numeric sign.
- Use a parsing function that:
  - Detects the sign character (leading/trailing/separate).
  - Applies it to the numeric value in C#.

**Example Pattern:**

```csharp
// For SIGN TRAILING SEPARATE (e.g., "1234-")
string cobolValue = "1234-";
int value = int.Parse(cobolValue.Substring(0, cobolValue.Length - 1));
if (cobolValue.EndsWith("-")) value = -value;

// For SIGN LEADING SEPARATE (e.g., "-1234")
string cobolValue = "-1234";
int value = int.Parse(cobolValue);
```

#### **B. Serializing C# Values to COBOL Format**

- When writing data back to COBOL format, format the sign as required:
  - **Leading sign**: `value.ToString("+#;-#;0")`
  - **Trailing sign**: Custom formatting (append sign after digits)
  - **Separate sign**: Store sign in a separate field

**Example Pattern:**

```csharp
// For SIGN TRAILING SEPARATE
int value = -1234;
string cobolValue = Math.Abs(value).ToString() + (value < 0 ? "-" : "+");

// For SIGN LEADING SEPARATE
string cobolValue = (value < 0 ? "-" : "+") + Math.Abs(value).ToString();
```

---

### 4. **COMP-3 (Packed Decimal) Special Handling**

- Use a library or custom code to decode/encode packed decimals, extracting the sign from the last nibble.
- Map to C# `decimal` or `long` with correct sign.

---

### 5. **Recommended Patterns**

| COBOL Convention         | C# Representation Pattern                |
|-------------------------|------------------------------------------|
| SIGN LEADING            | Parse/format sign at start of string     |
| SIGN TRAILING           | Parse/format sign at end of string       |
| SIGN SEPARATE           | Use separate field for sign              |
| COMP-3 (Packed Decimal) | Decode sign from packed bytes            |

---

### 6. **Summary**

- **Always convert sign conventions to C#'s native signed numeric types.**
- **Preserve sign position only when serializing/deserializing to COBOL formats.**
- **Use parsing/formatting helpers to handle sign location.**

---

**Source Reference:**  
- COBOL sign conventions: [Source: [[FILE:GL01FS.CPY|GL01FS.CPY]], Index: code]  
  Example:  
  ```cobol
  05  FS-CMONTH            PIC S9(9)V99 COMP-3.
  05  FS-LYMONTH           PIC S9(9)V99 COMP-3.
  05  FS-CYTD              PIC S9(9)V99 COMP-3.
  05  FS-LYTD              PIC S9(9)V99 COMP-3.
  ```
  These fields use COMP-3 (packed decimal) with sign encoded in the last nibble.

---

**Best Practice:**  
- Use C# signed numeric types for all business logic.
- Implement sign-aware parsing/serialization for COBOL interoperability.

**Q26: How do we design DTOs for COBOL copybooks that are used across multiple programs (shared data definitions)?**

When designing DTOs (Data Transfer Objects) for **COBOL copybooks** that are **shared across multiple programs** in the OTIS application, you should follow these patterns to ensure **consistency**, **reusability**, and **maintainability**:

---

## 1. **Centralize DTO Definitions**

- **Create a single DTO class per copybook**: Each COBOL copybook (e.g., `[[FILE:LP01SP.CPY|LP01SP.CPY]]`) should map to a single C# class or struct.
- **Namespace organization**: Place shared DTOs in a common namespace/module (e.g., `Otis.SharedDtos`), so all programs reference the same definition.

---

## 2. **Preserve Copybook Structure**

- **Group items as nested classes/properties**: If the copybook has nested group items, represent them as nested objects or properties in the DTO.
- **Field naming**: Use the copybook field names (possibly PascalCase for C#) to maintain clarity and traceability.

---

## 3. **Handle Data Types and Conventions**

- **Numeric fields**: Map COBOL numeric types (COMP, COMP-3, etc.) to appropriate C# types (`int`, `decimal`, etc.), handling packed decimals as described previously.
- **Sign conventions**: If the copybook uses SIGN LEADING/TRAILING, ensure your DTOs and serialization logic handle sign placement correctly.

---

## 4. **Flatten Only When Necessary**

- **Default: Nested structure**: Keep the hierarchical structure unless a specific consumer requires a flattened DTO.
- **Flattening for APIs/UI**: If a program or API needs a flat DTO, create a mapping layer (e.g., AutoMapper profile) that flattens nested objects into a flat DTO.

---

## 5. **Versioning and Compatibility**

- **DTO versioning**: If copybooks change over time, version your DTOs (e.g., `LoanPaymentV1`, `LoanPaymentV2`) to avoid breaking consumers.
- **Backward compatibility**: Maintain old DTOs for legacy programs if needed.

---

## 6. **Documentation and Metadata**

- **Annotate DTOs**: Use XML comments or attributes to document field origins (e.g., `[Source: [[FILE:LP01SP.CPY|LP01SP.CPY]]]`), business meaning, and any special handling.
- **Reference copybook**: Include the copybook name in the DTO documentation for traceability.

---

## Example Mapping

Suppose you have a copybook `[[FILE:LP01SP.CPY|LP01SP.CPY]]` used by multiple programs:

**COBOL Copybook Example:**
```cobol
01 LOAN-PAYMENT.
   05 PAYMENT-AMOUNT      PIC S9(7)V99 COMP-3.
   05 PAYMENT-DATE        PIC X(8).
   05 PAYMENT-DETAILS.
      10 PAYMENT-TYPE     PIC X(2).
      10 PAYMENT-METHOD   PIC X(1).
```

**C# DTO Example:**
```csharp
namespace Otis.SharedDtos
{
    /// <summary>
    /// DTO for [[FILE:LP01SP.CPY|LP01SP.CPY]] (Loan Payment Structure)
    /// Used by: APIPAY, REFUPD, LONPW9, etc.
    /// </summary>
    public class LoanPaymentDto
    {
        public decimal PaymentAmount { get; set; }      // COMP-3, handle packed decimal
        public string PaymentDate { get; set; }         // YYYYMMDD
        public PaymentDetailsDto PaymentDetails { get; set; }
    }

    public class PaymentDetailsDto
    {
        public string PaymentType { get; set; }
        public string PaymentMethod { get; set; }
    }
}
```

---

## 7. **Usage Across Programs**

- **Reference the shared DTO** in all relevant business logic, service layers, and API contracts.
- **Avoid duplicating DTOs** for the same copybook in different modules—use the centralized definition.

---

## Summary Table

| Pattern                | Benefit                       | Example                |
|------------------------|------------------------------|------------------------|
| Centralized DTO        | Consistency, reuse           | `LoanPaymentDto`       |
| Preserve hierarchy     | Data fidelity, clarity        | Nested DTOs            |
| Flatten when needed    | API/UI compatibility          | Mapping layer          |
| Version DTOs           | Change management             | `LoanPaymentV2`        |
| Annotate/document      | Traceability, maintainability | XML comments           |

---

**In OTIS, shared copybook DTOs should be centralized, preserve structure, and be referenced by all programs that use the copybook. Flattening should be handled via mapping layers only when required by consumers.**

If you need specific copybook-to-DTO mappings or examples from actual OTIS copybooks, please provide the copybook content or names.

**Q27: What strategy should we use to handle COBOL date formats (YYMMDD, CCYYMMDD) and convert them to C# DateTime or DateOnly types?**

To handle **COBOL date formats** such as **YYMMDD** and **CCYYMMDD** and convert them to C# `DateTime` or `DateOnly` types, use the following strategy:

---

## 1. **Understand COBOL Date Field Layouts**

- **YYMMDD**: 6-digit numeric, e.g., `230501` for May 1, 2023.
- **CCYYMMDD**: 8-digit numeric, e.g., `20230501` for May 1, 2023.
- COBOL often stores these as `PIC 9(6)` or `PIC 9(8)` fields, sometimes as COMP-3 (packed decimal).

---

## 2. **Extract and Parse Numeric Fields**

- Read the COBOL field as a string or integer (after unpacking COMP-3 if needed).
- For **YYMMDD**:
  - Extract substrings: `YY`, `MM`, `DD`.
  - Decide century logic (see below).
- For **CCYYMMDD**:
  - Extract substrings: `CCYY`, `MM`, `DD`.

---

## 3. **Century Logic for YYMMDD**

- COBOL often uses a cutoff (e.g., if `YY < 50` then 2000s, else 1900s).
- Example from [[FILE:DATER.CPY|DATER.CPY]]:
  ```cobol
  IF ( DATE-YYYYMMDD-YYYY < EXT-JULIAN-CC )
     ADD 2000 TO DATE-YYYYMMDD-YYYY
  ELSE
     ADD 1900 TO DATE-YYYYMMDD-YYYY
  ```
  (Source: [[FILE:DATER.CPY|DATER.CPY]], Lines 851-875)
- In C#, implement similar logic:
  ```csharp
  int year = int.Parse(yy);
  year += (year < 50) ? 2000 : 1900;
  ```

---

## 4. **Conversion Patterns in C#**

### **YYMMDD Example**
```csharp
string cobolDate = "230501"; // YYMMDD
int year = int.Parse(cobolDate.Substring(0, 2));
int month = int.Parse(cobolDate.Substring(2, 2));
int day = int.Parse(cobolDate.Substring(4, 2));
year += (year < 50) ? 2000 : 1900; // Century logic
var date = new DateOnly(year, month, day); // .NET 6+
```

### **CCYYMMDD Example**
```csharp
string cobolDate = "20230501"; // CCYYMMDD
int year = int.Parse(cobolDate.Substring(0, 4));
int month = int.Parse(cobolDate.Substring(4, 2));
int day = int.Parse(cobolDate.Substring(6, 2));
var date = new DateOnly(year, month, day); // or DateTime
```

---

## 5. **Handle Invalid Dates**

- COBOL often uses `000000` or `00000000` for "no date".
- In C#, check for all-zero values and map to `null` or a default value.

---

## 6. **Performance and Precision**

- Use direct substring parsing for speed.
- Avoid `DateTime.Parse` with custom formats for bulk conversion.
- For packed decimal (COMP-3), unpack first, then apply above logic.

---

## 7. **Automate with Helper Methods**

Create reusable methods for conversion:
```csharp
public static DateOnly? ParseCobolYYMMDD(string cobolDate)
{
    if (string.IsNullOrWhiteSpace(cobolDate) || cobolDate == "000000")
        return null;
    int year = int.Parse(cobolDate.Substring(0, 2));
    int month = int.Parse(cobolDate.Substring(2, 2));
    int day = int.Parse(cobolDate.Substring(4, 2));
    year += (year < 50) ? 2000 : 1900;
    return new DateOnly(year, month, day);
}

public static DateOnly? ParseCobolCCYYMMDD(string cobolDate)
{
    if (string.IsNullOrWhiteSpace(cobolDate) || cobolDate == "00000000")
        return null;
    int year = int.Parse(cobolDate.Substring(0, 4));
    int month = int.Parse(cobolDate.Substring(4, 2));
    int day = int.Parse(cobolDate.Substring(6, 2));
    return new DateOnly(year, month, day);
}
```

---

## 8. **Reference: COBOL Conversion Routines**

- COBOL routines like `CONVERT-YYMMDD-TO-YYYYMMDD`, `CONVERT-MMDDYY-TO-YYYYMMDD`, etc., perform similar logic (Source: [[FILE:DATER.CPY|DATER.CPY]], Lines 851-875, 551-575, 676-700).
- These routines move substrings and adjust century as shown above.

---

## Summary Table

| COBOL Format | Example    | C# Conversion Logic                | Century Handling      |
|--------------|------------|------------------------------------|----------------------|
| YYMMDD       | 230501     | Substring(0,2), (2,2), (4,2)       | If <50, +2000; else +1900 |
| CCYYMMDD     | 20230501   | Substring(0,4), (4,2), (6,2)       | Direct               |

---

**In summary:**  
- Parse substrings for year, month, day.
- Apply century logic for YYMMDD.
- Map zero dates to null/default.
- Use helper methods for consistency and performance.

(Source: [[FILE:DATER.CPY|DATER.CPY]], Lines 851-875, 551-575, 676-700; [[FILE:UP1534.CBL|UP1534.CBL]], Lines 6676-6700)

**Q28: How can we map COBOL conditional fields (88-level condition names) to C# enums or boolean properties?**

To map **COBOL conditional fields (88-level condition names)** to C# types, use the following strategy:

---

### 1. **Understand COBOL 88-Level Condition Names**

- **88-levels** define named conditions for a parent field.
- Example:
  ```cobol
  03  BR-CAN-LOANS-FG          PIC X.
      88  BR-CAN-LOANS-YES     VALUE "Y".
      88  BR-CAN-LOANS-NO      VALUE "N".
      88  BR-CAN-LOANS-ONLY    VALUE "O".
      88  BR-CAN-LOANS-VALID   VALUE "Y" "N" "O".
  ```
  (Source: [[FILE:RZCORP.CBL|RZCORP.CBL]], Index: code)

---

### 2. **Mapping Strategies in C#**

#### **A. Boolean Properties (for simple yes/no conditions)**
- If the parent field has only two possible values (e.g., "Y"/"N"):
  ```csharp
  public bool CanLoans { get; set; } // true if "Y", false if "N"
  ```
  - Conversion logic:
    ```csharp
    CanLoans = cobolValue == "Y";
    ```

#### **B. Enums (for multi-valued conditions)**
- If the parent field has more than two possible values:
  ```csharp
  public enum CanLoansFlag
  {
      Yes,    // "Y"
      No,     // "N"
      Only    // "O"
  }

  public CanLoansFlag CanLoans { get; set; }
  ```
  - Conversion logic:
    ```csharp
    switch (cobolValue)
    {
        case "Y": CanLoans = CanLoansFlag.Yes; break;
        case "N": CanLoans = CanLoansFlag.No; break;
        case "O": CanLoans = CanLoansFlag.Only; break;
        default: throw new InvalidOperationException("Unknown value");
    }
    ```

#### **C. Validation/Set Membership**
- For conditions like `BR-CAN-LOANS-VALID VALUE "Y" "N" "O"`, use validation logic:
  ```csharp
  bool IsValid = cobolValue == "Y" || cobolValue == "N" || cobolValue == "O";
  ```

---

### 3. **General Mapping Pattern**

- **Single 88-level (e.g., "YES")** → C# `bool` property
- **Multiple 88-levels (e.g., "YES", "NO", "ONLY")** → C# `enum`
- **88-levels representing sets/validation** → C# validation logic or helper method

---

### 4. **Example Mapping Table**

| COBOL Field           | 88-Level(s)                | C# Type/Property         | Conversion Logic                |
|-----------------------|----------------------------|--------------------------|---------------------------------|
| BR-CAN-LOANS-FG PIC X | YES ("Y"), NO ("N")        | bool CanLoans            | value == "Y"                    |
| BR-CAN-LOANS-FG PIC X | YES, NO, ONLY ("O")        | enum CanLoansFlag        | switch/case on value            |
| BR-CAN-LOANS-FG PIC X | VALID ("Y","N","O")        | bool IsValid             | value in {"Y","N","O"}          |

---

### 5. **Code Example**

```csharp
public enum CanLoansFlag { Yes, No, Only }

public class BranchLoanStatusDto
{
    public CanLoansFlag CanLoans { get; set; }
    public bool IsValid => CanLoans == CanLoansFlag.Yes
                       || CanLoans == CanLoansFlag.No
                       || CanLoans == CanLoansFlag.Only;
}
```

---

### 6. **Summary**

- **Boolean** for two-value conditions
- **Enum** for multi-value conditions
- **Validation logic** for set membership
- Use conversion helpers to map COBOL values to C# types

---

**Source Reference:**  
- Example 88-level mapping from [[FILE:RZCORP.CBL|RZCORP.CBL]] (Source: [[FILE:RZCORP.CBL|RZCORP.CBL]], Index: code)
- General COBOL 88-level usage patterns

If you need mapping for a specific copybook or field, provide its definition for tailored C# code.

### DTO Design and Validation

**Q29: What approach should we take for handling COBOL filler fields and padding in DTOs (include, exclude, or document)?**

When handling **COBOL filler fields and padding** in DTOs (Data Transfer Objects), follow this approach:

---

### 1. **Exclude FILLER Fields from DTOs**

- **COBOL FILLER fields** are used for alignment, reserved space, or padding in record layouts.
- They **do not contain business data** and are not referenced by program logic.
- **DTOs should NOT include FILLER fields** as properties, since they have no semantic meaning in the target system (e.g., C#).

---

### 2. **Document FILLER Fields in Mapping Documentation**

- **Document their presence and purpose** in your mapping specs or code comments.
- Example:  
  - "COBOL field FILLER (length 10) excluded from DTO; used for record alignment."
- This helps future maintainers understand any discrepancies in field counts or record sizes.

---

### 3. **Handle Padding/Alignment at Serialization Layer**

- If you need to **serialize/deserialize** COBOL records (e.g., for file I/O or interop), ensure your serialization logic accounts for FILLER bytes.
- **DTOs remain clean** (no FILLER properties), but your parser/writer should skip or insert FILLER bytes as needed to match COBOL record layouts.

---

### 4. **Special Cases: Reserved or Future Use Fields**

- If a FILLER field is labeled for **future use** or has a business comment (e.g., "reserved for future expansion"), document this in your mapping.
- Only include as a DTO property if business requirements dictate (rare).

---

### **Summary Table**

| COBOL Field Type | DTO Property | Mapping Documentation | Serialization Handling |
|------------------|-------------|----------------------|-----------------------|
| Business Field   | Yes         | Yes                  | Yes                   |
| FILLER           | No          | Yes                  | Yes (skip/insert)     |
| Reserved         | No (usually)| Yes                  | Yes                   |

---

**Best Practice:**  
- **Exclude FILLER fields from DTOs.**
- **Document their existence and purpose.**
- **Handle padding at the serialization layer, not in DTOs.**

This approach keeps DTOs focused on business data, avoids confusion, and ensures accurate record mapping for COBOL interoperability.

**Q30: How do we design validation attributes for DTOs based on COBOL field definitions (length, range, format)?**

To design **validation attributes for DTOs** based on COBOL field definitions (length, range, format), follow this structured approach:

---

### 1. **Extract COBOL Field Metadata**

- **Length**: Use the PIC clause (e.g., `PIC X(10)` means max length 10).
- **Range**: Look for comments, 88-level condition names, or explicit validation logic (e.g., `IF FIELD > 999`).
- **Format**: Identify numeric, alphanumeric, date, or custom formats (e.g., `PIC 9(6)` for numbers, `PIC X(8)` for text, date fields often have `YYMMDD` or `CCYYMMDD`).

---

### 2. **Map COBOL Attributes to C# Validation**

| COBOL Attribute         | C# DTO Attribute/Validation                | Example Implementation                |
|------------------------|--------------------------------------------|---------------------------------------|
| Length (PIC X(10))     | `[StringLength(10)]`                       | `public string Name { get; set; }`    |
| Numeric (PIC 9(6))     | `[Range(0, 999999)]`                       | `public int Amount { get; set; }`     |
| Date (YYMMDD/CCYYMMDD) | `[DataType(DataType.Date)]` + custom parse | `public DateOnly Date { get; set; }`  |
| Required               | `[Required]`                               | `public string Code { get; set; }`    |
| Custom Format          | `[RegularExpression("pattern")]`           | `public string Id { get; set; }`      |

---

### 3. **Handle Special COBOL Constructs**

- **88-Level Condition Names**: Map to enums or boolean properties, and add validation for allowed values.
- **FILLER Fields**: Exclude from DTOs (do not add validation).
- **Range/Domain Constraints**: If COBOL code or comments specify allowed ranges, use `[Range(min, max)]` or `[EnumDataType]`.

---

### 4. **Document Validation Logic**

- Add XML comments or documentation to DTO properties explaining the source COBOL rules.
- Example:
  ```csharp
  /// <summary>
  /// COBOL: PIC X(10), must be uppercase, required.
  /// </summary>
  [Required]
  [StringLength(10)]
  [RegularExpression(@"^[A-Z]+$")]
  public string Code { get; set; }
  ```

---

### 5. **Sample Mapping from COBOL to C# DTO**

**COBOL Field:**
```cobol
01  CUSTOMER-NAME    PIC X(30).
01  AMOUNT           PIC 9(6)V99.
01  TRANS-DATE       PIC 9(8).  * Format: CCYYMMDD
88  STATUS-VALID     VALUE 'A' 'C' 'D'.
```

**C# DTO:**
```csharp
public class TransactionDto
{
    [Required]
    [StringLength(30)]
    public string CustomerName { get; set; }

    [Range(0, 999999.99)]
    public decimal Amount { get; set; }

    [Required]
    [RegularExpression(@"^\d{8}$")] // CCYYMMDD
    public string TransDateRaw { get; set; }
    public DateOnly TransDate => ParseDate(TransDateRaw);

    [EnumDataType(typeof(Status))]
    public Status Status { get; set; }
}

public enum Status { A, C, D }
```

---

### 6. **Automate Extraction Where Possible**

- Use code generation tools or scripts to parse COBOL copybooks and generate DTOs with appropriate validation attributes.

---

### 7. **Reference to Source**

- For each DTO property, cite the COBOL source field and any relevant copybook or comment.
  - Example: "Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], Field: FD-OPTION, PIC X(1), 88-levels for valid values."

---

## Summary

- **Extract**: Length, type, range, format from COBOL PIC and 88-levels.
- **Map**: To C# `[StringLength]`, `[Range]`, `[RegularExpression]`, `[Required]`, enums.
- **Document**: Source and business rules for traceability.
- **Automate**: Use scripts/tools for large copybooks.

---

**Source references:**
- COBOL field definitions and 88-levels (e.g., [[FILE:LP01FD.CPY|LP01FD.CPY]], [[FILE:CSTEST_DEF.CPY|CSTEST_DEF.CPY]], [[FILE:DELCHK_DEF.CPY|DELCHK_DEF.CPY]])
- Comments and validation logic in copybooks and programs (Source: [[FILE:LP01FD.CPY|LP01FD.CPY]], [[FILE:CSTEST_DEF.CPY|CSTEST_DEF.CPY]], [[FILE:DELCHK_DEF.CPY|DELCHK_DEF.CPY]], [[FILE:SCREEN.CPY|SCREEN.CPY]])

If you need a specific mapping for a particular COBOL copybook or field, please provide the field definition and I can generate the exact C# DTO property and validation attributes.

**Q31: What patterns should we use to separate API DTOs from domain models to maintain clean architecture boundaries?**

To maintain **clean architecture boundaries** between API DTOs and domain models when translating COBOL systems, use these proven patterns:

---

### 1. **Explicit Separation of DTOs and Domain Models**

- **DTOs (Data Transfer Objects):**
  - Used for communication between layers (e.g., API ↔ client, API ↔ service)
  - Should only contain data needed for transfer (no business logic)
  - Structure based on COBOL field definitions, but exclude FILLER fields and internal-only fields (see previous guidance)
  - Include validation attributes derived from COBOL metadata (length, range, format)

- **Domain Models:**
  - Represent core business entities and logic
  - May have richer behaviors, invariants, and relationships
  - Should not be directly exposed via API endpoints

---

### 2. **Mapping Layer (Translator/Assembler Pattern)**

- Implement a **mapping layer** to convert between DTOs and domain models:
  - Use libraries like AutoMapper (C#) or manual mapping functions
  - Ensure all conversions are explicit and controlled
  - Example: `LoanDto → LoanDomainModel` and vice versa

---

### 3. **Validation Location**

- **DTOs:**  
  - Apply validation attributes (e.g., `[StringLength]`, `[Range]`, `[RegularExpression]`) based on COBOL field constraints
  - Validation is for incoming/outgoing data, not business rules

- **Domain Models:**  
  - Enforce business invariants and rules (e.g., "loan amount must be positive")
  - Use methods or constructors to guarantee valid state

---

### 4. **Avoid Direct Exposure of Domain Models**

- Never expose domain models directly in API responses or requests
- Always use DTOs for serialization/deserialization

---

### 5. **Document Mapping Decisions**

- Document which COBOL fields map to DTOs, which are excluded (e.g., FILLER), and which require transformation (e.g., 88-levels to enums/booleans)
- Example:  
  - `DT-TRCD` (COBOL) → `TransactionCode` (DTO, enum)
  - `DT-LPSEQNO` (COBOL) → `LoanSequenceNumber` (DTO, int)
  - `FILLER` (COBOL) → *excluded from DTO*

---

### 6. **Versioning and Extensibility**

- Keep DTOs versioned and stable for API consumers
- Domain models can evolve independently as business logic changes

---

### 7. **Testing and Validation**

- Unit test mapping logic to ensure correctness and boundary enforcement
- Validate that DTOs do not leak domain logic, and domain models are not serialized directly

---

## Example Pattern

```csharp
// DTO (API Layer)
public class LoanDto
{
    [StringLength(5)]
    public int LoanSequenceNumber { get; set; }

    [EnumDataType(typeof(TransactionCode))]
    public TransactionCode TransactionCode { get; set; }

    // ... other fields, validation attributes
}

// Domain Model (Core Layer)
public class Loan
{
    public int LoanSequenceNumber { get; }
    public TransactionType TransactionType { get; }
    // Business logic methods, invariants
}

// Mapping Layer
public static Loan MapToDomain(LoanDto dto) { /* ... */ }
public static LoanDto MapToDto(Loan domain) { /* ... */ }
```

---

## Summary Table

| Pattern                        | Purpose                                      |
|------------------------------- |----------------------------------------------|
| DTOs for API                   | Data transfer, validation, no business logic |
| Domain Models for Core Logic    | Business rules, invariants, relationships    |
| Mapping Layer                  | Explicit conversion, boundary enforcement    |
| Validation in DTOs             | Field-level, from COBOL metadata             |
| Validation in Domain Models    | Business-level, from requirements            |
| Exclude FILLER fields          | Prevent unnecessary data exposure            |
| Document mapping decisions     | Traceability, maintainability                |

---

**By following these patterns, you ensure a robust separation between API DTOs and domain models, preserving clean architecture boundaries and making COBOL-to-modern system translation maintainable and scalable.**

**Q32: How can we implement AutoMapper or similar tools to map between COBOL-derived DTOs and C# domain entities?**

To implement **AutoMapper** (or similar mapping tools) for mapping between **COBOL-derived DTOs** and **C# domain entities**, follow these best practices and patterns:

---

## 1. **Define Explicit Mapping Profiles**

- Create **AutoMapper profiles** that specify how each COBOL DTO maps to its corresponding C# domain entity.
- Use the extracted COBOL metadata (field names, lengths, types, validation rules) to inform mapping logic.

**Example:**
```csharp
public class LoanProfile : Profile
{
    public LoanProfile()
    {
        CreateMap<LoanDto, Loan>()
            .ForMember(dest => dest.AccountNumber, opt => opt.MapFrom(src => src.LoanAcctNo))
            .ForMember(dest => dest.Amount, opt => opt.MapFrom(src => src.LoanAmount))
            // Add custom mapping for date formats, padding, etc.
            .ForMember(dest => dest.StartDate, opt => opt.MapFrom(src => DateTime.ParseExact(src.LoanStartDate, "yyyyMMdd", null)));
    }
}
```

---

## 2. **Handle COBOL-Specific Data Transformations**

- **Padding/Trimming:** Remove COBOL padding (spaces, zeroes) from DTO fields before mapping.
- **FILLER Fields:** Exclude FILLER fields from DTOs and mappings.
- **Data Types:** Convert COBOL types (e.g., PIC X, PIC 9) to appropriate C# types (string, int, decimal).
- **Date Formats:** Parse COBOL date strings (often "YYYYMMDD" or "YYMMDD") to C# `DateTime`.

---

## 3. **Custom Value Converters**

- Use **AutoMapper value converters** for complex transformations (e.g., converting COBOL packed decimals, handling special flags).
- Example: If COBOL uses "Y"/"N" for booleans, map to C# `bool`.

```csharp
.ForMember(dest => dest.IsActive, opt => opt.MapFrom(src => src.ActiveFlag == "Y"))
```

---

## 4. **Validation Integration**

- Apply validation attributes (e.g., `[StringLength]`, `[Range]`, `[RegularExpression]`) to DTOs based on COBOL field definitions.
- Ensure mapped entities respect these constraints.

---

## 5. **Testing and Edge Cases**

- Write unit tests for mappings, especially for edge cases (e.g., max/min field lengths, invalid formats).
- Validate that all COBOL business rules are respected in the mapping logic.

---

## 6. **Documentation and Maintainability**

- Document mapping profiles, especially where COBOL field names differ from C# property names.
- Note any special handling (e.g., legacy fields, deprecated COBOL fields).

---

## 7. **Example Mapping Workflow**

1. **Extract COBOL DTOs:** Use code generation or manual extraction from COBOL copybooks.
2. **Define C# Domain Entities:** Model business logic and relationships.
3. **Configure AutoMapper Profiles:** Map DTO fields to entity properties, handling type conversions and business rules.
4. **Integrate in Service Layer:** Use AutoMapper in API controllers or service classes to convert DTOs to domain entities and vice versa.

---

## 8. **Handling Nested Structures**

- For COBOL OCCURS/arrays, map to C# collections (`List<T>`, arrays).
- Use `.ForMember()` and `.ConvertUsing()` for custom collection mapping.

---

## Summary Table

| COBOL Feature         | Mapping Strategy in AutoMapper/C#           |
|---------------------- |---------------------------------------------|
| FILLER fields         | Exclude from DTOs and mappings              |
| PIC X (string)        | Map to C# `string`, trim/pad as needed      |
| PIC 9 (numeric)       | Map to C# `int`, `decimal`, handle padding  |
| Dates (YYYYMMDD)      | Parse to C# `DateTime`                      |
| OCCURS (arrays)       | Map to C# `List<T>` or arrays               |
| Validation rules      | Use C# attributes, custom mapping logic     |

---

## Source References

- COBOL field definitions and DTO extraction: See previous guidance on DTO design and validation (conversation context).
- Mapping patterns: Standard AutoMapper documentation and best practices.
- COBOL-specific transformation examples: Based on typical COBOL-to-C# migration projects.

---

**In summary:**  
Use AutoMapper profiles with custom converters and validation logic to bridge COBOL DTOs and C# domain entities, ensuring all COBOL-specific quirks (padding, data types, field names) are handled explicitly and maintainably.

**Q33: What strategy should we use for API request/response DTOs versus internal data models to optimize API contracts?**

To optimize API contracts when translating COBOL systems, you should **strategically separate API request/response DTOs from internal data models** using the following approach:

---

## 1. **Explicit DTO Layer for API Contracts**

- **Purpose:** DTOs (Data Transfer Objects) define the shape of data exchanged over the API, independent of internal business logic or persistence models.
- **Strategy:**  
  - Design DTOs to match the API consumer’s needs, not the internal COBOL-derived structures.
  - Include only fields relevant to the API contract; omit internal fields, technical flags, or legacy artifacts.
  - Use clear, consumer-friendly naming and formatting (e.g., camelCase for JSON, ISO date formats).

---

## 2. **Internal Domain Models for Business Logic**

- **Purpose:** Internal models represent business entities and rules, often reflecting COBOL file layouts, validation, and relationships.
- **Strategy:**  
  - Keep domain models rich in business logic, validation, and relationships.
  - Preserve COBOL-derived constraints (length, range, format) in domain models, but expose only what’s needed in DTOs.
  - Allow domain models to evolve independently of API contracts.

---

## 3. **Mapping Layer (e.g., AutoMapper, Manual Mapping)**

- **Purpose:** Decouple DTOs from domain models, enabling flexible translation between external and internal representations.
- **Strategy:**  
  - Use mapping tools (like AutoMapper in C#) or explicit mapping functions to convert between DTOs and domain models.
  - Handle field renaming, type conversions, and validation during mapping.
  - Centralize mapping logic for maintainability and testability.

---

## 4. **Validation at the Right Layer**

- **DTO Validation:**  
  - Validate API inputs for format, required fields, and basic constraints (e.g., string length, numeric range) based on COBOL field definitions.
  - Use attributes or decorators (e.g., `[StringLength]`, `[Range]`, `[RegularExpression]` in C#) on DTOs.

- **Domain Model Validation:**  
  - Enforce deeper business rules, cross-field validation, and legacy constraints in domain models.

---

## 5. **Versioning and Contract Stability**

- **Strategy:**  
  - Version DTOs and API contracts independently from domain models.
  - Avoid leaking internal changes (e.g., COBOL file structure updates) into API contracts.
  - Use DTOs to shield consumers from internal refactoring.

---

## 6. **Example Workflow**

1. **API receives request DTO:**  
   - Validates input using DTO attributes.
2. **Maps DTO to domain model:**  
   - Converts fields, applies business logic.
3. **Processes business logic:**  
   - Uses domain model for operations.
4. **Maps domain model to response DTO:**  
   - Formats output for API consumer.

---

## 7. **Benefits**

- **Clean separation of concerns:** API contracts remain stable and consumer-focused.
- **Maintainability:** Internal changes do not break external APIs.
- **Security:** Sensitive/internal fields are not exposed.
- **Flexibility:** Easier to support multiple API versions or consumer types.

---

**Summary Table:**

| Layer         | Purpose                | Example Constraints         | Mapping Strategy         |
|---------------|------------------------|----------------------------|-------------------------|
| DTOs          | API contract           | Format, required, length   | Attributes, mapping     |
| Domain Models | Business logic         | COBOL rules, relationships | Rich validation         |
| Mapping       | Translation            | Field renaming, conversion | AutoMapper/manual       |

---

**Reference:**  
This strategy is widely used in modernizing COBOL systems to APIs, ensuring robust, maintainable, and consumer-friendly interfaces.  
(Source: [[FILE:I|I]]ndustry best practices; see also mapping and validation patterns in .NET and Java API frameworks.)

## Section 10.3: Designing CRUD Operations and Business Workflows

### RESTful CRUD Endpoint Design

**Q34: What RESTful conventions should we follow when designing CRUD operations for OTIS business entities (loans, payments, accounts)?**

To design RESTful CRUD operations for OTIS business entities (loans, payments, accounts), you should follow these conventions to ensure clarity, scalability, and alignment with modern API standards:

---

## 1. **Resource-Oriented URLs**

- Use nouns for resources, not verbs.
- Pluralize resource names for collections.

**Examples:**
- `/loans` – Collection of loan entities
- `/loans/{loanId}` – Specific loan
- `/payments` – Collection of payments
- `/accounts/{accountId}` – Specific account

---

## 2. **HTTP Methods for CRUD**

| Operation | HTTP Method | Example Endpoint         | Description                       |
|-----------|-------------|-------------------------|-----------------------------------|
| Create    | POST        | `/loans`                | Create a new loan                 |
| Read      | GET         | `/loans/{loanId}`       | Retrieve loan details             |
| Update    | PUT/PATCH   | `/loans/{loanId}`       | Update loan information           |
| Delete    | DELETE      | `/loans/{loanId}`       | Remove a loan                     |

- **POST** for creating new resources.
- **GET** for retrieving resources.
- **PUT** for full updates, **PATCH** for partial updates.
- **DELETE** for removing resources.

---

## 3. **Stateless Operations**

- Each request should contain all necessary information (no session state on server).
- Authentication (e.g., JWT, OAuth) should be included in headers.

---

## 4. **Standard HTTP Status Codes**

- `200 OK` – Successful GET, PUT, PATCH, DELETE
- `201 Created` – Successful POST
- `204 No Content` – Successful DELETE (no response body)
- `400 Bad Request` – Invalid input
- `404 Not Found` – Resource not found
- `409 Conflict` – Duplicate or conflicting resource

---

## 5. **Consistent Data Formats**

- Use JSON for request and response bodies.
- Define clear DTOs for each entity (e.g., LoanDTO, PaymentDTO, AccountDTO).
- Separate DTOs from internal domain models for clean architecture (see previous answers).

---

## 6. **Filtering, Sorting, and Pagination**

- Support query parameters for collections:
  - `/loans?status=open&sort=dueDate&page=2&pageSize=50`
- Use standard conventions for pagination (`page`, `pageSize`), filtering, and sorting.

---

## 7. **Hypermedia Links (HATEOAS, Optional)**

- Include links in responses to related resources:
  - `"links": [{ "rel": "payments", "href": "/loans/{loanId}/payments" }]`

---

## 8. **Versioning**

- Use URL versioning (`/v1/loans`) or header-based versioning for backward compatibility.

---

## 9. **Error Handling**

- Return structured error responses:
  ```json
  {
    "error": {
      "code": "LOAN_NOT_FOUND",
      "message": "Loan with ID 12345 does not exist."
    }
  }
  ```

---

## 10. **Business-Specific Actions**

- For non-CRUD actions (e.g., "approve loan", "process payment"), use sub-resources or action endpoints:
  - `/loans/{loanId}/approve` (POST)
  - `/payments/{paymentId}/process` (POST)

---

### Example: Loan CRUD Endpoints

- **Create:** `POST /loans`
- **Read:** `GET /loans/{loanId}`
- **Update:** `PUT /loans/{loanId}`
- **Delete:** `DELETE /loans/{loanId}`

---

### Example: Payment CRUD Endpoints

- **Create:** `POST /payments`
- **Read:** `GET /payments/{paymentId}`
- **Update:** `PATCH /payments/{paymentId}`
- **Delete:** `DELETE /payments/{paymentId}`

---

## Summary Table

| Entity   | Create         | Read                | Update              | Delete              |
|----------|----------------|---------------------|---------------------|---------------------|
| Loan     | POST /loans    | GET /loans/{id}     | PUT/PATCH /loans/{id}| DELETE /loans/{id}  |
| Payment  | POST /payments | GET /payments/{id}  | PUT/PATCH /payments/{id}| DELETE /payments/{id}|
| Account  | POST /accounts | GET /accounts/{id}  | PUT/PATCH /accounts/{id}| DELETE /accounts/{id}|

---

**Following these RESTful conventions will ensure your OTIS API is maintainable, predictable, and easy for consumers to integrate.**

**Q35: How do we design POST endpoints for creating new records based on COBOL file WRITE operations?**

To design **POST endpoints** for creating new records that correspond to **COBOL file WRITE operations**, follow these RESTful and legacy-mapping best practices:

---

## 1. **RESTful POST Endpoint Design**

- **Resource-Oriented URLs:**  
  Use plural nouns for resources, e.g.:
  - `/loans`
  - `/payments`
  - `/accounts`

- **HTTP Method:**  
  Use `POST` for creation.

- **Request Body:**  
  Accept a JSON payload that maps to the fields required for a COBOL WRITE operation (i.e., the record structure in the COBOL FD section).

- **Response:**  
  Return `201 Created` with the newly created resource (including its unique ID or key), or appropriate error codes.

---

## 2. **Mapping COBOL WRITE to API POST**

- **COBOL WRITE Operation:**  
  In COBOL, a new record is created using the `WRITE` statement, e.g.:
  ```cobol
  WRITE LOAN-REC
  ```
  The record structure (`LOAN-REC`) is defined in the FD section and/or copybooks.

- **API POST Operation:**  
  The POST endpoint should accept a DTO (Data Transfer Object) that matches the COBOL record structure.  
  Example:
  ```json
  POST /loans
  {
    "loanNumber": "123456",
    "borrowerId": "98765",
    "amount": 10000,
    "startDate": "2024-06-01",
    ...
  }
  ```

- **Validation:**  
  Validate incoming data against business rules (as enforced in COBOL before WRITE).  
  Example from COBOL:
  ```cobol
  IF LOAN-AMOUNT > 0 AND BORROWER-ID NOT = SPACES
     WRITE LOAN-REC
  ELSE
     DISPLAY "ERROR"
  END-IF
  ```
  In API, return `400 Bad Request` for invalid data.

---

## 3. **Error Handling**

- **COBOL File Status:**  
  COBOL uses `FILE STATUS` to detect errors on WRITE (e.g., duplicate key, file full).
  - Map these to API error responses:
    - Duplicate key → `409 Conflict`
    - File full/system error → `500 Internal Server Error`

---

## 4. **Idempotency and Uniqueness**

- **COBOL Indexed/Relative Files:**  
  If the COBOL file is indexed or relative, ensure the API enforces uniqueness (e.g., loan number must be unique).
  - Use appropriate constraints in the backend.

---

## 5. **Example: Loans POST Endpoint**

**COBOL FD Example (from context):**
```cobol
FD  LOAN-FILE
    LABEL RECORDS ARE STANDARD.
01  LOAN-REC.
    03  LOAN-NUMBER     PIC 9(6).
    03  BORROWER-ID     PIC 9(5).
    03  LOAN-AMOUNT     PIC 9(7)V99.
    03  START-DATE      PIC 9(8).
    ...
```
**API POST Example:**
```http
POST /loans
Content-Type: application/json

{
  "loanNumber": "123456",
  "borrowerId": "98765",
  "loanAmount": 10000.00,
  "startDate": "20240601"
}
```
**Response:**
```http
201 Created
Location: /loans/123456

{
  "loanNumber": "123456",
  "borrowerId": "98765",
  "loanAmount": 10000.00,
  "startDate": "20240601"
}
```

---

## 6. **Business Rule Enforcement**

- Mirror COBOL pre-WRITE validation in your API logic.
- Example: If COBOL checks for valid branch number, enforce this in the API.

---

## 7. **Summary Table: Mapping COBOL WRITE to REST POST**

| COBOL Concept         | RESTful API Equivalent           |
|-----------------------|----------------------------------|
| WRITE record          | POST endpoint                    |
| FD record structure   | Request DTO schema               |
| FILE STATUS           | HTTP status code                 |
| Key uniqueness        | Backend constraint, 409 Conflict |
| Validation logic      | Input validation, 400 Bad Request|

---

## 8. **Reference to COBOL Source**

- COBOL WRITE operations are defined in FD sections and executed with the WRITE statement (Source: [[FILE:TR1534.CBL|TR1534.CBL]], Index: code; [[FILE:BRFILE.CBL|BRFILE.CBL]], Index: code).
- Example FD:
  ```cobol
  FD  OBR-FILE
      LABEL RECORDS ARE STANDARD.
  01  OBR-REC.
      03  OBR-KEY      PIC 9(6).
      03  OBR-NAME     PIC X(30).
      ...
  ```
  (Source: [[FILE:BRFILE.CBL|BRFILE.CBL]], Index: code)

---

## 9. **Visual Representation**

Here's a visual representation of the mapping between API POST and COBOL WRITE:

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">API POST to COBOL WRITE Mapping</text>
  
  <!-- API Layer -->
  <rect x="50" y="70" width="250" height="100" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="175" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">API Layer</text>
  <text x="175" y="120" text-anchor="middle" font-size="14" fill="#4338ca">POST /loans</text>
  <text x="175" y="140" text-anchor="middle" font-size="12" fill="#6366f1">JSON DTO</text>
  
  <!-- Validation Layer -->
  <rect x="325" y="70" width="250" height="100" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="450" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">Validation Layer</text>
  <text x="450" y="120" text-anchor="middle" font-size="14" fill="#1e40af">Business Rules</text>
  <text x="450" y="140" text-anchor="middle" font-size="12" fill="#3b82f6">Field Checks</text>
  
  <!-- COBOL Layer -->
  <rect x="600" y="70" width="250" height="100" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="725" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">COBOL Layer</text>
  <text x="725" y="120" text-anchor="middle" font-size="14" fill="#15803d">WRITE OBR-REC</text>
  <text x="725" y="140" text-anchor="middle" font-size="12" fill="#22c55e">FD Record</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="300" y1="120" x2="325" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="575" y1="120" x2="600" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**Summary:**  
Design your POST endpoints to accept DTOs matching COBOL record structures, enforce business rules as in COBOL before WRITE, and map COBOL file status errors to appropriate HTTP status codes. This ensures a robust, maintainable API that faithfully represents legacy COBOL data creation logic.

If you need specific COBOL FD layouts for your DTOs, provide the relevant copybook or FD section for detailed mapping.

**Q36: What strategy should we use for designing PUT versus PATCH endpoints when converting COBOL REWRITE operations?**

When converting COBOL **REWRITE operations** to RESTful API endpoints, you should use the following strategy to distinguish between **PUT** and **PATCH**:

---

## 1. **Understand COBOL REWRITE Semantics**

- In COBOL, **REWRITE** replaces the entire record at the current file position, typically using the primary key for lookup.
- Comments in the code emphasize that REWRITE must use the primary key, not a secondary key, to avoid updating the wrong record (see below).

  > * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
  > *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
  > *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
  > *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
  > *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
  > (Source: [[FILE:METRO2.CBL|METRO2.CBL]], [[FILE:LPCI2IN.CPY|LPCI2IN.CPY]], [[FILE:GLGT2IN.CPY|GLGT2IN.CPY]], [[FILE:IODTIN.CBL|IODTIN.CBL]], [[FILE:WDAYBK.CBL|WDAYBK.CBL]], etc.)

---

## 2. **RESTful Mapping Strategy**

### **PUT Endpoint**
- **Use for full record replacement.**
- The client must send the **entire resource representation** (all fields).
- The endpoint should require the **primary key** in the URL (e.g., `/loans/{loanId}`).
- The server replaces the existing record with the new data, just like COBOL REWRITE.

  **Example:**
  ```
  PUT /loans/{loanId}
  Body: {
    "loanId": "12345",
    "amount": 1000,
    "term": 12,
    "status": "active"
    // ... all fields required
  }
  ```

### **PATCH Endpoint**
- **Use for partial updates.**
- The client sends only the fields to be changed.
- The endpoint should still require the **primary key** in the URL.
- The server updates only the specified fields, leaving others unchanged.

  **Example:**
  ```
  PATCH /loans/{loanId}
  Body: {
    "status": "closed"
  }
  ```

---

## 3. **Key Design Principles**

- **Primary Key Enforcement:**  
  Always require the primary key in the URL/path for both PUT and PATCH, mirroring COBOL's need for precise record targeting.

- **PUT = Full Replace:**  
  PUT should fail if the payload is missing required fields, as COBOL REWRITE expects a complete record.

- **PATCH = Partial Update:**  
  PATCH should validate only the fields provided, updating them in place.

- **Avoid Secondary Key Updates:**  
  Do not allow updates via secondary keys, as this risks modifying the wrong record (per COBOL comments).

---

## 4. **Error Handling**

- If the primary key does not match any record, return a 404 Not Found.
- If a PUT payload is incomplete, return a 400 Bad Request.
- If a PATCH tries to update immutable fields (e.g., primary key), return a 400 Bad Request.

---

## 5. **Business Rule Alignment**

- Ensure that business rules enforced in COBOL (e.g., only updating via primary key) are mirrored in API validation logic.
- Document these constraints in your API specification.

---

## 6. **Summary Table**

| COBOL Operation | REST Endpoint | URL Pattern         | Payload Required      | Update Type         |
|-----------------|--------------|---------------------|----------------------|---------------------|
| REWRITE         | PUT          | /entity/{id}        | Full resource        | Full replace        |
| REWRITE         | PATCH        | /entity/{id}        | Partial fields       | Partial update      |

---

## Source Citations

- "NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE THE PRIMARY KEY..."  
  (Source: [[FILE:METRO2.CBL|METRO2.CBL]], [[FILE:LPCI2IN.CPY|LPCI2IN.CPY]], [[FILE:GLGT2IN.CPY|GLGT2IN.CPY]], [[FILE:IODTIN.CBL|IODTIN.CBL]], [[FILE:WDAYBK.CBL|WDAYBK.CBL]], etc.)

---

## Visual Representation:

Here's a visual representation:

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL REWRITE → RESTful PUT vs PATCH</text>
  
  <!-- COBOL REWRITE box -->
  <rect x="50" y="80" width="200" height="80" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" fill="white" font-size="18" font-weight="bold">COBOL REWRITE</text>
  <text x="150" y="135" text-anchor="middle" fill="white" font-size="12">Full record replace</text>
  
  <!-- Arrow to PUT -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="250" y1="120" x2="400" y2="120" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- PUT box -->
  <rect x="400" y="80" width="200" height="80" fill="#4ade80" stroke="#16a34a" stroke-width="3" rx="8"/>
  <text x="500" y="110" text-anchor="middle" fill="white" font-size="18" font-weight="bold">PUT /entity/{id}</text>
  <text x="500" y="135" text-anchor="middle" fill="white" font-size="12">Full resource required</text>
  
  <!-- Arrow to PATCH -->
  <line x1="250" y1="120" x2="400" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  
  <!-- PATCH box -->
  <rect x="400" y="180" width="200" height="80" fill="#f59e0b" stroke="#d97706" stroke-width="3" rx="8"/>
  <text x="500" y="210" text-anchor="middle" fill="white" font-size="18" font-weight="bold">PATCH /entity/{id}</text>
  <text x="500" y="235" text-anchor="middle" fill="white" font-size="12">Partial fields allowed</text>
  
  <!-- Primary Key note -->
  <rect x="650" y="80" width="200" height="80" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="750" y="110" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Primary Key Required</text>
  <text x="750" y="135" text-anchor="middle" font-size="12" fill="#22c55e">No secondary key updates</text>
  
  <!-- Arrows from PUT and PATCH to Primary Key box -->
  <line x1="600" y1="120" x2="650" y2="120" stroke="#15803d" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="600" y1="220" x2="650" y2="120" stroke="#15803d" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

**In summary:**  
- Use **PUT** for full record replacement (mirrors COBOL REWRITE).
- Use **PATCH** for partial updates.
- Always require the **primary key** in the endpoint path.
- Never allow updates via secondary keys, matching COBOL's strict record targeting.  
(Source: [[FILE:METRO2.CBL|METRO2.CBL]], [[FILE:LPCI2IN.CPY|LPCI2IN.CPY]], [[FILE:GLGT2IN.CPY|GLGT2IN.CPY]], [[FILE:IODTIN.CBL|IODTIN.CBL]], [[FILE:WDAYBK.CBL|WDAYBK.CBL]])

**Q37: How can we design GET endpoints that support filtering, sorting, and pagination for COBOL sequential file reads?**

To design **GET endpoints** that support **filtering, sorting, and pagination** for COBOL sequential file reads, you should map modern RESTful API conventions to the constraints and capabilities of COBOL file processing. Here’s a recommended strategy:

---

## 1. **Filtering**

- **RESTful Convention:** Use query parameters to specify filter criteria.
  - Example: `GET /loans?status=active&branch=123`
- **COBOL Mapping:** In COBOL, filtering is typically done by reading each record sequentially and applying IF/EVALUATE logic to match criteria.
  - Implement logic in your API layer to translate query parameters into COBOL filter conditions.
  - Example COBOL pattern:
    ```cobol
    READ LOAN-FILE
    AT END
      MOVE 'NO MORE RECORDS' TO WS-MSG
    NOT AT END
      IF LOAN-STATUS = 'ACTIVE' AND BRANCH-NO = '123'
        ADD TO RESPONSE-LIST
      END-IF
    END-READ
    ```

---

## 2. **Sorting**

- **RESTful Convention:** Use query parameters like `sort` and `order`.
  - Example: `GET /loans?sort=amount&order=desc`
- **COBOL Mapping:** COBOL sequential files do not support native sorting during read. You have two options:
  - **Option A:** Sort in-memory after reading and filtering records.
  - **Option B:** Use a COBOL SORT step (if large data sets).
    - Example: Use `SORT` verb or external sort utility, then read sorted file.
  - Example API logic:
    - Parse `sort` and `order` parameters.
    - After collecting filtered records, sort the result set before returning.

---

## 3. **Pagination**

- **RESTful Convention:** Use `limit` and `offset` (or `page` and `pageSize`) query parameters.
  - Example: `GET /loans?limit=20&offset=40`
- **COBOL Mapping:** Sequential files require you to read and count records to implement pagination.
  - Read and filter records, count until reaching `offset`, then collect up to `limit` records.
  - Example COBOL pattern:
    ```cobol
    MOVE 0 TO WS-COUNT
    PERFORM UNTIL WS-COUNT = OFFSET + LIMIT OR END-OF-FILE
      READ LOAN-FILE
      IF MATCHES-FILTERS
        ADD 1 TO WS-COUNT
        IF WS-COUNT > OFFSET
          ADD TO RESPONSE-LIST
        END-IF
      END-IF
    END-PERFORM
    ```

---

## 4. **Endpoint Design Example**

```
GET /loans?status=active&branch=123&sort=amount&order=desc&limit=20&offset=40
```
- **Filters:** `status=active`, `branch=123`
- **Sorting:** `sort=amount`, `order=desc`
- **Pagination:** `limit=20`, `offset=40`

---

## 5. **Implementation Tips**

- **Efficiency:** For large files, consider batch processing or indexed files if available.
- **Response Structure:** Return results as an array, with metadata for total count, current page, etc.
  - Example JSON:
    ```json
    {
      "total": 1200,
      "limit": 20,
      "offset": 40,
      "records": [ ... ]
    }
    ```
- **Error Handling:** Return clear errors for invalid filter/sort fields or out-of-range pagination.

---

## 6. **COBOL Code Example for GET with Filtering, Sorting, Pagination**

```cobol
MOVE 0 TO WS-COUNT
PERFORM UNTIL END-OF-FILE OR WS-COUNT = OFFSET + LIMIT
  READ LOAN-FILE
  AT END
    SET END-OF-FILE TO TRUE
  NOT AT END
    IF LOAN-STATUS = 'ACTIVE' AND BRANCH-NO = '123'
      ADD 1 TO WS-COUNT
      IF WS-COUNT > OFFSET
        ADD TO RESPONSE-LIST
      END-IF
    END-IF
END-PERFORM
* After collecting, sort RESPONSE-LIST by AMOUNT DESCENDING
```
(Source: [[FILE:G|G]]eneral COBOL file read patterns, inferred from context)

---

## 7. **Summary Table**

| RESTful Feature | COBOL Mapping Strategy                  | API Example Parameter      |
|-----------------|----------------------------------------|---------------------------|
| Filtering       | IF/EVALUATE during sequential read      | `status=active`           |
| Sorting         | In-memory sort after read, or SORT step | `sort=amount&order=desc`  |
| Pagination      | Count records, skip to offset, limit    | `limit=20&offset=40`      |

---

**In summary:**  
Design GET endpoints with query parameters for filtering, sorting, and pagination. Map these to COBOL sequential file reads by applying filter logic during read, sorting results in-memory or via SORT, and counting records for pagination. This approach provides a modern API experience while respecting COBOL file processing constraints.

**Q38: What approach should we take for designing DELETE endpoints that handle COBOL file DELETE operations and referential integrity?**

To design **DELETE endpoints** that map to COBOL file DELETE operations and properly handle **referential integrity**, follow these best practices:

---

## 1. **Map COBOL DELETE to RESTful DELETE**

- COBOL DELETE operations typically remove a record from a file or database table.
- In REST, use the HTTP DELETE method:  
  `DELETE /resource/{id}`  
  This should remove the record identified by `{id}`.

---

## 2. **Identify Referential Integrity Constraints**

- COBOL files may have relationships (e.g., parent-child, foreign keys) enforced either in code or in the underlying database.
- Before deleting, determine if the record is referenced elsewhere (e.g., child records, related tables).
- In COBOL, this may be handled by:
  - Checking for dependent records before DELETE
  - Preventing deletion if dependencies exist
  - Cascading deletes (less common in legacy COBOL)

**Example from context:**  
COBOL programs often perform a sequence like:
```cobol
PERFORM DELETE-IT.
EXEC SQL
  DELETE FROM DBO.CDBFILE
  WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
    AND CDBFILE.CDB_BRANCH = :QCD
```
(Source: [[FILE:LPCDB1IN.CPY|LPCDB1IN.CPY]], Index: code)

---

## 3. **Design Endpoint Logic for Integrity**

- **Pre-Delete Check:**  
  Before deleting, query for dependent records.  
  - If dependencies exist, return an error (e.g., HTTP 409 Conflict).
  - Optionally, support a "cascade" flag to delete children (if business rules allow).

- **Transactionality:**  
  Ensure the delete and any related operations are atomic (all succeed or all fail).

- **Error Handling:**  
  - If deletion fails due to integrity constraints, return a clear error message.
  - Log the failure for audit purposes.

---

## 4. **Sample Endpoint Design**

**Endpoint:**  
`DELETE /customers/{customerId}`

**Logic:**
1. Check for related loans, payments, etc. for `customerId`.
2. If related records exist:
   - Return HTTP 409 Conflict with message: "Cannot delete customer with active loans."
3. If no dependencies:
   - Proceed with deletion.
   - Return HTTP 204 No Content on success.

---

## 5. **COBOL-to-API Mapping Considerations**

- COBOL may not enforce referential integrity at the file level; you may need to replicate these checks in your API/service layer.
- For SQL-backed COBOL (as seen in context), leverage database constraints where possible.
- For flat files, implement integrity checks in code before performing the DELETE.

---

## 6. **Audit and Logging**

- COBOL often logs deletes for audit.  
  Ensure your API also logs delete actions, including user, timestamp, and affected records.

---

## 7. **Example Flow**

**COBOL DELETE Operation:**
```cobol
PERFORM DELETE-IT.
EXEC SQL
  DELETE FROM DBO.DSFILE
  WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
```
(Source: [[FILE:LPDS1IN.CPY|LPDS1IN.CPY]], Index: code)

**API DELETE Endpoint:**
- Receives DELETE request.
- Checks for dependent records in related tables.
- If safe, deletes record.
- Returns appropriate HTTP status.

---

## 8. **Visual Representation**

Here's a visual representation of the DELETE endpoint flow with referential integrity checks:

<svg width="700" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">DELETE Endpoint Flow</text>
  
  <!-- Step 1: Receive Request -->
  <rect x="50" y="70" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="140" y="100" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Receive DELETE Request</text>
  <text x="140" y="120" text-anchor="middle" fill="white" font-size="11">/resource/{id}</text>
  
  <!-- Step 2: Check Dependencies -->
  <rect x="270" y="70" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="360" y="100" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Check Dependencies</text>
  <text x="360" y="120" text-anchor="middle" fill="white" font-size="11">Referential Integrity</text>
  
  <!-- Step 3a: Conflict -->
  <rect x="490" y="40" width="160" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="570" y="70" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Return 409 Conflict</text>
  <text x="570" y="90" text-anchor="middle" fill="white" font-size="11">Dependencies Exist</text>
  
  <!-- Step 3b: Proceed Delete -->
  <rect x="490" y="120" width="160" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="570" y="150" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Delete Record</text>
  <text x="570" y="170" text-anchor="middle" fill="white" font-size="11">COBOL DELETE/SQL</text>
  
  <!-- Step 4: Return Success -->
  <rect x="270" y="220" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="360" y="250" text-anchor="middle" fill="white" font-size="14" font-weight="bold">Return 204 Success</text>
  <text x="360" y="270" text-anchor="middle" fill="white" font-size="11">No Content</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="100" x2="270" y2="100" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="100" x2="490" y2="70" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="100" x2="490" y2="150" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="570" y1="180" x2="360" y2="220" stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
</svg>

---

## Summary

- Use RESTful DELETE endpoints to map COBOL DELETE operations.
- Always check for referential integrity before deleting.
- Return appropriate HTTP status codes (409 Conflict, 204 Success).
- Ensure transactional safety and audit logging.
- Replicate COBOL integrity logic in your API/service layer if not enforced by the database.

**Sources:**  
- [[FILE:LPCDB1IN.CPY|LPCDB1IN.CPY]], [[FILE:LPDS1IN.CPY|LPDS1IN.CPY]], [[FILE:IOBYIN.CBL|IOBYIN.CBL]], [[FILE:IORBIN.CBL|IORBIN.CBL]], [[FILE:LPBY1IN.CPY|LPBY1IN.CPY]], [[FILE:RZRERN.CBL|RZRERN.CBL]], [[FILE:SPCQ1IN.CPY|SPCQ1IN.CPY]], [[FILE:LPES1IN.CPY|LPES1IN.CPY]], [[FILE:IOGIIN.CBL|IOGIIN.CBL]], [[FILE:LPCL1IN.CPY|LPCL1IN.CPY]] (Index: code)  
- General COBOL DELETE operation patterns and SQL mapping

### Business Workflow and Process Design

**Q39: How do we handle complex business workflows (multi-step COBOL processes) in API design using orchestration or workflow patterns?**

To handle **complex business workflows**—such as multi-step COBOL processes—in API design, you should use **orchestration or workflow patterns** that mirror the original COBOL logic while leveraging modern best practices for reliability, maintainability, and scalability.

---

## 1. **Analyze the COBOL Workflow Structure**

- **COBOL workflows** often involve multiple sequential steps (e.g., validation, calculation, file updates, notifications).
- Each step may call different programs, update files, and branch based on business rules.
- Example: End-of-day processing might involve validating transactions, posting to ledgers, generating reports, and sending notifications.

---

## 2. **Choose an Orchestration Strategy**

### **A. Centralized Orchestration (API Gateway or Orchestrator Service)**
- Implement a **central orchestrator** (microservice or API gateway) that coordinates all workflow steps.
- Each step is exposed as an individual API endpoint (microservice).
- The orchestrator calls each endpoint in the required sequence, handling branching logic and error management.

**Benefits:**
- Centralized control and visibility.
- Easier to manage complex branching and error handling.
- Can log and audit the entire workflow.

### **B. Decentralized Choreography**
- Each API endpoint (microservice) knows which step comes next and triggers it (via events or direct calls).
- Useful for loosely coupled, event-driven architectures.
- Less centralized control, but can be more scalable.

**Benefits:**
- Services are independent and reusable.
- Good for workflows that change frequently or have parallel steps.

---

## 3. **Model Each COBOL Step as an API Endpoint**

- **Map each COBOL paragraph or program** to a RESTful endpoint or microservice.
- Example:  
  - Step 1: `/validate-transactions`
  - Step 2: `/post-ledger`
  - Step 3: `/generate-report`
  - Step 4: `/send-notification`

---

## 4. **Implement Workflow Logic**

- Use a **workflow engine** (e.g., Camunda, Temporal, AWS Step Functions) or custom orchestrator to define the sequence, branching, and error handling.
- The workflow engine can:
  - Call each API in order.
  - Handle conditional logic (e.g., IF/ELSE from COBOL).
  - Manage retries, compensation, and rollback if needed.

---

## 5. **Handle Data Passing and State**

- Pass data between steps using workflow context (e.g., JSON payloads).
- Persist state as needed (e.g., in a database or workflow engine).
- Ensure atomicity and consistency, especially for financial operations.

---

## 6. **Error Handling and Compensation**

- Implement robust error handling at each step.
- For critical failures, trigger compensation logic (e.g., rollback, notification).
- Log errors and workflow progress for auditability.

---

## 7. **Example Workflow Mapping**

**COBOL Workflow:**
1. Validate transactions
2. Post to ledger
3. Generate report
4. Send notification

**API Orchestration:**
- Orchestrator calls:
  1. `POST /validate-transactions`
  2. `POST /post-ledger`
  3. `POST /generate-report`
  4. `POST /send-notification`
- Handles branching (e.g., skip report if no transactions), error handling, and state management.

---

## 8. **Diagram: Workflow Orchestration Pattern**

Here's a visual representation:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">API Workflow Orchestration for COBOL Multi-Step Process</text>
  
  <!-- Orchestrator -->
  <rect x="50" y="80" width="200" height="60" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="150" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Orchestrator</text>
  <text x="150" y="130" text-anchor="middle" fill="white" font-size="12">Workflow Engine</text>
  
  <!-- Step 1 -->
  <rect x="300" y="60" width="160" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="380" y="90" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Validate Transactions</text>
  <text x="380" y="110" text-anchor="middle" fill="white" font-size="11">/validate-transactions</text>
  
  <!-- Step 2 -->
  <rect x="500" y="60" width="160" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="580" y="90" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Post to Ledger</text>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="11">/post-ledger</text>
  
  <!-- Step 3 -->
  <rect x="300" y="180" width="160" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="380" y="210" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Generate Report</text>
  <text x="380" y="230" text-anchor="middle" fill="white" font-size="11">/generate-report</text>
  
  <!-- Step 4 -->
  <rect x="500" y="180" width="160" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="580" y="210" text-anchor="middle" fill="white" font-size="15" font-weight="bold">Send Notification</text>
  <text x="580" y="230" text-anchor="middle" fill="white" font-size="11">/send-notification</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="250" y1="110" x2="300" y2="90" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="460" y1="90" x2="500" y2="90" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="210" y1="140" x2="300" y2="210" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="460" y1="210" x2="500" y2="210" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

## 9. **Summary of Best Practices**

- **Centralize orchestration** for complex, multi-step workflows.
- **Model each COBOL step as a separate API endpoint** for modularity.
- **Use a workflow engine** to manage sequence, branching, and error handling.
- **Pass data and state** between steps using workflow context.
- **Implement robust error handling and compensation logic.**
- **Log and audit** workflow execution for traceability.

---

**This approach ensures that complex COBOL business processes are reliably and maintainably translated into modern API-driven workflows, supporting scalability and future enhancements.**

**Q40: What patterns should we use to represent COBOL validation logic (field edits, business rules) in API request validation?**

To represent **COBOL validation logic**—including field edits and business rules—in API request validation, you should use patterns that translate legacy COBOL constructs into modern, maintainable API validation mechanisms. Here’s how to approach this:

---

## 1. **Explicit Field Constraints → Declarative Validation**

- **COBOL Pattern:**  
  COBOL uses `PIC` clauses, `88-level` condition names, and hard-coded IF statements to enforce field constraints (e.g., valid codes, ranges, required fields).
  - Example from [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]]:
    ```cobol
    88 SPA-DISFRMLA-VALID VALUE "A" "B" "C".
    88 SPA-CONTRFRMLA-VALID VALUE " " "A".
    ```
  - Example from [[FILE:LP01SP.CPY|LP01SP.CPY]]:
    ```cobol
    88 SP-PARVPAID-RB-CD-VALID VALUE " " "A" "Z".
    ```

- **API Pattern:**  
  Use declarative validation frameworks (e.g., JSON Schema, Joi, Marshmallow) to define allowed values, types, and required fields.
  - Example (JSON Schema):
    ```json
    {
      "type": "object",
      "properties": {
        "formulaType": { "enum": ["A", "B", "C"] },
        "contractFormula": { "enum": [" ", "A"] }
      },
      "required": ["formulaType"]
    }
    ```

---

## 2. **COBOL IF/EVALUATE Logic → Composable Validation Functions**

- **COBOL Pattern:**  
  Complex business rules are implemented with nested `IF` or `EVALUATE` statements, sometimes referencing multiple fields.
  - Example (inferred from context):
    ```cobol
    IF WS-AMOUNT > 9999999
      MOVE 'AMOUNT TOO LARGE' TO ERROR-MSG
    IF WS-AMOUNT < 0
      MOVE 'INVALID AMOUNT' TO ERROR-MSG
    ```

- **API Pattern:**  
  Use composable validation functions or middleware to enforce multi-field and conditional rules.
  - Example (Express.js middleware):
    ```javascript
    if (req.body.amount > 9999999) {
      return res.status(400).json({ error: "Amount too large" });
    }
    if (req.body.amount < 0) {
      return res.status(400).json({ error: "Invalid amount" });
    }
    ```

---

## 3. **COBOL OCCURS/MAX Constraints → Array Length Validation**

- **COBOL Pattern:**  
  Limits on array size or number of modifiable fields are enforced via `OCCURS N TIMES` or `MAX = N`.
  - Example from [[FILE:CHDEBT.CBL|CHDEBT.CBL]]:
    ```cobol
    01 MAX PIC 99 VALUE 2.
    01 SPEC-TAB REDEFINES SPEC-TABLE.
       03 SPEC-REC OCCURS 2 TIMES.
    ```

- **API Pattern:**  
  Use array length constraints in schema definitions.
  - Example (JSON Schema):
    ```json
    {
      "type": "array",
      "maxItems": 2,
      "items": { "type": "object", ... }
    }
    ```

---

## 4. **COBOL Lookup/Reference Validation → Foreign Key/Enum Validation**

- **COBOL Pattern:**  
  Validity checks against reference tables (e.g., codes must exist in SRROUTE file).
  - Example (inferred):
    ```cobol
    IF ROUTING-CODE NOT IN SRROUTE-FILE
      MOVE 'INVALID CODE' TO ERROR-MSG
    ```

- **API Pattern:**  
  Validate against allowed values or check existence in related resources.
  - Example (API):
    - Enum validation for codes
    - Database lookup for foreign keys

---

## 5. **COBOL Error Handling → Structured API Error Responses**

- **COBOL Pattern:**  
  Errors are handled by moving messages to an error field and displaying them.
  - Example:
    ```cobol
    MOVE 'INVALID CUSTOMER TYPE' TO ERROR-MSG
    ```

- **API Pattern:**  
  Return structured error responses with clear messages and codes.
  - Example:
    ```json
    { "error": "Invalid customer type", "field": "customerType" }
    ```

---

## Summary Table

| COBOL Pattern                   | API Validation Pattern                | Example                        |
|----------------------------------|--------------------------------------|--------------------------------|
| PIC/88-level constraints         | Declarative schema (enum/type)       | JSON Schema enum               |
| IF/EVALUATE business rules       | Composable validation functions      | Express.js middleware          |
| OCCURS/MAX array limits          | Array length validation              | maxItems in JSON Schema        |
| Reference file/code lookup       | Enum/foreign key validation          | DB lookup, enum                |
| MOVE to ERROR-MSG                | Structured error response            | JSON error object              |

---

## Source Citations

- Field constraints and valid codes:  
  "88 SPA-DISFRMLA-VALID VALUE 'A' 'B' 'C'."  
  (Source: [[FILE:LP01SPA_AREA.CPY|LP01SPA_AREA.CPY]], Index: code)

- Array limits:  
  "01 MAX PIC 99 VALUE 2. 03 SPEC-REC OCCURS 2 TIMES."  
  (Source: [[FILE:CHDEBT.CBL|CHDEBT.CBL]], Index: code)

- Error handling:  
  "MOVE 'INVALID CUSTOMER TYPE' TO ERROR-MSG"  
  (Source: [[FILE:APIPAY.CBL|APIPAY.CBL]], Index: code)

---

**Best Practice:**  
Document all business rules and validation logic in a central schema or validation layer, mirroring the explicitness of COBOL but using modern, maintainable patterns. This ensures legacy rules are preserved and easily updated as business needs evolve.

**Q41: How can we design API endpoints that support both synchronous operations (immediate response) and asynchronous processing (COBOL batch equivalents)?**

To design API endpoints that support **both synchronous operations** (immediate response) and **asynchronous processing** (COBOL batch equivalents), you should implement a dual-mode pattern that reflects the way COBOL systems handle real-time vs. batch workflows.

---

### 1. **Dual-Mode Endpoint Design**

- **Synchronous Mode**:  
  - For operations that can be completed quickly (e.g., record lookup, simple updates), the API should process the request immediately and return the result in the response.
  - Example:  
    - `POST /loan/payments` → Validates and posts a payment, returning success/failure instantly if possible.
  - **COBOL Equivalent**: Interactive screen transaction (e.g., APIPAY processing a payment and updating files directly).

- **Asynchronous Mode**:  
  - For operations that require batch processing (e.g., large file updates, complex calculations, or jobs that run overnight), the API should accept the request, enqueue it for later processing, and return a job/status ID.
  - Example:  
    - `POST /batch/loan-reconciliation` → Submits a batch job, returns `{ jobId: 12345 }`, and allows the client to poll for status.
  - **COBOL Equivalent**: Batch programs (e.g., BATCHP, LONPT0) that process files in scheduled runs.

---

### 2. **Implementation Patterns**

- **Endpoint Structure**:
  - Use a single endpoint with a mode parameter, or separate endpoints for sync/async.
    - `POST /process?mode=sync` or `POST /process?mode=async`
    - Or: `POST /process` (sync), `POST /process/batch` (async)

- **Request Handling**:
  - **Synchronous**: Validate, process, and respond immediately.
  - **Asynchronous**:  
    - Validate and enqueue the request.
    - Return a job ID.
    - Provide endpoints for job status (`GET /jobs/{jobId}`) and results retrieval.

- **Status Tracking**:
  - Store job metadata (status, progress, errors) in a database or queue.
  - Allow clients to poll or subscribe to job completion events.

- **Error Handling**:
  - For async jobs, provide detailed error/status reporting (e.g., failed, completed, in-progress).

---

### 3. **Mapping to COBOL Workflows**

- **Synchronous API → COBOL Transaction Programs**  
  - Directly call COBOL modules that perform immediate updates (e.g., APIPAY, CHBORR).
  - Return results as soon as the COBOL program finishes.

- **Asynchronous API → COBOL Batch Programs**  
  - Submit requests to a queue or scheduler that triggers COBOL batch jobs (e.g., BATCHP, LONPT0).
  - Use job control files or database tables to track job status.
  - Notify clients when processing is complete (via polling or callbacks).

---

### 4. **Example Workflow**

**Synchronous Payment Posting:**
1. API receives payment request.
2. Calls APIPAY COBOL program.
3. Returns success/failure immediately.

**Asynchronous Batch Reconciliation:**
1. API receives batch reconciliation request.
2. Enqueues job for COBOL BATCHP program.
3. Returns job ID.
4. Client polls `GET /jobs/{jobId}` for status.
5. When complete, client retrieves results.

---

### 5. **Visual Representation**

Here's a visual representation:

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">API Endpoint Modes: Sync vs Async (COBOL Mapping)</text>
  
  <!-- Synchronous Path -->
  <rect x="60" y="80" width="320" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="220" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Synchronous Endpoint</text>
  <text x="220" y="135" text-anchor="middle" font-size="14" fill="#4338ca">Immediate COBOL Call (e.g., APIPAY)</text>
  
  <rect x="60" y="180" width="320" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="220" y="210" text-anchor="middle" font-size="14" fill="#1e40af">Returns Result Instantly</text>
  
  <!-- Asynchronous Path -->
  <rect x="520" y="80" width="320" height="80" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="680" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">Asynchronous Endpoint</text>
  <text x="680" y="135" text-anchor="middle" font-size="14" fill="#4338ca">Enqueue Batch Job (e.g., BATCHP)</text>
  
  <rect x="520" y="180" width="320" height="60" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="680" y="210" text-anchor="middle" font-size="14" fill="#1e40af">Returns Job ID for Status Polling</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="380" y1="120" x2="520" y2="120" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="380" y1="210" x2="520" y2="210" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  
  <!-- API Client -->
  <rect x="20" y="300" width="180" height="60" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="110" y="330" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">API Client</text>
  <text x="110" y="350" text-anchor="middle" font-size="12" fill="#22c55e">Chooses Sync or Async</text>
  
  <line x1="110" y1="300" x2="220" y2="160" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="110" y1="300" x2="680" y2="160" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

### 6. **COBOL Program References**

- **Synchronous Example Programs**:
  - APIPAY (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: programs) – Immediate payment posting
  - CHBORR (Source: [[FILE:CHBORR|CHBORR]], [[FILE:I|I]]ndex: programs) – Real-time borrower changes

- **Asynchronous Example Programs**:
  - BATCHP (Source: [[FILE:BATCHP|BATCHP]], [[FILE:I|I]]ndex: programs) – Batch processing
  - LONPT0 (Source: [[FILE:LONPT0|LONPT0]], [[FILE:I|I]]ndex: flows) – Batch loan posting

---

**Summary:**  
Design your API endpoints to support both immediate (synchronous) and deferred (asynchronous/batch) processing, mirroring COBOL's transaction and batch paradigms. Use job IDs and status endpoints for async operations, and direct responses for sync. This approach ensures compatibility with legacy workflows and modern API expectations.

**Q42: What strategy should we use for implementing idempotency in API operations to handle retry scenarios common in legacy systems?**

To implement **idempotency** in API operations—especially to handle retry scenarios common in legacy COBOL systems—you should use a strategy that ensures repeated requests with the same intent produce the same result, without unintended side effects (such as duplicate records or payments).

**Recommended Strategy:**

### 1. Idempotency Key Pattern
- Require clients to send a unique **idempotency key** with each API request (e.g., a UUID or transaction reference).
- Store the key and the result of the operation in a persistent data store (database or cache).
- On receiving a request:
  - If the key is new, process the operation and store the result.
  - If the key already exists, return the previously stored result—do NOT re-execute the operation.

### 2. Transaction Reference Mapping (COBOL Analogy)
- Many COBOL batch and online processes use transaction IDs, reference numbers, or batch keys to prevent duplicate processing.
- Mirror this by mapping API idempotency keys to legacy transaction identifiers, ensuring that retries do not create duplicate entries.

### 3. Safe Operation Design
- Design API endpoints so that repeated calls with the same parameters and idempotency key are safe:
  - For **POST** (create) operations: Only one resource is created per key.
  - For **PUT/PATCH** (update) operations: Multiple identical requests result in the same final state.
  - For **DELETE** operations: Multiple requests remove the resource once.

### 4. Error Handling and Logging
- Log all incoming requests with their idempotency keys and outcomes.
- If a retry occurs due to network or system error, the API can safely return the original result.

### 5. Legacy System Integration
- When integrating with COBOL systems, ensure that the legacy process also checks for duplicate transaction IDs or batch keys before processing.
- If the COBOL program does not natively support idempotency, add a wrapper or pre-processing step to check for existing transactions.

---

**Business Purpose:**
- Prevents duplicate payments, record creation, or updates due to retries (common in unreliable networks or batch reprocessing).
- Ensures consistency and reliability when migrating or wrapping legacy COBOL workflows in modern APIs.

**Technical Implementation Example:**
- API receives a payment request with idempotency key "abc123".
- Checks database for "abc123":
  - If not found: Processes payment, stores result with key.
  - If found: Returns stored result, does NOT process payment again.

**COBOL Analogy:**
- Similar to checking for existing batch reference numbers before posting a transaction (see comments in [[FILE:LONPF2.CBL|LONPF2.CBL]] and [[FILE:LONPF8.CBL|LONPF8.CBL]] about batch refno logic).

---

**Summary Table:**

| Strategy Component      | Purpose                                 | COBOL Analogy                |
|------------------------|------------------------------------------|------------------------------|
| Idempotency Key        | Uniquely identifies request              | Transaction ID, Batch Refno  |
| Persistent Storage     | Stores request/result mapping            | Batch file, audit log        |
| Duplicate Check Logic  | Prevents re-processing                   | IF EXISTS, SKIP/RETURN       |
| Error Handling         | Safe retries, consistent results         | Batch rerun protection       |

---

**Source References:**
- [[FILE:LONPF2.CBL|LONPF2.CBL]], [[FILE:LONPF8.CBL|LONPF8.CBL]]: Comments about batch reference logic to prevent duplicate processing (Source: [[FILE:LONPF2.CBL|LONPF2.CBL]], Index: code; [[FILE:LONPF8.CBL|LONPF8.CBL]], Index: code)
- [[FILE:DECLRP_SQL.CPY|DECLRP_SQL.CPY]], [[FILE:DECLARE.CPY|DECLARE.CPY]]: Modern API programs use flags and keys to suppress duplicate actions and screen interactions (Source: [[FILE:DECLRP_SQL.CPY|DECLRP_SQL.CPY]], Index: code; [[FILE:DECLARE.CPY|DECLARE.CPY]], Index: code)

---

**Here's a visual representation:**

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">API Idempotency Strategy for Legacy COBOL Integration</text>
  
  <!-- API Client -->
  <rect x="50" y="100" width="180" height="70" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="8"/>
  <text x="140" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#4338ca">API Client</text>
  <text x="140" y="150" text-anchor="middle" font-size="12" fill="#6366f1">Sends Request + Idempotency Key</text>
  
  <!-- API Gateway -->
  <rect x="270" y="100" width="180" height="70" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="8"/>
  <text x="360" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#1e40af">API Gateway</text>
  <text x="360" y="150" text-anchor="middle" font-size="12" fill="#3b82f6">Checks Idempotency Key</text>
  
  <!-- Idempotency Store -->
  <rect x="270" y="200" width="180" height="70" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="8"/>
  <text x="360" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#15803d">Idempotency Store</text>
  <text x="360" y="250" text-anchor="middle" font-size="12" fill="#22c55e">Key/Result Mapping</text>
  
  <!-- COBOL System -->
  <rect x="530" y="100" width="180" height="70" fill="#fef9c3" stroke="#f59e0b" stroke-width="3" rx="8"/>
  <text x="620" y="130" text-anchor="middle" font-size="16" font-weight="bold" fill="#b45309">COBOL System</text>
  <text x="620" y="150" text-anchor="middle" font-size="12" fill="#f59e0b">Legacy Processing</text>
  
  <!-- Batch File / Audit Log -->
  <rect x="530" y="200" width="180" height="70" fill="#fee2e2" stroke="#ef4444" stroke-width="3" rx="8"/>
  <text x="620" y="230" text-anchor="middle" font-size="16" font-weight="bold" fill="#b91c1c">Batch File / Audit Log</text>
  <text x="620" y="250" text-anchor="middle" font-size="12" fill="#ef4444">Duplicate Check</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="230" y1="135" x2="270" y2="135" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="450" y1="135" x2="530" y2="135" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="360" y1="170" x2="360" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="620" y1="170" x2="620" y2="200" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="360" y1="270" x2="620" y2="270" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

This strategy ensures safe, reliable API operations that respect legacy COBOL retry patterns and prevent duplicate processing.

**Q43: How do we design API error responses that provide meaningful feedback comparable to COBOL error messages and FILE STATUS codes?**

To design API error responses that provide **meaningful feedback comparable to COBOL error messages and FILE STATUS codes**, you should adopt a structured, multi-field error response pattern that mirrors the clarity and detail found in legacy COBOL systems.

### COBOL Error Handling Patterns

COBOL programs typically use:
- **FILE STATUS codes** (e.g., "98" for open error, "23" for record not found)
- **Detailed error messages** (often constructed with context: file name, program name, operation type)
- **Error buffers/structures** (e.g., ERROR-MSG-HEAD, ERROR-MSG-STAT, ERROR-MSG-PATH, ERROR-MSG-KEY)
- **Conditional logic** to display or suppress errors depending on context (API vs. screen)

**Example from [[FILE:DECLARE.CPY|DECLARE.CPY]]:**
```cobol
MOVE ERROR-MSG-HEAD TO ERRMSG-HEAD.
MOVE ERROR-MSG-STAT TO ERRMSG-MSG1.
MOVE ERROR-MSG-PATH TO ERRMSG-MSG2.
MOVE ERROR-MSG-KEY  TO ERRMSG-MSG3.
PERFORM CALL-ERRMSG.
(Source: [[FILE:DECLARE.CPY|DECLARE.CPY]], Index: code)
```

**Example from [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]]:**
```cobol
MOVE "CODE: "  TO SQL-ERROR-05-SQLCODEX.
MOVE SQLCODE   TO SQL-ERROR-05-SQLCODE.
MOVE "STATE: " TO SQL-ERROR-05-SQLSTATEX.
MOVE SQLSTATE  TO SQL-ERROR-05-SQLSTATE.
MOVE "KEY : "  TO SQL-ERROR-06-KEYX.
MOVE E-KEYX    TO SQL-ERROR-06-KEY.
(Source: [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]], Index: code)
```

### API Error Response Design Strategy

#### 1. **Structured Error Object**
Design your API error responses as structured JSON objects with multiple fields, reflecting COBOL's multi-part error buffers.

**Recommended fields:**
- `errorCode`: Numeric or symbolic code (maps to COBOL FILE STATUS or SQLCODE)
- `errorType`: High-level category (e.g., "OPEN_ERROR", "VALIDATION_ERROR")
- `message`: Human-readable explanation (maps to COBOL error message text)
- `file`: Name of the file/resource involved (if applicable)
- `operation`: The operation attempted (e.g., "READ", "WRITE")
- `program`: The module or API endpoint where the error occurred
- `key`: Record or resource key (if relevant)
- `status`: Additional status info (maps to COBOL status fields)
- `timestamp`: When the error occurred

**Example API Error Response:**
```json
{
  "errorCode": "98",
  "errorType": "OPEN_ERROR",
  "message": "Could not open file CUSTOMER.DAT",
  "file": "CUSTOMER.DAT",
  "operation": "OPEN",
  "program": "CustomerService",
  "key": null,
  "status": "File status 98",
  "timestamp": "2024-06-01T12:34:56Z"
}
```

#### 2. **Mapping COBOL Codes to API Codes**
- Maintain a mapping table between COBOL FILE STATUS codes and API error codes.
- Include legacy codes in the response for traceability (e.g., `"fileStatus": "23"`).

#### 3. **Detailed Context**
- Always include context: file name, operation, program/module, and key if available.
- Use clear, actionable messages (e.g., "Record not found in CUSTOMER.DAT").

#### 4. **Suppress/Display Logic**
- For API-only operations, suppress UI-specific messages (as seen with `EXT-API-SCREEN-DISABLE` in COBOL).
- For interactive APIs, include user-facing messages.

#### 5. **Error Logging**
- Log the full error object for support and debugging, similar to COBOL's error logging routines.

### Example Mapping Table

| COBOL FILE STATUS | API errorCode | errorType         | Typical Message                |
|-------------------|--------------|-------------------|-------------------------------|
| 00                | 200          | SUCCESS           | Operation successful          |
| 23                | 404          | NOT_FOUND         | Record not found              |
| 98                | 500          | OPEN_ERROR        | Could not open file           |
| 22                | 400          | INVALID_KEY       | Invalid key supplied          |
| ...               | ...          | ...               | ...                           |

### Summary

- Use a **multi-field error object** in API responses, mirroring COBOL's error buffers.
- Include **legacy codes** for traceability.
- Provide **clear, actionable messages** with full context.
- Map COBOL FILE STATUS and error codes to API error codes.
- Suppress or display messages based on API context, as COBOL does for screen vs. batch/API.

**References:**
- Error buffer construction and message logic (Source: [[FILE:DECLARE.CPY|DECLARE.CPY]], [[FILE:CONNECT_SQL.CPY|CONNECT_SQL.CPY]], Index: code)
- FILE STATUS code handling (Source: [[FILE:DECLRP.CPY|DECLRP.CPY]], Index: code)
- API suppression logic (`EXT-API-SCREEN-DISABLE`) (Source: [[FILE:DECLARE.CPY|DECLARE.CPY]], Index: code)

---

**Here's a visual representation of the mapping from COBOL error handling to API error response structure:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Error Handling → API Error Response Mapping</text>
  
  <!-- COBOL Error Handling Box -->
  <rect x="50" y="80" width="300" height="220" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="12"/>
  <text x="200" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Error Handling</text>
  <text x="70" y="140" font-size="14" fill="#6366f1">• FILE STATUS codes</text>
  <text x="70" y="160" font-size="14" fill="#6366f1">• ERROR-MSG-HEAD, STAT, PATH, KEY</text>
  <text x="70" y="180" font-size="14" fill="#6366f1">• Operation type (READ/WRITE/OPEN)</text>
  <text x="70" y="200" font-size="14" fill="#6366f1">• Program name, file name</text>
  <text x="70" y="220" font-size="14" fill="#6366f1">• Conditional display/suppression</text>
  <text x="70" y="240" font-size="14" fill="#6366f1">• Error logging routines</text>
  
  <!-- API Error Response Box -->
  <rect x="550" y="80" width="300" height="220" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="12"/>
  <text x="700" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">API Error Response</text>
  <text x="570" y="140" font-size="14" fill="#22c55e">• errorCode (maps FILE STATUS)</text>
  <text x="570" y="160" font-size="14" fill="#22c55e">• errorType (category)</text>
  <text x="570" y="180" font-size="14" fill="#22c55e">• message (human-readable)</text>
  <text x="570" y="200" font-size="14" fill="#22c55e">• file, operation, program, key</text>
  <text x="570" y="220" font-size="14" fill="#22c55e">• status, timestamp</text>
  <text x="570" y="240" font-size="14" fill="#22c55e">• Suppress/display logic</text>
  <text x="570" y="260" font-size="14" fill="#22c55e">• Error logging</text>
  
  <!-- Mapping Arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="350" y1="150" x2="550" y2="150" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="170" x2="550" y2="170" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="190" x2="550" y2="190" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="210" x2="550" y2="210" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="230" x2="550" y2="230" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="350" y1="250" x2="550" y2="250" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

### Advanced API Patterns and Operations

**Q44: What approach should we take for designing bulk operations APIs that replace COBOL batch processing programs?**

To design bulk operations APIs that effectively replace COBOL batch processing programs, you should adopt an approach that mirrors the strengths of batch processing—high throughput, reliability, and clear status tracking—while leveraging modern API patterns for scalability and usability.

**Recommended Approach:**

### 1. Asynchronous Job Submission
- **Pattern:** Clients submit bulk requests (e.g., large file uploads, mass updates) via an API endpoint.
- **Implementation:** The API immediately returns a job ID and status ("pending"), allowing the client to poll or subscribe for completion.
- **COBOL Analogy:** This matches how COBOL batch jobs are queued and processed in the background.

### 2. Job Status & Results Endpoints
- **Pattern:** Provide endpoints to query job status (pending, running, completed, failed) and retrieve results or error logs.
- **Implementation:** Store job metadata and results in a database or object store, similar to COBOL batch logs and output files.
- **COBOL Analogy:** Equivalent to checking batch logs or output files for completion and errors.

### 3. Input Validation & Preprocessing
- **Pattern:** Validate bulk input (files, records) before accepting the job, rejecting malformed or duplicate requests.
- **Implementation:** Use schema validation, deduplication, and pre-checks before queueing the job.
- **COBOL Analogy:** COBOL batch jobs often validate input files before processing, rejecting invalid batches.

### 4. Idempotency & Retry Handling
- **Pattern:** Assign unique job/request IDs and ensure repeated submissions with the same ID do not duplicate processing.
- **Implementation:** Store job IDs and check for existing jobs before accepting new ones.
- **COBOL Analogy:** Batch jobs often use control numbers or batch IDs to prevent duplicate runs.

### 5. Error Reporting & Logging
- **Pattern:** Return structured error responses and logs, including error codes, messages, and failed record details.
- **Implementation:** Provide downloadable error files or detailed error objects in API responses.
- **COBOL Analogy:** COBOL batch programs generate error reports and file status codes for troubleshooting.

### 6. Scalability & Resource Management
- **Pattern:** Use background workers, queues, or cloud batch services to process jobs efficiently.
- **Implementation:** Decouple API submission from processing; scale workers based on load.
- **COBOL Analogy:** Batch jobs are scheduled and run on mainframe batch queues, often overnight.

### 7. Security & Access Control
- **Pattern:** Authenticate and authorize bulk operations, especially for sensitive data.
- **Implementation:** Use API keys, OAuth, and role-based access for job submission and result retrieval.
- **COBOL Analogy:** Batch jobs often require operator or system-level permissions.

---

**Example Workflow:**

1. **Client submits bulk operation** (e.g., POST /bulk-payments with a file).
2. **API validates input** and returns a job ID (e.g., {"jobId": "12345", "status": "pending"}).
3. **Background worker processes the job** asynchronously.
4. **Client polls GET /bulk-payments/12345/status** or subscribes to notifications.
5. **Upon completion, client retrieves results** (e.g., GET /bulk-payments/12345/results).
6. **Error details** are available via GET /bulk-payments/12345/errors.

---

**COBOL Batch Program Example:**
Programs like **BULKT1**, **BULKT2**, **BULKG1**, and **BULKG2** process bulk transfers, totals, and print jobs in batch mode. Their flow typically involves:
- Initialization
- Bulk transfer processing
- Totals calculation
- Output generation
(Source: [[FILE:BULKT1|BULKT1]], [[FILE:BULKT2|BULKT2]], [[FILE:BULKG1|BULKG1]], [[FILE:BULKG2|BULKG2]], [[FILE:I|I]]ndex: flows)

**API Replacement:**
- Each batch step becomes a modular API job stage.
- Status and results are tracked per job.
- Errors are reported in structured responses.

---

**Here's a visual representation:**

<svg width="900" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">Bulk Operations API vs COBOL Batch</text>
  
  <!-- COBOL Batch Side -->
  <rect x="50" y="80" width="320" height="260" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="12"/>
  <text x="210" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Batch Processing</text>
  <rect x="80" y="140" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="165" text-anchor="middle" font-size="14" fill="#1e40af">Submit Batch Job (e.g., BULKT1)</text>
  <rect x="80" y="190" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="215" text-anchor="middle" font-size="14" fill="#1e40af">Process Records & Totals</text>
  <rect x="80" y="240" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="265" text-anchor="middle" font-size="14" fill="#1e40af">Generate Output & Error Logs</text>
  <rect x="80" y="290" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="315" text-anchor="middle" font-size="14" fill="#1e40af">Check Status via Logs</text>
  
  <!-- API Side -->
  <rect x="530" y="80" width="320" height="260" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="12"/>
  <text x="690" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">Bulk Operations API</text>
  <rect x="560" y="140" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="165" text-anchor="middle" font-size="14" fill="#15803d">POST /bulk-jobs (submit)</text>
  <rect x="560" y="190" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="215" text-anchor="middle" font-size="14" fill="#15803d">Async Processing (worker)</text>
  <rect x="560" y="240" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="265" text-anchor="middle" font-size="14" fill="#15803d">GET /bulk-jobs/{id}/status</text>
  <rect x="560" y="290" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="315" text-anchor="middle" font-size="14" fill="#15803d">GET /bulk-jobs/{id}/results</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="370" y1="160" x2="530" y2="160" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="370" y1="210" x2="530" y2="210" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="370" y1="260" x2="530" y2="260" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  <line x1="370" y1="310" x2="530" y2="310" stroke="#666" stroke-width="2" marker-end="url(#arrowhead)"/>
  
  <text x="450" y="370" text-anchor="middle" font-size="14" fill="#1a1a1a">Modern APIs mirror batch job flow, but add real-time status, error reporting, and scalability.</text>
</svg>

---

**Summary:**  
- Use asynchronous job submission, status tracking, and structured error reporting.
- Ensure idempotency and robust input validation.
- Map each COBOL batch step to a modular API stage.
- Provide endpoints for job results and error details.

This approach ensures your bulk operations APIs are reliable, scalable, and provide clear feedback—matching and improving upon legacy COBOL batch processing patterns.

(Source: [[FILE:BULKT1|BULKT1]], [[FILE:BULKT2|BULKT2]], [[FILE:BULKG1|BULKG1]], [[FILE:BULKG2|BULKG2]], [[FILE:I|I]]ndex: flows; [[FILE:CLOSER.CBL|CLOSER.CBL]], Index: code)

**Q45: How can we implement API transaction management to ensure consistency across multiple operations (similar to COBOL commit units)?**

To implement **API transaction management** that ensures consistency across multiple operations—mirroring COBOL commit units—you should adopt a strategy that provides atomicity, consistency, isolation, and durability (ACID) for grouped API actions. Here’s how to approach this:

---

### 1. **Business Purpose**
- **COBOL Commit Units:** In legacy COBOL systems, a commit unit groups several file/database operations so that either all succeed (commit) or all are rolled back (abort), ensuring data consistency.
- **API Equivalent:** For APIs, this means treating a set of related operations as a single transaction—either all changes are applied, or none are, even if requests are distributed across multiple endpoints or microservices.

---

### 2. **Technical Implementation Strategies**

#### **A. Explicit Transaction Boundaries**
- **Begin Transaction:** Provide an API endpoint or header to start a transaction (e.g., `POST /transactions/start`).
- **Perform Operations:** Subsequent API calls include a transaction ID, associating them with the open transaction.
- **Commit/Rollback:** Finalize with a `POST /transactions/{id}/commit` or `POST /transactions/{id}/rollback`.

#### **B. Two-Phase Commit (2PC) for Distributed Systems**
- **Prepare Phase:** Each service/resource involved prepares to commit and reports readiness.
- **Commit Phase:** If all are ready, coordinator instructs all to commit; otherwise, instructs rollback.
- **Use Cases:** Useful when API operations span multiple microservices or databases.

#### **C. Saga Pattern for Long-Running Transactions**
- **Orchestrate Sequence:** Each API operation is a step; if a step fails, compensating APIs are called to undo previous steps.
- **Compensation Logic:** Define "undo" endpoints for each operation (e.g., cancel payment, revert account update).
- **Best for:** Systems where distributed transactions are impractical due to scale or technology.

#### **D. Idempotency and Error Handling**
- **Idempotency Keys:** Ensure retries don’t result in duplicate operations.
- **Consistent Error Codes:** Return structured error responses (see previous answer) so clients can detect partial failures and trigger rollback/compensation.

#### **E. Transaction Logging and Auditing**
- **Log All Steps:** Maintain audit trails for each operation within a transaction, similar to COBOL audit files.
- **Status Tracking:** Expose transaction status via API (`GET /transactions/{id}`).

---

### 3. **COBOL Parallels and Source Examples**

- **COBOL Commit Example:**  
  ```cobol
  IF ( IO-FG = 0 )
     EXEC SQL COMMIT END-EXEC
     PERFORM SQL-IO-VALIDATION.
  ```
  (Source: [[FILE:IORXIN.CBL|IORXIN.CBL]], Index: code, lines 1651-1675)

- **API Equivalent:**  
  - Begin transaction, perform multiple API calls, then commit or rollback based on outcome.

---

### 4. **Recommended API Transaction Flow**

**Step-by-step:**
1. **Client starts transaction:**  
   `POST /transactions/start` → returns transaction ID.
2. **Client performs operations:**  
   Each API call includes transaction ID (e.g., header or parameter).
3. **Client commits transaction:**  
   `POST /transactions/{id}/commit` → all changes applied atomically.
4. **If error occurs:**  
   `POST /transactions/{id}/rollback` → all changes reverted.

---

### 5. **Visual Representation**

Here's a visual representation:

<svg width="700" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">API Transaction Management Flow</text>
  
  <!-- Start Transaction -->
  <rect x="50" y="80" width="180" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="140" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Start Transaction</text>
  <text x="140" y="130" text-anchor="middle" fill="white" font-size="11">POST /transactions/start</text>
  
  <!-- Operation 1 -->
  <rect x="270" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="360" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Operation 1</text>
  <text x="360" y="130" text-anchor="middle" fill="white" font-size="11">e.g., Update Account</text>
  
  <!-- Operation 2 -->
  <rect x="490" y="80" width="180" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="580" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Operation 2</text>
  <text x="580" y="130" text-anchor="middle" fill="white" font-size="11">e.g., Record Payment</text>
  
  <!-- Commit -->
  <rect x="270" y="200" width="180" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="360" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Commit Transaction</text>
  <text x="360" y="250" text-anchor="middle" fill="white" font-size="11">POST /transactions/{id}/commit</text>
  
  <!-- Rollback -->
  <rect x="490" y="200" width="180" height="60" fill="#ef4444" stroke="#dc2626" stroke-width="2" rx="8"/>
  <text x="580" y="230" text-anchor="middle" fill="white" font-size="16" font-weight="bold">Rollback Transaction</text>
  <text x="580" y="250" text-anchor="middle" fill="white" font-size="11">POST /transactions/{id}/rollback</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="230" y1="110" x2="270" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="450" y1="110" x2="490" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="360" y1="140" x2="360" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="580" y1="140" x2="580" y2="200" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

### 6. **Summary**

- Use explicit transaction boundaries and commit/rollback endpoints.
- For distributed operations, consider 2PC or Saga patterns.
- Ensure idempotency and robust error handling.
- Log all transaction steps for audit and recovery.
- This approach provides consistency and reliability comparable to COBOL commit units.

**Source references:**  
- COBOL commit logic: [[FILE:IORXIN.CBL|IORXIN.CBL]], Index: code  
- API transaction management patterns: industry best practices, not directly in retrieved context

If you need specific COBOL code examples or API design templates, let me know!

**Q46: What patterns should we use for implementing optimistic or pessimistic locking in APIs to handle concurrent updates?**

To implement **optimistic or pessimistic locking in APIs**—mirroring COBOL file locking and concurrency control—you should adopt patterns that ensure data consistency and prevent lost updates during concurrent operations. Here’s how each approach translates from COBOL to modern API design:

---

### **Optimistic Locking Pattern**

**Business Purpose:**
- Prevents lost updates when multiple clients attempt to modify the same resource.
- Assumes conflicts are rare; checks for changes before committing updates.

**Technical Implementation:**
- **Versioning:** Each resource includes a version field (e.g., `version`, `updated_at`, or a hash).
- **Read:** Client retrieves the resource along with its version.
- **Update:** Client sends the intended changes **plus** the version it read.
- **Check:** On update, the API checks if the current version matches the client’s version.
    - **If match:** Apply update, increment version.
    - **If mismatch:** Reject update with a conflict error (HTTP 409), return current resource state.

**Example API Response for Conflict:**
```json
{
  "error": {
    "code": "CONFLICT",
    "message": "Resource has been modified by another user.",
    "currentVersion": 7,
    "yourVersion": 6,
    "resource": { ... }
  }
}
```

**COBOL Analogy:** Similar to checking FILE STATUS codes for concurrent update errors (e.g., status 22 or 23 for record locked/changed).

---

### **Pessimistic Locking Pattern**

**Business Purpose:**
- Prevents other clients from modifying a resource while one client is working on it.
- Used when conflicts are likely or updates are critical.

**Technical Implementation:**
- **Lock Endpoint:** API provides a `/lock` or `/checkout` endpoint for resources.
    - Client requests a lock before making changes.
    - API marks the resource as locked (stores lock owner and timestamp).
- **Update:** Only the lock owner can update or release the resource.
- **Timeouts:** Locks expire after a set period to prevent deadlocks.
- **Unlock Endpoint:** Client releases the lock when done.

**Example API Lock Workflow:**
1. `POST /resource/123/lock` → returns lock token
2. `PUT /resource/123` with lock token → update allowed
3. `POST /resource/123/unlock` → releases lock

**Error Handling:**
- If another client tries to update a locked resource, API returns:
```json
{
  "error": {
    "code": "LOCKED",
    "message": "Resource is currently locked by another user.",
    "lockedBy": "user123",
    "lockExpiresAt": "2024-06-30T12:00:00Z"
  }
}
```

**COBOL Analogy:** Similar to COBOL’s `LOCK MODE` on indexed files (see [[FILE:UP1534_FS.CPY|UP1534_FS.CPY]]), where records are locked for exclusive access during updates.

---

### **Recommended Patterns for APIs**

- **Optimistic Locking:** Use for most RESTful APIs where updates are infrequent and user experience is prioritized.
    - Include a version or timestamp field in all update requests.
    - Return HTTP 409 Conflict on version mismatch.
- **Pessimistic Locking:** Use for critical workflows (e.g., financial transactions, batch updates) where concurrent edits must be strictly controlled.
    - Implement explicit lock/unlock endpoints.
    - Track lock ownership and expiration.

---

### **COBOL Reference Example**

From [[FILE:UP1534_FS.CPY|UP1534_FS.CPY]]:
```cobol
SELECT OCX-FILE ASSIGN TO OCX-PATH
  ORGANIZATION INDEXED
  ACCESS DYNAMIC
  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
  RECORD KEY OCX1-KEY
  FILE STATUS FILE-STAT.
```
- This COBOL pattern uses **pessimistic locking** to prevent concurrent updates.
- In APIs, this translates to explicit lock management endpoints and error codes.

---

### **Summary Table**

| Pattern              | API Mechanism                | COBOL Analogy                | Use Case                        |
|----------------------|-----------------------------|------------------------------|----------------------------------|
| Optimistic Locking   | Version/timestamp check      | FILE STATUS 22/23 (conflict) | Most REST APIs, low conflict     |
| Pessimistic Locking  | Lock/unlock endpoints        | LOCK MODE AUTOMATIC          | High-value, high-conflict ops    |

---

**Key Takeaway:**  
Choose **optimistic locking** for general API updates and **pessimistic locking** for critical, high-conflict operations—both patterns provide robust concurrency control comparable to COBOL’s file locking and status codes.

(Source: [[FILE:UP1534_FS.CPY|UP1534_FS.CPY]], Index: code)

**Q47: How do we design APIs to support partial updates and field-level modifications without requiring full object replacement?**

To design APIs that support **partial updates and field-level modifications**—without requiring full object replacement (as is common in COBOL batch programs)—you should adopt the following patterns and strategies:

---

**1. Use PATCH or Field-Specific Update Endpoints**
- Implement HTTP PATCH methods for resources, allowing clients to send only the fields they wish to modify.
- Alternatively, provide dedicated endpoints for updating specific fields (e.g., `/loans/{id}/status`).

**2. Accept Sparse Payloads**
- Design your API to accept requests containing only the fields to be updated.
- The backend should merge these changes into the existing object, leaving other fields untouched.

**3. Support JSON Merge Patch or JSON Patch Standards**
- Use [RFC 7386 (JSON Merge Patch)](https://tools.ietf.org/html/rfc7386) or [RFC 6902 (JSON Patch)](https://tools.ietf.org/html/rfc6902) for fine-grained updates.
- Example:  
  - JSON Merge Patch: `{ "status": "closed" }`  
  - JSON Patch: `[ { "op": "replace", "path": "/status", "value": "closed" } ]`

**4. Field-Level Validation and Authorization**
- Validate only the fields present in the request.
- Apply field-level authorization to ensure users can only modify permitted fields.

**5. Optimistic Concurrency Control**
- Include a version or timestamp field in the resource.
- Require clients to provide the current version when updating, preventing lost updates.

**6. Audit and Change Tracking**
- Log which fields were changed, by whom, and when—mirroring COBOL audit trails for field-level changes.

**7. Error Handling for Partial Updates**
- Return clear error messages for invalid or unauthorized field changes.
- Support partial success responses if some fields fail validation.

---

**Business Rationale:**
- Enables efficient, targeted updates (e.g., changing only an address or status).
- Reduces risk of overwriting unrelated fields.
- Improves user experience and system performance, especially for large objects.

**Technical Implementation Example:**
Suppose you have a loan object:
```json
{
  "loanId": "123",
  "amount": 10000,
  "status": "active",
  "borrower": "John Doe"
}
```
To update only the status:
- PATCH `/loans/123`  
  Payload: `{ "status": "closed" }`
- Backend merges the change, leaving `amount` and `borrower` unchanged.

---

**COBOL Comparison:**
- COBOL batch programs often require full record replacement, risking accidental data loss.
- Modern APIs allow granular updates, reducing risk and improving maintainability.

---

**Summary Table: Partial Update Patterns**

| Pattern                   | Description                                  | Example Usage                |
|---------------------------|----------------------------------------------|------------------------------|
| PATCH method              | Update only specified fields                 | PATCH /loans/123             |
| JSON Merge Patch          | Merge provided fields into object            | `{ "status": "closed" }`     |
| JSON Patch                | Explicit field operations                    | `[{"op":"replace",...}]`     |
| Field-specific endpoints  | Update one field at a time                   | PUT /loans/123/status        |
| Optimistic concurrency    | Prevent lost updates with versioning         | PATCH with `If-Match` header |

---

**References:**
- [RFC 7386: JSON Merge Patch](https://tools.ietf.org/html/rfc7386)
- [RFC 6902: JSON Patch](https://tools.ietf.org/html/rfc6902)

If you need code samples or architectural diagrams for these patterns, let me know!

**Q48: What strategy should we use for implementing audit logging in APIs to track changes similar to COBOL audit trails?**

To implement **audit logging in APIs** that tracks changes similar to COBOL audit trails, you should adopt a strategy that ensures every significant data modification is recorded with sufficient detail for traceability, compliance, and troubleshooting.

### Recommended Strategy

**1. Centralized Audit Log Structure**
- Create a dedicated audit log data store (database table, file, or service) analogous to COBOL’s audit files (e.g., AUFILE).
- Each audit record should capture:
  - **Change type** (Add, Delete, Update)
  - **Timestamp**
  - **User or system identity**
  - **Affected object/record ID**
  - **Field changed**
  - **Original value**
  - **New value**
  - **Transaction or request ID**

**2. Automatic Logging on Data Modification**
- Integrate audit logging at the service or data access layer so every API call that modifies data triggers an audit entry.
- Use middleware, interceptors, or ORM hooks to capture changes automatically.

**3. Granular Field-Level Tracking**
- Log changes at the field level, not just the object level, to mirror COBOL’s detailed audit trails (see AUDITW-CHANGE-TYPE and AU-REC in COBOL).
- For partial updates (PATCH), record only the fields that changed.

**4. Consistent Change Types**
- Standardize change types (e.g., "A" for Add, "D" for Delete, "C" for Change) as in COBOL (Source: [[FILE:AUDITWW.CPY|AUDITWW.CPY]], Index: code).

**5. Secure and Immutable Storage**
- Ensure audit logs are write-only and tamper-evident (append-only, with access controls).
- Consider using cryptographic hashes or blockchain for high-integrity requirements.

**6. Query and Reporting Support**
- Provide API endpoints or tools to query audit logs by user, object, date, or change type for compliance and troubleshooting.

### Example Audit Log Schema (API Context)
| Field           | Description                       |
|-----------------|-----------------------------------|
| id              | Unique audit record ID            |
| timestamp       | When the change occurred          |
| user_id         | Who made the change               |
| object_type     | Type of object changed            |
| object_id       | Unique ID of the object           |
| field_name      | Name of the field changed         |
| change_type     | "A", "D", "C" (add, delete, change)|
| original_value  | Value before change               |
| new_value       | Value after change                |
| transaction_id  | API request or transaction ID     |

### COBOL Reference

- **AUDITW-CHANGE-TYPE**: "D" DELETE, "A" ADD, "C" CHANGE (Source: [[FILE:AUDITWW.CPY|AUDITWW.CPY]], Index: code)
- **AU-REC**: Contains keys, field descriptions, original and new values (Source: [[FILE:GB01AU.CPY|GB01AU.CPY]], Index: code)
- **AUDITW program**: Writes changes to GB/AUFILE for every modification (Source: [[FILE:AUDITW.CBL|AUDITW.CBL]], Index: code)

### Implementation Patterns

- Use event sourcing or change data capture for robust audit trails.
- For REST APIs, implement audit logging in POST, PUT, PATCH, DELETE handlers.
- For microservices, consider a centralized audit service or publish audit events to a message queue.

---

**Summary:**  
Implement a centralized, field-level, immutable audit log that records every data change with full context, mirroring COBOL’s audit trail approach. Automate logging at the data modification layer and standardize change types for consistency.

---

Here's a visual representation of the COBOL audit logging structure and its API equivalent:

<svg width="900" height="350" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">Audit Logging Structure: COBOL vs API</text>
  
  <!-- COBOL Audit Trail -->
  <rect x="60" y="70" width="320" height="180" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="12"/>
  <text x="220" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Audit Trail</text>
  <text x="80" y="130" font-size="14" fill="#6366f1">File: GB/AUFILE</text>
  <text x="80" y="150" font-size="14" fill="#6366f1">Fields:</text>
  <text x="100" y="170" font-size="13" fill="#6366f1">- Change Type ("A", "D", "C")</text>
  <text x="100" y="190" font-size="13" fill="#6366f1">- Date/Time</text>
  <text x="100" y="210" font-size="13" fill="#6366f1">- User ID</text>
  <text x="100" y="230" font-size="13" fill="#6366f1">- Field Changed</text>
  <text x="100" y="250" font-size="13" fill="#6366f1">- Original/New Value</text>
  
  <!-- API Audit Log -->
  <rect x="520" y="70" width="320" height="180" fill="#dbeafe" stroke="#3b82f6" stroke-width="3" rx="12"/>
  <text x="680" y="100" text-anchor="middle" font-size="18" font-weight="bold" fill="#1e40af">API Audit Log</text>
  <text x="540" y="130" font-size="14" fill="#1e40af">Table/Service: audit_log</text>
  <text x="540" y="150" font-size="14" fill="#1e40af">Fields:</text>
  <text x="560" y="170" font-size="13" fill="#1e40af">- change_type ("A", "D", "C")</text>
  <text x="560" y="190" font-size="13" fill="#1e40af">- timestamp</text>
  <text x="560" y="210" font-size="13" fill="#1e40af">- user_id</text>
  <text x="560" y="230" font-size="13" fill="#1e40af">- field_name</text>
  <text x="560" y="250" font-size="13" fill="#1e40af">- original_value / new_value</text>
  <text x="560" y="270" font-size="13" fill="#1e40af">- transaction_id</text>
  
  <!-- Arrow -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="380" y1="160" x2="520" y2="160" stroke="#666" stroke-width="3" marker-end="url(#arrow)"/>
  <text x="450" y="150" text-anchor="middle" font-size="15" fill="#333">Modern API Implementation</text>
</svg>

**References:**
- [[FILE:AUDITWW.CPY|AUDITWW.CPY]] - Audit change type and log structure (Source: [[FILE:AUDITWW.CPY|AUDITWW.CPY]], Index: code)
- [[FILE:GB01AU.CPY|GB01AU.CPY]] - Audit record fields (Source: [[FILE:GB01AU.CPY|GB01AU.CPY]], Index: code)
- [[FILE:AUDITW.CBL|AUDITW.CBL]] - Audit file writing logic (Source: [[FILE:AUDITW.CBL|AUDITW.CBL]], Index: code)

**Q49: How can we design search and query APIs that replace COBOL indexed file access and alternate key lookups?**

To design **search and query APIs** that effectively replace COBOL indexed file access and alternate key lookups, you should adopt patterns that provide flexible, efficient, and reliable data retrieval—mirroring the capabilities found in COBOL’s `SELECT ... ORGANIZATION INDEXED ... RECORD KEY ... ALTERNATE RECORD KEY ...` constructs.

### COBOL Indexed File Access Patterns

COBOL programs use indexed files to:
- Retrieve records by primary key (e.g., account number, SSN)
- Support alternate key lookups (e.g., by name, date, branch)
- Allow duplicate keys for alternate indexes
- Lock records for update/read consistency

**Example from context:**
```cobol
SELECT BI-FILE ASSIGN TO BI-PATH
       ORGANIZATION INDEXED
       ACCESS DYNAMIC
       LOCK MODE AUTOMATIC WITH LOCK ON RECORD
       RECORD KEY BI1-KEY
       ALTERNATE RECORD KEY BI2-KEY WITH DUPLICATES
       FILE STATUS FILE-STAT.
```
(Source: [[FILE:LPFSBI.CPY|LPFSBI.CPY]], Index: code)

### API Design Strategies

#### 1. **Flexible Query Endpoints**
- Implement endpoints that allow clients to query by any indexed field (primary or alternate).
- Example: `/api/loans?accountNo=12345678`, `/api/loans?ssn=987654321`, `/api/loans?branch=001&date=2023-01-01`

#### 2. **Support for Multiple Keys**
- Accept multiple query parameters for alternate keys.
- Use database indexes or search engine indexes to optimize lookup speed.

#### 3. **Duplicate Key Handling**
- For alternate keys that allow duplicates (e.g., multiple loans for same SSN), return arrays of matching records.
- Example response:  
  ```json
  [
    { "loanId": "A123", "ssn": "987654321", ... },
    { "loanId": "B456", "ssn": "987654321", ... }
  ]
  ```

#### 4. **Pagination and Sorting**
- Provide pagination (`limit`, `offset`, `page`) and sorting (`sortBy`, `order`) to handle large result sets efficiently.

#### 5. **Locking and Consistency**
- For update scenarios, use optimistic concurrency (e.g., version numbers) or pessimistic locking (e.g., lock tokens) as needed.
- For read-only queries, ensure APIs return consistent snapshots.

#### 6. **Error Handling**
- Return clear error codes/messages for not found, duplicate key, or access errors (similar to COBOL’s `FILE STATUS`).

#### 7. **Index Metadata Exposure**
- Optionally expose metadata about available keys/indexes so clients know which fields are searchable.

### Example API Endpoint Designs

| COBOL Indexed File Pattern                | API Equivalent Example                                 |
|-------------------------------------------|--------------------------------------------------------|
| Primary Key: `RECORD KEY BI1-KEY`         | `GET /api/loans/{loanId}`                             |
| Alternate Key: `ALTERNATE RECORD KEY BI2` | `GET /api/loans?ssn=987654321`                        |
| Multiple Alternate Keys                   | `GET /api/loans?branch=001&date=2023-01-01`           |
| Duplicates Allowed                        | `GET /api/loans?ssn=987654321` (returns array)        |

### Technical Implementation

- Use relational databases (with indexes on all key fields) or NoSQL/document stores with secondary indexes.
- For complex queries, consider integrating a search engine (e.g., Elasticsearch) for full-text and multi-field search.
- Map COBOL file status codes to HTTP status codes (e.g., 404 for not found, 409 for conflict).

### Summary Table: Key API Features

| Feature                | COBOL Indexed File | API Design Pattern                |
|------------------------|-------------------|-----------------------------------|
| Primary Key Lookup     | RECORD KEY        | Path param or query param         |
| Alternate Key Lookup   | ALTERNATE KEY     | Query param(s)                    |
| Duplicate Key Support  | WITH DUPLICATES   | Array response                    |
| Locking                | LOCK ON RECORD    | Versioning or lock tokens         |
| Error Handling         | FILE STATUS       | HTTP status + error body          |
| Pagination             | Not native        | `limit`, `offset`, `page` params  |

---

**Source references:**
- Indexed file and alternate key usage: [[FILE:LPFSBI.CPY|LPFSBI.CPY]], [[FILE:LPFSCI.CPY|LPFSCI.CPY]], [[FILE:UP1534_FS.CPY|UP1534_FS.CPY]] (multiple lines)
- Locking and file status: [[FILE:LPFSBI.CPY|LPFSBI.CPY]], [[FILE:UP1534_FS.CPY|UP1534_FS.CPY]]

---

**Here's a visual representation of how API search/query endpoints map to COBOL indexed file access:**

<svg width="900" height="420" xmlns="http://www.w3.org/2000/svg">
  <text x="450" y="30" text-anchor="middle" font-size="22" font-weight="bold" fill="#1a1a1a">COBOL Indexed File Access vs. API Search/Query</text>
  
  <!-- COBOL Side -->
  <rect x="50" y="80" width="320" height="260" fill="#e0e7ff" stroke="#667eea" stroke-width="3" rx="12"/>
  <text x="210" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#4338ca">COBOL Indexed File</text>
  <rect x="80" y="140" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="165" text-anchor="middle" font-size="14" fill="#1e40af">RECORD KEY: BI1-KEY</text>
  <rect x="80" y="190" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="215" text-anchor="middle" font-size="14" fill="#1e40af">ALTERNATE KEY: BI2-KEY</text>
  <rect x="80" y="240" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="265" text-anchor="middle" font-size="14" fill="#1e40af">WITH DUPLICATES</text>
  <rect x="80" y="290" width="260" height="40" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="8"/>
  <text x="210" y="315" text-anchor="middle" font-size="14" fill="#1e40af">LOCK ON RECORD</text>
  
  <!-- API Side -->
  <rect x="530" y="80" width="320" height="260" fill="#dcfce7" stroke="#22c55e" stroke-width="3" rx="12"/>
  <text x="690" y="110" text-anchor="middle" font-size="18" font-weight="bold" fill="#15803d">API Search/Query</text>
  <rect x="560" y="140" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="165" text-anchor="middle" font-size="14" fill="#15803d">GET /api/loans/{loanId}</text>
  <rect x="560" y="190" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="215" text-anchor="middle" font-size="14" fill="#15803d">GET /api/loans?ssn=987654321</text>
  <rect x="560" y="240" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="265" text-anchor="middle" font-size="14" fill="#15803d">Returns array if duplicates</text>
  <rect x="560" y="290" width="260" height="40" fill="#bbf7d0" stroke="#22c55e" stroke-width="2" rx="8"/>
  <text x="690" y="315" text-anchor="middle" font-size="14" fill="#15803d">Optimistic/Pessimistic Lock</text>
  
  <!-- Connecting arrows -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#666"/>
    </marker>
  </defs>
  <line x1="370" y1="160" x2="530" y2="160" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="370" y1="210" x2="530" y2="210" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="370" y1="260" x2="530" y2="260" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="370" y1="310" x2="530" y2="310" stroke="#666" stroke-width="2" marker-end="url(#arrow)"/>
</svg>

---

**In summary:**  
Design your APIs to support flexible queries by any indexed field, handle duplicate results, provide pagination, and map COBOL locking and error handling to modern API patterns. This ensures a seamless transition from COBOL indexed file access to robust, user-friendly API endpoints.

**Source references:**  
- [[FILE:LPFSBI.CPY|LPFSBI.CPY]], [[FILE:LPFSCI.CPY|LPFSCI.CPY]], [[FILE:UP1534_FS.CPY|UP1534_FS.CPY]] (COBOL indexed file and alternate key patterns)

**Q50: What approach should we take for implementing business workflow state machines in APIs based on COBOL process flows?**

To implement **business workflow state machines in APIs** based on COBOL process flows, you should adopt a strategy that translates the procedural, paragraph-driven logic of COBOL into explicit, manageable state transitions in your API architecture.

### Recommended Approach

#### 1. **Model States and Transitions Explicitly**
- **Identify Key States:** Analyze COBOL programs for major workflow stages (e.g., "INITIALIZATION", "VALIDATION", "PROCESSING", "ERROR", "COMPLETED").
  - Example: In APIPAY, states include INITIALIZATION, VALIDATE-REVERSAL, CREATE-LOG, etc.  
    (Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows)
- **Define State Machine:** Create a state machine model where each state represents a business step, and transitions are triggered by events or API calls.

#### 2. **Map COBOL Paragraphs to API Actions**
- Each COBOL paragraph (e.g., VALIDATE-REVERSAL, CREATE-LOG) becomes an API endpoint or internal handler.
- Transitions between paragraphs (PERFORM, GO TO) become explicit state transitions in your workflow engine.

#### 3. **Use Event-Driven or Command Pattern**
- Trigger state transitions via API requests, events, or commands (e.g., "submitPayment", "validateInput").
- Maintain workflow context (current state, data) in a persistent store (e.g., database, cache).

#### 4. **Persist Workflow State**
- Store the current state and relevant data for each workflow instance.
- Enables recovery, auditing, and long-running processes (similar to COBOL batch checkpoints).

#### 5. **Implement Transition Guards and Actions**
- Enforce business rules at each transition (as COBOL IF/EVALUATE statements do).
- Example: Only allow transition from VALIDATION to PROCESSING if all checks pass.

#### 6. **Expose Workflow via API**
- Provide endpoints to:
  - Start a workflow
  - Advance to the next state
  - Query current state and history
  - Cancel or rollback

#### 7. **Audit and Error Handling**
- Log each state transition and error, mirroring COBOL audit trails.
- Return meaningful error codes/messages for invalid transitions.

---

### Example Mapping: COBOL Flow to API State Machine

**COBOL Flow (APIPAY):**
- DECLARATIVES → INITIALIZATION → CREATE-LOG → VALIDATE-REVERSAL → PROCESS-PAYMENT → END

**API State Machine:**
- States: INIT → LOGGED → VALIDATED → PROCESSED → COMPLETED
- Transitions:  
  - POST /start → INIT  
  - POST /log → LOGGED  
  - POST /validate → VALIDATED  
  - POST /process → PROCESSED  
  - POST /complete → COMPLETED

---

### Visual Representation:

Here's a visual representation:

<svg width="700" height="400" xmlns="http://www.w3.org/2000/svg">
  <text x="350" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1a1a1a">API Workflow State Machine (COBOL Mapping)</text>
  
  <!-- States -->
  <rect x="50" y="80" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="110" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">INIT</text>
  <text x="110" y="130" text-anchor="middle" fill="white" font-size="11">Initialization</text>
  
  <rect x="220" y="80" width="120" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="280" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">LOGGED</text>
  <text x="280" y="130" text-anchor="middle" fill="white" font-size="11">Create Log</text>
  
  <rect x="390" y="80" width="120" height="60" fill="#f59e0b" stroke="#d97706" stroke-width="2" rx="8"/>
  <text x="450" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">VALIDATED</text>
  <text x="450" y="130" text-anchor="middle" fill="white" font-size="11">Validate</text>
  
  <rect x="560" y="80" width="120" height="60" fill="#4ade80" stroke="#16a34a" stroke-width="2" rx="8"/>
  <text x="620" y="110" text-anchor="middle" fill="white" font-size="16" font-weight="bold">PROCESSED</text>
  <text x="620" y="130" text-anchor="middle" fill="white" font-size="11">Process</text>
  
  <rect x="320" y="220" width="120" height="60" fill="#667eea" stroke="#4338ca" stroke-width="2" rx="8"/>
  <text x="380" y="250" text-anchor="middle" fill="white" font-size="16" font-weight="bold">COMPLETED</text>
  <text x="380" y="270" text-anchor="middle" fill="white" font-size="11">End</text>
  
  <!-- Arrows -->
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#333"/>
    </marker>
  </defs>
  <line x1="170" y1="110" x2="220" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="340" y1="110" x2="390" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="510" y1="110" x2="560" y2="110" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
  <line x1="620" y1="140" x2="380" y2="220" stroke="#333" stroke-width="3" marker-end="url(#arrowhead)"/>
</svg>

---

### Summary

- **Explicitly model states and transitions** based on COBOL paragraphs and business rules.
- **Persist workflow state** for reliability and auditability.
- **Expose state transitions via API endpoints** for modular, maintainable workflows.
- **Map COBOL process flows** to API state machines for clarity and control.

This approach ensures your APIs can replicate complex COBOL business workflows with modern, maintainable, and auditable architecture.

(Source: [[FILE:APIPAY|APIPAY]], [[FILE:I|I]]ndex: flows; general COBOL workflow analysis)

---

## Section 10.4: Domain-Driven API Architecture

### Organizing APIs by Functional Domains

**Q: How should OTIS APIs be organized based on the system's functional domains to create maintainable, scalable service boundaries?**

The OTIS system comprises **14 major functional domains** that provide natural boundaries for API organization. A domain-driven approach ensures clear service boundaries, team alignment, and independent deployment capabilities.

---

### **Domain-Based API Organization Strategy**

#### **1. Core Business Domain APIs**

##### **Loan Processing API** (`/api/loans`)

**Domain Scope:** Complete loan lifecycle management
**Key Endpoints:**

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| POST | `/api/loans` | Create new loan | LONPFC, APILNS |
| GET | `/api/loans/{id}` | Retrieve loan details | CPINQ0, LTINFO |
| PUT | `/api/loans/{id}` | Update loan information | LONPF3, LONPS4 |
| DELETE | `/api/loans/{id}` | Close/cancel loan | LPMAIN |
| POST | `/api/loans/{id}/payments` | Post payment | APIPAY, LONPW9 |
| GET | `/api/loans/{id}/payoff` | Calculate payoff | GP-PAYOFF-NONCASH |
| POST | `/api/loans/{id}/modifications` | Modify loan terms | LONPF3 |
| GET | `/api/loans/{id}/history` | Payment history | AUDIQ, AUDIQ1 |

**Sub-Domain APIs:**
- `/api/payments/*` - Payment processing sub-domain (APIPAY, REFUPD, REGPAY)
- `/api/loans/{id}/maintenance/*` - Loan maintenance operations (LP01LN, LP01LP)

---

##### **Dealer & Addon API** (`/api/dealers`, `/api/addons`)

**Domain Scope:** Dealer relationship and addon product management

**Dealer Endpoints:**

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| POST | `/api/dealers` | Create dealer record | DLMAIN |
| GET | `/api/dealers/{id}` | Retrieve dealer info | LP01DL |
| PUT | `/api/dealers/{id}` | Update dealer record | DLMAN3 |
| GET | `/api/dealers/{id}/statistics` | Dealer performance | DLMAIN |
| GET | `/api/dealers/{id}/memos` | Dealer memos | LP01DM |
| GET | `/api/dealers/{id}/transactions` | Transaction history | LP01DT |

**Addon Endpoints:**

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| POST | `/api/addons` | Add product to loan | LONPFE, ADMAIN |
| GET | `/api/addons/{id}` | Retrieve addon details | LP01IC |
| PUT | `/api/addons/{id}` | Update addon | ADMAIN |
| DELETE | `/api/addons/{id}` | Reverse addon | LONPFF |
| GET | `/api/addons/insurance-companies` | Insurance providers | LP01IC |

---

#### **2. Batch & Processing Domain APIs**

##### **Batch Processing API** (`/api/batch`)

**Domain Scope:** Scheduled operations and bulk processing

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| POST | `/api/batch/eod` | Trigger end-of-day | DAILY, DPFILE |
| POST | `/api/batch/eom` | Trigger end-of-month | EOMAIN, EOCRON |
| GET | `/api/batch/jobs/{id}` | Job status | BPOTH |
| POST | `/api/batch/jobs/{id}/cancel` | Cancel batch job | BPBRMN |
| GET | `/api/batch/schedule` | Scheduled jobs | EOCRON |
| POST | `/api/batch/accruals` | Run accrual calc | DAILY |

---

#### **3. Inquiry & Reporting Domain APIs**

##### **Reporting API** (`/api/reports`)

**Domain Scope:** Data access, analysis, and compliance reporting

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| GET | `/api/reports/audit-trail` | Audit inquiries | AUDIQ, AUDIQ1 |
| GET | `/api/reports/loan-summary` | Summary reports | SUMMBY |
| GET | `/api/reports/delinquency` | Delinquency listing | CPINQ0 |
| GET | `/api/reports/branch-stats` | Branch statistics | DBREPT |
| POST | `/api/reports/custom` | Generate custom report | PERUSE |
| GET | `/api/reports/{id}/download` | Download report | RZWRI4, TR1534 |

---

#### **4. Organization Domain APIs**

##### **Branch & Group API** (`/api/branches`, `/api/groups`)

**Domain Scope:** Organizational hierarchy management

**Branch Endpoints:**

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| GET | `/api/branches` | List all branches | BRMAN2 |
| POST | `/api/branches` | Create branch | BRMAN5 |
| GET | `/api/branches/{id}` | Branch details | BRMAN7 |
| PUT | `/api/branches/{id}` | Update branch | BPBRMN |
| GET | `/api/branches/{id}/performance` | Branch stats | BROPM1 |

**Group Endpoints:**

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| GET | `/api/groups` | List groups | GPMAN1 |
| POST | `/api/groups` | Create group | GPOPM1 |
| PUT | `/api/groups/{id}` | Update group | GPOPM2 |
| GET | `/api/groups/{id}/members` | Group members | EMPORG |

---

#### **5. Security Domain API** (`/api/security`)

**Domain Scope:** Authentication, authorization, audit

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| POST | `/api/security/authenticate` | User login | GPMAIN |
| POST | `/api/security/change-password` | Password change | PASSWD |
| GET | `/api/security/permissions` | User permissions | CHKSEC.CPY |
| GET | `/api/security/audit-log` | Security audit | AUDIT |
| POST | `/api/security/access-request` | Request access | ACCESS.CPY |

---

#### **6. Reference Data Domain API** (`/api/reference`)

**Domain Scope:** Configuration and lookup tables

| HTTP Method | Endpoint | Purpose | COBOL Program Origin |
|------------|----------|---------|---------------------|
| GET | `/api/reference/states` | State codes | STMNWO |
| GET | `/api/reference/counties` | County codes | BHMAIN |
| GET | `/api/reference/loan-classes` | Loan classifications | BICLAS, CLASTR |
| GET | `/api/reference/rate-tables` | Interest rate tables | Code tables |
| PUT | `/api/reference/codes` | Update code table | CDMAIN |

---

### **API Domain Architecture Diagram**

<svg width="1100" height="800" xmlns="http://www.w3.org/2000/svg">
  <text x="550" y="30" text-anchor="middle" font-size="24" font-weight="bold" fill="#1a1a1a">OTIS Domain-Driven API Architecture</text>
  
  <!-- API Gateway -->
  <rect x="400" y="70" width="300" height="60" fill="#667eea" stroke="#4338ca" stroke-width="3" rx="8"/>
  <text x="550" y="95" text-anchor="middle" font-size="18" font-weight="bold" fill="white">API Gateway</text>
  <text x="550" y="115" text-anchor="middle" font-size="12" fill="white">Authentication, Routing, Rate Limiting</text>
  
  <!-- Core Business APIs -->
  <text x="550" y="170" text-anchor="middle" font-size="16" font-weight="bold" fill="#1a1a1a">Core Business APIs</text>
  
  <rect x="50" y="190" width="200" height="110" fill="#e0e7ff" stroke="#667eea" stroke-width="2" rx="6"/>
  <text x="150" y="215" text-anchor="middle" font-size="14" font-weight="bold" fill="#4338ca">Loan API</text>
  <text x="150" y="235" text-anchor="middle" font-size="11" fill="#6366f1">/api/loans/*</text>
  <text x="150" y="255" text-anchor="middle" font-size="10" fill="#6366f1">Programs: LONPFC,</text>
  <text x="150" y="270" text-anchor="middle" font-size="10" fill="#6366f1">APIPAY, LPMAIN</text>
  <text x="150" y="290" text-anchor="middle" font-size="10" fill="#6366f1">Sub: /payments</text>
  
  <rect x="270" y="190" width="200" height="110" fill="#dbeafe" stroke="#3b82f6" stroke-width="2" rx="6"/>
  <text x="370" y="215" text-anchor="middle" font-size="14" font-weight="bold" fill="#1e40af">Dealer API</text>
  <text x="370" y="235" text-anchor="middle" font-size="11" fill="#3b82f6">/api/dealers/*</text>
  <text x="370" y="255" text-anchor="middle" font-size="10" fill="#3b82f6">Programs: DLMAIN,</text>
  <text x="370" y="270" text-anchor="middle" font-size="10" fill="#3b82f6">LP01DL, DLMAN3</text>
  <text x="370" y="290" text-anchor="middle" font-size="10" fill="#3b82f6">Related: /addons</text>
  
  <rect x="490" y="190" width="200" height="110" fill="#dcfce7" stroke="#22c55e" stroke-width="2" rx="6"/>
  <text x="590" y="215" text-anchor="middle" font-size="14" font-weight="bold" fill="#15803d">Addon API</text>
  <text x="590" y="235" text-anchor="middle" font-size="11" fill="#22c55e">/api/addons/*</text>
  <text x="590" y="255" text-anchor="middle" font-size="10" fill="#22c55e">Programs: ADMAIN,</text>
  <text x="590" y="270" text-anchor="middle" font-size="10" fill="#22c55e">LONPFE, LP01IC</text>
  <text x="590" y="290" text-anchor="middle" font-size="10" fill="#22c55e">Insurance Products</text>
  
  <rect x="710" y="190" width="200" height="110" fill="#fef3c7" stroke="#f59e0b" stroke-width="2" rx="6"/>
  <text x="810" y="215" text-anchor="middle" font-size="14" font-weight="bold" fill="#92400e">Reporting API</text>
  <text x="810" y="235" text-anchor="middle" font-size="11" fill="#d97706">/api/reports/*</text>
  <text x="810" y="255" text-anchor="middle" font-size="10" fill="#d97706">Programs: AUDIQ,</text>
  <text x="810" y="270" text-anchor="middle" font-size="10" fill="#d97706">SUMMBY, PERUSE</text>
  <text x="810" y="290" text-anchor="middle" font-size="10" fill="#d97706">Compliance Reports</text>
  
  <!-- Processing & Support APIs -->
  <text x="550" y="340" text-anchor="middle" font-size="16" font-weight="bold" fill="#1a1a1a">Processing & Management APIs</text>
  
  <rect x="110" y="360" width="200" height="100" fill="#fce7f3" stroke="#ec4899" stroke-width="2" rx="6"/>
  <text x="210" y="385" text-anchor="middle" font-size="14" font-weight="bold" fill="#9f1239">Batch API</text>
  <text x="210" y="405" text-anchor="middle" font-size="11" fill="#ec4899">/api/batch/*</text>
  <text x="210" y="425" text-anchor="middle" font-size="10" fill="#ec4899">Programs: BPOTH,</text>
  <text x="210" y="440" text-anchor="middle" font-size="10" fill="#ec4899">EOMAIN, DAILY</text>
  
  <rect x="330" y="360" width="200" height="100" fill="#e0e7ff" stroke="#8b5cf6" stroke-width="2" rx="6"/>
  <text x="430" y="385" text-anchor="middle" font-size="14" font-weight="bold" fill="#5b21b6">Branch API</text>
  <text x="430" y="405" text-anchor="middle" font-size="11" fill="#8b5cf6">/api/branches/*</text>
  <text x="430" y="425" text-anchor="middle" font-size="10" fill="#8b5cf6">Programs: BRMAN2,</text>
  <text x="430" y="440" text-anchor="middle" font-size="10" fill="#8b5cf6">GPMAN1, BROPM1</text>
  
  <rect x="550" y="360" width="200" height="100" fill="#dbeafe" stroke="#06b6d4" stroke-width="2" rx="6"/>
  <text x="650" y="385" text-anchor="middle" font-size="14" font-weight="bold" fill="#0e7490">Security API</text>
  <text x="650" y="405" text-anchor="middle" font-size="11" fill="#06b6d4">/api/security/*</text>
  <text x="650" y="425" text-anchor="middle" font-size="10" fill="#06b6d4">Programs: GPMAIN,</text>
  <text x="650" y="440" text-anchor="middle" font-size="10" fill="#06b6d4">PASSWD, CHKSEC</text>
  
  <rect x="770" y="360" width="200" height="100" fill="#dcfce7" stroke="#84cc16" stroke-width="2" rx="6"/>
  <text x="870" y="385" text-anchor="middle" font-size="14" font-weight="bold" fill="#3f6212">Reference API</text>
  <text x="870" y="405" text-anchor="middle" font-size="11" fill="#84cc16">/api/reference/*</text>
  <text x="870" y="425" text-anchor="middle" font-size="10" fill="#84cc16">Programs: STMNWO,</text>
  <text x="870" y="440" text-anchor="middle" font-size="10" fill="#84cc16">BHMAIN, BICLAS</text>
  
  <!-- Shared Services Layer -->
  <text x="550" y="500" text-anchor="middle" font-size="16" font-weight="bold" fill="#1a1a1a">Shared Services Layer</text>
  
  <rect x="150" y="520" width="180" height="80" fill="#fef3c7" stroke="#eab308" stroke-width="2" rx="6"/>
  <text x="240" y="545" text-anchor="middle" font-size="13" font-weight="bold" fill="#713f12">Date/Time Service</text>
  <text x="240" y="565" text-anchor="middle" font-size="10" fill="#ca8a04">TIMALL, GETDLS.CPY</text>
  <text x="240" y="580" text-anchor="middle" font-size="10" fill="#ca8a04">Shared Utilities</text>
  
  <rect x="350" y="520" width="180" height="80" fill="#fce7f3" stroke="#f472b6" stroke-width="2" rx="6"/>
  <text x="440" y="545" text-anchor="middle" font-size="13" font-weight="bold" fill="#831843">Validation Service</text>
  <text x="440" y="565" text-anchor="middle" font-size="10" fill="#f472b6">Business Rules</text>
  <text x="440" y="580" text-anchor="middle" font-size="10" fill="#f472b6">Cross-Domain Logic</text>
  
  <rect x="550" y="520" width="180" height="80" fill="#e0e7ff" stroke="#a78bfa" stroke-width="2" rx="6"/>
  <text x="640" y="545" text-anchor="middle" font-size="13" font-weight="bold" fill="#4c1d95">Error Handler</text>
  <text x="640" y="565" text-anchor="middle" font-size="10" fill="#a78bfa">ERRLOG, AUDIT</text>
  <text x="640" y="580" text-anchor="middle" font-size="10" fill="#a78bfa">Centralized Logging</text>
  
  <rect x="750" y="520" width="180" height="80" fill="#dbeafe" stroke="#60a5fa" stroke-width="2" rx="6"/>
  <text x="840" y="545" text-anchor="middle" font-size="13" font-weight="bold" fill="#1e40af">Help Service</text>
  <text x="840" y="565" text-anchor="middle" font-size="10" fill="#60a5fa">HELPLINK.CPY</text>
  <text x="840" y="580" text-anchor="middle" font-size="10" fill="#60a5fa">Documentation API</text>
  
  <!-- Data Layer -->
  <text x="550" y="640" text-anchor="middle" font-size="16" font-weight="bold" fill="#1a1a1a">Data Access Layer</text>
  
  <rect x="200" y="660" width="700" height="80" fill="#f3f4f6" stroke="#9ca3af" stroke-width="2" rx="6"/>
  <text x="550" y="690" text-anchor="middle" font-size="14" font-weight="bold" fill="#1f2937">Unified Data Access</text>
  <text x="550" y="710" text-anchor="middle" font-size="11" fill="#6b7280">Entity Framework | Dapper | ADO.NET</text>
  <text x="550" y="725" text-anchor="middle" font-size="11" fill="#6b7280">SQL Server | Azure SQL | Cosmos DB</text>
  
  <!-- Arrows from Gateway to APIs -->
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
      <polygon points="0 0, 10 3, 0 6" fill="#667eea"/>
    </marker>
  </defs>
  <line x1="450" y1="130" x2="150" y2="190" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="500" y1="130" x2="370" y2="190" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="550" y1="130" x2="590" y2="190" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  <line x1="600" y1="130" x2="810" y2="190" stroke="#667eea" stroke-width="2" marker-end="url(#arrow)"/>
  
  <!-- Legend -->
  <text x="50" y="770" font-size="12" font-weight="bold" fill="#1a1a1a">Legend:</text>
  <rect x="50" y="780" width="15" height="15" fill="#e0e7ff" stroke="#667eea"/>
  <text x="75" y="792" font-size="11" fill="#1a1a1a">Core Business</text>
  <rect x="180" y="780" width="15" height="15" fill="#fce7f3" stroke="#ec4899"/>
  <text x="205" y="792" font-size="11" fill="#1a1a1a">Processing</text>
  <rect x="300" y="780" width="15" height="15" fill="#fef3c7" stroke="#eab308"/>
  <text x="325" y="792" font-size="11" fill="#1a1a1a">Shared Services</text>
</svg>

---

### **API Versioning Strategy by Domain**

Each domain API should be independently versioned to allow evolutionary changes:

```
/api/v1/loans/*          - Loan API version 1
/api/v2/loans/*          - Loan API version 2 (breaking changes)
/api/v1/dealers/*        - Dealer API version 1
/api/v1/reports/*        - Reports API version 1
```

**Benefits:**
- Independent evolution of domains
- No forced coordination between teams
- Gradual migration for API consumers

---

### **Domain API Implementation Patterns**

#### **Pattern 1: Domain-Specific Controllers**

```csharp
// Loan Domain
[ApiController]
[Route("api/[controller]")]
public class LoansController : ControllerBase
{
    private readonly ILoanService _loanService;
    
    [HttpPost]
    public async Task<ActionResult<LoanDto>> CreateLoan([FromBody] CreateLoanRequest request)
    {
        // Maps to LONPFC program logic
        var loan = await _loanService.CreateLoan(request);
        return CreatedAtAction(nameof(GetLoan), new { id = loan.Id }, loan);
    }
    
    [HttpPost("{id}/payments")]
    public async Task<ActionResult<PaymentDto>> PostPayment(
        string id, 
        [FromBody] PaymentRequest request)
    {
        // Maps to APIPAY program logic
        var payment = await _loanService.PostPayment(id, request);
        return Ok(payment);
    }
}

// Dealer Domain
[ApiController]
[Route("api/[controller]")]
public class DealersController : ControllerBase
{
    private readonly IDealerService _dealerService;
    
    [HttpPost]
    public async Task<ActionResult<DealerDto>> CreateDealer([FromBody] CreateDealerRequest request)
    {
        // Maps to DLMAIN program logic
        var dealer = await _dealerService.CreateDealer(request);
        return CreatedAtAction(nameof(GetDealer), new { id = dealer.Id }, dealer);
    }
}
```

---

#### **Pattern 2: Domain-Specific DTOs**

```csharp
namespace OTIS.Api.Loans.Models
{
    // Loan domain DTOs
    public class LoanDto
    {
        public string LoanNumber { get; set; }
        public decimal PrincipalBalance { get; set; }
        public decimal InterestRate { get; set; }
        // Maps to COBOL loan file structure
    }
    
    public class PaymentRequest
    {
        public decimal Amount { get; set; }
        public DateTime PaymentDate { get; set; }
        public string PaymentType { get; set; }  // PL, RP, P2
        // Maps to APIPAY input structure
    }
}

namespace OTIS.Api.Dealers.Models
{
    // Dealer domain DTOs
    public class DealerDto
    {
        public string DealerId { get; set; }
        public string DealerName { get; set; }
        // Maps to LP01DL copybook
    }
}
```

---

#### **Pattern 3: Domain Services**

```csharp
namespace OTIS.Business.Loans
{
    public interface ILoanService
    {
        Task<LoanDto> CreateLoan(CreateLoanRequest request);
        Task<LoanDto> GetLoan(string loanId);
        Task<PaymentDto> PostPayment(string loanId, PaymentRequest request);
        Task<PayoffQuote> CalculatePayoff(string loanId, DateTime payoffDate);
    }
    
    public class LoanService : ILoanService
    {
        // Implements business logic from LONPFC, APIPAY, etc.
        // Calls data repositories
        // Applies business rules
    }
}

namespace OTIS.Business.Dealers
{
    public interface IDealerService
    {
        Task<DealerDto> CreateDealer(CreateDealerRequest request);
        Task<DealerDto> GetDealer(string dealerId);
        Task<DealerStatistics> GetStatistics(string dealerId);
    }
    
    public class DealerService : IDealerService
    {
        // Implements business logic from DLMAIN, LP01DL, etc.
    }
}
```

---

### **Cross-Domain Considerations**

#### **1. Shared Authentication & Authorization**

All domain APIs share common security infrastructure:

```csharp
// Centralized JWT authentication
services.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
    .AddJwtBearer(options => { /* config */ });

// Domain-specific authorization
[Authorize(Policy = "LoanProcessing")]
public class LoansController : ControllerBase { }

[Authorize(Policy = "DealerManagement")]
public class DealersController : ControllerBase { }
```

#### **2. Cross-Domain Transactions**

When operations span domains (e.g., loan creation + dealer assignment):

```csharp
public class LoanCreationService
{
    private readonly ILoanService _loanService;
    private readonly IDealerService _dealerService;
    private readonly IUnitOfWork _unitOfWork;
    
    public async Task<LoanDto> CreateLoanWithDealer(
        CreateLoanRequest loanRequest, 
        string dealerId)
    {
        using var transaction = await _unitOfWork.BeginTransactionAsync();
        try
        {
            // Validate dealer exists (cross-domain call)
            var dealer = await _dealerService.GetDealer(dealerId);
            
            // Create loan (loan domain)
            var loan = await _loanService.CreateLoan(loanRequest);
            
            // Associate (cross-domain operation)
            loan.DealerId = dealerId;
            await _loanService.Update Loan(loan);
            
            await transaction.CommitAsync();
            return loan;
        }
        catch
        {
            await transaction.RollbackAsync();
            throw;
        }
    }
}
```

#### **3. Domain Events**

Use events for loose coupling between domains:

```csharp
// Loan domain publishes event
public class LoanCreatedEvent : IDomainEvent
{
    public string LoanId { get; set; }
    public string DealerId { get; set; }
    public DateTime CreatedDate { get; set; }
}

// Dealer domain subscribes
public class DealerStatisticsHandler : IEventHandler<LoanCreatedEvent>
{
    public async Task Handle(LoanCreatedEvent evt)
    {
        // Update dealer statistics when loan created
        await _dealerService.IncrementLoanCount(evt.DealerId);
    }
}
```

---

### **Domain API Benefits**

| Benefit | Description |
|---------|-------------|
| **Clear Boundaries** | Each API has well-defined responsibility aligned with business domain |
| **Team Alignment** | Teams can own entire domain API (controllers, services, data) |
| **Independent Deployment** | Deploy Loan API without affecting Dealer API |
| **Scalability** | Scale high-traffic APIs (Payments) independently |
| **Technology Flexibility** | Different domains can use different tech stacks if needed |
| **Easier Testing** | Domain-focused integration tests |
| **Better Documentation** | API docs organized by business capability |

---

### **Migration Strategy**

#### **Phase 1: Core Transaction APIs**
- Loan API (highest priority for business operations)
- Payment API sub-domain
- Dealer API

#### **Phase 2: Management & Reporting**
- Reporting API
- Branch & Group APIs
- Security API

#### **Phase 3: Batch & Support**
- Batch Processing API
- Reference Data API
- Utility APIs

---

**Source References:**
- OTIS Functional Domain Analysis (Chapter 2, Section 2.9)
- Domain Investigation Results (November 2025)
- Program Flow Analysis (Index: new_cobol_program_deps, new_cobol_flow_edges_v2)
- Menu Structure (Index: new_cobol_menu_trees, new_cobol_ui_paths)

---

## Conclusion

API modeling and design represents the bridge between legacy COBOL systems and modern application architectures. This chapter has explored the three essential dimensions of API design: translating COBOL programs to REST endpoints, mapping data structures to DTOs and models, and designing CRUD operations and business workflows. Each dimension requires careful consideration of both technical constraints and business requirements to create APIs that are not only functional but also maintainable, scalable, and aligned with industry best practices.

The translation of COBOL programs to API endpoints demands a shift in thinking from procedural file operations to resource-oriented REST patterns. By identifying cohesive service boundaries, mapping transaction programs to HTTP operations, and designing workflow APIs that preserve business logic integrity, development teams can create APIs that accurately represent business capabilities while hiding the complexity of legacy implementations. The key is to think in terms of business resources and operations rather than direct translations of COBOL program logic.

Data structure mapping is more than a technical exercise—it's an opportunity to create clean, well-documented contracts between API consumers and the business domain. By thoughtfully converting COBOL PICTURE clauses to C# types, handling complex structures like REDEFINES and OCCURS, and designing DTOs that separate API contracts from internal models, teams can create APIs that are both accurate and convenient to use. Proper validation, clear naming conventions, and comprehensive documentation ensure that API consumers can work effectively without needing to understand COBOL internals.

Finally, designing CRUD operations and business workflows requires balancing RESTful principles with practical business requirements. By implementing proper error handling, transaction management, idempotency, and audit logging, APIs can provide the reliability and transparency that enterprise applications demand. Whether handling simple CRUD operations or complex multi-step workflows, the patterns and practices presented in this chapter provide a foundation for API designs that support both immediate migration needs and long-term architectural evolution. The result is an API layer that not only replaces COBOL functionality but enhances it with modern capabilities like asynchronous processing, fine-grained security, and comprehensive observability.
