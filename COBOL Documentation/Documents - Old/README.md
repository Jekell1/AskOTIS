# APIPAY System Documentation Index

**Generated Date:** July 17, 2025  
**Documentation Version:** 1.0  
**System:** Loan Payment Processing  

---

## Documentation Overview

This documentation set provides comprehensive coverage of the APIPAY loan payment processing system, designed for users who are unfamiliar with the program. The documentation explains the detailed flow of how the program functions and provides an overall summary of its operations.

---

## Main Documentation

### [APIPAY Program Documentation](APIPAY_Documentation.md)
The primary documentation file that provides:
- Complete program overview and purpose
- Input parameter specifications
- Detailed program flow with visual diagrams
- Transaction types and processing logic
- File processing details
- Error handling and return codes
- Logging mechanisms

---

## Referenced Program Documentation

The APIPAY program calls several specialized payment processing modules. Each has its own documentation:

### Payment Processing
- **[LONPFC Documentation](LONPFC_Documentation.md)** - Standard payment processing for transaction codes PY and PA

### Specialized Processing
- **[LONPFB Documentation](LONPFB_Documentation.md)** - Interest rebate processing for transaction code RP
- **[LONPF9 Documentation](LONPF9_Documentation.md)** - Profit & Loss processing for transaction codes PL, P2, P3
- **[LONPF7 Documentation](LONPF7_Documentation.md)** - Repository and other transaction processing for codes RE, O2, OT
- **[LONPF2 Documentation](LONPF2_Documentation.md)** - Reversal and bankruptcy processing for codes RV, BK, BD
- **[LONPFA Documentation](LONPFA_Documentation.md)** - Loan payoff processing called when payoffs are required

---

## Visual Diagrams

Professional diagrams are included to communicate complex processes clearly:

### Process Flow Diagrams
- **[Program Flow Diagram](Diagrams/APIPAY_Program_Flow.md)** - Complete program execution flow from start to finish
- **[Transaction Flow Diagram](Diagrams/APIPAY_Transaction_Flow.md)** - How different transaction types are routed and processed
- **[Data Flow Diagram](Diagrams/APIPAY_Data_Flow.md)** - File interactions and data movement through the system

---

## Quick Reference

### Transaction Codes Supported
| Code | Description | Processor | Purpose |
|------|-------------|-----------|---------|
| PY | Payment | LONPFC | Standard loan payment |
| PA | Payment Alternative | LONPFC | Alternative payment method |
| RP | Rebate Payment | LONPFB | Interest rebate processing |
| PL | Profit & Loss | LONPF9 | Charge-off to P&L |
| P2 | Profit & Loss Type 2 | LONPF9 | Alternative P&L processing |
| P3 | Profit & Loss Type 3 | LONPF9 | Third P&L option |
| RE | Repossession | LONPF7 | Repossession processing |
| O2 | Other Type 2 | LONPF7 | Secondary other processing |
| RV | Reversal | LONPF2 | Transaction reversal |
| BK | Bankruptcy | LONPF2 | Bankruptcy status change |
| BD | Bankruptcy Dismissal | LONPF2 | Bankruptcy dismissal |
| OT | Other | LONPF7 | Other transaction types |

### Common Return Status Codes
| Status | Description |
|--------|-------------|
| 0 | Successful update |
| 2 | Branch not on file |
| 3 | Reference code not on file |
| 10 | Account not on file |
| 99 | Default error status |

### Input Format Example
```
00100786740000001000CUACH090123OT 1234567890
│││││││││└─────────────────────────┘ └─────────┘
│││││││││                           │
│││││││││ Transaction Data           Transaction ID
│││││││││
│││││││└─ Transaction Code (OT)
│││││└─── Payment Date (090123)
││││└──── Reference Code (CU)
│││└───── Amount (0000001000 = $10.00)
││└────── Account Number (078674)
│└─────── Branch Number (0010)
```

---

## Usage Instructions

### For New Users
1. Start with the [APIPAY Program Documentation](APIPAY_Documentation.md) for a complete overview
2. Review the [Program Flow Diagram](Diagrams/APIPAY_Program_Flow.md) to understand the process
3. Examine specific transaction processing by reviewing the relevant LONPF program documentation
4. Use the visual diagrams to understand data flow and system interactions

### For Technical Staff
1. Review the main documentation for program architecture
2. Study the referenced program documentation for implementation details
3. Use the diagrams for system integration and troubleshooting
4. Reference the quick guide for common operations

### For Business Users
1. Focus on the transaction types table and their purposes
2. Review the input format and return status codes
3. Understand the logging mechanisms for audit purposes
4. Use the business context sections in each document

---

## Documentation Standards

### Format
- All documentation is in Markdown format for easy reading and maintenance
- Professional diagrams use Mermaid format for consistency
- Cross-references link related documentation
- Clear section headers and table of contents for navigation

### Organization
- Main documents in the `Documents` folder
- Diagrams separated in `Documents/Diagrams` folder
- Consistent naming conventions
- Wiki-ready format for easy publishing

### Maintenance
- Generated date included in each document
- Version control recommended for updates
- Regular review suggested as program evolves
- Links maintained between related documents

---

*This documentation set provides comprehensive coverage of the APIPAY system for users at all technical levels. Each document builds on the others to provide a complete understanding of the loan payment processing system.*
