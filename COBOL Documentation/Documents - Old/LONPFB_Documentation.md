# LONPFB Program Documentation

**Generated Date:** July 17, 2025  
**Program Type:** Rebate Processing Module  
**Called By:** APIPAY  

---

## Overview

**LONPFB** is a specialized rebate processing program that handles interest rebate calculations and applications. It is called by APIPAY when processing transaction code "RP" (Rebate Payment).

## Transaction Types Handled

- **RP** - Rebate Payment: Interest rebate processing for early payoffs and adjustments

## Purpose

LONPFB is designed to handle:
- Interest rebate calculations
- Early payoff rebate processing
- Interest adjustment applications
- "IN" rebate fixes (as noted in APIPAY comments)
- Account balance corrections for rebated interest

## Processing Logic

Based on the APIPAY program references, LONPFB:

1. **Calculates Rebate Amount**: Determines proper rebate based on loan terms
2. **Validates Rebate**: Ensures rebate is appropriate for the account
3. **Applies Rebate**: Credits the rebate amount to the account
4. **Updates Interest**: Adjusts interest balances and calculations
5. **Returns Status**: Provides processing results to APIPAY

## Business Rules

LONPFB implements specific business rules for:
- **Rebate Eligibility**: Determines when rebates are appropriate
- **Calculation Methods**: Various rebate calculation algorithms
- **Interest Adjustments**: Proper handling of interest corrections
- **Account Impact**: Ensuring rebates are properly applied

## Error Handling

LONPFB can reject rebate requests for:
- Ineligible accounts
- Invalid rebate amounts
- Timing restrictions
- Business rule violations

Failed rebate processing results in APIPAY logging appropriate error messages and terminating the transaction.

## Integration with APIPAY

LONPFB is called through the COBOL CALL statement:
```cobol
MOVE "LP/LONPFB" TO FORM-NAM
CALL FORM-PROGX USING ... 
```

Communication occurs through:
- **LONPF Buffer**: Shared payment processing structure
- **Rebate Calculations**: Complex interest rebate computations
- **Account Updates**: Direct loan record modifications
- **Status Returns**: Success/failure indicators

## Dependencies

LONPFB requires:
- Access to loan master file (LN1-FILE)
- Interest calculation routines
- Rebate calculation algorithms
- Proper LONPF buffer initialization
- Valid account and rebate data

## Special Notes

- LONPFB handles "IN" rebate fixes as mentioned in APIPAY documentation
- Critical for early payoff scenarios where interest rebates are required
- Integrates with overall payment processing workflow

---

*Note: This documentation is based on references found in the APIPAY program. LONPFB appears to be an external program that is called by APIPAY but not embedded within the APIPAY_Inlined.CBL file.*
