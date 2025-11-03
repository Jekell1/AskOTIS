# LONPFC Program Documentation

**Generated Date:** July 17, 2025  
**Program Type:** Payment Processing Module  
**Called By:** APIPAY  

---

## Overview

**LONPFC** is a specialized payment processing program that handles standard loan payments and alternative payment methods. It is called by APIPAY when processing transaction codes "PY" (Payment) and "PA" (Payment Alternative).

## Transaction Types Handled

- **PY** - Standard Payment: Regular loan payment processing
- **PA** - Payment Alternative: Alternative payment method processing

## Purpose

LONPFC serves as the primary payment processor for:
- Standard loan payments
- Alternative payment processing
- Payment validation and application
- Interest and principal allocation
- Account balance updates

## Processing Logic

Based on the APIPAY program references, LONPFC:

1. **Receives Payment Data**: Gets loan and payment information from APIPAY
2. **Validates Payment**: Ensures payment amount and timing are valid
3. **Applies Payment**: Allocates payment to interest and principal
4. **Updates Account**: Modifies loan record with new balances
5. **Returns Status**: Provides success/failure status to APIPAY

## Error Handling

LONPFC can reject payments for various reasons:
- Invalid payment amount
- Account restrictions
- Business rule violations
- System constraints

When LONPFC rejects a payment, APIPAY logs the error as "ALT PREPAYMENT" and exits with an appropriate error code.

## Integration with APIPAY

LONPFC is called through the COBOL CALL statement:
```cobol
MOVE "LP/LONPFC" TO FORM-NAM
CALL FORM-PROGX USING ... 
```

The program communicates with APIPAY through:
- **LONPF Buffer**: Shared memory structure for payment data
- **Error Codes**: Return status indicating success or failure
- **Account Updates**: Direct modifications to loan records

## Dependencies

LONPFC requires:
- Access to loan master file (LN1-FILE)
- Work file processing capability
- Proper LONPF buffer setup
- Valid account and payment data

---

*Note: This documentation is based on references found in the APIPAY program. LONPFC appears to be an external program that is called by APIPAY but not embedded within the APIPAY_Inlined.CBL file.*
