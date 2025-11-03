      *******************************************
      * ILLINOIS VERITEC LOAN/UPDATE EXTRACT
      * FIELDS FOR OUTPUT RECORD
      * 2022.1019 BAH CHANGED ILN-NUMBER-OF-PAYS TO Z(4) #1570A
      * 2023.0530 BAH INCREASED TRANS-ID, FEE-AMOUNT, DUE-AMOUNT,
      *               TOTAL-PAID, DEBUT-SOLD, DEBT-CONTINUING, 
      *               SURPLUS-DEFICIENCY, DEFICIENCY-WAIVED, NSF-FEE
      *               SO THE DELIMITED BY "  " WORKS AND DOESNT LEAVE ANY
      *               SPACES #1606
      *******************************************
       01  ILLINOIS-FIELDS.
      * EITHER "NEW" OR "UPDATE"
           03  ILN-ACTION             PIC X(6).
      * CILA LICENSE. LAST 4 DIGITS OF LICENSE NUMBER (BRANCH RECORD)
           03  ILN-LICENSE-ID         PIC 9(4).
      * LENDER UNIQUE IDENTIFICATION NUMBER
           03  ILN-LENDER-ID.
               05  ILN-LENDER-BRNO    PIC 9(4).
               05  ILN-ACCTNO         PIC 9(6).
      * NUMBER OF CUSTOMERS ASSOCIATED WITH THIS LOAN; 1,2
           03  ILN-CUST-COUNT         PIC 9.
      * ONLY REPORTING 2, LEAVE 3 AND 4 ALL BLANK
           03  ILN-CUST-DATA     OCCURS 4.
               05  ILN-SSNO           PIC 9(9).
               05  ILN-ID-TYPE        PIC X(3).
               05  ILN-FNAME          PIC X(35).
               05  ILN-LNAME          PIC X(35).
               05  ILN-ADR1           PIC X(30).
               05  ILN-ADR2           PIC X(30).
               05  ILN-CITY           PIC X(30).
               05  ILN-STATE          PIC X(2).
               05  ILN-ZIP            PIC X(10).

      * LOAN TYPE; INSTALLMENT (IL), TITLE LOAN (TL)
      *            COLLCD AP,AU,WA,WB = "TL" ELSE "IL"
           03  ILN-LOAN-TYPE          PIC XX.
      * LOANDATE
           03  ILN-AGREEMENT-DATE     PIC 99/99/9999.
      * ORIGINAL MATURITY DATE
           03  ILN-MATURITY-DATE      PIC 99/99/9999.
      * PRINCIPAL/AMOUNT FINANCED
           03  ILN-PRINCIPAL-AMOUNT   PIC Z(7).ZZ BLANK ZERO.
      * TOTAL OF ALL PAYMENT
           03  ILN-TOTAL-AMOUNT       PIC Z(7).99 BLANK ZERO.
      * PRECOMPUTED (PM) OR INTEREST BEARING (IB) USE IB/PC LOAN FLAG
           03  ILN-INTEREST-TYPE      PIC X(2).
      * NUMBER OF PAYMENT
           03  ILN-NUMBER-OF-PAYS     PIC Z(4)    BLANK ZERO.
      * SCHEDULED PAYMENT AMOUNT
           03  ILN-SCHEDULED-PAYMENT  PIC Z(7).ZZ BLANK ZERO.
      * SECURITY  (COLLATERAL)
           03  ILN-SECURITY           PIC XX.
      * SECURITY DESCRIPTION
           03  ILN-SECURITY-DESC      PIC X(50).
      * APR
           03  ILN-APRATE             PIC Z(4).9(3) BLANK ZERO.
      * PLPA APR  PREDITATORY LOAN PREVENTION AT APR
           03  ILN-PLPA-APRATE        PIC Z(4).9(3) BLANK ZERO.
      * PAYOFF (DOES THIS LOAN HAVE A PRIOR LOAN  Y OR N)
           03  ILN-PAYOFF             PIC X.
      * VIN (COLLATERAL ID)
           03  ILN-VIN                PIC X(17).
      ******************************************
      * UPDATE FIELDS - ALL PART OF SAME RECORD
      ******************************************
      * MISC #15
           03  ILU-TRANS-ID           PIC X(22).
           03  ILU-EVENT              PIC XX.
           03  ILU-EVENT-DATE         PIC 99/99/9999.
           03  ILU-FEE-AMOUNT         PIC Z(6).99 BLANK ZERO.
      * ACCELERATED OR IMMEDIATELY DUE AMOUNT
           03  ILU-DUE-AMOUNT         PIC Z(8).99 BLANK ZERO.
           03  ILU-TOTAL-PAID         PIC Z(9).99 BLANK ZERO.
      * PAID IN FULL (PF) OR OTHER (OT)
           03  ILU-CLOSE-REASON       PIC XX.
           03  ILU-CLOSE-DESC         PIC X(100).
      * NOTE: MADE THESE PIC XX SO THE STRING WOULD LEAVE IT EMPTY AND NOT
      *       LEAVE 1 SPACE
      * HAS THE LENDER SOLD THE DEBT
           03  ILU-DEBT-SOLD          PIC X(3).
      * IS LENDER CONTINUING TO ATTEMPT TO COLLECT THE DEBT
           03  ILU-DEBT-CONTINUING    PIC X(3).
      * SURPLUS/DEFICIENCY REMAINING AFTER SALE
           03  ILU-SURPLUS-DEFICIENCY PIC X(3).
      * IF SURPLUS/DEFICIENCY, IS THE DEFICIENCY WAIVED
           03  ILU-DEFICIENCY-WAIVED  PIC X(3).
           03  ILU-NSF-FEE            PIC Z(9).99 BLANK ZERO.
 
           03  ILN-CARRAGE-RET   PIC X          VALUE X"0D".
           03  ILN-LINE-FD       PIC X          VALUE X"0A".

       01  ILN-HEADER-INFO       PIC X(85).
       01  ILN-CUST-INFO         PIC X(1375).
       01  ILN-LOAN-INFO         PIC X(220).
       01  ILU-UPDATE-INFO       PIC X(200).
