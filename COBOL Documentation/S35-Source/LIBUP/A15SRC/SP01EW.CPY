      * COPYMEMBER: LIBSP/SP01EW
      **************************************************
      * LOAN ELECTRONIC PAYMENT WORK   FILE BYTES = 136
      * KEY - BR#, ACCTNO
      * REV:
      **************************************************
       78  EW-SIZE                   VALUE 136.
       01  EW-REC.
           03  EW1-KEY.
               05  EW-BRNO           PIC 9(4).
               05  EW2-KEY.
                   07  EW-TRANS-DATE PIC 9(6).
                   07  FILLER REDEFINES EW-TRANS-DATE.
                       09  EW-YY     PIC 99.
                       09  EW-MM     PIC 99.
                       09  EW-DD     PIC 99.
               05  EW-ACCTNO         PIC 9(8).
               05  EW-ACCTNOX REDEFINES EW-ACCTNO.
                   07  EW-NUMBER     PIC 9(6).
                   07  EW-SEQNO      PIC 9(2).
               05  EW-SEQ            PIC 99.
      *---------------------------------------------------
           03  EW-NAME               PIC X(22).
           03  EW-BANK-ACCTNO        PIC X(17).
           03  EW-BANK-ROUTING-NO    PIC 9(9)     COMP-3.
           03  EW-ACCT-TYPE          PIC X.
           03  EW-PAYMENT-AMT        PIC S9(7)V99 COMP-3.
           03  EW-PROGRAM            PIC X(2).
           03  EW-USERID             PIC X(8).
           03  EW-POST-DATE          PIC 9(6)     COMP-6.
           03  EW-POST-TIME          PIC 9(6)     COMP-6.
      *---------------------------------------------------
           03  FILLER                PIC X(50).
