      * COPYMEMBER: LIBSP/SP01EP
      **************************************************
      * LOAN ELECTRONIC PAYMENT FILE     BYTES = 175
      * KEY - BR#, ACCTNO
      * REV:
      **************************************************
       78  EP-SIZE                   VALUE 175.
       01  EP-REC.
           03  EP1-KEY.
               05  EP-BRNO           PIC 9(4).
               05  EP-ACCTNO         PIC 9(8).
               05  EP-ACCTNOX REDEFINES EP-ACCTNO.
                   07  EP-NUMBER     PIC 9(6).
                   07  EP-SEQNO      PIC 9(2).
      *---------------------------------------------------
           03  EP-SSNO               PIC 9(9)     COMP-3.
           03  EP-NAME-ON-ACCOUNT    PIC X(22).
           03  EP-BANK-ACCTNO        PIC X(17).
           03  EP-BANK-ROUTING-NO    PIC 9(9)     COMP-3.
           03  EP-ACCT-TYPE          PIC X.
           03  EP-PAYMENT-AMT        PIC S9(7)V99 COMP-3.
           03  EP-UNITPER-CD         PIC X.
               88  EP-VALID-UNITPER-CD VALUE "M" "W" "B" "S".
               88  EP-MONTHLY        VALUE "M".
               88  EP-SEMI-MONTHLY   VALUE "S".
               88  EP-WEEKLY         VALUE "W".
               88  EP-BIWEEKLY       VALUE "B".
           03  EP-PAY-DAY            PIC 9(2)     COMP-6.
           03  EP-BEG-PAY-DATE       PIC 9(6)     COMP-6.
           03  EP-END-PAY-DATE       PIC 9(6)     COMP-6.
           03  EP-STOP-FG            PIC X.
           03  EP-PRENOTE-FG         PIC X.
           03  EP-LAST-COMPUTED-DATE PIC 9(6)     COMP-6.
           03  EP-REFCD              PIC X(5).
           03  EP-ALTERNATE-DATE     PIC 9(6)     COMP-6.
           03  EP-ALTERNATE-AMT      PIC S9(7)V99 COMP-3.
           03  EP-LAST-BATCH-DATE    PIC 9(6)     COMP-6.
           03  EP-LAST-BATCH-AMT     PIC S9(7)V99 COMP-3.
           03  EP-LAST-BATCH-USERID  PIC X(8).
           03  EP-LAST-NACHA-DATE    PIC 9(6)     COMP-6.
           03  EP-LAST-NACHA-AMT     PIC S9(7)V99 COMP-3.
           03  EP-LAST-NACHA-USERID  PIC X(8).
      *---------------------------------------------------
           03  FILLER                PIC X(50).
