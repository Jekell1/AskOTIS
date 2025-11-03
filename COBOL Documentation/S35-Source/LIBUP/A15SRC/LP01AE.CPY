
      **************************************************************************
      *               ACCRUAL EARNINGS FILE            
      * *NEW*
      *   CS 111003 ACCRUAL (AC1ERN) WILL CREATE RECORDS OF FIELDS WRITTEN TO
      *             PAPER REPORTS FOR ACCESS BY DATA WAREHOUSE & FLEXGEN
      *
      *   CS 111024 REMOVED AE-NONACCRU, WE ARE NOT UPDATING FIELD
      *   CS 111028 ADDED AE-INDICATOR TO DETERMINE IF AE CAME FROM THE 
      *             ACCRUAL OR PAYMENT POSTING UPDATE PR#299
      *   CS 121005 ADD AE-NON-ACCRUAL-STATUS, AE-NONACCRUAL-START-DATE AND
      *             AE-MONTHS-PAY-NAPP-COUNTER
      *   CS 131202 ADD AE-NONACRUAL-STOP-DATE REGACC PR#347
      **************************************************************************
       78  AE-SIZE                    VALUE 584.
       01  AE-REC.
           03  AE1-KEY.     
               05  AE-ACCTNO           PIC 9(8).            
               05  FILLER REDEFINES AE-ACCTNO.
                   07  AE-NUMBER       PIC 9(6).
                   07  AE-SEQNO        PIC 99.                
               05  AE-YYMMDD           PIC 9(6).
               05  FILLER REDEFINES AE-YYMMDD.
                   07  AE-YY           PIC 9(2).
                   07  AE-MM           PIC 9(2).
                   07  AE-DD           PIC 9(2).
      *-------------------------------------------------
           03  AE-BRNO             PIC 9(4)   COMP.
           03  AE-CLASS            PIC 99     COMP.
           03  AE-TRDATE           PIC  9(06)    COMP-6.
           03  AE-INTPDDT          PIC  9(06)    COMP-6.
           03  AE-CURBAL           PIC S9(07)V99 COMP-3.
           03  AE-DLNO             PIC  9(04)V99 COMP-6.
           03  AE-SUBCLASS         PIC  9(02)    COMP-6.
           03  AE-LAWCODE          PIC  9(02)    COMP-6.
           03  AE-SSNO             PIC  9(09)    COMP-6.
           03  AE-DELINQ           PIC S9(07)    COMP-3.
      * ANTICIPATED
           03  AE-INT-ANTIC                   PIC S9(07)V99 COMP-3.
           03  AE-ADD-ANTIC                   PIC S9(07)V99 COMP-3.
           03  AE-SRV-ANTIC                   PIC S9(07)V99 COMP-3.
           03  AE-MFEE-ANTIC                  PIC S9(07)V99 COMP-3.
           03  AE-DISC-ANTIC                   PIC S9(07)V99 COMP-3.
           03  AE-OTH-ANTIC                   PIC S9(07)V99 COMP-3.

      * LAST MONTH CONTRACT TO DATE  -LM-TODATE
           03  AE-INTEARN-LM-TODATE           PIC S9(07)V99 COMP-3.
           03  AE-ADDEARN-LM-TODATE           PIC S9(07)V99 COMP-3.
           03  AE-SVCEARN-LM-TODATE           PIC S9(07)V99 COMP-3.
           03  AE-MFEARN-LM-TODATE            PIC S9(07)V99 COMP-3.
           03  AE-DISEARN-LM-TODATE           PIC S9(07)V99 COMP-3.
           03  AE-OTHEARN-LM-TODATE           PIC S9(07)V99 COMP-3.
      * LAST MONTH I.B. ACCRUAL
           03  AE-ACC-AMT-LMO      PIC S9(07)V99 COMP-3.

      *THIS MONTH ONLY   -TMO
           03  AE-INT-TMO          PIC S9(07)V99 COMP-3.
           03  AE-ADD-TMO          PIC S9(07)V99 COMP-3.
           03  AE-SVC-TMO          PIC S9(07)V99 COMP-3.
           03  AE-MF-TMO           PIC S9(07)V99 COMP-3.
           03  AE-DIS-TMO          PIC S9(07)V99 COMP-3.
           03  AE-OTH-TMO          PIC S9(07)V99 COMP-3.

           03  AE-LAST-ACCRU-AMT   PIC S9(07)V99 COMP-3.
           03  AE-INTOWE           PIC S9(07)V99 COMP-3.
           03  AE-TRUE-ASSESS      PIC S9(07)V99 COMP-3.
           03  AE-ACC-AMT          PIC S9(07)V99 COMP-3.
           03  AE-BEGINNING        PIC S9(07)V99 COMP-3.
           03  FILLER              PIC X(5).
      *    03  AE-NONACCRU         PIC S9(07)V99 COMP-3.
           03  AE-ENDING           PIC S9(07)V99 COMP-3.
           03  AE-MAX              PIC S9(07)V99 COMP-3.
           03  AE-STOP-FG          PIC X(01).
      *    -------------------------------------------------------------
           03  AE-POSTDATE         PIC  9(08)    COMP-6.
           03  AE-POSTTIME         PIC  9(08)    COMP-6.
           03  AE-USERID           PIC  X(10).
           03  AE-INDICATOR        PIC X.
           03  AE-NONACCRUAL-STATUS   PIC X.
           03  AE-NONACCRUAL-START-DATE   PIC 9(6) COMP-6. 
           03  AE-MONTHS-PAY-NAPP-COUNTER PIC 99   COMP-6.
           03  AE-NONACCRUAL-STOP-DATE    PIC 9(6) COMP-6.
           03  FILLER                     PIC X(379). 
      *    03  FILLER              PIC X(382).


