      * COPYMEMBER: LIBSP/SP01EW
      ******************************************************************
      *
      *   LOAN ELECTRONIC PAYMENT WORK FILE
      *
      *   NOTE: EW-TRANS-DATE IS ACTIVE CCYYMMDD AS OF A30
      *         IN A15, EW-TRANS-DATE IS STORED AS YYMMDD.
      *
      *=================================================================
      * REV:
      * KEC 2016.0108 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASED SIZE OF EW-REC; EW-SIZE = 200.
      *         ADDED EW-LTOUCH-DATE.
      *         MERGED NUMBER & SEQNO TO MAKE ACCOUNT NUMBER 9(8).
      *         INCREASED AMOUNT FROM 9(7)V99 TO 9(9)V99.
      *         INCREASED DATES TO 9(8) COMP-3.
      *         CHANGED COMP-6 TO COMP-3.
      *         INCREASED USERID FIELDS FROM 8 TO 10 BYTES.
      *
      ******************************************************************
       78  EW-SIZE                             VALUE 200.
       01  EW-REC.
           03  EW1-KEY.
               05  EW-BRNO                     PIC 9(4).
               05  EW2-KEY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    EW-TRANS-DATE IS ACTIVE CCYYMMDD AS OF A30
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *>>XDD DATE=YYYYMMDD
                   07  EW-TRANS-DATE           PIC 9(08).
                   07  FILLER        REDEFINES EW-TRANS-DATE.
                       09  EW-TRANS-DATE-CC    PIC 9(02).
                       09  EW-TRANS-DATE-YY    PIC 9(02).
                       09  EW-TRANS-DATE-MM    PIC 9(02).
                       09  EW-TRANS-DATE-DD    PIC 9(02).
               05  EW-ACCTNO                   PIC 9(8).
               05  EW-SEQ                      PIC 9(2).
      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  EW-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  EW-NAME                         PIC X(22).
           03  EW-BANK-ACCTNO                  PIC X(17).
           03  EW-BANK-ROUTING-NO              PIC 9(9)       COMP-3.
           03  EW-ACCT-TYPE                    PIC X.
           03  EW-PAYMENT-AMT                  PIC S9(9)V99   COMP-3.
           03  EW-PROGRAM                      PIC X(2).
           03  EW-USERID                       PIC X(10).
      *>>XDD DATE=YYYYMMDD
           03  EW-POST-DATE                    PIC 9(8)       COMP-3.
           03  EW-POST-TIME                    PIC 9(6)       COMP-3.
           03  FILLER                          PIC X(101).
