      * COPYMEMBER: LIBSP/SP01EP
      ******************************************************************
      *
      *   LOAN ELECTRONIC PAYMENT FILE
      *
      *=================================================================
      * REV:
      * KEC 2016.0108 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASED SIZE OF EP-REC; EP-SIZE = 300.
      *         ADDED EP-LTOUCH-DATE.
      *         MERGED NUMBER & SEQNO TO MAKE ACCOUNT NUMBER 9(8).
      *         INCREASED AMOUNT FROM 9(7)V99 TO 9(9)V99.
      *         INCREASED DATES TO 9(8) COMP-3.
      *         CHANGED COMP-6 TO COMP-3.
      *         INCREASED USERID FIELDS FROM 8 TO 10 BYTES.
      * BLM 160306 ADD REDEFINES OF EP-ACCTNO
      * BLM 161021 REMOVE EP-SEQNO, CHANGE EP-NUMBER TO 9(8)
      ******************************************************************
       78  EP-SIZE                             VALUE 300.
       01  EP-REC.
           03  EP1-KEY.
               05  EP-BRNO                     PIC 9(4).
               05  EP-ACCTNO                   PIC 9(8).
               05  EP-NUMBER REDEFINES EP-ACCTNO PIC 9(8).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  EP-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  EP-SSNO                         PIC 9(9)       COMP-3.
           03  EP-NAME-ON-ACCOUNT              PIC X(22).
           03  EP-BANK-ACCTNO                  PIC X(17).
           03  EP-BANK-ROUTING-NO              PIC 9(9)       COMP-3.
           03  EP-ACCT-TYPE                    PIC X.
           03  EP-PAYMENT-AMT                  PIC S9(9)V99   COMP-3.
           03  EP-UNITPER-CD                   PIC X.
               88  EP-VALID-UNITPER-CD            VALUE "M" "W" "B" "S".
               88  EP-MONTHLY                     VALUE "M".
               88  EP-SEMI-MONTHLY                VALUE "S".
               88  EP-WEEKLY                      VALUE "W".
               88  EP-BIWEEKLY                    VALUE "B".
           03  EP-PAY-DAY                      PIC 9(2)       COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  EP-BEG-PAY-DATE                 PIC 9(8)       COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  EP-END-PAY-DATE                 PIC 9(8)       COMP-3.
           03  EP-STOP-FG                      PIC X.
           03  EP-PRENOTE-FG                   PIC X.
      *>>XDD DATE=YYYYMMDD
           03  EP-LAST-COMPUTED-DATE           PIC 9(8)       COMP-3.
           03  EP-REFCD                        PIC X(5).
      *>>XDD DATE=YYYYMMDD
           03  EP-ALTERNATE-DATE               PIC 9(8)       COMP-3.
           03  EP-ALTERNATE-AMT                PIC S9(9)V99   COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  EP-LAST-BATCH-DATE              PIC 9(8)       COMP-3.
           03  EP-LAST-BATCH-AMT               PIC S9(9)V99   COMP-3.
           03  EP-LAST-BATCH-USERID            PIC X(10).
      *>>XDD DATE=YYYYMMDD
           03  EP-LAST-NACHA-DATE              PIC 9(8)       COMP-3.
           03  EP-LAST-NACHA-AMT               PIC S9(9)V99   COMP-3.
           03  EP-LAST-NACHA-USERID            PIC X(10).
           03  FILLER                          PIC X(149).

